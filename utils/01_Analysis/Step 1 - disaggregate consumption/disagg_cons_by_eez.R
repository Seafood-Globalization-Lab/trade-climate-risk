# Function name: disagg_cons_by_eez.R

# Function purpose: This script disaggregates Aquatic Resource Trade
# In Species (ARTIS) consumption data by EEZ, however the disaggregation
# will only run if the disaggregation has not already happened and been written
# to a .parquet file.

disagg_cons_by_eez <- function() {
  
  if (!file.exists("../data/miscellaneous/consumption.parquet")) {
    
    # Set directory for ARTIS database folder
    db_folder <- "K:/data-storage/artis/ARTIS_1.2.0_SAU_2025_11_14"
    
    # Get attribute tables for ARTIS v1.2.0 to obtain production data
    path_attr_tbls <- file.path(db_folder, "attribute_tables")
    
    # Cleaned and standardized SAU production with EEZ column names
    prod_sau_std_ds <- arrow::open_dataset(
      file.path(
        path_attr_tbls,
        "standardized_sau_prod_more_cols.parquet"
      )
    )
    
    prod_sau_std <- prod_sau_std_ds %>%
      filter(
        year %in% 2019
      ) %>%
      dplyr::collect() %>%
      rename(
        country_iso3c = country_iso3_alpha,
        sciname = SciName,
        method = prod_method
      )
    
    
    # Open ARTIS consumption
    artis_cons <- arrow::open_dataset(file.path(db_folder, "datasets", "ARTIS_v1.2.0_consumption_SAU_mid_all_HS_yrs_2025-12-01.parquet"))
    
    # Filter by analysis parameters and read into memory
    consumption <- artis_cons %>%
      filter(
        year == 2019,
        hs_version == "HS17",
        habitat %in% c("marine", "unknown"),
        method %in% c("capture", "unknown")
      ) %>%
      group_by(year, source_country_iso3c, consumer_iso3c,
               consumption_source, sciname_hs_modified,
               habitat, method, end_use) %>%
      summarize(
        consumption_live_t = sum(consumption_live_t),
        consumption_live_t_capped = sum(consumption_live_t_capped),
        consumption_percap_live_kg = sum(consumption_percap_live_kg),
        consumption_percap_live_kg_capped = sum(consumption_percap_live_kg_capped)
      ) %>% 
      dplyr::collect()
    
    # Clean SAU production EEZ -------------------------------------------------
    # original code from https://github.com/Seafood-Globalization-Lab/artis-dwf/blob/country-profiles/scripts/functions.R
    
    source("../utils/01_Analysis/standardize_sau_eez.R")
    
    prod_sau_eez <- prod_sau_std %>%
      
      # Break apart eez column - identify ISO3 codes with one of the 3 eez columns
      tidyr::separate(
        eez, 
        into = c("eez_1", "eez_2"), 
        sep = "\\(", 
        remove = FALSE, # retain input column in output
        extra = "merge", # what happens when there are too many pieces
        fill = "right" # fill with missing values on the right
      ) %>%
      # remove left over spaces and parentheses
      mutate(
        eez_1 = gsub(" $", "", eez_1),
        eez_2 = gsub("\\)", "", eez_2)
      ) %>%
      
      # create new cleaned eez column - std original value with country code
      mutate(
        eez_iso3c = countrycode(
          eez,
          origin = "country.name",
          destination = "iso3c",
          warn = FALSE # turn off warning "! Some values were not matched unambiguously: [...]"
        )
      ) %>%
      # If NA use eez_1 text
      mutate(
        eez_iso3c = case_when(
          is.na(eez_iso3c) ~ countrycode(
            eez_1,
            origin = "country.name",
            destination = "iso3c",
            warn = FALSE
          ),
          .default = eez_iso3c # use existing value is !is.na()
        )
      ) %>%
      # if still NA use eez_2 text
      mutate(
        eez_iso3c = case_when(
          is.na(eez_iso3c) ~ countrycode(
            eez_2,
            origin = "country.name",
            destination = "iso3c",
            warn = FALSE
          ),
          .default = eez_iso3c
        )
      ) %>%
      
      # use cleaned ISO3c codes to generate standard country name column
      mutate(
        eez_name = countrycode(
          eez_iso3c,
          origin = "iso3c",
          destination = "country.name"
        )
      ) %>%
      
      # adds artis_iso3 and eez_name columns
      standardize_sau_eez("eez_iso3c", "eez_name") %>%
      # remove columns only used for standardization process
      select(-eez_1, -eez, -eez_iso3c, -eez_name) %>%
      rename(
        eez_iso3c = artis_iso3,
        eez_name = artis_country_name,
        eez_detail = eez_2
      ) %>%
      
      # aggregate catch quantity records
      # group_by(group_by(across(-quantity))) %>%
      # summarize(live_weight_t = sum(quantity)) %>%
      group_by(
        year,
        country_iso3c,
        sciname,
        method,
        habitat,
        eez_iso3c,
        eez_name,
        eez_detail
      ) %>%
      summarise(live_weight_t = sum(quantity), .groups = "drop") %>% 
      
      # Tag domestic versus foreign fishing
      mutate(
        dwf = case_when(
          (eez_iso3c == country_iso3c) ~ "domestic",
          TRUE ~ "foreign"
        )
      )
    
    # Proportion of prod by eez ----------------------------------------------
    # Proportion of landings by country flag captured in recorded source eezs
    prod_sau_props <- prod_sau_eez %>%
      # aggregate landings amount -
      # disregard habitat, production method, sector, end use
      # eez_detail disagregates EEZ further - not sure we want to keep
      group_by(
        year,
        country_iso3c,
        sciname,
        eez_name,
        eez_iso3c,
        eez_detail,
        dwf
      ) %>%
      summarise(live_weight_t = sum(live_weight_t), .groups = "keep") %>%
      # calculate prop catch over each eez -
      # does not contract df over 2nd group_by()
      group_by(year, country_iso3c, sciname) %>% # needs to be exactly what data is joining by after
      mutate(prop_by_eez = live_weight_t / sum(live_weight_t)) %>%
      select(-live_weight_t)
    
    # Disaggregate ARTIS by EEZ of catch - join datasets
    consumption_eez <- consumption %>%
      # pull prod_sau_props data for year, source country, and species
      left_join(
        prod_sau_props,
        by = c(
          "year",
          "source_country_iso3c" = "country_iso3c",
          "sciname_hs_modified" = "sciname"
        ),
        # many eezs will match to many consumption year/source country/sciname combo
        relationship = "many-to-many"
      ) %>%
      replace_na(list(prop_by_eez = 1)) %>%
      # recalculate live_weight_t catch - each trade and product record gets split
      # apart by the number of catch eez from prod_sau_props - essentially assigning a
      # probability a product was caught in a specific eez.
      mutate(live_weight_t = consumption_live_t * prop_by_eez, 
             consumption_percap_live_kg = consumption_percap_live_kg * prop_by_eez) %>%
      group_by(
        year,
        eez_iso3c,
        eez_name,
        eez_detail,
        consumer_iso3c,
        sciname_hs_modified,
        end_use,
        consumption_percap_live_kg,
        dwf
      ) %>%
      summarise(live_weight_t = sum(live_weight_t), .groups = "keep", 
                consumption_percap_live_kg = sum(consumption_percap_live_kg))
    
    # FIXIT: think about whether we want just direct human consumption or to also includ fishmeal
    #FIXIT: get rid of everything other than source_country_iso3c
    consumption_eez_human <- consumption_eez %>%
      filter(end_use == "direct human consumption") %>%
      group_by()
    
    # Write consumption data to .parquet
    write_parquet(consumption_eez_human, "../data/miscellaneous/consumption.parquet")
    
    # Send messages to console
    cat("✅ consumption.parquet written to ../data/miscellaneous/\n")
    cat("Consumption-EEZ disaggregation complete. Returning clean file.\n")
    
    # Return clean consumption-EEZ disaggregated file
    return(consumption_eez_human)
    
  } else {
    
    # Read in clean consumption-EEZ disaggregated file
    consumption <- read_parquet("../data/miscellaneous/consumption.parquet")
    
    # Send message to console
    cat("📄 consumption.parquet already exists. Skipping preprocessing to avoid overwriting the existing file. No need to re-disaggregate. Returning clean file.\n")
    
    # Return clean consumption-EEZ disaggregated file
    return(consumption)
    
  }
  
}