# Function name: preprocess_catch_projection_data.R

# Function purpose: Preprocess SeaAroundUs catch projection data obtained
# from co-author Juliano Palacios-Abrantes. Big thanks for his work on acquiring
# this data as it a fundamental component for this research.

preprocess_catch_projection_data <- function(dir) {
  
  if (!file.exists("../data/exposure/mcp_sovereign.parquet")) {
    
    file_names <- list.files(
      paste0(dir, "exposure/species_projections/"),
      pattern = "\\.csv",
      full.names = TRUE
    )
    
    species_names <- fread(paste0(dir, "exposure/dbem_spp_list.csv"))
    
    # Extract SeaAroundUS (Juliano's; SAU) data across folders and combine into
    # one file
    mcp_combined <- map_dfr(file_names, function(f) {
      df_i <- fread(f)
      if (nrow(df_i) == 0) return(NULL)
      
      df_i %>%
        left_join(species_names, by = "taxon_key") %>%
        # mutate(worms_name = coalesce(worms_name, taxon_name)) %>%
        mutate(taxon_name = str_to_lower(worms_name)) %>% 
        select(taxon_name, eez_name, ssp, mean_mcp_delta_2030) %>%
        pivot_wider(names_from = ssp, values_from = mean_mcp_delta_2030, values_fn = mean) %>%
        mutate(file_found_in = substring(f, first = str_length(f) - 9, last = str_length(f) - 4))
    })
    
    # Correct species that ended up as NAs (this species information was obtained
    # from Juliano)
    mcp_combined_sp_clean <- mcp_combined %>%
      mutate(taxon_name = case_when(
        file_found_in == "603923" ~ "peprilus paru",
        TRUE ~ taxon_name
      )) %>%
      # There were only 2 remaining NA species names, which
      # are not present in ARTIS and thus can be dropped
      drop_na(taxon_name)
    
    # Problem: SAU catch projections contain information at
    # sub-sovereign EEZ level (for example, Russia (Kara Sea)) — we want
    # to aggregate EEZ projections across sub-eez names. To aggregate 
    # EEZ projections, we will take a weighted average of the projections
    # multiplied by each sub-EEZ's production weight, another SAU data source
    
    # setDTthreads(1) # this was for faster file reading initially when using 
    # fread to load in the .csv, but we converted to .parquet
    
    # Read in production data by EEZ
    sau_prod <-read_parquet("../data/miscellaneous/sau_prod.parquet")
    
    # Manually clean sau production EEZ names to match Juliano's catch projections
    # Note: North Korea will need to be aggregated to also match Juliano's data
    # which by default aggregates North Korea's sub-national measures into just
    # "Korea (North)"
    sau_prod_clean <- sau_prod %>%
      filter(!(eez == "Korea (North, Yellow Sea)" |
                 eez == "Korea (North, Sea of Japan)")) %>%
      mutate(eez = case_when(
        eez == "St Paul and St. Peter Archipelago (Brazil)" ~
          "Brazil (St Paul and St. Peter Archipelago)",
        eez == "Balearic Islands (Spain)" ~ "Balearic Island (Spain)",
        eez == "Chile (mainland)" ~ "Chile",
        eez == "Ecuador (mainland)" ~ "Ecuador",
        eez == "Saba and Sint Eustatius (Netherlands)" ~ 
          "Saba and Sint Eustaius (Netherlands)",
        eez == "Brazil (mainland)" ~ "Brazil",
        eez == "Fernando de Noronha (Brazil)" ~ "Brazil (Fernando de Noronha)",
        eez == "St Paul and St. Peter Archipelago (Brazil)" ~
          "Brazil (St Paul and St. Peter Archipelago)",
        eez == "Clipperton Isl. (France)" ~ "Clipperton Isl.  (France)",
        eez == "Italy (mainland)" ~ "Italy",
        eez == "Portugal (mainland)" ~ "Portugal",
        eez == "South Africa (Atlantic and Cape)" ~
          "South Africa (Atlantic Coast)",
        eez == "Spain (mainland, Med and Gulf of Cadiz)" ~
          "Spain (Mediterranean and Gulf of Cadiz)",
        eez == "United Kingdom (UK)" ~ "United Kingdom",
        eez == "Greenland (Denmark)" ~ "Greenland",
        TRUE ~ eez
      ))
    
    # Combine North Korea Yellow Sea and Sea of Japan production totals into one
    # North Korea (i.e., maximum resolution of Julianos' catch projection data)
    kor_corrections <- sau_prod %>%
      filter(eez == "Korea (North, Yellow Sea)" |
               eez == "Korea (North, Sea of Japan)") %>%
      mutate(clean_eez = "Korea (North)") %>%
      group_by(clean_eez) %>%
      summarize(sum = sum(sum)) %>% # Sum production totals
      rename(eez_name = clean_eez, total_production = sum)
    
    # Calculate production totals by EEZ
    prod_totals <- sau_prod_clean %>%
      group_by(eez) %>%
      summarize(total_production = sum(sum)) %>%
      rename(eez_name = eez) %>%
      # Correct specific cases to enable joins on catch projection data
      # Ask Juliano about Greece - his data names are "Greece" and "Crete (Greece)"
      # The SAU data only recognizes "Crete (Greece)" and "Greece (without Crete)"
      # Does your "Greece" not include Crete?
      mutate(eez_name = case_when(
        eez_name == "Greece (without Crete)" ~ "Greece",
        TRUE ~ eez_name
      )) %>%
      bind_rows(kor_corrections)
    
    # Clean up EEZ names
    mcp_combined_sp_eez_clean <- mcp_combined_sp_clean %>%
      mutate(
        eez_iso3c = case_when(
          eez_name == "Chagos Archipelago (UK)"            ~ "GBR",
          eez_name == "Christmas Isl. (Australia)"         ~ "AUS",
          eez_name == "High seas"                          ~ "NEI",
          eez_name == "Iran (Sea of Oman)"                 ~ "IRN",
          eez_name == "Mozambique Channel Isl. (France)"   ~ "FRA",
          eez_name == "Mayotte (France)"                   ~ "FRA",
          eez_name == "New Caledonia (France)"             ~ "FRA",
          eez_name == "Norfolk Isl. (Australia)"           ~ "AUS",
          eez_name == "Réunion (France)"                   ~ "FRA",
          eez_name == "Saudi Arabia (Persian Gulf)"        ~ "SAU",
          eez_name == "Svalbard Isl. (Norway)"             ~ "NOR",
          eez_name == "Tokelau (New Zealand)"              ~ "NZL",
          eez_name == "Wallis & Futuna Isl. (France)"      ~ "FRA",
          eez_name == "Yemen (Arabian Sea)"                ~ "YEM",
          TRUE ~ countrycode(
            eez_name, origin = "country.name", destination = "iso3c"
          ))) %>%
      # Get rid of NEI (not elsewhere included EEZ's) - won't be used in analysis
      filter(eez_iso3c != "NEI") %>%
      # The only warning that is thrown is that countrycode fails to
      # standardize the names that we manually standardize here. So we take care
      # of this warning, despite it still showing up. We use suppressWarnings
      # to get rid of this issues.
      suppressWarnings()
    
    
    # Calculate the proportion of production for each sub-eez name within each
    # sovereign EEZ (used for weighted averaging)
    prod_props_eez <- mcp_combined_sp_eez_clean %>%
      distinct(eez_name, eez_iso3c) %>%
      left_join(prod_totals, by = "eez_name") %>%
      # The join will result in NA catch projection values for Ecuador.
      # This doesn't matter though because Ecuador has no sub-eez names in the
      # catch projection data, so we do not need to weighted average any of its
      # values and thus will impute 1, otherwise leave the values unchanged
      mutate(total_production = case_when(
        eez_name == "Ecuador" ~ 1,
        TRUE ~ total_production
      )) %>%
      # Workflow for calculating weighted averages:
      # Calculate the proportion of production for each sub-eez within a
      # sovereign EEZ
      group_by(eez_iso3c) %>%
      mutate(prop_prod = total_production / sum(total_production)) %>%
      ungroup() %>%
      select(eez_name, prop_prod)
    
    # Join production proportions at sub-eez level onto catch projection data,
    # calculating the weighted average at the sovereign level - this will be the
    # data we use for exposure
    mcp_sovereign <- mcp_combined_sp_eez_clean %>%
      left_join(prod_props_eez, by = "eez_name") %>%
      # Calculate weighted averages for catch projections for each sovereign EEZ
      mutate(ssp126 = ssp126 * prop_prod,
             ssp585 = ssp585 * prop_prod) %>%
      group_by(taxon_name, eez_iso3c) %>%
      summarize(ssp126 = sum(ssp126),
                ssp585 = sum(ssp585))
    
    # Write catch projection data to .parquet
    write_parquet(mcp_sovereign, "../data/exposure/mcp_sovereign.parquet")
    
    # Send messages to console
    cat("\n✅ mcp_sovereign.parquet written to ../data/exposure/\n")
    cat("Catch projection data preprocessing complete. Returning clean file.\n")
    
    # Return clean catch projection file
    return(mcp_sovereign)
    
  } else {
    
    # Read in clean catch projection file
    mcp_sovereign <- read_parquet("../data/exposure/mcp_sovereign.parquet")
    
    # Send message to console
    cat("📄 mcp_sovereign.parquet already exists. Skipping preprocessing to avoid overwriting the existing file. No need to re-run preprocess_catch_projection_data.R. Returning clean file.\n")
    
    # Return clean catch projection file
    return(mcp_sovereign)
  }
  
  
  
}