acquire_e_s_ac_data <- function(foreign = TRUE , directory_path  = "") {
  
  ###############################################################################
  #####                        Initialize script                             #####
  ###############################################################################
  
  if (foreign == TRUE) {
    string = "foreign imports"
  } else {
    string = "total"
  }
  
  cat("\n==============================\n")
  cat("Acquiring exposure, sensitivity, and adaptive capacity data for each countries' **", string, "** portfolio\n")
  cat("==============================\n\n")
  
  ###############################################################################
  #####                        Load in packages                             #####
  ###############################################################################
  
  # General data cleaning + visualization + analysis packages
  library(tidyverse)
  library(ggformula)
  library(countrycode)
  library(data.table)
  library(arrow)
  library(cowplot)
  library(RColorBrewer)
  
  # Map creation
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  
  # Load in packages needed for ARTIS
  library(devtools)
  library(tidytext)
  # devtools::install_github("davidsjoberg/ggsankey")
  library(ggsankey)
  # devtools::install_github("Seafood-Globalization-Lab/exploreARTIS@v1.0.0", dependencies = TRUE)
  library(exploreARTIS)
  
  ###############################################################################
  #####                  Preprocess future climate data                     #####
  ###############################################################################
  # file_names <- list.files(path = paste0(directory_path, "exposure/mcp_per_change/mcp_per_change/"), pattern = "\\.csv", full.names = TRUE)
  # 
  # # Obtain species lookup key (will be used in loop join)
  # species_names <- fread(paste0(directory_path,"exposure/dbem_spp_list.csv"))
  # 
  # # initiate empty df
  # df <- data.frame()
  # 
  # for (i in 1:length(file_names)) {
  #   # Read in i'th species file in loop
  #   df_i <- fread(file_names[i])
  # 
  #   df_i <- left_join(df_i, species_names, by = "taxon_key")
  # 
  #   # Select only certain variables for certain years and pivot wider
  #   df_i <- df_i %>%
  #     mutate(taxon_name = str_to_lower(taxon_name)) %>%
  #     select(taxon_name, eez_name, ssp, per_change) %>%
  #     pivot_wider(names_from = ssp, values_from = per_change)
  # 
  #   # Combine all species observations into one file
  #   df <- df %>%
  #     bind_rows(df_i)
  # }
  # 
  # # Obtain previous state on dataframe (to get unmatched)
  # df_preprocessed <- df
  # 
  # # Convert eez variable countries to iso3c codes
  # for (i in 1:length(df$eez_name)) {
  #   df$eez_name[i] <- countrycode(df$eez_name[i], origin = 'country.name', destination = 'iso3c')
  # }
  # 
  # # Rename country variable so it can be joined
  # df <- df %>%
  #   rename(eez_iso3c = "eez_name",
  #          sciname = "taxon_name")
  # 
  # # Write joined future cimate data to .csv
  # write_csv(df, "../output/future_climate_joined.csv")
  # 
  # # # See how many countries were not matches
  # sum(is.na(df$eez_iso3c))
  # 
  # # # Number of countries that properly got matched
  # length(unique(df$eez_iso3c))
  # 
  # # # Unmatched countries via countrycode
  # unmatched_countries <- unique(df_preprocessed$eez_name[c(which(is.na(df$eez_iso3c)))])
  # 
  # # # Only keep rows in preprocessed dataset that are unmatched countries
  # df_preprocessed <- df_preprocessed %>%
  #   filter(eez_name %in% unmatched_countries)
  # 
  # # Territories that were not matched via countrycode (Tokelau & Svalbard Isl.)
  # df_preprocessed %>% distinct(eez_name)
  
  ###############################################################################
  #####                Load in consumption & sciname data                   #####
  ###############################################################################
 
  # Read in cosnumption data
  consumption <- read_parquet(paste0(directory_path,"example_consumption_eez_2024_12_06.parquet")) %>%
    mutate(sciname_hs_modified = case_when(
      is.na(sciname_hs_modified) ~ sciname,
      TRUE ~ sciname_hs_modified
    )) # Get rid of NA scinames
  
  # Read in sciname data (for joining to scinames of future climate)
  sciname <- read_csv(paste0(directory_path,"sciname.csv"))
  
  ###############################################################################
  #####         Look at how many consumed ARTIS species are invasive        #####
  ###############################################################################
  
  # Load in function for identifying which ARTIS species are invasive
  source("../R/get_invasives.R")
  
  # Get ARTIS scientific names
  artis_scinames <- sciname %>%
    distinct(sciname) %>%
    pull(sciname)
  
  # Get invasive list from fb/slb - last date acquired: 8/27/2025
  # fb_introductions <- rfishbase::introductions(server = "fishbase")
  # write_parquet(fb_introductions, "../data/fb_slb_data/fb_introductions.parquet")
  
  # ... same thing but for sealifebase - last date acquired: 8/27/2025
  # slb_introductions <- rfishbase::introductions(server = "sealifebase")
  # write_parquet(slb_introductions, "../data/fb_slb_data/slb_introductions.parquet")
  
  # Read in introductions data
  fb_introductions <- read_parquet(paste0(directory_path,"fb_slb_data/fb_introductions.parquet"))
  slb_introductions <- read_parquet(paste0(directory_path, "fb_slb_data/slb_introductions.parquet"))
  
  # Get fb/slb species codes
  fb_species_codes <- read_parquet(paste0(directory_path, "fb_slb_data/fb_species_codes.parquet"))
  slb_species_codes <- read_parquet(paste0(directory_path, "fb_slb_data/slb_species_codes.parquet"))
  
  # Import file for correcting fb/slb territory names to iso3c 
  territory_corrections <- read_csv(paste0(directory_path, "fb_slb_data/fb_slb_territory_corrections.csv")) %>%
    select(c(fb_slb_territories_iso3c, fb_slb_unmatched_territories, "associated_territory_country_iso3c" = "associated_country_iso3c...7", "associated_country_iso3c" = "associated_country_iso3c...5"))
  
  sciname_corrections <- read_csv(paste0(directory_path, "fb_slb_data/fb_slb_territory_corrections.csv")) %>%
    select(fb_slb_scientific_name, new_sciname)
  
  # Get invasive species for fishbase and sealifebase
  invasives <- get_invasives(introductions_data = fb_introductions, species_codes_data = fb_species_codes)
  
  # Write to parquet
  write_parquet(invasives, "../output/invasives.parquet")
  
  ###############################################################################
  #####               Disaggregate future climate measurements              #####
  ###############################################################################
  # ---- Compute species-level averages ----
  df1 <- df %>%
    drop_na() %>%
    group_by(eez_iso3c, sciname) %>%
    summarise(
      ssp126 = mean(ssp126, na.rm = TRUE),
      ssp585 = mean(ssp585, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- Prepare taxonomic data ----
  sciname_taxa <- sciname %>%
    mutate(
      taxa_level = case_when(
        sciname == kingdom ~ "kingdom",
        sciname == phylum ~ "phylum",
        sciname == superclass ~ "superclass",
        sciname == class ~ "class",
        sciname == order ~ "order",
        sciname == family ~ "family",
        sciname == subfamily ~ "subfamily",
        sciname == genus ~ "genus",
        str_detect(sciname, " ") ~ "species"
      ),
      species = case_when(
        str_count(sciname, " ") == 1 ~ word(sciname, 2),
        TRUE ~ NA_character_
      )
    ) %>%
    relocate(species, .before = genus) %>%
    select(-common_name, -isscaap)
  
  # ---- Filter consumption data to 2019 and join ----
  consumption_future <- consumption %>%
    filter(year == 2019)
  
  consumption_sciname <- left_join(consumption_future, sciname_taxa,
                                   by = c("sciname_hs_modified" = "sciname"))
  
  # Join with future climate data (species level)
  test_data <- left_join(consumption_sciname, df1,
                         by = c("eez_iso3c", "sciname_hs_modified" = "sciname"))
  
  # ---- Function to compute taxa averages ----
  compute_taxa_averages <- function(df, tax_level) {
    df %>%
      filter(!is.na(ssp126) | !is.na(ssp585)) %>%
      group_by(eez_iso3c, !!sym(tax_level)) %>%
      summarise(
        avg_ssp126 = mean(ssp126, na.rm = TRUE),
        avg_ssp585 = mean(ssp585, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(taxa_name = !!sym(tax_level)) %>%
      mutate(taxa_level = tax_level)
  }
  
  # ---- Compute averages for each taxa level ----
  averages_list <- list(
    compute_taxa_averages(test_data, "genus"),
    compute_taxa_averages(test_data, "subfamily"),
    compute_taxa_averages(test_data, "family"),
    compute_taxa_averages(test_data, "order"),
    compute_taxa_averages(test_data, "class"),
    compute_taxa_averages(test_data, "phylum")
  )
  
  lookup_table <- bind_rows(averages_list)
  
  # ---- Function to fill missing values hierarchically ----
  fill_with_higher_taxa <- function(df, lookup_table) {
    tax_levels <- c("genus", "subfamily", "family", "order", "class", "phylum")
    
    for (level in tax_levels) {
      df <- df %>%
        left_join(
          lookup_table %>% filter(taxa_level == level),
          by = c("eez_iso3c", level = "taxa_name")
        ) %>%
        mutate(
          ssp126 = if_else(is.na(ssp126), avg_ssp126, ssp126),
          ssp585 = if_else(is.na(ssp585), avg_ssp585, ssp585)
        ) %>%
        select(-avg_ssp126, -avg_ssp585)
    }
    
    return(df)
  }
  
  # ---- Apply hierarchical filling ----
  interpolated_future_data <- fill_with_higher_taxa(test_data, lookup_table)
  
  ###############################################################################
  ##### Derive exposure measurements via disaggregated future climate data  #####
  ###############################################################################
  
  ### BRINGING IN interpolated_future_data from 98_Testing_Future_Catch_Disaggregation ###
  
  # Join future data on consumption data
  consumption_future <- left_join(interpolated_future_data, df1, by = c("sciname_hs_modified" = "sciname", "eez_iso3c"))
  
  # Pivot dataset to longer to make multiscenario visualization easier
  consumption_future <- interpolated_future_data %>%
    pivot_longer(cols = c(`ssp126`,
                          `ssp585`), 
                 names_to = "scenario",
                 values_to = "future_change_catch")

  if (foreign == TRUE) {
    # Get % change in stock composition by country
    consumer_stock_change_pre <- consumption_future %>% 
      filter(consumer_iso3c != producer_iso3c, # Only include importing countries; drop NAs
             !is.na(live_weight_t),
             !is.na(future_change_catch)) %>%
      # filter(!is.na(live_weight_t),
      #        !is.na(future_change_catch)) %>% # Gives the entire portfolio
      mutate(future_change_catch = case_when( # Convert percentage to proportion
        future_change_catch > 0 ~ (future_change_catch / 100) + 1,
        future_change_catch < 0 ~ 1 - ((future_change_catch / 100) * -1), 
        future_change_catch == 0 ~ 1)) %>%
      # group_by(consumer_iso3c, scenario, sciname_hs_modified) %>%
      mutate(change_in_stock = live_weight_t * future_change_catch)
  } else {
    # Get % change in stock composition by country
    consumer_stock_change_pre <- consumption_future %>% 
      # filter(consumer_iso3c != producer_iso3c, # Only include importing countries; drop NAs
      #        !is.na(live_weight_t),
      #        !is.na(future_change_catch)) %>%
      filter(!is.na(live_weight_t),
             !is.na(future_change_catch)) %>% # Gives the entire portfolio
      mutate(future_change_catch = case_when( # Convert percentage to proportion
        future_change_catch > 0 ~ (future_change_catch / 100) + 1,
        future_change_catch < 0 ~ 1 - ((future_change_catch / 100) * -1), 
        future_change_catch == 0 ~ 1)) %>%
      # group_by(consumer_iso3c, scenario, sciname_hs_modified) %>%
      mutate(change_in_stock = live_weight_t * future_change_catch)
  }
  
  
  
  # Use future weights to calculate % change in weight by country
  consumer_stock_change <- consumer_stock_change_pre %>% 
    group_by(consumer_iso3c, scenario) %>%
    summarize(change_in_stock = sum(change_in_stock),
              live_weight_t = sum(live_weight_t)) %>%
    mutate(pct_change = 100 * ((change_in_stock - live_weight_t) / live_weight_t))
  
  # Write to csv
  write_csv(consumer_stock_change, "../output/consumer_stock_change.csv")
  
  # Remove previous objects to keep data usage small
  rm(df1, interpolated_future_data, consumer_stock_change_pre)
  gc() # Garbage collection to clean up storage
  
  ###############################################################################
  #####                   Sensitivity data acuqisition                      #####
  ###############################################################################
  
  # Read in supply importance data
  supply_importance <- read_csv(paste0(directory_path, "sensitivity/supply_importance.csv"))
  
  if (foreign == TRUE) {
    # Calculate aquatic animal reliance from foreign and domestic sources
    aa_reliance <- supply_importance %>% # Add in fao sensitivity data
      filter(year == 2019,
             habitat == "marine",
             method == "capture",
             food_group == "aquatic") %>%
      group_by(consumer_iso3c) %>% # Sum across domestic / foreign consumption
      summarize(aa_reliance_pct = sum(aa_reliance_pct)) %>%
      ungroup()
  } else {
    # Calculate aquatic animal reliance from foreign sources
    aa_reliance <- supply_importance %>% # Add in fao sensitivity data
      filter(year == 2019,
             habitat == "marine",
             method == "capture",
             food_group == "aquatic",
             consumption_source == "foreign") # No need to summarize / aggregate since this this data we want in disaggregated form.
  }
  
  # Remove previous objects to keep data usage small
  rm(supply_importance)
  gc() # Garbage collection to clean up storage
  
  ###############################################################################
  #####                Adaptive capacity data acuqisition                   #####
  ###############################################################################
  
  # Get ARTIS consuming countries (For identifying data coverage)
  # changed consumption to consumption_future
  consuming_countries <- consumption_future %>%
    filter(consumer_iso3c != producer_iso3c) %>%
    distinct(consumer_iso3c) %>%
    mutate(artis = 1)
  
  # Function for generalizing acquisition of some similar structure adaptive capacity datasets
  country_coverage <- function(data2, join_by = "") {
    # Full join ARTIS countries + AC countries
    data <- full_join(consuming_countries, data2 %>% 
                        mutate(gdp_coverage = 1), 
                      by = c("consumer_iso3c" = join_by)) %>%
      filter(is.na(gdp_coverage) & artis == 1)
    
    # Look at which countries are missing
    missing_countries <- data %>%
      pull(consumer_iso3c)
    
    return(missing_countries)
  }
  
  ##########
  # ASSETS #
  ##########
  
  # sanitation
  sanitation <- read_csv(paste0(directory_path,"adaptive capacity/assets/sanitation.csv"))
  
  sanitation_clean <- sanitation %>%
    filter(Indicator_Code == "SH_STA_BASS_ZS", Time_Period == 2019)
  
  a <- country_coverage(sanitation_clean, join_by = "Geography_Code")
  
  # gdp
  gdp <- read_csv(paste0(directory_path,"adaptive capacity/assets/gdp.csv"))
  
  gdp_clean <- gdp %>%
    select(`Country Code`, `2019`) %>%
    filter(!is.na(`2019`))
  
  b <- country_coverage(gdp_clean, join_by = "Country Code")
  
  # trade standardized by gdp
  trade_gdp <- read_csv(paste0(directory_path,"adaptive capacity/assets/trade_gdp.csv"))
  
  trade_gdp_clean <- trade_gdp %>%
    filter(`Indicator Code` == "NE.TRD.GNFS.ZS") %>%
    select(`Country Code`,`2019`) %>%
    filter(!is.na(`2019`))
  
  c <- country_coverage(trade_gdp_clean, join_by = "Country Code")
  
  ###############
  # FLEXIBILITY #
  ###############
  
  # life expectancy
  life_expectancy <- read_csv(paste0(directory_path,"adaptive capacity/flexibility/life_expectancy_at_birth.csv"))
  
  life_expectancy_clean <- life_expectancy %>%
    filter(`Indicator Code` == "SP.DYN.LE00.IN") %>%
    select(`Country Code`,`2019`) %>%
    filter(!is.na(`2019`))
  
  d <- country_coverage(life_expectancy_clean, join_by = "Country Code")
  
  # supermarkets per 100000
  supermarkets <- read_csv(paste0(directory_path,"adaptive capacity/flexibility/supermarkets_per_100000.csv"))
  
  supermarkets_clean <- supermarkets %>%
    mutate(iso3c = countrycode(Region, origin = 'country.name', destination = 'iso3c')) %>%
    select(iso3c, `2019`) %>%
    filter(!is.na(2019))
  
  e <- country_coverage(supermarkets_clean, join_by = "iso3c")
  
  # prop labor force
  total_labor_force <- read_csv(paste0(directory_path,"adaptive capacity/flexibility/total_labor_force.csv"))
  total_population <- read_csv(paste0(directory_path,"adaptive capacity/flexibility/total_population.csv"))
  
  total_population_clean <- total_population %>%
    select(`Country Code`, `2019`) %>%
    rename(total_pop_2019 = "2019")
  
  prop_population_clean <- left_join(total_labor_force, total_population_clean, by = "Country Code") %>%
    select(`Country Code`, `2019`, total_pop_2019) %>%
    mutate(prop_labor = `2019` / total_pop_2019) %>%
    filter(!is.na(prop_labor))
  
  f <- country_coverage(prop_population_clean, join_by = "Country Code")
  
  ############
  # LEARNING #
  ############
  
  # Human capital index
  hci <- read_csv(paste0(directory_path,"adaptive capacity/learning/HCIData.csv"))
  
  hci_clean <- hci %>%
    filter(`Indicator Name` == "Human Capital Index (HCI) (scale 0-1)") %>%
    select(`Country Code`, `2018`) %>%
    filter(!is.na(`2018`))
  
  h <- country_coverage(hci_clean, join_by = "Country Code")
  
  #######################
  # SOCIAL ORGANIZATION #
  #######################
  
  # government effectiveness
  government_effectiveness <- read_csv(paste0(directory_path,"adaptive capacity/social organization/government_effectiveness_percentile.csv"))
  
  government_effectiveness_clean <- government_effectiveness %>%
    filter(`Indicator ID` == "WB.WWGI.GE.PER.RNK") %>%
    select(`Economy ISO3`,`2019`) %>%
    filter(!is.na(`2019`))
  
  i <- country_coverage(government_effectiveness_clean, join_by = "Economy ISO3")
  
  # Food safety capacity
  fsc <- read_csv(paste0(directory_path,"adaptive capacity/social organization/food-systems-dashboard-2025-03-04.csv"))
  
  fsc_clean <- fsc %>%
    filter(`Start Year` == 2019 | `End Year` == 2019)
  
  j <- country_coverage(fsc_clean, join_by = "ISO3")
  
  # Rule of law
  rol <- read_csv(paste0(directory_path,"adaptive capacity/social organization/WB-WWGI.csv"))
  
  rol_clean <- rol %>% 
    filter(Indicator == "Rule of Law: Percentile Rank") %>%
    select(`Economy ISO3`, `Indicator`, `2019`)
  
  k <- country_coverage(rol_clean, join_by = "Economy ISO3")
  
  # Count total number of ARTIS countries missing across adaptive capacity sources
  length(unique(c(a,b,c,d,e,f,h,i,j,k)))
  
  #########################################################
  ##  Prepare AC data so that it can be joined together  ##
  #########################################################
  
  # Assets
  gdp_clean <- gdp_clean %>%
    rename(gdp = "2019", consumer_iso3c = "Country Code")
  
  trade_gdp_clean <- trade_gdp_clean %>%
    rename(gdp_trade = "2019", consumer_iso3c = "Country Code")
  
  sanitation_clean <- sanitation_clean %>%
    rename(sanitation = "Value", consumer_iso3c = "Geography_Code") %>%
    select(sanitation, consumer_iso3c)
  
  
  # Flexibility
  supermarkets_clean <- supermarkets_clean %>%
    rename(supermarkets = "2019", consumer_iso3c = "iso3c")
  
  life_expectancy_clean <- life_expectancy_clean %>%
    rename(life_expectancy = "2019", consumer_iso3c = "Country Code")
  
  prop_population_clean <- prop_population_clean %>%
    rename(consumer_iso3c = "Country Code") %>%
    select(consumer_iso3c, prop_labor)
  
  # Learning
  
  # Human capacity index
  hci_clean <- hci_clean %>%
    rename(hci = "2018", consumer_iso3c = "Country Code")
  
  # Social organization
  government_effectiveness_clean <- government_effectiveness_clean %>%
    rename(gov_effectiveness = "2019", consumer_iso3c = "Economy ISO3")
  
  fsc_clean <- fsc_clean %>%
    rename(fsc = "Value", consumer_iso3c = "ISO3") %>%
    select(consumer_iso3c, fsc)
  
  
  rol_clean <- rol_clean %>%
    rename(rol = "2019", consumer_iso3c = "Economy ISO3") %>%
    select(consumer_iso3c, rol)
  
  # Join all adaptive capacity data sources
  adaptive_capacity <- left_join(gdp_clean, trade_gdp_clean) %>%
    left_join(sanitation_clean) %>%
    left_join(supermarkets_clean) %>%
    left_join(life_expectancy_clean) %>%
    left_join(prop_population_clean) %>%
    left_join(hci_clean) %>%
    left_join(government_effectiveness_clean) %>%
    left_join(fsc_clean) %>%
    left_join(rol_clean)
  
  ###############################################################################
  #####   Combine exposure, sensitivity, and adaptive capacity  components  #####
  ###############################################################################
  
  e_s_ac_data <- adaptive_capacity %>%
    right_join(aa_reliance) %>% # Right join to only include consuming ARTIS countries (Aquatic animal reliance Sensitivity)
    left_join(consumer_foreign_dependencies %>% # Add in foreign dependency sensitivity
                filter(year == 2019) %>%
                ungroup() %>%
                select(consumer_iso3c, foreign_dependency)) %>%
    left_join(consumer_stock_change) # Add in exposure data
  
  # Save joined data to output folder
  write_parquet(e_s_ac_data, "../output/e_s_ac_data.parquet")
  
  cat("\n==============================\n")
  cat("Saving exposure, sensitivity, and adaptive capacity of each country's **", string, "** portfolio to file.\n", sep = "")
  cat("File successfully saved! ✅\n")
  cat("==============================\n\n")
  
  # Return combined exposure, sensitivty, and adaptive capacity dataset
  return(e_s_ac_data)
  
  # Remove previous objects to keep data usage small
  rm(a, b, c, d, e, f, h, i, j, k, aa_reliance, ac_coverage,
     consumer_foreign_dependencies, consumer_stock_change, consuming_countries, consumption_future, fsc, fsc_clean,
     gdp, gdp_clean, government_effectiveness, government_effectiveness_clean, hci, hci_clean, life_expectancy, life_expectancy_clean, missing_artis_ac_countries, prop_population_clean, rol, rol_clean, sanitation, sanitation_clean, supermarkets, supermarkets_clean, total_labor_force, total_population, total_population_clean, trade_gdp, trade_gdp_clean, variable_coverage)
  gc() # Garbage collection to clean up storage
  
}
