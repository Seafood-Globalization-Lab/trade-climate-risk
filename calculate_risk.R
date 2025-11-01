calculate_risk <- function(data, coverage = "foreign imports") {
  # Calculate adaptive capacity across variables
  adaptive_capacity_calcs <- data %>%
    mutate(across(c(gdp, gdp_trade, sanitation, 
                    supermarkets, life_expectancy, prop_labor, hci, gov_effectiveness,
                    fsc, rol), ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) ), .names = "{.col}_scaled")) %>% # scale variables using min/max scaling
    mutate(gdp_scaled = gdp_scaled * (1/3), # Weight each AC variable within AC component (will add to 4 for country if each value is it's max value)
           gdp_trade_scaled = gdp_trade_scaled * (1/3),
           sanitation_scaled = sanitation_scaled * (1/3),
           supermarkets_scaled = supermarkets_scaled * (1/3),
           life_expectancy_scaled = life_expectancy_scaled * (1/3),
           prop_labor_scaled = prop_labor_scaled * (1/3),
           hci_scaled = hci * 1,
           gov_effectiveness_scaled = gov_effectiveness_scaled * (1/3),
           fsc_scaled = fsc_scaled * (1/3),
           rol_scaled = rol_scaled * (1/3)) %>%
    distinct(consumer_iso3c, .keep_all = TRUE) %>%
    group_by(consumer_iso3c) %>%
    mutate(adaptive_capacity = sum(gdp_scaled, gdp_trade_scaled,
                                   sanitation_scaled, supermarkets_scaled,
                                   life_expectancy_scaled,
                                   prop_labor_scaled,
                                   hci_scaled,
                                   gov_effectiveness_scaled,
                                   fsc_scaled,
                                   rol_scaled)) %>%
    mutate(adaptive_capacity = adaptive_capacity * 0.5) %>% # Divide by two, meaning max value would now = two, which would nullify max level expsorue + sensitivity
    filter(!is.na(adaptive_capacity)) %>%
    select(consumer_iso3c, adaptive_capacity)
  
  
  # Calculate vulernability (Risk = Exposure + Risk - Adaptive Capacity)
  risk <- left_join(data %>% filter(scenario == "ssp126"), adaptive_capacity_calcs, by = "consumer_iso3c") %>%
    filter(!is.na(adaptive_capacity)) %>%
    mutate(across(c(pct_change, aa_reliance_pct), ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) ), .names = "{.col}_scaled")) %>%
    group_by(consumer_iso3c) %>%
    mutate(aa_reliance_pct_scaled = aa_reliance_pct,
           sensitivity = aa_reliance_pct_scaled) %>%
    filter(!is.na(aa_reliance_pct_scaled)) %>%
    rename(exposure = "pct_change_scaled") %>%
    select(consumer_iso3c, exposure, sensitivity, adaptive_capacity) %>%
    mutate(adaptive_capacity = adaptive_capacity) %>%
    mutate(vulnerability = (exposure + sensitivity) - adaptive_capacity)
  
  risk <- risk %>%
    add_region(col = "consumer_iso3c", region.col.name = "region") %>%
    arrange(region, .after = "consumer_iso3c") %>%
    mutate(coverage = coverage)
  
  return(risk)
}
