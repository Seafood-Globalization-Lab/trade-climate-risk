# Sensitivity Analysis
sensitivity_analysis <- function(data) {
  e_s_ac_long <- data %>% # Put e/s/ac data into long format
    mutate(across(c(gdp, gdp_trade, sanitation, 
                    supermarkets, life_expectancy, prop_labor, hci, gov_effectiveness,
                    fsc, rol), ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) ), .names = "{.col}_scaled")) %>% # scale variables using min/max scaling
    pivot_longer(cols = c("gdp_scaled", "gdp_trade_scaled", "sanitation_scaled", "supermarkets_scaled", "life_expectancy_scaled", "prop_labor_scaled", "hci_scaled", "gov_effectiveness_scaled", "fsc_scaled", "rol_scaled"), names_to = "ac_variable") %>%
    mutate(ac_component = case_when(ac_variable %in% c("gdp_scaled",
                                                       "gdp_trade_scaled",
                                                       "sanitation_scaled") ~
                                      "assets",
                                    ac_variable %in% c("supermarkets_scaled",
                                                       "life_expectancy_scaled",
                                                       "prop_labor_scaled") ~
                                      "flexibility",
                                    ac_variable %in% c("hci_scaled") ~ 
                                      "learning",
                                    ac_variable %in%
                                      c("gov_effectiveness_scaled",
                                        "fsc_scaled",
                                        "rol_scaled") ~ "organization"))
  
  
  #number of variables within each component
  ac_variables <- e_s_ac_long %>%
    group_by(ac_component) %>%
    distinct(ac_variable)
  
  ac_components <- e_s_ac_long %>%
    group_by(ac_component) %>%
    summarize(num_vars = n_distinct(ac_variable))
  
  # Get number of varibles within each component
  num_asse_vars <- ac_components[ac_components$ac_component == "assets", ]$num_vars
  num_flex_vars <- ac_components[ac_components$ac_component == "flexibility", ]$num_vars
  num_learn_vars <- ac_components[ac_components$ac_component == "learning", ]$num_vars
  num_org_vars <- ac_components[ac_components$ac_component == "organization", ]$num_vars
  
  # Set initial loop values
  pval = 1
  modifier = 0
  conditional_importance = 0.9
  for (i in ac_components$ac_component) {# iterate through each component
    
    # number of variables per adaptive capacity variable
    num_vars <- ac_components$num_vars[ac_components$ac_component == i]
    pval = 1
    for (j in 1:num_vars) { # Iterate through each variable
      # Reset modifier
      modifier = 0
      pval = 1
      conditional = 0
      while (conditional <= conditional_importance) {
        # Create the concentration vector with the correct number of 50's
        concentration <- rep(50, num_vars)
        concentration[j] <- concentration[j] + modifier
        
        # Generate 1000 samples from the Dirichlet distribution
        samples <- rdirichlet(1000, concentration)
        
        # Take a random sample from the Dirichlet distribution
        dirichlet_probs <- samples[sample(1:nrow(samples), 1), ]
        
        modifier <- modifier + 5
        conditional <- round((modifier + 50) / ((modifier + 50)+ (50 * (num_vars - 1))), 2)
        
        # assets
        if (i == "assets") {
          d01 <-  dirichlet_probs[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "gdp_scaled")]
          d02 <-  dirichlet_probs[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "gdp_trade_scaled")]
          d03 <-  dirichlet_probs[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "sanitation_scaled")]
          
        } else {
          d01 <- 1
          d02 <- 1
          d03 <- 1
        }
        
        
        # flexibility
        if (i == "flexibility") {
          d04 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "supermarkets_scaled")]
          d05 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "life_expectancy_scaled")]
          d06 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "prop_labor_scaled")]
        } else {
          d04 <- 1
          d05 <- 1
          d06 <- 1
        }
        
        
        # learning
        if (i == "learning") {
          d07 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "hci_scaled")]
        } else {
          d07 <- 1
        }
        
        
        # social organization
        if (i == "organization") {
          d08 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "gov_effectiveness_scaled")]
          d09 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "fsc_scaled")]
          d10 <- concentration[which(ac_variables[ac_variables$ac_component == i,]$ac_variable == "rol_scaled")]
        } else {
          d08 <- 1
          d09 <- 1
          d10 <- 1
        }
        
        
        
        ac_simulated <- e_s_ac_long %>%
          mutate(value_scaled = case_when(
            # Assets
            i == "assets" & 
              ac_variable == "gdp_scaled" ~ value * d01,
            i != "assets" & 
              ac_variable == "gdp_scaled" ~ value * 1 / num_asse_vars,
            
            i == "assets" & 
              ac_variable == "gdp_trade_scaled" ~ value * d02,
            i != "assets" & 
              ac_variable == "gdp_trade_scaled" ~ value * 1 / num_asse_vars,
            
            i == "assets" & 
              ac_variable == "sanitation_scaled" ~ value * d03,
            i != "assets" & 
              ac_variable == "sanitation_scaled" ~ value * 1 / num_asse_vars,
            
            # flexibility
            i == "flexilbity" & 
              ac_variable == "supermarkets_scaled" ~ value * d04,
            i != "flexilbity" & 
              ac_variable == "supermarkets_scaled" ~ value * 1 / num_flex_vars,
            
            i == "flexilbity" & 
              ac_variable == "life_expectancy_scaled" ~ value * d05,
            i != "flexilbity" & 
              ac_variable == "life_expectancy_scaled" ~ value * 1 / num_flex_vars,
            
            i == "flexilbity" & 
              ac_variable == "prop_labor_scaled" ~ value * d06,
            i != "flexilbity" & 
              ac_variable == "prop_labor_scaled" ~ value * 1 / num_flex_vars,
            
            # learning
            i == "learning" & 
              ac_variable == "hci_scaled" ~ value * d07,
            i != "learning" & 
              ac_variable == "hci_scaled" ~ value * 1 / num_learn_vars,
            
            # social organization
            i == "organization" & 
              ac_variable == "gov_effectiveness_scaled" ~ value * d08,
            i != "organization" & 
              ac_variable == "gov_effectiveness_scaled" ~ value * 1 / num_org_vars,
            
            i == "organization" & 
              ac_variable == "fsc_scaled" ~ value * d09,
            i != "organization" & 
              ac_variable == "fsc_scaled" ~ value * 1 / num_org_vars,
            
            i == "organization" & 
              ac_variable == "rol_scaled" ~ value * d10,
            i != "organization" & 
              ac_variable == "rol_scaled" ~ value * 1 / num_org_vars)
            
          ) %>%
          filter(scenario == "ssp126") %>%
          group_by(consumer_iso3c) %>%
          summarize(adaptive_capacity = sum(value_scaled),
                    adaptive_capacity = adaptive_capacity * 0.5) %>%
          filter(!is.na(adaptive_capacity))
        
        # Calculate simulated vulnerability
        risk_simulated <- left_join(e_s_ac_data %>%
                                      filter(scenario == "ssp126"), ac_simulated, by = "consumer_iso3c") %>%
          filter(!is.na(adaptive_capacity)) %>%
          mutate(across(c(pct_change, aa_reliance_pct,
                          foreign_dependency), ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) ), .names = "{.col}_scaled")) %>%
          group_by(consumer_iso3c) %>%
          mutate(aa_reliance_pct_scaled = aa_reliance_pct * 0.5,
                 foreign_dependency_scaled = foreign_dependency_scaled * 0.5,
                 sensitivity = sum(aa_reliance_pct_scaled,foreign_dependency_scaled, na.rm = TRUE)) %>%
          filter(!is.na(foreign_dependency_scaled),
                 !is.na(aa_reliance_pct_scaled)) %>%
          rename(exposure = "pct_change_scaled") %>%
          select(consumer_iso3c, exposure, sensitivity, adaptive_capacity) %>%
          mutate(adaptive_capacity = adaptive_capacity) %>%
          mutate(vulnerability = (exposure + sensitivity) - adaptive_capacity)
        
        correlation <- round(cor(risk_simulated$adaptive_capacity,
                                 risk$adaptive_capacity,
                                 method = "kendall"), 3)
      }
      
      if (which(i == ac_components$ac_component) == 1 & j == 1) {
        print(paste0("The following are Kendall Tau rank correlations between the null (even variable importance) and the simulated ranks of calculated adaptive capacity values given a `", conditional_importance * 100, "%` importance on the target adaptive capacity variable:"))
      }
      
      cat("\n")
      
      if (j == 1) {
        cat(sprintf("*** AC VARIABLE COMPONENT: %s ***\n", str_to_upper(i)))
      }
      
      cat(
        sprintf(
          "Variable `%s`:",
          ac_variables[ac_variables$ac_component == i, ]$ac_variable[j]
        ),
        correlation
      )
      
      if (j == num_vars & which(i == ac_components$ac_component) != 4) {
        cat("\n\n---\n")
      }
      
    }
  }
  
  # Set initial loop values
  pval = 1
  modifier = 0
  conditional_importance = 0.9
  for (i in ac_components$ac_component) {
    # iterate through each component
    # number of variables per adaptive capacity variable
    # Reset modifier
    pval = 1
    modifier = 0
    conditional = 0
    while (conditional <= conditional_importance) {
      
      
      # Create the concentration vector with the correct number of 50's
      concentration <- rep(50, 4)
      concentration[which(ac_components$ac_component == i)] <-
        concentration[which(ac_components$ac_component == i)] + modifier
      
      # Generate 1000 samples from the Dirichlet distribution
      samples <- rdirichlet(1000, concentration)
      
      # Take a random sample from the Dirichlet distribution
      dirichlet_probs <- samples[sample(1:nrow(samples), 1), ]
      
      modifier <- modifier + 5
      conditional <- round((modifier + 50) / ((modifier + 50) + (50 * (num_vars - 1))), 2)
      
      ac_simulated <- e_s_ac_long %>%
        mutate(value_scaled = case_when(
          # Assets,
          (i != "assets" | i == "assets") & 
            ac_variable == "gdp_scaled" ~ value * 1 / num_asse_vars,
          
          (i != "assets" | i == "assets") & 
            ac_variable == "gdp_trade_scaled" ~ value * 1 / num_asse_vars,
          
          (i != "assets" | i == "assets") & 
            ac_variable == "sanitation_scaled" ~ value * 1 / num_asse_vars,
          
          # flexibility,
          (i != "flexilbity" | i == "flexibility") & 
            ac_variable == "supermarkets_scaled" ~ value * 1 / num_flex_vars,
          
          (i != "flexilbity" | i == "flexibility") & 
            ac_variable == "life_expectancy_scaled" ~ value * 1 / num_flex_vars,
          
          (i != "flexilbity" | i == "flexibility") & 
            ac_variable == "prop_labor_scaled" ~ value * 1 / num_flex_vars,
          
          # learning
          (i != "learning" | i == "learning") & 
            ac_variable == "hci_scaled" ~ value * 1 / num_learn_vars,
          
          # social organization
          (i != "organization" | i == "organization") & 
            ac_variable == "gov_effectiveness_scaled" ~ value * 1 / num_org_vars,
          
          (i != "organization" | i == "organization") & 
            ac_variable == "fsc_scaled" ~ value * 1 / num_org_vars,
          
          (i != "organization" | i == "organization") & 
            ac_variable == "rol_scaled" ~ value * 1 / num_org_vars),
          
          # scale overall components of adaptive capacity
          value_scaled = case_when(
            i == "assets" &
              ac_component == "assets" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == i)],
            i != "assets" &
              ac_component == "assets" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == "assets")],
            
            i == "flexibility" &
              ac_component == "flexibility" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == i)],
            i != "flexibility" &
              ac_component == "flexibility" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == "flexibility")],
            
            i == "learning"  &
              ac_component == "learning" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == i)],
            i != "learning" & 
              ac_component == "learning"~ 
              value_scaled * dirichlet_probs[which(ac_components$ac_component == "learning")],
            
            i == "organization"  &
              ac_component == "organization" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == i)],
            i != "organization" & 
              ac_component == "organization" ~
              value_scaled * dirichlet_probs[which(ac_components$ac_component == "organization")],)
          
        ) %>%
        filter(scenario == "ssp126") %>%
        group_by(consumer_iso3c) %>%
        summarize(adaptive_capacity = sum(value_scaled),
                  adaptive_capacity = adaptive_capacity * 0.5) %>%
        filter(!is.na(adaptive_capacity))
      
      # Calculate simulated vulnerability
      risk_simulated <- left_join(e_s_ac_data, ac_simulated, by = "consumer_iso3c") %>%
        filter(!is.na(adaptive_capacity),
               scenario == "ssp126") %>%
        mutate(across(c(pct_change, aa_reliance_pct,
                        foreign_dependency), ~ ( . - min(., na.rm = TRUE) ) / ( max(., na.rm = TRUE) - min(., na.rm = TRUE) ), .names = "{.col}_scaled")) %>%
        group_by(consumer_iso3c) %>%
        mutate(aa_reliance_pct_scaled = aa_reliance_pct * 0.5,
               foreign_dependency_scaled = foreign_dependency_scaled * 0.5,
               sensitivity = sum(aa_reliance_pct_scaled,foreign_dependency_scaled, na.rm = TRUE)) %>%
        filter(!is.na(foreign_dependency_scaled),
               !is.na(aa_reliance_pct_scaled)) %>%
        rename(exposure = "pct_change_scaled") %>%
        select(consumer_iso3c, exposure, sensitivity, adaptive_capacity) %>%
        mutate(adaptive_capacity = adaptive_capacity) %>%
        mutate(vulnerability = (exposure + sensitivity) - adaptive_capacity)
      
      correlation <- round(cor(risk_simulated$adaptive_capacity,
                               risk$adaptive_capacity,
                               method = "kendall"), 3)
    }
    if (which(i == ac_components$ac_component) == 1) {
      print(paste0("The following are Kendall Tau rank correlations between the null (even variable importance) and the simulated ranks of calculated adaptive capacity values given a `", conditional_importance * 100, "%` importance on the target adaptive capacity component. Note that the individual importance between the other three components is '", round(((1 - conditional_importance) / 3) * 100, 1), "%`."))
    }
    
    cat("\n")
    
    cat(
      sprintf(
        "AC component `%s`:",
        i
      ),
      correlation
    )
  }
}