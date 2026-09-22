sens_ac_sensitivity_analysis <- function(input_df, analysis = c("sensitivity",
                                                              "adaptive_capacity_variables",
                                                              "adaptive_capacity_components")) { # Take in a dataframe of variables
  
  # Get measures
  
  if (analysis == "sensitivity") {
    var_list <- input_df %>% 
      drop_na() %>%
      filter(scenario == "ssp126") %>%
      select(consumer_iso3c, mc_reliance_pct, foreign_reliance_pct)
  } else {
    var_list <- input_df %>% 
      drop_na() %>%
      filter(scenario == "ssp126") %>%
      select(consumer_iso3c, gdp, gdp_trade, sanitation, supermarkets,
             life_expectancy, prop_labor, hci, gov_effectiveness, fsc, rol)
  }
  
  
  # Combine observed vulnerability FIXIT: Make dynamic with the input dataframe so that it matches the gapfilling method
  obs_vuln <- vuln_total_no_gapfill
  
  # Get input data ready
  input_data <- var_list %>%
    left_join(obs_vuln %>% select(consumer_iso3c, vulnerability), by = "consumer_iso3c")
  
  # Input: a list. The first item in the list will be the observed values to be simulated
  # The second item will be the e_s_ac data
  input_data <- list(input_data, input_df)
  
  # Acquire the number of variables contributing the observed value
  num_input_vars <- ncol(input_data[[1]]) - 2
  
  # Get last row of data (i.e., observed values) - for selecting column name
  obs_vals <- input_data[[1]] %>% select(last_col())
  
  # Create a matrix for sensitivity analyses to go into
  # First column: dirichlet assigned proportion
  # Second column: Kendall tau correlation between simulated & observed value
  # Third column: Target variable
  dir_probs_matrix <- matrix(ncol = 3)
  
  for (i in 1:num_input_vars) {
    
    dir_probs_matrix <- rbind(dir_probs_matrix, c(1,1,i)) # Set initial parameters
    
    
    # dir_probs <- rep(1 / num_input_vars, num_input_vars) # Set initial probabilities
    
    if (analysis == "sensitivity") {
      dir_alpha <- c(10000,10000) # Set initial dirichlet alpha values (used to proportion out)
    } else {
      dir_alpha <- c(rep(1/12, 9), 1/4) * 10000
    }
    
    dir_probs <- rdirichlet(n = 1, alpha = dir_alpha)

    if (analysis == "adaptive_capacity_components") {
      
      if (i != 10) {
        
        while (dir_probs[i] < 0.32) {
          
          if (i %in% c(1,4,7,10)) {
            
            sim_vals <- calculate_simulated_vulnerability(data = input_data[[2]],
                                                          analysis = analysis,
                                                          probs = dir_probs) %>% 
              select(colnames(obs_vals)) %>%
              suppressMessages()
            
            # Join simulated values to predicted values
            joined_vals <- left_join(input_data[[1]], sim_vals, by = "consumer_iso3c")
            
            r_kendall <- joined_vals %>%
              summarise(r = cor(pick(ends_with(".x")), pick(ends_with(".y")), method = "kendall")) %>%
              pull(r)
            
            dir_probs_matrix <- rbind(dir_probs_matrix, c(dir_probs[i],r_kendall, which(i == c(1,4,7,10))))
            
          }
          
          if (i %in% c(1,2,3)) {
            dir_alpha[c(1,2,3)] <- dir_alpha[c(1,2,3)] * 1.2
          } else if (i %in% c(4,5,6)) {
            dir_alpha[c(4,5,6)] <- dir_alpha[c(4,5,6)] * 1.2
          } else if (i %in% c(7,8,9)) {
            dir_alpha[c(7,8,9)] <- dir_alpha[c(7,8,9)] * 1.2
          } else if (i == 10) {
            dir_alpha[10] <- dir_alpha[10] * 1.2
          }
          
          dir_probs <- rdirichlet(n = 1, alpha = dir_alpha)
          
        }
        
      } else {
        
        while (dir_probs[i] < 0.99) {
          
          if (analysis == "adaptive_capacity_components" & i %in% c(1,4,7,10)) {
            
            sim_vals <- calculate_simulated_vulnerability(data = input_data[[2]],
                                                          analysis = analysis,
                                                          probs = dir_probs) %>% 
              select(colnames(obs_vals)) %>%
              suppressMessages()
            
            # Join simulated values to predicted values
            joined_vals <- left_join(input_data[[1]], sim_vals, by = "consumer_iso3c")
            
            r_kendall <- joined_vals %>%
              summarise(r = cor(pick(ends_with(".x")), pick(ends_with(".y")), method = "kendall")) %>%
              pull(r)
            
            dir_probs_matrix <- rbind(dir_probs_matrix, c(dir_probs[i],r_kendall, which(i == c(1,4,7,10))))
          
          }
          
          if (i %in% c(1,2,3)) {
            dir_alpha[c(1,2,3)] <- dir_alpha[c(1,2,3)] * 1.2
          } else if (i %in% c(4,5,6)) {
            dir_alpha[c(4,5,6)] <- dir_alpha[c(4,5,6)] * 1.2
          } else if (i %in% c(7,8,9)) {
            dir_alpha[c(7,8,9)] <- dir_alpha[c(7,8,9)] * 1.2
          } else if (i == 10) {
            dir_alpha[10] <- dir_alpha[10] * 1.2
          }
          
          dir_probs <- rdirichlet(n = 1, alpha = dir_alpha)
        
      }
      
      
      }
    } else {
      
      # Now that all null values are calculated, we will compare differing proportions of importance among combinations of variables
      while (dir_probs[i] < 0.99) {
        
        sim_vals <- calculate_simulated_vulnerability(data = input_data[[2]],
                                                      analysis = analysis,
                                                      probs = dir_probs) %>% 
          select(colnames(obs_vals)) %>%
          suppressMessages()
        
        # Join simulated values to predicted values
        joined_vals <- left_join(input_data[[1]], sim_vals, by = "consumer_iso3c")
        
        r_kendall <- joined_vals %>%
          summarise(r = cor(pick(ends_with(".x")), pick(ends_with(".y")), method = "kendall")) %>%
          pull(r)
        
        dir_probs_matrix <- rbind(dir_probs_matrix, c(dir_probs[i],r_kendall,i))
        
        
        
        if (analysis == "sensitivity") {
          dir_alpha[i] <- dir_alpha[i] + 10000
        } else if (analysis == "adaptive_capacity_variables") {
          dir_alpha[i] <- dir_alpha[i] * 1.2
        }
        
        dir_probs <- rdirichlet(n = 1, alpha = dir_alpha)
        
      }
      
    }
    
  }
  
  dir_probs_matrix <- dir_probs_matrix %>%
    data.frame() %>%
    drop_na() %>%
    rename(dirichlet_prob = X1, r_kendall = X2, var = X3)
  
  return(dir_probs_matrix)
  
}
