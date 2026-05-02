exp_sensitivity_analysis <- function(exp_list) {
  
  # Calculate the number of methods used for calculation exposure
  num_methods <-  exp_list %>% length()
  
  # Get the different unique combinations to run for comparison
  combination_clean <- number_unique_combs(num_methods)
  
  # Create a matrix for sensitivity analyses to go into
  dir_probs_matrix <- matrix(ncol = 3)
  
  # Run a sensitivity analysis for each comparison of exposure measurements
  for (i in 1:nrow(combination_clean)) {
    
    # Column 1: dirichlet proportion of importance for method 1
    # Column 2: dirichlet proportion of importance for method 2
    # Kendall tau correlation
    # Comparison: 
    
    # Get combination of exposure measurements to use in current sensitivity analysis
    combination_i <- combination_clean[i,]
    
    # Get exposure measurements for analysis i
    exposures_i <- list(exp_list[[combination_i[1]]], exp_list[[combination_i[2]]])
    
    # Compute a weighted average of all exposure measurements
    # based on the number of methods used. This is an even
    # weighted average. These will be the *null* exposures
    null_exp <- sapply(exposures_i, function(x) x * 0.5) %>% rowSums()
    
    
    dir_probs_matrix <- rbind(dir_probs_matrix, c(0.5,1,i)) # Set initial parameters
    
    dir_probs <- c(0.5,0.5) # Set initial probabilities
    dir_alpha <- c(10000,10000) # Set initial dirichlet alpha values (used to proportion out)
    
    # Now that all null values are calculated, we will compare differing proportions of importance among combinations of variables
    while (all(dir_probs < 0.99)) {
      dir_alpha[1] <- dir_alpha[1] + 100
      dir_probs <- rdirichlet(n = 1, alpha = dir_alpha)
      
      # Simulate exposure measurement with greater level of importance
      sim_1 <- dir_probs[1] * exposures_i[[1]]
      sim_2 <- dir_probs[2] * exposures_i[[2]]
      
      sim_3 <- dir_probs[1] * exposures_i[[2]]
      sim_4 <- dir_probs[2] * exposures_i[[1]]
      
      sim_exp1 <- sim_1 + sim_2
      sim_exp2 <- sim_3 + sim_4
      
      r_kendall <- cor(sim_exp1, sim_exp2, method = "kendall")
      
      dir_probs_matrix <- rbind(dir_probs_matrix, c(dir_probs[1],r_kendall,i))
      
    }
    
    
    
  }
  
  dir_probs_matrix <- dir_probs_matrix %>%
    data.frame() %>%
    drop_na() %>%
    rename(dirichlet_prob = X1, r_kendall = X2, combination = X3)
  
  return(dir_probs_matrix)
  
}

