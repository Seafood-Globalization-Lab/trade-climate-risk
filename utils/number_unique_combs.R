number_unique_combs <- function(num_features) {
  
  # Get the possible combinations given the number of features
  combinations <- gtools::permutations(num_features, r = 2, repeats.allowed = FALSE)
  
  # The permutations funciton shows 
  combination_clean <- matrix(ncol = 2)
  
  # Iterate through each row of combinations
  for (i in 1:nrow(combinations)) {
    row_indexes <- combinations[i,]
    counter = 0
    
    
    # If the counter is found in the clean row, add to the counter
    # we don't want duplicates. This creates a measurement that will
    # be used to dismiss duplicate combinations
    for (j in 1:nrow(combination_clean)) {
      
      if (all(row_indexes %in% combination_clean[j,])) {
        counter = counter + 1
      }
      
    }
    
    # If there the combination is not found in the clean output data,
    # Add the new one in.
    if (counter == 0) {
      
      for (k in 1:nrow(combinations)) {
        
        if (all(row_indexes %in% combinations[k,]) & counter == 0) {
          
          combination_clean <- rbind(combination_clean, row_indexes)
          counter = counter + 1 # Only add in combination once
        } else {
          next # If not satisfied, exit out of this for loop
        }
        
      }
      
    }
    
    if (i == nrow(combinations)) {
      rownames(combination_clean) <- NULL
      combination_clean <- combination_clean[complete.cases(combination_clean),] # Remove NAs
      combination_clean <- matrix(combination_clean, ncol = 2) # Convert to matrix
    }
    
  }
  
  return(combination_clean)
  
}