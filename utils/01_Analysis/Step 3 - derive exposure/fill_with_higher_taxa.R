# Function name: fill_with_higher_taxa.R

# Function purpose: conduct gap filling based on selected method

fill_with_higher_taxa <- function(df,
                                  gap_filling_method,
                                  averaging,
                                  ecoregion,
                                  num_species,
                                  assumed_sd) {
  
  # ---------------------------------------------------------------------------
  # Set up lookup table
  # ---------------------------------------------------------------------------
  
  if (gap_filling_method == "bayesian") {
    
    lookup_dir <- file.path(
      "../data/exposure/lookup_tables/bayesian",
      paste0(assumed_sd, "_sd")
    )
    
    lookup_file <- file.path(lookup_dir, "lookup_table.rds")
    
    build_lookup <- function() {
      build_bayesian_lookup(
        df = df,
        ecoregion = ecoregion,
        assumed_sd = assumed_sd
      )
    }
    
  } else {
    
    lookup_dir <- file.path(
      "../data/exposure/lookup_tables",
      paste0(num_species, "_species")
    )
    
    lookup_file <- file.path(lookup_dir, "lookup_table.rds")
    
    build_lookup <- function() {
      build_taxa_lookup(
        df = df,
        ecoregion = ecoregion,
        num_species = num_species
      )
    }
  }
  
  # Create directory if needed
  dir.create(lookup_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Load existing lookup table or build a new one
  if (file.exists(lookup_file)) {
    
    lookup_table <- readRDS(lookup_file)
    
  } else {
    
    lookup_table <- build_lookup()
    saveRDS(lookup_table, lookup_file)
  }
  
  cat("✔️ Lookup table loaded in\n")
  
  # Set taxonomic levels to iterate through
  tax_levels <- c("genus", "family", "order", "class", "phylum", "kingdom")
  
  # Create empty variables that will be filled as gap filling happens
  out_df <- df %>% mutate(gapfill_level = NA_character_,
                          gapfill_method = NA_character_,
                          cv_ssp126 = NA,
                          cv_ssp585 = NA)
  
  # ---------------------------------------------------------------------------
  # EEZ-level gap filling
  # ---------------------------------------------------------------------------
  if ((gap_filling_method != "hierarchical" & gap_filling_method != "diagonal" & gap_filling_method != "bayesian")) {
    
    for (level in tax_levels) {
      
      join_by_map <- setNames("taxa_name", level)
      
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = "eez",
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = level,
                                        join_by_map = join_by_map)
      
    }
    
    # ---------------------------------------------------------------------------
    # Realm-level gap filling
    # ---------------------------------------------------------------------------
    
    if (gap_filling_method == "realm") {
      
      for (level in tax_levels) {
        join_by_map <- setNames("taxa_name", level) 
        
        out_df <- fill_higher_taxa_helper(out_df,
                                          gap_filling_method = "realm",
                                          averaging = averaging,
                                          lookup_table = lookup_table,
                                          ecoregion = ecoregion,
                                          level = level,
                                          join_by_map = join_by_map)
      }
      
    }
    
    
    # ---------------------------------------------------------------------------
    # Region-level gap filling
    # ---------------------------------------------------------------------------
    if (gap_filling_method == "region") {
      
      for (level in tax_levels) {
        join_by_map <- setNames("taxa_name", level) 
        
        out_df <- fill_higher_taxa_helper(out_df,
                                          gap_filling_method = "region",
                                          averaging = averaging,
                                          lookup_table = lookup_table,
                                          ecoregion = ecoregion,
                                          level = level,
                                          join_by_map = join_by_map)
      }
      
    }
    
    # ---------------------------------------------------------------------------
    # Realm --> Region --> Global gapfilling 
    # ---------------------------------------------------------------------------
    if (gap_filling_method == "thorough") {
      
      for (level in tax_levels) {
        
        join_by_map <- setNames("taxa_name", level) 
        
        # 1. Attempt realm-level gap fills
        out_df <- fill_higher_taxa_helper(out_df,
                                          gap_filling_method = "realm",
                                          averaging = averaging,
                                          lookup_table = lookup_table,
                                          ecoregion = ecoregion,
                                          level = level,
                                          join_by_map = join_by_map)
        
      }
      
      for (level in tax_levels) {
        
        join_by_map <- setNames("taxa_name", level) 
        
        # 2. Attempt region-level gap fills
        out_df <- fill_higher_taxa_helper(out_df,
                                          gap_filling_method = "region",
                                          averaging = averaging,
                                          lookup_table = lookup_table,
                                          ecoregion = ecoregion,
                                          level = level,
                                          join_by_map = join_by_map)
        
      }
      
      for (level in tax_levels) {
        
        join_by_map <- setNames("taxa_name", level) 
        
        # 2. Attempt region-level gap fills
        out_df <- fill_higher_taxa_helper(out_df,
                                          gap_filling_method = "global",
                                          averaging = averaging,
                                          lookup_table = lookup_table,
                                          ecoregion = ecoregion,
                                          level = level,
                                          join_by_map = join_by_map)
        
      }
      
    }
    
  }
  
  # ---------------------------------------------------------------------------
  # Hierarchical gapfilling 
  # ---------------------------------------------------------------------------
  if (gap_filling_method == "hierarchical") {
    
    for (level in tax_levels) {
      join_by_map <- setNames("taxa_name", level)
      
      # 1. Attempt EEZ-level gap fills
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = "eez",
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = level,
                                        join_by_map = join_by_map)
      
      # 2. Attempt realm-level gap fills
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = "realm",
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = level,
                                        join_by_map = join_by_map)
      
      # 3. Attempt region-level gap fills
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = "region",
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = level,
                                        join_by_map = join_by_map)
      
      # 4. Attempt global-level gap fills
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = "global",
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = level,
                                        join_by_map = join_by_map)
    }
    
  }
  
  
  
  # ---------------------------------------------------------------------------
  # Diagonal gapfilling (frequentist & Bayesian)
  # ---------------------------------------------------------------------------
  if (gap_filling_method == "diagonal" | gap_filling_method == "bayesian") {
    
    # 1. Initialize the 7x4 matrix
    pattern <- matrix(data = c(1,3,6,10,2,5,9,14,4,8,13,18,7,12,
                               17,21,11,16,20,23,15,19,22,24), nrow = 6, ncol = 4,
                      byrow = TRUE)
    
    colnames(pattern) <- c("eez", "realm", "region", "global")
    rownames(pattern) <- c("genus", "family", "order", "class", "phylum", "kingdom")
    
    
    lookup <- expand.grid(row = rownames(pattern), col = colnames(pattern))
    lookup$value <- as.vector(pattern)
    
    lookup <- lookup[order(lookup$value), ]
    
    lookup$col <- as.character(lookup$col)
    lookup$row <- as.character(lookup$row)
    
    for (i in 1:nrow(lookup)) {
      
      # Set join by map key (e.g., join by EEZ & genus, EEZ & family, realm & phylum, etc)
      join_by_map <- setNames("taxa_name", lookup[,1][i])
      
      out_df <- fill_higher_taxa_helper(out_df,
                                        gap_filling_method = lookup$col[lookup$value == i],
                                        averaging = averaging,
                                        lookup_table = lookup_table,
                                        ecoregion = ecoregion,
                                        level = lookup$row[lookup$value == i],
                                        join_by_map = join_by_map)
      
    }
    
  }
  
  # Post gapfilling data processing, delineate whether the gapfilling
  # was not needed or was unable to be performed.
  # Gapfill levels / methods are otherwise left unchanged.
  out_df <- out_df %>%
    mutate(gapfill_method = case_when(
      !is.na(ssp126) & is.na(gapfill_method) ~ "Gapfill not needed",
      is.na(ssp126) ~ "Unsuccessful gapfill",
      TRUE ~ gapfill_method 
    ),
    gapfill_level = case_when(
      !is.na(ssp126) & is.na(gapfill_level) ~ "Gapfill not needed",
      is.na(ssp126) ~ "Unsuccessful gapfill",
      TRUE ~ gapfill_level 
    ),
    averaging = case_when(!(gapfill_method == "Gapfill not needed" |
                              gapfill_method == "Unsuccessful gapfill") ~ averaging,
                          TRUE ~ NA_character_) # define whether weighted or unweighted averaging was used.
    ) %>% 
    select(-taxa_level) # not needed in final output
  
  # Return outputted, gapfilled consumption file.
  return(out_df)
}