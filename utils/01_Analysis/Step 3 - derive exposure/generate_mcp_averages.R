# Function name: generate_mcp_averages.R

# Function purpose: For each non-species identified as consumption in ARTIS,
# (for example, a class of Actinopterygii), calculate either (1) a weighted
# or (2) unweighted average of the available catch projection data that is 
# available in that taxa. In this function you can select the spatial scale
# to draw averages from, with EEZ being the highest resolution, followed in
# resolution by realm, region, and global. You can also select the minimum
# number of species to draw averages from.

# Function purpose: for each higher taxonomic level, compute mean catch projection
# values per EEZ so that species with missing projections can borrow from relatives
generate_mcp_averages <- function(df,
                                  method = c("eez", "realm", "region", "global"),
                                  averaging_type = c("unweighted", "weighted"),
                                  ecoregion,
                                  num_species) {
  
  # Ensure that inputs to the following lines of code match function arguments
  method <- match.arg(method)
  averaging_type <- match.arg(averaging_type)
  
  # Set taxa levels to calculate averages for
  tax_levels <- c("genus", "family", "order", "class", "phylum", "kingdom")
  
  # Perform weighted / unweighted averaging_type and store in `x`
  x <- map_dfr(tax_levels, function(level) {
    
    # Based on function input, group by function method input
    group_cols <- switch(
      method,
      eez   = c("eez_iso3c", level),
      realm = c("realm", level),
      region = c("region", level),
      global = c(level)
    )
    
    if (averaging_type == "weighted") { # Compute weighted avearages
      
      df2 <- df %>%
        filter(!is.na(ssp126) | !is.na(ssp585)) %>%
        group_by(sciname_hs_modified, eez_iso3c) %>%
        summarize(live_weight_t = sum(live_weight_t), .groups = "drop") %>%
        ungroup() %>%
        left_join(
          ecoregion %>% select(iso3c, realm),
          by = c("eez_iso3c" = "iso3c")
        ) %>%
        add_region("eez_iso3c", "region")
      
      # Join ssp estimates back onto df dataset
      df2 <- df2 %>%
        left_join(
          df %>%
            add_region("eez_iso3c", "region") %>%
            left_join(ecoregion, by = c("eez_iso3c" = "iso3c")) %>%
            filter(!is.na(ssp126)) %>%
            distinct(sciname_hs_modified, across(all_of(c("eez_iso3c", group_cols[length(group_cols)]))), ssp126, ssp585),
          by = c("sciname_hs_modified", "eez_iso3c")
        )
      
      # Allow averages to only include at least `num_species` species
      # (so that one species doesn't dominate). Number of minimum species used
      # for gapfilling depends on user input.
      df2_sp_filter <- df2 %>%
        group_by(across(all_of(c(group_cols[1], level)))) %>%
        mutate(n = n_distinct(sciname_hs_modified)) %>%   # Fixit: Currently set to minimum number of species. Do we want this to be minimum number of eez / iso3c pairs? That would be much easier to satisfy.
        ungroup() %>%
        filter(n >= num_species)
      
      # Calculate weighted averages
      out <- df2_sp_filter %>%
        group_by(across(all_of(group_cols))) %>%
        mutate(total_live_weight_group = sum(live_weight_t)) %>%
        
        summarize(
          !!paste0(method, "_avg_ssp126") := weighted.mean(ssp126, live_weight_t, na.rm = TRUE),
          !!paste0(method, "_avg_ssp585") := sum(ssp585 * (live_weight_t / total_live_weight_group)),
          
          # Weighted Coefficients of variation
          !!paste0(method, "_cv_ssp126") := {
            
            wm <- weighted.mean(ssp126, live_weight_t, na.rm = TRUE)
            
            wvar <- sum(live_weight_t * (ssp126 - wm)^2, na.rm = TRUE) /
              sum(live_weight_t, na.rm = TRUE)
            
            sqrt(wvar) / wm
            
          },
          
          !!paste0(method, "_cv_ssp585") := {
            
            wm <- weighted.mean(ssp585, live_weight_t, na.rm = TRUE)
            
            wvar <- sum(live_weight_t * (ssp585 - wm)^2, na.rm = TRUE) /
              sum(live_weight_t, na.rm = TRUE)
            
            sqrt(wvar) / wm
            
          },
          .groups = "drop"
        ) %>%
        ungroup()
      
    } else {
      
      df2 <- df %>%
        distinct() %>%
        left_join(
          ecoregion %>% select(iso3c, realm),
          by = c("eez_iso3c" = "iso3c")
        ) %>%
        filter(!is.na(ssp126) | !is.na(ssp585)) %>%
        add_region("eez_iso3c", "region") %>%
        distinct(eez_iso3c, sciname_hs_modified, .keep_all = TRUE)
      
      
      # Allow averages to only include at least `num_species` species
      # (so that one species doesn't dominate). Number of minimum species used
      # for gapfilling depends on user input.
      df2_sp_filter <- df2 %>%
        group_by(across(all_of(c(group_cols[1], level)))) %>%
        mutate(n = n()) %>%   # number of rows in each pair
        ungroup() %>%
        filter(n >= num_species)
      
      # Calculate unweighted averages 
      out <- df2_sp_filter %>%
        group_by(across(all_of(group_cols))) %>%
        summarise(
          !!paste0(method, "_avg_ssp126") := mean(ssp126, na.rm = TRUE),
          !!paste0(method, "_avg_ssp585") := mean(ssp585, na.rm = TRUE),
          
          !!paste0(method, "_cv_ssp126") :=
            sd(ssp126, na.rm = TRUE) / mean(ssp126, na.rm = TRUE),
          
          !!paste0(method, "_cv_ssp585") :=
            sd(ssp585, na.rm = TRUE) / mean(ssp585, na.rm = TRUE)
        ) %>%
        ungroup()
      
    }
    
    # Rename columns and add a taxa level variable, for delineating the taxa
    # level of each gap filled taxa name
    out %>%
      rename(taxa_name = all_of(level)) %>%
      mutate(taxa_level = level)
    
  })
  
  return(x)
  
}