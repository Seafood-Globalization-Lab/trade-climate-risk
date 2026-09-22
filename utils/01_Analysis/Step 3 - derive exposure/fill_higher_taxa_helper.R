# Function name: fill_higher_taxa_helper.R

# Function purpose: gap fill NA catch projection values in the consumption with
# the averages found in the lookup table. The order of gap filling (for example,
# eez, genus --> eez, family --> realm genus --> realm family, etc.) will depend
# on the type of gap filling performend.

fill_higher_taxa_helper <- function(df,
                                    gap_filling_method,
                                    averaging,
                                    ecoregion,
                                    lookup_table,
                                    level,
                                    join_by_map) {
  
  suffix <- if (averaging == "weighted") "wgt" else "unwgt"
  selector <- paste0(gap_filling_method,"_gapfills_",suffix)
  
  if (level == "genus") {
    
    if (gap_filling_method == "realm") {
      df <- df %>%
        left_join(ecoregion %>% select(iso3c, realm), by = c("eez_iso3c" = "iso3c"))
    } else if (gap_filling_method == "region") {
      df <- df %>%
        add_region("eez_iso3c", "region")
    }
    
  }
  
  # Based on function input, group by function method input
  join_cols <- switch(
    gap_filling_method,
    eez   = c("eez_iso3c", join_by_map),
    realm = c("realm", join_by_map),
    region = c("region", join_by_map),
    global = c(join_by_map)
  )
  
  df <- df %>%
    select(-taxa_level) %>%
    ## Join data to EEZ interpolated data
    left_join(
      lookup_table[[selector]] %>% filter(taxa_level == level),
      by = join_cols) %>%
    ## Conduct EEZ gapfills
    ### Only assign the gapfill method if there was actually a gapfill occurring
    ### Only assign the taxonomic level for which the gapfill was performed at
    ### if a gapfill occurred
    mutate(
      gapfill_level = case_when( ## Define at which taxa level gap filled data was gotten from
        is.na(ssp126) & !is.na(!!sym(paste0(gap_filling_method,"_avg_ssp126"))) ~ level, # Data coverage between ssp126/585 are the same, so we only need to check one variable
        TRUE                                              ~ gapfill_level
      ),
      gapfill_method = case_when(
        is.na(ssp126) & !is.na(!!sym(paste0(gap_filling_method,"_avg_ssp126"))) ~ paste0(gap_filling_method,"_gapfill"),
        TRUE                                              ~ gapfill_method
      ),
      ssp126 = coalesce(ssp126, !!sym(paste0(gap_filling_method,"_avg_ssp126"))),
      ssp585 = coalesce(ssp585, !!sym(paste0(gap_filling_method,"_avg_ssp585"))),
      cv_ssp126 = coalesce(cv_ssp126, !!sym(paste0(gap_filling_method,"_cv_ssp126"))),
      cv_ssp585 = coalesce(cv_ssp585, !!sym(paste0(gap_filling_method,"_cv_ssp585")))
    ) %>%
    select(-!!sym(paste0(gap_filling_method,"_avg_ssp126")),
           -!!sym(paste0(gap_filling_method,"_avg_ssp585")),
           -!!sym(paste0(gap_filling_method,"_cv_ssp126")),
           -!!sym(paste0(gap_filling_method,"_cv_ssp585")))
  
  return(df)
  
}
