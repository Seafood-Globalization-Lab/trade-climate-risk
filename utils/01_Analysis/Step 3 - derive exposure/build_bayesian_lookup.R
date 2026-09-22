# Function name: build_bayesian_lookup.R

# Function purpose: Build lookup table from the bayesian output created from
# taxonomic_bayesian_gapfill.R that will be used to gap fill catch projections

build_bayesian_lookup <-function(df, ecoregion, assumed_sd) {
  
  cat(sprintf("Building Bayesian lookup table...\n"))
  
  # ------------------------------- EEZ
  
  ## SSP 126
  
  ### Run bayesian model DO NOT OPEN OUTPUT FIT OR R WILL CRASH
  output <- taxonomic_bayesian_gapfill(df = df, assumed_sd = assumed_sd,
                                       method = "eez_iso3c", ssp = "ssp126")
  
  fit_eez_iso3c_126 <- output$fit
  
  ### Extract mcmc samples from EEZ fit
  mcmc_eez_126 <- rstan::extract(fit_eez_iso3c_126, permuted = TRUE)
  
  saveRDS(mcmc_eez_126, paste0("../output/bayesian/",as.character(assumed_sd),"_sd","/mcmc_ssp126.rds"))
  
  ### calculate median posteriors for each taxa
  genus_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_genus_lookup, MARGIN = 2, FUN = median)
  family_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_family_lookup, MARGIN = 2, FUN = median) 
  order_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_order_lookup, MARGIN = 2, FUN = median) 
  class_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_class_lookup, MARGIN = 2, FUN = median) 
  phylum_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_phylum_lookup, MARGIN = 2, FUN = median) 
  kingdom_eez_med_126 <- apply(mcmc_eez_126$ssp_pred_kingdom_lookup, MARGIN = 2, FUN = median)
  
  # Refersh environment
  rm(output)
  
  ## SSP 585
  
  ### Run bayesian modelDO NOT OPEN OUTPUT FIT OR R WILL CRASH
  output <- taxonomic_bayesian_gapfill(df = df, assumed_sd = assumed_sd,
                                       method = "eez_iso3c", ssp = "ssp585")
  
  fit_eez_iso3c_585 <- output$fit
  
  ### Extract mcmc samples from EEZ fit
  mcmc_eez_585 <- rstan::extract(fit_eez_iso3c_585, permuted = TRUE)
  
  saveRDS(mcmc_eez_126, paste0("../output/bayesian/",as.character(assumed_sd),"_sd","/mcmc_ssp585.rds"))
  
  
  ### calculate median posteriors for each taxa
  genus_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_genus_lookup, MARGIN = 2, FUN = median)
  family_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_family_lookup, MARGIN = 2, FUN = median) 
  order_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_order_lookup, MARGIN = 2, FUN = median) 
  class_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_class_lookup, MARGIN = 2, FUN = median) 
  phylum_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_phylum_lookup, MARGIN = 2, FUN = median) 
  kingdom_eez_med_585 <- apply(mcmc_eez_585$ssp_pred_kingdom_lookup, MARGIN = 2, FUN = median) 
  
  
  
  ## Create EEZ lookup table
  bayesian_lookup_eez_unwgt <- data.frame(eez_iso3c = c(output$genus_eezs, output$family_eezs, output$order_eezs, output$class_eezs, output$phylum_eezs, output$kingdom_eezs),
                                          taxa_name = c(output$genus, output$family, output$order, 
                                                        output$class, output$phylum, output$kingdom),
                                          eez_avg_ssp126 = c(genus_eez_med_126, family_eez_med_126,
                                                             order_eez_med_126, class_eez_med_126,
                                                             phylum_eez_med_126, kingdom_eez_med_126),
                                          eez_avg_ssp585 = c(genus_eez_med_585, family_eez_med_585,
                                                             order_eez_med_585, class_eez_med_585,
                                                             phylum_eez_med_585, kingdom_eez_med_585),
                                          taxa_level = c(rep("genus", length(output$genus_eezs)),
                                                         rep("family", length(output$family_eezs)),
                                                         rep("order", length(output$order_eezs)),
                                                         rep("class", length(output$class_eezs)),
                                                         rep("phylum", length(output$phylum_eezs)),
                                                         rep("kingdom", length(output$kingdom_eezs)))) %>%
    # FIXIT: quick fix, figure out why ostreidae is having duplicates
    mutate(taxa_level = case_when(
      taxa_name == "ostreidae" ~ "family",
      TRUE ~ taxa_level
    )) %>%
    group_by(eez_iso3c, taxa_name, taxa_level) %>%
    summarize(eez_avg_ssp126 = mean(eez_avg_ssp126),
              eez_avg_ssp585 = mean(eez_avg_ssp585)) %>%
    mutate(eez_cv_ssp126 = NA,
           eez_cv_ssp585 = NA) %>%
    relocate(taxa_level, .after = eez_cv_ssp585) %>%
    ungroup()
  
  # ------------------------------- Realm
  
  bayesian_lookup_realm_unwgt <- bayesian_lookup_eez_unwgt %>%
    left_join(
      ecoregion %>%
        select(iso3c, realm), 
      by = c("eez_iso3c" = "iso3c")) %>%
    select(-eez_iso3c) %>%
    group_by(realm, taxa_name) %>%
    summarize(realm_avg_ssp126 = mean(eez_avg_ssp126),
              realm_avg_ssp585 = mean(eez_avg_ssp585),
              realm_cv_ssp126 = NA,
              realm_cv_ssp585 = NA) %>%
    ungroup() %>%
    distinct() %>%
    relocate(realm, .before = "taxa_name") %>%
    # Add on taxa level
    left_join(
      bayesian_lookup_eez_unwgt %>% select(taxa_name, taxa_level) %>% distinct(),
      by = "taxa_name")
  
  
  
  # ------------------------------- Region
  bayesian_lookup_region_unwgt <- bayesian_lookup_eez_unwgt %>%
    exploreARTIS::add_region(col = "eez_iso3c", region.col.name = "region") %>%
    group_by(region, taxa_name) %>%
    summarize(region_avg_ssp126 = mean(eez_avg_ssp126),
              region_avg_ssp585 = mean(eez_avg_ssp585),
              region_cv_ssp126 = NA,
              region_cv_ssp585 = NA) %>%
    ungroup() %>%
    distinct() %>%
    # Add on taxa level
    left_join(
      bayesian_lookup_eez_unwgt %>% select(taxa_name, taxa_level)  %>% distinct(),
      by = "taxa_name")
  
  # ------------------------------- Global
  bayesian_lookup_global_unwgt <- bayesian_lookup_eez_unwgt %>%
    exploreARTIS::add_region(col = "eez_iso3c", region.col.name = "region") %>%
    group_by(taxa_name) %>%
    summarize(global_avg_ssp126 = mean(eez_avg_ssp126),
              global_avg_ssp585 = mean(eez_avg_ssp585),
              global_cv_ssp126 = NA,
              global_cv_ssp585 = NA) %>%
    ungroup() %>%
    distinct() %>%
    # Add on taxa level
    left_join(
      bayesian_lookup_eez_unwgt %>% select(taxa_name, taxa_level)  %>% distinct(),
      by = "taxa_name")
  
  return(list(
    eez_gapfills_unwgt      = bayesian_lookup_eez_unwgt,
    realm_gapfills_unwgt    = bayesian_lookup_realm_unwgt,
    region_gapfills_unwgt   = bayesian_lookup_region_unwgt,
    global_gapfills_unwgt   = bayesian_lookup_global_unwgt
  ))
  
}