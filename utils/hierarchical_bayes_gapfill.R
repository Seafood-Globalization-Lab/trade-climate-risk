# =============================================================================
# Hierarchical Bayesian Gapfilling for SSP Projections
# =============================================================================
# Replaces the diagonal/hierarchical gapfill pipeline with a full Bayesian
# model. Species-level ssp126/ssp585 observations are the only true data;
# genus, family, order, class, phylum, kingdom means are latent variables
# that exist purely to pool information across related species within EEZs.
#
# Model structure (per EEZ e, taxonomic chain s -> g -> f -> o -> c -> p -> k):
#
#   ssp126_obs[i] ~ Normal(mu_species[s,e], sigma_obs)        # likelihood
#   mu_species[s,e] ~ Normal(mu_genus[g,e],   sigma_species)  # latent
#   mu_genus[g,e]   ~ Normal(mu_family[f,e],  sigma_genus)    # latent
#   mu_family[f,e]  ~ Normal(mu_order[o,e],   sigma_family)   # latent
#   mu_order[o,e]   ~ Normal(mu_class[c,e],   sigma_order)    # latent
#   mu_class[c,e]   ~ Normal(mu_phylum[p,e],  sigma_class)    # latent
#   mu_phylum[p,e]  ~ Normal(mu_kingdom[k,e], sigma_phylum)   # latent
#   mu_kingdom[k,e] ~ Normal(mu_global, sigma_kingdom)        # latent
#
# =============================================================================
# SETUP (run once before first use)
# =============================================================================
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::install_cmdstan()           # installs CmdStan to ~/.cmdstan
# cmdstanr::set_cmdstan_path()          # auto-detects after install; or pass path explicitly
# install.packages(c("posterior", "tidybayes"))
# =============================================================================

library(tidyverse)
library(cmdstanr)
library(posterior)
library(tidybayes)

# -----------------------------------------------------------------------------
# 1. Prepare indexing data from the consumption df
# -----------------------------------------------------------------------------
# df : full consumption data frame (3M+ rows), includes all scinames
#      and taxonomic columns; ssp126/ssp585 are NA for unobserved species.
#      Required columns: sciname, eez_iso3c, genus, family, order, class,
#                        phylum, kingdom, ssp126, ssp585

prepare_stan_data <- function(df) {
  
  # ---------------------------------------------------------------------------
  # 1a. Build unique taxonomic index tables, joining on the FULL lineage chain
  #     at each step to avoid many-to-many collisions from non-unique names.
  #     e.g. the family name "Sparidae" could appear under two different orders
  #     in the raw data; joining only on "family" would duplicate rows.
  # ---------------------------------------------------------------------------
  
  # One row per unique species with its complete taxonomic lineage
  taxonomy <- df %>%
    distinct(sciname, genus, family, order, class, phylum, kingdom) %>%
    filter(str_detect(sciname, " "))   # species only (binomial has a space)
  
  # Each index table carries enough parent columns to join unambiguously
  kingdoms <- taxonomy %>%
    distinct(kingdom) %>%
    mutate(kingdom_id = row_number())
  
  phyla <- taxonomy %>%
    distinct(phylum, kingdom) %>%              # kingdom disambiguates phylum
    mutate(phylum_id = row_number())
  
  classes <- taxonomy %>%
    distinct(class, phylum, kingdom) %>%       # full chain down from kingdom
    mutate(class_id = row_number())
  
  orders <- taxonomy %>%
    distinct(order, class, phylum, kingdom) %>%
    mutate(order_id = row_number())
  
  families <- taxonomy %>%
    distinct(family, order, class, phylum, kingdom) %>%
    mutate(family_id = row_number())
  
  genera <- taxonomy %>%
    distinct(genus, family, order, class, phylum, kingdom) %>%
    mutate(genus_id = row_number())
  
  species <- taxonomy %>%
    distinct(sciname, genus, family, order, class, phylum, kingdom) %>%
    mutate(species_id = row_number())
  
  # ---------------------------------------------------------------------------
  # 1b. Build parent-index vectors
  #     Each is a length-N integer vector where entry [i] gives the parent's id.
  #     Joins are done on the full lineage so names that collide across branches
  #     resolve to the correct parent.
  # ---------------------------------------------------------------------------
  
  phylum_to_kingdom <- phyla %>%
    left_join(kingdoms, by = "kingdom") %>%
    arrange(phylum_id) %>%
    pull(kingdom_id)
  
  # class joins phyla on both phylum AND kingdom to avoid cross-branch matches
  class_to_phylum <- classes %>%
    left_join(phyla, by = c("phylum", "kingdom")) %>%
    arrange(class_id) %>%
    pull(phylum_id)
  
  order_to_class <- orders %>%
    left_join(classes, by = c("class", "phylum", "kingdom")) %>%
    arrange(order_id) %>%
    pull(class_id)
  
  family_to_order <- families %>%
    left_join(orders, by = c("order", "class", "phylum", "kingdom")) %>%
    arrange(family_id) %>%
    pull(order_id)
  
  genus_to_family <- genera %>%
    left_join(families, by = c("family", "order", "class", "phylum", "kingdom")) %>%
    arrange(genus_id) %>%
    pull(family_id)
  
  species_to_genus <- species %>%
    left_join(genera, by = c("genus", "family", "order", "class", "phylum", "kingdom")) %>%
    arrange(species_id) %>%
    pull(genus_id)
  
  # Validate: every parent vector must be fully non-NA and in range
  stopifnot(
    !anyNA(phylum_to_kingdom),
    !anyNA(class_to_phylum),
    !anyNA(order_to_class),
    !anyNA(family_to_order),
    !anyNA(genus_to_family),
    !anyNA(species_to_genus)
  )
  
  # EEZ index
  eezs <- df %>%
    distinct(eez_iso3c) %>%
    mutate(eez_id = row_number())
  
  # ---------------------------------------------------------------------------
  # 1c. Observed data: rows where ssp126 is not NA
  #     Multiple rows per species×EEZ may exist in consumption data (different
  #     years, products, etc.) — we take the distinct ssp value per pair since
  #     the SSP projection is a species-level quantity, not a row-level one.
  # ---------------------------------------------------------------------------
  
  obs <- df %>%
    filter(!is.na(ssp126), str_detect(sciname, " ")) %>%
    distinct(sciname, eez_iso3c, ssp126, ssp585) %>%
    # If multiple ssp126 values exist per species×EEZ pair (shouldn't happen
    # for a species-level projection, but guards against duplicates), take mean
    group_by(sciname, eez_iso3c) %>%
    summarise(ssp126 = mean(ssp126, na.rm = TRUE),
              ssp585 = mean(ssp585, na.rm = TRUE),
              .groups = "drop") %>%
    left_join(species %>% select(sciname, species_id), by = "sciname") %>%
    left_join(eezs,                                     by = "eez_iso3c") %>%
    drop_na(species_id, eez_id)
  
  # ---------------------------------------------------------------------------
  # 1d. Prediction targets: ALL unique species×EEZ pairs in the consumption df
  #     (observed + unobserved). Stan's generated quantities block will return
  #     a posterior draw for each, enabling full posterior gapfilling.
  # ---------------------------------------------------------------------------
  
  all_pairs <- df %>%
    filter(str_detect(sciname, " ")) %>%
    distinct(sciname, eez_iso3c) %>%
    left_join(species %>% select(sciname, species_id), by = "sciname") %>%
    left_join(eezs,                                     by = "eez_iso3c") %>%
    drop_na(species_id, eez_id)
  
  cat(sprintf(
    "  Observations  : %d species×EEZ pairs with ssp data\n  Pred targets  : %d species×EEZ pairs total\n  Species       : %d | Genera: %d | Families: %d | Orders: %d | Classes: %d | Phyla: %d | Kingdoms: %d\n  EEZs          : %d\n",
    nrow(obs), nrow(all_pairs),
    nrow(species), nrow(genera), nrow(families), nrow(orders),
    nrow(classes), nrow(phyla), nrow(kingdoms), nrow(eezs)
  ))
  
  # ---------------------------------------------------------------------------
  # 1e. Pack into Stan-ready list
  # ---------------------------------------------------------------------------
  
  stan_data <- list(
    N_obs     = nrow(obs),
    N_species = nrow(species),
    N_genus   = nrow(genera),
    N_family  = nrow(families),
    N_order   = nrow(orders),
    N_class   = nrow(classes),
    N_phylum  = nrow(phyla),
    N_kingdom = nrow(kingdoms),
    N_eez     = nrow(eezs),
    
    ssp126_obs = obs$ssp126,
    ssp585_obs = obs$ssp585,
    
    obs_species_id = obs$species_id,
    obs_eez_id     = obs$eez_id,
    
    species_to_genus  = species_to_genus,
    genus_to_family   = genus_to_family,
    family_to_order   = family_to_order,
    order_to_class    = order_to_class,
    class_to_phylum   = class_to_phylum,
    phylum_to_kingdom = phylum_to_kingdom,
    
    N_pred          = nrow(all_pairs),
    pred_species_id = all_pairs$species_id,
    pred_eez_id     = all_pairs$eez_id
  )
  
  list(
    stan_data = stan_data,
    all_pairs = all_pairs,
    obs       = obs,
    species   = species,
    genera    = genera,
    families  = families,
    orders    = orders,
    classes   = classes,
    phyla     = phyla,
    kingdoms  = kingdoms,
    eezs      = eezs
  )
  
}


# -----------------------------------------------------------------------------
# 2. Fit the Stan model
# -----------------------------------------------------------------------------

fit_hierarchical_ssp <- function(prepared,
                                  stan_file       = "hierarchical_ssp.stan",
                                  chains          = 4,
                                  iter_warmup     = 500,
                                  iter_sampling   = 500,
                                  parallel_chains = 4,
                                  ...) {
  
  # Verify CmdStan is available before attempting compile
  tryCatch(
    cmdstan_path(),
    error = function(e) stop(
      "CmdStan not found. Run:\n",
      "  cmdstanr::install_cmdstan()\n",
      "  cmdstanr::set_cmdstan_path()   # or pass the path explicitly\n",
      call. = FALSE
    )
  )
  
  mod <- cmdstan_model(stan_file)
  
  fit <- mod$sample(
    data            = prepared$stan_data,
    chains          = chains,
    iter_warmup     = iter_warmup,
    iter_sampling   = iter_sampling,
    parallel_chains = parallel_chains,
    ...
  )
  
  fit
  
}


# -----------------------------------------------------------------------------
# 3. Extract posterior summaries and join back to consumption data
# -----------------------------------------------------------------------------

extract_gapfill_posteriors <- function(fit, prepared, df, ci = 0.9) {
  
  alpha <- (1 - ci) / 2
  
  summarise_pred_draws <- function(fit, param_name, prefix) {
    fit$draws(param_name, format = "draws_df") %>%
      pivot_longer(
        cols      = starts_with(param_name),
        names_to  = "param",
        values_to = "value"
      ) %>%
      # Stan indexes as mu_species_pred_126[1], mu_species_pred_126[2], ...
      mutate(pred_idx = as.integer(str_extract(param, "(?<=\\[)\\d+(?=\\])"))) %>%
      group_by(pred_idx) %>%
      summarise(
        !!paste0(prefix, "_mean") := mean(value),
        !!paste0(prefix, "_sd")   := sd(value),
        !!paste0(prefix, "_lo")   := quantile(value, alpha),
        !!paste0(prefix, "_hi")   := quantile(value, 1 - alpha),
        .groups = "drop"
      )
  }
  
  summary_126 <- summarise_pred_draws(fit, "mu_species_pred_126", "ssp126")
  summary_585 <- summarise_pred_draws(fit, "mu_species_pred_585", "ssp585")
  
  pred_summary <- prepared$all_pairs %>%
    mutate(pred_idx = row_number()) %>%
    left_join(summary_126, by = "pred_idx") %>%
    left_join(summary_585, by = "pred_idx") %>%
    select(sciname, eez_iso3c,
           ssp126_mean, ssp126_sd, ssp126_lo, ssp126_hi,
           ssp585_mean, ssp585_sd, ssp585_lo, ssp585_hi)
  
  # Flag observed vs gapfilled
  observed_keys <- prepared$obs %>%
    distinct(sciname, eez_iso3c) %>%
    mutate(was_observed = TRUE)
  
  pred_summary <- pred_summary %>%
    left_join(observed_keys, by = c("sciname", "eez_iso3c")) %>%
    mutate(
      was_observed   = coalesce(was_observed, FALSE),
      gapfill_method = if_else(was_observed, "observed", "hierarchical_bayes")
    )
  
  # Join posterior means back onto the full consumption df under the same
  # column names (ssp126 / ssp585) as the original pipeline output
  df %>%
    left_join(
      pred_summary %>%
        select(sciname, eez_iso3c,
               ssp126_bayes = ssp126_mean,
               ssp585_bayes = ssp585_mean,
               ssp126_sd, ssp126_lo, ssp126_hi,
               ssp585_sd, ssp585_lo, ssp585_hi,
               was_observed, gapfill_method),
      by = c("sciname", "eez_iso3c")
    )
  
}


# -----------------------------------------------------------------------------
# 4. Top-level wrapper  (mirrors fill_with_higher_taxa interface)
# -----------------------------------------------------------------------------

fill_with_higher_taxa_bayes <- function(df,
                                         stan_file       = "hierarchical_ssp.stan",
                                         cache_dir       = "../data/exposure/bayes_fits/",
                                         chains          = 4,
                                         iter_warmup     = 500,
                                         iter_sampling   = 500,
                                         parallel_chains = 4,
                                         ci              = 0.9,
                                         ...) {
  
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_path <- file.path(cache_dir, "fit.rds")
  prep_path  <- file.path(cache_dir, "prepared.rds")
  
  if (file.exists(cache_path) && file.exists(prep_path)) {
    
    cat("✔️  Loading cached Stan fit\n")
    fit      <- readRDS(cache_path)
    prepared <- readRDS(prep_path)
    
  } else {
    
    cat("⚙️  Preparing Stan data...\n")
    prepared <- prepare_stan_data(df)
    saveRDS(prepared, prep_path)
    
    cat("⚙️  Fitting Stan model...\n")
    fit <- fit_hierarchical_ssp(
      prepared        = prepared,
      stan_file       = stan_file,
      chains          = chains,
      iter_warmup     = iter_warmup,
      iter_sampling   = iter_sampling,
      parallel_chains = parallel_chains,
      ...
    )
    saveRDS(fit, cache_path)
    
  }
  
  cat("⚙️  Extracting posteriors...\n")
  extract_gapfill_posteriors(fit, prepared, df, ci = ci)
  
}

fit <- fill_with_higher_taxa_bayes(df = consumption_mpc)
