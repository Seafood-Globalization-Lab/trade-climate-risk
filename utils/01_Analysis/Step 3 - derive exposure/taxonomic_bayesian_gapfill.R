# Function name: preprocess_catch_projection_data.R

# Function purpose: perform Bayesian gap filling on catch projections to taxa
# that are not reported at the species level. This function returns the Stan
# fit; the posteriors will be extracted from in the
# build_bayesian_lookup function.

taxonomic_bayesian_gapfill <- function(df = df, 
                                       assumed_sd,
                                       method = c("eez_iso3c", "realm", "region"),
                                       ssp = c("ssp126", "ssp585")) {
  
  # Get distinct values of sciname / EEZ iso3c pairs with their respective ssp value
  df <- df %>%
    filter(!is.na(ssp126) | !is.na(ssp585)) %>%
    distinct(sciname, eez_iso3c, all_of(across(ssp)))
  
  # Average ssp values by sciname / realm pair
  if (method == "realm") {
    df <- df %>%
      left_join(
        ecoregion %>% select(iso3c, realm),
        by = c("eez_iso3c" = "iso3c")
      ) %>%
      group_by(sciname, realm) %>%
      summarize(!!sym(ssp) := mean(!!sym(ssp)))
  }
  
  # Average ssp values by sciname / region pair
  if (method == "region") {
    df <- df %>%
      add_region("eez_iso3c", "region") %>%
      group_by(sciname, region) %>%
      summarize(!!sym(ssp) := mean(!!sym(ssp)))
  }
  
  # Add taxa levels back to data
  df <- df %>%
    left_join(sciname_taxa, by = "sciname") %>%
    select(-species, -superclass, -subfamily, -taxa_level) %>%
    mutate(
      genus = case_when(
        sciname == "crassostrea gigas" ~ "crassostrea", TRUE ~ genus
      ),
      family = case_when(
        sciname == "crassostrea gigas" ~ "ostreidae", TRUE ~ family
      ),
      order = case_when(
        sciname == "crassostrea gigas" ~ "ostreidae", TRUE ~ order
      ),
      class = case_when(
        sciname == "crassostrea gigas" ~ "bivalvia", TRUE ~ class
      ),
      phylum = case_when(
        sciname == "crassostrea gigas" ~ "mollusca", 
        sciname == "petromyzon marinus" ~ "chordata",
        TRUE ~ phylum,
      ),
      kingdom = case_when(
        sciname == "crassostrea gigas" ~ "animalia", TRUE ~ kingdom
      )
    )
  
  ##################### KINGDOM #####################
  # Get the unique EEZ's used in each kingdom
  kingdom_eezs <- df %>%
    distinct(!!sym(method)) %>%
    pull(!!sym(method))
  
  # Get the unique kingdoms used
  kingdom <- df %>%
    distinct(kingdom) %>%
    pull(kingdom)
  
  # Get the all the kingdoms that correspond to the geographic grouping
  kingdoms <- df %>%
    distinct(!!sym(method), kingdom) %>%
    pull(kingdom)
  
  num_eezs <- length(kingdom_eezs) # Calculate the number of EEZs
  num_kingdom <- length(kingdom) 
  
  kingdom_combination_nrow <- num_eezs * num_kingdom
  
  eez_kingdom_lookup <- rep(NA_character_, kingdom_combination_nrow)
  kingdom_name_lookup <- rep(NA_character_, kingdom_combination_nrow)
  
  loop_iteration_nrow_kingdom <- 1
  
  for (eez in kingdom_eezs) {
    for (k in kingdom) {
      eez_kingdom_lookup[loop_iteration_nrow_kingdom] <- eez
      kingdom_name_lookup[loop_iteration_nrow_kingdom] <- k
      loop_iteration_nrow_kingdom <- loop_iteration_nrow_kingdom + 1
    }
  }
  
  ##################### PHYLUM #####################
  phylum_eezs <- df %>%
    
    distinct(!!sym(method), kingdom, phylum) %>%
    pull(!!sym(method))
  
  phylum_kingdom <- df %>%
    
    distinct(!!sym(method), kingdom, phylum) %>%
    pull(kingdom)
  
  phylum <- df %>%
    
    distinct(!!sym(method), kingdom, phylum) %>%
    pull(phylum)
  
  phylum_combination_nrow <- length(phylum)
  
  phylum_to_kingdom_lookup <- rep(NA_integer_, phylum_combination_nrow)
  
  for(loop in 1:phylum_combination_nrow){
    idx <- which(
      phylum_eezs[loop] == eez_kingdom_lookup &
        phylum_kingdom[loop] == kingdom_name_lookup
    )
    phylum_to_kingdom_lookup[loop] <- idx[1]
  }
  
  ##################### CLASS #####################
  class_eezs <- df %>%
    
    distinct(!!sym(method), phylum, class) %>%
    pull(!!sym(method))
  
  class_phylum <- df %>%
    
    distinct(!!sym(method), phylum, class) %>%
    pull(phylum)
  
  class <- df %>%
    
    distinct(!!sym(method), phylum, class) %>%
    pull(class)
  
  class_combination_nrow <- length(class)
  
  class_to_phylum_lookup <- rep(NA_integer_, class_combination_nrow)
  
  for(loop in 1:class_combination_nrow){
    idx <- which(
      class_eezs[loop] == phylum_eezs &
        class_phylum[loop] == phylum
    )
    class_to_phylum_lookup[loop] <- idx[1]
  }
  
  ##################### ORDER #####################
  order_eezs <- df %>%
    
    distinct(!!sym(method), class, order) %>%
    pull(!!sym(method))
  
  order_class <- df %>%
    
    distinct(!!sym(method), class, order) %>%
    pull(class)
  
  order <- df %>%
    
    distinct(!!sym(method), class, order) %>%
    pull(order)
  
  order_combination_nrow <- length(order)
  
  order_to_class_lookup <- rep(NA_integer_, order_combination_nrow)
  
  for(loop in 1:order_combination_nrow){
    idx <- which(
      order_eezs[loop] == class_eezs &
        order_class[loop] == class
    )
    order_to_class_lookup[loop] <- idx[1]
  }
  
  ##################### FAMILY #####################
  family_eezs <- df %>%
    
    distinct(!!sym(method), order, family) %>%
    pull(!!sym(method))
  
  family_order <- df %>%
    
    distinct(!!sym(method), order, family) %>%
    pull(order)
  
  family <- df %>%
    
    distinct(!!sym(method), order, family) %>%
    pull(family)
  
  family_combination_nrow <- length(family)
  
  family_to_order_lookup <- rep(NA_integer_, family_combination_nrow)
  
  for(loop in 1:family_combination_nrow){
    idx <- which(
      family_eezs[loop] == order_eezs &
        family_order[loop] == order
    )
    family_to_order_lookup[loop] <- idx[1]
  }
  
  ##################### GENUS #####################
  genus_eezs <- df %>%
    
    distinct(!!sym(method), family, genus) %>%
    pull(!!sym(method))
  
  genus_family <- df %>%
    distinct(!!sym(method), family, genus) %>%
    pull(family)
  
  genus <- df %>%
    distinct(!!sym(method), family, genus) %>%
    pull(genus)
  
  genus_combination_nrow <- length(genus)
  
  genus_to_family_lookup <- rep(NA_integer_, genus_combination_nrow)
  
  for(loop in 1:genus_combination_nrow){
    idx <- which(
      genus_eezs[loop] == family_eezs &
        genus_family[loop] == family
    )
    genus_to_family_lookup[loop] <- idx[1]
  }
  
  ##################### SPECIES #####################
  species_eezs <- df %>%
    distinct(!!sym(method), genus, sciname) %>%
    pull(!!sym(method))
  
  species_genus <- df %>%
    distinct(!!sym(method), genus, sciname) %>%
    pull(genus)
  
  species <- df %>%
    distinct(!!sym(method), genus, sciname) %>%
    pull(sciname)
  
  species_combination_nrow <- length(species)
  
  species_to_genus_lookup <- rep(NA_integer_, species_combination_nrow)
  
  for(loop in 1:species_combination_nrow){
    idx <- which(
      species_eezs[loop] == genus_eezs &
        species_genus[loop] == genus
    )
    species_to_genus_lookup[loop] <- idx[1]
  }
  
  ############################## Set input data for Stan model
  ssp_obs <- df %>%
    filter(!is.na(!!sym(ssp))) %>%
    distinct(
      across(all_of(method)),
      across(all_of(ssp)),
      genus,
      species,
    ) %>%
    pull(!!sym(ssp))
  
  
  # Test to ensure number of observations matches the number of predicted columns of the lookup table
  stopifnot(
    length(ssp_obs) ==
      species_combination_nrow
  )
  
  # Create list of data to be passed into Stan
  stan_data <- list(
    
    kingdom_combination_nrow =
      kingdom_combination_nrow,
    
    phylum_combination_nrow =
      phylum_combination_nrow,
    
    class_combination_nrow =
      class_combination_nrow,
    
    order_combination_nrow =
      order_combination_nrow,
    
    family_combination_nrow =
      family_combination_nrow,
    
    genus_combination_nrow =
      genus_combination_nrow,
    
    species_combination_nrow =
      species_combination_nrow,
    
    phylum_to_kingdom_lookup =
      as.integer(phylum_to_kingdom_lookup),
    
    class_to_phylum_lookup =
      as.integer(class_to_phylum_lookup),
    
    order_to_class_lookup =
      as.integer(order_to_class_lookup),
    
    family_to_order_lookup =
      as.integer(family_to_order_lookup),
    
    genus_to_family_lookup =
      as.integer(genus_to_family_lookup),
    
    species_to_genus_lookup =
      as.integer(species_to_genus_lookup),
    
    ssp_obs = as.vector(ssp_obs),
    
    sigma_obs = assumed_sd
    
  )
  
  ############################## Set inits for stan model
  init_fun <- function() {
    
    species_init <- ssp_obs
    
    genus_init <- rnorm(
      genus_combination_nrow,
      mean(species_init),
      5
    )
    
    family_init <- rnorm(
      family_combination_nrow,
      mean(genus_init),
      5
    )
    
    order_init <- rnorm(
      order_combination_nrow,
      mean(family_init),
      5
    )
    
    class_init <- rnorm(
      class_combination_nrow,
      mean(order_init),
      5
    )
    
    phylum_init <- rnorm(
      phylum_combination_nrow,
      mean(class_init),
      5
    )
    
    kingdom_init <- runif(
      kingdom_combination_nrow,
      -25,
      25  
    )
    
    list(
      
      ssp_pred_kingdom_lookup =
        kingdom_init,
      
      ssp_pred_phylum_lookup =
        phylum_init,
      
      ssp_pred_class_lookup =
        class_init,
      
      ssp_pred_order_lookup =
        order_init,
      
      ssp_pred_family_lookup =
        family_init,
      
      ssp_pred_genus_lookup =
        genus_init,
      
      ssp_pred_species_lookup =
        species_init,
      
      sigma_phylum = 5,
      
      sigma_class = 7,
      
      sigma_order = 8,
      
      sigma_family = 10,
      
      sigma_genus = 12,
      
      sigma_species = 15
      
    )
    
  }
  
  
  
  
  fit <- rstan::stan(
    file = "hierarchical_ssp.stan",
    data = stan_data,
    init = init_fun,
    chains = 3,
    iter = 2000, 
    warmup = 1000,
    cores = 3,
    refresh = 100)
  
  return(list(
    fit           = fit,
    genus_eezs    = genus_eezs,
    family_eezs   = family_eezs,
    order_eezs    = order_eezs,
    class_eezs    = class_eezs,
    phylum_eezs   = phylum_eezs,
    kingdom_eezs  = kingdom_eezs,
    genus         = genus,
    family        = family,
    order         = order,
    class         = class,
    phylum        = phylum,
    kingdom       = kingdoms
  ))
  
}