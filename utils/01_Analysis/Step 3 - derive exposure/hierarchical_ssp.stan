data {
  
  // Observed error (fixed)
  int<lower=0> sigma_obs;
  
  // Lookup table sizes
  int<lower=1> kingdom_combination_nrow;
  int<lower=1> phylum_combination_nrow;
  int<lower=1> class_combination_nrow;
  int<lower=1> order_combination_nrow;
  int<lower=1> family_combination_nrow;
  int<lower=1> genus_combination_nrow;
  int<lower=1> species_combination_nrow;
  
  // Hierarchy lookup indices
  array[phylum_combination_nrow]
    int<lower=1, upper=kingdom_combination_nrow>
    phylum_to_kingdom_lookup;
  array[class_combination_nrow]
    int<lower=1, upper=phylum_combination_nrow>
    class_to_phylum_lookup;
  array[order_combination_nrow]
    int<lower=1, upper=class_combination_nrow>
    order_to_class_lookup;
  array[family_combination_nrow]
    int<lower=1, upper=order_combination_nrow>
    family_to_order_lookup;
  array[genus_combination_nrow]
    int<lower=1, upper=family_combination_nrow>
    genus_to_family_lookup;
  array[species_combination_nrow]
    int<lower=1, upper=genus_combination_nrow>
    species_to_genus_lookup;
  
  // Observed ssp_obs values
  vector[species_combination_nrow] ssp_obs;
  
}

parameters {
  
  // Kingdom (top-level, no parent)
  vector[kingdom_combination_nrow] ssp_pred_kingdom_lookup;
  
  // Non-centered raw offsets
  vector[phylum_combination_nrow]  z_phylum;
  vector[class_combination_nrow]   z_class;
  vector[order_combination_nrow]   z_order;
  vector[family_combination_nrow]  z_family;
  vector[genus_combination_nrow]   z_genus;
  vector[species_combination_nrow] z_species;
  
  // Scale parameters
  real<lower=0> sigma_phylum;
  real<lower=0> sigma_class;
  real<lower=0> sigma_order;
  real<lower=0> sigma_family;
  real<lower=0> sigma_genus;
  real<lower=0> sigma_species;
  
}

transformed parameters {
  
  // Reconstructed on original scale
  vector[phylum_combination_nrow]  ssp_pred_phylum_lookup;
  vector[class_combination_nrow]   ssp_pred_class_lookup;
  vector[order_combination_nrow]   ssp_pred_order_lookup;
  vector[family_combination_nrow]  ssp_pred_family_lookup;
  vector[genus_combination_nrow]   ssp_pred_genus_lookup;
  vector[species_combination_nrow] ssp_pred_species_lookup;
  
  // Phylum = kingdom parent + scaled offset
  for (loop in 1:phylum_combination_nrow)
    ssp_pred_phylum_lookup[loop] =
      ssp_pred_kingdom_lookup[phylum_to_kingdom_lookup[loop]]
      + z_phylum[loop] * sigma_phylum;
  
  // Class = phylum parent + scaled offset
  for (loop in 1:class_combination_nrow)
    ssp_pred_class_lookup[loop] =
      ssp_pred_phylum_lookup[class_to_phylum_lookup[loop]]
      + z_class[loop] * sigma_class;
  
  // Order = class parent + scaled offset
  for (loop in 1:order_combination_nrow)
    ssp_pred_order_lookup[loop] =
      ssp_pred_class_lookup[order_to_class_lookup[loop]]
      + z_order[loop] * sigma_order;
  
  // Family = order parent + scaled offset
  for (loop in 1:family_combination_nrow)
    ssp_pred_family_lookup[loop] =
      ssp_pred_order_lookup[family_to_order_lookup[loop]]
      + z_family[loop] * sigma_family;
  
  // Genus = family parent + scaled offset
  for (loop in 1:genus_combination_nrow)
    ssp_pred_genus_lookup[loop] =
      ssp_pred_family_lookup[genus_to_family_lookup[loop]]
      + z_genus[loop] * sigma_genus;
  
  // Species = genus parent + scaled offset
  for (loop in 1:species_combination_nrow)
    ssp_pred_species_lookup[loop] =
      ssp_pred_genus_lookup[species_to_genus_lookup[loop]]
      + z_species[loop] * sigma_species;
  
}

model {
  
  // Kingdom prior (weakly informative)
  ssp_pred_kingdom_lookup ~ normal(0, 30);
  
  // Raw offsets — standard normal
  z_phylum  ~ normal(0, 1);
  z_class   ~ normal(0, 1);
  z_order   ~ normal(0, 1);
  z_family  ~ normal(0, 1);
  z_genus   ~ normal(0, 1);
  z_species ~ normal(0, 1);
  
  // Half-normal priors on scale parameters
  sigma_phylum  ~ normal(0, 5);
  sigma_class   ~ normal(0, 5);
  sigma_order   ~ normal(0, 5);
  sigma_family  ~ normal(0, 5);
  sigma_genus   ~ normal(0, 5);
  sigma_species ~ normal(0, 5);
  
  // Likelihood
  ssp_obs ~ normal(ssp_pred_species_lookup, sigma_obs);
  
}
