data {
  
  // Observed error (fixed)
  int<lower=0> sigma_obs;
  
  // Lookup table sizes
  int<lower=1> kingdom_combination_nrow_global;
  int<lower=1> phylum_combination_nrow_global;
  int<lower=1> class_combination_nrow_global;
  int<lower=1> order_combination_nrow_global;
  int<lower=1> family_combination_nrow_global;
  int<lower=1> genus_combination_nrow_global;
  int<lower=1> species_combination_nrow_global;
  
  int<lower=1> kingdom_combination_nrow_region;
  int<lower=1> phylum_combination_nrow_region;
  int<lower=1> class_combination_nrow_region;
  int<lower=1> order_combination_nrow_region;
  int<lower=1> family_combination_nrow_region;
  int<lower=1> genus_combination_nrow_region;
  int<lower=1> species_combination_nrow_region;
  
  int<lower=1> kingdom_combination_nrow_realm;
  int<lower=1> phylum_combination_nrow_realm;
  int<lower=1> class_combination_nrow_realm;
  int<lower=1> order_combination_nrow_realm;
  int<lower=1> family_combination_nrow_realm;
  int<lower=1> genus_combination_nrow_realm;
  int<lower=1> species_combination_nrow_realm;
  
  int<lower=1> kingdom_combination_nrow_eez;
  int<lower=1> phylum_combination_nrow_eez;
  int<lower=1> class_combination_nrow_eez;
  int<lower=1> order_combination_nrow_eez;
  int<lower=1> family_combination_nrow_eez;
  int<lower=1> genus_combination_nrow_eez;
  int<lower=1> species_combination_nrow_eez;
  
  // Hierarchy lookup indices FIXIT: update all names to have spatial groupings
  array[phylum_combination_nrow_global]
    int<lower=1, upper=kingdom_combination_nrow_global>
    phylum_to_kingdom_lookup_global;
  array[class_combination_nrow_global]
    int<lower=1, upper=phylum_combination_nrow_global>
    class_to_phylum_lookup_global;
  array[order_combination_nrow_global]
    int<lower=1, upper=class_combination_nrow_global>
    order_to_class_lookup_global;
  array[family_combination_nrow_global]
    int<lower=1, upper=order_combination_nrow_global>
    family_to_order_lookup_global;
  array[genus_combination_nrow_global]
    int<lower=1, upper=family_combination_nrow_global>
    genus_to_family_lookup_global;
  array[species_combination_nrow_global]
    int<lower=1, upper=genus_combination_nrow_global>
    species_to_genus_lookup_global;
    
  array[phylum_combination_nrow_region]
    int<lower=1, upper=kingdom_combination_nrow_region>
    phylum_to_kingdom_lookup_region;
  array[class_combination_nrow_region]
    int<lower=1, upper=phylum_combination_nrow_region>
    class_to_phylum_lookup_region;
  array[order_combination_nrow_region]
    int<lower=1, upper=class_combination_nrow_region>
    order_to_class_lookup_region;
  array[family_combination_nrow_region]
    int<lower=1, upper=order_combination_nrow_region>
    family_to_order_lookup_region;
  array[genus_combination_nrow_region]
    int<lower=1, upper=family_combination_nrow_region>
    genus_to_family_lookup_region;
  array[species_combination_nrow_region]
    int<lower=1, upper=genus_combination_nrow_region>
    species_to_genus_lookup_region;
    
  array[phylum_combination_nrow_realm]
    int<lower=1, upper=kingdom_combination_nrow_realm>
    phylum_to_kingdom_lookup_realm;
  array[class_combination_nrow_realm]
    int<lower=1, upper=phylum_combination_nrow_realm>
    class_to_phylum_lookup_realm;
  array[order_combination_nrow_realm]
    int<lower=1, upper=class_combination_nrow_realm>
    order_to_class_lookup_realm;
  array[family_combination_nrow_realm]
    int<lower=1, upper=order_combination_nrow_realm>
    family_to_order_lookup_realm;
  array[genus_combination_nrow_realm]
    int<lower=1, upper=family_combination_nrow_realm>
    genus_to_family_lookup_realm;
  array[species_combination_nrow_realm]
    int<lower=1, upper=genus_combination_nrow_realm>
    species_to_genus_lookup_realm;
    
  array[phylum_combination_nrow_eez]
    int<lower=1, upper=kingdom_combination_nrow_eez>
    phylum_to_kingdom_lookup_eez;
  array[class_combination_nrow_eez]
    int<lower=1, upper=phylum_combination_nrow_eez>
    class_to_phylum_lookup_eez;
  array[order_combination_nrow_eez]
    int<lower=1, upper=class_combination_nrow_eez>
    order_to_class_lookup_eez;
  array[family_combination_nrow_eez]
    int<lower=1, upper=order_combination_nrow_eez>
    family_to_order_lookup_eez;
  array[genus_combination_nrow_eez]
    int<lower=1, upper=family_combination_nrow_eez>
    genus_to_family_lookup_eez;
  array[species_combination_nrow_eez]
    int<lower=1, upper=genus_combination_nrow_eez>
    species_to_genus_lookup_eez;
  
  // Observed ssp_obs values
  vector[species_combination_nrow_eez + species_combination_nrow_realm +
  species_combination_nrow_region + species_combination_nrow_global] ssp_obs;
  
  // Data for initializing model
  int<lower = 1> num_eezs;                            // Number of EEZs
  array[num_eezs] int idx_length_by_eez;
  
  int<lower = 1> species_init_nrow;                  // Number of species to draw random posterior samples from
  vector[species_init_nrow] kingdom_prop_within_eez; // proportion of EEZ consumption that is dedicated to
                                                     // kingdom of capture
                                                     
  vector[species_init_nrow] eezs;                             // list of EEZs in init data
                                                 
  vector[num_eezs] kingdom_prop_across_EEZ_region;   // Share of total kingdom consumption weight that each EEZ contributes toward
  
}

parameters { // FIXIT: needs to be changed to include new spatial groupings for parameters

  // Kingdom inits
  vector[kingdom_combination_nrow_region] ssp_pred_kingdom_lookup_region;
  vector[kingdom_combination_nrow_realm] ssp_pred_kingdom_lookup_realm;
  vector[kingdom_combination_nrow_eez] ssp_pred_kingdom_lookup_eez;
  
  // Species inits
  vector[species_init_nrow] species_inits;
  
  // Non-centered offsets
  array[phylum_combination_nrow_global] real<lower=0> z_phylum_global;
  array[class_combination_nrow_global] real<lower=0> z_class_global;
  array[order_combination_nrow_global] real<lower=0> z_order_global;
  array[family_combination_nrow_global] real<lower=0> z_family_global;
  array[genus_combination_nrow_global] real<lower=0> z_genus_global;
  array[species_combination_nrow_global] real<lower=0> z_species_global;
  // vector[phylum_combination_nrow_global]  z_phylum_global;
  // vector[class_combination_nrow_global]   z_class_global;
  // vector[order_combination_nrow_global]   z_order_global;
  // vector[family_combination_nrow_global]  z_family_global;
  // vector[genus_combination_nrow_global]   z_genus_global;
  // vector[species_combination_nrow_global] z_species_global;
  
  real<lower=0>  z_phylum_region;
  real<lower=0>   z_class_region;
  real<lower=0>   z_order_region;
  real<lower=0>  z_family_region;
  real<lower=0>   z_genus_region;
  real<lower=0> z_species_region;
  
  real<lower=0>  z_phylum_realm;
  real<lower=0>   z_class_realm;
  real<lower=0>   z_order_realm;
  real<lower=0>  z_family_realm;
  real<lower=0>   z_genus_realm;
  real<lower=0> z_species_realm;
  
  real<lower=0>  z_phylum_eez;
  real<lower=0>   z_class_eez;
  real<lower=0>   z_order_eez;
  real<lower=0>  z_family_eez;
  real<lower=0>   z_genus_eez;
  real<lower=0> z_species_eez;
  
  // Scale parameters
  array[phylum_combination_nrow_global] real<lower=0> sigma_phylum_global;
  array[class_combination_nrow_global] real<lower=0> sigma_class_global;
  array[order_combination_nrow_global] real<lower=0> sigma_order_global;
  array[family_combination_nrow_global] real<lower=0> sigma_family_global;
  array[genus_combination_nrow_global] real<lower=0> sigma_genus_global;
  array[species_combination_nrow_global] real<lower=0> sigma_species_global;
  // real<lower=0> sigma_phylum_global;
  // real<lower=0> sigma_class_global;
  // real<lower=0> sigma_order_global;
  // real<lower=0> sigma_family_global;
  // real<lower=0> sigma_genus_global;
  // real<lower=0> sigma_species_global;
    
  real<lower=0> sigma_phylum_region;
  real<lower=0> sigma_class_region;
  real<lower=0> sigma_order_region;
  real<lower=0> sigma_family_region;
  real<lower=0> sigma_genus_region;
  real<lower=0> sigma_species_region;
    
  real<lower=0> sigma_phylum_realm;
  real<lower=0> sigma_class_realm;
  real<lower=0> sigma_order_realm;
  real<lower=0> sigma_family_realm;
  real<lower=0> sigma_genus_realm;
  real<lower=0> sigma_species_realm;
    
  real<lower=0> sigma_phylum_eez;
  real<lower=0> sigma_class_eez;
  real<lower=0> sigma_order_eez;
  real<lower=0> sigma_family_eez;
  real<lower=0> sigma_genus_eez;
  real<lower=0> sigma_species_eez;
  
}

transformed parameters {
  
  // Reconstructed on original scale
  real ssp_pred_kingdom_lookup_global;
  // vector[kingdom_combination_nrow_global] ssp_pred_kingdom_lookup_global;
  vector[phylum_combination_nrow_global]  ssp_pred_phylum_lookup_global;
  vector[class_combination_nrow_global]   ssp_pred_class_lookup_global;
  vector[order_combination_nrow_global]   ssp_pred_order_lookup_global;
  vector[family_combination_nrow_global]  ssp_pred_family_lookup_global;
  vector[genus_combination_nrow_global]   ssp_pred_genus_lookup_global;
  vector[species_combination_nrow_global] ssp_pred_species_lookup_global;
  
  vector[phylum_combination_nrow_region]  ssp_pred_phylum_lookup_region;
  vector[class_combination_nrow_region]   ssp_pred_class_lookup_region;
  vector[order_combination_nrow_region]   ssp_pred_order_lookup_region;
  vector[family_combination_nrow_region]  ssp_pred_family_lookup_region;
  vector[genus_combination_nrow_region]   ssp_pred_genus_lookup_region;
  vector[species_combination_nrow_region] ssp_pred_species_lookup_region;
  
  vector[phylum_combination_nrow_realm]  ssp_pred_phylum_lookup_realm;
  vector[class_combination_nrow_realm]   ssp_pred_class_lookup_realm;
  vector[order_combination_nrow_realm]   ssp_pred_order_lookup_realm;
  vector[family_combination_nrow_realm]  ssp_pred_family_lookup_realm;
  vector[genus_combination_nrow_realm]   ssp_pred_genus_lookup_realm;
  vector[species_combination_nrow_realm] ssp_pred_species_lookup_realm;
  
  vector[phylum_combination_nrow_eez]  ssp_pred_phylum_lookup_eez;
  vector[class_combination_nrow_eez]   ssp_pred_class_lookup_eez;
  vector[order_combination_nrow_eez]   ssp_pred_order_lookup_eez;
  vector[family_combination_nrow_eez]  ssp_pred_family_lookup_eez;
  vector[genus_combination_nrow_eez]   ssp_pred_genus_lookup_eez;
  vector[species_combination_nrow_eez] ssp_pred_species_lookup_eez;
  
  vector[num_eezs] eez_totals; // vector to hold EEZ totals of within EEZ weighted averages
  vector[num_eezs] wgt_avg; // vector to hold weighted averages 
                            // (ultimately for between EEZ weighted avg ssp value)
                            // at global level which will start the model run
  
//////////////////////////////////////////////////////////////
////////////////      GLOBAL PREDICTIONS      ////////////////
//////////////////////////////////////////////////////////////
 
  // Kingdom
  // Here I calculate a weighted average of all species init predictions within
  // an EEZ, then across EEZs to get the weighted average prediction kingdom
  // ssp prediction. This will be propagated aross spatial and taxonomic
  // hierarchies
  
  // Initialize weights
  vector[species_init_nrow] weights;
  
  
  // Weight species mcmc samples by their proportion of consumption weight
  for (i in 1:species_init_nrow) {
    
    weights[i] = species_inits[i] * kingdom_prop_within_eez[i];
    
  }
  
  // Sum all the weights within an EEZ to get an EEZ projected change in chatch
  // eez_totals_stan <- rep(NA, num_eezs)
    for (k in 1:num_eezs) {
      
      # Set vector length that corresponds with how many EEZs match species data
      vector[idx_length_by_eez[k]] indexes_k;
      // indexes_k = rep(NA, idx_length_by_eez[k])
      int idx_iteration = 1;
      // idx_iteration = 1
      
      # Get the indexes
      for (i in 1:species_init_nrow) {
      
        if (eezs[i] == k) {
          
          indexes_k[idx_iteration] = weights[i];
          idx_iteration = idx_iteration + 1;
          
        }
        
      # For each EEZ, calculate the sum of each vector
      eez_totals[k] = sum(indexes_k);
        
      }
      
    }

  // fixit: stan doesn't like this. Need to fix to R
  // for (eez in 1:num_eezs) {
  //   eez_totals[eez] = sum(weights[eezs == eez]);
  // }
  
  real wgt_sum = 0;
  
  for (i in 1:num_eezs) {
    
    wgt_sum = wgt_sum + (eez_totals[i] * kingdom_prop_across_EEZ_region[i]);

  }
  
  ssp_pred_kingdom_lookup_global = wgt_sum;

  // ssp_pred_kingdom_lookup_global = sum(eez_totals * kingdom_prop_across_EEZ_region); // global
  
  // Phylum
  for (i in 1:phylum_combination_nrow_global) {

    // deviation ~ normal(ssp_pred_kingdom_lookup_global,1);
    // ssp_pred_phylum_lookup_global[i] = 
    //  deviation + z_phylum_global * sigma_phylum_global;

    
    ssp_pred_phylum_lookup_global[i] =
    ssp_pred_kingdom_lookup_global +
    + z_phylum_global[i] * sigma_phylum_global[i];
    
  }
  
  // Class
  for (i in 1:class_combination_nrow_global) {
    
    ssp_pred_class_lookup_global[i] = 
    ssp_pred_phylum_lookup_global[class_to_phylum_lookup_global[i]] + z_class_global[i] * sigma_class_global[i];
    
  }
  
  // Order
  for (i in 1:order_combination_nrow_global) {
    
    ssp_pred_order_lookup_global[i] = 
    ssp_pred_class_lookup_global[order_to_class_lookup_global[i]] + 
    + z_order_global[i] * sigma_order_global[i];
    
  }
  
  // Family
  for (i in 1:family_combination_nrow_global) {
    
    ssp_pred_family_lookup_global[i] = 
    ssp_pred_order_lookup_global[family_to_order_lookup_global[i]] + 
    + z_family_global[i] * sigma_family_global[i];
    
  }
  
  // Genus
  for (i in 1:genus_combination_nrow_global) {
    
    ssp_pred_genus_lookup_global[i] = 
    ssp_pred_family_lookup_global[genus_to_family_lookup_global[i]] + 
    + z_genus_global[i] * sigma_genus_global[i];
    
  }

  // Species
  for (i in 1:species_combination_nrow_global) {
    
    ssp_pred_species_lookup_global[i] = 
    ssp_pred_genus_lookup_global[species_to_genus_lookup_global[i]] + z_species_global[i] * sigma_species_global[i];
    
  }
  
//////////////////////////////////////////////////////////////
////////////////      REGION PREDICTIONS      ////////////////
//////////////////////////////////////////////////////////////

  // Phylum
  for (i in 1:phylum_combination_nrow_region) {
    
    ssp_pred_phylum_lookup_region[i] = 
    ssp_pred_kingdom_lookup_region[phylum_to_kingdom_lookup_region[i]] + z_phylum_region * sigma_phylum_region;
    
  }
  
  // Class
  for (i in 1:class_combination_nrow_region) {
    
    ssp_pred_class_lookup_region[i] = 
    ssp_pred_phylum_lookup_region[class_to_phylum_lookup_region[i]] + z_class_region * sigma_class_region;
    
  }
  
  // Order
  for (i in 1:order_combination_nrow_region) {
    
    ssp_pred_order_lookup_region[i] = 
    ssp_pred_class_lookup_region[order_to_class_lookup_region[i]] + z_order_region * sigma_order_region;
    
  }
  
  // Family
  for (i in 1:family_combination_nrow_region) {
    
    ssp_pred_family_lookup_region[i] = 
    ssp_pred_order_lookup_region[family_to_order_lookup_region[i]] + z_family_region * sigma_family_region;
    
  }
  
  // Genus
  for (i in 1:genus_combination_nrow_region) {
    
    ssp_pred_genus_lookup_region[i] = 
    ssp_pred_family_lookup_region[genus_to_family_lookup_region[i]] + z_genus_region * sigma_genus_region;
    
  }

  // Species
  for (i in 1:species_combination_nrow_region) {
    
    ssp_pred_species_lookup_region[i] = 
    ssp_pred_genus_lookup_region[species_to_genus_lookup_region[i]] + z_species_region * sigma_species_region;
    
  }


 
//////////////////////////////////////////////////////////////
////////////////       REALM PREDICTIONS      ////////////////
//////////////////////////////////////////////////////////////

  // Kingdom
  // ssp_pred_kingdom_lookup_realm ~ normal(ssp_pred_kingdom_lookup_global, 5); 

  // Phylum
  for (i in 1:phylum_combination_nrow_realm) {
    
    ssp_pred_phylum_lookup_realm[i] = 
    ssp_pred_kingdom_lookup_realm[phylum_to_kingdom_lookup_realm[i]] + z_phylum_realm * sigma_phylum_realm;
    
  }
  
  // Class
  for (i in 1:class_combination_nrow_realm) {
    
    ssp_pred_class_lookup_realm[i] = 
    ssp_pred_phylum_lookup_realm[class_to_phylum_lookup_realm[i]] + z_class_realm * sigma_class_realm;
    
  }
  
  // Order
  for (i in 1:order_combination_nrow_realm) {
    
    ssp_pred_order_lookup_realm[i] = 
    ssp_pred_class_lookup_realm[order_to_class_lookup_realm[i]] + z_order_realm * sigma_order_realm;
    
  }
  
  // Family
  for (i in 1:family_combination_nrow_realm) {
    
    ssp_pred_family_lookup_realm[i] = 
    ssp_pred_order_lookup_realm[family_to_order_lookup_realm[i]] + z_family_realm * sigma_family_realm;
    
  }
  
  // Genus
  for (i in 1:genus_combination_nrow_realm) {
    
    ssp_pred_genus_lookup_realm[i] = 
    ssp_pred_family_lookup_realm[genus_to_family_lookup_realm[i]] + z_genus_realm * sigma_genus_realm;
    
  }

  // Species
  for (i in 1:species_combination_nrow_realm) {
    
    ssp_pred_species_lookup_realm[i] = 
    ssp_pred_genus_lookup_realm[species_to_genus_lookup_realm[i]] + z_species_realm * sigma_species_realm;
    
  }
  
//////////////////////////////////////////////////////////////
////////////////       EEZ PREDICTIONS        ////////////////
//////////////////////////////////////////////////////////////  

  // Kingdom
  // ssp_pred_kingdom_lookup_eez ~ normal(ssp_pred_kingdom_lookup_global, 5); 

  // Phylum
  for (i in 1:phylum_combination_nrow_eez) {
    
    ssp_pred_phylum_lookup_eez[i] = 
    ssp_pred_kingdom_lookup_eez[phylum_to_kingdom_lookup_eez[i]] + z_phylum_eez * sigma_phylum_eez;
    
  }
  
  // Class
  for (i in 1:class_combination_nrow_eez) {
    
    ssp_pred_class_lookup_eez[i] = 
    ssp_pred_phylum_lookup_eez[class_to_phylum_lookup_eez[i]] + z_class_eez * sigma_class_eez;
    
  }
  
  // Order
  for (i in 1:order_combination_nrow_eez) {
    
    ssp_pred_order_lookup_eez[i] = 
    ssp_pred_class_lookup_eez[order_to_class_lookup_eez[i]] + z_order_eez * sigma_order_eez;
    
  }
  
  // Family
  for (i in 1:family_combination_nrow_eez) {
    
    ssp_pred_family_lookup_eez[i] = 
    ssp_pred_order_lookup_eez[family_to_order_lookup_eez[i]] + z_family_eez * sigma_family_eez;
    
  }
  
  // Genus
  for (i in 1:genus_combination_nrow_eez) {
    
    ssp_pred_genus_lookup_eez[i] = 
    ssp_pred_family_lookup_eez[genus_to_family_lookup_eez[i]] + z_genus_eez * sigma_genus_eez;
    
  }

  // Species
  for (i in 1:species_combination_nrow_eez) {
    
    ssp_pred_species_lookup_eez[i] = 
    ssp_pred_genus_lookup_eez[species_to_genus_lookup_eez[i]] + z_species_eez * sigma_species_eez;
    
  }

// final vector that will contain ssp predicted values
  vector[species_combination_nrow_eez + species_combination_nrow_realm +
  species_combination_nrow_region + species_combination_nrow_global] ssp_pred;

 ///////////// transfer matrix values into vector for use in likelihood model /////////////
 
 // Since concatenating in Stan isn't possible (thanks low-level programming langauges lol)
 // Here is some fancy footwork for concatenating the values together
 for (i in 1:(species_combination_nrow_eez + species_combination_nrow_realm +
  species_combination_nrow_region + species_combination_nrow_global)) {
    
    // Concatenate on EEZ predictions
    if (i <= species_combination_nrow_eez) {
      
      ssp_pred[i] = ssp_pred_species_lookup_eez[i];
      
    }
    
    // Concatenate on realm predictions
    if (i > species_combination_nrow_eez && i <= species_combination_nrow_eez + species_combination_nrow_realm) {
      
      ssp_pred[i] = ssp_pred_species_lookup_realm[i - species_combination_nrow_eez];
      
    }
    
    // Concatenate on region predictions
    if (i > species_combination_nrow_eez + species_combination_nrow_realm &&
    i <= species_combination_nrow_eez + species_combination_nrow_realm + species_combination_nrow_region) {
      
      ssp_pred[i] = ssp_pred_species_lookup_region[i - (species_combination_nrow_eez + species_combination_nrow_realm)];
      
    }
    
    // Concatenate on region predictions
    if (i > species_combination_nrow_eez + species_combination_nrow_realm + species_combination_nrow_region) {
      
      ssp_pred[i] = ssp_pred_species_lookup_global[i - (species_combination_nrow_eez + species_combination_nrow_realm + species_combination_nrow_region)];
      
    }
    
  }
  
}

model {
  
  // Species init priors (weakly informative)
  species_inits ~ normal(0,30);
  
  
  // Derive global kingdom estimates at region, realm, and EEZ level from global estimate
  ssp_pred_kingdom_lookup_region ~ normal(ssp_pred_kingdom_lookup_global, 5); // Kingdom inits at Region level
  ssp_pred_kingdom_lookup_realm ~ normal(ssp_pred_kingdom_lookup_global, 5); // Kingdom inits at Realm level
  ssp_pred_kingdom_lookup_eez ~ normal(ssp_pred_kingdom_lookup_global, 5); // Kingdom inits at EEZ level
  
  // Raw offsets global — standard normal
  z_phylum_global  ~ normal(0, 1);
  z_class_global   ~ normal(0, 1);
  z_order_global   ~ normal(0, 1);
  z_family_global  ~ normal(0, 1);
  z_genus_global   ~ normal(0, 1);
  z_species_global ~ normal(0, 1);
  
  z_phylum_region  ~ normal(0, 1);
  z_class_region   ~ normal(0, 1);
  z_order_region   ~ normal(0, 1);
  z_family_region  ~ normal(0, 1);
  z_genus_region   ~ normal(0, 1);
  z_species_region ~ normal(0, 1);
  
  z_phylum_realm  ~ normal(0, 1);
  z_class_realm   ~ normal(0, 1);
  z_order_realm   ~ normal(0, 1);
  z_family_realm  ~ normal(0, 1);
  z_genus_realm   ~ normal(0, 1);
  z_species_realm ~ normal(0, 1);
  
  z_phylum_eez  ~ normal(0, 1);
  z_class_eez   ~ normal(0, 1);
  z_order_eez   ~ normal(0, 1);
  z_family_eez  ~ normal(0, 1);
  z_genus_eez   ~ normal(0, 1);
  z_species_eez ~ normal(0, 1);
  
  // Half-normal priors on scale parameters
  sigma_phylum_global  ~ normal(0, 5);
  sigma_class_global   ~ normal(0, 5);
  sigma_order_global   ~ normal(0, 5);
  sigma_family_global  ~ normal(0, 5);
  sigma_genus_global   ~ normal(0, 5);
  sigma_species_global ~ normal(0, 5);
  
  sigma_phylum_region  ~ normal(0, 5);
  sigma_class_region   ~ normal(0, 5);
  sigma_order_region   ~ normal(0, 5);
  sigma_family_region  ~ normal(0, 5);
  sigma_genus_region   ~ normal(0, 5);
  sigma_species_region ~ normal(0, 5);
  
  sigma_phylum_realm  ~ normal(0, 5);
  sigma_class_realm   ~ normal(0, 5);
  sigma_order_realm   ~ normal(0, 5);
  sigma_family_realm  ~ normal(0, 5);
  sigma_genus_realm   ~ normal(0, 5);
  sigma_species_realm ~ normal(0, 5);
  
  sigma_phylum_eez  ~ normal(0, 5);
  sigma_class_eez   ~ normal(0, 5);
  sigma_order_eez   ~ normal(0, 5);
  sigma_family_eez  ~ normal(0, 5);
  sigma_genus_eez   ~ normal(0, 5);
  sigma_species_eez ~ normal(0, 5);
  
  // Likelihood
  ssp_obs ~ normal(ssp_pred, sigma_obs);
  
}
