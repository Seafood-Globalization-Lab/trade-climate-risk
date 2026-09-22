# Function name: build_taxa_lookup.R

# Function purpose: compile averages across all taxa / spatial combinations
# (for example, averages for Actinopterygii at EEZ & Actinopterygii at region)
# into a lookup table that will another function down the pipeline will use
# to gap fill catch projections in consumption data.

build_taxa_lookup <- function(df, ecoregion, num_species) {
  tax_levels <- c("genus", "family", "order", "class", "phylum", "kingdom")
  
  # For each taxa in an EEZ, compute the average ssp126 / 585 value for all species within in the taxa
  
  # ---------------------------------------------------------------------------
  # EEZ-level averages
  # ---------------------------------------------------------------------------
  
  eez_gapfills_unwgt <- generate_mcp_averages(df = df,
                                              method = "eez",
                                              averaging = "unweighted",
                                              ecoregion = ecoregion,
                                              num_species = num_species)
  
  eez_gapfills_wgt <- generate_mcp_averages(df = df,
                                            method = "eez",
                                            averaging = "weighted",
                                            ecoregion = ecoregion,
                                            num_species = num_species)
  
  # ---------------------------------------------------------------------------
  # Realm-level averages
  # ---------------------------------------------------------------------------
  
  realm_gapfills_unwgt <- generate_mcp_averages(df = df,
                                                method = "realm",
                                                averaging = "unweighted",
                                                ecoregion = ecoregion,
                                                num_species = num_species)
  
  realm_gapfills_wgt <- generate_mcp_averages(df = df,
                                              method = "realm",
                                              averaging = "weighted",
                                              ecoregion = ecoregion,
                                              num_species = num_species)
  
  # ---------------------------------------------------------------------------
  # Region-level averages
  # ---------------------------------------------------------------------------
  region_gapfills_unwgt <- generate_mcp_averages(df = df,
                                                 method = "region",
                                                 averaging = "unweighted",
                                                 ecoregion = ecoregion,
                                                 num_species = num_species)
  
  region_gapfills_wgt <- generate_mcp_averages(df = df,
                                               method = "region",
                                               averaging = "weighted",
                                               ecoregion = ecoregion,
                                               num_species = num_species)
  
  # ---------------------------------------------------------------------------
  # Global-level averages
  # ---------------------------------------------------------------------------
  
  global_gapfills_unwgt <- generate_mcp_averages(df = df,
                                                 method = "global",
                                                 averaging = "unweighted",
                                                 ecoregion = ecoregion,
                                                 num_species = num_species)
  
  global_gapfills_wgt <- generate_mcp_averages(df = df,
                                               method = "global",
                                               averaging = "weighted",
                                               ecoregion = ecoregion,
                                               num_species = num_species)
  
  return(list(
    eez_gapfills_unwgt = eez_gapfills_unwgt,
    eez_gapfills_wgt = eez_gapfills_wgt,
    region_gapfills_unwgt = region_gapfills_unwgt,
    region_gapfills_wgt = region_gapfills_wgt,
    realm_gapfills_unwgt = realm_gapfills_unwgt,
    realm_gapfills_wgt = realm_gapfills_wgt,
    global_gapfills_unwgt = global_gapfills_unwgt,
    global_gapfills_wgt = global_gapfills_wgt
  ))
  
}