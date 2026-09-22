# Function name: build_taxa_levels.R

# Function purpose: assign a taxa level based on the scientific name

build_taxa_levels <- function(sciname_df) {
  sciname_df %>%
    mutate(
      taxa_level = case_when(
        sciname == kingdom   ~ "kingdom",
        sciname == phylum    ~ "phylum",
        sciname == superclass ~ "superclass",
        sciname == class     ~ "class",
        sciname == order     ~ "order",
        sciname == family    ~ "family",
        sciname == subfamily ~ "subfamily",
        sciname == genus     ~ "genus",
        str_detect(sciname, " ") ~ "species"
      ),
      species = if_else(str_count(sciname, " ") == 1, word(sciname, 2), NA_character_)
    ) %>%
    relocate(species, .before = genus) %>%
    select(-common_name, -isscaap)
}