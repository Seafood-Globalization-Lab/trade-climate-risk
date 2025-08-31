get_invasives <- function(introductions_data, species_codes_data) {
  # Join territory corrections to fb/slb data and convert country names to iso3c.
  introductions_data %>%
    filter(Invasive == 1) %>%
    rename(country = "TO") %>%
    left_join(species_codes_data, by = "SpecCode") %>%
    left_join(territory_corrections %>% 
                select(-associated_country_iso3c,
                       -fb_slb_territories_iso3c), 
              by = c("country" = 
                       "fb_slb_unmatched_territories")) %>%
    mutate(iso3c = 
             case_when(!is.na(associated_territory_country_iso3c) ~ 
                         associated_territory_country_iso3c,
                       is.na(associated_territory_country_iso3c) ~
                         countrycode(country, origin = "country.name", destination = "iso3c"),
                       TRUE ~ NA_character_)) %>%
    # FIXIT: Convert territory iso3c to country iso3c (update to country standardization scrit later)
    left_join(territory_corrections %>% select(-associated_territory_country_iso3c, -fb_slb_unmatched_territories),
              by = c("iso3c" = "fb_slb_territories_iso3c")) %>%
    mutate(iso3c = case_when(!is.na(associated_country_iso3c) ~ associated_country_iso3c,
                             TRUE ~ iso3c)) %>%
    select(c("scientificname" = "ScientificName", country, "invasive" = "Invasive", iso3c)) %>%
    mutate(scientificname = str_to_lower(scientificname)) %>%
    filter(!is.na(scientificname),
           !is.na(iso3c)) %>%
    # Clean scientific names so that they match ARTIS
    left_join(sciname_corrections, by = c("scientificname" = "fb_slb_scientific_name")) %>%
    mutate(scientificname = case_when(
      !is.na(new_sciname) ~ new_sciname,
      TRUE ~ scientificname
    )) %>%
    filter(scientificname %in% artis_scinames) %>%
    distinct(scientificname, iso3c) %>%
    mutate(invasive = factor(1))
  
}
