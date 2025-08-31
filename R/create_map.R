create_map <- function(data = data,
                       fill = "prop_missing",
                       country.col.name,
                       color.scale = TRUE) {
  
  # Define Robinson projection
  robinson_crs <- "+proj=robin +datum=WGS84"
  
  # Load and patch world map
  world_map <- ne_countries(scale = "medium", returnclass = "sf") %>%
    mutate(iso_a3 = case_when(
      sovereignt == "Norway" ~ "NOR",
      sovereignt == "France" ~ "FRA",
      TRUE ~ iso_a3
    )) %>%
    left_join(data, by = c("iso_a3" = country.col.name)) %>%
    st_transform(crs = robinson_crs)  # 🔥 Transform to Robinson
  
  # Transform ocean polygon too
  ocean_proj <- st_transform(ocean, crs = robinson_crs)
  
  ggplot() +
    geom_sf(data = ocean_proj, fill = "#8080ff80") +
    geom_sf(data = world_map, aes(fill = !!sym(fill)), color = "black") +
    {if (color.scale) scale_fill_viridis_c(option = "plasma") else scale_fill_manual(values = "gray80")} +
    coord_sf(crs = robinson_crs, expand = FALSE, default = FALSE) +  # 👈 important
    theme_void() +
    theme(
      legend.position = "bottom",
      panel.background = element_rect(fill = NA, colour = NA),  # 👈 this actually removes the gray
      plot.background  = element_rect(fill = NA, colour = NA),
      panel.grid = element_blank()
    )




  
  
  
}