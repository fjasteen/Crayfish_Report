# baseplot functie

get_baseplot <- function() {
  # Zorg dat libraries geladen zijn (sf, ggplot2)
  
  # Shapefiles inlezen (paden staan al in je config)
  # Gebruik de variabelen die je al hebt gedefinieerd
  vlaanderen    <- st_read(file_vlaanderen_grenzen, quiet = TRUE) %>% st_transform(31370)
  hoofdrivieren <- st_read(file_hoofdrivieren, quiet = TRUE) %>% st_transform(31370) %>% st_intersection(vlaanderen)
  kanalen       <- st_read(file_kanalen, quiet = TRUE) %>% st_transform(31370) %>% st_intersection(vlaanderen)
  
  # De plot constructie
  ggplot() +
    geom_sf(data = vlaanderen, fill= "#EEEEEE", size=0.2, colour= "black") +
    geom_sf(data = hoofdrivieren, size=0.3, colour="#6BA1D3") +
    geom_sf(data = kanalen, size=0.3, colour="#6BA1D3") +
    theme_void() +
    theme(legend.title = element_blank(), 
          legend.text = element_text(size=8, face="italic"), 
          legend.key.size = unit(0.2, "cm"),
          legend.position = "bottom",
          plot.title = element_text(face = "italic"))
}