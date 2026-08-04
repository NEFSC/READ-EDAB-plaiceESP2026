# map ----
us <- geodata::gadm(
  country = "USA",
  level = 1,
  resolution = 2,
  path = here::here("05_images")
)

############################
strata <- c(
  "09250",
  "09260",
  "09270",
  "09280",
  "09290",
  "09300",
  "09310",
  "09320",
  "09330",
  "09340",
  "09350",
  "09360",
  "01130",
  "01140",
  "01150",
  "01160",
  "01170",
  "01180",
  "01190",
  "01200",
  "01210",
  "01220",
  "01230",
  "01240",
  "01250",
  "01260",
  "01270",
  "01280",
  "01290",
  "01300",
  "01360",
  "01370",
  "01380",
  "01390",
  "01400",
  "03610",
  "03650",
  "03660"
)

shp <- terra::vect(here::here("05_images/BTS_Strata.shp"))

terra::plot(shp[shp$STRATUMA %in% strata, ], col = "lightblue")
terra::plot(us, add = TRUE)
terra::text(
  shp[shp$STRATUMA %in% strata, ],
  labels = "STRATUMA",
  halo = TRUE,
  inside = FALSE,
  pos = 3
)

## in ggplot

library(sf)
library(ggplot2)
library(ggrepel)

# Convert SpatVector to sf
shp_sf <- sf::st_as_sf(shp[shp$STRATUMA %in% strata, ])
us_sf <- sf::st_as_sf(us)

# Calculate centroids for label placement
shp_sf$label_x <- sf::st_coordinates(sf::st_centroid(shp_sf))[, 1]
shp_sf$label_y <- sf::st_coordinates(sf::st_centroid(shp_sf))[, 2]

plt <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = shp_sf, fill = "lightblue", color = "gray30") +
  ggplot2::geom_sf(data = us_sf, fill = "gray90", color = "black") +
  ggrepel::geom_text_repel(
    data = shp_sf,
    ggplot2::aes(x = label_x, y = label_y, label = STRATUMA),
    size = 3,
    max.overlaps = Inf, # Ensures all labels are drawn
    box.padding = 0.3, # Padding around text
    point.padding = 0.2, # Padding around centroid point
    # min.segment.length = 0.1, # Always draw leader lines when shifted
    # segment.size = 0.3,
    bg.color = "white", # Text halo effect
    bg.r = 0.15
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = NULL, y = NULL) +
  ggplot2::ylim(c(40, 44.5)) +
  ggplot2::xlim(c(-71, -65.5))
plt

## save
png(
  here::here("03_outputs", "plaice_map.png"),
  width = 6,
  height = 6,
  units = "in",
  res = 300
)
plt
dev.off()
