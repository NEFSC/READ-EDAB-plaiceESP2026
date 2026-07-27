# map ----
us <- geodata::gadm(
  country = "USA",
  level = 1,
  resolution = 2,
  path = here::here("05_images")
)

png(
  here::here("data-raw/2026", "{{ species }}_map.png"),
  width = 6,
  height = 6,
  units = "in",
  res = 300
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

create_shp <- function(strata, orig_shp = shp) {
  shp_out <- orig_shp[orig_shp$STRATUMA %in% strata, ] |>
    terra::aggregate()
  # add dummy attribute so it works with edab_utils
  shp_out$region <- "stock_area"

  return(shp_out)
}

species_shp <- create_shp(
  strata = strata,
  orig_shp = shp
)


terra::plot(species_shp, col = "lightblue")
terra::plot(us, add = TRUE)
