# 1. Load both NetCDF files as spatRasters
r1 <- terra::rast(here::here("01_inputs/oisst_1981_2000.nc"))
r2 <- terra::rast(here::here("01_inputs/oisst_2000_2026.nc"))

# 2. Combine (concatenate) the layers/time steps
combined_sst <- c(r1, r2)

# 3. Export to a single NetCDF file
terra::writeCDF(
  combined_sst,
  filename = "oisst_1981_2026jun.nc",
  overwrite = TRUE,
  varname = "sst", # e.g., "temperature"
  unit = "degC"
)


######################################
# convert longitude from 0-360 to -180-180 for the OISST data
sst_converted <- EDABUtilities::convert_2d_longitude_gridded(
  data = here::here('data-raw/inputs', 'oisst_1981_2026jun.nc')
)

## create stock shapefile from strata provided
shp <- terra::vect(here::here('data-raw/shapefiles', 'BTS_STRATA.shp'))

input_data <- list(
  plaice = list(
    species = "AMERICANPLAICE",
    strat = 'c(
   "09250", "09260", "09270", "09280", "09290",
   "09300", "09310", "09320", "09330", "09340",
   "09350", "09360", "01130", "01140", "01150",
   "01160", "01170", "01180", "01190", "01200",
   "01210", "01220", "01230", "01240", "01250",
   "01260", "01270", "01280", "01290", "01300",
   "01360", "01370", "01380", "01390", "01400",
   "03610", "03650", "03660"
   )'
  )
)

## functions ----
create_shp <- function(strata, orig_shp = shp) {
  shp_out <- orig_shp[orig_shp$STRATUMA %in% strata, ] |>
    terra::aggregate()
  # add dummy attribute so it works with edab_utils
  shp_out$region <- "stock_area"

  return(shp_out)
}

eval_spatial <- function(species, strata_nums) {
  exp <- knitr::knit_expand(
    file = here::here("data-raw/scripts/spatial_code_template_BTS.R"),
    species = species,
    strata = strata_nums
  )

  eval(parse(text = exp))
}

### run ----

purrr::map(
  input_data,
  ~ eval_spatial(species = .x$species, strata_nums = .x$strat)
)
