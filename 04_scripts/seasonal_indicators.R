### SEASONAL INDICATORS ----

### AMO ----
# 6 MONTHS PRIOR TO FALL SURVEY = APR - SEPT
fall_amo <- read.csv(here::here('01_inputs/amo_index.csv')) |>
  dplyr::filter(Year >= 1970) |>
  dplyr::filter(Month %in% 4:9) |>
  dplyr::group_by(Year) |>
  dplyr::summarize(DATA_VALUE = mean(SSTA, na.rm = TRUE)) |>
  dplyr::rename(YEAR = Year) |>
  dplyr::mutate(INDICATOR_NAME = "fall_amo")

NEesp2::plt_indicator(data = fall_amo,
                      ar = 1/4,
                      include_trends = TRUE)  

# 6 MONTHS PRIOR TO SPRING SURVEY = SEPT(year-1) - FEB
spring_amo <- read.csv(here::here('01_inputs/amo_index.csv')) |>
  dplyr::filter(Year >= 1970) |>
  dplyr::filter(Month %in% c(9, 10, 11, 12, 1, 2)) |>
  dplyr::mutate(Season_Start_Year = ifelse(Month %in% c(1, 2), Year - 1, Year)) |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(DATA_VALUE = mean(SSTA, na.rm = TRUE),
                   Months_Count = dplyr::n()) |> # keeps track of how many months went into the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "spring_amo")

NEesp2::plt_indicator(data = spring_amo,
                      ar = 1/4,
                      include_trends = TRUE)  


### GSI ----
# ANNUAL MEAN PRIOR TO FALL SURVEY = OCT(year-1) - SEPT
gsi <- ecodata::gsi |>
  tidyr::separate(Time, c("Year", "Month"), sep = "\\.") |>
  dplyr::mutate(Month = dplyr::if_else(Month == "1", "10", Month)) |> # has october as '1' instead of '10'
  dplyr::filter(Var == 'gulf stream index',
                Year >= 1970) |>
  dplyr::rename(INDICATOR_NAME = Var,
                YEAR = Year,
                MONTH = Month,
                DATA_VALUE = Value) |>
  dplyr::mutate(YEAR = as.numeric(.data$YEAR)) |>
  dplyr::select(YEAR, MONTH, INDICATOR_NAME, DATA_VALUE) |>
  dplyr::mutate(Season_Start_Year = ifelse(MONTH %in% c(1:8), YEAR - 1, YEAR)) |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE),
                   Months_Count = dplyr::n()) |>
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "gulf_stream_index")

NEesp2::plt_indicator(data = gsi,
                      ar = 1/4,
                      include_trends = TRUE) 



### Bottom Temperature ----
bt_data <- read.csv(url1) |>
  dplyr::bind_rows(read.csv(url2))

# SEASONAL (RECRUITMENT) = MAR - AUG
seasonal_bt <- bt_data |>
  dplyr::filter(YEAR >= 1970) |>
  dplyr::filter(MONTH %in% 3:8) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarize(DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE)) |>
  dplyr::mutate(INDICATOR_NAME = "seasonal_bt")

NEesp2::plt_indicator(data = seasonal_bt,
                      ar = 1/4,
                      include_trends = TRUE)  
  
# ANNUAL MEAN PRIOR TO FALL SURVEY = OCT(year-1) - SEPT
fall_bt <- bt_data |>
  dplyr::filter(YEAR >= 1970) |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(Season_Start_Year = ifelse(MONTH %in% c(1:8), YEAR - 1, YEAR)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE),
                   Days_Count = dplyr::n()) |> #daily value, how many days are included in the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "fall_bt")

NEesp2::plt_indicator(data = fall_bt,
                      ar = 1/4,
                      include_trends = TRUE)

# ANNUAL MEAN PRIOR TO SPRING SURVEY = MAR(year-1) - FEB
spring_bt <- bt_data |>
  dplyr::filter(YEAR >= 1970) |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(Season_Start_Year = ifelse(MONTH %in% c(1:2), YEAR - 1, YEAR)) |>
  dplyr::ungroup() |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE),
                   Days_Count = dplyr::n()) |> #daily value, how many days are included in the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "spring_bt") 

NEesp2::plt_indicator(data = spring_bt,
                      ar = 1/4,
                      include_trends = TRUE)

### SST ----
# SEASONAL = MAR - JUNE 
# ***2026 data not complete
seasonal_sst <- read.csv(here::here('01_inputs/AMERICANPLAICE_sst.csv')) |>
  dplyr::filter(YEAR >= 1970) |>
  dplyr::filter(MONTH %in% 3:6) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarize(DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE)) |>
  dplyr::mutate(INDICATOR_NAME = "seasonal_sst")

NEesp2::plt_indicator(data = seasonal_sst,
                      ar = 1/4,
                      include_trends = TRUE)  

### NAO ----
# 2 YEAR LAG (ANNUAL JAN-DEC)
nao <- read.csv(here::here('01_inputs/nao_index.csv')) |>
  dplyr::mutate(yearly_nao = rowMeans(dplyr::across(Jan:Dec), na.rm = TRUE))|>
  dplyr::filter(Year >= 1970) |>
  dplyr::rename(YEAR = Year) |>
  dplyr::mutate(INDICATOR_NAME = "yearly_nao") |>
  dplyr::select(YEAR, yearly_nao, INDICATOR_NAME) |>
  dplyr::arrange(YEAR) |>
  dplyr::mutate(NAO_lag2 = dplyr::lag(yearly_nao, n = 2)) |>
  dplyr::rename(DATA_VALUE = NAO_lag2)

NEesp2::plt_indicator(data = nao,
                      ar = 1/4,
                      include_trends = TRUE)  
