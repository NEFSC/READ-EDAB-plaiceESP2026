### BT annual (fall and spring)
url1 <- here::here("01_inputs/AMERICANPLAICE_hubert_bottomT.csv")
url2 <- here::here("01_inputs/AMERICANPLAICE_glorys_bottomT.csv")

bt_data <- read.csv(url1) |>
  dplyr::bind_rows(read.csv(url2))

fall_bt <- bt_data |>
  dplyr::filter(YEAR %in% 1970:2025) |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(
    Season_Start_Year = ifelse(MONTH %in% c(1:8), YEAR - 1, YEAR)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(
    DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE),
    Days_Count = dplyr::n()
  ) |> #daily value, how many days are included in the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "fall_bt")

spring_bt <- bt_data |>
  dplyr::filter(YEAR %in% 1970:2025) |>
  dplyr::group_by(YEAR) |>
  dplyr::mutate(
    Season_Start_Year = ifelse(MONTH %in% c(1:2), YEAR - 1, YEAR)
  ) |>
  dplyr::ungroup() |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(
    DATA_VALUE = mean(DATA_VALUE, na.rm = TRUE),
    Days_Count = dplyr::n()
  ) |> #daily value, how many days are included in the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "spring_bt")

bt_combined <- dplyr::bind_rows(fall_bt, spring_bt) |>
  dplyr::mutate(
    Season = dplyr::case_when(
      INDICATOR_NAME == "fall_bt" ~ "Fall",
      INDICATOR_NAME == "spring_bt" ~ "Spring"
    )
  )

NEesp2::plt_indicator(data = bt_combined, ar = 1 / 4, include_trends = TRUE) +
  ggplot2::facet_grid(Season ~ ., scales = "free_y") +
  ggplot2::scale_x_continuous(
    breaks = c(seq(1970, 2020, by = 10), 2025)
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

### Latitude (fall and spring)
dismap_fall <- read.csv(here::here('01_inputs/plaice_dismap_fall.csv')) |>
  dplyr::rename(DATA_VALUE = COG.Lat) |>
  dplyr::mutate(INDICATOR_NAME = "COG.Lat.fall")

dismap_spring <- read.csv(here::here('01_inputs/plaice_dismap_spring.csv')) |>
  dplyr::rename(DATA_VALUE = COG.Lat) |>
  dplyr::mutate(INDICATOR_NAME = "COG.Lat.spring")

lat_combined <- dplyr::bind_rows(dismap_fall, dismap_spring) |>
  dplyr::mutate(
    Season = dplyr::case_when(
      INDICATOR_NAME == "COG.Lat.fall" ~ "Fall",
      INDICATOR_NAME == "COG.Lat.spring" ~ "Spring"
    )
  )

NEesp2::plt_indicator(data = lat_combined, ar = 1 / 4, include_trends = TRUE) +
  ggplot2::facet_grid(Season ~ ., scales = "free_y")

### AMO (fall and spring)
fall_amo <- read.csv(here::here('01_inputs/unsmoothed_amo.csv')) |>
  dplyr::filter(Year >= 1970) |>
  dplyr::filter(Month %in% 4:9) |>
  dplyr::group_by(Year) |>
  dplyr::summarize(DATA_VALUE = mean(Value, na.rm = TRUE)) |>
  dplyr::rename(YEAR = Year) |>
  dplyr::mutate(INDICATOR_NAME = "fall_amo")

spring_amo <- read.csv(here::here('01_inputs/unsmoothed_amo.csv')) |>
  dplyr::filter(Year >= 1970) |>
  dplyr::filter(Month %in% c(9, 10, 11, 12, 1, 2)) |>
  dplyr::mutate(
    Season_Start_Year = ifelse(Month %in% c(1, 2), Year - 1, Year)
  ) |>
  dplyr::group_by(Season_Start_Year) |>
  dplyr::summarize(
    DATA_VALUE = mean(Value, na.rm = TRUE),
    Months_Count = dplyr::n()
  ) |> # keeps track of how many months went into the average
  dplyr::rename(YEAR = Season_Start_Year) |>
  dplyr::mutate(INDICATOR_NAME = "spring_amo")

amo_combined <- dplyr::bind_rows(fall_amo, spring_amo) |>
  dplyr::mutate(
    Season = dplyr::case_when(
      INDICATOR_NAME == "fall_amo" ~ "Fall",
      INDICATOR_NAME == "spring_amo" ~ "Spring"
    )
  )

NEesp2::plt_indicator(data = amo_combined, ar = 1 / 4, include_trends = TRUE) +
  ggplot2::facet_grid(Season ~ ., scales = "free_y") +
  ggplot2::scale_x_continuous(
    breaks = c(seq(1970, 2020, by = 10), 2022)
  ) +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 10)
  )
