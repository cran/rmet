## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----library------------------------------------------------------------------
library(rmet)
hourly <- rmet_example
hourly[, c("station_code", "datetime", "temp_dry_c", "humid_rel_pct", "precip_mm")]

## ----summary------------------------------------------------------------------
summary(hourly)

## ----derive-------------------------------------------------------------------
hourly <- inmet_derive(hourly)
summary(as.data.frame(hourly[, c("vapor_pressure_kpa", "vpd_kpa", "heat_index_c")]))

## ----qc-----------------------------------------------------------------------
hourly <- inmet_qc(hourly, quiet = TRUE)
hourly <- inmet_derive(hourly)

## ----et0----------------------------------------------------------------------
daily <- inmet_daily(hourly)
daily <- inmet_et0(daily)
head(daily[, c("date", "temp_max_c", "temp_min_c", "radiation_mj_m2", "et0_mm")])

## ----monthly------------------------------------------------------------------
monthly <- inmet_monthly(daily)
monthly[, c("month", "n_days", "temp_mean_c", "precip_mm", "rain_days", "et0_mm")]

## ----balance------------------------------------------------------------------
monthly$balance_mm <- monthly$precip_mm - monthly$et0_mm
round(monthly$balance_mm, 1)

## ----balance2-----------------------------------------------------------------
monthly <- inmet_pet_thornthwaite(monthly)
balance <- inmet_water_balance(monthly, awc = 100)
round(balance[, c("precip_mm", "pet_mm", "storage_mm", "deficit_mm", "excess_mm")][2:13, ], 1)

## ----indices------------------------------------------------------------------
indices <- inmet_indices(daily)
t(indices[, c("txx", "tnn", "summer_days", "gdd", "prcptot", "rx1day", "cdd", "cwd")])

## ----fill---------------------------------------------------------------------
holes <- hourly
holes$temp_dry_c[100:102] <- NA
filled <- inmet_fill(holes, "temp_dry_c", method = "linear")
table(filled$temp_dry_c_filled, useNA = "ifany")

## ----locations----------------------------------------------------------------
inmet_nearest(c(-30.03, -23.55), c(-51.23, -46.63), n = 2, source = "bundled")[
  , c("location", "rank", "station_code", "station_name", "distance_km")]

## ----availability-------------------------------------------------------------
inmet_availability(dest_dir = file.path(tempdir(), "rmet-vignette-cache"))

