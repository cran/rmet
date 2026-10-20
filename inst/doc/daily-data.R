## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----library------------------------------------------------------------------
library(rmet)
x <- rmet_example

## ----utc----------------------------------------------------------------------
x$datetime[1:3]
attr(x$datetime, "tzone")

## ----local--------------------------------------------------------------------
local <- x$datetime[1:3]
attr(local, "tzone") <- "America/Sao_Paulo"
local

## ----compare-days-------------------------------------------------------------
utc_day <- as.Date(x$datetime)
local_day <- as.Date(x$datetime - 1800, tz = "America/Sao_Paulo")
mean(utc_day != local_day)

## ----compare-values-----------------------------------------------------------
wrong <- tapply(x$precip_mm, utc_day, sum)
right <- inmet_daily(x, min_hours = 1)
right <- setNames(right$precip_mm, right$date)
common <- intersect(names(wrong), names(right))
summary(abs(wrong[common] - right[common]))

## ----zones--------------------------------------------------------------------
inmet_timezone(c("RS", "MT", "AC", "AM"), c(-51.2, -56.1, -67.8, -69.9))

## ----completeness-------------------------------------------------------------
daily <- inmet_daily(x)
table(cut(daily$n_hours, c(0, 12, 19, 23, 25), right = TRUE))
daily[daily$n_hours < 20, c("date", "n_hours", "temp_max_c", "precip_mm")]

## ----completeness-table-------------------------------------------------------
head(inmet_completeness(x, variables = c("temp_dry_c", "precip_mm", "wind_speed_ms")))

## ----extremes-----------------------------------------------------------------
from_extremes <- inmet_daily(x)$temp_max_c
from_hourly <- inmet_daily(x[, setdiff(names(x), "temp_max_prev_c")])$temp_max_c
summary(from_extremes - from_hourly)

## ----qc-----------------------------------------------------------------------
checked <- inmet_qc(x)

## ----qc-spike-----------------------------------------------------------------
y <- x
y$temp_dry_c[1000] <- y$temp_dry_c[1000] + 15
flagged <- inmet_qc(y, quiet = TRUE)
flagged[998:1002, c("datetime", "temp_dry_c", "temp_max_prev_c", "temp_dry_c_flag")]

