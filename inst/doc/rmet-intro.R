## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center",
  dpi = 150
)

## ----install, eval = FALSE----------------------------------------------------
# install.packages("rmet")

## ----library------------------------------------------------------------------
library(rmet)

## ----stations-----------------------------------------------------------------
rs <- inmet_stations(state = "RS", status = "Operante", source = "bundled")
head(rs[, c("station_code", "station_name", "latitude", "longitude", "start_date")])

## ----nearest------------------------------------------------------------------
inmet_nearest(latitude = -30.03, longitude = -51.23, n = 3, source = "bundled")[
  , c("station_code", "station_name", "distance_km", "start_date")
]

## ----map, eval = requireNamespace("sf", quietly = TRUE), fig.cap = "Operating automatic stations in Rio Grande do Sul, by elevation."----
plot(inmet_as_sf(rs)["elevation"], pch = 19, main = "")

## ----get, eval = FALSE--------------------------------------------------------
# poa <- inmet_get(
#   years = 2023,
#   stations = "A801",
#   variables = c("temp_dry_c", "temp_max_prev_c", "temp_min_prev_c", "precip_mm")
# )

## ----example------------------------------------------------------------------
dim(rmet_example)
head(rmet_example[, c("datetime", "temp_dry_c", "temp_max_prev_c", "precip_mm")])

## ----qc-----------------------------------------------------------------------
checked <- inmet_qc(rmet_example)
head(inmet_completeness(checked, variables = c("temp_dry_c", "precip_mm")), 3)

## ----daily--------------------------------------------------------------------
daily <- inmet_daily(checked)
head(daily[, c("date", "n_hours", "temp_max_c", "temp_min_c", "precip_mm")])

## ----plot-temp, fig.cap = "Daily maximum and minimum temperature at Porto Alegre (A801), 2023."----
daily <- daily[format(daily$date, "%Y") == "2023", ]
oldpar <- par(mar = c(4, 4.5, 2, 1))
plot(daily$date, daily$temp_max_c, type = "l", col = "#C0392B",
     ylim = range(daily$temp_min_c, daily$temp_max_c, na.rm = TRUE),
     xlab = "", ylab = "Temperature (°C)")
lines(daily$date, daily$temp_min_c, col = "#2471A3")
legend("topright", c("Maximum", "Minimum"), col = c("#C0392B", "#2471A3"),
       lwd = 1, bty = "n")
par(oldpar)

## ----plot-precip, fig.cap = "Monthly precipitation at Porto Alegre (A801), 2023."----
monthly <- tapply(daily$precip_mm, format(daily$date, "%m"), sum, na.rm = TRUE)
oldpar <- par(mar = c(4, 4.5, 2, 1))
barplot(monthly, names.arg = month.abb, col = "#5DADE2", border = NA,
        ylab = "Precipitation (mm)")
par(oldpar)

## ----steps, eval = FALSE------------------------------------------------------
# inmet_download(2020:2023)        # download (or update) archives
# inmet_cache_status()             # what is cached, and how large
# weather <- inmet_read(
#   years = 2020:2023,
#   stations = c("A801", "A832"),
#   start_date = "2020-06-01",
#   end_date = "2023-08-31",
#   variables = c("temp_dry_c", "precip_mm")
# )

