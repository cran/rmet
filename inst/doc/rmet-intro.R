## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  fig.width = 7,
  fig.height = 4,
  fig.align = "center",
  dpi = 150,
  warning = FALSE,
  message = FALSE
)

## ----install, eval = FALSE----------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("rodrigosqrt3/rmet")

## ----library------------------------------------------------------------------
library(rmet)

## ----stations, eval = FALSE---------------------------------------------------
# stations <- inmet_stations(state = "RS")
# head(stations[, c("code", "name", "latitude", "longitude", "elevation")])

## ----download, eval = FALSE---------------------------------------------------
# # Downloads the 2023 archive (~70 MB) to the default cache directory.
# # Safe to re-run — skips files that are already complete.
# inmet_download(2023)

## ----cache, eval = FALSE------------------------------------------------------
# inmet_cache_status()

## ----read, eval = FALSE-------------------------------------------------------
# df <- inmet_read(
#   years      = 2023,
#   stations   = "A801",
#   start_date = "2023-01-01",
#   end_date   = "2023-12-31"
# )

## ----load-data----------------------------------------------------------------
df <- rmet_example
str(df[, c("datetime", "station_code", "state",
           "temp_dry_c", "precip_mm", "humid_rel_pct",
           "wind_speed_ms", "pressure_station_hpa")])

## ----daily-agg----------------------------------------------------------------
df$date <- as.Date(df$datetime)

daily <- aggregate(
  cbind(temp_dry_c, precip_mm, humid_rel_pct, wind_speed_ms) ~ date,
  data = df,
  FUN  = function(x) mean(x, na.rm = TRUE)
)

daily$temp_max <- aggregate(temp_dry_c ~ date, df, max, na.rm = TRUE)$temp_dry_c
daily$temp_min <- aggregate(temp_dry_c ~ date, df, min, na.rm = TRUE)$temp_dry_c
daily$precip   <- aggregate(precip_mm  ~ date, df, sum, na.rm = TRUE)$precip_mm
daily$month    <- as.integer(format(daily$date, "%m"))

## ----plot-temp, fig.cap = "Daily temperature range at Porto Alegre (A801) — 2023."----
col_max  <- "#E74C3C"
col_min  <- "#3498DB"
col_band <- "#F1948A"

oldpar <- par(mar = c(4, 4.5, 3, 1), family = "sans", cex.lab = 0.95, cex.axis = 0.85)

plot(
  daily$date, daily$temp_max,
  type = "n",
  ylim = range(c(daily$temp_min, daily$temp_max), na.rm = TRUE),
  xlab = "",
  ylab = "Temperature (\u00b0C)",
  main = "Daily Temperature Range \u2014 Porto Alegre (A801), 2023",
  axes = FALSE
)

polygon(
  c(daily$date, rev(daily$date)),
  c(daily$temp_max, rev(daily$temp_min)),
  col    = adjustcolor(col_band, alpha.f = 0.35),
  border = NA
)

lines(daily$date, daily$temp_max, col = col_max, lwd = 1.2)
lines(daily$date, daily$temp_min, col = col_min, lwd = 1.2)

axis(1,
     at     = seq(min(daily$date), max(daily$date), by = "2 months"),
     labels = format(seq(min(daily$date), max(daily$date), by = "2 months"), "%b"),
     las    = 1)
axis(2, las = 1)
box(col = "grey80")

legend(
  "topright",
  legend = c("Daily max", "Daily min"),
  col    = c(col_max, col_min),
  lwd    = 2,
  bty    = "n",
  cex    = 0.85
)

par(oldpar)

## ----plot-precip, fig.cap = "Daily accumulated precipitation at Porto Alegre (A801) — 2023."----
col_prec <- "#3498DB"

oldpar <- par(mar = c(4, 4.5, 3, 1), family = "sans", cex.lab = 0.95, cex.axis = 0.85)

barplot(
  daily$precip,
  col    = adjustcolor(col_prec, alpha.f = 0.75),
  border = NA,
  space  = 0,
  xlab   = "",
  ylab   = "Precipitation (mm)",
  main   = "Daily Precipitation \u2014 Porto Alegre (A801), 2023",
  axes   = FALSE
)

axis(2, las = 1)

# Month labels on x axis
month_starts <- which(!duplicated(format(daily$date, "%m")))
axis(1,
     at     = month_starts,
     labels = format(daily$date[month_starts], "%b"),
     tick   = FALSE)

box(col = "grey80")

par(oldpar)

## ----plot-seasonal, fig.cap = "Seasonal cycle of hourly dry-bulb temperature at Porto Alegre (A801) — 2023."----
col_box <- "#E74C3C"

df$month <- as.integer(format(df$datetime, "%m"))
month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")

oldpar <- par(mar = c(4, 4.5, 3, 1), family = "sans", cex.lab = 0.95, cex.axis = 0.85)

boxplot(
  temp_dry_c ~ month,
  data     = df,
  col      = adjustcolor(col_box, alpha.f = 0.40),
  border   = col_box,
  names    = month_labels,
  xlab     = "Month",
  ylab     = "Dry-bulb temperature (\u00b0C)",
  main     = "Temperature Seasonality \u2014 Porto Alegre (A801), 2023",
  outline  = FALSE,
  whisklty = 1,
  medlwd   = 2,
  axes     = FALSE
)

axis(1, at = 1:12, labels = month_labels, las = 1)
axis(2, las = 1)
box(col = "grey80")

par(oldpar)

## ----plot-wind, fig.cap = "Distribution of hourly wind speed at Porto Alegre (A801) — 2023."----
col_wind <- "#2C3E50"

ws <- df$wind_speed_ms[!is.na(df$wind_speed_ms) & df$wind_speed_ms >= 0]

oldpar <- par(mar = c(4, 4.5, 3, 1), family = "sans", cex.lab = 0.95, cex.axis = 0.85)

h <- hist(ws, breaks = 30, plot = FALSE)

plot(h,
     col    = adjustcolor(col_wind, alpha.f = 0.55),
     border = "white",
     xlab   = "Wind speed (m/s)",
     ylab   = "Frequency",
     main   = "Wind Speed Distribution \u2014 Porto Alegre (A801), 2023",
     axes   = FALSE)

abline(v   = mean(ws),
       col = "#E74C3C",
       lwd = 2,
       lty = 2)

axis(1, las = 1)
axis(2, las = 1)
box(col = "grey80")

legend("topright",
       legend = paste0("Mean: ", round(mean(ws), 1), " m/s"),
       col    = "#E74C3C",
       lwd    = 2,
       lty    = 2,
       bty    = "n",
       cex    = 0.85)

par(oldpar)

## ----filter, eval = FALSE-----------------------------------------------------
# # Only summer months (December–February), temperature and humidity
# summer <- inmet_read(
#   years      = 2023,
#   stations   = "A801",
#   start_date = "2023-12-01",
#   end_date   = "2023-12-31",
#   variables  = c("temp_dry_c", "humid_rel_pct")
# )
# 
# # Multiple stations across Rio Grande do Sul
# rs_stations <- inmet_read(
#   years    = 2023,
#   stations = c("A801", "A802", "A827"),
#   variables = c("temp_dry_c", "precip_mm")
# )

## ----extract, eval = FALSE----------------------------------------------------
# inmet_extract(
#   years   = 2023,
#   out_dir = file.path(tempdir(), "inmet_csv")
# )

## ----cache-mgmt, eval = FALSE-------------------------------------------------
# # See what is cached and how large each file is
# inmet_cache_status()
# 
# # Remove all cached archives to free disk space
# inmet_cache_clear()

