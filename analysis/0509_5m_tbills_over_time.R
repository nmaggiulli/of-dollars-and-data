cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(tidyverse)

folder_name <- "0509_5m_tbills_over_time"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #
# =====================================================================
# Real income from 1-year Treasury bills, 1980 -> present
# Tidyverse | FRED data: CPIAUCNS (monthly CPI) + DGS1 (1-yr Treasury)
#
# Question answered: how much income would $X of TODAY's purchasing power
# have thrown off if parked in 1-year T-bills at each point back to 1980,
# adjusting for inflation?
# =====================================================================

# ------------------------------- EDIT ME -----------------------------
principal_today <- 5e6                       # $ amount, in TODAY's money
start_date      <- as_date("1987-01-01")     # how far back to go
start_year      <- year(start_date)
cpi_path        <- paste0(importdir, "/", folder_name, "/CPIAUCNS.xlsx")         
tbill_path      <- paste0(importdir, "/", folder_name, "/DGS1.xlsx")
# ---------------------------------------------------------------------

# (1) IMPORT ----------------------------------------------------------
# FRED .xlsx files put a README on the first sheet; the actual data is on
# the 2nd sheet ("Monthly" / "Daily") with a clean 2-column header
# (observation_date + series id). Blank days read in as NA, not ".".
cpi_raw <- read_excel(cpi_path, sheet = "Monthly") |>
  rename(date = observation_date, cpi = CPIAUCNS) |>
  mutate(date = as_date(date))

tbill_raw <- read_excel(tbill_path, sheet = "Daily") |>
  rename(date = observation_date, yield_1yr = DGS1) |>
  mutate(date = as_date(date))

# DGS1 is DAILY (NA on weekends/holidays); CPI is monthly. Collapse the
# yield to a monthly average so the two series line up. Swap mean() for
# last()/first() if you'd rather use a point-in-time monthly rate.
tbill_monthly <- tbill_raw |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(yield_1yr = mean(yield_1yr, na.rm = TRUE), .groups = "drop")


# (2) MERGE -----------------------------------------------------------
# CPI is the limiting monthly series (a 1-yr yield exists for every month),
# and we need CPI for every calc, so join the yield onto CPI. CPI is
# released ~1 month in arrears, so the last row is the most recent month
# that has BOTH a CPI print and a yield (currently May 2026).
merged <- cpi_raw |>
  left_join(tbill_monthly, by = "date") |>
  arrange(date)


# (3) BUILD THE REAL-INCOME SERIES ------------------------------------
# "Today's" price level = the latest available CPI print.
cpi_latest <- merged |>
  filter(!is.na(cpi)) |>
  slice_max(date, n = 1) |>
  pull(cpi)

result <- merged |>
  mutate(
    # Trailing 12-month inflation. Computed BEFORE the 1980 filter so the
    # 1980 rows have their 1979 lag available. (CPI is gap-free monthly,
    # so lag(.,12) is exactly 12 months.)
    inflation_yoy = cpi / lag(cpi, 12) - 1,
    
    y_nom      = yield_1yr / 100,                          # nominal yield, decimal
    real_yield = (1 + y_nom) / (1 + inflation_yoy) - 1,    # Fisher-exact real yield
    
    # --- principal, viewed two ways ---
    principal_today_dollars  = principal_today,            # $5M held constant in today's $
    principal_period_dollars = principal_today * cpi / cpi_latest,
    #   ^ how much money had the SAME purchasing power as $5M-today, in
    #     that period's dollars (e.g. ~$1.16M in early 1980). This is the
    #     "real value of $5M" column you asked for.
    
  ) |>
  filter(date >= start_date) |>
  transmute(
    date,
    yield_1yr_pct                 = yield_1yr,            # nominal 1-yr yield, %
    inflation_yoy_pct             = inflation_yoy * 100,  # trailing 12-mo CPI, %
    real_yield_pct                = real_yield * 100,     # real 1-yr yield, %
    principal_today_dollars,                              # constant $5M (today's $)
    principal_period_dollars,                             # "real value of $5M" at each date
  )

first_value <- pull(result[1, "principal_period_dollars"])

to_plot <- result

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/tbill_1yr_rate_", start_year, ".jpeg")
source_string <- "Source: FRED (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x = date, y = yield_1yr_pct/100)) +
  geom_line() +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("1-Year U.S. Treasury Bill Rate")) +
  labs(x = "Year" , y = "1-Year Rate",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/value_5m_real_", start_year, ".jpeg")
source_string <- "Source: FRED (OfDollarsAndData.com)"

plot <- ggplot(to_plot, aes(x = date, y = principal_period_dollars)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Inflation-Adjusted Value of $5M")) +
  labs(x = "Year" , y = "Real Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

to_plot2 <- to_plot %>%
              mutate(nominal_income = first_value*yield_1yr_pct/100,
                real_income = nominal_income*(principal_today_dollars/principal_period_dollars)) %>%
              filter(month(date) == 1)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/tbill_real_income_2026_", start_year, ".jpeg")
source_string <- "Source: FRED (OfDollarsAndData.com)"

plot <- ggplot(to_plot2, aes(x = date, y = real_income)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Inflation-Adjusted Income of $1.66M in\n1-Year U.S. Treasury Bills")) +
  labs(x = "Year" , y = "Real Income\n(2026 Dollars)",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #