cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(scales)

########################## Start Program Here ######################### #

n_years_working <- 40
starting_income <- 50000
discount_rate   <- 0.03
savings_rate    <- 0.1
annual_return   <- 0.05

assets <- data.frame(matrix(NA, nrow = n_years_working+1, ncol = 0))

for (i in 1:(n_years_working+1)){
  if (i == 1){
    assets[i, "financial_assets"] <- 0
    assets[i, "human_capital"]    <- sum(starting_income / ((1 + discount_rate)^(seq(1, n_years_working))))
  } else if (i == (n_years_working+1)){
    assets[i, "financial_assets"] <- starting_income * savings_rate + (assets[(i-1), "financial_assets"] * (1 + annual_return))
    assets[i, "human_capital"]    <- 0
  } else if (i > 1){
    assets[i, "financial_assets"] <- starting_income * savings_rate + (assets[(i-1), "financial_assets"] * (1 + annual_return))
    assets[i, "human_capital"]    <- sum(starting_income / ((1 + discount_rate)^(seq(1, n_years_working-i+1))))
  }
    assets[i, "year"] <- (i-1)
}

# Convert wide to long so I can plot both the financial assets and human capital side by side
assets_long <- gather(assets, `financial_assets`, `human_capital`, key = "asset_type", value = "value")

# Function to find a rounded max/min based on the specifications of y_unit
create_max_min <- function(x, unit, ceilfloor) {
  ceilfloor(x/unit)*unit
}

# Set the y_max dynamically
y_max <- create_max_min(max(assets_long$value), 100000, ceiling)

ggplot(data = assets_long, aes(x = year, y = value, col = asset_type, fill = asset_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1", guide = FALSE) +
  scale_colour_brewer(palette = "Set1", guide = FALSE) +
  scale_y_continuous(label = dollar, breaks = seq(0, y_max, 200000), limits = c(0, y_max)) +
  of_dollars_and_data_theme +
  labs(x = "Years" , y = "Value ($)")

# ############################  End  ################################## #