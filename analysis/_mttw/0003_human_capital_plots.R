cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(ggrepel)
library(stringr)

folder_name <- "_mttw/0003_human_capital_plots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

n_years_working           <- 40
starting_income           <- 1000000
discount_rate             <- 0.03
savings_rate              <- 0.15
annual_return             <- 0.06
growth_rate               <- 0.0
starting_financial_assets <- 0

assets <- data.frame(matrix(NA, nrow = n_years_working+1, ncol = 0))

for (i in 1:(n_years_working+1)){
  if (i == 1){
    assets[i, "financial_assets"] <- starting_financial_assets
    assets[i, "human_capital"]    <- sum(starting_income * (1+growth_rate)^(seq(1, n_years_working)) / ((1 + discount_rate)^(seq(1, n_years_working))))
  } else if (i == (n_years_working+1)){
    assets[i, "financial_assets"] <- starting_income*(1+growth_rate)^(n_years_working+1) * savings_rate + (assets[(i-1), "financial_assets"] * (1 + annual_return))
    assets[i, "human_capital"]    <- 0
  } else if (i > 1){
    assets[i, "financial_assets"] <- starting_income*(1+growth_rate)^(i) * savings_rate + (assets[(i-1), "financial_assets"] * (1 + annual_return))
    assets[i, "human_capital"]    <- sum(starting_income * (1+growth_rate)^(seq(1, n_years_working-i+1)) / ((1 + discount_rate)^(seq(1, n_years_working-i+1))))
  }
    assets[i, "year"]            <- (i-1)
    assets[i, "alpha_financial"] <- (1/n_years_working)*i + 0.1
    assets[i, "alpha_human"]     <- 1 - (1/n_years_working)*i + 0.1
}

# Convert wide to long so I can plot both the financial assets and human capital side by side
assets_long <- gather(assets, 
                      `financial_assets`, 
                      `human_capital`, 
                      key = "asset_type", 
                      value = "value")

# Convert the alphas into their own columns afterwards as well
# Make sure to keep only the relevant pairings
assets_long <- gather(assets_long, 
                      `alpha_financial`, 
                      `alpha_human`, 
                      key = "alpha_type", 
                      value = "alpha") %>%
                filter((asset_type == "financial_assets" & alpha_type == "alpha_financial") | 
                        (asset_type == "human_capital" & alpha_type == "alpha_human"))

# Function to find a rounded max/min based on the specifications of y_unit
create_max_min <- function(x, unit, ceilfloor) {
  ceilfloor(x/unit)*unit
}

# Find the maximum financial asset value
fin_max <- max(filter(assets_long, asset_type == "financial_assets")$value)

# Set the file path
file_path = paste0(exportdir, "/", folder_name, "/human_capital_lifetime_plot.jpeg")

# Create plot
plot <- ggplot(data = assets_long, aes(x = year, y = value, fill = asset_type, alpha = alpha)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text_repel(data = filter(assets_long, 
                                        year == 3, 
                                        asset_type == "human_capital"),
            aes(x = year, 
                y= value,
                col = asset_type,
                label = str_wrap("人力資本現值", width = 18),
                family = "my_font"),
            nudge_x = 12) +
          geom_text_repel(data = filter(assets_long, 
                                year == (n_years_working-7), 
                                asset_type == "financial_assets"),
                  aes(x = year, 
                      y = value,
                      col = asset_type,
                      label = "金融資產",
                      family = "my_font"),
                  nudge_y = 5000000) +
          scale_alpha_continuous(guide = FALSE) +
          scale_fill_brewer(palette = "Set1", guide = FALSE) +
          scale_colour_brewer(palette = "Set1", guide = FALSE) +
          scale_y_continuous(label = dollar, breaks = seq(0, 24000000, 2000000), limits = c(0, 24000000)) +
          of_dollars_and_data_theme +
          labs(x = "年份" , y = "價值") +
          ggtitle(paste0("隨著年齡增長，你的金融資產應逐漸取代人力資本"))

  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  

# ############################  End  ################################## #