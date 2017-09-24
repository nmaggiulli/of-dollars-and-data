cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
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

########################## Start Program Here ######################### #

n_years_working           <- 40
starting_income           <- 50000
discount_rate             <- 0.03
savings_rate              <- 0.15
annual_return             <- 0.05
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

# Set the y_max dynamically
y_max <- create_max_min(max(assets_long$value), 100000, ceiling)

# Find the maximum financial asset value
fin_max <- max(filter(assets_long, asset_type == "financial_assets")$value)

# Set the file path
file_path = paste0(exportdir, "14-human-capital-plots/human-capital-lifetime-plot.jpeg")

# Create plot
plot <- ggplot(data = assets_long, aes(x = year, y = value, fill = asset_type, alpha = alpha)) +
          geom_bar(stat = "identity", position = "dodge") +
          geom_text_repel(data = filter(assets_long, 
                                        year == 3, 
                                        asset_type == "human_capital"),
            aes(x = year, 
                y= value,
                col = asset_type,
                label = str_wrap("Present Value of Human Capital", width = 18),
                family = "my_font"),
            nudge_x = 12) +
          geom_text_repel(data = filter(assets_long, 
                                year == (n_years_working-4), 
                                asset_type == "financial_assets"),
                  aes(x = year, 
                      y= value,
                      col = asset_type,
                      label = "Financial Assets",
                      family = "my_font"),
                  nudge_y = y_max/3) +
          scale_alpha_continuous(guide = FALSE) +
          geom_hline(data = assets_long, yintercept = fin_max, col = "red") +
          scale_fill_brewer(palette = "Set1", guide = FALSE) +
          scale_colour_brewer(palette = "Set1", guide = FALSE) +
          scale_y_continuous(label = dollar, breaks = seq(0, y_max, 200000), limits = c(0, y_max)) +
          of_dollars_and_data_theme +
          labs(x = "Years" , y = "Value (in real $)") +
          ggtitle(paste0("As You Age, Your Financial Assets\nShould Replace Your Human Capital"))

  # Add a source and note string for the plots
  source_string <- "Source:  Simulated data (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes a ", 
                          savings_rate*100,
                          "% savings rate, ",  
                          discount_rate*100,
                          "% discount rate, ",
                          annual_return*100,
                          "% real return, and $",
                          formatC(as.numeric(starting_income), format="f", digits=0, big.mark=","),
                          " in annual income.") 
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  

# ############################  End  ################################## #