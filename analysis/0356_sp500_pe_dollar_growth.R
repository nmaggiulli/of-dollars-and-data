cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(dplyr)

folder_name <- "0356_sp500_pe_dollar_growth"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Create a custom palette with black using ColorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C")

# Load in S&P data from Shiller
sp500_ret_pe   <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

yr_10 <- sp500_ret_pe %>%
  filter(!is.na(cape)) %>%
  mutate(value = lead(price_plus_div, 10*12)/price_plus_div)

plot_pe_yrs <- function(n_years){
  to_plot <- sp500_ret_pe %>%
                  filter(!is.na(cape)) %>%
                  mutate(value = lead(price_plus_div, n_years*12)/price_plus_div,
                         below_zero = ifelse(value < 1, 1, 0))
  
  title <- paste0("Over ", n_years," Years, Lower Starting Valuations\nImply Higher Future Growth")
  
  # Set the file_path for the next output
  file_path <- paste0(out_path, "/sp500_pe_dollar_growth_", n_years, "yr.jpeg")
  
  source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Performance includes dividends and is adjusted for inflation."), width = 85)
  
  plot <- ggplot(data = to_plot, aes(x = cape, y = value, col = as.factor(below_zero))) +
    geom_hline(yintercept = 1, col = "black") +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = my_palette, guide = FALSE) +
    scale_y_continuous(label = dollar) +
    scale_x_continuous(limits = c(0, 45)) +
    ggtitle(title) +
    of_dollars_and_data_theme +
    labs(x = "U.S. Stocks P/E Ratio" , y = "Growth of $1",
         caption = paste0(source_string, "\n", note_string))
  
  # Save the gtable
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
  if(n_years == 10){
    title2 <- paste0("Over ", n_years," Years, There is a Negative Relationship\nBetween Valuation and Future Growth")
    
    # Set the file_path for the next output
    file_path <- paste0(out_path, "/sp500_lm2_pe_dollar_growth_", n_years, "yr.jpeg")
    
    plot <- ggplot(data = to_plot, aes(x = cape, y = value)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_hline(yintercept = 1, col = "black") +
      scale_color_manual(guide = FALSE) +
      scale_y_continuous(label = dollar) +
      scale_x_continuous(limits = c(0, 45)) +
      ggtitle(title2) +
      of_dollars_and_data_theme +
      labs(x = "U.S. Stocks P/E Ratio" , y = "Growth of $1",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  
    title3 <- paste0("With a P/E Ratio of 31, Real Returns Are\nProjected To Be 0% For The Next Decade")
    
    # Set the file_path for the next output
    file_path <- paste0(out_path, "/anno_sp500_lm2_pe_dollar_growth_", n_years, "yr.jpeg")
    
    plot <- ggplot(data = to_plot, aes(x = cape, y = value)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_hline(yintercept = 1, col = "black") +
      scale_color_manual(guide = FALSE) +
      scale_y_continuous(label = dollar) +
      scale_x_continuous(limits = c(0, 45)) +
      ggtitle(title3) +
      of_dollars_and_data_theme +
      labs(x = "U.S. Stocks P/E Ratio" , y = "Growth of $1",
           caption = paste0(source_string, "\n", note_string))
    
    # Save the gtable
    ggsave(file_path, plot, width = 15, height = 12, units = "cm")
  }
}

plot_pe_yrs(10)

#Plot PE ratio over time
to_plot <- sp500_ret_pe %>%
              select(date, cape) %>%
              filter(date >= "1920-01-01")

file_path <- paste0(out_path, "/sp500_shiller_pe_since_1920.jpeg")
source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")

plot <- ggplot(data = to_plot, aes(x = date, y = cape)) +
  geom_line() +
  scale_y_continuous(label = comma) +
  ggtitle("The Shiller P/E Ratio Over Time") +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "U.S. Stocks P/E Ratio",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #