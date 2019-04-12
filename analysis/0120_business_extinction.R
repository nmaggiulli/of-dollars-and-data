cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidyverse)

folder_name <- "0120_business_extinction"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read_excel(paste0(importdir, "/0120_bls_business_birth_death/bls_business_births_deaths_qtr.xlsx")) %>%
          mutate(net = Births-Deaths)

to_plot <- raw %>%
            select(-net) %>%
            gather(-Quarter, key=key, value=value) 

file_path <- paste0(out_path, "/bls_business_births_deaths.jpeg")

min_year <- min(year(to_plot$Quarter))
max_year <- max(year(to_plot$Quarter))

# Set source/note
source_string <- paste0("Source:  BLS, ",  min_year, "-", max_year, " (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Shows number of U.S. business establishments created and terminated by quarter."), 
                          width = 80)

# Find nearest 100,000
y_max <- round_to_nearest(max(to_plot$value), "up", 50000)

# Text label df
text_labels <- filter(to_plot, as.Date(Quarter) == as.Date("2005-03-01"))

plot <- ggplot(to_plot, aes(x = Quarter, y = value, col = key)) +
  geom_line() +
  scale_y_continuous(label = comma, limits = c(0, y_max)) +
  scale_color_manual(values = c("green", "red"), guide = FALSE) +
  geom_text_repel(data= text_labels, 
                  aes(x= Quarter, y = value, col = key),
                  label = paste0(text_labels$key),
                  max.iter = 2000, 
                  nudge_y = ifelse(text_labels$key %in% c("Deaths"), -25000, 25000),
                  segment.color = "transparent") +
  of_dollars_and_data_theme +
  ggtitle("U.S. Business Births and Deaths by Quarter") +
  labs(x = "Quarter" , y = "Total U.S. Business Births/Deaths",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in S&P 500 data
sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
  rename(index = price_plus_div) %>%
  select(date, index)

# Calculate drawdown
dd <- drawdown_path(sp500)

# Reset file path
file_path <- paste0(out_path, "/sp500_all_drawdowns.jpeg")

# Set source/note
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Real return includes reinvested dividends."), 
                          width = 80)

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(position = "identity", fill = "red") +
  scale_y_continuous(label = percent, limits = c(-1, 0)) +
  of_dollars_and_data_theme +
  of_dollars_and_data_theme +
  ggtitle("All U.S. Stock Drawdowns") +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in extinction data
ext <- read.csv(paste0(importdir, "/0120_bls_business_birth_death/extinction_data.csv")) %>%
          mutate(ext_rate = ext_rate/-100)

# Create a species index based on the extinction rate
for (i in 1:nrow(ext)){
  yago <- ext[i, "years_ago_millions"]
  pct <- ext[i, "ext_rate"]
  
  if(i == 1){
    ext[i, "life_index"] <- 1 * (1+pct)
  } else if(yago > -450 & yago < -443 |
    yago > -385 & yago < -374 |
    yago > -265 & yago < -250 |
    yago > -208 & yago < -198 |
    yago > -67 & yago < -64){
    ext[i, "life_index"] <- ext[(i-1), "life_index"] * (1+pct)
    print("mass")
  } else{
    ext[i, "life_index"] <- 1 + pct
  }
}

to_plot <- ext %>%
            mutate(pct_extinction = life_index-1)

# Reset file path
file_path <- paste0(out_path, "/genetic_diversity_drawdowns.jpeg")

# Set source/note
source_string <- paste0("Source:  https://www.e-education.psu.edu/earth103/node/713 (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Shows the percentage loss in genetic diversity from millions of years ago until today.  ",
                                 "The extinction rate is aggregated only during known mass extinction events."), 
                          width = 80)

plot <- ggplot(to_plot, aes(x = years_ago_millions, y = pct_extinction)) +
  geom_area(position = "identity", fill = "red") +
  scale_y_continuous(label = percent, limits = c(-1, 0)) +
  of_dollars_and_data_theme +
  of_dollars_and_data_theme +
  ggtitle("Mass Extinctions on Earth") +
  labs(x = "Millions of Years Ago" , y = "Percentage of Genetic Diversity Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #