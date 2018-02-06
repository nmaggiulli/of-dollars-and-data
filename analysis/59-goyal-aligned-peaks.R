cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(dplyr)

########################## Start Program Here ######################### #

df_full <- readRDS(paste0(localdir, "58-goyal-stock-bond-data.Rds"))

plot_aligned_crash <- function(n_years_after_peak){
  
  # Peak date list
  date_list <- c("1929-09-01", "1973-03-01", "1987-08-01", "2007-10-01")
  
  # Create a counter for the subsets 
  counter            <- 1
  
  for (peak_date in date_list){
    df <- filter(df_full, date >= as.Date(peak_date), date < as.Date(peak_date) %m+% months(n_years_after_peak * 12))
    for (i in 1:nrow(df)){
      df[i, "year"] <- i/12
      if (i == 1){
        df[i, "stock_index"] <- 100
        df[i, "bond_index"]  <- 100
      } else {
        df[i, "stock_index"] <- df[(i-1), "stock_index"] * (1 + (df[(i-1), "stock"]))
        df[i, "bond_index"] <- df[(i-1), "bond_index"] * (1 + (df[(i-1), "lt_bond"]))
      }
    }
    
    # Drop unneeded columns
    df <- df %>% 
      mutate(peak = year(peak_date)) %>%
      select(year, peak, stock_index, bond_index) %>%
      gather(key = key, value = value, -year, -peak) %>%
      mutate(key = ifelse(key=="bond_index", "U.S. Bonds", "U.S. Stocks"))
    
    # Append the rows as we loop through each subset
    if (counter == 1){
      to_plot <- df
    } else{
      to_plot <- bind_rows(to_plot, df)
    }
    counter <- counter + 1
  }
  
  # Set the file_path based on the function input 
  file_path <- paste0(exportdir, "59-goyal-aligned-peaks/stock-aligned-peaks-", n_years_after_peak, ".jpeg")
  
  # Set note and source string
  source_string <- str_wrap("Source: Amit Goyal, http://www.hec.unil.ch/agoyal/ (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note:  Shows inflation-adjusted U.S. stock and bond returns from a market peak.",
                                   ""),
                            width = 85)
  
  # Plot the results
  plot <- ggplot(to_plot, aes(x = year, y = value, linetype = key, col = key)) +
            facet_wrap(~peak) +
            geom_line() +
            scale_color_discrete() +
            theme(legend.title=element_blank(),
                  legend.position = "top") +
            scale_linetype() +
            of_dollars_and_data_theme +
            ggtitle("Bonds Usually Provide Stability\nWhen Stocks Tumble") +
            labs(x = "Number of Years Since Peak" , y = "Index (Start = 100)",
              caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

plot_aligned_crash(5)


# ############################  End  ################################## #