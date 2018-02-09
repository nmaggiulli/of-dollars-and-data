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

df_full <- readRDS(paste0(localdir, "59-goyal-stock-bond-data.Rds"))

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
      mutate(key = ifelse(key=="bond_index", "Bonds", "Stocks"))
    
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
  note_string   <- str_wrap(paste0("Note:  Shows inflation-adjusted U.S. stock and bond returns from market peak. 
                                   Stock returns include continuously compounded dividends.",
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
            ggtitle("Each Market Crisis Provides\nA Different Experience") +
            labs(x = "Number of Years Since Peak" , y = "Index (Start = 100)",
              caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))

  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
  for (d in 1:length(date_list)){
    date <- date_list[d]
    peak_year <- year(date)
    
    color_vector <- c()
    size_vector <- c()
    
    for (n in 1:length(date_list)){
      if(d == n){
        color_vector[n] <- "red"
        size_vector[n] <- 1
      } else {
        color_vector[n] <- "gray"
        size_vector[n] <- 0.5
      }
    }
  
    # Set the file_path based on the function input 
    file_path <- paste0(exportdir, "59-goyal-aligned-peaks/all-", peak_year ,"-", n_years_after_peak, ".jpeg")
    
    # Set note and source string
    source_string <- str_wrap("Source: Amit Goyal, http://www.hec.unil.ch/agoyal/ (OfDollarsAndData.com)",
                              width = 85)
    note_string   <- str_wrap(paste0("Note:  Shows inflation-adjusted U.S. stock and bond returns from market peak.  
                                     Stock returns include continuously compounded dividends.",
                                     ""),
                              width = 85)
    
    # Plot the results
    plot <- ggplot(to_plot, aes(x = year, y = value, col = as.factor(peak), linetype = key, size = as.factor(peak))) +
      geom_line() +
      scale_color_manual(values = color_vector, guide = FALSE) +
      scale_size_manual(values = size_vector, guide = FALSE) +
      geom_text_repel(data = filter(to_plot, peak == peak_year, year == n_years_after_peak), 
                      aes(x = year, 
                          y = value, 
                          col = as.factor(peak), 
                          label = as.character(key),
                          family = "my_font"
                      ), size = 4,
                      max.iter = 3000) +
      scale_linetype(guide = FALSE) +
      of_dollars_and_data_theme +
      ggtitle(paste0("U.S. Bond vs. U.S. Stock Performance From Peak\n", peak_year)) +
      labs(x = "Number of Years Since Peak" , y = "Index (Start = 100)",
           caption = paste0("\n", source_string, "\n", note_string))
    
    # Turn plot into a gtable for adding text grobs
    my_gtable   <- ggplot_gtable(ggplot_build(plot))
    
    # Save the plot
    ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  }
}

plot_aligned_crash(5)

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# convert -delay 110 loop -0 all-*.jpeg all_plots.gif

# Create plot of stocks by decade
decade <- df_full %>%
            filter(date > "1929-12-31") %>%
            mutate(decade = paste0(as.character(year(floor_date(date, years(10))))),
                   stock = 1 + stock) %>%
            group_by(decade) %>%
            summarize(stock = (prod(stock)^(1/10) - 1)) %>%
            select(decade, stock)

# Set the file_path based on the function input 
file_path <- paste0(exportdir, "59-goyal-aligned-peaks/stock-returns-by-decade.jpeg")

# Set note and source string
source_string <- str_wrap("Source: Amit Goyal, http://www.hec.unil.ch/agoyal/ (OfDollarsAndData.com)",
                          width = 85)
note_string   <- str_wrap(paste0("Note:  Returns include dividends and are adjusted for inflation using FRED CPI.",
                                 ""),
                          width = 85)

# Plot the results
plot <- ggplot(decade, aes(x = decade, y = stock)) +
  geom_bar(stat="identity", fill = "blue") +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Stock Returns Vary Widely by Decade")) +
  labs(x = "Decade" , y = "Annualized Real Return",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #