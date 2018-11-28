cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(lubridate)
library(stringr)
library(ggrepel)
library(dplyr)
library(ggplot2)

out_path <- paste0(exportdir, "0080_drawdown_recency_sp500")

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds"))

# Calculate returns for the S&P data


# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, price_plus_div) %>%
                  mutate(date = as.Date(paste0(
                    substring(as.character(date), 1, 4),
                    "-", 
                    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
                    "-01", 
                    "%Y-%m-%d")))
  
# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
  loc_max <- 0
  for (i in 1:(nrow(vp))){
    if (vp[i, 2] < loc_max & i != 1){
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- vp[i, 2]/loc_max - 1
    } else{
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- 0
      loc_max  <- vp[i, 2]
    }
  }
  return(dd)
}
  

plot_dd <- function(date_start, date_end, num){
  
if (num < 10){
  num_string <- paste0("0", num)
} else {
  num_string <- paste0(num)
}
  
  source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"
  
  note_string <- str_wrap(paste0("Note:  Adjusted for dividends and inflation.  
                                 Shading is based on recency and the peak drawdown over the time period listed."),
                          width = 85)
  
  temp <- filter(sp500_ret_pe, date >= date_start, date <= date_end)
  # Run function on specific data for drawdowns
  to_plot        <- drawdown_path(temp)
    
  to_plot$shade <- head(seq(0, 1, 1/nrow(to_plot)), nrow(to_plot))
  
  to_plot <- to_plot %>%
              mutate(shade = ifelse(pct == min(to_plot$pct), 1, shade))
  
  assign("tp", to_plot, envir = .GlobalEnv)
  
  # Set the file_path based on the function input 
  file_path = paste0(out_path, "/drawdowns_", num_string, ".jpeg")
  
  # Create title with ticker in subtitle
  top_title <- paste0("S&P 500 Drawdowns\n", year(date_start), " - ", year(date_end))
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = date, y = pct, fill = shade)) +
    geom_bar(stat = "identity") +
    scale_fill_gradient(low = "white", high = "red") +
    scale_alpha(guide = FALSE) + 
    ggtitle(top_title) +
    guides(fill = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(label = percent, limits = c(-1, 0)) +
    scale_x_date(date_labels = "%Y") +
    labs(x = "Year", y = "Percentage of Value Lost",
         caption = paste0("\n", source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}

all_dates <- seq.Date(as.Date("1990-01-01"), as.Date("2008-01-01"), "6 months")

for (d in 1:length(all_dates)){
  start_date <- all_dates[d]
  end_date <- start_date + years(10)
  plot_dd(start_date, end_date, d)
}

create_gif(out_path, "drawdowns_*.jpeg", 30, out_name = "dd_time.gif")


# ############################  End  ################################## #

  
