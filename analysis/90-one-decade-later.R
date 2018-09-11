cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(ggrepel)
library(tidyverse)

folder_name <- "90-one-decade-later"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "09-sp500-ret-pe.Rds"))

# Calculate returns for the S&P data
for (i in 1:nrow(sp500_ret_pe)){
  if (i == 1){
    sp500_ret_pe[i, "n_shares"]       <- 1
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  } else{
    sp500_ret_pe[i, "n_shares"]       <- sp500_ret_pe[(i - 1), "n_shares"] + sp500_ret_pe[(i-1), "new_div"]/ 12 / sp500_ret_pe[i, "real_price"]
    sp500_ret_pe[i, "new_div"]        <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_div"]
    sp500_ret_pe[i, "price_plus_div"] <- sp500_ret_pe[i, "n_shares"] * sp500_ret_pe[i, "real_price"]
  }
}


# Change the Date to a Date type for plotting the S&P data
sp500_ret_pe <- select(sp500_ret_pe, date, price_plus_div) %>%
  mutate(date = as.Date(paste0(
    substring(as.character(date), 1, 4),
    "-", 
    ifelse(substring(as.character(date), 6, 7) == "1", "10", substring(as.character(date), 6, 7)),
    "-01", 
    "%Y-%m-%d")))

to_plot <- sp500_ret_pe %>%
            filter(date >= "2008-08-01")

# Find the first value for the index and the 1yr, 3yr, 5yr, and 10yr anniversaries
first_val <- pull(to_plot[1, "price_plus_div"])

flag_dates <- data.frame(date = c(as.Date("2009-08-01"), 
                                  as.Date("2010-08-01"), 
                                  as.Date("2013-08-01"), 
                                  as.Date("2018-08-01")
                                  ),
                         flag = rep(1, 4)
)

to_plot <- to_plot %>%
            mutate(index = price_plus_div/first_val * 100) %>%
            select(-price_plus_div) %>%
            left_join(flag_dates)

flagged_dates <- filter(to_plot, flag == 1)

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

dd <- drawdown_path(to_plot)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/price_10years.jpeg")

source_string <- "Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)"

note_string <- str_wrap(paste0("Note:  Adjusted for dividends and inflation."),
                        width = 85)

plot <- ggplot(to_plot, aes(x=date, y = index)) +
  geom_line() +
  geom_point(data=flagged_dates, aes(x=date, y=index), col = "red") +
  geom_text_repel(data=flagged_dates, aes(x=date, y=index), col = "red",
            label = paste0(round(-(100-flagged_dates$index), 0), "%"),
            nudge_y = -12,
            nudge_x = 15,
            max.iter = 2000,
            segment.colour = "transparent") +
  of_dollars_and_data_theme +
  ggtitle("10 Years of the S&P 500 Since Lehman") +
  labs(x = "Date" , y = "Index\n(August 2008 = 100)",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
## Drawdown plot
file_path <- paste0(out_path, "/drawdown_10years.jpeg")

plot <- ggplot(dd, aes(x = date, y = pct)) +
  geom_area(fill = "red") +
  scale_y_continuous(label = percent, limits = c(-0.5, 0)) +
  of_dollars_and_data_theme +
  ggtitle("S&P 500 Drawdowns Since Lehman") +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
  
# ############################  End  ################################## #