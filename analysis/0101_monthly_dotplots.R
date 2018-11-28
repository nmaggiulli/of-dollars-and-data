cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(tidyverse)

folder_name <- "0101_monthly_dotplots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    mutate(decade = as.factor(year(date) - year(date) %% 10),
                           month = as.factor(month(date)),
                           ret = price_plus_div/lag(price_plus_div) - 1)

compare_months <- function(start_date, end_date){
                      
  to_plot <- sp500_ret_pe %>%
              filter(date >= start_date, date <= end_date)
  
  first_year <- min(year(to_plot$date))
  last_year <- max(year(to_plot$date))
  
  # Compare the monthly returns by using a KS test across each monthly pairing
  results_df <- data.frame(month1 = c(), month2 = c(), p_value_ks = c())
  
  counter <- 0
  for (i in 1:11){
    dist1 <- filter(to_plot, month == i) %>% pull(ret)
    
    for(j in (i+1):12){
      dist2 <- filter(to_plot, month == j) %>% pull(ret)
      counter <- counter + 1
      
      results_df[counter, "month1"] <- i
      results_df[counter, "month2"] <- j
      results_df[counter, "p_value_ks"] <- ks.test(dist1, dist2)$p.value
    }
    if (i == 1){
      dist2 <- filter(to_plot, month != 1) %>% pull(ret)
      
      results_df[counter, "month1"] <- i
      results_df[counter, "month2"] <- "Not Jan"
      results_df[counter, "p_value_ks"] <- ks.test(dist1, dist2)$p.value
    }
  }
  
  assign("results_df", results_df, envir = .GlobalEnv)
  
  # Calculate the number of significant months
  sig_months <- nrow(filter(results_df, p_value_ks < 0.05))
  
  # Find the monthly average returns
  monthly_avg <- to_plot %>%
                  group_by(month) %>%
                  summarize(ret = mean(ret)) %>%
                  ungroup()
  
  assign("monthly_avg", monthly_avg, envir = .GlobalEnv)
  
  total_years <- nrow(filter(to_plot, month == 1))
  
  if(sig_months == 1){
    sig_line <- "There is 1 month comparison"
  } else{
    sig_line <- paste0("There are ", sig_months, " month comparisons")
  }
  
  # Set note and source string
  source_string <- str_wrap("Source: http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)",
                            width = 85)
  note_string   <- str_wrap(paste0("Note:  Monthly returns include dividends and are adjusted for inflation.  Each month has ", total_years, " years of data.  ",
                                   sig_line, " with a statistically significant difference in returns."), 
                            width = 85)
  
  # Set the file_path
  file_path <- paste0(out_path, "/monthly_dotplot_", first_year, "_", last_year, ".jpeg")
  
  plot <- ggplot(to_plot, aes(x=month, y=ret, fill=month)) +
    geom_dotplot(binaxis='y', dotsize = 1, binwidth = 0.007) +
    scale_fill_discrete(guide = FALSE) +
    scale_y_continuous(label = percent) +
    of_dollars_and_data_theme +
    ggtitle(paste0("S&P 500 Monthly Real Return by Month\n", first_year, "-", last_year)) +
    labs(x = "Month", y="One-Month Return (%)",
         caption = paste0(source_string, "\n", note_string))
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Save the plot
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}

compare_months("1920-01-01", "1941-12-31")
compare_months("1920-01-01", "2017-12-31")
compare_months("1880-01-01", "1941-12-31")

# ############################  End  ################################## #