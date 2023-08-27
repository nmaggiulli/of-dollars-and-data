cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "0097_spx_mcrib"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

end_analysis_date <- "2022-12-31"
run_sim <- 0

spx <- read.csv(paste0(importdir, "0097_spx_daily/spx_daily.csv")) %>%
        rename(date = Period,
               index = `S.P.500...SPX..Level`) %>%
        mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
        filter(date >= "2009-12-31", date < end_analysis_date) %>%
        arrange(date) %>%
        mutate(year = year(date),
               date = as.Date(date),
               ret = index/lag(index) - 1) %>%
        select(date, index, year, ret) %>%
        filter(!is.na(ret))

max_year <- max(spx$year)

mcrib_dates <- data.frame(start = c("2010-11-02", "2011-10-24", "2012-12-17",
                                    "2013-10-15", "2014-11-05", "2015-09-15",
                                    "2016-11-09", "2017-11-09", "2018-10-29",
                                    "2019-10-07", "2020-12-02", "2021-10-08",
                                    "2022-10-17"
                                    ),
                          end = c("2010-12-05", "2011-11-14", "2013-01-15",
                                  "2013-12-15", "2014-12-31", "2015-11-30", 
                                  "2016-12-31", "2017-12-31", "2018-12-31",
                                  "2019-12-31", "2020-12-31", "2022-01-23",
                                  "2022-12-31"
                                  )) %>%
                          mutate(n_days = as.Date(end)-as.Date(start),
                                 sim_start = as.Date(paste0(year(start), "-01-01")),
                                 sim_end = as.Date(paste0(year(start), "-12-31")) - n_days) %>%
                          filter(sim_end < end_analysis_date)

for(i in 1:nrow(mcrib_dates)){
  s <- mcrib_dates[i, "start"]
  e <- mcrib_dates[i, "end"]
  date_seq <- data.frame(date=seq.Date(as.Date(s), as.Date(e), by="day"))
  
  if(i == 1){
    all_dates <- date_seq
  } else{
    all_dates <- bind_rows(all_dates, date_seq)
  }
  
  if (i == nrow(mcrib_dates)){
    all_dates <- all_dates %>%
                    mutate(mcrib = 1)
  }
}

spx_mcrib <- spx %>%
              left_join(all_dates) %>%
              mutate(mcrib = ifelse(is.na(mcrib), "Without McRib", "With McRib"),
                     mcrib_dummy = ifelse(mcrib == "With McRib", 1, 0),
                     pos = ifelse(ret > 0, 1, 0))

n_pct_days <- nrow(all_dates)/nrow(spx)

t.test(spx_mcrib$ret~spx_mcrib$mcrib_dummy)

# Set note and source string
source_string <- str_wrap("Source: YCharts.com, http://www.jeffreysward.com/editorials/mcrib.htm (OfDollarsAndData.com)",
                          width = 80)
note_string   <- str_wrap(paste0("Note:  Only includes McRib release data from 2010-", max_year, ".  The McRib was available on ", 100*round(n_pct_days, 3), "% of all trading days from 2010-", max_year, "."), 
                          width = 80)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/mcrib_days.jpeg")

to_plot <- spx_mcrib %>%
            group_by(mcrib) %>%
            summarise(ret = mean(ret, na.rm = TRUE)) %>%
            ungroup() %>%
            arrange(mcrib) %>%
            mutate(label = paste0(round(100*ret,2), "%"))

plot <- ggplot(to_plot, aes(x=mcrib, y=ret, fill = mcrib)) +
          geom_bar(stat="identity") +
          geom_text(data=to_plot, aes(x=mcrib, y=ret+0.00004, label=label),
                    col = "black") +
          scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
          of_dollars_and_data_theme +
          scale_y_continuous(label = percent_format(accuracy = 0.01)) +
          ggtitle("The S&P 500 Has a Higher Daily Return\nWhen the McRib is Available") +
          labs(x = "McRib Status", y = "Average Daily Return (%)",
               caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# Find maximum difference for comparisons
ret_diff <- max(to_plot$ret) - min(to_plot$ret)

if(run_sim == 1){
  # This seed allows us to have reproducible random sampling
  set.seed(12345)     
  
  n_simulations <- 10000
  
  final_results <- matrix(NA, nrow = n_simulations, ncol = 1)
  
  for (i in 1:n_simulations){
    for (j in 1:nrow(mcrib_dates)){
      n_days       <- mcrib_dates[j, "n_days"]
      sim_start    <- mcrib_dates[j, "sim_start"]
      sim_year_end <- as.Date(paste0(year(sim_start), "-12-31"))
      sim_end      <-mcrib_dates[j, "sim_end"]
      
      
      start_date_num <- sample(1:yday(sim_end), 1)
      all_possible_dates <- data.frame(date=seq.Date(sim_start, sim_year_end, "day"))
      
      sim_dates <- data.frame(date=all_possible_dates[start_date_num:(start_date_num+n_days), "date"])
      
      if(j == 1){
        all_sim_dates <- sim_dates
      } else{
        all_sim_dates <- bind_rows(all_sim_dates, sim_dates)
      }
      
      if (j == nrow(mcrib_dates)){
        all_sim_dates <- all_sim_dates %>%
          mutate(mcrib = 1)
      }
    }
    sim_mcrib <- spx %>%
      left_join(all_sim_dates) %>%
      mutate(mcrib = ifelse(is.na(mcrib), "Without McRib", "With McRib")) %>%
      group_by(mcrib) %>%
        summarise(ret = mean(ret, na.rm = TRUE)) %>%
        ungroup()
    
    final_results[i, 1] <- pull(sim_mcrib[1, 2]) - pull(sim_mcrib[2, 2])
    
    if (i == n_simulations){
      final_results <- data.frame(diff=final_results)
    }
  }
  
  final_results <- final_results %>%
          mutate(mcrib_bigger_diff = ifelse(diff > ret_diff, 1, 0),
                 no_mcrib_bigger_diff = ifelse(diff < -ret_diff, 1, 0))
  
  print(mean(final_results$mcrib_bigger_diff))      
  print(mean(final_results$no_mcrib_bigger_diff))
}

# ############################  End  ################################## #
