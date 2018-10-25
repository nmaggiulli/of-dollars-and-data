cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Orion data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(RGA)
library(scales)
library(RColorBrewer)
library(quantmod)
library(ggrepel)
library(slackr)
library(lubridate)
library(readxl)
library(tidyverse)

folder_name <- "xxxx_spx_mcrib"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

spx <- read_excel(paste0(importdir, "xxxx_spx_daily/spx_daily.xlsx")) %>%
        rename(date = Period,
               index = `S&P 500 Level`) %>%
        filter(date >= "2009-12-31", date < "2018-01-01") %>%
        mutate(year = year(date),
               date = as.Date(date),
               ret = index/lag(index) - 1) %>%
        select(date, index, year, ret) %>%
        filter(!is.na(ret))

mcrib_dates <- data.frame(start = c("2010-11-02", "2011-10-24", "2012-12-17",
                                    "2013-10-15", "2014-11-05", "2015-09-01",
                                    "2016-11-09", "2017-11-09"
                                    ),
                          end = c("2010-12-05", "2011-11-14", "2013-01-15",
                                  "2013-12-15", "2014-12-15", "2015-11-30", 
                                  "2016-12-15", "2017-12-15"
                                  ))

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

mcrib_spx <- spx %>%
              left_join(all_dates) %>%
              mutate(mcrib = ifelse(is.na(mcrib), "Without McRib", "With McRib"))

n_pct_days <- nrow(all_dates)/nrow(spx)

# Set note and source string
source_string <- str_wrap("Source: YCharts.com, http://www.jeffreysward.com/editorials/mcrib.htm (OfDollarsAndData.com)",
                          width = 85)
note_string   <- str_wrap(paste0("Note:  Only includes McRib release data from 2010-2017.  The McRib was available on ", 100*round(n_pct_days, 3), "% of all trading days from 2010-2017."), 
                          width = 85)

# Set the file_path based on the function input 
file_path <- paste0(out_path, "/mcrib_days.jpeg")

to_plot <- mcrib_spx %>%
            group_by(mcrib) %>%
            summarize(ret = mean(ret, na.rm = TRUE)) %>%
            ungroup()

plot <- ggplot(to_plot, aes(x=mcrib, y=ret, fill = mcrib)) +
          geom_bar(stat="identity") +
          scale_fill_manual(values = c("red", "blue"), guide = FALSE) +
          of_dollars_and_data_theme +
          scale_y_continuous(label = percent) +
          ggtitle("The S&P 500 Has a Higher Daily Return\nWhen the McRib is Available") +
          labs(x = "McRib Status", y = "Average Daily Return (%)",
               caption = paste0("\n", source_string, "\n", note_string))

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Save the plot
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #
