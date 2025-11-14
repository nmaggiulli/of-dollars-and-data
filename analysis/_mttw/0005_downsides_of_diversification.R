cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "_mttw/0005_downsides_of_diversification"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/", folder_name, "/ycharts_port_data.csv"),
                col.names = c("date", "NM 投資組合", "美國股票", "美國房地產投資信託基金", "國際已開發市場股票", "國際新興市場股票", "小型價值股", "美國中期債券")) %>%
        drop_na() %>%
        mutate(date = as.Date(date)) %>%
        arrange(date)

to_plot <- raw %>%
          gather(-date, key=key, value=value) %>%
          mutate(value = value/100,
                 key = str_replace_all(key, "\\.", " ")) 

file_path <- paste0(out_path, "/nm_port_since_aug_2012.jpeg")

last_date <- filter(to_plot, date == max(to_plot$date))
sorted_keys <- last_date %>% arrange(desc(value)) %>% pull(key)

standard_size <- 0.4

end_month <- format.Date(max(to_plot$date), format = "%b %Y")

plot <- ggplot(data = to_plot, aes(x = date, y = value, col = key, size = key)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_x_date(date_labels = "%Y") +
  scale_colour_manual(values = c("#e41a1c", "#377eb8", "black", "#4daf4a", "#984ea3", "#ff7f00", "brown"), 
                      limits = sorted_keys) +
  scale_size_manual(values = c(rep(standard_size, 3), standard_size*2, rep(standard_size, 3)),
                    guide = "none") +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right") +
  ggtitle(paste0("Nick Maggiulli 投資組合對比\n基礎持股\n2012年8月 - 2025年10月")) +
  labs(x = "日期" , y = "總回報率 (%)")

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #