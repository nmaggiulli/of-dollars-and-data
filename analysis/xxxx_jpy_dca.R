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
library(gganimate)
library(tidylog)
library(zoo)
library(tidyverse)
library(tidylog)

folder_name <- "xxxx_jpy_dca"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#969696", "#000000")

jpy <- read.csv(paste0(importdir,"/_jkb/0010_buying_during_a_crisis/NIKKEI225_fred.csv")) %>%
  mutate(date = as.Date(DATE),
         index_jpy = as.numeric(NIKKEI225),
         ret_jpy = index_jpy/lag(index_jpy, 1) - 1) %>%
  select(date, index_jpy, ret_jpy) %>%
  drop_na %>%
  filter(date >= "1980-01-01")

final_jpy <- jpy %>% tail(1) %>% pull(index_jpy)

for(i in 1:nrow(jpy)){
  if(i == 1){
    jpy[i, "market_value"] <- 1
    jpy[i, "basis"] <- 1
  } else{
    jpy[i, "market_value"] <- jpy[(i-1), "market_value"] * (1 + jpy[i, "ret_jpy"]) + 1
    jpy[i, "basis"] <- jpy[(i-1), "basis"] + 1
  }
  jpy[i, "total_ret"] <- final_jpy/jpy[i, "index_jpy"] - 1
}

summary <- jpy %>%
            mutate(dca_contribution = total_ret/nrow(jpy)) %>%
            summarise(total_ret = sum(dca_contribution)^(1/44) - 1) 

to_plot <- jpy %>%
  select(date, market_value, basis) %>%
  gather(-date, key=key, value=value) %>%
  mutate(key = case_when(
    key == "market_value" ~ "Market Value",
    TRUE ~ "Cost Basis"
  )
  )

# DCA into Japan
file_path <- paste0(out_path, "/jpy_1980_onward_dca.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) + 
  geom_line() +
  scale_color_manual(values = bw_colors) +
  scale_y_continuous(label = dollar) + 
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("$1 Per Day into Japanese Stocks\nPortfolio Value vs. Cost Basis")) +
  labs(x = "Date" , y = "Value")  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #