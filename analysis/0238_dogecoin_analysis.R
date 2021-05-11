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

folder_name <- "0238_dogecoin_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0238_dogecoin/IDOGEUSD_data.csv")) %>%
            mutate(date = as.Date(Period),
                   index = `Dogecoin.Price`) %>%
            select(date, index) %>%
            arrange(date) %>%
            filter(date >= "2020-03-27") %>%
            mutate(index = ifelse(date == as.Date("2021-05-04"), 0.66, index))

df <- raw %>%
        mutate(payment = case_when(date == as.Date("2020-03-27") ~  1200,
                                    date == as.Date("2020-12-29") ~  600,
                                   date == as.Date("2021-03-15") ~ 1400,
               TRUE ~ 0))

for(i in 1:nrow(df)){
  payment <- df[i, "payment"]
  
  if(i == 1){
    df[i, "shares"] <- df[i, "payment"]/df[i, "index"]
  } else{
    df[i, "shares"] <- df[(i-1), "shares"] + (df[i, "payment"]/df[i, "index"])
  }
  df[i, "portfolio"] <- df[i, "shares"] * df[i, "index"]
}

file_path <- paste0(out_path, "/dogecoin_buys.jpeg")
source_string <- "Source:  YCharts (OfDollarsAndData.com)"

purchases <- data.frame(date = c(as.Date("2020-03-27"), 
                                 as.Date("2020-12-29"),
                                 as.Date("2021-03-15")
                                 ))

purchases <- purchases %>%
              inner_join(df) %>%
              select(date, portfolio)
                        
to_plot <- df

plot <- ggplot(to_plot, aes(x=date, y=portfolio)) +
  geom_line() +
  geom_point(data = purchases, col = "green") +
  scale_y_continuous(label = dollar, breaks = seq(0, 600000, 100000)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Putting all 3 U.S. Stimulus Checks into Dogecoin")) +
  labs(x="Date", y="Portfolio Value",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #