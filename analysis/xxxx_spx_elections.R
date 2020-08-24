cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "xxxx_spx_elections"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

election_years <- seq(1952, 2020, 4)

election_years_df <- data.frame(yr = election_years,
                                election_year = rep(1, length(election_years)))

spx <- read.csv(paste0(importdir, "0201_spx_elections/SPX_data.csv"),
                col.names = c("date","index_sp500")) %>%
  mutate(date = as.Date(date),
         yr = year(date),
         mt = month(date),
         dy = day(date)) %>%
  arrange(date) %>%
  mutate(
    ret = index_sp500/lag(index_sp500) - 1
  ) %>%
  filter(!is.na(ret)) %>%
  mutate(
    sd = rollapply(ret, 20, FUN = sd, align = "right", fill = NA)
  )

for(i in 1:nrow(spx)){
  if(i == 1){
    spx[i, "day"] <- 1
    spx[i, "growth_of_dollar"] <- 1
  } else{
    yr <- spx[i, "yr"]
    prior_yr <- spx[(i-1), "yr"]
    
    if(yr == prior_yr){
      spx[i, "day"] <- spx[(i-1), "day"] + 1
      spx[i, "growth_of_dollar"] <- spx[(i-1), "growth_of_dollar"] * (1 + spx[i, "ret"])
    } else{
      spx[i, "day"] <- 1
      spx[i, "growth_of_dollar"] <- 1
    }
  }
}

df <- spx %>%
  left_join(election_years_df) %>%
  mutate(election_year = ifelse(is.na(election_year), "Non-Election Year", "Election Year"))

to_plot <- df %>%
  group_by(election_year, day) %>%
  summarize(mean_sd = mean(sd, na.rm = TRUE),
            mean_dollar_growth = mean(growth_of_dollar, na.rm = TRUE),
            n_years = n()) %>%
  ungroup() %>%
  filter(n_years > 5)

election_day <- spx %>%
  filter(mt == 11, dy == 5) %>%
  summarize(median = ceiling(quantile(day, probs = 0.5))) %>%
  pull(median)

file_path <- paste0(out_path, "/spx_volatility_election_year.jpeg")
source_string <- "Source:  YCharts" 

last_day <- df %>% filter(date == max(df$date)) %>% pull(day)

point <- to_plot %>%
          filter(election_year == "Election Year", day == last_day)

plot <- ggplot(to_plot, aes(x=day, y=mean_sd, col = election_year)) + 
  geom_line() +
  geom_point(data=point, aes(x=day, y=mean_sd), col = "red") +
  geom_vline(xintercept = election_day, linetype = "dashed") +
  geom_text(mapping = aes(x = election_day,
                          y = min(to_plot$mean_sd),
                          label = "Election Day"),
            col = "blue",
            size = 2.5,
            show.legend = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 0.01)) +
  scale_color_manual(values = c("blue", "grey")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 30-Day Average Standard Deviation\nin Election and Non-Election Years")) +
  labs(x = "Day" , y = "Standard Deviation",
       caption = paste0("\n", source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


file_path <- paste0(out_path, "/spx_growth_dollar_election_year.jpeg")
source_string <- "Source:  YCharts" 

plot <- ggplot(to_plot, aes(x=day, y=mean_dollar_growth, col = election_year)) + 
  geom_line() +
  geom_vline(xintercept = election_day, linetype = "dashed") +
  geom_text(mapping = aes(x = election_day,
                          y = max(to_plot$mean_dollar_growth),
                          label = "Election Day"),
            col = "blue",
            size = 2.5,
            show.legend = FALSE) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = c("blue", "grey")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 Growth of $1\nin Election and Non-Election Years")) +
  labs(x = "Day" , y = "Growth of $1",
       caption = paste0("\n", source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #