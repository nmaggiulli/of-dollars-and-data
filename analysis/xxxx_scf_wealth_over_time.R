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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "xxxx_scf_wealth_over_time"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

age_year_groups <- data.frame(year = c(seq(1992, 2019, 3),
                                       seq(1995, 2019, 3),
                                       seq(1998, 2019, 3),
                                       seq(2001, 2019, 3),
                                       seq(2004, 2019, 3),
                                       seq(2007, 2019, 3)),
                              age_group = c(rep(1, 10),
                                            rep(2, 9),
                                            rep(3, 8),
                                            rep(4, 7),
                                            rep(5, 6),
                                            rep(6, 5)),
                              min_age = c(seq(64, 91, 3),
                                          seq(64, 88, 3),
                                          seq(64, 85, 3),
                                          seq(64, 82, 3),
                                          seq(64, 79, 3),
                                          seq(64, 76, 3)),
                              max_age = c(seq(66, 93, 3),
                                          seq(66, 90, 3),
                                          seq(66, 87, 3),
                                          seq(66, 84, 3),
                                          seq(66, 81, 3),
                                          seq(66, 78, 3)))
                                          
for(i in 1:nrow(age_year_groups)){
  print(i)
  min_age <- age_year_groups[i, "min_age"]
  max_age <- age_year_groups[i, "max_age"]
  yr <- age_year_groups[i, "year"]
  ag_group <- age_year_groups[i, "age_group"]
  
  tmp <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
            filter(year == yr, age >= min_age, age <= max_age) %>%
            mutate(age_group = ag_group,
                   min_max_age = paste0(min_age, "-", max_age)) %>%
        select(hh_id, imp_id, year, age_group, min_max_age, networth, reteq, wgt)
  
  if(i == 1){
    df <- tmp
  } else{
    df <- df %>% bind_rows(tmp)
  }
}


summary <- df %>%
  group_by(year, age_group, min_max_age) %>%
  summarise(
    median_nw = wtd.quantile(networth, weights = wgt, probs=0.5),
    median_reteq = wtd.quantile(reteq, weights = wgt, probs=0.5)
  ) %>%
  ungroup() %>%
  gather(-year, -age_group, -min_max_age, key=key, value=value) %>%
  mutate()

to_plot <- summary %>%
              filter(key == "median_reteq")

# Save plot
file_path <- paste0(out_path, "/median_nw_over_time.jpeg")

# Create plot
plot <- ggplot(data = to_plot, aes(x = year, y=value, col = as.factor(age_group))) +
  geom_line() +
  scale_color_discrete(guide = FALSE) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Median Net Worth") +
  ggtitle(paste0("Median Liquid Net Worth"))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #