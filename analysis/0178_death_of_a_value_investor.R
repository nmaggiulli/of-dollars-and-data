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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0178_death_of_a_value_investor"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

ycharts <- read.csv(paste0(importdir, "/0178_ycharts_value_v_growth/RLGTR_RLVTR_annual_ret.csv"),
                col.names = c("date", "ret_growth", "ret_value")) %>%
          mutate(year = year(as.Date(date)),
                 ret_growth = as.numeric(ret_growth)/100,
                 ret_value = as.numeric(ret_value)/100,
                 hml = ret_value-ret_growth) %>%
          arrange(year) %>%
          select(year, hml)

#Bring in FF
ff <- read.csv(paste0(importdir, "/0178_ycharts_value_v_growth/F-F_Research_Data_Factors.csv"),
               skip =1130, col.names = c("year", "beta", "smb", "hml", "rf")) %>%
  select(year, hml) %>%
  mutate(year = as.numeric(year),
         hml = hml/100) %>%
  filter(!is.na(hml))

plot_hml <- function(df_string, start_year, end_year){
  
  to_plot <- get(df_string) %>%
    filter(year >= start_year, year<= end_year)
  
  t_test <- t.test(to_plot$hml, var.equal = FALSE, mu = 0, alternative = "greater")
  
  if(t_test$p.value <0.05){
    sig_string <- ""
  } else{
    sig_string <- "not"
  }
  
  note_string <-  str_wrap(paste0("Note:  The annual outperformance of value over growth is ", sig_string, " significant over this time period with a p-value of ", round(t_test$p.value,2), ".  ",
                                  "The difference in means (of annual returns) is calculated using a one-sided t-test."),
                           width = 85)
  
  if(df_string == "ff"){
    source_string <- str_wrap(paste0("Source:  Fama-French Data Library, https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html (OfDollarsAndData.com)"),
                              width = 85)
    hml_high <- 0.35
    y_max <- 0.4
  } else if (df_string == "ycharts"){
    source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
    hml_high <- 0.27
    y_max <- 0.3
  }
  
  # Plot all rise dates
  file_path <- paste0(out_path, "/value_v_growth_", df_string, "_", start_year, "_", end_year, ".jpeg")
  
  text_labels <- data.frame()
  
  text_labels[1, "year"] <- 2010
  text_labels[1, "hml"] <- hml_high
  text_labels[1, "label"] <- "Value Outperforms"
  
  text_labels[2, "year"] <- text_labels[1, "year"]
  text_labels[2, "hml"] <- -1*text_labels[1, "hml"]
  text_labels[2, "label"] <- "Growth Outperforms"
  
  plot <- ggplot(to_plot, aes(x=year, y=hml)) + 
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data=text_labels, aes(x=year, y=hml, label=label), 
              size = 3.5, 
              family = "my_font", 
              col = "black") +
    scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-y_max, y_max), breaks = seq(-y_max, y_max, 0.1)) +
    of_dollars_and_data_theme +
    ggtitle(paste0("Value Minus Growth Annual Return\n", start_year, "-", end_year)) +
    labs(x = "Year" , y = "Net Return",
         caption = paste0("\n", source_string, "\n", note_string))  
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_hml("ycharts", 1979, 2019)
plot_hml("ff", 1979, 2019)
plot_hml("ff", 1975, 2019)

# Hypothetical dotcom again
bubble_pop_ff <- ff %>%
  filter(year >= 2000, year<= 2001)

final_hyp_ff <- ff %>% filter(year>= 1979) %>% bind_rows(bubble_pop_ff)

print(t.test(final_hyp_ff$hml, var.equal = FALSE, mu = 0, alternative = "greater"))

# Create 30-year total return diff
calculate_hml_over_time <- function(start_year, end_year){
  n_yr <- end_year - start_year + 1
  
  prod <- ff %>%
            filter(year >= start_year, year <= end_year) %>%
            mutate(hml = hml + 1) %>%
            summarise(prod = prod(hml)^(1/n_yr) - 1) %>%
            pull(prod)
  return(prod)
}

n_years <- 5
yrs <- sort(unique(ff$year))

yrs <- yrs[1:(length(yrs)-n_years)]

full_df <- data.frame()

counter <- 1
for(y in yrs){
  full_df[counter, "hml"] <- calculate_hml_over_time(y, y+n_years)
  full_df[counter, "start_year"] <- y
  full_df[counter, "end_year"] <- y + n_years
  counter <- counter + 1
}

to_plot <- full_df

file_path <- paste0(out_path, "/rolling_hml_", n_years, ".jpeg")
source_string <- str_wrap(paste0("Source:  Fama-French Data Library, https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html (OfDollarsAndData.com)"),
                          width = 85)

plot <- ggplot(to_plot, aes(x=start_year, y=hml)) + 
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Value Minus Growth Rolling Outperformance\nOver ", n_years, " Years")) +
  labs(x = "Starting Year" , y = "Rolling Annualized Outperformance",
       caption = paste0("\n", source_string))  

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #