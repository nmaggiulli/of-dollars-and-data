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
library(xtable)
library(tidyverse)

folder_name <- "0344_global_stock_performance"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

pct_for_html <- function(x){
  t <- paste0(100*round(x, 3), "%")
  return(t)
}

countries <- c("US", "Canada", "UK", 
              "India", "Singapore", "Germany", 
              "Australia", "Italy", "Poland", "CPI", "rfr")

exclude <- c("CPI", "rfr")

my_colors <- c("black",
                "#1f78b4",
                "#b2df8a",
                "#33a02c",
                "#fb9a99",
                "#e31a1c",
                "#fdbf6f",
                "#ff7f00",
                "#cab2d6")

raw <- read.csv(paste0(importdir, "/0344_global_stock_performance/GrowthOfWealth_20230305183049.csv"),
                skip = 7,
                col.names = c("date", countries)) %>%
        filter(!is.na(US)) %>%
        mutate(date = as.Date(date, "%m/%d/%Y"),
               yr = year(date),
               mt = month(date)) %>%
        filter(date <= "2022-12-31") %>%
        arrange(date)

long <- raw %>%
  gather(-date, -yr, -mt, key=key, value=value) 

last_cpi_value <- long %>%
                    filter(date == max(long$date),
                           key == "CPI") %>%
                    pull(value)

annual_rets <- long %>%
                  filter(key != "CPI") %>%
                  mutate(ret_1yr = 
                           ifelse(yr == 1993 & mt == 12, 
                                  value - 1,
                                  value/lag(value, 12) - 1)
                  ) %>%
                filter(mt == 12)

rfr <- annual_rets %>%
          filter(key == "rfr") %>%
          summarise(mean_ret = mean(ret_1yr)) %>%
          pull(mean_ret)

summary_by_country <- annual_rets %>%
                        filter(!(key %in% exclude)) %>%
                        group_by(key) %>%
                        summarise(`Average Return` = mean(ret_1yr),
                               `Standard Deviation` = sd(ret_1yr),
                               `Sharpe Ratio` = (`Average Return` - rfr)/`Standard Deviation`) %>%
                        ungroup() %>%
                        rename(Country = key) %>%
                        arrange(desc(`Sharpe Ratio`)) %>%
                        mutate(`Average Return` = pct_for_html(`Average Return`),
                               `Standard Deviation` = pct_for_html(`Standard Deviation`)
                               )

print(xtable(summary_by_country), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/sharpe_by_country.html"))

last_value <- long %>%
              filter(date == max(long$date)) %>%
              arrange(desc(value)) %>%
              mutate(annualized = value^(1/30) - 1)

cpi_ann <- last_value %>%
              filter(key == "CPI") %>%
              pull(annualized)

last_value <- last_value %>%
                filter(!(key %in% exclude)) %>%
                mutate(real_annualized = (1+annualized)/(1+cpi_ann) - 1)

summary_annualized <- last_value %>%
  select(key, annualized, real_annualized) %>%
  mutate(annualized = pct_for_html(annualized),
         real_annualized = pct_for_html(real_annualized)) %>%
  rename(Country = key,
         `Annualized Return` = annualized,
         `Real Annualized Return` = real_annualized)

print(xtable(summary_annualized), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/annualized_by_country.html"))

to_plot <- long %>%
              filter(!(key %in% exclude))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

file_path <- paste0(out_path, "/global_stock_growth_of_dollar_1993_2022.jpeg")
source_string <- str_wrap(paste0("Source: Returns 2.0 (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: The U.S. is represented by the S&P 500. ",
                                "All other countries are represented by their respective MSCI Country Index (gross dividends)."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("Growth of $1\n1993-2022")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Growth of dollar since march 2009
march_2009 <- long %>%
              filter(!(key %in% exclude)) %>%
                filter(yr == 2009, mt == 3) %>%
                rename(first = value) %>%
                select(key, first)

to_plot <- long %>%
  filter(!(key %in% exclude),
         date >= "2009-03-01") %>%
  left_join(march_2009) %>%
  mutate(value = value/first) %>%
  select(-first)

last_value <- to_plot %>%
              filter(date == max(to_plot$date)) %>%
              arrange(desc(value))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

file_path <- paste0(out_path, "/global_stock_growth_of_dollar_2009_2022.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("Growth of $1\n2009-2022")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do Growth of dollar from 2000-2009
jan_2000 <- long %>%
  filter(!(key %in% exclude)) %>%
  filter(yr == 2000, mt == 1) %>%
  rename(first = value) %>%
  select(key, first)

to_plot <- long %>%
  filter(!(key %in% exclude),
         date >= "2000-01-01",
         date <= "2009-03-01") %>%
  left_join(jan_2000) %>%
  mutate(value = value/first) %>%
  select(-first)

last_value <- to_plot %>%
  filter(date == max(to_plot$date)) %>%
  arrange(desc(value))

to_plot$key <- factor(to_plot$key, levels = last_value$key)

file_path <- paste0(out_path, "/global_stock_growth_of_dollar_2000_2009.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle(paste0("Growth of $1\n2000-2009")) +
  labs(x = "Date" , y = "Growth of $1",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do drawdowns
for(c in countries){
  tmp <- long %>%
          filter(key == c) %>%
          select(date, value)
  
  dd <- drawdown_path(tmp) %>%
          mutate(key = c)
  
  if(c == countries[1]){
    dd_stack <- dd
  } else{
    dd_stack <- dd_stack %>% bind_rows(dd)
  }
}

to_plot <- dd_stack %>%
              filter(!(key %in% exclude))

file_path <- paste0(out_path, "/global_stock_dd_facet_1993_2022.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=pct, col = key)) +
  geom_line() +
  facet_wrap(~key) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_color_manual(values = my_colors, guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Declines from All Time Highs\n1993-2022")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Overlay
file_path <- paste0(out_path, "/global_stock_dd_overlay_1993_2022.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=pct, col = key)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_color_manual(values = my_colors) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "right") +
  ggtitle(paste0("Declines from All Time Highs\n1993-2022")) +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do DCA analysis
monthly_payment <- 10000/12

for(c in countries){
  tmp_cpi <- long %>%
      filter(key == "CPI") %>%
      select(date, value) %>%
      rename(cpi = value)
  
  tmp <- long %>%
    filter(key == c) %>%
    select(date, key, value) %>%
    left_join(tmp_cpi)
  
  for(i in 1:nrow(tmp)){
    if(i == 1){
      tmp[i, "port"] <- monthly_payment*tmp[i, "value"]
      tmp[i, "cb"] <- monthly_payment
      tmp[i, "cb_real"] <- monthly_payment*(last_cpi_value/tmp[i, "cpi"])
    } else{
      ret <- tmp[i, "value"]/tmp[(i-1), "value"] - 1
      
      tmp[i, "port"] <- (tmp[(i-1), "port"] * (1 + ret)) + monthly_payment
      tmp[i, "cb"] <- tmp[(i-1), "cb"] + monthly_payment
      tmp[i, "cb_real"] <- tmp[(i-1), "cb_real"] + (monthly_payment*(last_cpi_value/tmp[i, "cpi"]))
    }
  }
  
  if(c == countries[1]){
    dca_stack <- tmp %>% select(-cpi)
  } else {
    dca_stack <- dca_stack %>% bind_rows(tmp %>% select(-cpi))
  }
}

to_plot <- dca_stack %>%
  filter(!(key %in% exclude)) %>%
  select(-value, -cb_real) %>%
  gather(-date, -key, key = type, value = value)

last_dca_values <- to_plot %>%
                      filter(date == max(to_plot$date),
                             type == "port")
  
file_path <- paste0(out_path, "/global_stock_dca_1993_2022.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = type)) +
  geom_line() +
  facet_wrap(~key) +
  scale_color_manual(values = c("black", "green"), guide = "none") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("$300,000 DCA Investment by Country\n1993-2022")) +
  labs(x = "Date" , y = "Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #