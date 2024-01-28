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
library(tidyverse)

folder_name <- "0383_all_time_high_analysis"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dow_pre_2020 <- read_excel(paste0(importdir, "/0383_daily_dow/Dow_daily_pre_2020.xlsx"),
                           col_names = c("date", "index")) %>%
                  mutate(date = as.Date(date))

dow_post_2020 <- read.csv(paste0(importdir, "/0383_daily_dow/ycharts_DJI_data_2020_onward.csv"),
                          col.names = c("date", "index")) %>%
                  mutate(date = as.Date(date))

djia <- dow_pre_2020 %>%
        bind_rows(dow_post_2020) %>%
        arrange(date)

for(i in 1:nrow(djia)){
  if(i == 1){
    djia[i, "ath"] <- 1
    ath <- djia[i, "index"]
  } else{
    current <- djia[i, "index"]
    
    if(current > ath){
      djia[i, "ath"] <- 1
      ath <- current
    } else{
      djia[i, "ath"] <- 0
    }
  }
}

min_year <- min(year(djia$date))
max_year <- max(year(djia$date))

to_plot <- djia %>%
            mutate(ret_1yr = lead(index, 250)/index - 1,
                   ret_5yr = (lead(index, 250*5)/index)^(1/5) - 1,
                   ret_10yr = (lead(index, 250*10)/index)^(1/10) - 1)

t.test(to_plot$ret_1yr~to_plot$ath)
t.test(to_plot$ret_5yr~to_plot$ath)
t.test(to_plot$ret_10yr~to_plot$ath)

file_path <- paste0(out_path, "/djia_ath_", min_year, "_", max_year, ".jpeg")
source_string <- paste0("Source:  Blooomberg (OfDollarsAndData.com)")

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = index)) +
  geom_line() +
  geom_point(data = djia %>% filter(ath == 1), aes(x=date, y=index), 
             col = "red",
             size = 0.6) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Dow Jones Industrial Average\nAll-Time Highs\n", min_year, "-", max_year)) +
  labs(x = "Date", y = "Index Value (Log Scale)",
       caption = paste0(source_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Get return summaries
ret_summary <- to_plot %>%
                group_by(ath) %>%
                summarise(`1 Year` = mean(ret_1yr, na.rm = TRUE),
                          `5 Years` = mean(ret_5yr, na.rm = TRUE),
                          `10 Years` = mean(ret_10yr, na.rm = TRUE)) %>%
                ungroup() %>%
                mutate(ath = ifelse(ath == 1, 
                                    "At All-Time Highs",
                                    "All Other Days")) %>%
                gather(-ath, key=key, value=value)

ret_summary$ath <- factor(ret_summary$ath, levels = c("At All-Time Highs", "All Other Days"))
ret_summary$key <- factor(ret_summary$key, levels = c("1 Year", "5 Years", "10 Years"))

labels <- ret_summary %>%
            mutate(label = paste0(100*round(value, 3), "%"),
                   label = case_when(!grepl("\\.", label) ~ paste0(gsub("(.*?)%", "\\1", label), ".0%"),
                                       TRUE ~ label))

file_path <- paste0(out_path, "/djia_returns_by_ath_status_", min_year, "_", max_year, ".jpeg")
source_string <- paste0("Source:  Blooomberg (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Performance does not include dividends and is not adjusted for inflation."),
                        width = 80)

# Create the plot object
plot <- ggplot(ret_summary, aes(x = key, y = value, fill = as.factor(ath))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = labels, aes(x= key, y = value, 
                               label = label,
                               hjust = ifelse(ath == "At All-Time Highs", 1.5, -0.6),
                               vjust = -0.25),
                               color = "black",
            family = my_font) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.1), breaks = seq(0, 0.1, 0.01)) +
  scale_fill_manual(values = c("#349800", "black")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Dow Future Average Annualized Return\nBy Time Horizon and All-Time High Status\n", min_year, "-", max_year)) +
  labs(x = "Time Horizon", y = "Future Annualized Return",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Now do switching analysis
my_rets <- read.csv(paste0(importdir, "0383_daily_dow/GrowthOfWealth_20240126044615.csv"),
                    skip = 7, col.names = c("date", "index_world", "index_5yr", "index_sp500")) %>%
            filter(!is.na(index_world)) %>%
            mutate(date = as.Date(date, format = "%m/%d/%Y")) %>%
            arrange(date) %>%
            mutate(ret_world = index_world/lag(index_world) - 1,
                   ret_5yr = index_5yr/lag(index_5yr) - 1,
                   ret_sp500 = index_sp500/lag(index_sp500) - 1)

df <- my_rets

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "value_bh_world"] <- 1
    df[i, "value_switch_world"] <- 1
    
    df[i, "value_bh_us"] <- 1
    df[i, "value_switch_us"] <- 1
    
    ath_world <- df[i, "index_world"]
    ath_us <- df[i, "index_sp500"]
  } else{
    prior_world <- df[(i-1), "index_world"]
    if(prior_world >= ath_world){
      df[i, "value_switch_world"] <- df[(i-1), "value_switch_world"] * (1 + df[i, "ret_world"])
      ath_world <- prior_world
    } else{
      df[i, "value_switch_world"] <- df[(i-1), "value_switch_world"] * (1 + df[i, "ret_5yr"])
    }
    
    prior_us <- df[(i-1), "index_sp500"]
    if(prior_us >= ath_us){
      df[i, "value_switch_us"] <- df[(i-1), "value_switch_us"] * (1 + df[i, "ret_sp500"])
      ath_us <- prior_us
    } else{
      df[i, "value_switch_us"] <- df[(i-1), "value_switch_us"] * (1 + df[i, "ret_5yr"])
    }
    
    df[i, "value_bh_world"] <- df[(i-1), "value_bh_world"] * (1 + df[i, "ret_world"])
    df[i, "value_bh_us"] <- df[(i-1), "value_bh_us"] * (1 + df[i, "ret_sp500"])
  }
}

# Do Global Stocks then US stocks
to_plot <- df %>%
              rename(`Buy & Hold` = value_bh_world,
                     `Invest Following\nAll-Time Highs` = value_switch_world) %>%
              select(date, `Buy & Hold`, `Invest Following\nAll-Time Highs`) %>%
              gather(-date, key=key, value = value)

mr_min_year <- min(year(to_plot$date))
mr_max_year <- max(year(to_plot$date))

file_path <- paste0(out_path, "/switch2_world_vs_buy_hold_", mr_min_year, "_", mr_max_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Performance includes dividends, but is not adjusted for inflation. ",
                               "The 'Invest Following All-Time Highs' model is invested in Global Stocks (MSCI World Index) when the index hits an all-time high in the prior month, ",
                               "otherwise invests in 5-Year U.S. Treasury Bonds/Notes."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1998-01-01")
text_labels[1, "value"] <- 120
text_labels[1, "label"] <- "Invest Following\nAll-Time Highs"
text_labels[2, "date"] <- as.Date("2000-01-01")
text_labels[2, "value"] <- 9
text_labels[2, "label"] <- "Buy & Hold"

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = value, color = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x = date, y = value, color = label,
                                    label = label),
            family = my_font) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  scale_x_date(date_labels = "%Y") +
  scale_color_manual(values = c("black", "#349800"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Invest Following All-Time Highs vs. Buy & Hold\nGlobal Stocks\n", mr_min_year, "-", mr_max_year)) +
  labs(x = "Date", y = "Growth of $1 (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do US
to_plot <- df %>%
  rename(`Buy & Hold` = value_bh_us,
         `Invest Following\nAll-Time Highs` = value_switch_us) %>%
  select(date, `Buy & Hold`, `Invest Following\nAll-Time Highs`) %>%
  gather(-date, key=key, value = value)

mr_min_year <- min(year(to_plot$date))
mr_max_year <- max(year(to_plot$date))

file_path <- paste0(out_path, "/switch2_us_vs_buy_hold_", mr_min_year, "_", mr_max_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Performance includes dividends, but is not adjusted for inflation. ",
                               "The 'Invest Following All-Time Highs' model is invested in U.S. Stocks (S&P 500) when U.S. Stocks hit an all-time high in the prior month, ",
                               "otherwise invests in 5-Year U.S. Treasury Bonds/Notes."),
                        width = 80)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("2005-01-01")
text_labels[1, "value"] <- 9
text_labels[1, "label"] <- "Invest Following\nAll-Time Highs"
text_labels[2, "date"] <- as.Date("2000-01-01")
text_labels[2, "value"] <- 120
text_labels[2, "label"] <- "Buy & Hold"

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = value, color = key)) +
  geom_line() +
  geom_text(data = text_labels, aes(x = date, y = value, color = label,
                                    label = label),
            family = my_font) +
  scale_y_continuous(label = dollar, trans = log10_trans()) +
  scale_x_date(date_labels = "%Y") +
  scale_color_manual(values = c("black", "#349800"), guide = "none") +
  of_dollars_and_data_theme +
  ggtitle(paste0("Invest Following All-Time Highs vs. Buy & Hold\nU.S. Stocks\n", mr_min_year, "-", mr_max_year)) +
  labs(x = "Date", y = "Growth of $1 (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do ATH analysis for world and US
ath <- df

for(i in 1:nrow(ath)){
  if(i == 1){
    ath[i, "ath_world"] <- 1
    ath[i, "ath_us"] <- 1
    
    ath_world <- ath[i, "index_world"]
    ath_us <- ath[i, "index_sp500"]
  } else{
    current_world <- ath[i, "index_world"]
    current_us <- ath[i, "index_sp500"]
    
    if(current_world > ath_world){
      ath[i, "ath_world"] <- 1
      ath_world <- ath[i, "index_world"]
    } else{
      ath[i, "ath_world"] <- 0
    }
    
    if(current_us > ath_us){
      ath[i, "ath_us"] <- 1
      ath_us <- ath[i, "index_sp500"]
    } else{
      ath[i, "ath_us"] <- 0
    }
  }
}

to_plot <- ath

min_year_ath_world <- min(year(to_plot$date))
max_year_ath_world <- max(year(to_plot$date))

file_path <- paste0(out_path, "/msci_world_ath_", min_year, "_", max_year, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <- paste0("Note:  The MSCI World Index includes dividends, but is not adjusted for inflation.")

# Create the plot object
plot <- ggplot(to_plot, aes(x = date, y = index_world)) +
  geom_line() +
  geom_point(data = to_plot %>% filter(ath_world == 1), aes(x=date, y=index_world), 
             col = "red",
             size = 0.6) +
  scale_y_continuous(label = comma, trans = log10_trans()) +
  scale_x_date(date_labels = "%Y") +
  of_dollars_and_data_theme +
  ggtitle(paste0("MSCI World Index\nAll-Time Highs\n", min_year_ath_world, "-", max_year_ath_world)) +
  labs(x = "Date", y = "Growth of $1 (Log Scale)",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do ret world summary
to_plot_world <- to_plot %>%
  rename(ath = ath_world,
         index = index_world) %>%
  select(index, ath) %>%
  mutate(ret_1yr = lead(index, 12)/index - 1,
         ret_5yr = (lead(index, 12*5)/index)^(1/5) - 1,
         ret_10yr = (lead(index, 12*10)/index)^(1/10) - 1)

ret_world_summary <- to_plot_world %>%
                        group_by(ath) %>%
                        summarise(`1 Year` = mean(ret_1yr, na.rm = TRUE),
                                  `5 Years` = mean(ret_5yr, na.rm = TRUE),
                                  `10 Years` = mean(ret_10yr, na.rm = TRUE)) %>%
                        ungroup() %>%
                        mutate(ath = ifelse(ath == 1, 
                                            "At All-Time Highs",
                                            "All Other Months")) %>%
                        gather(-ath, key=key, value=value)

ret_world_summary$ath <- factor(ret_world_summary$ath, levels = c("At All-Time Highs", "All Other Months"))
ret_world_summary$key <- factor(ret_world_summary$key, levels = c("1 Year", "5 Years", "10 Years"))


labels <- ret_world_summary %>%
  mutate(label = paste0(100*round(value, 3), "%"),
         label = case_when(!grepl("\\.", label) ~ paste0(gsub("(.*?)%", "\\1", label), ".0%"),
                           TRUE ~ label))

file_path <- paste0(out_path, "/msci_world_returns_by_ath_status_", min_year_ath_world, "_", max_year_ath_world, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Performance includes dividends, but is not adjusted for inflation."),
                        width = 80)

# Create the plot object
plot <- ggplot(ret_world_summary, aes(x = key, y = value, fill = as.factor(ath))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = labels, aes(x= key, y = value, 
                               label = label,
                               hjust = ifelse(ath == "At All-Time Highs", 1.4, -0.52),
                               vjust = -0.25),
            color = "black",
            family = my_font) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.14), breaks = seq(0, 0.14, 0.01)) +
  scale_fill_manual(values = c("#349800", "black")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("MSCI World Future Average Annualized Return\nBy Time Horizon and All-Time High Status\n", min_year_ath_world, "-", max_year_ath_world)) +
  labs(x = "Time Horizon", y = "Future Annualized Return",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do S&P 500 monthly summary
to_plot_us <- to_plot %>%
  rename(ath = ath_us,
         index = index_sp500) %>%
  select(index, ath) %>%
  mutate(ret_1yr = lead(index, 12)/index - 1,
         ret_5yr = (lead(index, 12*5)/index)^(1/5) - 1,
         ret_10yr = (lead(index, 12*10)/index)^(1/10) - 1)

ret_us_summary <- to_plot_us %>%
                      group_by(ath) %>%
                      summarise(`1 Year` = mean(ret_1yr, na.rm = TRUE),
                                `5 Years` = mean(ret_5yr, na.rm = TRUE),
                                `10 Years` = mean(ret_10yr, na.rm = TRUE)) %>%
                      ungroup() %>%
                      mutate(ath = ifelse(ath == 1, 
                                          "At All-Time Highs",
                                          "All Other Months")) %>%
                      gather(-ath, key=key, value=value)

ret_us_summary$ath <- factor(ret_us_summary$ath, levels = c("At All-Time Highs", "All Other Months"))
ret_us_summary$key <- factor(ret_us_summary$key, levels = c("1 Year", "5 Years", "10 Years"))

labels <- ret_us_summary %>%
  mutate(label = paste0(100*round(value, 3), "%"),
         label = case_when(!grepl("\\.", label) ~ paste0(gsub("(.*?)%", "\\1", label), ".0%"),
                           TRUE ~ label))

file_path <- paste0(out_path, "/sp500_returns_by_ath_status_", min_year_ath_world, "_", max_year_ath_world, ".jpeg")
source_string <- paste0("Source:  Returns 2.0 (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Performance includes dividends, but is not adjusted for inflation."),
                        width = 80)

# Create the plot object
plot <- ggplot(ret_us_summary, aes(x = key, y = value, fill = as.factor(ath))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = labels, aes(x= key, y = value, 
                               label = label,
                               hjust = ifelse(ath == "At All-Time Highs", 1.4, -0.4),
                               vjust = -0.25),
            color = "black",
            family = my_font) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(0, 0.15), breaks = seq(0, 0.15, 0.01)) +
  scale_fill_manual(values = c("#349800", "black")) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("S&P 500 Future Average Annualized Return\nBy Time Horizon and All-Time High Status\n", min_year_ath_world, "-", max_year_ath_world)) +
  labs(x = "Time Horizon", y = "Future Annualized Return",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Run extra t-tests
t.test(to_plot_world$ret_1yr~to_plot_world$ath)
t.test(to_plot_world$ret_5yr~to_plot_world$ath)
t.test(to_plot_world$ret_10yr~to_plot_world$ath)

t.test(to_plot_us$ret_1yr~to_plot_us$ath)
t.test(to_plot_us$ret_5yr~to_plot_us$ath)
t.test(to_plot_us$ret_10yr~to_plot_us$ath)  

# ############################  End  ################################## #