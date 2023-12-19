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

folder_name <- "0378_big_up_year"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

min_year <- 1900

sp500 <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
        filter(date >= as.Date(paste0(min_year, "-01-01")))

calendar_rets <- sp500 %>%
                   filter(month(date) == 1) %>%
            mutate(calendar_ret = price_plus_div/lag(price_plus_div) - 1,
                   calendar_year = year(date) - 1,
                   fwd_ret_1yr = lead(price_plus_div)/price_plus_div - 1,
                   fwd_ret_5yr = (lead(price_plus_div, 5)/price_plus_div)^(1/5) - 1,
                   big_up_year = ifelse(calendar_ret > 0.20, "After a Big Up Year (>20%)", "All Other Years"),
                   big_down_year = ifelse(calendar_ret < -0.20, "After a Big Down Year (>20%)", "All Other Years")) %>%
            select(calendar_year, calendar_ret, contains("fwd_ret"), big_up_year, big_down_year) %>%
            filter(!is.na(big_up_year),
                   !is.na(fwd_ret_1yr))



summary_up <- calendar_rets %>%
                group_by(big_up_year) %>%
                summarise(median_ret = quantile(fwd_ret_1yr, probs = 0.5),
                         avg_ret = mean(fwd_ret_1yr)) %>%
                ungroup()

t.test(calendar_rets$fwd_ret_1yr~calendar_rets$big_up_year)

summary_down <- calendar_rets %>%
                group_by(big_down_year) %>%
                summarise(median_ret = quantile(fwd_ret_1yr, probs = 0.5),
                         avg_ret = mean(fwd_ret_1yr)) %>%
                ungroup()


t.test(calendar_rets$fwd_ret_1yr~calendar_rets$big_down_year)

to_plot <- calendar_rets

max_year <- max(to_plot$calendar_year) + 1

to_plot$big_up_year <- factor(to_plot$big_up_year, levels = c("All Other Years", "After a Big Up Year (>20%)"))

file_path <- paste0(out_path, "/dists_fwd_1yr_", min_year, "_", max_year, ".jpeg")
source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown include dividends and are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = fwd_ret_1yr, fill = big_up_year)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c(chart_standard_color, "red")) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("1-Year Return of U.S. Stocks\nAfter a Big Up Year and for All Other Years\n", min_year, "-", max_year)) +
  labs(x = "1-Year Return", y = "Frequency",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Now do 5yr
to_plot <- calendar_rets %>%
  filter(!is.na(fwd_ret_5yr))

to_plot$big_up_year <- factor(to_plot$big_up_year, levels = c("All Other Years", "After a Big Up Year (>20%)"))

file_path <- paste0(out_path, "/dists_fwd_5yr_", min_year, "_", max_year, ".jpeg")
source_string <- paste0("Source:  Shiller data (OfDollarsAndData.com)")
note_string <- str_wrap("Note: Returns shown include dividends and are adjusted for inflation.",
                        width = 80)

plot <- ggplot(data = to_plot, aes(x = fwd_ret_5yr, fill = big_up_year)) +
  geom_density(alpha = 0.3) +
  scale_fill_manual(values = c(chart_standard_color, "red")) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  ggtitle(paste0("5-Year Return of U.S. Stocks\nAfter a Big Up Year and for All Other Years\n", min_year, "-", max_year)) +
  labs(x = "5-Year Return (Annualized)" , y = "Frequency",
       caption = paste0(source_string, "\n", note_string))

# Save the gtable
ggsave(file_path, plot, width = 15, height = 12, units = "cm")



# ############################  End  ################################## #