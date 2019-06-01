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
library(tidyverse)

folder_name <- "0128_worst_bubble"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Bring in JPY data
fred_jpy_mcap_to_gdp <- read_excel(paste0(importdir, "0128_bubble_data/fred_mcap_to_gdp_DDDM01JPA156NWDB.xls"),
                                   skip = 10)

colnames(fred_jpy_mcap_to_gdp) <- c("date", "mcap_to_gdp")

fred_jpy_real_gdp <- read_excel(paste0(importdir, "0128_bubble_data/fred_japan_realgdp_JPNRGDPR.xls"),
                                   skip = 10)

colnames(fred_jpy_real_gdp) <- c("date", "real_gdp")

jpy <- fred_jpy_mcap_to_gdp %>%
          left_join(fred_jpy_real_gdp) %>%
          mutate(mcap_billions = mcap_to_gdp/100*real_gdp/1000,
                 date = as.Date(date)) %>%
          filter(!is.na(mcap_billions)) %>%
          select(date, mcap_billions)

jpy_index <- read.csv(paste0(importdir, "0114_japan_nikkei/nikk.csv"), skip = 1) %>%
            rename(date = Date,
                   index = Close) %>%
            mutate(date = as.Date(date, "%m/%d/%Y")) %>%
            select(date, index) 

# Bring in Bitcoin data
bcoin_mcap <- read_excel(paste0(importdir, "0128_bubble_data/bitcoin_coin_market_cap.xlsx")) %>%
                mutate(mcap_billions = mcap/(10^9),
                       date = as.Date(date)) %>%
                select(date, mcap_billions) %>%
                arrange(date)

# Bring in data for 1929
sp500 <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
            filter(date >= "1920-01-01")

price_gain_1929 <- pull(filter(sp500, date == "1929-09-01"), price_plus_div)/pull(sp500[1, "price_plus_div"])

# Reset file path
file_path <- paste0(out_path, "/jpy_mcap.jpeg")

# Set source/note
source_string <- paste0("Source:  FRED (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Japan's market capitalization is listed in 2011 dollars."), 
                          width = 85)

to_plot <- jpy

# Dates of interest for Japan
bubble_high <- as.Date("1989-01-01")
bubble_low <- as.Date("1998-01-01")

text_labels <- data.frame(date = c(bubble_high, bubble_low))

text_labels[1, "mcap_billions"] <- filter(to_plot, date == bubble_high) %>% pull(mcap_billions)
text_labels[1, "label"] <- paste0("$", formatC(round(text_labels[1, "mcap_billions"],0), 
                                               big.mark = ",", format = "f", digits = 0))
text_labels[2, "mcap_billions"] <- filter(to_plot, date == bubble_low) %>% pull(mcap_billions)
text_labels[2, "label"] <- paste0("$",  formatC(round(text_labels[2, "mcap_billions"],0), 
                                                big.mark = ",", format = "f", digits = 0))

plot <- ggplot(to_plot, aes(x=date, y=mcap_billions)) +
  geom_line() +
  geom_point(data=text_labels, aes(x=date, y=mcap_billions), col = ifelse(text_labels$date == bubble_high, "green","red")) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=text_labels, aes(x=date, y=mcap_billions),
                  color = ifelse(text_labels$date == bubble_high, "green","red"),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$date == bubble_high, 0, -200),
                  nudge_x = ifelse(text_labels$date == bubble_high, -800, 0)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Japan's Stock Market Lost Over $2 Trillion\nAfter the Bubble Burst")) +
  labs(x="Date", y="Market Capitalization\n(in billions)",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Reset file path
file_path <- paste0(out_path, "/bcoin_mcap_loss.jpeg")

# Set source/note
source_string <- paste0("Source:  CoinMarketCap.com (OfDollarsAndData.com)")

# Dates of interest for Bitcoin
bubble_high <- as.Date("2017-12-16")
bubble_low <- as.Date("2018-12-15")

text_labels <- data.frame(date = c(bubble_high, bubble_low))

text_labels[1, "mcap_billions"] <- filter(bcoin_mcap, date == bubble_high) %>% pull(mcap_billions)
text_labels[1, "label"] <- paste0("$", formatC(round(text_labels[1, "mcap_billions"],0), 
                                               big.mark = ",", format = "f", digits = 0))
text_labels[2, "mcap_billions"] <- filter(bcoin_mcap, date == bubble_low) %>% pull(mcap_billions)
text_labels[2, "label"] <- paste0("$",  formatC(round(text_labels[2, "mcap_billions"],0), 
                                                big.mark = ",", format = "f", digits = 0))

plot <- ggplot(bcoin_mcap, aes(x=date, y=mcap_billions)) +
  geom_line() +
  geom_point(data=text_labels, aes(x=date, y=mcap_billions), col = ifelse(text_labels$date == bubble_high, "green","red")) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=text_labels, aes(x=date, y=mcap_billions),
                  color = ifelse(text_labels$date == bubble_high, "green","red"),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$date == bubble_high, 0, -15),
                  nudge_x = ifelse(text_labels$date == bubble_high, -100, 0)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Bitcoin Lost Over $250 Billion\nFollowing Its 2017 Peak")) +
  labs(x="Date", y="Market Capitalization\n(in billions)",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Bring in DotCom data for NASDAQ
nq <- read.csv(paste0(importdir, "0128_bubble_data/IXIC_data.csv")) %>%
  rename(index = `NASDAQ.Composite.Level`) %>%
  mutate(date = as.Date(Period)) %>%
  arrange(date) %>%
  select(date, index) %>%
  filter(date >= as.Date("1990-01-01"))

# Dates of interest for NASDAQ
bubble_high <- as.Date("2000-03-10")
bubble_low <- as.Date("2002-10-09")

highest_nq <- filter(nq, date == bubble_high) %>%
  pull(index)

highest_nq_mcap <- 6600

nq <- nq %>%
  mutate(mcap_billions = highest_nq_mcap*index/highest_nq)

lowest_nq_mcap <- filter(nq, date == bubble_low) %>%
  pull(mcap_billions)

print(paste0("The NASDAQ lost $", formatC(highest_nq_mcap - lowest_nq_mcap, digits = 0, format="f", big.mark = ","), "B in market cap."))

# Plot NASDAQ
# Reset file path
file_path <- paste0(out_path, "/nasdaq_mcap_loss.jpeg")

# Set source/note
source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")

text_labels <- data.frame(date = c(bubble_high, bubble_low))

text_labels[1, "mcap_billions"] <- filter(nq, date == bubble_high) %>% pull(mcap_billions)
text_labels[1, "label"] <- paste0("$", formatC(round(text_labels[1, "mcap_billions"],0), 
                                               big.mark = ",", format = "f", digits = 0))
text_labels[2, "mcap_billions"] <- filter(nq, date == bubble_low) %>% pull(mcap_billions)
text_labels[2, "label"] <- paste0("$",  formatC(round(text_labels[2, "mcap_billions"],0), 
                                                big.mark = ",", format = "f", digits = 0))

plot <- ggplot(nq, aes(x=date, y=mcap_billions)) +
  geom_line() +
  geom_point(data=text_labels, aes(x=date, y=mcap_billions), col = ifelse(text_labels$date == bubble_high, "green","red")) +
  scale_y_continuous(label = dollar) +
  geom_text_repel(data=text_labels, aes(x=date, y=mcap_billions),
                  color = ifelse(text_labels$date == bubble_high, "green","red"),
                  label = text_labels$label,
                  size = 3.5,
                  family = "my_font",
                  max.iter = 1,
                  segment.colour = "transparent",
                  nudge_y = ifelse(text_labels$date == bubble_high, 400, -400)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("NASDAQ Lost Over $5 Trillion\nFollowing Its 2000 Peak")) +
  labs(x="Date", y="Market Capitalization\n(in billions)",
       caption = paste0("\n", source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #