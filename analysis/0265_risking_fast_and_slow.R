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
library(tidylog)
library(zoo)
library(ggjoy)
library(tidyverse)

folder_name <- "0265_risking_fast_and_slow"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "/0265_risking_fast_and_slow/returns20_stock_cash_cpi.csv"), skip = 7,
                col.names = c("date", "ret_1m", "ret_sp500", "ret_cpi", "drop")) %>%
        select(-drop) %>%
        drop_na() %>%
        mutate(date = as.Date(date, "%m/%d/%Y"),
               ret_sp500_real = ret_sp500 - ret_cpi,
               ret_1m_real = ret_1m - ret_cpi) %>%
        select(date, contains("_real"))

df <- raw 

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "index_sp500"] <- 1
    df[i, "index_cash"] <- 1
  } else{
    df[i, "index_sp500"] <- df[(i-1), "index_sp500"] * (1 + df[i, "ret_sp500_real"])
    df[i, "index_cash"] <- df[(i-1), "index_cash"] * (1 + df[i, "ret_1m_real"])
  }
}

months_to_run <- c(1, 12, 60, 120, 240)

final_results <- data.frame()
counter <- 1
for(m in months_to_run){
  tmp <-  df %>%
            mutate(pos_sp500 = ifelse(index_sp500 > 0.95*lag(index_sp500, m), 1, 0),
                   pos_cash = ifelse(index_cash > 0.95*lag(index_cash, m), 1, 0)) %>%
            drop_na()
  
  if(m == 1){
    label <- "1 Month"
  } else if (m > 1 & m <= 12){
    label <- paste0(m, " Months")
  } else{
    label <- paste0(m/12, " Years")
  }
  
  final_results[counter, "label"] <- label
  final_results[counter, "pos_sp500"] <- 1-mean(tmp$pos_sp500)
  final_results[counter, "pos_cash"] <- 1-mean(tmp$pos_cash)
  
  counter <- counter + 1
}

final_results$label <- factor(final_results$label, levels = final_results$label)

to_plot <- final_results %>%
            gather(-label, key=key, value=value) %>%
            mutate(key = ifelse(key == "pos_sp500", "S&P 500", "Cash"))

text_labels <- to_plot %>%
                mutate(lbl = paste0(round(value*100, 0), "%"))

first_year <- year(min(df$date))
last_year <- year(max(df$date))

file_path <- paste0(out_path, "/sp500_vs_cash_positive_bars.jpeg")
source_string <- paste0("Source: Returns 2.0, ", first_year, "-", last_year, " (OfDollarsAndData.com)")
note_string <- "Note: All returns adjusted for inflation. Cash return is 1-Month Treasury Bills."

plot <- ggplot(to_plot, aes(x= label, y=value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = text_labels, aes(x=label, y=value + 0.0095, col = key, label = lbl),
            position = position_dodge(width = 1),
            size = 3,
            family = "my_font") +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("black", "green")) +
  scale_color_manual(values = c("black", "green")) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle(paste0("Probability of Investment Being Down >5%\nby Holding Period")) +
  labs(x="Holding Period", y="Percentage of Months",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #