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

folder_name <- "0260_dip_buy_threshold"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- readRDS(paste0(localdir, "/0009_sp500_ret_pe.Rds")) %>%
        filter(date >= "1920-01-01", date <= "2020-12-31") %>%
        select(date, price_plus_div)

dd <- drawdown_path(raw)
monthly_amount <- 100

run_dip_buying <- function(start_date, end_date, dd_threshold){
  tmp <- raw %>%
          filter(date >= start_date,
                 date <= end_date) %>%
          left_join(dd)
  

  cash_saving <- 1
  
  for(i in 1:nrow(tmp)){
    if(i == 1){
      tmp[i, "value_dca"] <- monthly_amount
      tmp[i, "value_cash"] <- monthly_amount
      tmp[i, "value_dip"] <- 0
    } else{
      dd <- tmp[(i-1), "pct"]
      ret <- tmp[i, "price_plus_div"]/tmp[(i-1), "price_plus_div"] - 1
      
      if(dd < dd_threshold){
        cash_saving <- 0
        tmp[i, "value_cash"] <- 0
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret) + tmp[(i-1), "value_cash"] + monthly_amount
      } else if (dd == 0){
        cash_saving <- 1
        tmp[i, "value_cash"] <- tmp[(i-1), "value_cash"] + monthly_amount
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret)
      } else if (cash_saving == 1){
        tmp[i, "value_cash"] <- tmp[(i-1), "value_cash"] + monthly_amount
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret)
      } else if (cash_saving == 0){
        tmp[i, "value_cash"] <- 0
        tmp[i, "value_dip"] <- tmp[(i-1), "value_dip"] * (1 + ret) + monthly_amount
      }
      tmp[i, "value_dca"] <- tmp[(i-1), "value_dca"] * (1 + ret) + monthly_amount
    }
  }
  
  if(start_date == as.Date("1980-01-01") & dd_threshold == -0.5){
    assign("tmp_1980_50pct", tmp, envir = .GlobalEnv)
  } else if(start_date == as.Date("1963-01-01") & dd_threshold == -0.4){
    assign("tmp_1963_40pct", tmp, envir = .GlobalEnv)
  } else if(start_date == as.Date("1970-01-01") & dd_threshold == -0.4){
    assign("tmp_1970_40pct", tmp, envir = .GlobalEnv)
  }
  
  return(tmp)
}

n_years <- 20

jan_only <- raw %>%
              filter(month(date) == 1)

start_dates <- pull(jan_only[1:(nrow(jan_only)- n_years + 1), "date"])
dd_thresholds <- seq(-0.1, -0.5, -0.1)

final_results <- data.frame()

counter <- 1
for(s in 1:length(start_dates)){
  start_dt <- start_dates[s]
  print(start_dt)
  end_dt <- as.Date(start_dt + months((12*n_years)-1))
  print(end_dt)
  
  for(d in dd_thresholds){
    fnl <- run_dip_buying(start_dt, end_dt, d)
    
    final_results[counter, "start_date"] <- start_dt
    final_results[counter, "end_date"] <- end_dt
    final_results[counter, "dd_threshold"] <- d
    final_results[counter, "final_dca"] <- fnl[nrow(fnl), "value_dca"]
    final_results[counter, "final_dip"] <- fnl[nrow(fnl), "value_dip"] + fnl[nrow(fnl), "value_cash"]
    counter <- counter + 1
  }
}

final_results <- final_results %>%
            mutate(btd_win = ifelse(final_dip > final_dca, 1, 0),
                   btd_outperf = final_dip/final_dca - 1,
                   dd_threshold = paste0(100*dd_threshold, "%"))

summary <- final_results %>%
            group_by(dd_threshold) %>%
            summarise(mean_btd_win = mean(btd_win),
                      median_btd_outperf = quantile(btd_outperf, probs = 0.5)) %>%
            ungroup()

export_to_excel(df = summary,
                outfile = paste0(out_path, "/summary_results.xlsx"),
                sheetname = "results",
                new_file = 1,
                fancy_formatting = 0)

# Do when BTD wins
to_plot <- tmp_1963_40pct %>%
              mutate(`Buy the Dip` = value_dip + value_cash,
                     DCA = value_dca) %>%
              select(date, DCA, `Buy the Dip`) %>%
              gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/btd_1963_win.jpeg")
source_string <- paste0("Source:  http://www.econ.yale.edu/~shiller/data.htm (OfDollarsAndData.com)")
note_string <- paste0("Note: Both strategies save $", monthly_amount, " per month.")

plot <- ggplot(to_plot, aes(x= date, y=value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("When Buy the Dip Beats DCA\n1963-1983")) +
  labs(x="Date", y="Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# When DCA beats BTD
to_plot <- tmp_1980_50pct %>%
  mutate(`Buy the Dip` = value_dip + value_cash,
         DCA = value_dca) %>%
  select(date, DCA, `Buy the Dip`) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/dca_1980_win.jpeg")

plot <- ggplot(to_plot, aes(x= date, y=value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("When DCA Beats Buy the Dip\n1980-2000")) +
  labs(x="Date", y="Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Plot 1990 cash and dip
first_value <- pull(tmp_1970_40pct[1, "price_plus_div"])

to_plot <- tmp_1970_40pct %>%
  mutate(Index = 10000*price_plus_div/first_value,
         Cash = value_cash) %>%
  select(date, Index, Cash) %>%
  gather(-date, key=key, value=value)

points <- tmp_1970_40pct %>%
              mutate(Index = 10000*price_plus_div/first_value,
                     Cash = value_cash) %>%
                    filter(Cash == 0) %>%
              select(date, Index) %>%
              gather(-date, key=key, value=value)

text_labels <- data.frame()
text_labels[1, "date"] <- as.Date("1980-01-01")
text_labels[1, "value"] <- 15000
text_labels[1, "label"] <- "Buy the Dip\nPurchases"

file_path <- paste0(out_path, "/how_btd_works_1970.jpeg")
note_string2 <- paste0("Note: Buy the Dip saves $", monthly_amount, " per month.")

plot <- ggplot(to_plot, aes(x= date, y=value, col = key)) +
  geom_line() +
  geom_point(data = points, aes(x=date, y=value), col = "red", alpha = 0.5) +
  geom_text(data = text_labels, aes(x=date, y= value, label = label), col = "red",
            family = "my_font") +
  scale_color_manual(values = c("green", "black")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("How Buy the Dip Works\n1970-1990")) +
  labs(x="Date", y="Growth of $10,000",
       caption = paste0(source_string, "\n", note_string2))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do one more BTD vs DCA
to_plot <- tmp_1970_40pct %>%
  mutate(`Buy the Dip` = value_dip + value_cash,
         DCA = value_dca) %>%
  select(date, DCA, `Buy the Dip`) %>%
  gather(-date, key=key, value=value)

file_path <- paste0(out_path, "/btd_win_1970.jpeg")

plot <- ggplot(to_plot, aes(x= date, y=value, col = key)) +
  geom_line() +
  scale_color_manual(values = c("red", "black")) +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Buy the Dip vs. DCA\n1970-1990")) +
  labs(x="Date", y="Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do geom joy plot of all results
to_plot <- final_results 

to_plot$dd_threshold <- factor(to_plot$dd_threshold, levels = rev(c("-10%", "-20%", "-30%", "-40%", "-50%")))

file_path <- paste0(out_path, "/btd_outperf_by_dd.jpeg")

text_labels <- data.frame()
text_labels[1, "btd_outperf"] <- -0.8
text_labels[1, "dd_threshold"] <- "-20%"
text_labels[1, "label"] <- "Buy the Dip\nUnderperforms"
text_labels[2, "btd_outperf"] <- 0.45
text_labels[2, "dd_threshold"] <- "-20%"
text_labels[2, "label"] <- "Buy the Dip\nOutperforms"

plot <- ggplot(to_plot, aes(x= btd_outperf, y=dd_threshold, fill = dd_threshold)) +
  geom_joy_gradient(rel_min_height = 0.01, scale = 3) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_text(data=text_labels, aes(x=btd_outperf, y=dd_threshold, label = label),
            family = "my_font",
            size = 3) +
  scale_fill_discrete(guide = "none") +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Buy the Dip Outperformance Over DCA\nby Dip Threshold")) +
  labs(x="BTD Outperformance over DCA", y="Dip Threshold",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #