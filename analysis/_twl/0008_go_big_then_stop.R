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
library(tidyverse)

folder_name <- "_twl/0008_go_big_then_stop"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

pv <- 10000
ret <- 0.07
n_years <- 40

final_results <- data.frame()

for(n in 1:n_years){
   fv <- pv * (1+ret)^(n_years - n)
   
   final_results[n, "year"] <- n
   final_results[n, "fv_constant"] <- fv
   
   if(n == 1){
      final_results[n, "fv_early"] <- fv
      final_results[n, "fv_late"] <- 0
      final_results[n, "cumulative_fv_constant"] <- fv
      final_results[n, "cumulative_fv_early"] <- fv
      final_results[n, "cumulative_fv_late"] <- 0
      final_results[n, "value_port_early"] <- pv
      final_results[n, "value_port_late"] <- 0
   } else{
      if(n <= 10){
         final_results[n, "fv_early"] <- fv
         final_results[n, "fv_late"] <- 0
         final_results[n, "value_port_early"] <- final_results[(n-1), "value_port_early"] * (1+ret) + pv
         final_results[n, "value_port_late"] <- 0
      } else{
         final_results[n, "fv_early"] <- 0
         final_results[n, "fv_late"] <- fv
         final_results[n, "value_port_early"] <- final_results[(n-1), "value_port_early"] * (1+ret)
         final_results[n, "value_port_late"] <- final_results[(n-1), "value_port_late"] * (1+ret) + pv
      }
      
      
      final_results[n, "cumulative_fv_constant"] <- final_results[(n-1), "cumulative_fv_constant"] + final_results[n, "fv_constant"]
      final_results[n, "cumulative_fv_early"] <- final_results[(n-1), "cumulative_fv_early"] + final_results[n, "fv_early"]
      final_results[n, "cumulative_fv_late"] <- final_results[(n-1), "cumulative_fv_late"] + final_results[n, "fv_late"]
      
   }
}

final_cumulative <- final_results[nrow(final_results), "cumulative_fv_constant"]

to_plot <- final_results %>%
               mutate(cumulative_pct = cumulative_fv_constant/final_cumulative,
                      individual_pct = fv_constant/final_cumulative) %>%
               select(year, cumulative_pct, individual_pct)

# Individual plot
file_path <- paste0(out_path, "/individual_pct_final_portfolio_by_year.jpeg")
source_string <- "Source: Simulated data (OfDollarsAndData.com)"
note_string <- str_wrap(paste0("Note: Assumes constant savings for ", n_years, " years and a ", 100*ret, "% annual rate of return."))

plot <- ggplot(to_plot, aes(x= year, y=individual_pct)) +
   geom_bar(stat = "identity", fill = chart_standard_color) +
   scale_y_continuous(label = percent_format(accuracy = 1)) +
   of_dollars_and_data_theme +
   ggtitle(paste0("Percentage of Final Portfolio\nBy Year Saved")) +
   labs(x="Year", y="Percentage of Final Portfolio",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Cumulative plot
file_path <- paste0(out_path, "/cumulative_pct_final_portfolio_by_year.jpeg")

point <- to_plot %>%
            filter(year == 10)

plot <- ggplot(to_plot, aes(x= year, y=cumulative_pct)) +
   geom_line() +
   geom_point(data=point, aes(x=year, y=cumulative_pct), col = "red") +
   scale_y_continuous(label = percent_format(accuracy = 1)) +
   of_dollars_and_data_theme +
   ggtitle(paste0("Over Half Your Final Portfolio is Built\nin the First Decade of Saving")) +
   labs(x="Year", y="Cumulative Percentage of Final Portfolio",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Early vs. late plot
to_plot <- final_results %>%
   select(year, contains("value_")) %>%
   rename(`Start Early` = value_port_early,
          `Start Late` = value_port_late) %>%
   gather(-year, key=key, value=value)

file_path <- paste0(out_path, "/early_vs_late_portfolio.jpeg")
note_string <- str_wrap(paste0("Note: Assumes $10,000 in annual savings for ", n_years, " years and a ", 100*ret, "% annual rate of return.  ",
                               "The 'Start Early' portfolio saves for 10 years then stops saving, while the 'Start Late' portfolio ",
                               "waits 10 years before saving for the next 30 years."),
                        width = 80)

text_labels <- data.frame()

text_labels[1, "year"] <- 25
text_labels[1, "value"] <- 600000
text_labels[1, "key"] <-"Start Early"
text_labels[1, "label"] <- "Start Early"

text_labels[2, "year"] <- 32
text_labels[2, "value"] <- 250000
text_labels[2, "key"] <-"Start Late"
text_labels[2, "label"] <- "Start Late"

point <- to_plot %>%
            filter(year == 10, key == "Start Early")

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   geom_text(data = text_labels, aes(x=year, y=value, col = key, label = label)) +
   scale_color_manual(values = c("blue", "black"), guide = FALSE) +
   scale_y_continuous(label = dollar) +
   of_dollars_and_data_theme +
   ggtitle(paste0("The Importance of Starting Early")) +
   labs(x="Year", y="Portfolio Value",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Taken from post 19
delay_vector   <- seq(1, 20)
returns_vector <- ret

results_df    <- data.frame()

i <- 0
for (d in delay_vector){
   for (r in returns_vector){
      i <- i + 1
      future_value                   <- pv * (((1 + r)^n_years - 1) / r)
      delay_payment                  <- (future_value * r)/((1 + r)^(n_years - d) - 1)
      total_capital                  <- pv * n_years
      total_capital_delayed          <- delay_payment * (n_years - d)
      
      results_df[i, "increase_pct"]            <- delay_payment/pv - 1
      results_df[i, "return"]                  <- r
      results_df[i, "years_delayed"]           <- d
      results_df[i, "extra_initial_payment"]   <- (total_capital_delayed - total_capital)/ pv
   }
}

to_plot <- results_df %>%
            select(years_delayed, increase_pct)

file_path <- paste0(out_path, "/pct_increase_delay.jpeg")
note_string <- str_wrap(paste0("Note: Assumes constant savings for ", n_years, " years and a ", 100*ret, "% annual rate of return."))


plot <- ggplot(to_plot, aes(x= years_delayed, y=increase_pct)) +
   geom_line() +
   scale_y_continuous(label = percent_format(accuracy = 1)) +
   of_dollars_and_data_theme +
   ggtitle(paste0("How Much More You Have to Save\nFor Each Year You Delay")) +
   labs(x="Years Delayed", y="How Much More You Have to Save",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# ############################  End  ################################## #