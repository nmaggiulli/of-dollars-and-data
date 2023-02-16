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

folder_name <- "0151_simons_returns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

end_year <- 2021

# From Zuckerman's book + 2019-2021 (public data)
simons_ret <- data.frame(
                  year = seq(1987, end_year),
                  ret_net = c(0, 0.09, -0.04, 0.55, 0.394, 0.336, 0.391, 0.707, 0.383,
                              0.315, 0.212, 0.417, 0.245, 0.985,
                              0.33, 0.258, 0.219, 0.249, 0.295,
                              0.443, 0.737, 0.824, 0.39, 0.294,
                              0.37, 0.29, 0.469, 0.392, 0.36,
                              0.356, 0.45, 0.40, 0.19, 0.398, 0.241),
                  fee_m = rep(0.05, 35),
                  fee_perf = c(0, rep(0.2, 13), 0.36, rep(0.44, 20)),
                  ret_gross = c(0, 0.163, 0.01, 0.778, 0.543, 0.47, 0.539, 0.934, 0.529,
                                0.444, 0.315, 0.571, 0.356, 1.281,
                                0.566, 0.511, 0.441, 0.495, 0.577, 
                                0.841, 1.366, 1.521, 0.746, 0.575,
                                0.711, 0.568, 0.888, 0.75, 0.693,
                                0.686, 0.854, 0.764, 
                                0.39, 0.76, 0.48)
)

sp500_dfa <- data.frame(
  year = seq(1987, end_year),
  ret_sp500 = c(0, 0.1681, 0.3149, -.0310, 0.3047, 0.0762, 0.1007, 0.0132, 0.3758,
                    0.2296, 0.3336, 0.2858, 0.2104, -.091, 
                    -.1189, -.2210, .2869, 0.1088, 0.0491,
                    .1580, .0549, -.37, 0.2646, 0.1506,
                    0.0211, 0.16, 0.3239, 0.1369, 0.0138,
                    0.1196, 0.2183, -0.0438, 0.315, 0.184, 0.287),
  ret_buffett = c(0, 0.593, 0.846, -0.231, 0.356, 0.298, 0.389, 0.25, 0.574,
                  0.062, .349, 0.522, -.199, .266,
                  0.065, -0.038, 0.158, 0.043, 0.008,
                  0.241, 0.287, -.318, 0.027, 0.214, 
                  -0.047, 0.168, 0.327, 0.27, -0.125, 
                  0.234, 0.219, 0.028, 0.11, 0.024, 0.296)
)

df <- simons_ret %>%
        left_join(sp500_dfa)

fee_m_high <- 0.40
fee_perf_high <- 0

# Loop through the years
for(i in 1:nrow(df)){
  ret_sp500 <- df[i, "ret_sp500"]
  ret_buffett <- df[i, "ret_buffett"]
  ret_simons_gross <- df[i, "ret_gross"]
  ret_simons_net <- df[i, "ret_net"]
  
  # Replication
  fee_m <- df[i, "fee_m"]
  fee_perf <- df[i, "fee_perf"]
  
  if(ret_simons_gross - fee_m > 0){
    fee_perf <- fee_perf
  } else{
    fee_perf <- 0
  }
  
  # Calculate with our own high fees
  if(ret_simons_gross - fee_m_high - (fee_perf_high*ret_simons_gross)  > 0){
    ret_high_fee <- ret_simons_gross - fee_m_high - (fee_perf_high*ret_simons_gross)
  } else{
    ret_high_fee <- ret_simons_gross - fee_m_high 
  }
   
  
  if(i == 1){
    df[i, "value_simons_gross"] <- 1 
    df[i, "value_simons_net"] <- 1 
    df[i, "value_simons_high_fee"] <- 1 
    df[i, "value_sp500"] <- 1 
    df[i, "value_buffett"] <- 1
    df[i, "value_simons_personal"] <- 0 
  } else{
    df[i, "value_simons_gross"] <- df[(i-1), "value_simons_gross"] * (1 + ret_simons_gross)
    df[i, "value_simons_net"] <- df[(i-1), "value_simons_net"] * (1 + ret_simons_net)
    df[i, "value_simons_high_fee"] <- df[(i-1), "value_simons_high_fee"] * (1 + ret_high_fee) 
    df[i, "value_sp500"] <- df[(i-1), "value_sp500"] * (1 + ret_sp500)
    df[i, "value_buffett"] <- df[(i-1), "value_buffett"] * (1 + ret_buffett)
    df[i, "value_simons_personal"] <- (df[(i-1), "value_simons_personal"] * (1 + ret_simons_gross)) + (df[(i-1), "value_simons_net"] * ret_simons_gross * fee_perf) 
  }
}

years_to_plot <- c(end_year, 1999)

for(y in years_to_plot){
  
  if(y == 1999){
    file_string <- "short"
    y_axis_label <- "Growth of $1"
  } else{
    file_string <- ""
    y_axis_label <- "Growth of $1 (Log Scale)"
  }

  to_plot <- df %>%
              filter(year <= y) %>%
              select(year, value_simons_high_fee, value_sp500) %>%
              rename(`S&P 500` = value_sp500,
                     `Medallion Fund` = value_simons_high_fee) %>%
              gather(-year, key=key, value=value)
  
  source_string <- str_wrap(paste0("Source: DFA, Gregory Zuckerman (OfDollarsAndData.com)"), 
                            width = 85)
  note_string <-  str_wrap(paste0("Note:  Assumes the Medallion Fund charges a ", 100*fee_m_high, "% annual management fee and a ", 100*fee_perf_high, "% annual performance fee.  ",
                                  "Management fees are assessed on the beginning year balance and charged at year end."),
                           width = 80)
  
  file_path <- paste0(out_path, "/medallion_v_sp500_", file_string, "_", 100*fee_m_high, "_m_", 100*fee_perf_high, "_p_", end_year, ".jpeg")
  
  text_labels <- to_plot %>%
                  filter(year == max(to_plot$year)) 
  
  if(y == end_year){
    plot <- ggplot(data = to_plot, aes(x=year, y = value, col = key)) +
      geom_line() +
      geom_text_repel(data = text_labels,
                      aes(x = year,
                          y = value,
                          col = key,
                          label = paste0(key, "\n$", formatC(value, digits = 2, format = "f", big.mark = ",")),
                          family = "my_font"),
                      size = 3,
                      segment.colour = "transparent",
                      max.iter  = 1000,
                      nudge_x = ifelse(text_labels$key == "Medallion Fund", -4, 0),
                      nudge_y = ifelse(text_labels$key == "Medallion Fund", 0, -0.75)
      ) +
      scale_y_continuous(label = dollar, trans = log_trans(), breaks = c(0, 1, 10, 100, 1000)) +
      scale_color_manual(values = c("black", "blue"), guide = FALSE) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Medallion Fund with ", 100*fee_m_high, "% Fee vs. S&P 500\nGrowth of $1")) +
      labs(x = paste0("Year"), y = y_axis_label,
           caption = paste0(source_string, "\n", note_string))
  } else{
    plot <- ggplot(data = to_plot, aes(x=year, y = value, col = key)) +
      geom_line() +
      geom_text_repel(data = text_labels,
                      aes(x = year,
                          y = value,
                          col = key,
                          label = paste0(key, "\n$", formatC(value, digits = 2, format = "f", big.mark = ",")),
                          family = "my_font"),
                      size = 3,
                      segment.colour = "transparent",
                      max.iter  = 1000,
                      nudge_x = ifelse(text_labels$key == "Medallion Fund", 0, -1),
                      nudge_y = ifelse(text_labels$key == "Medallion Fund", 0.8, 0)
      ) +
      scale_y_continuous(label = dollar) +
      scale_color_manual(values = c("black", "blue"), guide = FALSE) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Medallion Fund with ", 100*fee_m_high, "% Fee vs. S&P 500\nGrowth of $1")) +
      labs(x = paste0("Year"), y = y_axis_label,
           caption = paste0(source_string, "\n", note_string))
  }
  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

# Plot bars too
to_plot <- df %>%
  filter(year >= 1988) %>%
  select(year, ret_net, ret_sp500) %>%
  rename(`S&P 500` = ret_sp500,
         `Medallion Fund` = ret_net) %>%
  gather(-year, key=key, value=value)

source_string <- str_wrap(paste0("Source: DFA, Gregory Zuckerman (OfDollarsAndData.com)"), 
                          width = 85)
note_string <-  str_wrap(paste0("Note:  Shows the total annual returns for the S&P 500 and the Medallion Fund (net of fees)."), 
                         width = 85)

file_path <- paste0(out_path, "/ret_sp500_medallion_1988_", end_year, ".jpeg")

text_labels <- data.frame()

text_labels[1, "year"] <- 2017
text_labels[1, "value"] <- 0.55
text_labels[1, "key"] <- "Medallion Fund"
text_labels[2, "year"] <- 2017
text_labels[2, "value"] <- -0.15
text_labels[2, "key"] <- "S&P 500"

plot <- ggplot(data = to_plot, aes(x=year, y = value, fill = key, col = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = text_labels, aes(x= year, y = value, col = key, label = key),
            family = "my_font") +
  scale_color_manual(values = c("black", "blue"), guide = FALSE) +
  scale_fill_manual(values = c("black", "blue"), guide = FALSE) +
  scale_y_continuous(label = percent, limits = c(-0.4, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Annual Returns for the S&P 500 vs.\nThe Medallion Fund (Net of Fees)\n1988-", end_year)) +
  labs(x = paste0("Year"), y = paste0("Annual Return"),
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #