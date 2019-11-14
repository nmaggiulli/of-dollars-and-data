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

# From Zuckerman's book
simons_ret <- data.frame(
                  year = seq(1987, 2018),
                  ret_net = c(0, 0.09, -0.04, 0.55, 0.394, 0.336, 0.391, 0.707, 0.383,
                              0.315, 0.212, 0.417, 0.245, 0.985,
                              0.33, 0.258, 0.219, 0.249, 0.295,
                              0.443, 0.737, 0.824, 0.39, 0.294,
                              0.37, 0.29, 0.469, 0.392, 0.36,
                              0.356, 0.45, 0.40),
                  fee_m = rep(0.05, 32),
                  fee_perf = c(0, rep(0.2, 13), 0.36, rep(0.44, 17)),
                  ret_gross = c(0, 0.163, 0.01, 0.778, 0.543, 0.47, 0.539, 0.934, 0.529,
                                0.444, 0.315, 0.571, 0.356, 1.281,
                                0.566, 0.511, 0.441, 0.495, 0.577, 
                                0.841, 1.366, 1.521, 0.746, 0.575,
                                0.711, 0.568, 0.888, 0.75, 0.693,
                                0.686, 0.854, 0.764)
)

sp500_dfa <- data.frame(
  year = seq(1987, 2018),
  ret_sp500_dfa = c(0, 0.1681, 0.3149, -.0310, 0.3047, 0.0762, 0.1007, 0.0132, 0.3758,
                    0.2296, 0.3336, 0.2858, 0.2104, -.091, 
                    -.1189, -.2210, .2869, 0.1088, 0.0491,
                    .1580, .0549, -.37, 0.2646, 0.1506,
                    0.0211, 0.16, 0.3239, 0.1369, 0.0138,
                    0.1196, 0.2183, -0.0438)
)

df <- simons_ret %>%
        left_join(sp500_dfa)

fee_m_high <- 0.40
fee_perf_high <- 0

# Loop through the years
for(i in 1:nrow(df)){
  ret_sp500 <- df[i, "ret_sp500_dfa"]
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
    df[i, "value_simons_personal"] <- 0 
  } else{
    df[i, "value_simons_gross"] <- df[(i-1), "value_simons_gross"] * (1 + ret_simons_gross)
    df[i, "value_simons_net"] <- df[(i-1), "value_simons_net"] * (1 + ret_simons_net)
    df[i, "value_simons_high_fee"] <- df[(i-1), "value_simons_high_fee"] * (1 + ret_high_fee) 
    df[i, "value_sp500"] <- df[(i-1), "value_sp500"] * (1 + ret_sp500)
    df[i, "value_simons_personal"] <- (df[(i-1), "value_simons_personal"] * (1 + ret_simons_gross)) + (df[(i-1), "value_simons_net"] * ret_simons_gross * fee_perf) 
  }
}

to_plot <- df %>%
            select(year, value_simons_high_fee, value_sp500) %>%
            rename(`S&P 500` = value_sp500,
                   `Medallion Fund` = value_simons_high_fee) %>%
            gather(-year, key=key, value=value)

source_string <- str_wrap(paste0("Source: DFA, Gregory Zuckerman (OfDollarsAndData.com)"), 
                          width = 85)
note_string <-  str_wrap(paste0("Note:  Assumes the Medallion Fund charges a ", 100*fee_m_high, "% annual management fee and a ", 100*fee_perf_high, "% annual performance fee."),
                         width = 85)

file_path <- paste0(out_path, "/mf_v_sp500_", 100*fee_m_high, "_m_", 100*fee_perf_high, "_p.jpeg")

plot <- ggplot(data = to_plot, aes(x=year, y = value, col = key)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("Medallion Fund with ", 100*fee_m_high, "% Fee vs. S&P 500\nGrowth of $1")) +
  labs(x = paste0("Year"), y = paste0("Growth of $1"),
       caption = paste0(source_string, "\n", note_string))

ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #