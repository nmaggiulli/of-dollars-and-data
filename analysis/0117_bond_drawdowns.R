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

folder_name <- "0117_bond_drawdowns"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

dfa_data <- read_excel(paste0(importdir, "0113_life_cycle_simulations/dfa_tbill_sp500_5yr.xlsx"), skip = 4) %>%
  filter(!is.na(Date)) %>%
  mutate(Date = as.Date(Date))

colnames(dfa_data) <- c("date", "cpi", "ret_tbill", "ret_sp500", "ret_treasury_5yr")

dfa_data <- dfa_data %>%
              select(date, ret_tbill, ret_sp500)

for(i in 1:nrow(dfa_data)){
  if(i == 1){
    dfa_data[i, "index_tbill"] <- 100 * (1 + dfa_data[i, "ret_tbill"])
    dfa_data[i, "index_sp500"]   <- 100 * (1 + dfa_data[i, "ret_sp500"])
  } else{
    dfa_data[i, "index_tbill"] <- dfa_data[(i-1), "index_tbill"] * (1 + dfa_data[i, "ret_tbill"])
    dfa_data[i, "index_sp500"]   <- dfa_data[(i-1), "index_sp500"] * (1 + dfa_data[i, "ret_sp500"])
  }
}

dd_tbill <- drawdown_path(select(dfa_data, date, index_tbill)) %>%
              mutate(key = "U.S. T-Bills")
dd_sp500 <- drawdown_path(select(dfa_data, date, index_sp500)) %>%
              mutate(key = "U.S. Stocks")

to_plot <- dd_tbill %>%
            bind_rows(dd_sp500)

file_path <- paste0(out_path, "/dd_bonds_bills.jpeg")

# Set source/note
source_string <- paste0("Source:  DFA, 1926-2018 (OfDollarsAndData.com)")
note_string   <- str_wrap(paste0("Note:  Returns are adjusted for inflation."), width = 85)

plot <- ggplot(to_plot, aes(x = date, y = pct, fill = key)) +
  geom_area(position = "identity", alpha = 0.3) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  ggtitle("Real Drawdowns for U.S. Stocks and T-Bills") +
  labs(x = "Date" , y = "Percentage of Value Lost",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #