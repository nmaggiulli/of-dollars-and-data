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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(tidyverse)

folder_name <- "0344_rich_vs_wealthy"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2019

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

df <- scf_stack %>%
      select(hh_id, imp_id, age, agecl,
             networth,
             income, wgt)

n_hh <- length(unique(df$hh_id))

plot_var <- function(var, title_var){
  
  to_plot <- df %>%
                rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                summarise(`Top 10%` = wtd.quantile(var_for_qtile, weights = wgt, probs = 0.9),
                          `Top 5%` = wtd.quantile(var_for_qtile, weights = wgt, probs = 0.95),
                          `Top 1%` = wtd.quantile(var_for_qtile, weights = wgt, probs = 0.99)) %>%
                gather(key=key, value = value)
  
  to_plot$key <- factor(to_plot$key, levels = c("Top 10%", "Top 5%", "Top 1%"))
  
  file_path <- paste0(out_path, "/rich_v_wealthy_", var, "_", data_year, "_scf.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                           , width = 85)
  
  text_labels <- to_plot %>%
    mutate(label = case_when(
      value > 10^6 ~  paste0("$", formatC(round(value/1000000, 2), big.mark=",", format="f", digits=1), "M"),
      value > 1000 ~  paste0("$", formatC(round(value/1000, 0), big.mark=",", format="f", digits=0), "k"),
      TRUE ~ paste0("$", value)
    ))
  
  
  plot <- ggplot(to_plot, aes(x=key, y=value)) +
    geom_bar(stat = "identity", fill = chart_standard_color) +
    geom_text(data = text_labels, aes(x=key, y=value, label = label),
              col = chart_standard_color,
              size = 3,
              vjust= -0.5) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0("U.S. Household ", title_var, " for the\nTop 10%, Top 5%, and Top 1%")) +
    labs(x = "Percentile" , y = title_var,
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

plot_var("networth", "Net Worth")
plot_var("income", "Income")

# ############################  End  ################################## #