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
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(tidyverse)

folder_name <- "0165_liquid_net_worth"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

data_year <- 2016

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == data_year)

liquid_net_worth <- scf_stack %>%
                      mutate(liquid_net_worth = fin - reteq - ccbal) %>%
                      select(hh_id, imp_id, 
                             networth, debt, fin, reteq, homeeq, ccbal, liquid_net_worth, wgt, 
                             agecl, edcl) %>%
                      arrange(hh_id, imp_id)

n_hh <- length(unique(liquid_net_worth$hh_id))

create_median_chart <- function(var, var_title){

  to_plot <- liquid_net_worth %>%
                    rename_(.dots = setNames(paste0(var), "var_for_qtile")) %>%
                    group_by(agecl, edcl) %>%
                    summarize(
                      `25th Percentile` = wtd.quantile(var_for_qtile, weights = wgt, probs=0.25),
                      `50th Percentile` = wtd.quantile(var_for_qtile, weights = wgt, probs=0.5),
                      `75th Percentile` = wtd.quantile(var_for_qtile, weights = wgt, probs=0.75)) %>%
                    ungroup() %>%
                  gather(-agecl, -edcl, key=key, value=value) %>%
                  filter(key == "50th Percentile")
  
  file_path <- paste0(out_path, "/", var, "_age_educ.jpeg")
  source_string <- paste0("Source:  Survey of Consumer Finances, ", data_year, " (OfDollarsAndData.com)")
  note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                  formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                  " U.S. households.")
                                  , width = 85)
  
  plot <- ggplot(to_plot, aes(x=agecl, y=value, fill = key)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = c("left", "bottom")) +
    scale_fill_discrete(guide = FALSE) +
    scale_y_continuous(label = dollar) +
    of_dollars_and_data_theme +
    ggtitle(paste0(var_title, "\nby Age & Education Level")) +
    labs(x="Age", y=paste0(var_title),
         caption = paste0(source_string, "\n", note_string))
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

create_median_chart("networth", "Net Worth")
create_median_chart("liquid_net_worth", "Liquid Net Worth")
create_median_chart("ccbal", "Credit Card Balance")
create_median_chart("reteq", "Retirement Account Balance")
create_median_chart("homeeq", "Home Equity")
create_median_chart("fin", "Total Financial Assets")

# ############################  End  ################################## #