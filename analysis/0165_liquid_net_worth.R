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

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds")) %>%
              filter(year == 2016)

liquid_net_worth <- scf_stack %>%
                      mutate(liquid_net_worth = fin - reteq - ccbal) %>%
                      select(hh_id, imp_id, 
                             networth, debt, fin, reteq, ccbal, liquid_net_worth, wgt, 
                             agecl, edcl) %>%
                      arrange(hh_id, imp_id)

n_hh <- length(unique(liquid_net_worth$hh_id))

to_plot <- liquid_net_worth %>%
                  group_by(agecl, edcl) %>%
                  summarize(median_lnw =  wtd.quantile(liquid_net_worth, weights = wgt, probs=0.5))

file_path <- paste0(out_path, "/liquid_net_worth_age_educ.jpeg")
source_string <- "Source:  Survey of Consumer Finances (OfDollarsAndData.com)"
note_string <-  str_wrap(paste0("Note:  Percentiles are calculated using data based on ", 
                                formatC(n_hh, digits = 0, format = "f", big.mark = ","), 
                                " U.S. households.")
                                , width = 85)

plot <- ggplot(to_plot, aes(x=agecl, y=median_lnw)) +
  geom_bar(stat = "identity", fill = chart_standard_color) +
  facet_rep_wrap(edcl ~ ., scales = "free_y", repeat.tick.labels = "bottom") +
  scale_y_continuous(label = dollar) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Liquid Net Worth by Age & Education Level")) +
  labs(x="Age", y=paste0("Liquid Net Worth"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #