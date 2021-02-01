cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

folder_name <- "_jkb/0003_bls_cx_area_charts"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load data fom local library
bls_cx <- readRDS(paste0(localdir, "0008_bls_cx.Rds"))

bw_colors <- c("#cccccc", "#969696", "#525252", "#252525")

read_in <- function(string){
  temp <- readRDS(paste0(localdir, "0008_bls_cx_", string, ".Rds"))
  return(temp)
}

names <- c("item", "demographics", "characteristics", "subcategory", "category")

# Loop through datasets to read in
for (i in names){
  tmpname <- paste0(i)
  df      <- read_in(i)
  assign(tmpname, df, envir = .GlobalEnv)
  rm(df)
  rm(tmpname)
}

max_year <- max(bls_cx$year)

inc_characteristics_list <- c("Lowest 20 percent income quintile",
                              "Second 20 percent income quintile",
                              "Third 20 percent income quintile", 
                              "Fourth 20 percent income quintile",
                              "Highest 20 percent income quintile")

# Filter main cx data
bls_cx_expenditures <- filter(bls_cx, category_name == "Expenditures",
                              demographics_name == "Quintiles of income before taxes",
                              characteristics_name %in% inc_characteristics_list
                              )

bls_cx_tot_avg_exp <- filter(bls_cx_expenditures,
                             item_name == "Total average annual expenditures") %>%
                      mutate(year_avg_exp = value) %>%
                      select(year, year_avg_exp, characteristics_name)

bls_cx_income <- filter(bls_cx, subcategory_name == "Income after taxes",
                        demographics_name == "Quintiles of income before taxes",
                        characteristics_name %in% inc_characteristics_list
                        ) %>%
                        mutate(income = value) %>%
                        select(year, demographics_name, characteristics_name, income)

current_inc <- filter(bls_cx_income, year == max_year)
current_exp <- filter(bls_cx_tot_avg_exp, year == max_year)
 
#Add an item list
item_list <- c("Food", "Housing", "Transportation", "Healthcare") 
                             
bls_cx_expenditures <-  bls_cx_expenditures %>%
                            left_join(bls_cx_tot_avg_exp) %>% 
                            left_join(bls_cx_income)      %>%
                            mutate(share = value / income,
                                   monthly_spend = value/12) %>%
                            filter(item_name %in% item_list)

cx_2019 <- bls_cx_expenditures %>%
            filter(year == 2019) %>%
            select(year, characteristics_name, item_name, monthly_spend)

loop_list <- select(bls_cx_expenditures,characteristics_name) %>%
              distinct() %>%
              filter(!grepl("Third|Fourth", characteristics_name))

for (i in 1:nrow(loop_list)){
  to_plot     <- filter(bls_cx_expenditures, characteristics_name == loop_list[i, 1])
  last_year   <- max(to_plot$year)
  first_year  <- min(to_plot$year)
  last        <- filter(to_plot, year == last_year) %>%
                    arrange(desc(item_name))
  last$cumsum <- cumsum(last$share)
  
  # Set the file_path 
  file_path = paste0(out_path, "/", loop_list[i, 1], "_", last_year, ".jpeg")
  
  if(loop_list[i,1] == "Lowest 20 percent income quintile"){
    top_title <- "The Lowest 20 Percent of Earners\nSpend More Than They Earn\non Basic Necessities"
  } else if (loop_list[i,1] == "Second 20 percent income quintile"){
    top_title <- "The Next 20 Percent is Doing Better,\n But Not Much"
  } else if (loop_list[i,1] == "Highest 20 percent income quintile"){
    top_title <- "The Top 20 Percent Spends Much Less\nOn Basic Necessities"
  }
  
  # Plot the time trends
  plot <- ggplot(to_plot, aes(x = year, y = share, fill = item_name))  +
    geom_area() +
    geom_hline(yintercept = 1, color = "black", linetype="dashed") +
    scale_fill_manual(values = bw_colors) +
    scale_y_continuous(label = percent, limits = c(0, 2.75), breaks = seq(0, 2.75, 0.25)) +
    scale_x_continuous(breaks = seq(first_year, last_year, 5)) +
    ggtitle(top_title)  +
    of_dollars_and_data_theme +
    theme(legend.position = "bottom",
          legend.title = element_blank()) +
    labs(x = "Year" , y = "Share of After-Tax Income")
  
  # Save the plot  
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}


  

# ############################  End  ################################## #