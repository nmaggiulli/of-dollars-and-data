cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

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

########################## Start Program Here ######################### #

# Load data fom local library
bls_cx <- readRDS(paste0(localdir, "07-bls-cx.Rds"))

read_in <- function(string){
  temp <- readRDS(paste0(localdir, "07-bls-cx-", string, ".Rds"))
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
                            mutate(share = value / income) %>%
                            filter(item_name %in% item_list)


loop_list <- unique(select(bls_cx_expenditures,characteristics_name))

for (i in 1:nrow(loop_list)){
  to_plot     <- filter(bls_cx_expenditures, characteristics_name == loop_list[i, 1])
  last_year   <- max(to_plot$year)
  first_year  <- min(to_plot$year)
  last        <- filter(to_plot, year == last_year) %>%
                    arrange(desc(item_name))
  last$cumsum <- cumsum(last$share)
  
  # Set the file_path 
  file_path = paste0(exportdir, "07-bls-consumer-expenditures/", loop_list[i, 1], ".jpeg")
  
  if(loop_list[i,1] == "Lowest 20 percent income quintile"){
    top_title <- "The Lowest 20 Percent of Income\nSpend More Than They Earn\non Basic Necessities"
  } else if (loop_list[i,1] == "Second 20 percent income quintile"){
    top_title <- "The Next 20 Percent is Doing Better,\n But Not Much"
  } else if (loop_list[i,1] == "Highest 20 percent income quintile"){
    top_title <- "The Top 20 Percent Spends Much Less\nOn Basic Necessities"
  } else{
    top_title <- paste0("Key Expenditures as a Percentage of\n After-Tax Income\n", loop_list[i,1])
  }
  
  # Plot the time trends
  if (loop_list[i,1] == "Highest 20 percent income quintile"){
    plot <- ggplot(to_plot, aes(x = year, y = share, col = item_name, fill = item_name))  +
      geom_area() +
      geom_text_repel(data = last,
                      aes(year, 
                          cumsum, 
                          label = item_name,
                          family = "my_font"), 
                      size = 3,
                      nudge_y = -0.05,
                      nudge_x = -3.1,
                      col = "black",
                      segment.alpha = .5,
                      max.iter = 5000) +
      geom_hline(yintercept = 1, color = "black", linetype="dashed") +
      scale_color_discrete(guide = FALSE) +
      scale_fill_discrete(guide = FALSE) +
      scale_y_continuous(label = percent, limits = c(0, 2.75), breaks = seq(0, 2.75, 0.25)) +
      scale_x_continuous(breaks = seq(first_year, last_year, 5)) +
      ggtitle(top_title)  +
      of_dollars_and_data_theme +
      labs(x = "Year" , y = "Share of After-Tax Income")
  } else{
    plot <- ggplot(to_plot, aes(x = year, y = share, col = item_name, fill = item_name))  +
      geom_area() +
      geom_text_repel(data = last,
                      aes(year, 
                          cumsum, 
                          label = item_name,
                          family = "my_font"), 
                      size = 3,
                      nudge_y = -0.09,
                      nudge_x = -3.1,
                      col = "black",
                      segment.alpha = 0,
                      max.iter = 5000) +
      geom_hline(yintercept = 1, color = "black", linetype="dashed") +
      scale_color_discrete(guide = FALSE) +
      scale_fill_discrete(guide = FALSE) +
      scale_y_continuous(label = percent, limits = c(0, 2.75), breaks = seq(0, 2.75, 0.25)) +
      scale_x_continuous(breaks = seq(first_year, last_year, 5)) +
      ggtitle(top_title)  +
      of_dollars_and_data_theme +
      labs(x = "Year" , y = "Share of After-Tax Income")
  }
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Bureau of Labor Statistics, Consumer Expenditures (OfDollarsAndData.com)"
  note_string   <- "Note:  Excludes expenses related to education, entertainment, apparel, and other services." 
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}


########################### Now plot Income minus Total Average Expenses ######################
  to_plot <- bls_cx_income %>%
                    left_join(bls_cx_tot_avg_exp) %>%
                    mutate(inc_minus_exp = income - year_avg_exp)

  last_year <- max(to_plot$year)
  
  # Set the file_path 
  file_path = paste0(exportdir, "07-bls-consumer-expenditures/income-minus-expenses.jpeg")
  
  # Plot the time trends
  plot <- ggplot(to_plot, aes(x = year, y = inc_minus_exp, col = characteristics_name))  +
    geom_line() +
    geom_hline(yintercept = 0, col = "black") +
    geom_text_repel(data = filter(to_plot, year == last_year, characteristics_name %in% c(
                    "Highest 20 percent income quintile",
                    "Lowest 20 percent income quintile")
                    ),
                    aes(year, 
                        inc_minus_exp, 
                        label = characteristics_name, 
                        family = "my_font"), 
                    nudge_y = -4000,
                    segment.colour = "white",
                    size = 3) +
    scale_color_discrete(guide = FALSE) +
    scale_y_continuous(label = dollar, breaks = seq(-20000, 60000, 10000), limits=c(-20000, 60000)) +
    ggtitle("40% of U.S. Households Spent\nMore Than They Earned in 2015")  +
    of_dollars_and_data_theme +
    labs(x = "Year" , y = "After-Tax Income Minus Expenses")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Bureau of Labor Statistics, Consumer Expenditures (OfDollarsAndData.com)"
  note_string   <- "Note:  Income includes government assisstance.  Expenditures are average annual expenditures." 
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

  

# ############################  End  ################################## #