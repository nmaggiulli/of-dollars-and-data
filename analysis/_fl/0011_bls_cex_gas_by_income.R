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

folder_name <- "_fl/0011_bls_cex_gas_by_income"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Load data fom local library
bls_cx <- readRDS(paste0(localdir, "0008_bls_cx.Rds"))

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
item_list <- c("Gasoline, other fuels, and motor oil") 
                             
bls_cx_expenditures <-  bls_cx_expenditures %>%
                            left_join(bls_cx_tot_avg_exp) %>% 
                            left_join(bls_cx_income)      %>%
                            mutate(share = value / income) %>%
                            filter(item_name %in% item_list)
                            
to_plot <- bls_cx_expenditures %>%
              select(year, characteristics_name, value, income, share) %>%
              filter(year > 1990) %>%
              mutate(inc_bracket = str_replace(characteristics_name, " income quintile", "")) %>%
              select(-characteristics_name)

export_to_excel(df = to_plot, 
                outfile = paste0(out_path, "/bls_cex_gas_by_income.xlsx"),
                sheetname = "bls_cex",
                new_file = 1,
                fancy_formatting = 0)
              
to_plot$inc_bracket <- factor(to_plot$inc_bracket, 
                              levels = c("Lowest 20 percent", "Second 20 percent",
                                                              "Third 20 percent", "Fourth 20 percent", 
                                                              "Highest 20 percent"))



file_path <- paste0(out_path, "/bls_cex_gas_share_by_income.jpeg")
source_string <- paste0("Source: BLS")

plot <- ggplot(data = to_plot, aes(x=year, y=share, col = inc_bracket)) +
  geom_line() +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  guides(col = guide_legend(nrow = 2)) +
  ggtitle(paste0("Gas Share of Income by Income Level")) +
  labs(x = "Year", y = "Gas as a % of Income",
       caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")              

# ############################  End  ################################## #