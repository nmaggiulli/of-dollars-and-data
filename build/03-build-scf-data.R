cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

# Use the LibreBaskerville font
windowsFonts(my_font=windowsFont("Libre Baskerville"))

########################## Load in Libraries ########################## #

library(dplyr)

########################## Start Program Here ######################### #

# Create a year list to loop through
year_list <- c(1989, 1992, 1995, 1998, 2001, 2004, 2007, 2010, 2013)

for (x in year_list){
  # Load SCF data into memory
  load(paste0(importdir,"03-scf-data/scf", x, ".rda"))
  
  # Subset to only the variables we care about in the data
  # Definitions are from here:
  #
  # https://www.federalreserve.gov/econresdata/scf/files/bulletin.macro.txt
  # 
  # networth = total networth (asset - debt)
  # asset = value of all assets
  # debt = value of all debt
  # homeeq = value of home equity
  # hdebt = dummy, 1 if has debt, 0 if no debt
  # fin = total finanical assets
  # resdbt = residential debt
  # agecl = age class, 1:<35, 2:35-44, 3:45-54, 4:55-64, 5:65-74, 6:>=75
  # hhsex = gender, 1 = male , 2 = female
  # race = race, 1 = white non-Hispanic, 2 = nonwhite or Hispanic
  # edcl = education class, 1 = no high school diploma/GED, 2 = high school diploma or GED,
  #   3 = some college, 4 = college degree
  # married = marital status, 1 = married/living with partner, 2 = neither married nor living with partner
  # kids = number of kids
  # savres1-9 = reason for saving, 1 = cant save, 2 = education, 3 = family, 4 = home, 5 = purchases, 
  #   6 = retirement, 7 = liquidity/the future, 8 = investment, 9 = no particular reason
  
  vars_to_keep <- c('networth', 'debt', 'asset', 
                    'homeeq','hdebt', 'fin', 'income', 'resdbt',
                    'agecl', 'hhsex', 'race', 'edcl', 'married', 'kids', 
                    'savres1', 'savres2', 'savres3', 'savres4', 'savres5',
                    'savres6', 'savres7', 'savres8', 'savres9')
  
  # Write a function to subset our data
  subset_data <- function(name){
    data <- get(name)
    data <- data[ , vars_to_keep]
    data["year"] <- x
    assign(name, data, envir = .GlobalEnv)
  }
  
  # Loop through each of the imp datasets to subset them to relevant variables
  for (i in 1:5){
    string <- paste0("imp", i)
    subset_data(string)
  }
  
  # Stack the datasets
  if (x == 1989){
    scf_stack <- rbind(imp1, imp2, imp3, imp4, imp5)
  } else{
    scf_stack <- rbind(scf_stack, imp1, imp2, imp3, imp4, imp5)
  }
}

# Clean some additional variables
scf_stack <- mutate(scf_stack, married = married %% 2,
                    white = race %% 2,
                    male = hhsex %% 2) %>%
              select(-hhsex, - race)

# Save down data to permanent file
saveRDS(scf_stack, paste0(localdir, "03-scf-stack.Rds"))

# ############################  End  ################################## #