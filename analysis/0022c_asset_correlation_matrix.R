cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(Hmisc)

########################## Start Program Here ######################### #

# Read in BV data
bv <- readRDS(paste0(localdir, "0006_bv_returns.Rds"))

# Create years_list for looping
years_list <- seq(as.numeric(substr(min(unique(bv$year)), 1, 4)) + 4,
                  as.numeric(substr(max(unique(bv$year)), 1, 4)),
                  1)


create_corr_matrix <- function(y){

  bv_subset <- filter(bv, 
                            year > as.POSIXct(paste0(y - 5, "-12-31"), format = "%Y-%m-%d"),
                            year < as.POSIXct(paste0(y, "-12-31"), format = "%Y-%m-%d")) %>%
                select(-year)
  
  res <- cor(bv_subset)
  
  t <- paste0("\nCorrelations From ", y - 4, "-", y)
  corrplot(res, type = "upper", order = "alphabet",
           tl.col = "black", tl.srt = 45,
           title = t)
}

for (y in years_list){
  # Create Jpegs
  jpeg(paste0(exportdir, "0022c_corr_matrix/corr-",y, ".jpeg"))
  create_corr_matrix(y)
  dev.off()
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 120 loop -0 *.jpeg all_corr_matrix.gif
#
# 


# ############################  End  ################################## #