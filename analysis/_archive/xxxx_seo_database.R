cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(zoo)
library(ggrepel)
library(tidyverse)

folder_name <- "xxxx_seo_database"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

fp <- paste0(importdir, "_ahrefs_seo/_comps/")

file_list <- data.frame(file_name = list.files(path = fp))

import_ahrefs <- function(path, f_name){
  tmp <- read.csv(paste0(path, f_name), sep = "\t", fileEncoding = "UCS-2LE") %>%
          clean_cols() %>%
          rename(traffic = current_traffic,
                 top_keyword = current_top_keyword,
                volume = current_top_keyword__volume,
                 position = current_top_keyword__position) %>%
          select(url, traffic, top_keyword, volume, position) %>%
          mutate(site = gsub("([a-z]+)\\.com.*", "\\1", f_name)) %>%
          filter(traffic > 1000 & volume > 2000)

  return(tmp)
}

for(i in 1:nrow(file_list)){
  f <- file_list[i, "file_name"]
  
  tmp2 <- import_ahrefs(fp, f)
  
  if(i == 1){
    stack <- tmp2
  } else{
    stack <- stack %>% bind_rows(tmp2)
  }
}

stack <- stack %>%
            arrange(-traffic)

export_to_excel(df = stack,
                outfile = paste0(out_path, "/keywords_to_target.xlsx"),
                sheetname = "comp_data",
                new_file = 1,
                fancy_formatting = 0)


# ############################  End  ################################## #