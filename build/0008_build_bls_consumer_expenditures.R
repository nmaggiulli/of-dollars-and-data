cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(stringr)

########################## Start Program Here ######################### #

# Load in raw BLS productivity data
bls_cx <-readRDS(paste0(importdir, "0008_bls_consumer_expenditures/bls_cx_data.1.AllData.Rds"))

# Load in other datasets and create a code based on their row number
# Will use these datasets to merge to the main productivity dataset
read_file <- function(string){
  name            <- deparse(substitute(string))
  temp            <- readRDS(paste0(importdir, "0008_bls_consumer_expenditures/bls_cx_", name, ".Rds")) %>%
                        select(-display_level, -selectable, -sort_sequence)
}

item             <- read_file(item) 
demographics     <- read_file(demographics)
characteristics  <- read_file(characteristics)
subcategory      <- read_file(subcategory)
category         <- read_file(category)

# Parse the series ID based on the "cx.txt" file here:  https://download.bls.gov/pub/time.series/cx/
bls_cx <- mutate(bls_cx,
                 lb_pos               = regexpr('LB', series_id),
                 item_code            = substr(series_id, 4, lb_pos - 1),
                 demographics_code    = substr(series_id, lb_pos, lb_pos + 3),
                 characteristics_code = substr(series_id, lb_pos + 4, lb_pos + 5))

# Merge on the item, demographics, characteristics, subcategory and category
bls_cx <- bls_cx                     %>%
          left_join(item)            %>%
          left_join(demographics)    %>%
          left_join(characteristics) %>%
          left_join(subcategory)     %>%
          left_join(category)        %>%
            select(series_id, year, value, footnote_codes, item_text,
                   demographics_text, characteristics_text,
                   subcategory_text, category_text, subcategory_code,
                   item_code, characteristics_code, demographics_code) %>%
            rename(demographics_name = demographics_text,
                   item_name = item_text,
                   characteristics_name = characteristics_text,
                   subcategory_name = subcategory_text,
                   category_name = category_text)

# Save down final build before doing analysis
saveRDS(bls_cx, paste0(localdir, "0008_bls_cx.Rds"))

# Save down other built datasets for reference
saveRDS(item,            paste0(localdir, "0008_bls_cx_item.Rds"))
saveRDS(demographics,    paste0(localdir, "0008_bls_cx_demographics.Rds"))
saveRDS(characteristics, paste0(localdir, "0008_bls_cx_characteristics.Rds"))
saveRDS(subcategory,     paste0(localdir, "0008_bls_cx_subcategory.Rds"))
saveRDS(category,        paste0(localdir, "0008_bls_cx_category.Rds"))


# ############################  End  ################################## #