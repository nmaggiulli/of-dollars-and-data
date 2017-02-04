cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(stringr)

########################## Start Program Here ######################### #

# Load in raw BLS productivity data
bls_cx <-readRDS(paste0(importdir, "08-bls-consumer-expenditures/bls_cx_data.1.AllData.Rds"))

# Load in other datasets and create a code based on their row number
# Will use these datasets to merge to the main productivity dataset
create_index <- function(string){
  name            <- deparse(substitute(string))
  temp            <- readRDS(paste0(importdir, "08-bls-consumer-expenditures/bls_cx_", name, ".Rds"))
  if (name %in% c("category", "subcategory")){
    new_col         <- paste0(name, "_name")
    old_col         <- paste0(name, "_text")
    code            <- paste0(name, "_code")
    temp[, new_col] <- temp[, old_col]
    if(name %in% (c("subcategory"))){
    temp            <- temp[, c(code, new_col, "category_code")]
    } else{
    temp            <- temp[, c(code, new_col)]
    }
  } else if (name %in% (c("item", "demographics", "characteristics"))){
    new_col         <- paste0(name, "_name")
    old_col         <- paste0(name, "_code")
    temp[, new_col] <- temp[, old_col]
    if (name %in% (c("characteristics"))){
      temp[, old_col] <- temp[, "demographics_code"]
      temp[, "demographics_code"] <- temp[, "row.names"]
      temp            <- temp[, c(old_col, new_col, "demographics_code")]
    } else if (name %in% (c("item"))){
      temp[, old_col] <- temp[, "subcategory_code"]
      temp[, "subcategory_code"] <- temp[, "row.names"]
      temp            <- temp[, c(old_col, new_col, "subcategory_code")]
    } else {
      temp[, old_col] <- temp[, "row.names"]
      temp            <- temp[, c(old_col, new_col)]
    }
  }
  return(temp)
}

item             <- create_index(item)
demographics     <- create_index(demographics)
characteristics  <- create_index(characteristics)
subcategory      <- create_index(subcategory)
category         <- create_index(category)

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
            select(series_id, year, value, footnote_codes, item_name,
                   demographics_name, characteristics_name,
                   subcategory_name, category_name, subcategory_code,
                   item_code, characteristics_code, demographics_code)

# Save down final build before doing analysis
saveRDS(bls_cx, paste0(localdir, "08-bls-cx.Rds"))

# Save down other built datasets for reference
saveRDS(item,            paste0(localdir, "08-bls-cx-item.Rds"))
saveRDS(demographics,    paste0(localdir, "08-bls-cx-demographics.Rds"))
saveRDS(characteristics, paste0(localdir, "08-bls-cx-characteristics.Rds"))
saveRDS(subcategory,     paste0(localdir, "08-bls-cx-subcategory.Rds"))
saveRDS(category,        paste0(localdir, "08-bls-cx-category.Rds"))


# ############################  End  ################################## #