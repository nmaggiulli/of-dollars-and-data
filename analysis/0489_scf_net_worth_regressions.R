cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(survey)
library(lemon)
library(mitools)
library(Hmisc)
library(xtable)
library(tidyverse)

folder_name <- "0489_scf_net_worth_regressions"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

df <- scf_stack %>%
      filter(year == 2022) %>%
    select(year, hh_id, imp_id, 
             agecl, edcl, race, married, kids,
             networth,
             wgt)

df$race <- factor(df$race, levels = c("White", "Black", "Hispanic", "Other"))
df$edcl <- factor(df$edcl, levels = c("High School", "No High School", "Some College", "College Degree"))

# 1. Create the copy number again (if you haven't already)
df$copy_number <- df$imp_id %% 10

# 2. Split the big dataframe into a list of 5 dataframes
scf_list <- split(df, df$copy_number)
scf_implicates <- imputationList(scf_list)

# 3. Define the survey design (telling R about the weights)
# We use the list of dataframes we just created
scf_design <- svydesign(ids = ~1, 
                        weights = ~wgt, 
                        data = scf_implicates)

# 4. Run the regression across all 5 copies automatically
# We use the Inverse Hyperbolic Sine (IHS) to handle debt/negatives in net worth
ihs <- function(x) { log(x + sqrt(x^2 + 1)) }

model_results <- with(scf_design, svyglm(ihs(networth) ~ agecl + edcl + race + married + kids, design = scf_design))

# 5. Combine the results using Rubin's Rules
final_pooled_results <- MIcombine(model_results)

# 1. Get the base summary data (Coefficients and Standard Errors)
summary_data <- summary(final_pooled_results)

# 2. Calculate P-values manually (using normal approximation)
# We do this BEFORE any exponentiation
t_stats <- summary_data$results / summary_data$se
p_values <- 2 * (1 - pnorm(abs(t_stats)))

# 3. Calculate Multipliers and CIs (Exponentiated)
# We use the raw coefficients from the model
multipliers <- exp(coef(final_pooled_results))
conf_ints_multipliers <- exp(confint(final_pooled_results))

# 4. Combine into one Master Table
final_table <- cbind(
  Multiplier = multipliers, 
  conf_ints_multipliers, 
  P_Value = p_values
)

# 5. Clean up the formatting
# Round Multipliers to 2 decimal places
# Round P-values to 4 decimal places (so you can see small numbers)
final_table <- round(final_table, 4) 

# (Optional) Make Multipliers easier to read by overwriting them with 2 decimals
final_table[, 1:3] <- round(final_table[, 1:3], 2)

# 6. View final result
print(final_table)

# ############################  End  ################################## #