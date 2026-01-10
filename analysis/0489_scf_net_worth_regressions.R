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
    mutate(businessowner = ifelse(bus > 0, 1, 0)) %>%
    select(year, hh_id, imp_id, 
             agecl, edcl, race, married, kids, businessowner, inheritance, income,
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

# 4. Create a function for the regression
ihs <- function(x) { log(x + sqrt(x^2 + 1)) }

# 5. Define your variables
# Ensure your column names match EXACTLY what is in your dataframe
vars <- c("agecl", "edcl", "race", "married", "kids", "businessowner", "inheritance")

# 6. Generate all possible formula combinations
# This creates a list of strings like "ihs(networth) ~ agecl + race"
formulas <- unlist(lapply(1:length(vars), function(n) {
  combos <- combn(vars, n, simplify = FALSE)
  lapply(combos, function(c) {
    paste("ihs(networth) ~", paste(c, collapse = " + "))
  })
}))

# 7. Create a function to run one model and extract metrics
# We return a simple dataframe row with the Formula, R-Squared, and AIC
evaluate_model <- function(formula_str) {
  
  # Run the model on the design object
  # (assuming 'scf_design' is your existing design object from previous steps)
  model_list <- with(scf_design, svyglm(as.formula(formula_str), design = scf_design))
  
  # Calculate Average AIC across the 5 implicates
  avg_aic <- mean(sapply(model_list, AIC))
  
  # Calculate Average Pseudo R-Squared
  # (1 - Deviance/NullDeviance)
  avg_r2 <- mean(sapply(model_list, function(m) {
    1 - (m$deviance / m$null.deviance)
  }))
  
  return(data.frame(
    Formula = formula_str,
    AIC = avg_aic,
    Pseudo_R2 = avg_r2
  ))
}

# 8. Loop through all formulas (This might take a minute or two)
print(paste("Testing", length(formulas), "combinations..."))
results_list <- lapply(formulas, evaluate_model)

# 9. Combine into one table and Sort
results_table <- do.call(rbind, results_list)

# Sort by AIC (Lower is better)
# You can also sort by R2 (Higher is better) using: results_table[order(-results_table$Pseudo_R2), ]
best_models <- results_table[order(results_table$AIC), ]

# 10. View the Top 10 Models
print(head(best_models, 10))

# 11. Automatically select the winning formula
winning_formula <- best_models$Formula[1]
print(paste("The Winner Is:", winning_formula))

# 12. Run the winning model dynamically
final_model <- with(scf_design, 
                    svyglm(as.formula(winning_formula), design = scf_design))

# 13. Pool the results
final_pooled <- MIcombine(final_model)

# 14. Calculate metrics
summary_data <- summary(final_pooled)
t_stats <- summary_data$results / summary_data$se
p_values <- 2 * (1 - pnorm(abs(t_stats)))

# 15. Create the Final Table
final_table <- cbind(
  Multiplier = exp(coef(final_pooled)), 
  exp(confint(final_pooled)), 
  P_Value = p_values
)

# 16. Clean up formatting
final_table <- round(final_table, 4)
final_table[, 1:3] <- round(final_table[, 1:3], 2)

# 17. Print Final Result
print("Final Results for the Best Model:")
print(final_table)

#### Do a sanity check on income vs. wealth with the top model ###########
# Define the Formula (Winning Model + Transformed Income)
# We use 'ihs(income)' to match the scale of 'ihs(networth)'
sanity_check_formula <- "ihs(networth) ~ agecl + edcl + race + married + businessowner + ihs(income)"

# Run the model
sanity_model <- with(scf_design, 
                     svyglm(as.formula(sanity_check_formula), design = scf_design))

# Pool the results
sanity_pooled <- MIcombine(sanity_model)

# Generate the Clean Table
summary_data <- summary(sanity_pooled)
t_stats <- summary_data$results / summary_data$se
p_values <- 2 * (1 - pnorm(abs(t_stats)))

sanity_table <- cbind(
  Multiplier = exp(coef(sanity_pooled)), 
  exp(confint(sanity_pooled)), 
  P_Value = p_values
)

# Format for reading
sanity_table <- round(sanity_table, 4)
sanity_table[, 1:3] <- round(sanity_table[, 1:3], 2)

print(sanity_table)

# ############################  End  ################################## #