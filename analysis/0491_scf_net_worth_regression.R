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
library(gt)
library(tidyverse)

folder_name <- "0491_scf_net_worth_regressions"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

scf_stack <- readRDS(paste0(localdir, "0003_scf_stack.Rds"))

date_string <- date_to_string(Sys.Date())

df <- scf_stack %>%
      filter(year == 2022) %>%
    mutate(businessowner = ifelse(bus > 0, 1, 0)) %>%
    select(year, hh_id, imp_id, 
             agecl, edcl, married, businessowner,inheritance, income,
             networth,
             wgt)

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

# 5. Define the 4 Cumulative Formulas
formulas <- list(
  Model_1_Age    = "ihs(networth) ~ agecl",
  Model_2_Education  = "ihs(networth) ~ agecl + edcl",
  Model_3_Marriage = "ihs(networth) ~ agecl + edcl + married",
  Model_4_EducationMarriage  = "ihs(networth) ~ agecl + edcl*married",
  Model_5_Inheritance = "ihs(networth) ~ agecl + edcl*married + inheritance",
  Model_6_Business  = "ihs(networth) ~ agecl + edcl*married + inheritance + businessowner",
  Model_7_BusinessMarriage = "ihs(networth) ~ agecl + edcl*married + inheritance + married*businessowner"
)

# 6. Run them all, PRINT detailed results, and store summary
results_list <- list()

for (name in names(formulas)){
  f <- formulas[[name]]
  
  # Run Model
  mod <- with(scf_design, svyglm(as.formula(f), design = scf_design))
  
  # Pool Results
  pool <- MIcombine(mod)
  
  # Get Coefficients directly
  betas <- coef(pool)
  
  # Calculate Standard Errors directly (Square root of diagonal variance)
  se <- sqrt(diag(vcov(pool)))
  
  # Calculate P-values manually
  t_stats <- betas / se
  p_vals <- 2 * (1 - pnorm(abs(t_stats)))
  
  p_vals <- setNames(as.vector(p_vals), names(coef(pool)))
  
  # --- PRINT DETAILED TABLE FOR THIS MODEL ---
  model_table <- cbind(
    Multiplier = exp(coef(pool)), 
    P_Value = p_vals
  )
  
  # Formatting for readability
  # First, round everything (including P_Value) to 4 decimal places
  model_table <- round(model_table, 4)
  
  # Then, round ONLY the first column (Multiplier) to 2 decimal places
  model_table[, 1] <- round(model_table[, 1], 2)
  
  print(paste("---------- RESULTS:", name, "-----------"))
  print(model_table)
  
  df_viz <- as.data.frame(model_table) %>%
    rownames_to_column(var = "Raw_Name") %>%
    mutate(
      # Create a "Significance" string based on P_Value
      Stars = case_when(
        P_Value < 0.01 ~ "***",
        P_Value < 0.05 ~ "**",
        P_Value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      # Combine P_Value and Stars into one column for a cleaner look
      P_Value_Clean = paste0(sprintf("%.4f", P_Value), " ", Stars),
      
      # Your existing renaming logic
      Variable = case_when(
        str_detect(Raw_Name, "income")  ~ "Income",
        Raw_Name == "married" ~ "Married",
        Raw_Name == "inheritance" ~ "Inheritance",
        Raw_Name == "businessowner" ~ "Business Owner (Single)",
        Raw_Name == "married:businessowner" ~ "Business Owner + Married",
        Raw_Name == "(Intercept)" ~ "Baseline (Intercept)",
        str_detect(Raw_Name, "edcl") & str_detect(Raw_Name, "married") ~ str_replace(str_replace(Raw_Name, "edcl", "Ed: "), ":married", " + Married"),
        str_detect(Raw_Name, "agecl") ~ str_replace(Raw_Name, "agecl", "Age: "),
        str_detect(Raw_Name, "edcl") ~ str_replace(Raw_Name, "edcl", "Ed: "),
        TRUE ~ Raw_Name
      )
    ) %>%
    select(Variable, Multiplier, P_Value_Clean) # Use the new clean column
  
  # 2. Generate the table
  final_table <- df_viz %>%
    gt() %>%
    tab_header(
      title = md(paste0("**Results: ", name, "**")),
      subtitle = "Significance: *** p<0.01, ** p<0.05, * p<0.1"
    ) %>%
    cols_label(
      P_Value_Clean = "P Value" # Rename header so it looks normal
    ) %>%
    fmt_number(
      columns = c(Multiplier),
      decimals = 2
    ) %>%
    # Keep your bolding and alignment logic...
    cols_align(align = "center", columns = everything())
  
  file_name <- paste0(out_path, "/results_", name, "_", date_string, ".png")
  
  # Save the file
  gtsave(final_table, file_name)
}

# 7. Calculate Average AIC for each Model
aic_list <- list()

for (name in names(formulas)) {
  f <- formulas[[name]]
  
  # Re-run the model to access the underlying svyglm objects
  # (Since we didn't save the full 'mod' objects in a list earlier)
  mod <- with(scf_design, svyglm(as.formula(f), design = scf_design))
  
  # Calculate AIC for each of the 5 imputations and take the mean
  # The 'extractAIC' function returns (df, AIC), so we take the 2nd element
  avg_aic <- mean(sapply(mod, function(m) extractAIC(m)[2]))
  
  aic_list[[name]] <- avg_aic
}

# Convert to Dataframe for sorting
aic_df <- data.frame(
  Model = names(aic_list),
  AIC = unlist(aic_list)
)

# Sort by AIC (Lower is Better)
aic_df <- aic_df[order(aic_df$AIC), ]

# Calculate Delta AIC (Difference from best model)
aic_df$Delta_AIC <- aic_df$AIC - min(aic_df$AIC)

print("--- MODEL COMPARISON (AIC) ---")
print(aic_df)

# ############################  End  ################################## #