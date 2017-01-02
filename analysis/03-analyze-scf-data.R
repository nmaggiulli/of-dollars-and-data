cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #


########################## Start Program Here ######################### #

# Load data fom local library
scf_stack <- readRDS(paste0(localdir, "03-scf-stack.Rds"))

for_model <- scf_stack[, !names(scf_stack) %in% c("asset", "debt")]

# Create an initial lm to explore the data
lm_model <- lm (networth ~ homeeq + fin + income + resdbt + agecl + male + white + edcl + married + kids, data = for_model)
summary(lm_model)

# ############################  End  ################################## #