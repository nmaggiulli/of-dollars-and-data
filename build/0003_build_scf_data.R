cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(tidyverse)

########################## Start Program Here ######################### #

in_path <- paste0(importdir, "0003_scf_data/SCF")

# Create a year list to loop through
year_list <- seq(1989, 2022, 3)

# Log everything printed here so the diagnostics can be shared
while(sink.number() > 0) sink()
log_con <- file(paste0(localdir, "0003_scf_stack_build_log.txt"), open = "wt")
sink(log_con, split = TRUE)

# Definitions are from here:
#
# https://www.federalreserve.gov/econresdata/scf/files/bulletin.macro.txt
#
# networth = total networth (asset - debt)
# asset = value of all assets (fin + nfin)

# fin = total finanical assets (LIQ+CDS+NMMF+STOCKS+BOND+RETQLIQ+SAVBND+CASHLI+OTHMA+OTHFIN)
# liq = all types of transactions accounts (liquid assets)
# cds = Certificate of deposit
# nmmf = Mutual Funds
# stocks = Stocks
# bond = Bonds
# retqliq = total quasi-liquid: sum of IRAs, thrift accounts, and future pensions;
# savbnd = savings bonds
# cashli = Cash life insurance
# othma = other managed assets (trusts, annuities and managed investment)
# othfin = other financial assets: includes loans from the household to someone else, future proceeds, royalties, futures, non-public stock, deferred compensation, oil/gas/mineral invest., cash;
# reteq = retirement equity

# nfin = total non-financial assets (VEHIC+HOUSES+ORESRE+NNRESRE+BUS+OTHNFIN)
# vehic = value of all vehicles
# houses = primary residence
# oresre = other residential real estate
# nnresre = non-residential real estate
# bus = business value
# othnfin = other non-financial assets (jewelry)
# homeeq = value of home equity

# debt = value of all debt // DEBT=MRTHEL+RESDBT+OTHLOC+CCBAL+INSTALL+ODEBT
# mrthel = mortgage debt
# resdbt = other residential debt
# rent = Monthly rent spending: all housing types (including mobile homes);
# ccbal = credit card balance
# install = installment loan
# odebt = Other debt
# hdebt = dummy, 1 if has debt, 0 if no debt
# payedu1-7 = student loans

# income = total household income
# wageinc = Wage and salary income
# intdivinc = 	Interest (taxable and nontaxable) and dividend income
# bussefarminc = 	Income from business, sole proprietorship, and farm
# kginc = Capital gain or loss income
# ssretinc = Social security and pension income
# agecl = age class, 1:<35, 2:35-44, 3:45-54, 4:55-64, 5:65-74, 6:>=75
# age = age,
# hhsex = gender, 1 = male , 2 = female
# race = race, 1 = white non-Hispanic, 2 = nonwhite or Hispanic
# racelcl4 = 1=white non-Hispanic, 2=black, 3=Hispanic or Latino, 4=Other or Multiple race;
# edcl = education class, 1 = no high school diploma/GED, 2 = high school diploma or GED,
#   3 = some college, 4 = college degree
# married = marital status, 1 = married/living with partner, 2 = neither married nor living with partner
# kids = number of kids

# NEW - unrealized capital gains (summary extract, same real dollars as
# networth):
# kgtotal = total unrealized capital gains (KGHOUSE+KGORE+KGBUS+KGSTMF)
# kghouse = on the primary residence
# kgore   = on other real estate
# kgbus   = on businesses
# kgstmf  = on stocks and mutual funds

# x5801 = Inheritance (1 = yes, 5 = no)
# x3915 = stock options (actual value)
# NEW - from the summary extract (same real dollars as networth):
# trusts   = trusts the household has an equity interest in (part of othma)
# annuit   = annuities (the other part of othma)
# ifinpro  = 1 if uses a lawyer, accountant, banker or broker for saving/
#            investment information (1998+; 0 in earlier years)
# ifinplan = 1 if uses a financial planner for saving/investment information
#
# x4006 = cash value of life insurance, NOMINAL. Only used to back out each
#         year's dollar adjustment (cashli = x4006 x adjustment) - no CPI
#         table needed. Do NOT use income/x5729 for this: the summary income
#         also carries a prior-year-to-survey-year income adjustment.

vars_to_keep <- c('y1', 'yy1', 'networth', 'asset',
                  'fin' , 'liq', 'cds', 'nmmf', 'stocks', 'bond', 'retqliq', 'savbnd', 'cashli', 'othma', 'othfin', 'reteq',
                  'nfin', 'vehic', 'houses', 'oresre', 'nnresre', 'bus', 'othnfin', 'homeeq',
                  'debt', 'mrthel','resdbt', 'rent','ccbal', 'install', 'odebt', 'hdebt',
                  'payedu1', 'payedu2', 'payedu3', 'payedu4', 'payedu5', 'payedu6', 'payedu7',
                  'income', 'wageinc', 'intdivinc', 'bussefarminc',  'kginc', 'ssretinc',
                  'kgtotal', 'kghouse', 'kgore', 'kgbus', 'kgstmf',
                  'trusts', 'annuit', 'ifinpro', 'ifinplan',
                  'agecl', 'age', 'hhsex', 'race', 'racecl4', 'edcl', 'married', 'kids',
                  'x5801', 'x3915', 'x4006',
                  'wgt')

# ---------------------------------------------------------------------- #
# RAW QUESTIONS FOR THE INHERITANCE / ESTATE / GIVING / TRUST ANALYSIS
#
# Raw x-variables are NOT in real dollars and are NOT clean 0/1 flags:
#   - dollar amounts are nominal; 0 = inapplicable, -1 = a real "none"
#   - yes/no questions are usually 1 = yes, 5 = no
#
# VERIFY EVERY NUMBER AND CODE BELOW against the 2022 codebook before
# trusting the output (and again against the 2025 codebook). Open
# codebk2022.txt in a browser and search the quoted phrase. Set any you
# can't confirm to NA - the clean variable is then left out of the stack
# and the analysis script skips that piece instead of running on a guess.
#
# Confidence: x5804/x5809/x5814/x5818/x5821 are confirmed dollar-amount
# fields in this block; the yes/no and scale codes are my best recollection.
# The build log prints every code that actually appears, so a wrong number
# or code shows up right away.
# ---------------------------------------------------------------------- #

x_lookup <- list(
  # "ever received an inheritance" amounts: 1st, 2nd, 3rd, all others
  inh_amt_1     = "x5804",   # search: "inheritance" amount, 1st
  inh_amt_2     = "x5809",
  inh_amt_3     = "x5814",
  inh_amt_other = "x5818",   # search: "other inheritances"
  # "expect to receive a substantial inheritance"
  inh_expect    = "x5819",   # search: "expect to receive"
  inh_expect_amt = "x5821",
  # X5825 (Q1409) "EXPECT LEAVE ESTATE?" - confirmed in the Fed's
  # variable search
  bequest_expect    = "x5825",
  # importance of leaving an estate - check X5826 in the variable search
  bequest_important = NA,
  # X5822 (Q1404) "CHARITABLE CONTRIBS?" and X5823 (Q1405) "AMT CONTRIB" -
  # confirmed. Check X5822's wording for the $500 threshold.
  charity_500 = "x5822",
  charity_amt = "x5823",
  # financial support given to people outside the household
  support_given = NA         # search: "support" / "relatives or friends"
)
# Trusts come from the summary variable `trusts` instead (has_trust below),
# so there is no raw trust question to look up. It cannot separate trusts
# the household set up from ones it benefits from.

# Response codes. Check these against the codebook too.
yes_code        <- 1
no_code         <- 5
possibly_codes  <- c(2, 3)      # "possibly" on the sizable-estate question
important_codes <- c(1, 2)      # codes counted as "important" on the scale

raw_x <- unique(na.omit(unlist(x_lookup)))
vars_to_keep <- unique(c(vars_to_keep, raw_x))

for (x in year_list){
  print(paste0("Now processing: ", x))
  # Load SCF data into memory
  t <- readRDS(paste0(in_path, "/scf ", x, ".rds"))
  print("Data read in")
  
  # Subset to the variables we care about. Questions that weren't asked in
  # a given year come back as NA instead of stopping the build.
  subset_data <- function(name){
    data <- get(name)
    missing_vars <- setdiff(vars_to_keep, names(data))
    data[missing_vars] <- NA_real_
    data <- data[ , vars_to_keep]
    data["year"] <- x
    assign(name, data, envir = .GlobalEnv)
  }
  
  if (x == year_list[1] | x == max(year_list)){
    missing_now <- setdiff(vars_to_keep, names(as.data.frame(t[1])))
    if (length(missing_now) > 0){
      print(paste0("Not in ", x, " file (filled with NA): ",
                   paste(missing_now, collapse = ", ")))
    }
  }
  
  # Loop through each of the imp datasets to subset them to relevant variables
  for (i in 1:5){
    assign(paste0("imp", i), as.data.frame(t[i]), envir = .GlobalEnv)
    string <- paste0("imp", i)
    subset_data(string)
  }
  
  # Stack the datasets
  if (x == year_list[1]){
    scf_stack <- rbind(imp1, imp2, imp3, imp4, imp5)
  } else{
    scf_stack <- rbind(scf_stack, imp1, imp2, imp3, imp4, imp5)
  }
}

# ---- Dollar adjustment for raw amounts -------------------------------- #
# The summary variables are already in the latest year's dollars; the raw
# x-variables are nominal. cashli / x4006 is the same ratio for every
# household in a year, so the median of it IS that year's adjustment
# factor - taken straight from the data, no CPI table. cashli is just
# MAX(0, x4006) times the Fed's real-dollar factor (bulletin.macro).
dollar_adj <- scf_stack %>%
  filter(x4006 > 0, cashli > 0) %>%
  group_by(year) %>%
  summarise(dollar_adj = median(cashli / x4006),
            spread     = max(cashli / x4006) - min(cashli / x4006),
            .groups = "drop")

print("Dollar adjustment by year (latest year should be 1.00; spread ~0):")
print(as.data.frame(dollar_adj))

scf_stack <- scf_stack %>% left_join(dollar_adj %>% select(year, dollar_adj),
                                     by = "year")

# ---- Clean the raw questions ------------------------------------------ #
get_x <- function(key){
  v <- x_lookup[[key]]
  if (is.na(v)) return(NULL)
  scf_stack[[v]]
}

# Amounts: 0 = inapplicable and -1 = none, so both become 0. Then adjust to
# real dollars.
clean_amt <- function(values){
  ifelse(is.na(values) | values <= 0, 0, values) * scf_stack$dollar_adj
}

yes_no <- function(values){
  case_when(values == yes_code ~ 1,
            values == no_code  ~ 0,
            TRUE ~ NA_real_)
}

scf_stack_final <- scf_stack

if (!is.null(get_x("inh_amt_1"))){
  # NOTE: respondents report each inheritance's value when received, so
  # this is in the dollars of the year it was received, adjusted only to
  # the survey year. Fine for "who inherited" and rough sizing; not a
  # precise real value.
  scf_stack_final$inh_amt <- clean_amt(get_x("inh_amt_1")) +
    clean_amt(get_x("inh_amt_2")) +
    clean_amt(get_x("inh_amt_3")) +
    clean_amt(get_x("inh_amt_other"))
}

if (!is.null(get_x("inh_expect"))){
  scf_stack_final$inh_expect     <- yes_no(get_x("inh_expect"))
  scf_stack_final$inh_expect_amt <- clean_amt(get_x("inh_expect_amt"))
}

if (!is.null(get_x("bequest_expect"))){
  raw <- get_x("bequest_expect")
  scf_stack_final$bequest_expect <- case_when(raw == yes_code ~ "Yes",
                                              raw %in% possibly_codes ~ "Possibly",
                                              raw == no_code ~ "No",
                                              TRUE ~ NA_character_)
}

if (!is.null(get_x("bequest_important"))){
  raw <- get_x("bequest_important")
  scf_stack_final$bequest_important <- ifelse(raw <= 0 | is.na(raw), NA,
                                              ifelse(raw %in% important_codes, 1, 0))
}

if (!is.null(get_x("charity_500"))){
  scf_stack_final$charity_500 <- yes_no(get_x("charity_500"))
  scf_stack_final$charity_amt <- clean_amt(get_x("charity_amt"))
}

if (!is.null(get_x("support_given"))){
  scf_stack_final$support_given <- yes_no(get_x("support_given"))
}

# Trusts the household has an equity interest in. Likely undercounts
# revocable living trusts, which respondents often report as the underlying
# assets. Before 1998 `trusts` is an estimate split out of othma.
scf_stack_final$has_trust <- as.numeric(scf_stack_final$trusts > 0)

# Info sources: 0 before 1998 means "not asked", not "no"
scf_stack_final <- scf_stack_final %>%
  mutate(ifinpro  = ifelse(year < 1998, NA, ifinpro),
         ifinplan = ifelse(year < 1998, NA, ifinplan))

# ---- Diagnostics: what codes actually appear --------------------------- #
# For every raw question: share of records with each value, latest year
# only, and which years have it at all. Yes/no questions should show only
# 0/1/5 (plus maybe -1 or 0 for inapplicable). Anything else means the
# number or the codes are wrong.
latest <- max(year_list)

for (key in names(x_lookup)){
  v <- x_lookup[[key]]
  if (is.na(v)) next
  vals <- scf_stack[[v]]
  years_present <- unique(scf_stack$year[!is.na(vals)])
  print(paste0("---- ", key, " (", v, ") present in: ",
               paste(sort(years_present), collapse = ", ")))
  latest_vals <- vals[scf_stack$year == latest]
  if (grepl("amt", key)){
    print(paste0("  ", latest, ": share > 0 = ",
                 round(mean(latest_vals > 0, na.rm = TRUE), 3),
                 ", median of positives = ",
                 round(median(latest_vals[latest_vals > 0], na.rm = TRUE))))
  } else {
    print(round(prop.table(table(latest_vals, useNA = "ifany")), 3))
  }
}

# ---- Original cleaning ------------------------------------------------- #
scf_stack_final <- mutate(scf_stack_final, married = married %% 2,
                          white = race %% 2,
                          male = hhsex %% 2) %>%
  select(-hhsex, - race) %>%
  mutate(race = case_when(racecl4 == 1 ~ "White",
                          racecl4 == 2 ~ "Black",
                          racecl4 == 3 ~ "Hispanic",
                          racecl4 == 4 ~ "Other",
                          TRUE ~ "Missing"),
         agecl = case_when(agecl == 1 ~ "<35",
                           agecl == 2 ~ "35-44",
                           agecl == 3 ~ "45-54",
                           agecl == 4 ~ "55-64",
                           agecl == 5 ~ "65-74",
                           agecl == 6 ~ "75+",
                           TRUE ~ "99"),
         edcl = case_when(edcl == 1 ~ "No High School",
                          edcl == 2 ~ "High School",
                          edcl == 3 ~ "Some College",
                          edcl == 4 ~ "College Degree",
                          TRUE ~ "99"),
         birthyear = year - age,
         inheritance = ifelse(x5801 == 1, 1, 0),
         stock_options = x3915,
         payedu = payedu1 + payedu2 + payedu3 + payedu4 + payedu5 + payedu6 + payedu7) %>%
  select(-payedu1, -payedu2, -payedu3, -payedu4, -payedu5, -payedu6, -payedu7,
         -x3915, -x5801, -x4006, -dollar_adj, -any_of(raw_x))

# Make edcl into a factor
scf_stack_final$edcl <- factor(scf_stack_final$edcl,levels = c("No High School",
                                                               "High School",
                                                               "Some College",
                                                               "College Degree"))

# Make agecl into a factor
scf_stack_final$agecl <- factor(scf_stack_final$agecl,levels = c("<35", "35-44", "45-54", "55-64",
                                                                 "65-74", "75+"))

scf_stack_final <- scf_stack_final %>%
  rename(hh_id = yy1,
         imp_id = y1)

print("New variables in the stack:")
print(intersect(c("kgtotal", "kghouse", "kgore", "kgbus", "kgstmf", "inh_amt",
                  "inh_expect", "inh_expect_amt", "bequest_expect",
                  "bequest_important", "charity_500", "charity_amt",
                  "support_given", "trusts", "has_trust", "ifinpro", "ifinplan"),
                names(scf_stack_final)))

# Save down data to permanent file
saveRDS(scf_stack_final, paste0(localdir, "0003_scf_stack.Rds"))

sink()
close(log_con)

# ############################  End  ################################## #