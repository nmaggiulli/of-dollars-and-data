cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(BenfordTests)

########################## Start Program Here ######################### #

# Create a custom palette with black using COlorBrewer
# From here:  http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=7
my_palette <- c("#4DAF4A", "#E41A1C", "#377EB8", "#000000", "#984EA3", "#FF7F00", "#A65628")

nyse_fundamentals <- readRDS(paste0(localdir, "18-nyse-fundamentals.Rds"))

vars_to_test <- c("Accounts.Payable", "Accounts.Receivable", "Capital.Expenditures",
                  "Cash.and.Cash.Equivalents", "Depreciation", "Earnings.Before.Interest.and.Tax",
                  "Goodwill", "Gross.Profit", "Income.Tax",
                  "Net.Income", "Operating.Income", "Total.Assets",
                  "Total.Equity", "Total.Revenue")

vars_shortname <- c("accounts_payable", "accounts_receivable", "capex", "cash", "depreciation", "ebit",
                    "goodwill", "gross_profit", "income_tax",
                    "net_income", "operating_income", "tot_assets", "tot_equity", "tot_revenue")

# Bind the full names with short names
vars_df <- as.data.frame(cbind(vars_to_test, vars_shortname))

# Create a sequence of the digits to use for the Benford analysis
benford_digits <- data.frame(leading_digit = as.character(seq(1,9)), stringsAsFactors = FALSE)

# Write a function to create the benford counts from the data
create_bedford_counts <- function(df, name_df){
  var_string              <- paste0(name_df[,1])
  temp                    <- select_(df, var_string) 
  temp[, var_string]      <- as.character(temp[,  var_string])
  temp["leading_digit"]  <- gsub("-?0?\\.?(\\d).*", "\\1", temp[, var_string])
  
  temp <- temp %>%
            inner_join(benford_digits)
  
  temp <- temp %>%
          group_by(leading_digit) %>%
          summarize(count = n()) %>%
          mutate(benford_count = nrow(temp) * log10(1 + 1/(as.numeric(leading_digit))),
                 n_obs = nrow(temp)) %>%
                 select(leading_digit, n_obs, count, benford_count)
  
  temp["shortname"] <- name_df[,2]
    
  return(temp)
}

# Loop through the function for each variable of interest to build the data
for (i in 1:nrow(vars_df)){
  string <- as.character(vars_df[i,1])
  print(string)
  if (i == 1){
    benford_stats                <- create_bedford_counts(nyse_fundamentals, vars_df[i, ])
    benford_stats[1:9,"p_value"] <- ks.benftest(nyse_fundamentals[, string])$p.value
  } else {
    new_stats                    <- create_bedford_counts(nyse_fundamentals, vars_df[i, ])
    new_stats[1:9,"p_value"]     <- ks.benftest(nyse_fundamentals[, string])$p.value
    benford_stats                <- bind_rows(benford_stats, new_stats)
  }
  sname <- vars_df[i,2]
  to_plot <- filter(benford_stats, shortname == sname)
  
  # Set the file path
  file_path = paste0(exportdir, "18-nyse-benford-plots/benford-", sname,".jpeg")
  
  # Get the p-value from the statisical test
  p_value <- round(min(to_plot$p_value)*100, 2)
  if (p_value == 0){
    p_value <- "less than 0.01"
  }
  
  # Create plot 
  plot <- ggplot(data = to_plot, aes(x = leading_digit, y = count)) +
    geom_bar(stat = "identity", col = "black", fill = "black") +
    geom_point(data =  to_plot, aes(x = leading_digit, y = benford_count), col = "red", size = 5) +
    geom_text_repel(data = filter(to_plot, leading_digit == "1"),
                    aes(x = leading_digit,
                        y = (benford_count*0.75)),
                    label = "Actual Frequency",
                    col =  "black",
                    family = "my_font",
                    nudge_y = 60,
                    nudge_x = 2.2) +
    geom_text_repel(data = filter(to_plot, leading_digit == "5"),
                    aes(x = leading_digit,
                        y = benford_count),
                    label = "Expected Frequency\nUnder Benford's Law",
                    col =  "red",
                    family = "my_font",
                    nudge_y = 130,
                    nudge_x = 1) +
    scale_color_manual(values = my_palette, guide = FALSE) +
    of_dollars_and_data_theme +
    labs(x = "Leading Digit" , y = "Frequency") +
    ggtitle(paste0("Actual Values vs. Benford's Law\n", string))
  
  # Add a source and note string for the plots
  source_string <- "Source:  NYSE data from Kaggle, 2010 - 2016 (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  The probability of seeing this result by chance is ", p_value, "%, when running a KS test.") 
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the gtable
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")
}






# ############################  End  ################################## #