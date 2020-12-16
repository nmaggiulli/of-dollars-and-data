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

folder_name <- "0217_taxable_account_cost"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

ret_high <- 0.1
ret_low <- 0.06
cap_gains <- 0.15
inc_tax <- 0.30
starting_amount <- 10000

df <- data.frame(year = seq(0, 30, 1))

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "port_bh_high"] <- starting_amount
    df[i, "port_bh_low"] <- starting_amount
    df[i, "port_bh_high_liq"] <- starting_amount
    df[i, "port_high_sell_ann"] <- starting_amount
    df[i, "port_low_sell_ann"] <- starting_amount
  } else{
    df[i, "port_bh_high"] <- df[(i-1), "port_bh_high"] * (1 + ret_high)
    df[i, "port_bh_low"] <- df[(i-1), "port_bh_low"] * (1 + ret_low)
    
    df[i, "port_bh_high_liq"] <-  df[i, "port_bh_high"] - ((df[i, "port_bh_high"] - starting_amount)*(cap_gains))

    df[i, "port_high_sell_ann"] <- df[(i-1), "port_high_sell_ann"] * (1 + (ret_high*(1-cap_gains)))
    df[i, "port_low_sell_ann"] <- df[(i-1), "port_low_sell_ann"] * (1 + (ret_low*(1-inc_tax)))
  }
  
  df[i, "port_high_tax_low_nontax"] <- df[i, "port_bh_high_liq"] + df[i, "port_bh_low"]
  df[i, "port_low_tax_high_nontax"] <- df[i, "port_low_sell_ann"] + df[i, "port_bh_high"]
  df[i, "premium_low_tax_high_nontax"] <- df[i, "port_low_tax_high_nontax"]/df[i, "port_high_tax_low_nontax"]
  df[i, "premium_low_tax_high_nontax_ann"] <- df[i, "premium_low_tax_high_nontax"]^(1/i) - 1
  
  df[i, "premium_bh_high_no_sell"] <- df[i, "port_bh_high_liq"]/df[i, "port_high_sell_ann"]
  df[i, "premium_bh_high_no_sell_ann"] <- df[i, "premium_bh_high_no_sell"]^(1/i) - 1
  
  df[i, "premium_bh_high_nontax"] <- df[i, "port_bh_high"]/df[i, "port_bh_high_liq"]
  df[i, "premium_bh_high_nontax_ann"] <- df[i, "premium_bh_high_nontax"]^(1/i) - 1
  
  df[i, "premium_bh_high"] <- df[i, "port_bh_high"]/df[i, "port_high_sell_ann"]
  df[i, "premium_bh_high_ann"] <- df[i, "premium_bh_high"]^(1/i) - 1
}

to_plot <- df %>%
            select(year, contains("port_bh_high"), "port_high_sell_ann") %>%
            gather(-year, key=key, value=value) %>%
            mutate(key = case_when(
              key == "port_bh_high" ~ "No Tax",
              key == "port_bh_high_liq" ~ "Taxed Once",
              key == "port_high_sell_ann" ~ "Taxed Annually",
              TRUE ~ "Error"
            ))

file_path <- paste0(out_path, "/tax_vs_nontax_account.jpeg")
source_string <- paste0("Source: Simulated data (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Assumes all accounts have an annual return of ", 100*ret_high, "% and that the tax rate on capital gains is ", 100*cap_gains, "%."),
                        width = 85)

text_labels <- data.frame()

text_labels[1, "year"] <- 24
text_labels[1, "value"] <- 64000
text_labels[1, "label"] <- "No Tax"

text_labels[2, "year"] <- 21
text_labels[2, "value"] <- 54000
text_labels[2, "label"] <- "Taxed Once"

text_labels[3, "year"] <- 26
text_labels[3, "value"] <- 32000
text_labels[3, "label"] <- "Taxed Annually"

plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=year, y=value, col = label, label = label)) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c("black", "red", "blue")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $10,000 by Account Type")) +
  labs(x = "Year" , y = paste0("Growth of $10,000"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Do it over shorter periods
to_plot <- df %>%
  select(year, contains("port_bh_high"), "port_high_sell_ann") %>%
  gather(-year, key=key, value=value) %>%
  mutate(key = case_when(
    key == "port_bh_high" ~ "No Tax",
    key == "port_bh_high_liq" ~ "Taxed Once",
    key == "port_high_sell_ann" ~ "Taxed Annually",
    TRUE ~ "Error"
  )) %>%
  filter(year <= 3)

text_labels <- data.frame()

text_labels[1, "year"] <- 2.3
text_labels[1, "value"] <- 12000
text_labels[1, "label"] <- "No Tax"

text_labels[2, "year"] <- 2.5
text_labels[2, "value"] <- 11200
text_labels[2, "label"] <- "Taxed Once"

text_labels[3, "year"] <- 2.3
text_labels[3, "value"] <- 11000
text_labels[3, "label"] <- "Taxed Annually"

file_path <- paste0(out_path, "/tax_vs_nontax_account_early.jpeg")

plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=year, y=value, col = label, label = label)) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c("black", "red", "blue")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $10,000 by Account Type\nFor First 3 Years")) +
  labs(x = "Year" , y = paste0("Growth of $10,000"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

#Do final plot of taxable vs. non-taxable
to_plot <- df %>%
  select(year, "port_high_tax_low_nontax", "port_low_tax_high_nontax") %>%
  gather(-year, key=key, value=value) %>%
  mutate(key = case_when(
    key == "port_high_tax_low_nontax" ~ "High Growth Taxable",
    key == "port_low_tax_high_nontax" ~ "High Growth Nontaxable",
    TRUE ~ "Error"
  )) 

file_path <- paste0(out_path, "/port_high_low_tax_nontax.jpeg")
note_string <- str_wrap(paste0("Note: Assumes that the high growth asset returns ", 
                               100*ret_high, 
                               "% annually, the low growth asset returns ",
                               100*ret_low,
                               "% annually, and the tax rate on the high growth asset is ", 
                               100*cap_gains, 
                               "% (LTCG) while the tax rate on the low growth asset is ",
                               100*inc_tax, "%."),
                        width = 85)


text_labels <- data.frame()

text_labels[1, "year"] <- 25
text_labels[1, "value"] <- 45000
text_labels[1, "label"] <- "High Growth Taxable"

text_labels[2, "year"] <- 20
text_labels[2, "value"] <- 80000
text_labels[2, "label"] <- "High Growth Nontaxable"

plot <- ggplot(to_plot, aes(x = year, y = value, col = key)) +
  geom_line() +
  geom_text(data=text_labels, aes(x=year, y=value, col = label, label = label)) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c("black", "blue")) +
  of_dollars_and_data_theme +
  ggtitle(paste0("High Growth Assets Are Better Off\nin Nontaxable Accounts")) +
  labs(x = "Year" , y = paste0("Growth of $20,000"),
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #