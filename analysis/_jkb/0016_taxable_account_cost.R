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

folder_name <- "_jkb/0016_taxable_account_cost"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bw_colors <- c("#000000", "#525252")

ret <- 0.07
cap_gains <- 0.15
starting_amount <- 10000

df <- data.frame(year = seq(0, 30, 1))

for(i in 1:nrow(df)){
  if(i == 1){
    df[i, "port_bh"] <- starting_amount
    df[i, "port_bh_liq"] <- starting_amount
    df[i, "port_sell_ann"] <- starting_amount
  } else{
    df[i, "port_bh"] <- df[(i-1), "port_bh"] * (1 + ret)
    
    df[i, "port_bh_liq"] <-  df[i, "port_bh"] - ((df[i, "port_bh"] - starting_amount)*(cap_gains))

    df[i, "port_sell_ann"] <- df[(i-1), "port_sell_ann"] * (1 + (ret*(1-cap_gains)))
  }
  
  df[i, "premium_bh_no_sell"] <- df[i, "port_bh_liq"]/df[i, "port_sell_ann"]
  df[i, "premium_bh_no_sell_ann"] <- df[i, "premium_bh_no_sell"]^(1/i) - 1
  
  df[i, "premium_bh_nontax"] <- df[i, "port_bh"]/df[i, "port_bh_liq"]
  df[i, "premium_bh_nontax_ann"] <- df[i, "premium_bh_nontax"]^(1/i) - 1
  
  df[i, "premium_bh"] <- df[i, "port_bh"]/df[i, "port_sell_ann"]
  df[i, "premium_bh_ann"] <- df[i, "premium_bh"]^(1/i) - 1
}

to_plot <- df %>%
            select(year, contains("port_bh"), "port_sell_ann") %>%
            gather(-year, key=key, value=value) %>%
            mutate(key = case_when(
              key == "port_bh" ~ "No Tax",
              key == "port_bh_liq" ~ "Taxed Once",
              key == "port_sell_ann" ~ "Taxed Annually",
              TRUE ~ "Error"
            )) %>%
            filter(key != "Taxed Annually")

#Do plot
file_path <- paste0(out_path, "/tax_vs_nontax_account.jpeg")

text_labels <- data.frame()

text_labels[1, "year"] <- 24
text_labels[1, "value"] <- 64000
text_labels[1, "label"] <- "No Tax"

text_labels[2, "year"] <- 26
text_labels[2, "value"] <- 35000
text_labels[2, "label"] <- "Taxed Once"

plot <- ggplot(to_plot, aes(x = year, y = value, color = key)) +
  geom_line(aes(linetype = key)) +
  geom_text(data=text_labels, aes(x=year, y=value, col = label, label = label)) +
  scale_y_continuous(label = dollar) +
  scale_color_manual(guide = FALSE, values = c(bw_colors[1], bw_colors[2])) +
  scale_linetype(guide = FALSE) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Growth of $10,000 by Account Type")) +
  labs(x = "Year" , y = paste0("Growth of $10,000"))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")




# ############################  End  ################################## #