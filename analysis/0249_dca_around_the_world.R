cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidylog)
library(zoo)
library(Hmisc)
library(lemon)
library(tidyverse)

folder_name <- "0249_dca_around_the_world"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

period_length <- 120

raw_1970 <- read.csv(paste0(importdir, "/0249_world_returns/dfa_msci_1970.csv"), skip = 6,
                col.names = c("date", "US", "Spain",
                              "UK", "Canada", "Japan", "Italy"),
                ) %>%
        filter(row_number() != 1, `US` != "") %>%
        mutate(date = as.Date(date, "%m/%d/%y"))

df_1970 <- sapply(raw_1970[2:ncol(raw_1970)], as.numeric) %>%
                as.data.frame() %>%
                bind_cols(date = raw_1970[, "date"]) %>%
            select(date, everything())

raw_1999 <- read.csv(paste0(importdir, "/0249_world_returns/dfa_msci_1999.csv"), skip = 6,
                     col.names = c("date", "China", "Greece", "Russia"),
) %>%
  filter(row_number() != 1, `China` != "") %>%
  mutate(date = as.Date(date, "%m/%d/%y"))

df_1999 <- sapply(raw_1999[2:ncol(raw_1999)], as.numeric) %>%
  as.data.frame() %>%
  bind_cols(date = raw_1999[, "date"]) %>%
  select(date, everything())

df <- df_1970 %>%
        left_join(df_1999)

index_cols <- colnames(df[, 2:ncol(df)])

final_results <- data.frame()
counter <- 0

for(x in index_cols){
  print(x)
  pre_filter <- df %>%
            select_("date", x) %>%
            rename_(.dots = setNames(paste0(x), "index")) %>%
            drop_na %>%
            mutate(ret = index/lag(index, 1) - 1)
            
  start_months <- pre_filter[1:(nrow(pre_filter)-period_length+1), "date"]
   
  for(s in 1:length(start_months)){
    print(start_months[s])
    start_month <- start_months[s]
    end_month <- start_month + days(1) + months(period_length-1) - days(1)
    
    tmp <- pre_filter %>%
      filter(date >= start_month, date <= end_month)
    
    dca_amount <- 100
    
    for(j in 1:nrow(tmp)){
      if(j == 1){
        tmp[j, "cost_basis"] <- dca_amount
        tmp[j, "value"] <- dca_amount
      } else{
        ret <- 1 + tmp[j, "ret"]
        mt <- month(tmp[j, "date"])
        
        tmp[j, "cost_basis"] <- tmp[(j-1), "cost_basis"] + dca_amount
        tmp[j, "value"] <- (tmp[(j-1), "value"] * ret) + dca_amount
      }
    }
    
    tail <- tmp %>%
      tail(1)
    
    counter <- counter + 1
    
    final_results[counter, "start_date"] <- start_month
    final_results[counter, "end_date"]   <- end_month
    final_results[counter, "cost_basis"] <- tail$cost_basis
    final_results[counter, "value"]      <- tail$value
    final_results[counter, "country"]    <- x
  }
}

final_results <- final_results %>%
              mutate(stock_premium = value/cost_basis - 1,
                stock_beats_cash = ifelse(stock_premium > 0, 1, 0),
                stock_premium_bins = case_when(
                  stock_premium < 0 ~ "<0%",
                  stock_premium < 0.5~ "0%-50%",
                  TRUE ~ ">50%"
                )
              )
                
final_results$stock_premium_bins <- factor(final_results$stock_premium_bins, levels = c("<0%", "0%-50%",">50%"))

source_string <- paste0("Source: Returns 2.0")
note_string <- str_wrap(paste0("Note: Assumes that DCA invests $100 a month for 10 years. Returns are shown net of dividends."),
                        width = 80)

file_path <- paste0(out_path, "/dca_all_countries.jpeg")
to_plot <- final_results 

plot <- ggplot(to_plot, aes(x= start_date, y=value, col = country)) +
  geom_line() +
  scale_y_continuous(label = dollar) +
  geom_hline(yintercept = period_length*dca_amount, linetype = "dashed") +
  of_dollars_and_data_theme +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle(paste0("DCA Final Portfolio Value\nOver 10 Years\nBy Country")) +
  labs(x="Start Date", y="Final Portfolio Value",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do binning
file_path <- paste0(out_path, "/dca_bins_all_countries.jpeg")

country_bin <- final_results %>%
              group_by(stock_premium_bins, country) %>%
              summarise(count = n()) %>%
              ungroup()

country <- final_results %>%
  group_by(country) %>%
  summarise(country_total = n()) %>%
  ungroup()

to_plot <- country_bin %>%
            left_join(country) %>%
            mutate(pct = count/country_total,
                   label = paste0(100*round(pct, 2), "%")) %>%
            select(country, stock_premium_bins, pct, label)
  
plot <- ggplot(to_plot, aes(x=stock_premium_bins, y=pct, fill = country)) +
  geom_bar(stat = "identity") +
  geom_text(data=to_plot, aes(x=stock_premium_bins, y=pct + 0.05, label = label), col = "black", size = 2) + 
  facet_wrap(~country) +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(label = percent_format(accuracy = 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("DCA Return Above Cash\nOver 10 Years\nBy Country and Return Bin")) +
  labs(x="Return Bin", y="Percentage",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

print(paste0("Stock beats cash: ", 100*round(mean(final_results$stock_beats_cash), 2), "% of the time (avg)"))
print(paste0("Stock beats cash by: ", 100*round(mean(final_results$stock_premium), 2), "% (avg)"))
print(paste0("Stock beats cash by: ", 100*round(quantile(final_results$stock_premium, probs = 0.5), 2), "% (median)"))

# ############################  End  ################################## #