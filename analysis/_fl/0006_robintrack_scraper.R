cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(stringr)
library(lubridate)
library(ggrepel)
library(quantmod)
library(tidyverse)

folder_name <- "/_fl/0006_robintrack_scraper"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

download_data <- 0
start_dt <- as.Date("2020-02-19")

ycharts_comp <- read.csv(paste0(importdir, folder_name, "/ycharts_comp_table_8-12-2020.csv")) %>%
                clean_cols() %>%
                  select(-name)

raw_leaderboard <- read.csv(paste0(importdir, folder_name, "/leaderboard_2020-08-11_23-00-00.csv"))

top_200 <- raw_leaderboard %>%
            arrange(-popularity) %>%
            head(200) %>%
            mutate(pop_rank = row_number())

download_robintrack <- function(symbol){
  Sys.sleep(2)
  string <- paste0("https://robintrack.net/api/stocks/", symbol, "/popularity_history_csv")
  tryCatch({
    read.csv(url(string), col.names = c("dt", "pop"))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

if(download_data == 1){
  for(i in 1:nrow(top_200)){
    print(i)
    sym <- top_200[i, "symbol"]
    rt <- download_robintrack(sym) %>%
            mutate(date = as.Date(dt, format = "%Y-%m-%d")) %>%
            group_by(date) %>%
            summarize(pop = max(pop)) %>%
            ungroup() %>%
            select(date, pop)
    
    max_dt <- max(rt$date)
    
    tryCatch({
      yh <- getSymbols(sym, from = start_dt, to = max_dt, 
                       env = NULL,
                       src="yahoo", periodicity = "daily") %>%
        as.data.frame()
      
      yh <- yh %>% mutate(date= as.Date(rownames(yh)))
      
      fnl <- yh %>%
        rename_(.dots = setNames(paste0(sym, ".Adjusted"), "index")) %>%
        mutate(symbol = sym) %>%
        filter(!is.na(index)) %>%
        select(date, index, symbol) %>%
        inner_join(rt) %>%
        drop_na() %>%
        mutate(pop_diff = pop/lag(pop, 1) - 1,
               ret = index/lag(index, 1) - 1) %>%
        drop_na()
      
      if(i == 1){
        final_df <- fnl
      } else{
        final_df <- final_df %>% bind_rows(fnl)
      }
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
  saveRDS(final_df, paste0(localdir, "0006_robintrack_data.Rds"))
} else{
  final_df <- readRDS(paste0(localdir, "0006_robintrack_data.Rds")) %>%
                filter(date >= start_dt)
}

corr_summary <- final_df %>%
                left_join(top_200) %>%
                mutate(lag_ret = lag(ret),
                       lag_pop = lag(pop_diff))
  
to_plot <- corr_summary %>%
                group_by(symbol, name, pop_rank) %>%
                summarize(correlation = cor(pop_diff, ret)) %>%
                ungroup() %>%
                arrange(correlation) %>%
                mutate(cor_rank = row_number())

export_to_excel(to_plot, 
                outfile = paste0(out_path, "/robintrack_correlations.xlsx"),
                sheet = "raw_corr",
                new_file = 1,
                fancy_formatting = 0)

# Plot rank vs. corr
file_path <- paste0(out_path, "/rank_vs_cor.jpeg")
source_string <- paste0("Source:  Robintrack, YahooFinance (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Only shows Robintrack data for top 200 stocks as of 08/11/2020.  ",
                               "Correlation shown is between 1-day change in number of Robinhood users holding and the 1-day price return starting as of 02/19/2020."),
                        width =85)

# Plot the results
plot <- ggplot(to_plot, aes(x = pop_rank, y = correlation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(-1, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Robinhood Popularity Rank vs. Correlation")) +
  labs(x = "Popularity Rank" , y = "Correlation",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot rank vs. corr
file_path <- paste0(out_path, "/rank_vs_cor_ordered.jpeg")

text_labels <- to_plot %>%
                filter(symbol %in% c("KODK", "NKLA", "MRNA", "HTZ", "AMZN", "TSLA", "AAPL", "GOOG", "SBUX")) %>%
                mutate(label = ifelse(name == "Alphabet Class C", "Google", name),
                       hjust = case_when(
                         symbol %in% c("KODK", "NKLA", "HTZ", "TSLA")  ~ 1.5,
                         symbol %in% c("SBUX") ~ 1.0,
                         symbol %in% c("MRNA", "GOOG") ~ -0.4,
                         TRUE ~ 0
                       ),
                       vjust = case_when(
                         symbol %in% c("AMZN") ~ -2.5,
                         symbol %in% c("AAPL") ~ -1.5,
                         symbol %in% c("SBUX") ~ -0.9,
                         symbol %in% c("GOOG") ~ 0.4,
                         TRUE ~ 0
                       ))
                
# Plot the results
plot <- ggplot(to_plot, aes(x = cor_rank, y = correlation)) +
  geom_point() +
  geom_point(data = text_labels, aes(x=cor_rank, y = correlation), col = "red") +
  geom_text(data=text_labels, 
                  aes(x=cor_rank, 
                      y = correlation, 
                      label = label),
                  hjust = text_labels$hjust,
                  vjust = text_labels$vjust,
                  col = "red") +
  scale_y_continuous(limits = c(-1, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Least to Most Correlated Stocks\nBased on Robinhood Activity")) +
  labs(x = "Rank" , y = "Correlation",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Look at a sector summary
by_sector <- to_plot %>%
                drop_na() %>%
                 left_join(ycharts_comp) %>%
                group_by(sector) %>%
                summarize(n_companies = n(),
                  mean_cor = mean(correlation)) %>%
                ungroup()

# Do a one-day lag correlation
to_plot <- corr_summary %>%
  drop_na() %>%
  group_by(symbol, name, pop_rank) %>%
  summarize(correlation = cor(pop_diff, lag_ret)) %>%
  ungroup() %>%
  arrange(desc(correlation))

# Plot rank vs. lagged_corr
file_path <- paste0(out_path, "/rank_vs_cor_lag_ret.jpeg")
note_string <- str_wrap(paste0("Note: Only shows Robintrack data for top 200 stocks as of 08/11/2020.  ",
                               "Correlation shown is between 1-day change in number of Robinhood users holding and the prior day's price return starting as of 02/19/2020."),
                        width =85)

# Plot the results
plot <- ggplot(to_plot, aes(x = pop_rank, y = correlation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(-1, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Robinhood Popularity Rank vs.\nCorrelation with Prior-Day's Return")) +
  labs(x = "Popularity Rank" , y = "Correlation",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Do a one-day lag correlation
to_plot <- corr_summary %>%
  drop_na() %>%
  group_by(symbol, name, pop_rank) %>%
  summarize(correlation = cor(lag_pop, ret)) %>%
  ungroup() %>%
  arrange(desc(correlation))

# Plot rank vs. lagged_corr
file_path <- paste0(out_path, "/rank_vs_cor_lag_pop.jpeg")
note_string <- str_wrap(paste0("Note: Only shows Robintrack data for top 200 stocks as of 08/11/2020.  ",
                               "Correlation shown is between 1-day change in number of Robinhood users holding from prior day and the current day's price return starting as of 02/19/2020."),
                        width =85)

# Plot the results
plot <- ggplot(to_plot, aes(x = pop_rank, y = correlation)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(-1, 1)) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Robinhood Popularity Rank vs.\nCorrelation with Prior-Day's Popularity Change")) +
  labs(x = "Popularity Rank" , y = "Correlation",
       caption = paste0("\n", source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #

  
