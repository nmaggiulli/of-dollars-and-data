cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(stringr)
library(ggrepel)
library(zoo)
library(Hmisc)
library(igraph)
library(tidyverse)

folder_name <- "0170_asset_correlation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

raw <- read.csv(paste0(importdir, "0170_ycharts_daily_ret/timeseries_3-15-2020.csv"), skip = 6) %>%
  rename(symbol = Symbol,
         name = Name) %>%
  select(-Metric) %>%
  gather(-symbol, -name, key=key, value=value) %>%
  mutate(year = gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\1", key, perl = TRUE),
         month =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\2", key, perl = TRUE),
         day =  gsub("X(\\d+)\\.(\\d+)\\.(\\d+)", "\\3", key, perl = TRUE),
         date = as.Date(paste0(year, "-", month, "-", day), format = "%Y-%m-%d")) %>%
  filter(symbol != "", !is.na(value)) %>%
  select(date, year, month, symbol, name, value) %>%
  arrange(symbol, date)

full_dates <- raw %>%
                group_by(date) %>%
                summarize(n_assets = n()) %>%
                ungroup() %>%
                filter(n_assets == 10) %>%
                select(date)

# Add formal names and filter out to have 6-non S&P asset classes
df <- raw %>%
        inner_join(full_dates) %>%
        mutate(formal_name = case_when(
          name == "Bloomberg Barclays US Corporate" ~ "U.S. Corp Bond",
          name == "Bloomberg Barclays US Treasury Bills 1-3 Month" ~ "3m TBill",
          name == "SPDR Gold Shares" ~ "Gold",
          name == "iShares National Muni Bond ETF" ~ "Muni Bonds",
          name == "iShares TIPS Bond ETF" ~ "U.S. TIPS",
          name == "iShares 20+ Year Treasury Bond ETF" ~ "20YR Treasuries",
          name == "Vanguard FTSE Developed Markets ETF" ~ "Int. Stocks",
          name == "Vanguard Real Estate ETF" ~ "U.S. REITs",
          name == "Vanguard FTSE Emerging Markets ETF" ~ "EM",
          TRUE ~ "S&P 500"
        )) %>%
        select(date, name, formal_name, value)

plot_all_cor <- function(start_date, end_date){
  
  tmp_sp500 <- df %>%
          filter(name == "S&P 500", date >= start_date, date <= end_date) %>%
          rename(ret_sp500 = value) %>%
          select(date, ret_sp500)
  
  tmp <- df %>%
    filter(name != "S&P 500", 
           formal_name != "U.S. TIPS", 
           formal_name != "Int. Stocks",
           formal_name != "3m TBill",
           date >= start_date, date <= end_date) %>%
    left_join(tmp_sp500)

  
  n_day_cor <- 60
  start_analysis_date <- tmp %>%
                          mutate(lead_date = lead(date, n_day_cor)) %>%
                          filter(date == min(tmp$date)) %>%
                          select(lead_date) %>%
                          distinct() %>%
                          pull(lead_date)
  
  for(i in 1:nrow(tmp)){
    dt <- tmp[i, "date"]
    if(dt < start_analysis_date){
      tmp[i, "cor"] <- NA
    } else{
      tmp[i, "cor"] <- cor(tmp[(i-n_day_cor+1):i, "value"], tmp[(i-n_day_cor+1):i, "ret_sp500"])
    }
  }
  
  to_plot <- tmp %>%
              filter(!is.na(cor)) %>%
              select(date, formal_name, cor)
  
  start_date_string <- date_to_string(start_date)
              
  file_path <- paste0(out_path, "/spx_all_cor_", start_date_string, ".jpeg")
  source_string <- paste0("Source:  YCharts (OfDollarsAndData.com)")
  note_string <- str_wrap(paste0("Note:  Correlations are run on a rolling ", n_day_cor, "-session basis.  "),
                          width = 85)
  
  plot <- ggplot(to_plot, aes(x=date, y=cor)) + 
    facet_wrap(~formal_name) +
    geom_line() +
    scale_y_continuous(label = comma) +
    scale_x_date(date_labels = "%m/%y") +
    of_dollars_and_data_theme +
    ggtitle(paste0("Rolling ", n_day_cor, "-Session Correlation with S&P 500\n",
                   format.Date(start_date, format = "%b %Y"), 
                               "-",
                   format.Date(end_date, format = "%b %Y"))) +
    labs(x = "Date" , y = "Rolling Correlation",
         caption = paste0("\n", source_string, "\n", note_string)) 
  
  # Save the plot
  ggsave(file_path, plot, width = 15, height = 12, units = "cm")
}

#Create network plots
network_plot <- function(start_date, end_date){
  
  #Convert dates to strings
  start_date_string <- date_to_string(start_date)
  end_date_string <- date_to_string(end_date)
  
  # Create PDF
  pdf(paste0(out_path, "/correlation_network_", start_date_string, "_", end_date_string, ".pdf"))
  
  asset_subset <- filter(df, 
                      date >= start_date,
                      date <= end_date,
                      formal_name != "U.S. TIPS",
                      formal_name != "3m TBill"
                      ) %>%
                      select(-name) %>%
                      spread(key=formal_name, value=value)
                    
  asset_num    <- as.matrix(asset_subset[, seq(2, ncol(asset_subset))]) 
  cor_mat   <- cor(asset_num)
  
  # Create a weighted complete graph from the correlation matrix
  g <- graph.adjacency(cor_mat, mode = "undirected", weighted = TRUE, diag = FALSE)
  
  # Chose the layout function
  custom.layout <- function(g, ...) {
    # layout.drl(g, weights = E(g)$weight, ...) # For bigger graphs
    layout.fruchterman.reingold(g, weights = E(g)$weight, ...)
  }
  
  # Format edges
  E(g)$cor        <- E(g)$weight
  E(g)$weight     <- abs(E(g)$cor)
  E(g)$width      <- 5*atanh(E(g)$weight)
  
  # Format vertices
  V(g)$size        <- 55
  V(g)$color       <- "grey"
  V(g)$label.color <- "black"
  V(g)$label       <- colnames(asset_num)
  V(g)$label.cex   <- 0.8
  
  l <- custom.layout(g)
  
  corr_limit <- 0.5
  
  # Remove edges below a certain threshold
  w <- 1
  while(length(E(g)$weight) > w - 1){
    if (abs(E(g)$weight[w]) < corr_limit){
      g <- delete.edges(g, w)
    } else {
      w <- w + 1
    }
  }
  
  # Define a color scale and bin the weight values
  n_colors        <- 4
  c_scale         <- colorRampPalette(c('blue', 'red'))(n_colors)
  c_divisions     <- seq(-1, 1, 2/n_colors)
  c_seq           <- vector(mode="numeric", length=length(E(g)$cor))
  for (c in 1:length(E(g)$cor)){
    for (v in 1:length(c_scale)){
      if (E(g)$cor[c] > c_divisions[v] & E(g)$cor[c] < c_divisions[(v+1)]){
        c_seq[c] <- v
      }
    }
  }
  
  # Set the color
  E(g)$color      <- c_scale[c_seq]
  
  # Set the layout manually (this will take a while)
  # Treasuries
  l[1, 1] <- 1
  l[1, 2] <- 1
  
  #EM
  l[2, 1] <- 0
  l[2, 2] <- -1.5
  
  # Gold
  l[3, 1] <- -1
  l[3, 2] <- 1
  
  #Int. Stocks
  l[4, 1] <- -1
  l[4, 2] <- -1
  
  # Munis
  l[5, 1] <- 0
  l[5, 2] <- 1.5
  
  #S&P 500
  l[6, 1] <- -1.5
  l[6, 2] <- 0
  
  # Corp Bond
  l[7, 1] <- 1.5
  l[7, 2] <- 0
  
  # REITs
  l[8, 1] <- 1
  l[8, 2] <- -1
  
  # Plot the network
  plot(g, layout = l, main = paste0("Correlation Between Different Assets\n", 
                                    format.Date(start_date, format = "%m/%d/%y"),
                                    "-",
                                    format.Date(end_date, format = "%m/%d/%y")))
  text(-1.75, -1.5, 
       label = paste0("Source:  YCharts (OfDollarsAndData.com)",
                      "\nNote: Correlations between -", corr_limit, " and ", corr_limit, " have been excluded.  Red lines correspond to positive correlations,\n",
                      "while blue lines correspond to negative correlations.  Thicker lines correspond to larger correlations."),
       cex = 0.75,
       adj = 0)
  
  dev.off()
}

plot_all_cor("2016-01-01", "2020-03-13")
plot_all_cor("2008-09-01", "2009-06-30")

network_plot("2014-01-01", "2017-12-31")
network_plot("2008-09-01", "2008-10-31")
network_plot("2020-02-19", "2020-03-13")




# ############################  End  ################################## #