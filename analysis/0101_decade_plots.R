cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(lubridate)
library(scales)
library(tidyverse)

folder_name <- "0101_decade_plots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Read in data for individual stocks and sp500 Shiller data
sp500_ret_pe    <- readRDS(paste0(localdir, "0009_sp500_ret_pe.Rds")) %>%
                    mutate(decade = as.factor(year(date) - year(date) %% 10),
                           ret = price_plus_div/lag(price_plus_div) - 1)
                    
for (i in 1:nrow(sp500_ret_pe)){
  if(i >= 10){
    sp500_ret_pe[i, "ma"] <- mean(pull(sp500_ret_pe[((i-9):i), "price_plus_div"]))
    
    if (i > 10){
      if(sp500_ret_pe[i-1, "price_plus_div"] > sp500_ret_pe[i-1, "ma"]){
        sp500_ret_pe[i, "above"] <- "Above MA"
      } else{
        sp500_ret_pe[i, "above"] <- "Below MA"
      }
    }
  } else{
    sp500_ret_pe[i, "ma"] <- NA
    sp500_ret_pe[i, "above"] <- NA
  }
}

to_plot <- sp500_ret_pe %>%
            filter(date >= "1949-12-31")

ggplot(to_plot, aes(x=decade, y=ret, fill=decade)) +
  geom_dotplot(binaxis='y', dotsize = 1, binwidth = 0.005) +
  facet_wrap(~above) +
  scale_fill_discrete(guide = FALSE) +
  scale_y_continuous(label = percent) +
  of_dollars_and_data_theme +
  labs(x = "Decade", y="Monthly Return (%)")

ks.test(filter(to_plot, above=="Above MA") %>% pull(ret),
        filter(to_plot, above=="Below MA") %>% pull(ret))

# ############################  End  ################################## #