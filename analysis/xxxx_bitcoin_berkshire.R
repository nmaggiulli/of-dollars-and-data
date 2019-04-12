cat("\014") # Clear your console
rm(list = ls()) #clear your environment

# Import the Bitcoin data to ensure we have the most updated Orion data
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/import/0027_import_quandl_bitcoin.R")))
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(quantmod)
library(ggjoy)
library(tidyr)
library(readxl)
library(dplyr)

folder_name <- "xxxx_bitcoin_berkshire"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bcoin <- readRDS(paste0(localdir, "0027_quandl_bitcoin.Rds")) %>%
  mutate(day = day(date)) %>%
  rename(`Bitcoin` = value) %>%
  select(date, `Bitcoin`)

last_day <- Sys.Date() - 1

getSymbols("BRK-A", from = paste0('2010-01-01'), to = paste0(last_day), 
           src="yahoo", periodicity = "daily")

brk <- data.frame(date = index(get("BRK-A")), 
                 get("BRK-A"), row.names=NULL) %>%
  select(date, contains("Adjust")) 

colnames(brk) <- c("date", "BRK")

first_bc <- bcoin[1, "Bitcoin"]
first_brk <- brk[1, "BRK"]

# Combine data
to_plot <- brk %>%
        left_join(bcoin) %>%
        gather(key=key, value=value, -date) %>%
        mutate(value = ifelse(key=="Bitcoin", value/first_bc, value/first_brk))

# Plot lines
file_path <- paste0(out_path, "/bc_v_brk.jpeg")

plot <- ggplot(to_plot, aes(x=date, y=value, col = key)) +
          geom_line() +
          scale_color_discrete(guide = FALSE) +
          scale_y_continuous(label = dollar) +
          of_dollars_and_data_theme +
          ggtitle("Bitcoin vs. Berkshire Hathaway") +
          labs(x="Date", y="Growth of $1")

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #