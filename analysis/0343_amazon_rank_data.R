cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(stringr)
library(ggrepel)
library(xtable)
library(tidyverse)

folder_name <- "0343_amazon_rank_data"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

to_plot <- read_excel(paste0(importdir, "/0343_book_publishing/amazon_rank_data.xlsx"))

file_path <- paste0(out_path, "/amazon_rank_vs_sales.jpeg")
source_string <- str_wrap(paste0("Source: https://www.tckpublishing.com/ (OfDollarsAndData.com)"),
                          width = 85)
note_string <- str_wrap(paste0("Note: Rank shown estimates the daily U.S. sales of paperback/hardcover books on Amazon.com."),
                        width = 80)

plot <- ggplot(to_plot, aes(x=amazon_rank, y=copies_sold)) +
  geom_point() +
  scale_y_continuous(label = comma, breaks = seq(0, 600, 100)) +
  scale_x_continuous(label = comma) +
  of_dollars_and_data_theme +
  ggtitle(paste0("Paperback/Hardcover Daily Sales\nby Amazon Rank")) +
  labs(x = "Amazon Rank" , y = "Daily Copies Sold",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

summary <- to_plot %>%
            rename(`Amazon Book Rank` = amazon_rank,
                   `Daily Sales` = copies_sold) %>%
            mutate(`Amazon Book Rank` =  formatC(`Amazon Book Rank`, digits = 0, format = "f", big.mark = ","),
                   `Daily Sales` =  formatC(`Daily Sales`, digits = 0, format = "f", big.mark = ","))

print(xtable(summary), 
      include.rownames=FALSE,
      type="html", 
      file=paste0(out_path, "/amazon_sales_by_rank.html"))

# ############################  End  ################################## #