cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

########################## Start Program Here ######################### #

bcoin <- readRDS(paste0(localdir, "27-quandl-bitcoin.Rds")) %>%
                filter(date > "2017-01-01")

start_date <- min(bcoin$date)
end_date   <- max(bcoin$date)

# Set the file_path based on the function input 
file_path = paste0(exportdir, "27a-quandl-bitcoin-price/bitcoin-price.jpeg")

# Create title with ticker in subtitle
top_title <- paste0("Bitcoin Has Almost Tripled in Value\nOver the Past Few Months")

# Create the plot object
plot <- ggplot(bcoin, aes(x = date, y = value)) +
  geom_line(col = "green") +
  ggtitle(top_title) +
  guides(fill = FALSE) +
  of_dollars_and_data_theme +
  scale_y_continuous(label = dollar) +
  scale_x_date(date_label = "%b %y") +
  labs(x = "Date", y = "Value of 1 Bitcoin")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Quandl, https://blockchain.info (OfDollarsAndData.com)"

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)

# Save the plot  
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 


# ############################  End  ################################## #