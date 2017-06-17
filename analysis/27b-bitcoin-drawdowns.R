cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/nmaggiulli/git/of-dollars-and-data/header.R"))

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

bcoin <- readRDS(paste0(localdir, "27-quandl-bitcoin.Rds"))

start_date <- min(bcoin$date)
end_date   <- max(bcoin$date)

# Create function to calculate the drawdowns over time
drawdown_path <- function(vp){
  dd      <- data.frame(date = as.Date(1:nrow(vp), origin=Sys.Date()), pct = numeric(nrow(vp)))
  loc_max <- 0
  for (i in 1:(nrow(vp))){
    if (vp[i, 2] < loc_max & i != 1){
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- vp[i, 2]/loc_max - 1
    } else{
      dd[i, 1] <- vp[i, 1]
      dd[i, 2] <- 0
      loc_max  <- vp[i, 2]
    }
  }
  return(dd)
}

date_sequence <- seq(as.Date("2010/09/01"), as.Date("2017/06/17"), "3 months")

date_sequence <- c(date_sequence, rep(date_sequence[length(date_sequence)], 5))

for (i in 1:length(date_sequence)){
  filtered <- filter(bcoin, date < date_sequence[i])

  to_plot  <- drawdown_path(filtered)
  
  if (i < 10){
    i_string <- paste0("0",i)
  } else{
    i_string <- i
  }

  # Set the file_path based on the function input 
  file_path = paste0(exportdir, "27b-quandl-bitcoin-drawdowns/drawdowns-bitcoin-", i_string,".jpeg")

  # Create title with ticker in subtitle
  top_title <- paste0("Bitcoin Has Experienced Multiple\nDrawdowns of Over 50%")
  
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = date, y = pct)) +
    geom_area(fill = "red") +
    ggtitle(top_title) +
    guides(fill = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(label = percent, limits = c(-1, 0)) +
    scale_x_date(date_labels = "%Y", limits = c(start_date, end_date), date_breaks = "1 year") +
    labs(x = "Year", y = "Percentage of Value Lost")
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source:  Quandl, https://blockchain.info (OfDollarsAndData.com)"
  note_string <- paste0("Note:  Drawdowns are based on the USD price for Bitcoin.") 
  
  # Make the source and note text grobs
  source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                          gp =gpar(fontfamily = "my_font", fontsize = 8))
  
  # Add the text grobs to the bototm of the gtable
  my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
  my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)
  
  # Save the plot  
  ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm") 
}

# Instead of creating these images as a GIF in R, do it in Bash
# I use Git Bash + magick because this is way faster than creating the GIF in R
# After navigating to the correct folder, use this command:
#
# magick convert -delay 20 loop -0 *.jpeg all_plots.gif

# ############################  End  ################################## #