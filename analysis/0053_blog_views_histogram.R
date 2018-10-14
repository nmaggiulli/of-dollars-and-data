cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(scales)
library(grid)
library(readxl)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(lubridate)
library(tidyr)
library(Quandl)
library(dplyr)

########################## Start Program Here ######################### #

# Bring in Blog views data
views <- read_excel(paste0(importdir, "0053_blog_views/blog-views-2017-12-30.xlsx")) %>%
            mutate(views = ifelse(str_detect(Views, "\\."), 
                                        str_replace(trimws(Views), "K", "00"),
                                        str_replace(trimws(Views), "K", "000")),
                   name = Name) %>%
            select(name, views)

# Fix an issue with the .9X having too many decimal places
for (i in 1:nrow(views)){
  if(nchar(views[i, "views"]) == 6 & substr(views[i, "views"], 2, 2) == "."){
    views[i, "views"] <- substr(views[i, "views"], 1, 5)
  }
}

# Convert the views to numerics
to_plot <- views %>%
              mutate(views = as.numeric(str_replace(views, "\\.", "")))

file_path <- paste0(exportdir, "0053_blog_views_histogram/blog-views-hist.jpeg")

plot <- ggplot(to_plot, aes(x = views, y = ..density..)) +
          geom_histogram(fill = "red") +
          geom_density() +
          scale_y_continuous(label = percent) +
          ggtitle(paste0("Distribution of Post Views\nOf Dollars And Data"))  +
          of_dollars_and_data_theme +
          labs(x = "Total Post Views" , y = "Density")

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

source_string <- "Source:  Medium.com (OfDollarsAndData.com)"
note_string   <- paste0("Note:  Views calculated as of 12/30/17 and excludes views from RSS feeds.")

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

# ############################  End  ################################## #