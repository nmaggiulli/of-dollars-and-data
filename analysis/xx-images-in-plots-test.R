cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(ggplot2)
library(grid)
library(jpeg)

########################## Start Program Here ######################### #

set.seed(123)
d = data.frame(x=rnorm(10), y=rnorm(10))

local_path <- "/Users/nmaggiulli/data/of-dollars-and-data/file.jpg"
download.file('https://thumb1.shutterstock.com/display_pic_with_logo/1510844/153802253/stock-vector-transparent-globe-153802253.jpg',
              local_path, 
              mode = 'wb')

img <- readJPEG(local_path, native=TRUE)

g <- rasterGrob(img, interpolate=TRUE)

ggplot(d, aes(x, y)) +
  annotation_custom(g, xmin=0, xmax=1, ymin=0, ymax=1) +
  geom_point()



# ############################  End  ################################## #