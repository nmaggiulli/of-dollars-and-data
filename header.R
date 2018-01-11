# Set dataroot location
dataroot <- "/Users/nmaggiulli/data/of-dollars-and-data/"

# Set dataset libraries
localdir <- paste0(dataroot, "datasets/")

# Set the import/export directories
importdir <- paste0(dataroot, "import/")
exportdir <- paste0(dataroot, "export/")

# Set the programroot location
programroot <- "~/git/of-dollars-and-data/"

# Set options  
# This option is used to prevent strings from being imported as factors
options(StringsAsFactors=FALSE)

# Set my font for MAC (just download and install OTF fonts then use quartzFonts)
quartzFonts(my_font = quartzFont(c("Libre Baskerville", 
                                 "Libre Baskerville Bold", 
                                 "Libre Baskerville Italic",
                                 "Libre Baskerville Bold")))
require(ggplot2)

# Make a theme that matches the OfDollarsAndData.com blog
of_dollars_and_data_theme <- theme(
                  plot.title       = element_text(family = "my_font", size = 14, face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0)),
                  axis.title.y     = element_text(face = "bold", size = 10, family = "my_font", margin = margin(0, 10, 0, 0)),
                  axis.text.y      = element_text(color = "black"), 
                  axis.ticks.y     = element_line(color = "black"),
                  axis.text.x      = element_text(color = "black"),
                  axis.ticks.x     = element_line(color = "black"),
                  axis.title.x     = element_text(face = "bold", size = 10, family = "my_font", margin = margin(10, 0, 0, 0)),
                  axis.line.x      = element_line(color = "black"),
                  axis.line.y      = element_line(color = "black"),
                  legend.key       = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border     = element_blank(),
                  panel.background = element_blank(),
                  plot.caption     = element_text(hjust = 0, family = "my_font", size = 8))
