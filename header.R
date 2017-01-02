# Set dataroot location
dataroot <- "C:/Users/Nick/data/of-dollars-and-data/"

# Set dataset libraries
localdir <- paste0(dataroot, "datasets/local/")

# Set the import/export directories
importdir <- paste0(dataroot, "import/")
exportdir <- paste0(dataroot, "export/")

# Set the programroot location
programroot <- "~/git/of-dollars-and-data/"

# Set options  
# This option is used to prevent strings from being imported as factors
options(StringsAsFactors=FALSE)