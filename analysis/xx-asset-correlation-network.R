cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
source(file.path("C:/Users/Nick/git/of-dollars-and-data/header.R"))

########################## Load in Libraries ########################## #

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)
library(igraph)

########################## Start Program Here ######################### #

# Read in BV data
bv <- readRDS(paste0(localdir, "06-bv-returns.Rds"))


# # Reproduce colors from ggplot
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Create years_list for looping
years_list <- seq(as.numeric(substr(min(unique(bv$year)), 1, 4)) + 4,
                  as.numeric(substr(max(unique(bv$year)), 1, 4)), 
                  1)

# Make footnote function for plot
makeFootnote <- function(footnoteText=
                           format(Sys.time(), "%d %b %Y"),
                         size= .7, color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

pdf(paste0(exportdir, "xx-bv-correlations/5-yr-correlations-1980-2015.pdf"))
for (y in years_list){
  bv_subset <- filter(bv, 
                      year > as.POSIXct(paste0(y - 5, "-12-31"), format = "%Y-%m-%d"),
                      year < as.POSIXct(paste0(y, "-12-31"), format = "%Y-%m-%d"))
  bv_num    <- as.matrix(bv_subset[, -1]) 
  cor_mat   <- cor(bv_num)
  
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
  E(g)$color      <- ifelse(E(g)$cor > 0, 
                            colorRampPalette(c("red", "dark red"))(5), 
                            colorRampPalette(c("blue", "dark blue"))(5))
  E(g)$width      <- 5*atanh(E(g)$weight)
  
  # Format vertices
  V(g)$size        <- 40
  V(g)$color       <- "grey"
  V(g)$label.color <- "black"
  V(g)$label       <- colnames(bv_num)
  
  # Do the plot
  if (y == min(years_list)){
    l <- custom.layout(g)
  }
  w <- 1
  while(length(E(g)$weight) > w-1){
    if (abs(E(g)$weight[w]) < 0.7){
      g <- delete.edges(g, w)
    } else {
      w <- w + 1
    }
  }
  plot(g, layout = l, main = paste0("5 Year Correlations\n", y))
  makeFootnote(paste0("Note:  Red lines and blue lines correspond to positive and negative correlations, respectively. Correlations below 0.7 are excluded."))
}
dev.off()

# ############################  End  ################################## #