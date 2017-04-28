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

years_list <- 1980

pdf(paste0(exportdir, "22-bv-correlations/5-yr-correlations-1980-2015.pdf"))
for (y in years_list){
  bv_subset <- filter(bv, 
                      year > as.POSIXct(paste0(y - 5, "-12-31"), format = "%Y-%m-%d"),
                      year < as.POSIXct(paste0(y, "-12-31"), format = "%Y-%m-%d"))
  bv_num    <- as.matrix(bv_subset[, c(-1, -9)]) 
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
  E(g)$width      <- 5*atanh(E(g)$weight)
  
  # Format vertices
  V(g)$size        <- 55
  V(g)$color       <- "grey"
  V(g)$label.color <- "black"
  V(g)$label       <- colnames(bv_num)
  V(g)$label.cex   <- 0.8
  
  # Create the custom layout
  if (y == min(years_list)){
    l <- custom.layout(g)
  }
  
  # Remove edges below a certain threshold
  w <- 1
  while(length(E(g)$weight) > w - 1){
    if (abs(E(g)$weight[w]) < 0.5){
      g <- delete.edges(g, w)
    } else {
      w <- w + 1
    }
  }
  
  # Define a color scale and bin the weight values
  n_colors        <- 4
  c_scale         <- colorRampPalette(c('blue', 'red'))(n_colors)
  c_divisions     <- seq(-1, 1, 2/n_colors)
  c_seq           <- vector(mode="numeric", length=length(E(g)$cor))
  for (c in 1:length(E(g)$cor)){
    for (v in 1:length(c_scale)){
      if (E(g)$cor[c] > c_divisions[v] & E(g)$cor[c] < c_divisions[(v+1)]){
        c_seq[c] <- v
      }
    }
  }
  
  # Set the color
  E(g)$color      <- c_scale[c_seq]
  
  # Set the layout manually
  # Risky stocks/bonds
  l[1, 1] <- -1.5
  l[1, 2] <- 0
  l[2, 1] <- -1
  l[2, 2] <- -1
  l[3, 1] <- 0
  l[3, 2] <- -1.5
  l[4, 1] <- 1
  l[4, 2] <- -1
  
  # Safe assets
  l[5, 1] <- 1
  l[5, 2] <- 1
  l[6, 1] <- 1.5
  l[6, 2] <- 0
  
  # Alternatives (RE and Gold)
  l[7, 1] <- -1
  l[7, 2] <- 1
  l[8, 1] <- 0
  l[8, 2] <- 1.5
  
  plot(g, layout = l, main = paste0("Correlation Over Previous 5 Years\n", y))
  text(-1.75, -1.5, 
       label = paste0("Source:  BullionVault U.S. Asset Class Performance Data," , min(years_list)-4, "-", max(years_list)," (OfDollarsAndData.com)",
                      "\nNote: Correlations between -0.5 and 0.5 are excluded.  Red lines correspond to positive\ncorrelations, while blue lines correspond to negative correlations."),
       cex = 0.75,
       adj = 0)
}
dev.off()

# ############################  End  ################################## #