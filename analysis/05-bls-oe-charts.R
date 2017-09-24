cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of-dollars-and-data")
source(file.path(paste0(getwd(),"/header.R")))

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
library(plotly)

########################## Start Program Here ######################### #

# Plotly credentials 
py <- plotly("ofdollarsanddata",as.character(plotly_api_key))

# Load data fom local library
bls_oe <- readRDS(paste0(localdir, "05-bls-oe.Rds"))

# Filter the data to the national level and for hourly pay percentiles
bls_oe_filtered <- filter(bls_oe, areatype_name == "National",
                        datatype_name %in% c("Hourly 10th percentile wage", 
                                             "Hourly 25th percentile wage",
                                             "Hourly median wage",
                                             "Hourly 75th percentile wage",
                                             "Hourly 90th percentile wage"),
                        industry_name == "Cross-industry, Private Ownership Only",
                        str_trim(value) !=  "-") %>%
                        select(year, value, footnote_codes, area_name, areatype_name,
                               industry_name, occupation_name, datatype_name)

# Treat the datatype variable as a factor
bls_oe_filtered$datatype_name <- factor(bls_oe_filtered$datatype_name,
                                         levels = c("Hourly 10th percentile wage", 
                                                    "Hourly 25th percentile wage", 
                                                    "Hourly median wage",
                                                    "Hourly 75th percentile wage",
                                                    "Hourly 90th percentile wage"))

bls_oe_filtered$value <- as.numeric(as.character(bls_oe_filtered$value))

# Remove pure duplicates
bls_oe_long <- distinct(bls_oe_filtered) %>%
                    unite(group, industry_name, occupation_name, datatype_name)

# Choose the lower "value" for any duplicates remaining             
bls_oe_long <- bls_oe_long[order(bls_oe_long$group, abs(bls_oe_long$value)), ]
bls_oe_long     <- bls_oe_long[!duplicated(bls_oe_long$group),]

# Recreate the original vars before dedupping
bls_oe_long     <- separate(bls_oe_long, sep = "_", group, c("industry_name", "occupation_name", "datatype_name"))

# Turn the long dataset into a wide dataset
# Also calculate the percentage diff between the top and bottom percentiles
bls_oe_wide <-  spread(bls_oe_long, datatype_name, value) %>%
                mutate(pct75_10_diff = `Hourly 75th percentile wage` / `Hourly 10th percentile wage` - 1)

# Remove those without the relevant statistics  
to_plot     <-  filter(bls_oe_wide, !is.na(pct75_10_diff) & !is.na(`Hourly median wage`)) %>%
                arrange(pct75_10_diff)

# Define the y_unit for the y-axis dynamically
y_unit <- 10^ceiling(min(log10(abs(max(to_plot$pct75_10_diff))), log10(abs(min(to_plot$pct75_10_diff)))))

# Function to find a rounded max/min based on the specifications of y_unit
create_max_min <- function(x, unit, ceilfloor) {
  ceilfloor(x/unit)*unit
}

y_max <- create_max_min(max(to_plot$pct75_10_diff), y_unit, ceiling)
y_min <- create_max_min(min(to_plot$pct75_10_diff), y_unit, floor)

# If the distance between the max and min is too large, increase y_unit
# until the distance is less than 10 ticks away
while (ceiling(abs(y_max - y_min))/y_unit > 10){
  y_unit <- y_unit * 2
}

# Define a new y_max if the y_unit has changed
y_max <- create_max_min(y_max, y_unit, ceiling)

n_occupations_removed <- nrow(bls_oe_wide) - nrow(to_plot)

# Set the file_path based on the function input 
file_path = paste0(exportdir, "05-bls-occupational-employment/bls-oe-inequality.jpeg")

top_title <- "Jobs in the Entertainment Industry\nHave More Unequal Outcomes"

# Create the plot
plot <- ggplot(to_plot, aes(x = `Hourly median wage`, y = pct75_10_diff)) +
    geom_point() +
    geom_smooth() +
  geom_text_repel(data = filter(to_plot, pct75_10_diff > 3.5), 
                  aes(`Hourly median wage`, pct75_10_diff, label = occupation_name, family = "my_font"), 
                  size = 2.4,
                  nudge_x = 4,
                  max.iter = 5000) +
    of_dollars_and_data_theme +
    scale_y_continuous(limits = c(y_min, y_max), labels = percent) +
    # I defined the x axis manually as the dynamic way was not precise enough
    scale_x_continuous(limits = c(0, 75), breaks = seq(0, 75, 15), labels = dollar) +
    ggtitle(top_title)  +
    labs(x = "Hourly Median Wage", y = "Hourly Wage Percentage Increase\nBetween the 10th and 75th Percentiles")

# Add a source and note string for the plots
source_string <- "Source:  Bureau of Labor Statistics (OfDollarsAndData.com)"
note_string   <- paste0("Note:  ", n_occupations_removed, " occupations were removed due to incomplete data.  Data shown is for 2015.") 

# Turn plot into a gtable for adding text grobs
my_gtable   <- ggplot_gtable(ggplot_build(plot))

# Make the source and note text grobs
source_grob <- textGrob(source_string, x = (unit(0.5, "strwidth", source_string) + unit(0.2, "inches")), y = unit(0.1, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))
note_grob   <- textGrob(note_string, x = (unit(0.5, "strwidth", note_string) + unit(0.2, "inches")), y = unit(0.15, "inches"),
                        gp =gpar(fontfamily = "my_font", fontsize = 8))

# Add the text grobs to the bototm of the gtable
my_gtable   <- arrangeGrob(my_gtable, bottom = source_grob)
my_gtable   <- arrangeGrob(my_gtable, bottom = note_grob)

# Save the gtable
ggsave(file_path, my_gtable, width = 15, height = 12, units = "cm")

# Add a post on plotly for more interactivity
# Create an x-axis for the plot_ly plot
xaxis <- list(title = "Hourly Median Wage",
              showline = TRUE,
              showgrid = TRUE,
              showticklabels = TRUE,
              linecolor = 'rgb(204, 204, 204)',
              linewidth = 2,
              dtick = 15,
              autotick = FALSE,
              ticks = 'outside',
              tickcolor = 'rgb(204, 204, 204)',
              tickwidth = 2,
              ticklen = 5,
              tickfont = list(family = 'Arial',
                              size = 12,
                              color = 'rgb(82, 82, 82)'))

# Create an y-axis for the plot_ly plot
yaxis <- list(title = "Hourly Wage % Increase\nBetween the 75th and 10th Percentiles",
              showgrid = TRUE,
              zeroline = TRUE,
              showline = TRUE,
              dtick = 1,
              showticklabels = TRUE)

# Use the information above and create an interactive plot_ly plot
to_post_1 <- plot_ly(data = to_plot, x = ~`Hourly median wage`, y = ~pct75_10_diff, 
                     type = 'scatter', mode = 'markers',
                     text = ~paste('Occupation: ', occupation_name)) %>%
  layout(title = top_title, xaxis = xaxis, yaxis = yaxis)

# Post it publically on the ofdollarsanddata plotly profile
plotly_POST(x = to_post_1, filename =  "05-bls-occupational-employment", sharing =  "public")

# ############################  End  ################################## #