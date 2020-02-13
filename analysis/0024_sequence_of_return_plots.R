cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(stringr)
library(ggrepel)

folder_name <- "0024_sequence_of_return_plots"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

# Define the number of years and the level of good and bad returns
n_years        <- 20
n_bad_years    <- 10
annual_savings <- 5000

good_return <- 0.1
bad_return  <- -0.1

# Define the scenario vectors
scenario_1 <- c(rep(bad_return, n_bad_years), rep(good_return, n_years - n_bad_years))
scenario_2 <- c(rep(good_return, n_years - n_bad_years), rep(bad_return, n_bad_years))

value_path <- data.frame(matrix(NA, nrow = n_years, ncol = 0))

for (i in 1:n_years){
  value_path[i, "year"] <- i
  if(i == 1){
    value_path[i, "Negative Returns Early"] <- annual_savings
    value_path[i, "Negative Returns Later"] <- annual_savings
  } else {
    value_path[i, "Negative Returns Early"] <- annual_savings + (value_path[(i - 1), "Negative Returns Early"] * (1 + scenario_1[i]))
    value_path[i, "Negative Returns Later"] <- annual_savings + (value_path[(i - 1), "Negative Returns Later"] * (1 + scenario_2[i]))
  }
}

value_path <- gather(value_path, scenario, value, -year)
  
for (i in 1:n_years){      
  to_plot <- filter(value_path, year <= i)
  
  # Set the file_path based on the function input 
  if (i < 10){
    file_path = paste0(exportdir, "0024_sequence_of_return_plots/plot-0",i ,".jpeg")
  } else{
    file_path = paste0(exportdir, "0024_sequence_of_return_plots/plot-",i ,".jpeg")
  }
  
  # Create title with ticker in subtitle
  top_title <- paste0("Negative Returns Have A Larger Impact\nLater in Life")
  
  if (i < (n_bad_years + 1)){
  # Create the plot object
  plot <- ggplot(to_plot, aes(x = year, y = value)) +
    geom_line() +
    geom_point(data=filter(to_plot, year == i, scenario == "Negative Returns Early"), col = "red") +
    geom_point(data=filter(to_plot, year == i, scenario == "Negative Returns Later"), col = "green") +
    geom_text_repel(data=filter(to_plot, year == i, scenario == "Negative Returns Early"),
                    aes(x = year,
                        y = value,
                        label = paste0("-", abs(scenario_1[i])*100, "%"),
                        family = "my_font"),
                    col = "red"
                    ) +
    geom_text_repel(data=filter(to_plot, year == i, scenario == "Negative Returns Later"),
                    aes(x = year,
                        y = value,
                        label = paste0("+", abs(scenario_2[i])*100, "%"),
                        family = "my_font"),
                    col = "green"
                    ) +
    geom_vline(xintercept = 10, linetype =  "dashed", col = "black") +
    facet_grid(~scenario) +
    ggtitle(top_title) +
    guides(fill = FALSE) +
    of_dollars_and_data_theme +
    scale_y_continuous(limits = c(0, 200000), label = dollar) +
    scale_x_continuous(limits = c(1, n_years)) +
    labs(x = paste0("Year ", i), y = "Total Portfolio Value")
  } else {
    plot <- ggplot(to_plot, aes(x = year, y = value)) +
      geom_line() +
      geom_point(data=filter(to_plot, year == i, scenario == "Negative Returns Early"), col = "green") +
      geom_point(data=filter(to_plot, year == i, scenario == "Negative Returns Later"), col = "red") +
      geom_text_repel(data=filter(to_plot, year == i, scenario == "Negative Returns Early"),
                      aes(x = year,
                          y = value,
                          label = paste0("+", abs(scenario_1[i])*100, "%"),
                          family = "my_font"),
                      col = "green",
                      nudge_y = 5000,
                      max.iter = 1
      ) +
      geom_text_repel(data=filter(to_plot, year == i, scenario == "Negative Returns Later"),
                      aes(x = year,
                          y = value,
                          label = paste0("-", abs(scenario_2[i])*100, "%"),
                          family = "my_font"),
                      col = "red",
                      nudge_y = 5000,
                      max.iter = 1
      ) +
      geom_vline(xintercept = 10, linetype =  "dashed", col = "black") +
      facet_grid(~scenario) +
      ggtitle(top_title) +
      guides(fill = FALSE) +
      of_dollars_and_data_theme +
      scale_y_continuous(limits = c(0, 200000), label = dollar) +
      scale_x_continuous(limits = c(1, n_years)) +
      labs(x = paste0("Year ", i), y = "Total Portfolio Value")
  }
  
  # Turn plot into a gtable for adding text grobs
  my_gtable   <- ggplot_gtable(ggplot_build(plot))
  
  source_string <- "Source: Simulated returns (OfDollarsAndData.com)"
  note_string   <- paste0("Note:  Assumes $",  
                          formatC(as.numeric(annual_savings), format="f", digits=0, big.mark=",")
                          ," of annual savings over a ", n_years, " year period.") 
  
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

create_gif(path = out_path,
           file_stub = paste0("*.jpeg"),
           speed_milliseconds = 50,
           out_name = paste0("_gif_all_sequence_plots24.gif")
)

# ############################  End  ################################## #

  
