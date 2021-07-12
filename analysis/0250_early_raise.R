cat("\014") # Clear your console
rm(list = ls()) #clear your environment

########################## Load in header file ######################## #
setwd("~/git/of_dollars_and_data")
source(file.path(paste0(getwd(),"/header.R")))

########################## Load in Libraries ########################## #

library(scales)
library(readxl)
library(lubridate)
library(ggrepel)
library(tidylog)
library(zoo)
library(tidyverse)

folder_name <- "0250_early_raise"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

starting_salary <- 50000
raise_size_singular <- 20000
raise_size <- 10000
n_years_raise_early <- 5
n_years_working <- 40

# These inputs don't affect the relative outcomes
investment_ret <- 0.05
savings_rate_early <- 0.15
savings_rate_late <- 0.15
savings_rate_constant <- 0.15
savings_rate_post_raise <- 0.30

df <- data.frame()

for(i in 1:n_years_working){
   df[i, "year"] <- i
   df[i, "salary_constant"] <- starting_salary
   
   if(i == 1){
      df[i, "port_constant"] <-  starting_salary * savings_rate_constant
      df[i, "port_change_savings"] <- starting_salary * savings_rate_constant
      df[i, "single_raise_premium"] <- 0
      df[i, "change_savings_premium"] <- 0
      
      df[i, "salary_early"] <- starting_salary
      df[i, "salary_late"] <- starting_salary
      df[i, "port_early"] <- starting_salary*savings_rate_early
      df[i, "port_late"] <- starting_salary*savings_rate_late
      df[i, "multi_raise_early_premium"] <- 0
      
      df[i, "salary_early_single"] <- starting_salary
      df[i, "salary_late_single"] <- starting_salary
      df[i, "port_early_single"] <- starting_salary*savings_rate_early
      df[i, "port_late_single"] <- starting_salary*savings_rate_late
      df[i, "single_raise_early_premium"] <- 0
   } else{
      if(i == n_years_raise_early){
         df[i, "salary_early"] <- df[(i-1), "salary_early"] + raise_size
         df[i, "salary_late"] <- df[(i-1), "salary_late"]
         
         df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"] + raise_size_singular
         df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
      } else if(i %% n_years_raise_early == 0){
         if(i == n_years_working){
            df[i, "salary_early"] <- df[(i-1), "salary_early"]
            df[i, "salary_late"] <- df[(i-1), "salary_late"] + raise_size
         } else{
            df[i, "salary_early"] <- df[(i-1), "salary_early"] + raise_size
            df[i, "salary_late"] <- df[(i-1), "salary_late"] + raise_size
         }
   
         if(i == n_years_raise_early*2){
            df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
            df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"] + raise_size_singular
         } else{
            df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
            df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
         }
      } else{
         df[i, "salary_early"] <- df[(i-1), "salary_early"]
         df[i, "salary_late"] <- df[(i-1), "salary_late"]
         
         df[i, "salary_early_single"] <- df[(i-1), "salary_early_single"]
         df[i, "salary_late_single"] <- df[(i-1), "salary_late_single"]
      }
   
      df[i, "port_constant"] <- (df[(i-1), "port_constant"] * (1 + investment_ret)) + (df[i, "salary_constant"] * savings_rate_constant) 
      
      df[i, "port_early"] <- (df[(i-1), "port_early"] * (1 + investment_ret)) + (df[i, "salary_early"] * savings_rate_early) 
      df[i, "port_late"] <- (df[(i-1), "port_late"] * (1 + investment_ret)) + (df[i, "salary_late"] * savings_rate_late) 
      df[i, "multi_raise_early_premium"] <- df[i, "port_early"]/df[i, "port_late"] - 1
      
      df[i, "port_early_single"] <- (df[(i-1), "port_early_single"] * (1 + investment_ret)) + (df[i, "salary_early_single"] * savings_rate_early) 
      df[i, "port_late_single"] <- (df[(i-1), "port_late_single"] * (1 + investment_ret)) + (df[i, "salary_late_single"] * savings_rate_late) 
      df[i, "single_raise_early_premium"] <- df[i, "port_early_single"]/df[i, "port_late_single"] - 1
      
      df[i, "single_raise_premium"] <- df[i, "port_early_single"]/df[i, "port_constant"] - 1
      
      if(i < n_years_raise_early){
         df[i, "port_change_savings"] <- df[(i-1), "port_change_savings"] * (1 + investment_ret) + (df[i, "salary_early_single"] * savings_rate_early)
      } else{
         df[i, "port_change_savings"] <- df[(i-1), "port_change_savings"] * (1 + investment_ret) + (df[i, "salary_early_single"] * savings_rate_post_raise)
      }
      
      df[i, "change_savings_premium"] <- df[i, "port_change_savings"]/df[i, "port_early"] - 1
   }
}

# Charts
source_string <- paste0("Source: Simulated data")
note_string <- str_wrap(paste0("Note: Salaries move up every ", n_years_raise_early, " years with a delay of ",
                               n_years_raise_early, " years for the Late Bloomber."),
                        width = 80)

file_path <- paste0(out_path, "/salaries_multi_raise.jpeg")

to_plot <- df %>%
            select(year, salary_early, salary_late) %>%
            rename(`Early Riser` = salary_early,
                   `Late Bloomer` = salary_late) %>%
            gather(-year, key=key, value=value)

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   scale_y_continuous(label = dollar, limits = c(0, max(to_plot$value)), breaks = seq(0, max(to_plot$value), 10000)) +
   scale_x_continuous(breaks = seq(0, max(to_plot$year), n_years_raise_early)) +
   of_dollars_and_data_theme +
   theme(legend.position = "bottom",
         legend.title = element_blank()) +
   ggtitle(paste0("Early Riser vs. Later Bloomer\nSalaries")) +
   labs(x="Year", y="Salary",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Plot of single raise
file_path <- paste0(out_path, "/salaries_single_raise.jpeg")
note_string <- str_wrap(paste0("Note: Salaries move up once after ", n_years_raise_early, " years with a delay of ",
                               n_years_raise_early, " years for the Late Bloomer."),
                        width = 80)

to_plot <- df %>%
   select(year, salary_constant, salary_early_single, salary_late_single) %>%
   rename(`Early Riser` = salary_early_single,
          `Late Bloomer` = salary_late_single,
          `No Raise` = salary_constant) %>%
   gather(-year, key=key, value=value)

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   scale_y_continuous(label = dollar, limits = c(0, max(to_plot$value)), breaks = seq(0, max(to_plot$value), 10000)) +
   scale_x_continuous(breaks = seq(0, max(to_plot$year), n_years_raise_early)) +
   of_dollars_and_data_theme +
   theme(legend.position = "bottom",
         legend.title = element_blank()) +
   ggtitle(paste0("Single Raise vs. No Raise\nSalaries")) +
   labs(x="Year", y="Salary",
        caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")


# Portfolios
file_path <- paste0(out_path, "/portfolios_single_vs_no_raise.jpeg")

to_plot <- df %>%
   select(year, port_early_single, port_late_single, port_constant) %>%
   rename(`Early Riser` = port_early_single,
          `Late Bloomer` = port_late_single,
          `No Raise` = port_constant) %>%
   gather(-year, key=key, value=value)

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   scale_y_continuous(label = dollar, limits = c(0, max(to_plot$value))) +
   scale_x_continuous(breaks = seq(0, max(to_plot$year), n_years_raise_early)) +
   of_dollars_and_data_theme +
   theme(legend.position = "bottom",
         legend.title = element_blank()) +
   ggtitle(paste0("Single Raise vs. No Raise\nPortfolio Values")) +
   labs(x="Year", y="Portfolio Value",
        caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Change savings rate
file_path <- paste0(out_path, "/portfolios_change_savings.jpeg")

to_plot <- df %>%
   select(year, port_early, port_change_savings) %>%
   rename(`Constant Savings Rate` = port_early,
          `Increased Savings Rate` = port_change_savings) %>%
   gather(-year, key=key, value=value)

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   scale_y_continuous(label = dollar, limits = c(0, max(to_plot$value))) +
   scale_x_continuous(breaks = seq(0, max(to_plot$year), n_years_raise_early)) +
   of_dollars_and_data_theme +
   theme(legend.position = "bottom",
         legend.title = element_blank()) +
   ggtitle(paste0("Portfolio Values by Change in Savings Rate")) +
   labs(x="Year", y="Portfolio Value",
        caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# Change savings rate
file_path <- paste0(out_path, "/salaries_change_savings.jpeg")

to_plot <- df %>%
   select(year, salary_early, salary_early_single) %>%
   rename(`Constant Savings Rate` = salary_early,
          `Increased Savings Rate` = salary_early_single) %>%
   gather(-year, key=key, value=value)

plot <- ggplot(to_plot, aes(x= year, y=value, col = key)) +
   geom_line() +
   scale_y_continuous(label = dollar, limits = c(0, max(to_plot$value)), breaks = seq(0, max(to_plot$value), 10000)) +
   scale_x_continuous(breaks = seq(0, max(to_plot$year), n_years_raise_early)) +
   of_dollars_and_data_theme +
   theme(legend.position = "bottom",
         legend.title = element_blank()) +
   ggtitle(paste0("Salaries Based on Savings Rate Behavior")) +
   labs(x="Year", y="Salary",
        caption = paste0(source_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

# ############################  End  ################################## #