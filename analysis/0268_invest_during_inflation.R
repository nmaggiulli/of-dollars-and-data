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
library(ggjoy)
library(tidyverse)

folder_name <- "0268_invest_during_inflation"
out_path <- paste0(exportdir, folder_name)
dir.create(file.path(paste0(out_path)), showWarnings = FALSE)

########################## Start Program Here ######################### #

bv_returns <- read_csv(paste0(importdir, "0006_bullion_vault_asset_returns/asset-returns-bullion-vault-1976.csv")) %>%
                mutate(year = as.Date(year, format = "%m/%d/%y")) %>%
                gather(-year, key=key, value=value)

min_year <- min(bv_returns$year)
max_year <- max(bv_returns$year)

bv_no_cpi <- bv_returns %>%
              filter(key != "CPI")

cpi <- bv_returns %>%
  filter(key == "CPI") %>%
  rename(cpi = value) %>%
  select(-key)


limit <- 0.2

all_data <- bv_no_cpi %>%
            left_join(cpi)

to_plot <- all_data %>%
             mutate(value_capped = case_when(value > limit ~ limit,
                                      value < -limit ~ -limit,
                                      TRUE ~ value),
                      value_real = case_when(value - cpi > limit ~ limit,
                                             value - cpi < -limit ~ -limit,
                                             TRUE ~ value - cpi)) 

file_path <- paste0(out_path, "/asset_real_return_grid.jpeg")
source_string <- paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Returns are adjusted for inflation. ",
                               "Returns with a magnitude greater than ", 100*limit, "% have been capped at ", 100*limit, "%."),
                        width = 85)

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = year, y = value_real, col = key, fill = key)) +
  geom_bar(stat = "identity") +
  facet_wrap(~key) +
  ggtitle(paste0("Real Asset Class Returns\n", format.Date(min_year, "%Y"), "-", format.Date(max_year, "%Y"))) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-limit, limit)) +
  scale_x_date(date_breaks = "10 years", date_labels = "%y",
               limits = c(min_year, max_year)) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  of_dollars_and_data_theme +
  labs(x = "Year" , y = "Annual Real Return (%)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

file_path <- paste0(out_path, "/asset_vs_cpi_grid.jpeg")
source_string <- paste0("Source: BullionVault, ", min_year, "-", max_year, " (OfDollarsAndData.com)")
note_string <- str_wrap(paste0("Note: Returns are not adjusted for inflation. ",
                               "Returns with a magnitude greater than ", 100*limit, "% have been capped at ", 100*limit, "%."),
                        width = 85)

# Plot the returns to show how much they change over time
plot <- ggplot(data = to_plot, aes(x = cpi, y = value_capped, col = key, fill = key)) +
  geom_point() +
  facet_wrap(~key) +
  ggtitle(paste0("Asset Class Return vs. CPI\n", format.Date(min_year, "%Y"), "-", format.Date(max_year, "%Y"))) +
  scale_x_continuous(label = percent_format(accuracy = 1)) +
  scale_y_continuous(label = percent_format(accuracy = 1), limits = c(-limit, limit)) +
  scale_color_discrete(guide = FALSE) +
  scale_fill_discrete(guide = FALSE) +
  of_dollars_and_data_theme +
  labs(x = "CPI" , y = "Annual Return (%)",
       caption = paste0(source_string, "\n", note_string))

# Save the plot
ggsave(file_path, plot, width = 15, height = 12, units = "cm")

table1 <- all_data %>%
            mutate(value_real = value - cpi) %>%
            group_by(key) %>%
            summarise(value_real = mean(value_real)) %>%
            ungroup() %>%
            arrange(desc(value_real)) %>%
  rename(`Asset Class` = key,
         `Real Return` = value_real)

export_to_excel(df = table1,
                outfile = paste0(out_path, "/asset_class_real_returns_bv.xlsx"),
                sheetname = "real_median_returns",
                new_file = 1,
                fancy_formatting = 1)
            
table2 <- all_data %>%
          mutate(value_real = value - cpi) %>%
          filter(cpi > 0.04) %>%
          group_by(key) %>%
            summarise(n_obs = n(),
              value_real_high_cpi = quantile(value_real, probs = 0.5)) %>%
            ungroup() %>%
            arrange(desc(value_real_high_cpi)) %>%
            select(-n_obs) %>%
            rename(`Asset Class` = key,
                   `Real Return` = value_real_high_cpi)

export_to_excel(df = table2,
                outfile = paste0(out_path, "/asset_class_real_returns_bv.xlsx"),
                sheetname = "real_returns_cpi_above_4pct",
                new_file = 0,
                fancy_formatting = 1)

# ############################  End  ################################## #