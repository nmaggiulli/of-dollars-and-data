cat("\014") # Clear your console
rm(list = ls()) #clear your environment


########################## Load in Libraries ########################## #

library(scales)
library(lubridate)
library(quantmod)
library(shiny)
library(tidyverse)

########################## Start Program Here ######################### #

quartzFonts(my_font = quartzFont(c("Libre Baskerville", 
                                   "Libre Baskerville Bold", 
                                   "Libre Baskerville Italic",
                                   "Libre Baskerville Bold")))

of_dollars_and_data_theme <- theme(
  plot.title       = element_text(family = "my_font", size = 14, face = "bold", hjust = 0.5, margin = ggplot2::margin(0, 0, 10, 0)),
  axis.title.y     = element_text(face = "bold", size = 10, family = "my_font", margin = ggplot2::margin(0, 10, 0, 0)),
  axis.text.y      = element_text(color = "black"), 
  axis.ticks.y     = element_line(color = "black"),
  axis.text.x      = element_text(color = "black"),
  axis.ticks.x     = element_line(color = "black"),
  axis.title.x     = element_text(face = "bold", size = 10, family = "my_font", margin = ggplot2::margin(10, 0, 0, 0)),
  axis.line.x      = element_line(color = "black"),
  axis.line.y      = element_line(color = "black"),
  legend.key       = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border     = element_blank(),
  panel.background = element_blank(),
  plot.caption     = element_text(hjust = 0, family = "my_font", size = 8))


# Shiny App
ui <- fluidPage(
  titlePanel("CEF Data Pull and Plot"),
  plotOutput("premium_discount"),
  h1(),
  fluidRow(
    column(3,
           textInput("ticker", paste0("Ticker:"), 
                     value = "GIM")
    ),
    column(3,
           dateInput("start_dt", paste0("Start Date:"), 
                     value = as.Date("1999-03-04"), 
                     format ="mm/dd/yyyy")
    ),
    column(3,
           dateInput("end_dt", paste0("End Date:"), 
                     value = as.Date(Sys.Date()-1), 
                     format ="mm/dd/yyyy")
    )
  ),
  plotOutput("annualized_ret_plot")
)

server <- function(input, output) {
  filtered <- reactive({
    tkr <- toupper(input$ticker)
    getSymbols(tkr, from = input$start_dt, to = input$end_dt, 
               src="yahoo", periodicity = "daily") 
    
    xticker <- paste0("X", tkr, "X")
    
    getSymbols(xticker, from = input$start_dt, to = input$end_dt, 
               src="yahoo", periodicity = "daily") 
    
    x_raw <- data.frame(date=index(get(xticker)), coredata(get(xticker))) %>%
      rename_(.dots = setNames(paste0(xticker, ".Close"), "x_close")) %>%
      select(date, x_close)
    
    raw <- data.frame(date=index(get(tkr)), coredata(get(tkr))) %>%
      rename_(.dots = setNames(paste0(tkr, ".Close"), "close")) %>%
      rename_(.dots = setNames(paste0(tkr, ".Adjusted"), "adj")) %>%
      left_join(x_raw) %>%
      select(date, close, adj, x_close) 
    
    df <- raw %>%
      mutate(premium_discount = close/x_close - 1,
             bucket = case_when(
               premium_discount < -0.05 ~ "<-5%",
               premium_discount < 0 ~ "-5% to 0%",
               premium_discount < 0.05 ~ "0% to 5%",
               TRUE ~ ">5%"
             ),
             bucket_order = case_when(bucket == "<-5%" ~ 1,
                                      bucket == "-5% to 0%" ~ 2,
                                      bucket == "0% to 5%" ~ 3,
                                      TRUE ~ 4),
             next_day_ret = lead(adj)/adj)
  })
  
  output$annualized_ret_plot <- renderPlot({
    if (is.null(filtered())) {
      return()
    }
    
    all <- filtered() %>%
      summarise(count = n(),
                sumproduct = prod(next_day_ret, na.rm = TRUE),
                ret_ann = sumproduct^(252/count)-1) %>%
      mutate(bucket = "All")
    
    final_results <-  filtered() %>%
      group_by(bucket, bucket_order) %>%
      summarise(count = n(),
                sumproduct = prod(next_day_ret, na.rm = TRUE),
                ret_ann = sumproduct^(252/count)-1) %>%
      ungroup() %>%
      arrange(bucket_order) %>%
      select(bucket, ret_ann, sumproduct, count) %>%
      bind_rows(all)
    
    plot <- ggplot(final_results, aes(x=factor(bucket, levels = c("<-5%", "-5% to 0%", "0% to 5%", ">5%", "All")), y=ret_ann)) +
      geom_bar(stat="identity", fill = "blue") +
      geom_hline(yintercept = 0, linetype = "solid") +
      scale_y_continuous(label = percent) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Annualized Return")) +
      labs(x = "Bucket", y="Annualized Return")
    
    return(plot)
  })
  
  output$premium_discount <- renderPlot({
    ggplot(filtered(), aes(x=date, y=premium_discount)) +
      geom_line(col = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      scale_y_continuous(label = percent) +
      of_dollars_and_data_theme +
      ggtitle(paste0("Premium / Discount to NAV")) +
      labs(x = "Date", y="Premium / Discount")
  })
}

shinyApp(ui = ui, server = server)


# ############################  End  ################################## #
