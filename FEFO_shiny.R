# Load required libraries
library(shiny)
library(tidyverse)
library(scales)
library(DT)
library(lubridate)
library(shinyWidgets)
library(writexl)

source("FEFO_data.R")
fefo_df <- readRDS("fefo_df.rds")

# Define a function to sort choices alphabetically or numerically
sort_choices <- function(choices) {
  if (all(is.numeric(choices))) {
    return(sort(choices))
  } else {
    return(sort(choices, na.last = TRUE))
  }
}

# Sort choices for pickerInput filters
sorted_month_choices <- sort_choices(unique(fefo_df$Month))
sorted_date_choices <- sort_choices(unique(eod_fefo_report$"Report Date"))
sorted_branch_choices <- sort_choices(unique(eod_fefo_report$Branch))
sorted_sku_choices <- sort_choices(unique(eod_fefo_report$SKU))

# Define the UI
ui <- fluidPage(
  navbarPage("FEFO Non Compliance Report",
             column(12, tags$img(src = "VenturaFoodsLogo.png", height = 72, width = 500, align = "right")),  
             tabPanel("Weekly Update",
                      tabsetPanel(
                        tabPanel("Graph",
                                 fluidRow(
                                   column(12,
                                          h2("Weekly Update"),
                                          pickerInput("selected_month", "Select Month:", 
                                                      choices = sorted_month_choices,
                                                      selected = as.numeric(format(Sys.Date(), "%Y%m")),
                                                      multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE  
                                                      )),
                                          plotOutput("barplot", height = 600),
                                          h6("Green: < 10% (Meet the target)"),
                                          h6("Yellow: <= 20% & > 10%"),
                                          h6("Red: >= 20%")
                                   )
                                 )),
                        tabPanel("Data",
                                 fluidRow(
                                   column(12,
                                          h2(paste("Previous week's report: ", 
                                                   format(floor_date(Sys.Date(), "week") -6, "%m/%d/%Y"),
                                                   " (Monday) - ",
                                                   format(floor_date(Sys.Date(), "week"), "%m/%d/%Y"),
                                                   " (Sunday)")),
                                          pickerInput("selected_date", "Report Date:", choices = sorted_date_choices, selected = sorted_date_choices, multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE 
                                                      )),
                                          pickerInput("selected_branch", "Branch:", choices = sorted_branch_choices, selected = sorted_branch_choices, multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE  
                                                      )),
                                          pickerInput("selected_sku", "SKU:", choices = sorted_sku_choices, 
                                                      selected = sorted_sku_choices, multiple = TRUE,
                                                      options = list(
                                                        `actions-box` = TRUE,  
                                                        `live-search` = TRUE,
                                                        `size` = 10
                                                      )),
                                          DTOutput("summary_table"),
                                          downloadButton("downloadData", "Download Filtered Data"),
                                          downloadButton("downloadFEFO_xlsx", "Download FEFO Full Data"))
                                 ))
                      )
             ),
             tabPanel("Monthly Update",
                      fluidRow(
                        column(12,
                               h2("Monthly Update"),
                               pickerInput("selected_monthly_branch", "Select Branch:",  
                                           choices = sort_choices(unique(fefo_df$branch)),
                                           selected = sort_choices(unique(fefo_df$branch)), 
                                           multiple = TRUE,
                                           options = list(
                                             `actions-box` = TRUE 
                                           )),
                               plotOutput("monthly_barplot", height = 600),
                               h6("Green: < 10% (Meet the target)"),
                               h6("Yellow: <= 20% & > 10%"),
                               h6("Red: >= 20%"),
                               tags$br(),
                               h6("Green: Down Trend Line"),
                               h6("Red: Up Trend Line")
                        )
                      )
             )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  output$barplot <- renderPlot({
    selected_months <- input$selected_month
    filtered_data <- if ("All" %in% selected_months) {
      fefo_df
    } else {
      fefo_df %>% dplyr::filter(Month %in% selected_months)
    }
    
    avg_data <- filtered_data %>% 
      group_by(branch) %>% 
      summarise(avg_RF_Trx = mean(`RF_Trx_Cnt_ByBranch%`, na.rm = TRUE)) %>% 
      arrange(desc(avg_RF_Trx))
    
    avg_data$branch <- factor(avg_data$branch, levels = avg_data$branch)
    
    gg <- ggplot(avg_data, aes(x=branch, y=avg_RF_Trx, fill = ifelse(avg_RF_Trx < 0.1, "green", ifelse(avg_RF_Trx < 0.2, "yellow", "red")))) +
      geom_bar(stat="identity") +
      geom_hline(yintercept = 0.1, linetype="dotted", color = "black", size = 1) +
      geom_text(aes(label=sprintf("%.0f%%", 100*avg_RF_Trx)), vjust=-0.3) +
      annotate("text", x=Inf, y=0.1, label="Target 10%", hjust=1, vjust=2.5, color="blue", size=4) +
      scale_fill_manual(values=c("green"="#91cf60", "yellow"="#ffffbf", "red"="firebrick")) +
      labs(title="FEFO Non Compliance % of Total By Branch",
           x="Branch",
           y="FEFO % OF TOTAL") +
      theme_classic() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), labels = scales::percent, expand = c(0, 0)) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 20),
            axis.title.x = element_text(face = "bold", size = 14, color = "blue"),  
            axis.title.y = element_text(face = "bold", size = 14, color = "blue"),  
            axis.text.x = element_text(face = "bold", size = 14),
            axis.text.y = element_text(face = "bold", size = 14)) 
    
    
    gg
  })
  
  
  filtered_data <- reactive({
    data <- eod_fefo_report
    if ("All" %in% input$selected_date) {
      data <- data
    } else {
      data <- data %>% dplyr::filter(`Report Date` %in% input$selected_date)
    }
    if ("All" %in% input$selected_branch) {
      data <- data
    } else {
      data <- data %>% dplyr::filter(Branch %in% input$selected_branch)
    }
    if ("All" %in% input$selected_sku) {
      data <- data
    } else {
      data <- data %>% dplyr::filter(SKU %in% input$selected_sku)
    }
    return(data)
  })
  
  output$summary_table <- renderDT({
    datatable(
      transform(
        filtered_data(),
        `Std Cost` = scales::dollar(`Std Cost`),
        `Lot To Be Picked Cost Risk` = scales::dollar(`Lot To Be Picked Cost Risk`)
      ),
      rownames = FALSE,
      extensions = c("Buttons", "FixedHeader"),
      options = list(
        pageLength = 100,
        dom = "Blfrtip",
        buttons = c("copy", "csv", "excel"),
        fixedHeader = TRUE, 
        fixedColumns = list(leftColumns = 2),
        scrollX = TRUE,
        scrollY = "800px"
      )
    )
  })
  
  
  output$monthly_barplot <- renderPlot({
    selected_branch <- input$selected_monthly_branch
    monthly_data <- if ("All" %in% selected_branch) {
      fefo_df
    } else {
      fefo_df %>% dplyr::filter(branch %in% selected_branch)
    }
    
    avg_data_monthly <- monthly_data %>% 
      group_by(Month) %>% 
      summarise(avg_RF_Trx = mean(`RF_Trx_Cnt_ByBranch%`, na.rm = TRUE))
    
    # Compute the slope of the linear regression
    fit <- lm(avg_RF_Trx ~ Month, data = avg_data_monthly)
    slope <- coef(fit)[2]
    
    # Determine the color based on the slope
    trend_color <- ifelse(slope > 0, "lightcoral", "lightgreen")
    
    gg <- ggplot(avg_data_monthly, aes(x=Month, y=avg_RF_Trx)) +
      geom_bar(aes(fill = ifelse(avg_RF_Trx >= 0.2, "red", ifelse(avg_RF_Trx >= 0.1, "yellow", "green"))), 
               stat="identity", alpha=0.7, width=0.1) +
      scale_fill_manual(values = c("red" = "red", "yellow" = "yellow", "green" = "green")) +
      
      geom_text(aes(label=sprintf("%.2f%%", 100*avg_RF_Trx)), vjust=-0.3) +
      geom_hline(yintercept = 0.1, linetype="dotted", color = "black", size = 1) +
      annotate("text", x=Inf, y=0.1, label="Target 10%", hjust=1, vjust=2.5, color="blue", size=4) +
      labs(title="FEFO Non Compliance % of Total By Month",
           x="Month",
           y="FEFO % OF TOTAL") +
      theme_classic() +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1), labels = scales::percent, expand = c(0, 0)) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 20),
            axis.title.x = element_text(face = "bold", size = 14, color = "blue"),  
            axis.title.y = element_text(face = "bold", size = 14, color = "blue"),  
            axis.text.x = element_text(face = "bold", size = 14),
            axis.text.y = element_text(face = "bold", size = 14)) +
      ggplot2::geom_smooth(aes(group=1), method="lm", se=FALSE, color=trend_color, linetype="dashed")
    
    gg
    
  })
  
  # Download handler for filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    })
  





output$downloadFEFO_xlsx <- downloadHandler(
  filename = function() {
    paste("FEFO_data-", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {
    writexl::write_xlsx(fefo_df, file)
})



}


# Run the Shiny app
shinyApp(ui, server)
