library(tidyverse)
library(shinythemes)
library(shiny)

# Load and process the data when the application is instantiated
perf_data <- read_csv("./data/City_of_Austin_Performance_Measures.csv", col_names = TRUE)
perf_data$FY <- as.factor(perf_data$FY)
perf_data$ACTIVITY_NAME <- as.factor(perf_data$ACTIVITY_NAME)
perf_data$PROGRAM_NAME <- as.factor(perf_data$PROGRAM_NAME)
perf_data$DEPARTMENT_NAME <- as.factor(perf_data$DEPARTMENT_NAME)
perf_data$LEVEL_NAME <- as.factor(perf_data$LEVEL_NAME)
perf_data$MEASURE_TYPE_NAME <- as.factor(perf_data$MEASURE_TYPE_NAME)
perf_data$FREQUENCY_NAME <- as.factor(perf_data$FREQUENCY_NAME)
perf_data$KEY_PERFORMANCE <- as.factor(perf_data$KEY_PERFORMANCE)
perf_data$DASHBOARD_IND <- as.factor(perf_data$DASHBOARD_IND)

# Just keep the columns we need
cols_to_keep <- c("FY", "ACTIVITY_NAME", "PROGRAM_NAME", "DEPARTMENT_NAME", "LEVEL_NAME", "MEASURE_TYPE_NAME", 
                  "FREQUENCY_NAME", "KEY_PERFORMANCE", "DASHBOARD_IND", "QTR1_CURRENT_YR", "QTR2_CURRENT_YR", 
                  "QTR3_CURRENT_YR", "QTR4_CURRENT_YR")
perf_data <- perf_data %>% select(one_of(cols_to_keep))

# Define the User Interface
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("Austin Performance Measures"),
                sidebarPanel(
                    selectInput(inputId = "sel_Dept", label = "Choose Dept", "Names" ),
                ),
                mainPanel(
                    plotOutput("dept_plot"),
                    plotOutput("activity_plot")
                )
) #end ui 


# Define the server 
server <- function(input, output, session) {
    # Populate the SelectInput drop-down list dynamically
    observe({
        updateSelectInput(session, "sel_Dept", choices = levels(perf_data$DEPARTMENT_NAME))
    }) 
    
    # Filter the data dynamically based on the drop-down selection
    data <- reactive({ 
        req(input$sel_Dept) 
        perf_data <- perf_data %>% filter(DEPARTMENT_NAME %in% input$sel_Dept)
     }) 
     
    # Plot dynamically 
    output$dept_plot <- renderPlot({
        gp1 <- ggplot2::ggplot(data = data(), aes(x = DEPARTMENT_NAME)) +
            geom_bar(fill = "darkblue") +  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))+
            labs(title = "Total Count of Performance Measures by Dept", x = "Depart Name", y = "Count")
        gp1    
    })
    output$activity_plot <- renderPlot({
        gp2 <- ggplot2::ggplot(data = data(), aes(x = ACTIVITY_NAME)) +
            geom_bar(fill = "lightblue") + theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1)) +
            labs(title = paste("Breakout Count of Performance Meaasure by Activty \n in Dept",input$sel_Dept), 
                 x = paste(input$sel_Dept,"Actvities"), y = "Count")
        gp2    
    })
    
} # end server
    


# Instantiate the application 
shinyApp(ui = ui, server = server)
