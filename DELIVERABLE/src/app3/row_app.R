library(tidyverse)
library(shinythemes)
library(shiny)

# Load the right-of-way data
row_data <- read_csv("./data/Strategic_Measure_Right_of_Way_Obstructions.csv", col_names = TRUE)

# Data Cleanup
row_data$Status <- as.factor(row_data$Status)
row_data$`Work Group` <- as.factor(row_data$`Work Group`)
row_data$Program <- as.factor(row_data$Program)
row_data$`Failure Class` <- as.factor(row_data$`Failure Class`)
row_data$Problem <- as.factor(row_data$Problem)
row_data$`Meets Standard` <- as.factor(row_data$`Meets Standard`)

row_data <- row_data %>% rename(duration = `Duration (hours)`)

# Convert dates from characters to POSIX dates 
row_data <- row_data %>% dplyr::mutate(`Report Date` = strptime(`Report Date`, "%m/%d/%Y"))

# Expand the criteria
row_data$`Meets Standard` <- dplyr::recode(row_data$`Meets Standard`, "1" = "Meets Standard") 
row_data$`Meets Standard` <- dplyr::recode(row_data$`Meets Standard`, "0" = "Does Not Meet Standard") 
row_data$`Meets Standard` <- dplyr::recode(row_data$`Meets Standard`, "-1" = "Excluded from Standard") 

cols_to_keep <- c("Status", "Work Group", "Program", "Failure Class", "Problem", "Meets Standard", "Report Date", "duration")
row_data <- row_data %>% select(one_of(cols_to_keep))

min_date <- min(row_data$`Report Date`)
max_date <- max(row_data$`Report Date`)
min_duration <- min(row_data$duration)
max_duration <- max(ceiling(row_data$duration))

# Define the User Interface
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("Austin Right of Way"),
                sidebarPanel(
                    selectInput(inputId = "sel_prog", label = "Choose Program", "Names" ),
                    selectInput(inputId = "sel_std", label = "Choose Standard", "Names" ),
                    dateRangeInput("dates", label = h3("Infraction Report Date"), start = min_date , end = max_date),
                    sliderInput(
                        "work_duration", label = "Hours of Work:",
                        min = 0, value = 100, max = max_duration
                    )
                ),
                mainPanel(
                    plotOutput("results_plot")
                )
) #end ui 


# Define the server 
server <- function(input, output, session) {
     
    # Populate the SelectInput drop-down list dynamically
    observe({
        updateSelectInput(session, "sel_prog", choices = levels(row_data$Program))
    }) 
    
    # Populate the SelectInput drop-down list dynamically
    observe({
        updateSelectInput(session, "sel_std", choices = levels(row_data$`Meets Standard`))
    }) 
    
    # Filter the data dynamically based on the race and dates selected 
    data <- reactive({ 
        row_data <- row_data %>% filter(Program %in% input$sel_prog & duration <= input$work_duration & `Meets Standard` %in% input$sel_std) %>% filter(`Report Date` >= input$dates[1] & `Report Date` <= input$dates[2]) 
    }) 
    
    # Plot the box-plots dynamically 
    output$results_plot <- renderPlot({
        row_data_gp <- ggplot(data = data(), aes(x = `Work Group`, y = duration)) + 
            geom_boxplot(outlier.colour = "red") + 
            theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust = 1))+
            labs(y = "Hours of Duration")
        row_data_gp 
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
