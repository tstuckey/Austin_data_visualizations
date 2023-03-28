library(tidyverse)
library(shinythemes)
library(shiny)

# Load the data
inmate_data <- read_csv("./data/High_Value_Dataset_December_2020.csv", col_names = TRUE)

# Inmate data cleanup
inmate_data$`Current Facility` <- as.factor(inmate_data$`Current Facility`)
inmate_data$Gender <- as.factor(inmate_data$Gender)
inmate_data$Race <- as.factor(inmate_data$Race)
inmate_data$County <- as.factor(inmate_data$County)
inmate_data$`Sentence (Years)` <- as.factor(inmate_data$`Sentence (Years)`)
inmate_data$`TDCJ Offense` <- as.factor(inmate_data$`TDCJ Offense`)

# Convert dates from characters to POSIX dates 
inmate_data<- inmate_data %>% dplyr::mutate(`Sentence Date`= strptime(`Sentence Date`, "%m/%d/%Y"))
inmate_data<- inmate_data %>% dplyr::mutate(`Offense Date`= strptime(`Offense Date`, "%m/%d/%Y"))

# Expand the Race abbreviations

races <- c("Asian", "White", "Hispanic", "Black", "Indian", "Other", "Unknown")
inmate_data$Race <- dplyr::recode(inmate_data$Race, "A" = races[1])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "W" = races[2])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "H" = races[3])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "B" = races[4])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "I" = races[5])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "O" = races[6])
inmate_data$Race <- dplyr::recode(inmate_data$Race, "U" = races[7])

cols_to_keep <- c("Current Facility", "Gender", "Race", "Age", "County", "Sentence (Years)","TDCJ Offense","Sentence Date","Offense Date")
inmate_data <- inmate_data %>% select(one_of(cols_to_keep))
inmate_data <- inmate_data %>% dplyr::relocate("Offense Date", .before = "Sentence Date")

min_offense_date <- min(inmate_data$`Offense Date`)
max_offense_date <- max(inmate_data$`Offense Date`)
races <- levels(inmate_data$Race)

# Define the User Interface
ui <- fluidPage(theme = shinytheme("slate"),
                titlePanel("Texas Incarcerated Inmate Insights"),
                sidebarPanel(
                    dateRangeInput("dates", label = h3("Offense Date range"), start = min_offense_date , end = max_offense_date),
                ),
                checkboxGroupInput("inCheckboxGroup", "Select Races:",
                                   races), 
                mainPanel(
                    # textOutput("text_output1"),
                    # textOutput("text_output2"),
                    plotOutput("results_plot")
                )
) #end ui 


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # output$text_output1 <- renderText({
        # input$inCheckboxGroup 
        # })
    # output$text_output2 <- renderText({
        # paste(format(input$dates[1]), format(input$dates[2]))
        # })
    
    # Filter the data dynamically based on the race and dates selected 
    data <- reactive({ 
        inmate_data <- inmate_data %>% filter(Race %in% input$inCheckboxGroup) %>% filter(`Offense Date` >= input$dates[1] & `Offense Date` <= input$dates[2])
    }) 
    
    # Plot dynamically
    output$results_plot <- renderPlot({
        gp1 <- ggplot(data = data(), aes(x = Age, fill = Race)) + geom_bar() + labs(title = paste("Inmate Distribution by Age \n Between",input$dates[1], "and", input$dates[2]))  
        gp1 
    }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
