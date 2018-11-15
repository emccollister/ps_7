library(shiny)
library(tidyverse)
library(dplyr)

race <- read_rds("./race.rds")
education <- read_rds("./education.rds")
landline <- read_rds("./landline.rds")
demtakehouse <- read_rds("./genballot.rds")
women <- read_rds("./women.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Different Populations Influence on 2018 Midterm Elections"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("choice", "Select a Type of Voter",
                    choices = c("Race", "Education Level", "Landline versus Cell Phone", 
                                "Thoughts on if Dems Will Take the House", "Margin of Women"))

      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  made_a_choice <- reactive({
    if (input$choice == "Race") {
      data <- race %>%
        ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
        geom_point() +
        geom_label_repel(aes(label = state_district), size = 3, force = 1) +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Non-White Voters") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("hhhh",
                subtitle = "hhhh")
    }
    
    if (input$choice == "Education Level") {
      data <- education %>%
        ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
        geom_point() +
        geom_label_repel(aes(label = state_district), size = 3, force = 1) +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("hhhh",
                subtitle = "hhhh")
    }
    
    if (input$choice == "Landline versus Cell Phone") {
      data <- education %>%
        ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
        geom_point() +
        geom_label_repel(aes(label = state_district), size = 3, force = 1) +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Poll Respondants Who Answered a Landline") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("hhhh",
                subtitle = "hhhh")
    }
    
  })
   
   output$distPlot <- renderPlot({
     
     data <- made_a_choice()
     
    
   }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

