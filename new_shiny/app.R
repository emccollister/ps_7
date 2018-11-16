library(shiny)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(plotly)

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
                              "Predictions on Democrats Taking the House", "Margin of Women"))
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if (input$choice == "Race") {
      race %>%
        ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
        geom_point() +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Non-White Voters") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("Percentage of Non-White Votes vs. Predicted Democratic Advantage by House District",
                subtitle = "hhhh")
    }
    
    else if (input$choice == "Education Level") {
      education %>%
        ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
        geom_point() +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("Percentage of Poll Respondants with Greater than a Bachelor's Degree vs. Predicted Democratic Advantage by House District",
                subtitle = "hhhh")
    }
    
    else if (input$choice == "Landline versus Cell Phone") {
      landline %>%
        ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
        geom_point() +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Poll Respondants Who Answered a Landline") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("Percentage of Poll Respondants Who Answered a Landline vs. Predicted Democratic Advantage by House District",
                subtitle = "hhhh")
    }
    
    else if (input$choice == "Predictions on Democrats Taking the House") {
      demtakehouse %>%
        ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
        geom_point() +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Percentage of Poll Respondants Who Thought Democrats Would Retake the House") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("Percentage of Poll Respondants Who Thought Democrats Would Retake the House vs. Predicted Democratic Advantage by House District",
                subtitle = "hhhh")
    }
    
    else if (input$choice == "Margin of Women") {
      women %>%
        ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
        geom_point() +
        scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
        xlab("Margin of Women Polled") +
        ylab("Predicted Democratic Advantage") + 
        ggtitle("Margin of Women Polled vs. Democratic Advantage",
                subtitle = "hhhh")
    }
    
    
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

