library(shiny)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(plotly)

race <- read_rds("./race.rds")

race <- race %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

education <- read_rds("./education.rds")

education <- education %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

landline <- read_rds("./landline.rds")

landline <- landline %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

demtakehouse <- read_rds("./genballot.rds")

demtakehouse <- demtakehouse %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

women <- read_rds("./women.rds")

women <- women %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Different Populations Influence on 2018 Midterm Elections"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("choice", "Select a Type of Voter",
                    choices = c("Race", "Education Level", "Landline versus Cell Phone", 
                                "Predictions on Democrats Taking the House", "Margin of Women")),
        checkboxInput("on_off", label = "Show Congressional District Labels", value = FALSE)

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
     
     if (input$choice == "Race" & input$on_off == FALSE) {
       race %>%
         ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Non-White Voters") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Non-White Votes vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was a weak positive correlation between the percentage of nonwhite voters per district \n and the Predicted Democratic Advantage with a few outliers.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Race" & input$on_off == TRUE) {
       race %>%
         ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Non-White Voters") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Non-White Votes vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was a weak positive correlation between the percentage of nonwhite voters per district \n and the Predicted Democratic Advantage with a few outliers.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Education Level" & input$on_off == FALSE) {
       education %>%
         ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants with Greater than a Bachelor's Degree vs.\n Predicted Democratic Advantage by House District",
                 subtitle = "There was a very weak positive correlation between the percentage of poll respondents with a \n degree higher than a Bachelor’s per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Education Level" & input$on_off == TRUE) {
       education %>%
         ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants with Greater than a Bachelor's Degree vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a very weak positive correlation between the percentage of poll respondents with a \n degree higher than a Bachelor’s per district and the Predicted Democratic Advantage.")
     }
     
     else if (input$choice == "Landline versus Cell Phone" & input$on_off == FALSE) {
       landline %>%
         ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Answered a Landline") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Answered a Landline vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was no correlation between the percentage of poll respondents with who answered on a landline \n phone per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Landline versus Cell Phone" & input$on_off == TRUE) {
       landline %>%
         ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Answered a Landline") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Answered a Landline vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was no correlation between the percentage of poll respondents with who answered on a landline \n phone per district and the Predicted Democratic Advantage.")
     }
     
     else if (input$choice == "Predictions on Democrats Taking the House" & input$on_off == FALSE) {
       demtakehouse %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Thought Democrats Would Retake the House") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Thought Democrats Would Retake the House vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a positive correlation between the percentage of poll respondents who thought Democrats would \n retake the House per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Predictions on Democrats Taking the House" & input$on_off == TRUE) {
       demtakehouse %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Thought Democrats Would Retake the House") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Thought Democrats Would Retake the House vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a positive correlation between the percentage of poll respondents who thought Democrats would retake \n the House per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Margin of Women" & input$on_off == FALSE) {
       women %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Margin of Women Polled") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Margin of Women Polled vs. Democratic Advantage",
                 subtitle = "There was a positive correlation between the percentage of female poll respondents per district and the \n Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
     else if (input$choice == "Margin of Women" & input$on_off == TRUE) {
       women %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Margin of Women Polled") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Margin of Women Polled vs. Democratic Advantage",
                 subtitle = "There was a positive correlation between the percentage of female poll respondents per district and the \n Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
     }
     
    
   }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

