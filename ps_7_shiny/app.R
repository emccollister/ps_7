library(shiny)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(plotly)
library(ggpubr)

# Reads rds from the process_data file 
race <- read_rds("./race.rds")

# Long string of mutate commands (this probably wasn't the most efficient way!) to properly format congressional district
race <- race %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Reads rds from the process_data file 
education <- read_rds("./education.rds")

# Properly formats congressional district
education <- education %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Reads rds from the process_data file 
landline <- read_rds("./landline.rds")

# Properly formats congressional district
landline <- landline %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Reads rds from the process_data file 
demtakehouse <- read_rds("./genballot.rds")

# Properly formats congressional district
demtakehouse <- demtakehouse %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Reads rds from the process_data file 
women <- read_rds("./women.rds")

# Properly formats congressional district
women <- women %>%
  mutate(state = substr(state_district, 1, 2)) %>%
  mutate(info = substr(state_district, 3, 6)) %>%
  mutate(name = paste(state, info, sep = "-")) %>%
  mutate(state = str_to_upper(state)) %>%
  mutate(nicename = paste(state, info, sep = "-"))

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Do Different Populations Help Predict Democratic Advantage in the 2018 Midterm Elections?"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        # Defines the UI inputs
        # Including, a dropdown menu with types of data to examine
        # A checkbox to toggle on/off the congressional district lables
        # and a checkbox to toggle on/off the line of best fit to the data
        selectInput("choice", "Select a Type of Voter",
                    choices = c("Race", "Education Level", "Landline versus Cell Phone", 
                                "Predictions on Democrats Taking the House", "Margin of Women")),
        checkboxInput("on_off", label = "Show Congressional District Labels", value = FALSE),
        checkboxInput("line", label = "Show Line of Best Fit", value = FALSE)

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
     
     # Code to render the plots 
     # input$choice corresponds to what specific populations the user is viewing, while 
     # input$on_off refers to the checkbox which allows users to toggle on/off the state-district identification.
     # Similarly, input$line refers to the checkbox which allows users to toggle on/off a line of best fit. 
     
     if (input$choice == "Race" & input$on_off == FALSE) {
       raceplot <- race %>%
         ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Non-White Voters") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Non-White Votes vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was a weak positive correlation between the percentage of nonwhite voters per district \n and the Predicted Democratic Advantage with a few outliers.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       print(raceplot)
       if (input$line == TRUE) {
         withline<- raceplot + 
           geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
         
     }
     
     else if (input$choice == "Race" & input$on_off == TRUE) {
       raceplot <- race %>%
         ggplot(aes(x = non_white, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Non-White Voters") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Non-White Votes vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was a weak positive correlation between the percentage of nonwhite voters per district \n and the Predicted Democratic Advantage with a few outliers.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
        print(raceplot)
       
       if (input$line == TRUE) {
         withline<- raceplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
  
     }
     
     else if (input$choice == "Education Level" & input$on_off == FALSE) {
       eduplot <- education %>%
         ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants with Greater than a Bachelor's Degree vs.\n Predicted Democratic Advantage by House District",
                 subtitle = "There was a very weak positive correlation between the percentage of poll respondents with a \n degree higher than a Bachelor’s per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       
       print(eduplot)
       
       if (input$line == TRUE) {
         withline <- eduplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
     }
     
     else if (input$choice == "Education Level" & input$on_off == TRUE) {
       eduplot <- education %>%
         ggplot(aes(x = higher_ed, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants with Greater than a Bachelor's Degree") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants with Greater than a Bachelor's Degree vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a very weak positive correlation between the percentage of poll respondents with a \n degree higher than a Bachelor’s per district and the Predicted Democratic Advantage.")
        print(eduplot)
       if (input$line == TRUE) {
         withline <- eduplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
       }
     
     else if (input$choice == "Landline versus Cell Phone" & input$on_off == FALSE) {
       phoneplot <- landline %>%
         ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Answered a Landline") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Answered a Landline vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was no correlation between the percentage of poll respondents with who answered on a landline \n phone per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       print(phoneplot)
       
       if (input$line == TRUE) {
         withline <- phoneplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
     }
     
     else if (input$choice == "Landline versus Cell Phone" & input$on_off == TRUE) {
       phoneplot<- landline %>%
         ggplot(aes(x = landline, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Answered a Landline") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Answered a Landline vs. Predicted Democratic Advantage by House District",
                 subtitle = "There was no correlation between the percentage of poll respondents with who answered on a landline \n phone per district and the Predicted Democratic Advantage.")
     print(phoneplot)
     
     if (input$line == TRUE) {
       withline <- phoneplot + geom_smooth(method = lm, se = FALSE)
       print(withline)
     }
       
       }
     
     else if (input$choice == "Predictions on Democrats Taking the House" & input$on_off == FALSE) {
       demplot<- demtakehouse %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Thought Democrats Would Retake the House") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Thought Democrats Would Retake the House vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a positive correlation between the percentage of poll respondents who thought Democrats would \n retake the House per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       
       print(demplot)
       if (input$line == TRUE) {
         withline <- demplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
     }
     
     else if (input$choice == "Predictions on Democrats Taking the House" & input$on_off == TRUE) {
       demplot <- demtakehouse %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Percentage of Poll Respondants Who Thought Democrats Would Retake the House") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Percentage of Poll Respondants Who Thought Democrats Would Retake the House vs. \n Predicted Democratic Advantage by House District",
                 subtitle = "There was a positive correlation between the percentage of poll respondents who thought Democrats would retake \n the House per district and the Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       print(demplot)
       if (input$line == TRUE) {
         withline <- demplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
     }
     
     else if (input$choice == "Margin of Women" & input$on_off == FALSE) {
       womenplot <- women %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Margin of Women Polled") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Margin of Women Polled vs. Democratic Advantage",
                 subtitle = "There was a positive correlation between the percentage of female poll respondents per district and the \n Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       print(womenplot)
       
       if (input$line == TRUE) {
         withline <- womenplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
       
       
     }
     
     else if (input$choice == "Margin of Women" & input$on_off == TRUE) {
       womenplot <- women %>%
         ggplot(aes(x = genballot, y = dem_margin, color = dem_margin)) +
         geom_point() +
         geom_label_repel(aes(label = nicename), size = 3, force = 1) +
         scale_color_gradient(low= "red", high= "blue", name = "Predicted Dem. Adv.") +
         xlab("Margin of Women Polled") +
         ylab("Predicted Democratic Advantage") + 
         ggtitle("Margin of Women Polled vs. Democratic Advantage",
                 subtitle = "There was a positive correlation between the percentage of female poll respondents per district and the \n Predicted Democratic Advantage.") +
         theme(plot.title = element_text(lineheight=.8, face="bold"))
       print(womenplot)
       
       if (input$line == TRUE) {
         withline <- womenplot + geom_smooth(method = lm, se = FALSE)
         print(withline)
       }
     }
     
    
   }) 
}

# Run the application 
shinyApp(ui = ui, server = server)

