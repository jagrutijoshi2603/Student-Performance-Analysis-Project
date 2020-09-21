library(shiny)
library(shinycssloaders)
library(shinythemes)
library(reshape)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 12 pixels. */
font-size: 12px;
}"


# Define UI
ui <- fluidPage(
    
    #Navbar structure for UI
    navbarPage("Student Performance Analysis", theme = shinytheme("lumen"),
               tabPanel("Distribution of Scores", fluid = TRUE, icon = icon("graduation-cap"),
                        tags$style(button_color_css),
                        # Sidebar layout with a input and output definitions
                        sidebarLayout(
                            sidebarPanel(
                                
                                titlePanel("Demographic Variables"),
                                #shinythemes::themeSelector(),
                                fluidRow(column(3,
                                                # Select which Gender(s) to plot
                                                checkboxGroupInput(inputId = "GenderFinder",label = "Select Gender(s):",choices = c("Male", "Female"),selected = "Male"),
                                                
                                                # Select which Division(s) to plot
                                                checkboxGroupInput(inputId = "Race_EthnicityFinder",label = "Select Race/Ethnicity:",choices = c("Group A", "Group B", "Group C", "Group D", "Group E"),selected = "Group A")
                                ),
                                column(6, offset = 2,
                                       # Select which Region(s) to plot
                                       checkboxGroupInput(inputId = "ParentalLevelOfEducationFinder",label = "Select Parental Level Of Education:",choices = c("High School", "Associate's Degree", "Bachelor's Degree", "Master's Degree"),selected = "High School"))),
                                # Select which School Type to plot
                                checkboxGroupInput(inputId = "LunchFinder",label = "Select Lunch Type(s):",choices = c("Standard", "Free/Reduced"),selected = c("Standard")),
                                # Select which Test Preparation Course to plot
                                checkboxGroupInput(inputId = "TestPreparationCourseFinder",label = "Select Test Preparation Course(s):",choices = c("Yes", "No"),selected = c("Yes"))
                                ),
                            
                            mainPanel(
                                plotOutput(outputId = "BoxPlot")
                                ,tableOutput(outputId = "Table")
                                
)
                        )
               ),
tabPanel("Effect of Test Preparation Course", fluid = TRUE, icon = icon("university"),
         tags$style(button_color_css),
         # Sidebar layout with a input and output definitions
         sidebarLayout(
           sidebarPanel(
             
             titlePanel("Demographic Variables"),
             #shinythemes::themeSelector(),
             fluidRow(column(3,
                             # Select which variable to plot
                             radioButtons("VariableFinder", "Choose one:",
                                          choiceNames = c("gender","race_ethnicity","parental_level_of_education"),
                                          choiceValues = c("gender","race_ethnicity","parental_level_of_education"))))),
           
           mainPanel(
             plotOutput(outputId = "BarChart")
           )
         )
),
tabPanel("Recommendations & Next Steps", fluid = TRUE, icon = icon("poll"),
         tags$style(button_color_css),
         h2("Recommendations"),
         h4("1. The difference in the mean test scores for math, writing and reading for the two groups (test prep course taken vs not taken) is statistically significant on an overall level meaning that the test prep course does have an impact and should be expanded across other schools too"),h4("2. Most categories of students across different genders, races and parental level of education have the lowest test scores in math compared to writing and reading. The test prep course should we redesigned with an emphasis on math."),
         h2("Next Steps"),
         h4("1. A Regression Model can be built to predict the test scores of students for math, writing and reading using predictors such as gender, race, parental level of education, lunch. This will help the stakeholders associated with test prep course company make better predictions regarding the improvements they can make at a student level"),
         h4("2. The same analysis of test prep course effectiveness can be repeated by taking lunch as the target variable. Schools can further leverage the results from this analysis to secure funds from governments to help them move from a free/ reduced lunch model to a standard lunch model to ensure the growth and well-being of its students")
           )
    )   
)