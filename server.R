library(shiny)
library(shinycssloaders)
library(shinythemes)
library(reshape)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(ggpubr)

setwd("C:/Users/Jagruti Joshi/Documents/GitHub/Student-Performance-Analysis-Project")
data <- read.csv("StudentsPerformance - Copy.csv")

temp <- melt(data, id = c("student_id", "gender", "race_ethnicity", "parental_level_of_education", "lunch", "test_preparation_course_completed"))
temp <- temp %>% rename(course = variable, score = value)
temp <- temp %>% dplyr::mutate(course = str_replace(course, "_score", ""))

data_summary <- data %>% 
  group_by(gender, race_ethnicity, parental_level_of_education, lunch, test_preparation_course_completed) %>% 
  summarise(
  avg_math_score = mean(math_score),
  avg_reading_score = mean(reading_score),
  avg_writing_score = mean(writing_score),
)

test_prep_effect <- rbind(data %>% 
  group_by(gender, test_preparation_course_completed) %>%
    rename(variable_value = gender) %>%
  summarise(
    avg_math_score = mean(math_score),
    avg_reading_score = mean(reading_score),
    avg_writing_score = mean(writing_score),
  ) %>%
  add_column(variable = "gender"),
             data %>% 
               group_by(race_ethnicity, test_preparation_course_completed) %>%
               rename(variable_value = race_ethnicity) %>%
               summarise(
                 avg_math_score = mean(math_score),
                 avg_reading_score = mean(reading_score),
                 avg_writing_score = mean(writing_score),
               ) %>%
               add_column(variable = "race_ethnicity"),
  data %>% 
    group_by(parental_level_of_education, test_preparation_course_completed) %>%
    rename(variable_value = parental_level_of_education) %>%
    summarise(
      avg_math_score = mean(math_score),
      avg_reading_score = mean(reading_score),
      avg_writing_score = mean(writing_score),
    ) %>%
    add_column(variable = "parental_level_of_education"))

t.test(avg_math_score ~ test_preparation_course_completed, data = test_prep_effect, paired = TRUE)
t.test(avg_writing_score ~ test_preparation_course_completed, data = test_prep_effect, paired = TRUE)
t.test(avg_reading_score ~ test_preparation_course_completed, data = test_prep_effect, paired = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  temp_server <- reactive({
    req(input$GenderFinder)
    req(input$Race_EthnicityFinder)
    req(input$ParentalLevelOfEducationFinder)
    req(input$LunchFinder)
    req(input$TestPreparationCourseFinder)
    #req(Input$School_Rank)
    filter(temp, gender %in% input$GenderFinder) %>%
      filter(race_ethnicity %in% input$Race_EthnicityFinder) %>%
      filter(parental_level_of_education %in% input$ParentalLevelOfEducationFinder) %>%
      filter(lunch %in% input$LunchFinder) %>%
      filter(test_preparation_course_completed %in% input$TestPreparationCourseFinder)
  })
  
  data_summary_server <- reactive({
    req(input$GenderFinder)
    req(input$Race_EthnicityFinder)
    req(input$ParentalLevelOfEducationFinder)
    req(input$LunchFinder)
    req(input$TestPreparationCourseFinder)
    #req(Input$School_Rank)
    filter(data_summary,gender %in% input$GenderFinder) %>%
      filter(race_ethnicity %in% input$Race_EthnicityFinder) %>%
      filter(parental_level_of_education %in% input$ParentalLevelOfEducationFinder) %>%
      filter(lunch %in% input$LunchFinder) %>%
      filter(test_preparation_course_completed %in% input$TestPreparationCourseFinder)
      
  })
  
  test_prep_effect_server <- reactive({
    req(input$VariableFinder)
    filter(test_prep_effect,variable %in% input$VariableFinder)
    
  })

    output$BoxPlot <- renderPlot({

        ggplot(temp_server(), aes(x=course, y=score, fill=course)) + geom_boxplot()+ labs(title="BoxPlot: Distribution of Scores",x="Course", y = "Score")+ scale_fill_brewer(palette="RdBu") + theme(text=element_text(size=16,  family="Helvetica"))

    })
    
    output$Table <- renderTable(data_summary_server())
    
    output$BarChart <- renderPlot({
      
      p1 <- ggplot(data=test_prep_effect_server(), aes(x=variable_value, y=avg_math_score, fill=test_preparation_course_completed)) + geom_bar(stat="identity", color="black", position=position_dodge()) + scale_fill_brewer(palette="RdBu") + theme(text=element_text(size=12,  family="Helvetica"))
      
      p2 <- ggplot(data=test_prep_effect_server(), aes(x=variable_value, y=avg_reading_score, fill=test_preparation_course_completed)) + geom_bar(stat="identity", color="black", position=position_dodge()) + scale_fill_brewer(palette="RdBu") + theme(text=element_text(size=12,  family="Helvetica"))
      
      p3 <- ggplot(data=test_prep_effect_server(), aes(x=variable_value, y=avg_writing_score, fill=test_preparation_course_completed)) + geom_bar(stat="identity", color="black", position=position_dodge()) + scale_fill_brewer(palette="RdBu") + theme(text=element_text(size=12,  family="Helvetica"))
      
      ggarrange(p1, p2, p3, 
                #labels = c("A", "B", "C"),
                ncol = 1, nrow = 3)
      
    })
})