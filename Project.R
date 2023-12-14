install.packages(c("ggplot2", "dplyr", "shiny","reshape2", "tidyr", "car", "lmtest", "readr", "data.table", "caret", "randomForest", "zoo", "xts", "forecast", "tm", "knitr", "rmarkdown"))
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rmarkdown)
library(shiny)
# 1.Data exploration and cleaning using (tidyveres and dplyr)
# Set the path to your CSV file
csv_file_path <- "StudentsPerformance.csv"

# Read the CSV file into a data frame
Student_data <- read.csv(csv_file_path)

# Print the first few rows of the data frame
head(Student_data)
# Exploring the dataset structure
str(Student_data)

# View summary statistics
summary(Student_data)

# Reading the first few rows of the dataset
head(Student_data)

missing_values <- Student_data %>%
  summarise_all(~ sum(is.na(.)))


#2.Compute the discreptive statistics using dplr library

# Descriptive statistics for exam scores
# Function to calculate descriptive statistics: mean,Sd,min,count,median,Q1,Q3 and max values for  exam scores
describe_data <- function(column) {
  stats <- Student_data %>%
    summarise(
      Count = n(),
      Mean = mean(column, na.rm = TRUE),
      Standard_deviation = sd(column, na.rm = TRUE),
      Min = min(column, na.rm = TRUE),
      First_quartile_25th = quantile(column, 0.25, na.rm = TRUE),
      Median = median(column, na.rm = TRUE),
      Third_quartile_75th = quantile(column, 0.75, na.rm = TRUE),
      Max = max(column, na.rm = TRUE)
    )
  return(stats)
}

# Descriptive statistics for math score
math_stats <- describe_data(Student_data$math.score)
print("Math Score Statistics:")
print(math_stats)

# Descriptive statistics for reading score
reading_stats <- describe_data(Student_data$reading.score)
print("Reading Score Statistics:")
print(reading_stats)

# Descriptive statistics for writing score
writing_stats <- describe_data(Student_data$writing.score)
print("Writing Score Statistics:")
print(writing_stats)




# Computing counts for categorical  socio-economic variables

# Parental level of education
parental_education_counts <- table(Student_data$parental.level.of.education)

# Lunch type
lunch_type_counts <- table(Student_data$lunch)

# Test preparation course completion
test_prep_course_counts <- table(Student_data$`test.preparation.course`)

# outputting the results
print("Parental Level of Education Counts:")
print(parental_education_counts)

print("Lunch Type Counts:")
print(lunch_type_counts)

print("Test Preparation Course Completion Counts:")
print(test_prep_course_counts)



#Boxplot showing comparison in performance between female and male students in exam scores.

# Reshape the data for easy plotting
long_data <- Student_data %>%
  gather(key = "subject", value = "score", math.score, reading.score, writing.score)

# Plotting
ggplot(long_data, aes(x = subject, y = score, fill = gender)) +
  geom_boxplot() +
  facet_wrap(~ subject, scales = "free") +
  labs(title = "Performance Comparison by Gender across Different Subjects",
       x = "Subject",
       y = "Score") +
  theme_minimal()



#Bargraph showing average score by gender  across  subjects.
# Reshape the data for plotting
long_data <- Student_data %>%
  gather(key = "subject", value = "score", math.score, reading.score, writing.score)

# Plotting a grouped bar graph
ggplot(long_data, aes(x = subject, y = score, fill = gender)) +
  geom_bar(stat = "summary", fun.y = "mean", position = position_dodge()) +
  labs(title = "Average Scores by Gender across Subjects",
       x = "Subject",
       y = "Average Score") +
  theme_minimal()



#A histogram for math, reading,and writing  to show the distribution of the spread and skewness of the scores for all students
# Plotting histogram for Math Scores
ggplot(Student_data, aes(x = math.score)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Math Scores", x = "Math Score", y = "Count") +
  theme_minimal()

# Plotting histogram for Reading Scores
ggplot(Student_data, aes(x = reading.score)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  labs(title = "Histogram of Reading Scores", x = "Reading Score", y = "Count") +
  theme_minimal()

# Plotting histogram for Writing Scores
ggplot(Student_data, aes(x = writing.score)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  labs(title = "Histogram of Writing Scores", x = "Writing Score", y = "Count") +
  theme_minimal()


#Pic chart representing proportion of students for each category of categorical variables.

# Pie Chart showing proportion of students by Parental Level of Education with count figures
Student_data %>%
  count(parental.level.of.education) %>%
  ggplot(aes(x = "", y = n, fill = parental.level.of.education)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Students by Parental Level of Education", fill = "Parental Education") +
  theme_void()


# Pie Chart showing proportion of students by Lunch Types
Student_data %>%
  count(lunch) %>%
  ggplot(aes(x = "", y = n, fill = lunch)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Students by Lunch Type", fill = "Lunch Type") +
  theme_void()

# Pie Chart  showing proportion of students by Test Preparation Course Completion
Student_data %>%
  count(`test.preparation.course`) %>%
  ggplot(aes(x = "", y = n, fill = `test.preparation.course`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Students by Test Preparation Course Status", fill = "Test Preparation Course") +
  theme_void()




#Using dplyr library to
#To create stacked bar showing the distribution of scores  categorized as (low, medium, high) across different categorical variables such as parental level of education, lunch type and test preparation course.

#First, (categorize the scores into 'Low', 'Medium', and 'High')
#Then, plot stacked bar graphs for each subject score (Math, Reading, Writing) by parental level of education,test preparation course and lunch type

# Function to categorize scores <60,<80 and >=80 The categorize_score function assigns a category to each score.
categorize_score <- function(score) {
    if (score < 60) {
         "Low"
       } else if (score <= 80) {
           "Moderate"
         } else {
             "High"
           }
   }
# using  sapply  function to create new columns for categorized scores
Student_data$math_cat <- sapply(Student_data$math.score, categorize_score)
Student_data$reading_cat <- sapply(Student_data$reading.score, categorize_score)
Student_data$writing_cat <- sapply(Student_data$writing.score, categorize_score)

   #head(Student_data)
   # Plottinug stacked bar graph for Math Scores by Parental Level of Education
Student_data$math_cat <- factor(Student_data$math_cat, levels = c("Low", "Moderate","High"))
   ggplot(Student_data, aes(x = parental.level.of.education, fill=math_cat)) +
     geom_bar(position = "fill") +
     labs(title = "Distribution of Math Scores by Parental Level of Education",
         x = "Parental Level of Education", y = "Proportion") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1))



#  Plotting stacked bar graph for writing Scores by
   Student_data$writing_cat <- factor(Student_data$writing_cat, levels = c("Low", "Moderate","High"))
ggplot(Student_data, aes(x = lunch, fill=writing_cat)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Writing scores by Lunch type",
       x = "Lunch type", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Plotting stacked bar graph for reading Scores by Parental Level of Education
Student_data$reading_cat <- factor(Student_data$reading_cat, levels = c("Low", "Moderate","High"))
ggplot(Student_data, aes(x = test.preparation.course , fill=reading_cat)) +
  geom_bar(position = "fill") +
  labs(title = "Distribution of Reading Scores by Test preparation Course",
       x = "Test preparation Course", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#3

library(reshape2) # to reshape the correlation matrix into a format suitable for ggplot


# Computing the  correlation coefficients between numeric variables( Exam scores)
cor_matrix <- cor(Student_data[, c("math.score", "reading.score", "writing.score")], use = "complete.obs")

# Melt the data for ggplot
melted_cor_matrix <- melt(cor_matrix)

# heatmap to showng Correlation Coefficients
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Heatmap of Correlation Coefficients", x = "Variable 1", y = "Variable 2") +
  coord_fixed()

--------------------------------------------------------------------


#4.Hypothesis Testing:

#Testing  the hypotheses related to the impact of parental education, lunch type, and test preparation on exam scores.Formulate hypotheses and conduct t-tests or ANOVA tests using R functions.
#Evaluate p-values and make statistical inferences.

# ANOVA for Parental Education Impact on Math Scores
anova_result_parental_education <- aov(math.score ~ parental.level.of.education, data = Student_data)
summary(anova_result_parental_education)

# T-test for Lunch Type Impact on Math Scores (assuming two types of lunch)
t_test_result_lunch_type <- t.test(math.score ~ lunch, data = Student_data)
summary(t_test_result_lunch_type)

# T-test for Test Preparation Impact on Math Scores
t_test_result_test_preparation <- t.test(math.score ~ test.preparation.course, data = Student_data)
summary(t_test_result_test_preparation)
-----------------------------------------------------------------------
#5.Regression model



# Linear regression model for Math Score
math_model <- lm(math.score ~ parental.level.of.education + lunch + test.preparation.course, data = Student_data)
summary(math_model)

# linear regression model for Reading score
reading_model <- lm(reading.score ~ parental.level.of.education + lunch + test.preparation.course, data = Student_data)
summary(reading_model)
# linear regression model for Writing score
writing_model <- lm(writing.score ~ parental.level.of.education + lunch + test.preparation.course, data = Student_data)
summary(writing_model)


# Generate predictions for each model
math_predictions <- predict(math_model, newdata = Student_data)
reading_predictions <- predict(reading_model, newdata = Student_data)
writing_predictions <- predict(writing_model, newdata = Student_data)

# Creating dataframes for plotting
plot_data_math <- data.frame(Actual = Student_data$math.score, Predicted = math_predictions)
plot_data_reading <- data.frame(Actual = Student_data$reading.score, Predicted = reading_predictions)
plot_data_writing <- data.frame(Actual = Student_data$writing.score, Predicted = writing_predictions)

# Plotting Predicted vs. Actual values for Math Scores
ggplot(plot_data_math, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual Math Scores", x = "Actual Scores", y = "Predicted Scores") +
  theme_minimal()

# Plotting Predicted vs. Actual values for reading Scores
ggplot(plot_data_reading, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual reading Scores", x = "Actual Scores", y = "Predicted Scores") +
  theme_minimal()

# Plotting Predicted vs. Actual values for writing Scores
ggplot(plot_data_writing, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs. Actual wrting Scores", x = "Actual Scores", y = "Predicted Scores") +
  theme_minimal()



# Coefficient plot for Math Model
coef_data <- broom::tidy(math_model)
ggplot(coef_data, aes(estimate, term)) +
  geom_point() +
  labs(title = "Coefficients of Math Score Model")


# Coefficient plot for reading Model

coef_data <- broom::tidy(reading_model)
ggplot(coef_data, aes(estimate, term)) +
  geom_point() +
  labs(title = "Coefficients of Reading Score Model")

# Coefficient plot for writing Model

coef_data <- broom::tidy(writing_model)
ggplot(coef_data, aes(estimate, term)) +
  geom_point() +
  labs(title = "Coefficients of Writing Score Model")


# Residual Plot for Math Model
ggplot(math_model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot for Math Score Model", x = "Fitted Values", y = "Residuals")


# Residual Plot for Reading Model
ggplot(reading_model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot for Reading Score Model", x = "Fitted Values", y = "Residuals")

# Residual Plot for Writing Model
ggplot(writing_model, aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residual Plot for Writing Score Model", x = "Fitted Values", y = "Residuals")

#6.Comparing the impact of socio-economic factors on  exam scores.
#Using side-by-side box plots or a paired t-test to compare scores.

# Parental level of education
# Box plot comparing math scores based on parental education
ggplot(Student_data, aes(x = parental.level.of.education, y = math.score, fill = parental.level.of.education)) +
  geom_boxplot() +
  labs(title = "Math Scores by Parental Education", x = "Parental Education", y = "Math Score") +
  theme_minimal()

# Box plot comparing Reading scores based on parental education
ggplot(Student_data, aes(x = parental.level.of.education, y = reading.score, fill = parental.level.of.education)) +
  geom_boxplot() +
  labs(title = "Reading Scores by Parental Education", x = "Parental Education", y = "reading Score") +
  theme_minimal()
# Box plot comparing Writing scores based on parental education
ggplot(Student_data, aes(x = parental.level.of.education, y = writing.score, fill = parental.level.of.education)) +
  geom_boxplot() +
  labs(title = "Writing Scores by Parental Education", x = "Parental Education", y = "writing Score") +
  theme_minimal()

# Lunch Type


# Box plot comparing Math scores based on Lunch Type
ggplot(Student_data, aes(x = lunch, y = math.score, fill = lunch)) +
  geom_boxplot() +
  labs(title = "Math Scores by Lunch Type", x = "Lunch Type", y = "Math Score") +
  theme_minimal()

# Box plot comparing Reading scores based on Lunch Type
ggplot(Student_data, aes(x = lunch, y = reading.score, fill = lunch)) +
  geom_boxplot() +
  labs(title = "Reading Scores by Lunch Type", x = "Lunch Type", y = "reading Score") +
  theme_minimal()

# Box plot comparing Math scores based on Lunch Type
ggplot(Student_data, aes(x = lunch, y = writing.score, fill = lunch)) +
  geom_boxplot() +
  labs(title = "Writing Scores by Lunch Type", x = "Lunch Type", y = "writing Score") +
  theme_minimal()




#Test preparation Course
# Box plot comparing Math scores based on test preparation course
ggplot(Student_data, aes(x = test.preparation.course, y = math.score, fill = test.preparation.course)) +
  geom_boxplot() +
  labs(title = "Math Scores by Test Preperation Course", x = "Test Preperation Course", y = "Math Score") +
  theme_minimal()

# Box plot comparing Reading scores based on test preparation course
ggplot(Student_data, aes(x = test.preparation.course, y = reading.score, fill = test.preparation.course)) +
  geom_boxplot() +
  labs(title = "Reading Scores by Test Preperation Course", x = "Test Preperation Course", y = "Reading Score") +
  theme_minimal()

# Box plot comparing Reading scores based on test preparation course
ggplot(Student_data, aes(x = test.preparation.course, y = writing.score, fill = test.preparation.course)) +
  geom_boxplot() +
  labs(title = "Writing Scores by Test Preperation Course", x = "Test Preperation Course", y = "Writing Score") +
  theme_minimal()



#7.Dashboard to allow users easily interact with the data and insights.

ui <- fluidPage(
  titlePanel("Student Performance Dashboard"),

  # Sidebar for input controls
  sidebarLayout(
    sidebarPanel(
      # Add input controls like dropdowns, sliders, etc.
      selectInput("variable", "Choose a Variable:",
                  choices = c("Math Score", "Reading Score", "Writing Score")),
      selectInput("factor", "Choose a Socio-Economic Factor:",
                  choices = c("Parental Education", "Lunch Type", "Test Preparation"))
    ),

    # Main panel for displaying outputs
    mainPanel(
      # Output elements like plots and tables
      plotOutput("scorePlot")
    )
  )
)

server <- function(input, output) {
  output$scorePlot <- renderPlot({
    # Assuming the dataset is loaded as 'Student_data'
    data <- Student_data

    # Create dynamic plots based on user input
    if(input$variable == "Math Score") {
      score_var <- "math.score"
    } else if(input$variable == "Reading Score") {
      score_var <- "reading.score"
    } else {
      score_var <- "writing.score"
    }

    ggplot(data, aes_string(x = input$factor, y = score_var)) +
      geom_boxplot() +
      labs(title = paste(input$variable, "by", input$factor),
           x = input$factor,
           y = input$variable)
  })
}

shinyApp(ui = ui, server = server)

