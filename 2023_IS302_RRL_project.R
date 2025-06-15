# IS302 Semester 2 2023 (Broncos Group)
#Team members
#Ravneel Sewak - S11199333
#Roska Takayawa - S11187423
#Liu Ying-S11188808

rm(list = ls())

############# Load Libraries ######################################################################

library(readxl)#read cvs file
library(tidyverse)
library(ggplot2)#in order to plot graphs later on
library(broom)
library(MASS)#for build ordinal regression model
library(tidymodels)
library(pROC)# roc plot
library(ggrepel)

############## Loading Data #########################################
getwd()
setwd("/Users/roska/Downloads")
data <- read_csv('studentdata.csv') #student performance Data
data
head(data)
str(data)
View(data) #have a look
class(data) #a dataframe or a tibble
glimpse(data) #look at the structure of data

############# Clean-up data #######################################################################
#change column name
colnames(data) <- c("STUDENT ID", "Student Age", "Sex",
                    "Graduated high-school type","Scholarship type",
                    "Additional work","Regular artistic or sports activity",
                    "Do you have a partner","Total salary if available",
                    "Transportation to the university","Accommodation type in Cyprus",
                    "Mothers education","Fathers education","Number of sisters/brothers",
                    "Parental status","Mothers occupation","Fathers occupation",
                    "Weekly study hours","Reading frequency","Reading frequency (scientific books/journals)",
                    "Attendance to the seminars/conferences related to the department",
                    "Impact of your projects/activities on your success",
                    "Attendance to classes","Preparation to midterm exams 1",
                    "Preparation to midterm exams 2","Taking notes in classes",
                    "Listening in classes","Discussion improves my interest and success in the course",
                    "Flip-classroom","Cumulative grade point average in the last semester (/4.00)",
                    "Expected Cumulative grade point average in the graduation (/4.00)",
                    "Course ID","OUTPUT Grade")

#Convert the variables 
data$`Student Age`<-factor(data$`Student Age`,c("1","2","3"),labels = c("18-21","22-25","above 26"))
data$Sex <- factor(data$Sex,c("1", "2"), labels = c( "Female","Male"))
data$`Graduated high-school type`<-factor(data$`Graduated high-school type`,c("1","2","3"),labels=c("private","state","other"))
data$`Scholarship type`<-factor(data$`Scholarship type`, c("1","2","3","4","5"),labels=c("None", "25%", "50%","75%", "Full"))
data$`Additional work`<- factor(data$`Additional work`,c("1", "2"), labels = c( "Yes","No"))
data$`Regular artistic or sports activity`<-factor(data$`Regular artistic or sports activity`,c("1", "2"), labels = c( "Yes","No"))
data$`Do you have a partner`<-factor(data$`Do you have a partner`,c("1", "2"), labels = c( "Yes","No"))
data$`Total salary if available`<-factor(data$`Total salary if available`,c("1","2","3","4","5"),labels = c("USD 135-200","USD 201-270","USD 271-340","USD 341-410","above 410"))
data$`Transportation to the university`<-factor(data$`Transportation to the university`,c("1","2","3","4"),labels = c("Bus","Private car/taxi","bicycle","Other"))
data$`Accommodation type in Cyprus`<-factor(data$`Accommodation type in Cyprus`,c("1","2","3","4"),labels = c("rental", "dormitory","with family","Other"))
data$`Mothers education`<-factor(data$`Mothers education`,c("1","2","3","4","5","6"),labels = c("primary school","secondary school","high school","university", "MSc.", "Ph.D."))
data$`Fathers education`<-factor(data$`Fathers education`,c("1","2","3","4","5","6"),labels = c("primary school","secondary school","high school","university", "MSc.", "Ph.D."))
data$`Parental status`<-factor(data$`Parental status`,c("1","2","3"),labels = c("married","divorced","died - one of them or both"))
data$`Mothers occupation`<-factor(data$`Mothers occupation`,c("1","2","3","4","5","6"),labels = c("retired","housewife","government officer", "private sector employee","self-employment","other"))
data$`Fathers occupation`<-factor(data$`Fathers occupation`,c("1","2","3","4","5"),labels = c("retired","government officer", "private sector employee","self-employment","other"))
data$`Reading frequency`<-factor(data$`Reading frequency`,c("1","2","3"),labels = c("None","Sometimes","Often"))
data$`Reading frequency (scientific books/journals)`<-factor(data$`Reading frequency (scientific books/journals)`,c("1","2","3"),labels = c("None","Sometimes","Often"))
data$`Attendance to the seminars/conferences related to the department`<- factor(data$`Attendance to the seminars/conferences related to the department`,c("1", "2"), labels = c( "Yes","No"))
data$`Impact of your projects/activities on your success`<- factor(data$`Impact of your projects/activities on your success`,c("1", "2","3"), labels = c( "positive","negative","neutral"))
data$`Attendance to classes`<- factor(data$`Attendance to classes`,c("1", "2","3"), labels = c( "always","sometimes","never"))
data$`Preparation to midterm exams 1`<-factor(data$`Preparation to midterm exams 1`,c("1","2","3"),labels = c("alone","with friends","not applicable"))
data$`Preparation to midterm exams 2`<-factor(data$`Preparation to midterm exams 2`,c("1","2","3"),labels = c("closest date to the exam","regularly during the semester","never"))
data$`Taking notes in classes`<-factor(data$`Taking notes in classes`,c("1","2","3"),labels=c("never","sometimes","always"))
data$`Listening in classes`<-factor(data$`Listening in classes`,c("1","2","3"),labels=c("never","sometimes","always"))
data$`Discussion improves my interest and success in the course`<-factor(data$`Discussion improves my interest and success in the course`,c("1","2","3"),labels=c("never","sometimes","always"))
data$`Flip-classroom`<-factor(data$`Flip-classroom`,c("1","2","3"),labels = c("not useful","useful","not applicable"))
data$`OUTPUT Grade`<-factor(data$`OUTPUT Grade`,c("0","1","2","3","4","5","6","7"),labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"))
write.csv(data, file = "performance.csv", row.names = FALSE)#save the cvs file after change the name and levels

str(data)
View(data)
head(data)
summary(data)

# ---- Output ----
# students:145
# 18-21:65;22-25:70; above 26:10
# female: 58; male:87
# Graduated high-school: private-25;state-103;other-17
#regular artistic or sports activity: yes-58;no-87
#Mothers education        Fathers education 
#primary school  :54      primary school  :29                   
#secondary school:27      secondary school:36                
#high school     :39      high school     :46                  
#university      :21      university      :28                 
#MSc.            : 2      MSc.            : 5              
#Ph.D.           : 2      Ph.D.           : 1 


#######General analysis of students grades#######
#General analysis of students grades
Grade_counts <- data %>%
  group_by(`OUTPUT Grade`) %>%
  summarise(Count = n())#summary the number if students in each grades
ggplot(Grade_counts, aes(x =`OUTPUT Grade` , y = Count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Student Grades", x = "grades", y = "couts") +
  theme_minimal()#plot the summary grades diagram of students


###########Graduated high school type Vs students grades#######
#Research Question 1: Does the type of high school (private, state, or other) 
#that students graduated from significantly affect their final GPA?
#Hypothesis 1: Students who graduated from private high schools will have, on average, 
#higher final GPAs compared to those from state or other types of high schools.
# Analyse the relationship between the type of high school that student graduated and the output grades
# Visualize the distribution of GPAs by High School Type
ggplot(data, aes(x = `OUTPUT Grade`, fill = `Graduated high-school type`)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Final GPA VS Graduated High School Type",
       x = "GPA",
       y = "Count",
       fill ="High School Type" ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
#Clearly that most student graduated from state hight school
#due to the huge different number of the students that graduated from each type,we still have to do more analysis to identify
#calculate the number for the student who get AA and BA(general A's students)
AABA_count <- data %>%
  filter(`OUTPUT Grade` == "AA" | `OUTPUT Grade` == "BA") %>%
  group_by(`Graduated high-school type`)%>%
  summarise(Count = n())
#Calculate the number of students that graduate from different type of highs chool
School_counts <- data %>%
  group_by(`Graduated high-school type`) %>%
  summarise(Total_Count = n())

#calculate the ration of students get A’s that graduated from different school
AABA_Ratio <- left_join(AABA_count, School_counts, by = NULL) %>%
  mutate(Ratio = Count / Total_Count)

ggplot(AABA_Ratio, aes(x = `Graduated high-school type`, y = Ratio, color = `Graduated high-school type`)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = round(Ratio, 4)), 
                  box.padding = 0.1)+
  labs(title = "AA & BA Grade Ratio by High School Type",
       x = "High School Type",
       y = "AA & BA Grade Ratio",
       color = "High School Type") +
  theme_minimal()


# calculate the number of student of different gardes by differen school
grade_counts <- data %>%
  group_by(`Graduated high-school type`, `OUTPUT Grade`) %>%
  summarise(Count = n())

# calculate all ration of different grades by differen school
grade_ratios <- grade_counts %>%
  group_by(`Graduated high-school type`) %>%
  mutate(Total = sum(Count),
         Ratio = Count / Total)


ggplot(grade_ratios, aes(x = `OUTPUT Grade`, y = Ratio, color = `Graduated high-school type`, group = `Graduated high-school type`)) +
  geom_point(size = 3) +
  geom_line(aes(group = `Graduated high-school type`), size = 1) +
  geom_text_repel(aes(label = round(Ratio, 4)), 
                  box.padding = 0.4, ) +
  labs(title = "Grade Distribution by High School Type",
       x = "Grade",
       y = "Ratio",
       color = "High School Type") +
  theme_minimal()
# conclusion:
# students from state hight school have, on average, 
# higher final GPAs compared to those from private or other types of high schools.

######regular artistic or sports activities VS student grades######
#Research question 2:Do students who engage in regular artistic or sports activities have higher or lower GPAs 
#compared to students who do not participate in these activities?
#Hypothesis 2: Students who engage in regular artistic or sports activities will have, on average, 
#similar GPAs compared to those who do not participate in these activities.

#summary plot of student gpa by engage in regular artistic or sports activities or not
ggplot(data, aes(x = `OUTPUT Grade`, fill = `Regular artistic or sports activity`)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Final GPA VS regular artistic or sports activity",
       x = "GPA",
       y = "Count",
       fill ="regular artistic or sports activity" ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# calculate the number of student that engage in regular artistic or sports activities or not
sports_counts <- data %>%
  group_by(`Regular artistic or sports activity`, `OUTPUT Grade`) %>%
  summarise(Count = n())

# calculate all ration of student gpa that engage in regular artistic or sports activities or not
sports_ratios <- sports_counts %>%
  group_by(`Regular artistic or sports activity`) %>%
  mutate(Total = sum(Count),
         sportsRatio = Count / Total)

ggplot(sports_ratios, aes(x = `OUTPUT Grade`, y = sportsRatio, color = `Regular artistic or sports activity`, group = `Regular artistic or sports activity`)) +
  geom_point(size = 3) +
  geom_line(aes(group = `Regular artistic or sports activity`), size = 1) +
  geom_text_repel(aes(label = round(sportsRatio, 4)), 
                  box.padding = 0.4, ) +
  labs(title = "Grade Distribution by engage in regular artistic or sports activities",
       x = "Grade",
       y = "Ratio",
       color = "regular artistic or sports activities") +
  theme_minimal()

#conclusion
#ratio: yes   NO
# AA  0.1207 0.1149
# BA  0.1207 0.069
# people who engage in regular artistic or sports activities has the higher rates to get good grades
# than the people who doeSn't 

###### parents education levels Vs students grades#####
#Research Question 3: How does the level of parents' education (mothers' and fathers' separately) 
#correlate with a student's expected cumulative GPA upon graduation?
#Hypothesis 3: Students with parents who have higher levels of education (e.g., university, MSc., or Ph.D.) will, 
#on average, have higher expected cumulative GPAs upon graduation.

#summary plot of each parent 
ggplot(data, aes(x = `OUTPUT Grade`, fill = `Mothers education`)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Final GPA affect by mother education level",
       x = "GPA",
       y = "Count",
       fill ="mother education" ) +
  theme_minimal() 


ggplot(data, aes(x = `OUTPUT Grade`, fill = `Fathers education`)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Final GPA affect by father education level",
       x = "GPA",
       y = "Count",
       fill ="Father education" ) +
  theme_minimal() 

data$`Mothers education`
data$`Fathers education`
data$`OUTPUT Grade`


# Combine mother's and father's education levels into a new variable
data$ParentsEducation <- paste(data$`Mothers education`, data$`Fathers education`, sep="&")

# Convert the Parents Education variable to a factor
data$ParentsEducation <- as.factor(data$ParentsEducation)

ggplot(data, aes(x = `OUTPUT Grade`  , fill = ParentsEducation)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Final GPA affect by parents education level",
       x = "GPA",
       y = "Count",
       fill ="GPA" ) +
  theme_minimal() 

# Calculate counts for AA and BA grades based on parents' education level
PEgrades_count <- data %>%
  filter(`OUTPUT Grade` %in% c("AA", "BA")) %>%
  group_by(ParentsEducation, `OUTPUT Grade`) %>%
  summarise(Count = n())

# Plotting the bar chart
ggplot(PEgrades_count, aes(x = `OUTPUT Grade`, y = Count, fill = ParentsEducation)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(title = "AA & BA Grade Count by Parents' Education Level",
       x = "GPA",
       y = "Count",
       fill = "Parents Education level") +
  theme_minimal()
----
#Conclusion
#students who get higher grade one of their parents education level is equal to primary school or higher
#we conclude that parent does affect student's mark but more important is students self


#######ordinal logistic regression model to predict GPA######
summary(data)

# Select relevant columns for prediction
features <- data[c("Graduated high-school type", "Regular artistic or sports activity", 
                   "Mothers education", "Fathers education", 
                   "Cumulative grade point average in the last semester (/4.00)")]

output_grade_ordinal <- data$`OUTPUT Grade` # Output Grade is ordinal (e.g., "AA", "BA", "CB","Fail")

# Train the ordinal logistic regression model
ordinal_model <- polr(output_grade_ordinal ~ ., data = features, method = "probit")

ordinal_model
summary(ordinal_model)

#Call:
 # polr(formula = output_grade_ordinal ~ ., data = features, method = "probit")

#Coefficients:
# Value Std. Error  t value
#`Graduated high-school type`state                              0.25741     0.2535  1.01543
#`Graduated high-school type`other                              0.32535     0.3439  0.94603
#`Regular artistic or sports activity`No                       -0.08354     0.1839 -0.45421
#`Mothers education`secondary school                            0.26921     0.2636  1.02137
#`Mothers education`high school                                -0.16449     0.2327 -0.70673
#`Mothers education`university                                  0.28250     0.3080  0.91711
#`Mothers education`MSc.                                       -0.86282     0.8384 -1.02914
#`Mothers education`Ph.D.                                       1.45824     0.8274  1.76240
#`Fathers education`secondary school                            0.02705     0.2808  0.09636
#`Fathers education`high school                                 0.03873     0.2618  0.14792
#`Fathers education`university                                  0.30604     0.3149  0.97195
#`Fathers education`MSc.                                        0.25481     0.5783  0.44063
#`Fathers education`Ph.D.                                       0.51830     1.0956  0.47307
#`Cumulative grade point average in the last semester (/4.00)`  0.31899     0.0730  4.36946

#Intercepts:
#  Value   Std. Error t value
#Fail|DD -0.5003  0.4129    -1.2116
#DD|DC    0.7227  0.4036     1.7906
#DC|CC    1.2308  0.4111     2.9937
#CC|CB    1.6468  0.4187     3.9334
#CB|BB    1.8499  0.4220     4.3832
#BB|BA    2.2330  0.4276     5.2225
#BA|AA    2.6162  0.4336     6.0339

#Residual Deviance: 546.1616 
#AIC: 588.1616


# Data frame of estimated coefficients
tidy(ordinal_model)

# Performance metrics on training data
glance(ordinal_model)

performance_test<- data

predict_data<-predict(ordinal_model, new_data = performance_test)

predict_data

# Generate predicted probabilities for each class
predicted_probabilities <- predict(ordinal_model, newdata = performance_test, type = "probs")

# Create ROC curves for each class
roc_curves <- lapply(1:ncol(predicted_probabilities), function(i) {
  roc(response = ifelse(performance_test$`OUTPUT Grade` == colnames(predicted_probabilities)[i], 1, 0),
      predictor = predicted_probabilities[, i])
})

# Plot ROC curves for each class
colors <- c("red", "green", "blue", "purple", "orange", "pink", "brown","black")  # Define colors
for (i in 1:length(roc_curves)) {
  plot(roc_curves[[i]], col = colors[i], add = i > 1, 
       main = "ROC Curves for Ordinal Model",
       xlab = "False Positive Rate", ylab = "True Positive Rate")
  legend("bottomright", legend = colnames(predicted_probabilities), fill = colors[1:i])
}

#######Chi-square test######
summary(data)
chisq.test(data$`OUTPUT Grade`, data$`Student Age`)# significant relationship
chisq.test(data$`OUTPUT Grade`, data$Sex)#significant relationship
chisq.test(data$`OUTPUT Grade`, data$`Cumulative grade point average in the last semester (/4.00)`)#highly significant relationship 
chisq.test(data$`OUTPUT Grade`, data$`Expected Cumulative grade point average in the graduation (/4.00)`)#significant relationship;statistically significant
chisq.test(data$`OUTPUT Grade`, data$`Scholarship type`)#significant relationship
chisq.test(data$`OUTPUT Grade`, data$`Impact of your projects/activities on your success`)#significant relationship


chisq.test(data$`OUTPUT Grade`, data$`Weekly study hours`)#relationship might be worth further investigation or analysis

chisq.test(data$`OUTPUT Grade`, data$`Transportation to the university`)#no significant relationship
chisq.test(data$`OUTPUT Grade`, data$`Additional work`)#no significant relationship  

######ordinal logistic regression model to predict GPA #2 ######
# Select relevant columns for prediction
features1 <- data[c("Student Age","Sex", 
                    "Scholarship type",
                    "Impact of your projects/activities on your success",
                    "Expected Cumulative grade point average in the graduation (/4.00)",
                    "Cumulative grade point average in the last semester (/4.00)",
                    "Weekly study hours",
                    "Reading frequency",
                    "Additional work",
                    "Transportation to the university",
                    "Accommodation type in Cyprus",
                    "Taking notes in classes")]

output_grade_ordinal <- data$`OUTPUT Grade` # Output Grade is ordinal (e.g., "AA", "BA", "CB","Fail")

# Train the ordinal logistic regression model
ordinal_model <- polr(output_grade_ordinal ~ ., data = features1, method = "probit")

ordinal_model
summary(ordinal_model)

# Data frame of estimated coefficients
tidy(ordinal_model)

# Performance metrics on training data
glance(ordinal_model)

performance_test<- data

predict_data<-predict(ordinal_model, new_data = performance_test)

predict_data

#######Multiple linear regression #########
#create new data-set because we want keep the values in numbers for building the linear regression
data2<-read_csv('studentdata.csv')
colnames(data2) <- c("STUDENT ID", "Student Age", "Sex",
                    "Graduated.high.school.type","Scholarship type",
                    "Additional work","Regular.artistic.or.sports.activity",
                    "Do you have a partner","Total salary if available",
                    "Transportation to the university","Accommodation type in Cyprus",
                    "Mothers.education","Fathers.education","Number of sisters/brothers",
                    "Parental status","Mothers occupation","Fathers occupation",
                    "Weekly study hours","Reading frequency","Reading frequency (scientific books/journals)",
                    "Attendance to the seminars/conferences related to the department",
                    "Impact of your projects/activities on your success",
                    "Attendance to classes","Preparation to midterm exams 1",
                    "Preparation to midterm exams 2","Taking notes in classes",
                    "Listening in classes","Discussion improves my interest and success in the course",
                    "Flip-classroom","Cumulative.grade.point.average.in.the.last.semester",
                    "Expected Cumulative grade point average in the graduation (/4.00)",
                    "Course ID","OUTPUT.Grade")

# Perform linear regression
linear_model <- lm(`OUTPUT.Grade` ~ `Graduated.high.school.type` + 
                     `Regular.artistic.or.sports.activity` + 
                     `Mothers.education` + 
                     `Fathers.education` + 
                     `Cumulative.grade.point.average.in.the.last.semester`, 
                   data = data2)

# Summary of the linear regression model
summary(linear_model)

#The math equation for the prediction is:
# 0.2956+0.5062*Graduated.high.school.type -0.251*Regular.artistic.or.sports.activity +
# 0.12*Mothers.education + 0.1*Fathers.education + 0.5*Cumulative.grade.point.average.in.the.last.semester
#For each unit increase in the variable "Graduated high school type",and the rest remain constant, the predicted output grade increases by 0.5062 points. 
#However, this coefficient is not statistically significant at the conventional levels (p-value = 0.127), 
#meaning it might not have a significant impact on the output grade.
#For each unit increase in the variable "Regular artistic or sports activity", the predicted output grade decreases by 0.2513 points. 
#However, this coefficient is also not statistically significant (p-value = 0.484).
#For each unit increase in the variables "Mothers education" and "Fathers education", the predicted output grade increases by 0.1286 and 0.1325 points, respectively. 
#However, neither of these coefficients is statistically significant (p-values = 0.421 and 0.435).
#For each unit increase in the variable "Cumulative grade point average in the last semester", the predicted output grade increases by 0.5464 points. 
#This coefficient is statistically significant (p-value < 0.001), indicating a strong positive impact of the cumulative GPA on the output grade.

#The multiple R-squared value (0.1277) indicates that approximately 12.77% of the variance in the output grade is explained by the predictors in the model.
#The p-value associated with the F-statistic (p-value < 0.001) suggests that the overall 
#model is statistically significant, meaning at least one of the predictors has a significant effect on the output grade.


# Create a new data-frame for prediction
prediction_data <- data.frame('Graduated.high.school.type' = data2$`Graduated.high.school.type`,
                              'Regular.artistic.or.sports.activity' = data2$`Regular.artistic.or.sports.activity`,
                              'Mothers.education' = data2$`Mothers.education`,
                              'Fathers.education' = data2$`Fathers.education`,
                              'Cumulative.grade.point.average.in.the.last.semester' = data2$`Cumulative.grade.point.average.in.the.last.semester`,
                              'OUTPUT.Grade'= data2$OUTPUT.Grade)
view(prediction_data)

# Add predicted values to the prediction data and round to the nearest whole number
prediction_data$predicted_grade <- round(predict(linear_model, newdata = prediction_data), 0)

#Plot
ggplot(prediction_data, aes(x = OUTPUT.Grade, fill = factor(predicted_grade))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Actual vs. Predicted Grades",
       x = "output grade",
       y = "Count",
       fill = "Predicted Grade") +
  scale_fill_manual(values = c("0" = "red", "1" = "purple", "2" = "skyblue", "3" = "tomato", "4" = "grey", "5" = "black", "6" = "pink", "7" = "brown"),
                    breaks = c("0", "1", "2", "3", "4", "5", "6", "7"),
                    labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"),
                    name = "Predicted Grade") +
  theme_minimal()

# Create a data frame for comparison
compare_data <- data.frame(
  actual_Grade = factor(prediction_data$OUTPUT.Grade),
  predicted_Grade = factor(prediction_data$predicted_grade)
)

# Create a contingency table
compare_table <- table(compare_data$actual_Grade, compare_data$predicted_Grade)

# Print the contingency table
print(compare_table)

########correlation test######
cor.test(data2$OUTPUT.Grade,data2$Cumulative.grade.point.average.in.the.last.semester) #moderate positive corr
cor.test(data2$OUTPUT.Grade,data2$`Additional work`)#weak positive
cor.test(data2$OUTPUT.Grade,data2$`Student Age`)#weak negative
cor.test(data2$OUTPUT.Grade,data2$`Do you have a partner`)#weak negative
cor.test(data2$OUTPUT.Grade,data2$`Parental status`)#weak positive
cor.test(data2$OUTPUT.Grade,data2$`Accommodation type in Cyprus`)#weak positive
cor.test(data2$OUTPUT.Grade,data2$`Transportation to the university`)#weak negative
cor.test(data2$OUTPUT.Grade,data2$Mothers.education)#weak positive
cor.test(data2$OUTPUT.Grade,data2$`Fathers occupation`)#weak negative
cor.test(data2$OUTPUT.Grade,data2$`Mothers occupation`)#weak negative

########Multiple linear regression model 2######
data2$Sex <- factor(data2$Sex,c("1", "2"), labels = c( "Female","Male"))
linear_model2 <- lm(`OUTPUT.Grade` ~ `Student Age` + 
                      `Sex`+
                      `Additional work` +
                      `Weekly study hours`+
                      `Transportation to the university`+
                      `Accommodation type in Cyprus`+
                      `Preparation to midterm exams 1` + 
                      `Preparation to midterm exams 2`+
                      `Reading frequency` +
                      `Student Age`*`Reading frequency`+
                      `Additional work`*`Listening in classes`*`Taking notes in classes`+
                      `Preparation to midterm exams 1`*`Preparation to midterm exams 2`+
                      `Cumulative.grade.point.average.in.the.last.semester`, 
                    data = data2)
# Summary of the linear regression model
summary(linear_model2)

#Intercept (Constant): The intercept term (15.5228) represents the expected value of the outcome variable when 
#all predictor variables are zero.

#Sex (Male): Being male is significant (p-value < 0.001), with a coefficient of 1.5994. On average, 
#males have a higher outcome value compared to females.

#Additional Work: Having additional work experience is significant (p-value = 0.003), with a coefficient of -13.7933. 
#This suggests that students with additional work experience tend to have lower outcomes.

#Weekly Study Hours: Weekly study hours, while not statistically significant (p-value = 0.153), have a coefficient of -0.2784. This suggests a negative relationship, although it's not strong enough to be considered significant at conventional levels.

#Transportation to the University: The type of transportation to the university is significant (p-value = 0.009), 
#with a coefficient of -0.4065. Certain transportation methods are associated with lower outcomes.

#Accommodation Type in Cyprus: Accommodation type is not statistically significant (p-value = 0.116), 
#indicating that it might not have a significant impact on the outcome in this model.

#Preparation to Midterm Exams, Reading Frequency, Listening in Classes, Taking Notes in Classes: These variables, 
#either individually or in interaction with others, are significant predictors, indicating that students' study habits and preparation methods are related to the outcome variable.

#Cumulative Grade Point Average in the Last Semester: Cumulative GPA in the last semester is significant (p-value = 0.004) 
#with a coefficient of 0.3744. Higher GPA is associated with higher outcomes.

#The overall model (Multiple R-squared: 0.4397) explains approximately 44% of the variance in the outcome variable. 
#The adjusted R-squared accounts for the number of predictors in the model, providing a more accurate measure of 
#the model's goodness-of-fit when considering the number of predictors. 
#The F-statistic tests the overall significance of the model and indicates 
#that the model is statistically significant (p-value < 0.001).

# Create a new data-frame for prediction
prediction_data2 <- data.frame('Cumulative.grade.point.average.in.the.last.semester' = data2$`Cumulative.grade.point.average.in.the.last.semester`,
                               'OUTPUT.Grade'= data2$OUTPUT.Grade)
view(prediction_data2)

# Add predicted values to the prediction data and round to the nearest whole number
prediction_data2$predicted_grade <- round(predict(linear_model2, newdata = data2), 0)

#Plot
ggplot(prediction_data2, aes(x = factor(OUTPUT.Grade, levels = c("0", "1", "2", "3", "4", "5", "6", "7")), fill = factor(predicted_grade))) +
  geom_bar() +
  labs(title = "Composition of Predicted Grades within Actual Grades",
       x = "Output Grade",
       y = "Count",
       fill = "Predicted Grade") +
  scale_fill_manual(values = c("0" = "red", "1" = "purple", "2" = "skyblue", "3" = "tomato", 
                               "4" = "grey", "5" = "black", "6" = "pink", "7" = "brown"),
                    breaks = c("0", "1", "2", "3", "4", "5", "6", "7"),
                    labels = c("Fail", "DD", "DC", "CC", "CB", "BB", "BA", "AA"),
                    name = "Predicted Grade") +
  scale_x_discrete(labels = c("0" = "Fail", "1" = "DD", "2" = "DC", "3" = "CC", "4" = "CB", "5" = "BB", "6" = "BA", "7" = "AA")) +
  theme_minimal()

#change levels
prediction_data2$`OUTPUT.Grade`<-factor(prediction_data2$`OUTPUT.Grade`,c("0","1","2","3","4","5","6","7"),labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"))
prediction_data2$predicted_grade<-factor(prediction_data2$`predicted_grade`,c("0","1","2","3","4","5","6","7"),labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"))
#Filter the data for grades from Fail to AA
filtered_data <- subset(prediction_data2, predicted_grade %in% c("Fail","DD","DC","CC","CB","BB","BA","AA"))

# Create a text-based comparison
comparison_table <- table(filtered_data$OUTPUT.Grade, filtered_data$predicted_grade)
print(comparison_table)

########Assumptions######
# Linearity Assumption
# Scatter plots for each predictor variable against the residuals
library(olsrr)#load packages
ols_plot_resid_fit(linear_model2)

#NORMALITY
ols_plot_resid_qq(linear_model2)
#the data is not normally distributed
#The points do not fall along the 45-degree reference line,
#indicating that the quantiles of the data are not the same as the quantiles of the normal distribution
#the plot shows that the data is skewed to the left, with more extreme values in the left tail than in the right tail. 
#This suggests that the median of the data is less than the mean

ols_plot_resid_hist(linear_model2)
#the majority of residuals falling close to zero,it suggests that the model is making good predictions on average
#However, there are a few outliers in the histogram, with some residuals falling far from zero,
#This suggests that the model is not perfect, and that there are some cases where it is making large errors

ols_test_normality(linear_model2)#test fail to reject the null hypothesis 
#Shapiro-Wilk:no strong evidence to conclude that the residuals are not normally distributed.
#Kolmogorov-Smirnov :no significant departure from normality
#Cramer-von Mises:there is a significant departure from normality
#Anderson-Darling:the data does not significantly deviate from a normal distribution

#HETEROSCEDASTICITY
ols_plot_resid_fit(linear_model2)

ols_test_breusch_pagan(linear_model2)
#The variance of the residuals is constant (homoskedasticity).
#The variance of the residuals is not constant (heteroskedasticity)
#The p-value (0.2712939) associated with the Chi-square statistic is greater than the typical significance level (such as α = 0.05). 
#Therefore, do not have enough evidence to reject the null hypothesis.
#Based on the Breusch-Pagan test, there is no significant evidence to suggest that the variance of the residuals is 
#not constant across different levels of the independent variables. Hence, you do not have sufficient grounds to 
#conclude that heteroskedasticity is present in your regression model.
ols_test_breusch_pagan(linear_model2, rhs = TRUE)
#there isn't enough evidence to conclude that 
#there is a significant association between the categorical variables being analyzed in the chi-squared test.


#Heteroskedasticity
ols_test_f(linear_model2)
#do not have sufficient evidence to conclude that the variance of errors is not homogenous across the levels 
#of the independent variables
ols_test_f(linear_model2, rhs = TRUE)


######ordinal logistic regression using data3####
data3<-read_csv('studentdata.csv')
colnames(data3) <- c("STUDENT ID", "Student Age", "Sex",
                     "Graduated.high.school.type","Scholarship type",
                     "Additional work","Regular.artistic.or.sports.activity",
                     "Do you have a partner","Total salary if available",
                     "Transportation to the university","Accommodation type in Cyprus",
                     "Mothers.education","Fathers.education","Number of sisters/brothers",
                     "Parental status","Mothers occupation","Fathers occupation",
                     "Weekly study hours","Reading frequency","Reading frequency (scientific books/journals)",
                     "Attendance to the seminars/conferences related to the department",
                     "Impact of your projects/activities on your success",
                     "Attendance to classes","Preparation to midterm exams 1",
                     "Preparation to midterm exams 2","Taking notes in classes",
                     "Listening in classes","Discussion improves my interest and success in the course",
                     "Flip-classroom","Cumulative.grade.point.average.in.the.last.semester",
                     "Expected Cumulative grade point average in the graduation (/4.00)",
                     "Course ID","OUTPUT.Grade")

data3$`OUTPUT.Grade`<-factor(data3$`OUTPUT.Grade`,c("0","1","2","3","4","5","6","7"),labels = c("Fail","DD","DC","CC","CB","BB","BA","AA"))
# Select relevant columns for prediction
features <- data3[c("Graduated.high.school.type", "Regular.artistic.or.sports.activity", 
                   "Mothers.education", "Fathers.education", 
                   "Cumulative.grade.point.average.in.the.last.semester")]

output_grade_ordinal <- data3$`OUTPUT.Grade` # Output Grade is ordinal (e.g., "AA", "BA", "CB","Fail")

# Train the ordinal logistic regression model
ordinal_model <- polr(output_grade_ordinal ~ ., data = features, method = "probit")

ordinal_model
summary(ordinal_model)

predict_data <- data.frame('Graduated.high.school.type' = data3$`Graduated.high.school.type`,
                              'Regular.artistic.or.sports.activity' = data3$`Regular.artistic.or.sports.activity`,
                              'Mothers.education' = data3$`Mothers.education`,
                              'Fathers.education' = data3$`Fathers.education`,
                              'Cumulative.grade.point.average.in.the.last.semester' = data3$`Cumulative.grade.point.average.in.the.last.semester`,
                              'OUTPUT.Grade'= data3$OUTPUT.Grade)
predict_data$predicted_grade <- predict(ordinal_model, newdata = predict_data)
view(predict_data)

ggplot(predict_data, aes(x = OUTPUT.Grade, fill = factor(predicted_grade))) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Actual vs. Predicted Grades",
       x = "output grade",
       y = "Count",
       fill = "Predicted Grade") +
  theme_minimal()
# Create a stacked bar plot comparing actual vs. predicted grades
ggplot(predict_data, aes(x = factor(OUTPUT.Grade), fill = factor(predicted_grade))) +
  geom_bar(position = "stack") +
  labs(title = "Comparison of Actual vs. Predicted Grades",
       x = "Actual Output Grade",
       y = "Count",
       fill = "Predicted Output Grade") +
  theme_minimal() +
  scale_fill_discrete(name = "Predicted Output Grade")
# Create a data frame for comparison
comparison_data <- data.frame(
  Actual_Grade = factor(predict_data$OUTPUT.Grade),
  Predicted_Grade = factor(predict_data$predicted_grade)
)

# Create a contingency table
comparison_table <- table(comparison_data$Actual_Grade, comparison_data$Predicted_Grade)

# Print the contingency table
print(comparison_table)

#Graduated High School Type: The coefficient 0.22452 suggests that students from a different high school type are more likely to move to a higher grade category.
#Regular Artistic or Sports Activity: The coefficient -0.04855 indicates a slight negative effect on the odds of moving to a higher grade category, although it is not statistically significant.
#Mothers' Education: The coefficient 0.06458 suggests that higher maternal education slightly increases the odds of moving to a higher grade category.
#Fathers' Education: The coefficient 0.05087 suggests that higher paternal education slightly increases the odds of moving to a higher grade category.
#Cumulative Grade Point Average: The coefficient 0.28717 indicates a significant positive effect, meaning higher grades in the last semester significantly increase the odds of moving to a higher grade category.
