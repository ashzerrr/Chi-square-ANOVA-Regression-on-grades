
library(tidyverse)
library(Hmisc)
library(AICcmodavg)
library(car)
install.packages("GGally")
library(GGally)
install.packages("readxl")
library(readxl)
install.packages("rcompanion")
library(rcompanion)
library(MASS)
install.packages("MASS")



#Chi Square

merged_data<-read.csv("uci_student (1).csv")
merged_data<- na.omit(merged_data)
#transforming data to categorical variable if they are not

merged_data$famsize <- as.factor(merged_data$famsize)
merged_data$Mjob <- as.factor(merged_data$Mjob)

#create contingency table
table <- table(merged_data$famsize,merged_data$Mjob)
print(table)

# Chi-square of independence

chi_squared_result <- chisq.test(table)

print(chi_squared_result)


#Anova

merged_data$Dalc.x<- as.factor(merged_data$Dalc.x)

# Convert failures to categorical variables

merged_data$failures.x <- as.factor(merged_data$failures.x)
merged_data$Dalc.x<- as.factor(merged_data$Dalc.x)


#create boxplots

boxplot(
  G3.x ~ Dalc.x * failures.x,
  data = merged_data,
  main = "G3 Grade Distribution by Alcohol Consumption and Failures",
  xlab = "Weekday Alcohol Consumption and Failures",
  ylab = "G3 Grade",
  col = "steelblue",
  border = "black",
  las = 2
)
# Normality check
qqnorm(merged_data$G3.x)
qqline(merged_data$G3.x)

#transformation
merged_data$G3.x_trans<-transformTukey(merged_data$G3.x)


##Additive
G3.x_add <- aov(G3.x_trans ~ Dalc.x + failures.x,data=merged_data)
summary(G3.x_add)

##With Interaction
G3.x_int <- aov(G3.x_trans ~ Dalc.x * failures.x, data=merged_data)
summary(G3.x_int)


# Compare model output
model_set <- list(G3.x_add, G3.x_int)
model_names <- c("G3 grades Additive", "G3 grades Interactive")
aictab(model_set, model_names)  # Compare the models using AICc

summary(G3.x_add) # only failure.x is statistically significant
summary(G3.x_int)# only failure.x is statistically significant

TukeyHSD(G3.x_add, "failures.x")
TukeyHSD(G3.x_int, "failures.x")


# Checking for Normality ( normally distribution of residuals, equal variance and independence)


#define model residuals

resid_add <- residuals(G3.x_add)
resid_int <- residuals(G3.x_int)

#qqplot of residuals
qqnorm(resid_add)
qqline(resid_add)

qqnorm(resid_int)
qqline(resid_int)

#  histogram of residuals
hist(resid_add, main = "Histogram of Residuals", xlab = "Residuals", col = "steelblue")
hist(resid_int, main = "Histogram of Residuals", xlab = "Residuals", col = "red")

# check for homogenity of variance: Levene's test
leveneTest(G3.x_trans ~ Dalc.x: failures.x, data = merged_data) # residals are approximatelly normal


#Simple linear regression


math_grades<-read.csv("student_math (1).csv")
#Check normality

qqnorm(math_grades$G1)
qqline(math_grades$G1)

qqnorm(math_grades$absences)
qqline(math_grades$absences)

#transformation
G1_trans<-transformTukey(math_grades$G1)
absences_trans<-transformTukey(math_grades$absences)

qqnorm(G1_trans)
qqline(G1_trans)

qqnorm(absences_trans)
qqline(absences_trans)

#linear regression
model <- lm(G1_trans ~ absences_trans, data = math_grades)
summary(model)

#Checking assumptions:

#Scatter points to check linear relationship

scatter.smooth(absences_trans,G1_trans, main = "g1 grades VS absences")

#normality of residuals
residuals_model <- residuals(model)

# Q-Q plot of residuals
qqnorm(residuals_model)
qqline(residuals_model)


#Homoscedasticity
# Plot residuals vs fitted values
plot(model$fitted.values, residuals_model, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")

abline(h = 0, col = "black")




