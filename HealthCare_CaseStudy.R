install.packages("survminer")
install.packages("factoextra")
install.packages(("ROSE"))
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(survival)
library(survminer)
library(cluster)
library(factoextra)

getwd()
setwd("/Users/pranavsrinivasvenkatesh/Projects/Statistics Project")

list.files("/Users/pranavsrinivasvenkatesh/Projects/Statistics Project")

# Assuming the dataset is in CSV format
data <- read.csv("/Users/pranavsrinivasvenkatesh/Projects/Statistics Project/healthcare_dataset.csv")


# Preview the dataset
head(data)


##Data Preprocessing
# Convert categorical variables to factors
data$Gender <- factor(data$Gender, levels = c("Male", "Female"))
data$Blood.Type <- factor(data$Blood.Type)
data$Medical.Condition <- factor(data$Medical.Condition)
data$Doctor <- factor(data$Doctor)
data$Hospital <- factor(data$Hospital)
data$Insurance.Provider <- factor(data$Insurance.Provider)
data$Admission.Type <- factor(data$Admission.Type, levels = c("Emergency", "Elective", "Urgent"))
data$Test.Results <- factor(data$Test.Results, levels = c("Normal", "Abnormal", "Inconclusive"))

# Convert date columns to Date format
data$Date.of.Admission <- as.Date(data$Date.of.Admission, format = "%Y-%m-%d")
data$Discharge.Date <- as.Date(data$Discharge.Date, format = "%Y-%m-%d")

# Calculate the length of stay
data$Length.of.Stay <- as.numeric(data$Discharge.Date - data$Date.of.Admission)

# Handle missing values
sum(is.na(data))

# Handle missing values if any (e.g., impute or remove)
data <- na.omit(data)



##EDA(Explonatory Data Analysis)
# Summary of the dataset
summary(data)

# Visualize the distribution of age
ggplot(data, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black") + 
  labs(title = "Age Distribution of Patients")

# Visualize the relationship between Admission Type and Billing Amount
ggplot(data, aes(x = Admission.Type, y = Billing.Amount, fill = Admission.Type)) +
  geom_boxplot() +
  labs(title = "Billing Amount by Admission Type")

# Visualize Length of Stay by Medical Condition
ggplot(data, aes(x = Medical.Condition, y = Length.of.Stay, fill = Medical.Condition)) +
  geom_boxplot() +
  labs(title = "Length of Stay by Medical Condition") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Hypothesis test:

#Is there a significant difference in billing amount between emergency and elective admissions?
billing_emergency_elective <- data %>% filter(Admission.Type %in% c("Emergency", "Elective"))
t_test_result <- t.test(Billing.Amount ~ Admission.Type, data = billing_emergency_elective)
print(t_test_result)

# Chi-square test: Is there a relationship between Medical Condition and Test Results?
chi_test_result <- chisq.test(table(data$Medical.Condition, data$Test.Results))
print(chi_test_result)


# K-means clustering based on age and billing amount
set.seed(123)
data_numeric <- data %>% select(Age, Billing.Amount)
kmeans_result <- kmeans(scale(data_numeric), centers = 5)
data$Cluster <- factor(kmeans_result$cluster)

fviz_cluster(kmeans_result, data = scale(data_numeric),
             geom = "point", ellipse.type = "norm", palette = "jco")


# Examine cluster centers
print(kmeans_result$centers)

# Calculate mean and standard deviation for Age and Billing Amount
mean_age <- mean(data_numeric$Age)
sd_age <- sd(data_numeric$Age)

mean_billing <- mean(data_numeric$Billing.Amount)
sd_billing <- sd(data_numeric$Billing.Amount)

# Display the calculated statistics
print(paste("Mean Age:", mean_age))
print(paste("Standard Deviation Age:", sd_age))

print(paste("Mean Billing Amount:", mean_billing))
print(paste("Standard Deviation Billing Amount:", sd_billing))


# Unscaling the cluster centers
cluster_centers_original <- data.frame(
  Age = kmeans_result$centers[, "Age"] * sd_age + mean_age,
  Billing.Amount = kmeans_result$centers[, "Billing.Amount"] * sd_billing + mean_billing
)

# Print the unscaled cluster centers
print(cluster_centers_original)


## PCA on numeric variables

# Check for missing values in the data_numeric and Outcome.Variable
sum(is.na(data_numeric))
sum(is.na(data$Outcome.Variable))

# Check the number of rows in both datasets
nrow(data_numeric)
length(data$Outcome.Variable)

# Subset the Outcome.Variable if necessary
outcome_aligned <- data$Outcome.Variable[rownames(data_numeric)]
# Perform PCA visualization with aligned outcome variable
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = outcome_aligned) +
  labs(title = "PCA of Patients", x = "PC1", y = "PC2")
# Verify column names
colnames(data_numeric)
# Perform PCA again if necessary
pca_result <- prcomp(data_numeric, center = TRUE, scale. = TRUE)

# Visualize PCA with the correct outcome variable
fviz_pca_ind(pca_result, geom.ind = "point", col.ind = outcome_aligned) +
  labs(title = "PCA of Patients", x = "PC1", y = "PC2")

##Predictive Analysis
# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$Test.Results, p = .8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Fit a logistic regression model
logistic_model <- glm(Test.Results ~ Age + Gender + Blood.Type + Medical.Condition + Length.of.Stay + Billing.Amount + Admission.Type, 
                      data = train_data, 
                      family = binomial)

# Summary of the model
summary(logistic_model)

# Predict on the test data
predictions <- predict(logistic_model, test_data, type = "response")

# Convert probabilities to class labels (assuming "Normal" is the reference level)
test_data$Predicted <- ifelse(predictions > 0.5, "Abnormal", "Normal")


# Convert Predicted to a factor
test_data$Predicted <- factor(test_data$Predicted, levels = levels(test_data$Test.Results))

# Create a confusion matrix to evaluate the predictions
conf_matrix <- confusionMatrix(test_data$Predicted, test_data$Test.Results)

# Print the confusion matrix and related statistics
print(conf_matrix)

# Check the class distribution in the training data
table(train_data$Test.Results)



# Load necessary libraries
library(dplyr)

# Check the class distribution
table(train_data$Test.Results)

# Determine the maximum class size
max_class_size <- max(table(train_data$Test.Results))

# Function to oversample a class
oversample_class <- function(data, class_name, target_size) {
  class_data <- data %>% filter(Test.Results == class_name)
  oversampled_data <- class_data[sample(nrow(class_data), target_size, replace = TRUE), ]
  return(oversampled_data)
}

# Oversample each class to match the size of the largest class
oversampled_data <- lapply(unique(train_data$Test.Results), function(class_name) {
  oversample_class(train_data, class_name, max_class_size)
})

# Combine all oversampled data into a single dataset
train_data_balanced <- bind_rows(oversampled_data)

# Check the new class distribution
table(train_data_balanced$Test.Results)

# Install and load the randomForest package
install.packages("randomForest")
library(randomForest)

# Fit a Random Forest model with class weights
rf_model <- randomForest(Test.Results ~ Age + Gender + Blood.Type + Medical.Condition + Length.of.Stay + Billing.Amount + Admission.Type,
                         data = train_data,
                         ntree = 500,
                         importance = TRUE,
                         classwt = c("Normal" = 1, "Abnormal" = 1, "Inconclusive" = 1))  # Adjust weights based on class distribution

# Predict on the test data
rf_predictions <- predict(rf_model, test_data)

# Evaluate model performance with a confusion matrix
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$Test.Results)
print(conf_matrix_rf)

##Survival Analysis
surv_obj <- Surv(time = data$Length.of.Stay, event = data$Test.Results == "Abnormal")
km_fit <- survfit(surv_obj ~ Gender, data = data)

# Plot the survival curve
ggsurvplot(km_fit, data = data, pval = TRUE, risk.table = TRUE,
           title = "Survival Analysis by Gender",
           xlab = "Length of Stay (days)", ylab = "Survival Probability")



# Stratified by both Gender and Medical Condition
km_fit_stratified <- survfit(Surv(Length.of.Stay, Test.Results == "Abnormal") ~ Gender + strata(Medical.Condition), data = data)

# Plot the survival curve with stratification
ggsurvplot(km_fit_stratified, data = data, pval = TRUE, risk.table = TRUE,
           title = "Survival Analysis by Gender and Medical Condition",
           xlab = "Length of Stay (days)", ylab = "Survival Probability")



# Fit a Cox proportional hazards model with additional covariates
cox_model <- coxph(Surv(Length.of.Stay, Test.Results == "Abnormal") ~ Gender + Age + Medical.Condition + Blood.Type + Admission.Type, data = data)

# Summary of the model to see hazard ratios and significance
summary(cox_model)

# Plot the adjusted survival curves by Gender
ggsurvplot(survfit(cox_model, newdata = data.frame(Gender = c("Male", "Female"), Age = median(data$Age), Medical.Condition = "Hypertension", Blood.Type = "O+", Admission.Type = "Emergency")),
           data = data, pval = TRUE, risk.table = TRUE,
           title = "Adjusted Survival Curves by Gender",
           xlab = "Length of Stay (days)", ylab = "Survival Probability")


# Add an interaction term between Gender and Medical Condition
cox_model_interaction <- coxph(Surv(Length.of.Stay, Test.Results == "Abnormal") ~ Gender * Medical.Condition + Age + Blood.Type + Admission.Type, data = data)

# Summary of the model to interpret interaction effects
summary(cox_model_interaction)

# Plot the adjusted survival curves based on the interaction model
ggsurvplot(survfit(cox_model_interaction, newdata = data.frame(Gender = c("Male", "Female"), Medical.Condition = "Hypertension", Age = median(data$Age), Blood.Type = "O+", Admission.Type = "Emergency")),
           data = data, pval = TRUE, risk.table = TRUE,
           title = "Adjusted Survival Curves by Gender and Medical Condition",
           xlab = "Length of Stay (days)", ylab = "Survival Probability")


# Check the proportional hazards assumption
cox.zph(cox_model_interaction)

# Refine by categorizing Age into groups
data$AgeGroup <- cut(data$Age, breaks = c(0, 18, 35, 50, 65, 100), labels = c("0-18", "19-35", "36-50", "51-65", "66+"))
cox_model_age_group <- coxph(Surv(Length.of.Stay, Test.Results == "Abnormal") ~ Gender * Medical.Condition + AgeGroup + Blood.Type + Admission.Type, data = data)
summary(cox_model_age_group)
