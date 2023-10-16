
# Import & Load Library Packages.

## Install the library packages (if needed).
install.packages("magrittr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("broom")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages('fastDummies')
install.packages("PerformanceAnalytics")
#install.packages("InformationValue")
install.packages("caret", dependencies = TRUE)
install.packages("pROC")
install.packages("caTools")
install.packages("car")
install.packages("plyr")
install.packages("mice")
install.packages("VIM")
#install.packages("DMwR2")
install.packages("heatmaply")
#install.packages("smotefamily")
#install.packages("ROSE")
install.packages("randomForest")

## Import the library packages. 
library(magrittr) # Needs to be run everytime when want to use %>% function.
library(dplyr) # alternatively, this also loads %>% function.
library(tidyr) # Works with dplyr library package to tidy data.
library(tidyverse) # Used in performing sub-setting, transformations, and visualizations.
library(broom) # Summarizes key information about models in tidy library package.
library(ggplot2) # Used for data visualization.
library(cowplot) # Used to improve ggplot2 data visualization.
library(corrplot) # To build correlation matrix.
library(ggcorrplot) # Used to easily visualize a correlation matrix using ggplot2.
library(fastDummies) # Used to create dummy variables (especially for categorical variables)
library(PerformanceAnalytics) # Used for Chart Correlation
#library(InformationValue) # Used for model assessment and evaluation
library(caret) # To view confusion matrix.
library(pROC) # This is to analyze and compare ROC curves.
library(caTools) # Used to calculate AUC using ROC plot
library(car) # Used to make use of the box-tidwell function and calculate VIF values 
library(plyr)
#library(ROSE)
library(mice)
library(VIM)
#library(DMwR)
#library(DMwR2)
library(heatmaply)
#library(smotefamily)
library(randomForest)

# Import the Heart Disease Data Set.

## setwd() before to check before importing.
## setwd("C:/Users/jarro/Documents/University Related Stuff/Master in Data Science/8. MDA5023 - Forecasting Analytics")

## Import the data set into a data frame using the read.csv() function and read NA as missing values.
df_loan <- read.csv(file="loan_dataset.csv", na.strings=c("", "NA"), 
                    header=TRUE)



# Basic Data Exploration.

## Print the first 6 rows of data frame.
head(df_loan) 

## Display the variable's names.
names(df_loan) 

## Display the list structure.
str(df_loan) 

## Display the basic descriptive statistics.
summary(df_loan) 

## Display the number of rows.
nrow(df_loan) 

## Display the number of columns.
ncol(df_loan) 

## Display the number of missing (NULL/NA) values.
colSums(is.na(df_loan)) 



# EDA

## Target Variable Frequency Bar Chart
ggplot(data = df_loan, aes(x = Loan_Status)) + 
  geom_bar(fill = c("red", "green"), color = "black") + 
  labs(title = "Loan Status Frequency Bar Chart", x = "Loan Status", y = "Frequency")

dev.off()

## Histograms & Boxplots for Numerical Variables
par(mfrow=c(2,2))
hist(df_loan$ApplicantIncome, 
     main="Histogram for Applicant Income", 
     xlab="Applicant Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,80000),
     breaks=50)
boxplot(df_loan$ApplicantIncome, col='maroon',xlab = 'ApplicantIncome', main = 'Box Plot for Applicant Income')

hist(df_loan$CoapplicantIncome, 
     main="Histogram for Coapplicant Income", 
     xlab="Coapplicant Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,80000),
     breaks=50)
boxplot(df_loan$CoapplicantIncome, col='maroon',xlab = 'CoapplicantIncome', main = 'Box Plot for Coapplicant Income')

dev.off()

par(mfrow=c(2,2))
hist(df_loan$LoanAmount, 
     main="Histogram for Loan Amount", 
     xlab="Loan Amount", 
     border="blue", 
     col="maroon",
     xlim=c(0,700),
     breaks=20)
boxplot(df_loan$LoanAmount, col='maroon',xlab = 'LoanAmount', main = 'Box Plot for Loan Amount')

## Convert chr to num datatype
df_loan$Total_Income <- as.numeric(gsub("[^0-9.]+", "", df_loan$Total_Income))

hist(df_loan$Total_Income, 
     main="Histogram for Total Income", 
     xlab="Total Income", 
     border="blue", 
     col="maroon",
     xlim=c(0,80000),
     breaks=50)
boxplot(df_loan$Total_Income, col='maroon',xlab = 'Total_Income', main = 'Box Plot for Total Income')

dev.off()

## Distribution of LoanAmount by Loan Status
data(df_loan, package="lattice")
ggplot(data=df_loan, aes(x=LoanAmount, fill=Education)) +
  geom_density() +
  facet_grid(Education~.) 

dev.off()

## Visualizing Categorical Variables
par(mfrow=c(3,3))
counts <- table(df_loan$Loan_Status, df_loan$Gender)
barplot(counts, main="Loan Status by Gender",
        xlab="Gender", col=c("darkgrey","maroon"),
        legend = rownames(counts))
counts2 <- table(df_loan$Loan_Status, df_loan$Education)
barplot(counts2, main="Loan Status by Education",
        xlab="Education", col=c("darkgrey","maroon"),
        legend = rownames(counts2))
counts3 <- table(df_loan$Loan_Status, df_loan$Married)
barplot(counts3, main="Loan Status by Married",
        xlab="Married", col=c("darkgrey","maroon"),
        legend = rownames(counts3))
counts4 <- table(df_loan$Loan_Status, df_loan$Self_Employed)
barplot(counts4, main="Loan Status by Self Employed",
        xlab="Self_Employed", col=c("darkgrey","maroon"),
        legend = rownames(counts4))
counts5 <- table(df_loan$Loan_Status, df_loan$Property_Area)
barplot(counts5, main="Loan Status by Property_Area",
        xlab="Property_Area", col=c("darkgrey","maroon"),
        legend = rownames(counts5))
counts6 <- table(df_loan$Loan_Status, df_loan$Credit_History)
barplot(counts6, main="Loan Status by Credit_History",
        xlab="Credit_History", col=c("darkgrey","maroon"),
        legend = rownames(counts6))
counts7 <- table(df_loan$Loan_Status, df_loan$Dependents)
barplot(counts7, main="Loan Status by Dependents",
        xlab="Dependents", col=c("darkgrey","maroon"),
        legend = rownames(counts7))

dev.off()



# Data Pre-processing 

## (1) Handling Missing Values: Mice Imputation

## Remove column X and Loan_ID
df_loan2 <- subset(df_loan, select = -c(X, Loan_ID))

## Standardise data value 3+ to 3
df_loan2$Dependents <- revalue(df_loan2$Dependents, c("3+"="3"))

## Convert chr to num datatype
## df_loan2$Total_Income <- as.numeric(gsub("[^0-9.]+", "", df_loan2$Total_Income))

# Mice Imputation: Use mice() function to impute missing values
md.pattern(df_loan2)
### Factorize categorical features
#df_loan2$Gender <- as.factor(df_loan2$Gender)
#df_loan2$Married <- as.factor(df_loan2$Married)
#df_loan2$Self_Employed <- as.factor(df_loan2$Self_Employed)
#df_loan2$Dependents <- as.factor(df_loan2$Dependents)

# select columns with categorical variables
cat_cols <- c("Gender", "Married", "Dependents", "Education", "Self_Employed",
              "Credit_History", "Property_Area", "Loan_Status")

# factorize categorical columns
df_loan2 <- df_loan2 %>%
  mutate_if(names(.) %in% cat_cols, factor)

### Use mice function with method cart for Classification and regression trees
imputed <- mice(df_loan2, method = "cart", seed = 123)
### Save the imputed data
df_loan2 <- complete(imputed)
### Check for missing values in the imputed data
colSums(is.na(df_loan2)) 

#########################################################################################

## (2) Handling Missing Values: Deletion 
## Remove column X and Loan_ID
df_loan2_2 <- subset(df_loan, select = -c(X, Loan_ID))

## Standardise data value 3+ to 3
df_loan2_2$Dependents <- revalue(df_loan2_2$Dependents, c("3+"="3"))

## Convert chr to num datatype
## df_loan2$Total_Income <- as.numeric(gsub("[^0-9.]+", "", df_loan2$Total_Income))

# Remove rows with missing values in selected columns
df_loan2_2 <- df_loan2_2[complete.cases(df_loan2_2[, c("Gender", "Married", "Self_Employed", "Dependents", "LoanAmount", "Loan_Amount_Term", "Credit_History", "Total_Income")]), ]


## Remove column X and Loan_ID
df_loan2_3 <- subset(df_loan, select = -c(X, Loan_ID))

## Standardise data value 3+ to 3
df_loan2_3$Dependents <- revalue(df_loan2_3$Dependents, c("3+"="3"))


# Remove rows with missing values in selected columns
df_loan2_3 <- df_loan2_3[complete.cases(df_loan2_3[, c("LoanAmount", "Loan_Amount_Term")]), ]


# Calculate Imputed mean and standard deviation for LoanAmount and Loan_Amount_Term
mean_la <- mean(df_loan2$LoanAmount, na.rm = TRUE)
sd_la <- sd(df_loan2$LoanAmount, na.rm = TRUE)
# Print the results
cat("Mean of LoanAmount:", mean_la, "\n")
cat("Standard deviation of LoanAmount:", sd_la, "\n")

mean_la <- mean(df_loan2$Loan_Amount_Term, na.rm = TRUE)
sd_la <- sd(df_loan2$Loan_Amount_Term, na.rm = TRUE)
# Print the results
cat("Mean of Loan_Amount_Term:", mean_la, "\n")
cat("Standard deviation of Loan_Amount_Term:", sd_la, "\n")


# Calculate Deleted mean and standard deviation for LoanAmount and Loan_Amount_Term
mean_la <- mean(df_loan2_3$LoanAmount, na.rm = TRUE)
sd_la <- sd(df_loan2_3$LoanAmount, na.rm = TRUE)
# Print the results
cat("Mean of LoanAmount:", mean_la, "\n")
cat("Standard deviation of LoanAmount:", sd_la, "\n")

mean_la <- mean(df_loan2_3$Loan_Amount_Term, na.rm = TRUE)
sd_la <- sd(df_loan2_3$Loan_Amount_Term, na.rm = TRUE)
# Print the results
cat("Mean of Loan_Amount_Term:", mean_la, "\n")
cat("Standard deviation of Loan_Amount_Term:", sd_la, "\n")
###################################################################################################





## Handling Outliers 
par(mfrow=c(2,2))
### Loan Amount
df_loan2$LogLoanAmount <- log(df_loan2$LoanAmount)
hist(df_loan2$LogLoanAmount, 
     main="Histogram for Loan Amount (Log)", 
     xlab="Loan Amount (Log)", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=20, prob = TRUE)
lines(density(df_loan2$LogLoanAmount), col='black', lwd=3)
boxplot(df_loan2$LogLoanAmount, col='maroon',xlab = 'Loan Amount (Log)', main = 'Box Plot for Loan Amount (Log)')
### Total Income
df_loan2$LogTotalIncome <- log(df_loan2$Total_Income)
hist(df_loan2$LogTotalIncome, 
     main="Histogram for Total Income (Log)", 
     xlab="Total Income (Log)", 
     border="blue", 
     col="maroon",
     las=1, 
     breaks=20, prob = TRUE)
lines(density(df_loan2$LogTotalIncome), col='black', lwd=3)
boxplot(df_loan2$LogTotalIncome, col='maroon',xlab = 'Total Income (Log)', main = 'Box Plot for Total Income (Log)')

dev.off()



# Feature Engineering 

## Remove unwanted columns / variables
df_loan3 <- subset(df_loan2, select = -c(ApplicantIncome, CoapplicantIncome, LoanAmount, Total_Income))

## Get Dummies
### Convert categorical variables to binary using get_dummies
df_loan3 <- dummy_cols(df_loan3, select_columns = c("Gender", "Married", "Education", "Self_Employed", "Credit_History", "Loan_Status"), remove_first_dummy = TRUE)
### Rename the column
colnames(df_loan3)[colnames(df_loan3) == "Education_Not Graduate"] <- "Education_Not_Graduate"
colnames(df_loan3)[colnames(df_loan3) == "Loan_Status_Y"] <- "Loan_Status"
### Remove original categorical columns
df_loan3 <- subset(df_loan3, select = -c(Gender, Married, Education, Self_Employed, Credit_History, Loan_Status))
### View updated dataset
head(df_loan3)

df_loan3 <- df_loan3 %>%
  mutate(Gender_Male = as.numeric(Gender_Male),
         Married_Yes = as.numeric(Married_Yes),
         Education_Not_Graduate = as.numeric(Education_Not_Graduate),
         Self_Employed_Yes = as.numeric(Self_Employed_Yes),
         Credit_History_1 = as.numeric(Credit_History_1),
         Loan_Status = as.numeric(Loan_Status))

### Convert Dependents to categorical and get dummies with first column removed
dependents_dummies <- data.frame(model.matrix(~ factor(df_loan3$Dependents, levels = c('0', '1', '2', '3')))[,-1])
colnames(dependents_dummies) <- c("Dependents_1", "Dependents_2", "Dependents_3")
df_loan3 <- cbind(df_loan3, dependents_dummies)
### Convert Property_Area to categorical and get dummies with first column removed
property_dummies <- data.frame(model.matrix(~ factor(df_loan3$Property_Area))[,-1])
colnames(property_dummies) <- c("Property_Area_Rural", "Property_Area_Semiurban")
df_loan3 <- cbind(df_loan3, property_dummies)
### Remove original categorical columns
df_loan3 <- subset(df_loan3, select = -c(Dependents, Property_Area))

## Handling Target Class Imbalance
### Check class distribution of target variable
#df_loan3$Loan_Status <- as.integer(df_loan3$Loan_Status == "Y")
### Check the distribution
#table(df_loan3$Loan_Status)
### Balance the target class using ROSE
#df_loan3 <- SMOTE(Loan_Status ~ ., df_loan3, perc.over = 100, perc.under = 200)
#data_balanced_both <- ovun.sample(Loan_Status ~ ., data = df_loan3, method = "both", p=0.5,                             N=1000, seed = 1)$data
#table(data_balanced_both$Loan_Status)
### Check class distribution of target variable in balanced dataset
#table(df_loan3$Loan_Status)


## Check Correlation Coefficients
### Subset dataframe to remove target variable
df_loan_corr <- df_loan3[, !colnames(df_loan3) %in% c("Loan_Status")]
### Check Correlation Coefficients
heatmaply_cor(x = cor(df_loan_corr), xlab = "Features",
              ylab = "Features", k_col = 2, k_row = 2)

dev.off()

# Data Modeling # Evaluation

# Split data into training and testing sets
set.seed(123)
trainIndex_60 <- createDataPartition(df_loan3$Loan_Status, p = 0.6, list = FALSE, times = 1)
trainIndex_70 <- createDataPartition(df_loan3$Loan_Status, p = 0.7, list = FALSE, times = 1)
trainIndex_80 <- createDataPartition(df_loan3$Loan_Status, p = 0.8, list = FALSE, times = 1)

train_60 <- df_loan3[trainIndex_60,]
test_60 <- df_loan3[-trainIndex_60,]

train_70 <- df_loan3[trainIndex_70,]
test_70 <- df_loan3[-trainIndex_70,]

train_80 <- df_loan3[trainIndex_80,]
test_80 <- df_loan3[-trainIndex_80,]

# Fit Random Forest models and make predictions
set.seed(123)
rf_model_60 <- randomForest(as.factor(Loan_Status) ~ ., data = train_60)
rf_pred_60 <- predict(rf_model_60, newdata = test_60)

rf_model_70 <- randomForest(as.factor(Loan_Status) ~ ., data = train_70)
rf_pred_70 <- predict(rf_model_70, newdata = test_70)

rf_model_80 <- randomForest(as.factor(Loan_Status) ~ ., data = train_80)
rf_pred_80 <- predict(rf_model_80, newdata = test_80)

# Calculate evaluation metrics
accuracy_60 <- confusionMatrix(rf_pred_60, as.factor(test_60$Loan_Status))$overall['Accuracy']
precision_60 <- confusionMatrix(rf_pred_60, as.factor(test_60$Loan_Status))$byClass['Pos Pred Value']
recall_60 <- confusionMatrix(rf_pred_60, as.factor(test_60$Loan_Status))$byClass['Sensitivity']
f1_score_60 <- confusionMatrix(rf_pred_60, as.factor(test_60$Loan_Status))$byClass['F1']
auc_60 <- roc(test_60$Loan_Status, predict(rf_model_60, newdata = test_60, type = "prob")[,2])$auc

accuracy_70 <- confusionMatrix(rf_pred_70, as.factor(test_70$Loan_Status))$overall['Accuracy']
precision_70 <- confusionMatrix(rf_pred_70, as.factor(test_70$Loan_Status))$byClass['Pos Pred Value']
recall_70 <- confusionMatrix(rf_pred_70, as.factor(test_70$Loan_Status))$byClass['Sensitivity']
f1_score_70 <- confusionMatrix(rf_pred_70, as.factor(test_70$Loan_Status))$byClass['F1']
auc_70 <- roc(test_70$Loan_Status, predict(rf_model_70, newdata = test_70, type = "prob")[,2])$auc

accuracy_80 <- confusionMatrix(rf_pred_80, as.factor(test_80$Loan_Status))$overall['Accuracy']
precision_80 <- confusionMatrix(rf_pred_80, as.factor(test_80$Loan_Status))$byClass['Pos Pred Value']
recall_80 <- confusionMatrix(rf_pred_80, as.factor(test_80$Loan_Status))$byClass['Sensitivity']
f1_score_80 <- confusionMatrix(rf_pred_80, as.factor(test_80$Loan_Status))$byClass['F1']
auc_80 <- roc(test_80$Loan_Status, predict(rf_model_80, newdata = test_80, type = "prob")[,2])$auc

# Print evaluation metrics
cat("60% training set accuracy:", accuracy_60, "\n")
cat("60% training set precision:", precision_60, "\n")
cat("60% training set recall:", recall_60, "\n")
cat("60% training set F1 score:", f1_score_60, "\n")
cat("60% training set AUC:", auc_60, "\n\n")

cat("70% training set accuracy:", accuracy_70, "\n")
cat("70% training set precision:", precision_70, "\n")
cat("70% training set recall:", recall_70, "\n")
cat("70% training set F1 score:", f1_score_70, "\n")
cat("70% training set AUC:", auc_70, "\n\n")

cat("80% training set accuracy:", accuracy_80, "\n")
cat("80% training set precision:", precision_80, "\n")
cat("80% training set recall:", recall_80, "\n")
cat("80% training set F1 score:", f1_score_80, "\n")
cat("80% training set AUC:", auc_80, "\n")


# Create data frame
df_eval <- data.frame(
  train_set = c("60%", "70%", "80%"),
  accuracy = c(accuracy_60, accuracy_70, accuracy_80),
  precision = c(precision_60, precision_70, precision_80),
  recall = c(recall_60, recall_70, recall_80),
  f1_score = c(f1_score_60, f1_score_70, f1_score_80),
  auc = c(auc_60, auc_70, auc_80)
)

# Print data frame
print(df_eval)


# Calculate ROC curve for each model
roc_60 <- roc(test_60$Loan_Status, as.numeric(rf_pred_60))
roc_70 <- roc(test_70$Loan_Status, as.numeric(rf_pred_70))
roc_80 <- roc(test_80$Loan_Status, as.numeric(rf_pred_80))

# Plot ROC curves
plot(roc_60, col = "blue", print.auc = TRUE, auc.polygon = TRUE, legacy.axes = TRUE, xlim = c(0, 1), ylim = c(0, 1))
lines(roc_70, col = "red", print.auc = TRUE, auc.polygon = TRUE)
lines(roc_80, col = "green", print.auc = TRUE, auc.polygon = TRUE)
legend("bottomright", c("60% train", "70% train", "80% train"), lty = 1, col = c("blue", "red", "green"))




# Create variable importance plots
par(mfrow=c(1,3))
varImpPlot(rf_model_60, main="60% train")
varImpPlot(rf_model_70, main="70% train")
varImpPlot(rf_model_80, main="80% train")

dev.off()

