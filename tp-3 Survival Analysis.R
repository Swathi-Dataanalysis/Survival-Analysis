# Load libraries
library(survival)
library(survminer)
library(tidyverse)
library(devtools)
# Set the working directory
setwd("/Users/rameshchidurala/Documents/DSC-520 Assignments")

# Load the data with fill = TRUE to handle lines with missing values
df <- read.table("/Users/rameshchidurala/Documents/DSC-520 Assignments/echocardiogram/echocardiogram.data", 
                 header = FALSE, sep = ',', na.strings = "?", fill = TRUE)

# Assign column names
colnames(df) <- c("Survival", "StillAlive", "AgeAtHeartAttack", "PericardialEffusion", 
                  "FractionalShortening", "Epss", "Lvdd", "WallMotionScore", 
                  "WallMotionIndex", "Mult", "Name", "Group", "AliveAt1")

# View the first few rows of the dataset
head(df)

# Display the structure and summary of the data
str(df)


# Summarize the count of missing values per column
missing_values_summary <- sapply(df, function(x) sum(is.na(x))) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  rename(Count = 2)

# Create a histogram of missing values with numbers on top of each bar
ggplot(missing_values_summary, aes(x = Variable, y = Count, fill = Variable)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = -0.5) +  # Adding numbers on top
  theme_minimal() +
  labs(title = "Histogram of Missing Values Per Column", x = "Variable", y = "Count of Missing Values") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Impute Missing Values with Mean for the specified columns
library(Hmisc)
library(dplyr)

COLUMNS <- c("AgeAtHeartAttack", "PericardialEffusion", "FractionalShortening", "Epss", "Lvdd", "WallMotionScore", "WallMotionIndex")

df_imputed <- df %>%
  mutate(across(all_of(COLUMNS), ~impute(., mean)))

COLUMNS_keep <- c("Survival", "StillAlive")

df_keep <- df[, COLUMNS_keep]
df_combined <- cbind(df_keep, df_imputed[COLUMNS])
#Drop rows where 'Survival' or 'StillAlive' is NA
df_combined <- df_combined %>% filter(!is.na(Survival) & !is.na(StillAlive))
# Print the dimensions (shape) of the dataframe
dim(df_combined)

summary(df_combined)
# Visualize the data using kable

knitr::kable(head(df))
#Formulate a Research Question
#"Is there a significant difference in survival between groups with different levels of pericardial effusion?"

# Kaplan-Meier Estimator
# Build a Surv object
surv_object <- Surv(time = df_combined$Survival, event = df_combined$StillAlive)

# Kaplan-Meier fit
km_fit <- survfit(surv_object ~ 1)

# Print the Kaplan-Meier fit
print(km_fit)
#Visualize Survival Data
#Create a Kaplan-Meier plot using ggsurvplot()
# Kaplan-Meier plot
ggsurvplot(km_fit, data = df_combined)
ggsurvplot(km_fit, data = df_combined, conf.int = TRUE,
           main = "Kaplan-Meier Survival Curve", xlab = "Time", ylab = "Survival Probability")

#Generate K-M Curves with Predictor
# Stratify analysis by PericardialEffusion
km_fit_strata <- survfit(surv_object ~ df_combined$PericardialEffusion)

# Display summary of strata
print(summary(km_fit_strata))


# Plot the strata
ggsurvplot(km_fit_strata, data = df_combined, palette = "Dark2",
           title = "Stratified KM Curve by Pericardial Effusion")

#Perform Log-Rank Analysis
#Test the difference between survival curves.
# Log-Rank Test
log_rank_test <- survdiff(surv_object ~ df_combined$PericardialEffusion)

# Examine log-rank output
print(log_rank_test)

# Plot K-M curve with log-rank test results
ggsurvplot(km_fit_strata, data = df_combined, palette = "Dark2", 
           title = "KM Curve with Log-Rank Test",
           surv.median.line = "hv", ggtheme = theme_minimal())

# Technical Report on Survival Analysis of Echocardiogram Data #

Introduction
This report details the survival analysis conducted on the Echocardiogram Data Set. The analysis aims to understand the time to event (in this case, death) post-heart attack and how various factors might influence survival rates.

Methodology
Data Preparation and Exploration

Data Loading and Cleaning: The dataset was loaded using R, and missing values were imputed with mean values for specific columns: Age at Heart Attack, Pericardial Effusion, Fractional Shortening, EPSS, LVDD, and Wall Motion Score.
Initial Exploration: The initial exploration included summarizing missing values and understanding the dataset's structure. A histogram was used to visualize missing values across different columns.
Survival Analysis

Kaplan-Meier Estimator: The Surv() function from the survival package was employed to create survival objects, and survfit() was used to generate Kaplan-Meier survival estimates.
Stratification: The analysis was further stratified by the Pericardial Effusion variable to understand its impact on survival.
Visualization: Kaplan-Meier survival curves were visualized using the ggsurvplot() function.
Statistical Testing

Log-Rank Test: A log-rank test (survdiff()) was performed to statistically compare survival distributions between patients with and without pericardial effusion.
Results
Overall Survival: The Kaplan-Meier plot for the overall survival showed the survival probability over time for the entire cohort.
Stratified Survival Analysis:
The survival analysis stratified by Pericardial Effusion revealed differences in survival probabilities between patients with and without this condition.
The survival curves illustrated a discernible separation, indicating a potential impact of pericardial effusion on survival.
Log-Rank Test:
The test yielded a chi-square value of 4.6 with a p-value of 0.03, suggesting a significant difference in survival distributions between the two groups.
This result indicates that pericardial effusion is a significant factor in determining survival post-heart attack.
Discussion
The survival analysis indicated that pericardial effusion plays a significant role in the survival of heart attack patients. Patients with pericardial effusion showed different survival probabilities compared to those without. The statistical significance of these differences was confirmed by the log-rank test.

It's crucial to acknowledge the limitations of the analysis, including potential biases in the dataset and the generalizability of the findings. Additionally, while per
icardial effusion appears to be a significant factor, it's important to consider other variables that might also influence survival post-heart attack.

Conclusion
The survival analysis conducted on the Echocardiogram Data Set demonstrates the utility of statistical methods in understanding patient outcomes following heart attacks. Specifically, the analysis highlights the significance of pericardial effusion as a factor influencing survival. This insight could be valuable for medical practitioners in assessing patient prognosis and tailoring treatment plans.

Future Work
Further research could involve a more comprehensive analysis, including other potentially influential factors such as lifestyle, medical history, and demographic variables. Additionally, advanced modeling techniques like Cox proportional hazards models could be employed to quantify the impact of various predictors on survival time.

Acknowledgments
This analysis was conducted using R and relied on packages such as survival, survminer, and tidyverse. The dataset was sourced from the Echocardiogram Data Set provided in the course resources.

