# data.R - Load and prepare the Telco Customer Churn dataset
# This script selects only the 10 focused columns needed for the dashboard

library(dplyr)
library(tidyr)

# Load the data
cat("Loading Telco Customer Churn dataset...\n")
df_raw <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv", 
                   stringsAsFactors = FALSE,
                   na.strings = c("", " ", "NA"))

cat(sprintf("Original dataset: %d rows, %d columns\n", nrow(df_raw), ncol(df_raw)))

# Select only the 10 focused columns as specified in the documentation
# Focused columns: Churn, tenure, Contract, MonthlyCharges, TotalCharges,
# PaymentMethod, InternetService, OnlineSecurity, TechSupport, StreamingTV

df_clean <- df_raw %>%
  select(
    Churn,                    # Target variable
    tenure,                   # Customer tenure in months
    Contract,                 # Month-to-month, One year, Two year
    MonthlyCharges,           # Monthly bill amount
    TotalCharges,             # Total lifetime charges
    PaymentMethod,            # Payment method (e.g., Electronic check)
    InternetService,          # DSL, Fiber optic, No
    OnlineSecurity,           # Yes, No, No internet service
    TechSupport,              # Yes, No, No internet service
    StreamingTV               # Yes, No, No internet service
  )

cat(sprintf("After column selection: %d rows, %d columns\n", nrow(df_clean), ncol(df_clean)))

# Data Cleaning Steps

# 1. Convert TotalCharges from character to numeric
#    (Some values may be empty strings or spaces)
df_clean$TotalCharges <- as.numeric(df_clean$TotalCharges)

# 2. Check for missing values before removal
rows_before <- nrow(df_clean)
missing_totalcharges <- sum(is.na(df_clean$TotalCharges))
cat(sprintf("Rows with missing TotalCharges: %d\n", missing_totalcharges))

# 3. Remove rows with NA in TotalCharges (these are invalid records)
df_clean <- df_clean %>%
  filter(!is.na(TotalCharges))

rows_after <- nrow(df_clean)
cat(sprintf("Rows removed due to missing TotalCharges: %d\n", rows_before - rows_after))
cat(sprintf("Clean dataset: %d rows\n", nrow(df_clean)))

# 4. Convert Churn to factor for modeling (No=0, Yes=1 for binary classification)
df_clean$Churn <- factor(df_clean$Churn, levels = c("No", "Yes"))

# 5. Ensure proper factor levels for categorical variables
df_clean$Contract <- factor(df_clean$Contract, 
                            levels = c("Month-to-month", "One year", "Two year"))

df_clean$PaymentMethod <- factor(df_clean$PaymentMethod)

df_clean$InternetService <- factor(df_clean$InternetService,
                                   levels = c("DSL", "Fiber optic", "No"))

# For add-on services, keep the original levels including "No internet service"
df_clean$OnlineSecurity <- factor(df_clean$OnlineSecurity,
                                  levels = c("Yes", "No", "No internet service"))

df_clean$TechSupport <- factor(df_clean$TechSupport,
                               levels = c("Yes", "No", "No internet service"))

df_clean$StreamingTV <- factor(df_clean$StreamingTV,
                               levels = c("Yes", "No", "No internet service"))

# 6. Create derived metrics for KPIs (will be used in multiple tabs)
total_customers <- nrow(df_clean)
churned_customers <- sum(df_clean$Churn == "Yes")
retained_customers <- sum(df_clean$Churn == "No")
churn_rate <- round(100 * churned_customers / total_customers, 1)

# Revenue at Risk: Total monthly charges from churned customers
revenue_at_risk <- sum(df_clean$MonthlyCharges[df_clean$Churn == "Yes"])

# Total monthly revenue from all customers
total_monthly_revenue <- sum(df_clean$MonthlyCharges)

# Average tenure for churned vs retained
avg_tenure_churned <- round(mean(df_clean$tenure[df_clean$Churn == "Yes"]), 1)
avg_tenure_retained <- round(mean(df_clean$tenure[df_clean$Churn == "No"]), 1)

# Store these metrics as a list for use in UI
dashboard_metrics <- list(
  total_customers = total_customers,
  churned_customers = churned_customers,
  retained_customers = retained_customers,
  churn_rate = churn_rate,
  revenue_at_risk = revenue_at_risk,
  total_monthly_revenue = total_monthly_revenue,
  avg_tenure_churned = avg_tenure_churned,
  avg_tenure_retained = avg_tenure_retained
)

# 7. Data validation summary
cat("\n=== Data Validation Summary ===\n")
cat(sprintf("Total customers: %d\n", total_customers))
cat(sprintf("Churned customers: %d (%.1f%%)\n", churned_customers, churn_rate))
cat(sprintf("Retained customers: %d (%.1f%%)\n", retained_customers, 100 - churn_rate))
cat(sprintf("Revenue at Risk: $%s\n", formatC(revenue_at_risk, format = "f", digits = 0, big.mark = ",")))
cat(sprintf("Average Monthly Bill: $%.2f\n", mean(df_clean$MonthlyCharges)))
cat(sprintf("Average Tenure: %.1f months\n", mean(df_clean$tenure)))
cat("\nColumn types after cleaning:\n")
print(sapply(df_clean, class))

# 8. Quick check for any remaining issues
cat("\n=== Missing Values Check ===\n")
missing_values <- colSums(is.na(df_clean))
if (sum(missing_values) > 0) {
  print(missing_values[missing_values > 0])
} else {
  cat("No missing values found in the cleaned dataset.\n")
}

# 9. Display distribution of key variables
cat("\n=== Churn Distribution ===\n")
print(table(df_clean$Churn))

cat("\n=== Contract Type Distribution ===\n")
print(table(df_clean$Contract))

cat("\n=== Internet Service Distribution ===\n")
print(table(df_clean$InternetService))

cat("\n=== Payment Method Distribution ===\n")
print(table(df_clean$PaymentMethod))

# Final dataset is stored in 'df' variable (as required by app.R)
df <- df_clean

# Optional: Save cleaned data to a new CSV for faster loading in the future
# write.csv(df, "telco_data_cleaned.csv", row.names = FALSE)

cat("\n✅ Data loading and cleaning complete!\n")
cat(sprintf("Final dataset 'df' has %d rows and %d columns\n", nrow(df), ncol(df)))
cat("Columns in final dataset:\n")
print(names(df))