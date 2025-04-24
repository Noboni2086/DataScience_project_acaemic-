# Load Required Packages
install.packages(c("dplyr", "tidyr", "ggplot2", "caret", "data.table", "DescTools"), dependencies = TRUE)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(data.table)
library(DescTools)

#Dataset Load
df <- fread("heart_disease_uci - modified.csv")

# Initial Exploration
head(df)
str(df)
summary(df)

# missing Values
colSums(is.na(df))  # Check missing

fill_missing <- function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else {
    x[is.na(x)] <- names(which.max(table(x)))
  }
  return(x)
}
df <- df[, lapply(.SD, fill_missing)]

#detect Invalid Data

setDT(df)

# Check for invalid values 
invalid_age <- df[age < 0 | age > 120]
invalid_trestbps <- df[trestbps < 50 | trestbps > 250]
invalid_chol <- df[chol < 0 | chol > 600]

# Display the number of invalid rows
cat("Invalid age entries:", nrow(invalid_age), "\n")
cat("Invalid trestbps entries:", nrow(invalid_trestbps), "\n")
cat("Invalid cholesterol entries:", nrow(invalid_chol), "\n")

# Optional: print invalid rows
print(invalid_age)
print(invalid_trestbps)
print(invalid_chol)


df$chol[df$chol < 0] <- mean(df$chol[df$chol > 0])
df$trestbps[df$trestbps < 0] <- mean(df$trestbps[df$trestbps > 0])


# Outlier Detection and Handling
detect_outliers <- function(x) {
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    return(sum(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)))
  } else {
    return(0)
  }
}
sapply(df, detect_outliers)

cap_outliers <- function(x) {
  if (is.numeric(x)) {
    Q1 <- quantile(x, 0.25)
    Q3 <- quantile(x, 0.75)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    x[x < lower] <- lower
    x[x > upper] <- upper
  }
  return(x)
}
df <- df[, lapply(.SD, cap_outliers)]

# convert Attributes (max two conversions)
# Numeric → Categorical
df$age_group <- ifelse(df$age < 50, "Under 50", "50 and Above")

# Categorical → Numeric
df$sex_num <- ifelse(df$sex == "male", 1, 0.5)

# Normalization (any continuous attribute)
df$chol_norm <- (df$chol - min(df$chol)) / (max(df$chol) - min(df$chol))

# Remove Duplicates
sum(duplicated(df))
df <- df[!duplicated(df), ]

# Apply Filtering Method
df_over_60 <- df[df$age > 60, ]


# handle Imbalanced Dataset (Oversampling)
table(df$num)
prop.table(table(df$num))

class_0 <- df[df$num == 0, ]
class_1 <- df[df$num == 1, ]
diff <- nrow(class_0) - nrow(class_1)
set.seed(123)
oversampled_class_1 <- class_1[sample(nrow(class_1), diff, replace = TRUE), ]
df_balanced <- rbind(df, oversampled_class_1)
table(df_balanced$num)

# split Dataset (Training & Testing)
df_balanced$num <- as.factor(df_balanced$num)
set.seed(42)
train_index <- createDataPartition(df_balanced$num, p = 0.8, list = FALSE)
train_data <- df_balanced[train_index, ]
test_data <- df_balanced[-train_index, ]

# central Tendency (Two Numeric, Two Categorical)
get_mode <- function(v) {
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

# Numeric attributes: age, chol
numeric_attrs <- c("age", "chol")
for (attr in numeric_attrs) {
  values <- df_balanced[[attr]]
  cat("\nCentral Tendency for", attr, ":\n")
  cat("Mean:", mean(values), "\n")
  cat("Median:", median(values), "\n")
  cat("Mode:", get_mode(values), "\n")
}

# Categorical attributes: sex_num, cp (converted to numeric)
df_balanced$cp_num <- ifelse(df_balanced$cp == "asymptomatic", 1,
                             ifelse(df_balanced$cp == "atypical angina", 2,
                                    ifelse(df_balanced$cp == "non-anginal", 3,
                                           ifelse(df_balanced$cp == "typical angina", 4, NA))))

categorical_attrs <- c("sex_num", "cp_num")
for (attr in categorical_attrs) {
  values <- df_balanced[[attr]]
  cat("\nCentral Tendency for", attr, ":\n")
  cat("Mean:", mean(values), "\n")
  cat("Median:", median(values), "\n")
  cat("Mode:", get_mode(values), "\n")
}

# spread (Range, IQR, Variance, Std Dev) for 2 Attributes
attributes_to_check <- c("age", "chol")
for (attr in attributes_to_check) {
  values <- df_balanced[[attr]]
  cat("\nSpread for", attr, ":\n")
  cat("Range:", max(values) - min(values), "\n")
  Q1 <- quantile(values, 0.25)
  Q3 <- quantile(values, 0.75)
  cat("IQR:", Q3 - Q1, "\n")
  mean_val <- mean(values)
  variance <- sum((values - mean_val)^2) / (length(values) - 1)
  cat("Variance:", variance, "\n")
  cat("Standard Deviation:", sqrt(variance), "\n")
}




