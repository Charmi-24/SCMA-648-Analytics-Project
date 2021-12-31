# Loading the file with all required the pre-processing  
load("df_Q3.rda")

library(dplyr)

# 2. Determine the mean and median of the borrowers' months since last public 
# record (mths_since_last_record).  What is the number of observations for which 
# this value is missing?  Create a new variable where the value is imputed with the 
# median.  What are the mean and median after imputation?

summary(df$mths_since_last_record)

df$new_var <- df$mths_since_last_record

df$new_var[is.na(df$new_var)] <- median(df$new_var, na.rm=TRUE)

summary(df$new_var)

# 3. Plot a histogram of the settlement amount for loans (for those with a settlement) 
# and determine if the distribution is skewed.  If so, create a transformation of the 
# settlement amount data and create a histogram of the result.  Does the new data 
# appear to be normally distributed?

summary(df$settlement_amount)

hist(df$settlement_amount, col="blue", xlab="amount",
     ylab="Frequency", main="Histogram of settlement amt")

hist(log(df$settlement_amount), col="blue", xlab="amount",
     ylab="Frequency", main="Histogram of settlement amt")

# 4. Create a boxplot of interest rate (int_rate) for each loan status (loan_status).
# Which status has loans with the highest median interest rate?  Do the interest rates
# for fully paid loans tend to be higher or lower than those that are charged off?

boxplot(df$int_rate ~ df$loan_status, xlab="Loan Status",ylab="int_rate", main="", col = 4)
