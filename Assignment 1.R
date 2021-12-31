col_names <-scan("LoanStats_securev1_2017Q3.csv", 
                 what="character", 
                 skip=1, 
                 nlines=1, 
                 sep=",")

character_columns <-c("id", "member_id", "emp_title", "issue_d", "url","desc", 
                      "zip_code", "addr_state", "earliest_cr_line", "last_pymnt_d",
                      "next_pymnt_d", "last_credit_pull_d", "sec_app_earliest_cr_line",
                      "hardship_type", "hardship_reason", "hardship_start_date",
                      "hardship_end_date", "payment_plan_start_date",
                      "debt_settlement_flag_date", "settlement_date")

factor_columns <-c("term", "grade", "sub_grade", "emp_length","home_ownership", 
                   "verification_status", "loan_status","pymnt_plan", "purpose", 
                   "title", "initial_list_status","policy_code", "application_type", 
                   "verification_status_joint","hardship_flag", "hardship_status", 
                   "hardship_loan_status","disbursement_method", "debt_settlement_flag", 
                   "settlement_status")

my_col_classes <-character(length(col_names))
my_col_classes[col_names%in%character_columns] <- "character"
my_col_classes[col_names%in%factor_columns] <- "factor"
my_col_classes[!(col_names%in% c(character_columns, factor_columns))] <- NA

df <- read.table("LoanStats_securev1_2017Q3.csv", 
                          skip=1, 
                          sep=",", 
                          nrows=122701,
                          colClasses = my_col_classes, 
                          header=TRUE)

df$revol_util <-as.character(df$revol_util)
df$revol_util <-sub("%", "", df$revol_util)
df$revol_util <-as.numeric(df$revol_util)
df$int_rate <-as.character(df$int_rate)
df$int_rate <-sub("%", "", df$int_rate)
df$int_rate <-as.numeric(df$int_rate)

date_columns <-c("issue_d", "last_pymnt_d", "next_pymnt_d", "last_credit_pull_d",
                 "sec_app_earliest_cr_line", "hardship_start_date", "hardship_end_date",
                 "payment_plan_start_date", "debt_settlement_flag_date", "settlement_date",
                 "earliest_cr_line")

df[,date_columns] <-lapply(df[,date_columns],function(x)  {strptime(paste("1", x), "%d %b-%Y")})

save(df, file="df_Q3.rda")

View(df)

loan_s <- table (df$loan_status)
View(loan_s)                        

write.table(loan_s, file = 'loan_status.txt',
            sep = ',',
            quote = FALSE,
            row.names = FALSE)





