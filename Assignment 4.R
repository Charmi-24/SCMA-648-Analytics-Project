library(rpart)
library(dplyr)
library(ROCR)
library(caret)

set.seed(12345)

load("df_Q3.rda")

# Question 2

df1 <- df %>%
  dplyr::select(loan_status, loan_amnt, term, int_rate, home_ownership, annual_inc, 
                fico_range_high, tot_cur_bal, dti, application_type, total_bc_limit) %>%
  dplyr:: filter(loan_status %in% c("Fully Paid", "Charged Off")) %>%
  droplevels()

df1 <- df1 %>%
  dplyr::filter(annual_inc < 200000 & 
                  home_ownership != "NONE" &
                  home_ownership != "ANY" &
                  dti < 25 & 
                  tot_cur_bal < 500000 & 
                  total_bc_limit < 150000) %>%
  droplevels()

summary(df1$loan_status)
summary(df1)

train_rows <- createDataPartition(df1$loan_status, p=0.01, list=FALSE)

train_1 <- df1[train_rows,]
test_1 <- df1[-train_rows,]

summary(train_1$loan_status)

my_lend_weights <- numeric(nrow(train_1))
my_lend_weights[train_1$loan_status == "Fully Paid"] <- 1
my_lend_weights[train_1$loan_status == "Charged Off"] <- 4

my_lend_lr <- glm(loan_status ~ ., data=train_1, weights=my_lend_weights, 
                  family=binomial("logit"))

summary(train_1)
summary(test_1)

my_lend_lr_predict <- predict(my_lend_lr, newdata=test_1, type="response")
my_lend_lr_predict_class <- character(length(my_lend_lr_predict))
my_lend_lr_predict_class[my_lend_lr_predict < 0.5] <- "Charged Off"
my_lend_lr_predict_class[my_lend_lr_predict >= 0.5] <- "Fully Paid"
cm_1 = table(test_1$loan_status, my_lend_lr_predict_class)
cm_1

summary(my_lend_lr)

1-sum(diag(cm_1))/sum(cm_1)

my_lend_rpart <- rpart(loan_status ~ ., data=train_1, weights=my_lend_weights)
my_lend_rpart_predict <- predict(my_lend_rpart, newdata=test_1, type="class")
cm_2 = table(test_1$loan_status, my_lend_rpart_predict)
cm_2

my_lend_rpart$variable.importance

1-sum(diag(cm_2))/sum(cm_2)

# Question 3

df2 <- df %>%
  dplyr::select(loan_status, loan_amnt, funded_amnt_inv, term, int_rate, installment, grade, 
                emp_length, home_ownership, annual_inc, verification_status, purpose,
                title, dti, total_pymnt, delinq_2yrs, open_acc, pub_rec, last_pymnt_d, 
                last_pymnt_amnt, application_type, revol_bal, revol_util, recoveries) %>%
  dplyr:: filter(loan_status %in% c("Fully Paid", "Charged Off")) %>%
  droplevels()

summary(df2$loan_status)

summary(df2)

df2$emp_length <- as.character(df2$emp_length)
df2$emp_length <- sub("years", "", df2$emp_length)
df2$emp_length <- sub("< 1 year", "1", df2$emp_length)
df2$emp_length <- sub("1 year", "1", df2$emp_length)
df2$emp_length <- sub("10\\+ ", "10", df2$emp_length)
df2$emp_length[df2$emp_length == "n/a"] <- NA
df2$emp_length <- as.numeric(df2$emp_length)

df2$last_pymnt_d <- as.POSIXct(df2$last_pymnt_d)

train_2 <- df2[train_rows,]
test_2 <- df2[-train_rows,]

train_2 <- train_2 %>%
  dplyr::filter(annual_inc < 200000 & 
                  home_ownership != "NONE" &
                  home_ownership != "ANY" &
                  dti < 25 &
                  revol_bal < 50000) %>%
  droplevels()

test_2 <- test_2 %>%
  dplyr::filter(annual_inc < 200000 & 
                  home_ownership != "NONE" &
                  home_ownership != "ANY" &
                  dti < 25 &
                  revol_bal < 50000) %>%
  droplevels()

summary(df2)

summary(train_2$loan_status) 

train_2$last_pymnt_d[is.na(train_2$last_pymnt_d)] <- 
  median(train_2$last_pymnt_d,na.rm=TRUE)

test_2$last_pymnt_d[is.na(test_2$last_pymnt_d)] <-
  median(test_2$last_pymnt_d,na.rm=TRUE)

lend_med_impute <- preProcess(train_2, method="medianImpute")
train_2 <- predict(lend_med_impute, train_2)
test_2 <- predict(lend_med_impute, test_2)

summary(train_2)
summary(test_2)

summary(train_2$loan_status)

my_lend_weights_2 <- numeric(nrow(train_2))
my_lend_weights_2[train_2$loan_status == "Fully Paid"] <- 1
my_lend_weights_2[train_2$loan_status == "Charged Off"] <- 3

my_lend_lr_2 <- glm(loan_status ~ ., data=train_2, weights=my_lend_weights_2, 
                  family=binomial("logit"))

my_lend_lr_predict_2 <- predict(my_lend_lr_2, newdata=test_2, type="response")
my_lend_lr_predict_class_2 <- character(length(my_lend_lr_predict_2))
my_lend_lr_predict_class_2[my_lend_lr_predict_2 < 0.5] <- "Charged Off"
my_lend_lr_predict_class_2[my_lend_lr_predict_2 >= 0.5] <- "Fully Paid"
cm_3 = table(test_2$loan_status, my_lend_lr_predict_class_2)
cm_3

summary(my_lend_lr_2)

1-sum(diag(cm_3))/sum(cm_3)

my_lend_rpart_2 <- rpart(loan_status ~ ., data=train_2, weights=my_lend_weights_2)
my_lend_rpart_predict_2 <- predict(my_lend_rpart_2, newdata=test_2, type="class")
cm_4 = table(test_2$loan_status, my_lend_rpart_predict_2)
cm_4

my_lend_rpart_2$variable.importance

1-sum(diag(cm_4))/sum(cm_4)

# Question 4

lend_lr_predict <- predict(my_lend_lr, test_1, type="response")
lend_lr_pred <- prediction(lend_lr_predict,
                           test_1$loan_status,
                           label.ordering=c("Charged Off", "Fully Paid"))
lend_lr_perf <- performance(lend_lr_pred, "tpr", "fpr")

lend_rpart_predict <- predict(my_lend_rpart, test_1, type="prob")
lend_rpart_pred <- prediction(lend_rpart_predict[,2],
                              test_1$loan_status,
                              label.ordering=c("Charged Off", "Fully Paid"))
lend_rpart_perf <- performance(lend_rpart_pred, "tpr", "fpr")



lend_lr_predict_1 <- predict(my_lend_lr_2, test_2, type="response")
lend_lr_pred_1 <- prediction(lend_lr_predict_1,
                           test_2$loan_status,
                           label.ordering=c("Charged Off", "Fully Paid"))
lend_lr_perf_1 <- performance(lend_lr_pred_1, "tpr", "fpr")

lend_rpart_predict_1 <- predict(my_lend_rpart_2, test_2, type="prob")
lend_rpart_pred_1 <- prediction(lend_rpart_predict_1[,2],
                              test_2$loan_status,
                              label.ordering=c("Charged Off", "Fully Paid"))
lend_rpart_perf_1 <- performance(lend_rpart_pred_1, "tpr", "fpr")

plot(lend_lr_perf, col=1)
plot(lend_rpart_perf, col=2, add=TRUE)
plot(lend_lr_perf_1, col=3, add = TRUE)
plot(lend_rpart_perf_1, col=4, add=TRUE)
