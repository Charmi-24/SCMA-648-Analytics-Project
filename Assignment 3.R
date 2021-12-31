load("df_Q3.rda")

library(dplyr)
library(dummies)
library(rgl)
library(cluster)
library(fpc)

set.seed(12345)

#income, loan amount, employment length, home ownership status, and debt-to-income ratio
sub <- df[,c('annual_inc','loan_amnt','emp_length','home_ownership','dti')]
lapply(sub, class)

apply(is.na(sub),2,sum)

sub$dti[is.na(sub$dti)] <- median(sub$dti, na.rm=TRUE)

summary(sub)

#sub <- sub[sub$emp_length != "n/a", ] 
#dim(sub)

df2 <- dummy.data.frame(sub, names = c("emp_length", "home_ownership"))

df1 <- scale(df2, center = TRUE, scale = TRUE)
kms <- kmeans(df1, centers = 7)
received_pca <- prcomp(df1, retx=TRUE)
summary(received_pca$x[,1:2])
plot(received_pca$x[,1:2], col=kms$cluster, pch=kms$cluster)

new <- df2[received_pca$x[,1] < 2 & received_pca$x[,2] > -1 & received_pca$x[,2] < 1,]

new_df <- scale(new, center = TRUE, scale = TRUE)
new_kms <- kmeans(new_df, centers = 7)
new_pca <- prcomp(new_df, retx=TRUE)
plot(new_pca$x[,1:2], col = new_kms$cluster, pch = new_kms$cluster)

new_pca$rotation

plot(new_pca$x[,1:2], col = as.integer(df$grade), pch = as.integer(df$grade))


