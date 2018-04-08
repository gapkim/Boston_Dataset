##############################
### Initial setup

knitr::opts_chunk$set(comment=NA, echo=FALSE, warning=FALSE, message=FALSE,
                      fig.align="center")
options(knitr.table.format="html", knitr.table.align="center")
options(digits=4)

rm(list=ls())

##############################
### Explortory Data Analysis
##############################

library(MASS)
data("Boston")

str(Boston)
sum(is.na(Boston))

mydata = Boston
mydata$chas = factor(mydata$chas, levels=c(0, 1), labels=c("Bounds river","Otherwise"))
mydata$rad = as.factor(mydata$rad)

str(mydata)

# Table 1: Summary of categorical variable, 'chas'

library(knitr)

kable(summary(mydata$chas), col.names="",
      caption="Table 1: Summary of categorical variable, 'chas'",
      format="html", table.attr = "style='width:40%;'")

# Figure 1: Histogram of variable 'rad'

library(ggplot2)

ggplot(data=mydata) +
   geom_histogram(aes(rad), stat="count")

# Figure 2: Boxplots of all 12 quantitative variables

library(reshape2)

melt.data = melt(data=mydata)

ggplot(data=melt.data) +
   geom_boxplot(aes(x="", y=value)) +
   facet_wrap(~variable, scale="free") +
   labs(x="All categorical variables", y="count")

# Figure 3: Histogram of variable 'crim'

library(car)

ggplot(data=mydata) +
   geom_histogram(aes(crim)) +
   scale_x_continuous(trans="log", labels=function(x) round(x, digits=2))

# Table 2: Summary statistics of 'crim'

table2 = as.matrix(round(summary(mydata$crim),2))

kable(table2, col.names="",
      caption="Table 2: Summary statistics of 'crim'",
      format="html", table.attr = "style='width:40%;'")

# Figure 4: Scatterplot and correlation plot matrix of the dataset

library(GGally)
ggpairs(mydata)

# Figure 5: Scatterplots of 'crim' versus other variables

library(gridExtra)

scatterplot_col = function(column, yvar, data) {
   ggplot(data, aes_string(y = yvar)) +
      geom_point(aes_string(x = column))
}

mplot = lapply(colnames(mydata)[-1], scatterplot_col, yvar="crim", data=mydata)

grid.arrange(mplot[[1]], mplot[[2]], mplot[[3]], mplot[[4]], mplot[[5]],
             mplot[[6]], mplot[[7]], mplot[[8]], mplot[[9]], mplot[[10]],
             mplot[[11]], mplot[[12]], mplot[[13]], ncol=3)

# Figure 6: Scatterplot of 'log.crim' versus other variables

mydata2 = mydata
mydata2$log.crim = log(mydata2$crim)
mydata2 = mydata2[-1]

mplot = lapply(colnames(mydata)[-1], scatterplot_col, yvar="log.crim", data=mydata2)

grid.arrange(mplot[[1]], mplot[[2]], mplot[[3]], mplot[[4]], mplot[[5]],
             mplot[[6]], mplot[[7]], mplot[[8]], mplot[[9]], mplot[[10]],
             mplot[[11]], mplot[[12]], mplot[[13]], ncol=3)

# Figure 7: Scatterplot of 'log.crim' versus 'exp.black'

mydata3 = mydata2
mydata3$exp.black = exp(10*mydata3$black / max(mydata3$black)) / 100
mydata3 = subset(mydata3, select=-black)

ggplot(data=mydata3) +
   geom_point(aes(x=exp.black, y=log.crim))


########################################
### Full Model with Original Variables
########################################

full.lm = lm(crim ~ ., data=mydata)
full.lm_summary = summary.lm(full.lm)
full.lm_summary

# Figure 8: Residual vs. fits plot (left), and Normal Q-Q plot of residuals(right)

par(mfrow=c(1,2))

plot(full.lm, which=1)
plot(full.lm, which=2)


# Figure 9: Standardized residuals vs. fits plot (left), and Standard residuals vs. leverage with contours of Cook's distance

par(mfrow=c(1,2))

plot(full.lm, which=3)
abline(h=sqrt(3), col='blue')

hii = hatvalues(full.lm)
h2 = 2*sum(hii) / length(hii)
h3 = 3*sum(hii) / length(hii)

plot(full.lm, which=5)
abline(v=c(h2, h3), col=c('blue', 'blue'), lty=c(2, 1))
abline(h=3, col='blue')

# Table 3: VIFs from full model with original variables

kable(vif(full.lm), caption="Table 3: VIFs from full model with original variables",
      format="html", table.attr="style='width:40%;')")

#############################################
### Full Model with Transformed Variables
#############################################

full.lm1 = lm(log.crim ~ ., data=mydata3)
full.lm1_summary = summary.lm(full.lm1)
full.lm1_summary

#############################################
### Manual Model Selection
#############################################

### Ordered full model

full.lm2 = lm(log.crim ~ zn + nox + rad + ptratio + exp.black + age + lstat + chas + indus + dis + rm + tax + medv, data=mydata3)

# Table 4: ANOVA result of transformed full model (Type I, Sequential sum of squares)

kable(anova(full.lm2),
      caption="Table 4: ANOVA result of transformed full model
      <br>(Type I, Sequential sum of squares)",
      format="html", table.attr="style='width:60%;')")

### Manual model selected

manual.pick = lm(log.crim ~ zn + nox + rad + ptratio + exp.black + age + lstat, data=mydata3)
manual.pick_summary = summary.lm(manual.pick)
manual.pick_summary

# Figure 10: Diagnostic plots for transformed and reduce model

par(mfrow=c(2,2))

plot(manual.pick, which=1)
plot(manual.pick, which=2)

plot(manual.pick, which=3)
abline(h=sqrt(3), col='blue')

hii = hatvalues(manual.pick)
h2 = 2*sum(hii) / length(hii)
h3 = 3*sum(hii) / length(hii)

plot(manual.pick, which=5)
abline(v=c(h2, h3), col=c('blue', 'blue'), lty=c(2, 1))
abline(h=c(-3,3), col='blue')


# Table 5: VIFs from reduced transformed model

kable(vif(manual.pick), caption="Table 5: VIFs from reduced transformed model",
      format="html", table.attr="style='width:40%;')")

###################################
### Best Subsets Regression
###################################

library(leaps)

# Code from ISLR for predict() method for regsubsets(): not used in calculation
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
}

###################################
# Best Subsets Cross Validation

library(leaps)

npred = 20
nmax = 20  # Total number of predictors including factor levels

k = 10
set.seed(10)
folds = sample(1:k, nrow(mydata3), replace=TRUE)

cv.errors = matrix(NA, k, npred)
train.errors = matrix(NA, k, nmax)

for(j in 1:k) {
   best.subset = regsubsets(log.crim ~ ., data=mydata3[folds!=j,], nvmax=npred)
   train.errors[j,] = summary(best.subset)$rss / best.subset$nn
   
   x.test = model.matrix(log.crim ~ ., data=mydata3[folds==j,])
   y.test = mydata3$log.crim[folds==j]
   
   for(i in 1:npred) {
      coefi = coef(best.subset, id=i)
      pred = x.test[, names(coefi)] %*% coefi
      cv.errors[j,i] = mean((y.test - pred)^2)
   }
}

mean.cv.errors = apply(cv.errors, 2, mean)
mean.train.errors = apply(train.errors, 2, mean)

# Best subset CV optimal number of predictors
index.best = which.min(mean.cv.errors)  

# Test MSE of Best Subsets CV
test.MSE_bestsub = mean.cv.errors[index.best]

##########################
### Plots and Tables

#Figure 11: Testing and training MSE of Best Subsets

ggplot() +
   geom_point(aes(x=c(1:npred),y=mean.cv.errors, color="Test MSE"), size=3) +
   geom_line(aes(x=c(1:npred),y=mean.cv.errors, color="Test MSE"), linetype="solid") +
   geom_point(aes(x=c(1:npred),y=mean.train.errors, color="Training MSE"), size=3) +
   geom_line(aes(x=c(1:npred),y=mean.train.errors, color="Training MSE"), linetype="solid") +
   scale_x_continuous(breaks=c(1:npred)) +
   geom_vline(xintercept=c(index.best), color="blue") +
   annotate("text", label="min Test MSE", x=16, y=1) +
   labs(x="Number of Predictors", y="Mean Squared Error", color=c("Test/Training")) +
   ggtitle("Test and training mean squared error from cross validation (k=10)")

###
### Best subset with all data
###

reg.best = regsubsets(log.crim ~ ., data=mydata3, nvmax=nmax)
summary.BS = summary(reg.best)

# Optimal predictor numbers for Mallows Cp, Adj.R2, BIC
BS.Cp = which.min(summary.BS$cp)
BS.AdjR2 = which.max(summary.BS$adjr2)
BS.BIC = which.min(summary.BS$bic)

# Table 6: Number of optimal predictors selected by Best Subsets
BS.summary = data.frame("Method" = c("Mallows Cp", "Adj R2", "BIC", "Cross_Validation"), "Number of Predictors" = c(BS.Cp, BS.AdjR2, BS.BIC, index.best))

kable(BS.summary, caption="Table 6: Number of optimal predictors selected by Best Subsets", format="html", table.attr = "style='width:40%;'")

# Table 7: Coefficients selected by Best Subsets CV
kable(coef(reg.best, index.best), col.names="",
      caption="Table 7: Coefficients selected by Best Subsets CV",
      format="html", table.attr = "style='width:30%;'")

# Chekcing coefficients selected by Adj R2 and BIC
names(coef(reg.best, BS.AdjR2))
names(coef(reg.best, BS.BIC))

##############################
### Ridge Regression
##############################

library(glmnet)

k = 5
x = model.matrix(log.crim ~ ., data=mydata3)[,-1]
y = mydata3$log.crim

grid = 8*10^seq(2, -5, length=100)

# Store errors and best lambda with lambda.min
cv.errors = rep(NA, k)
bestlam_ridge = rep(NA, k)

# Store errors and best lambda with lambda.1se
cv.errors.1se = rep(NA, k)
bestlam_ridge.1se = rep(NA, k)

for(j in 1:k) {
   set.seed(12)
   ridge.cv.out = cv.glmnet(x[folds!=j,], y[folds!=j], alpha=0, nfolds=10, lambda=grid)
   bestlam_min_ridge = ridge.cv.out$lambda.min
   bestlam_1se_ridge = ridge.cv.out$lambda.1se
   
   bestlam_ridge[j] = bestlam_min_ridge
   ridge.cv.pred = predict(ridge.cv.out, s=bestlam_ridge[j], newx=x[folds==j,])
   cv.errors[j] = mean((ridge.cv.pred - y[folds==j])^2)
   
   bestlam_ridge.1se[j] = bestlam_1se_ridge
   ridge.cv.pred.1se = predict(ridge.cv.out, s=bestlam_ridge.1se[j], newx=x[folds==j,])
   cv.errors.1se[j] = mean((ridge.cv.pred.1se - y[folds==j])^2)
}

cv.output_ridge = data.frame("folds"=c(1:k), "lambda"=bestlam_ridge, "Test_MSE"=cv.errors)

cv.output_ridge.1se = data.frame("folds"=c(1:k), "lambda"=bestlam_ridge.1se, "Test_MSE"=cv.errors.1se)

##############################
### Ridge Regression Plots

###
### for lambda.min
###

avg.bestlam_ridge = mean(bestlam_ridge)
avg.cv.errors = mean(cv.errors)

# Test MSE from Ridge Regression with lambda.min
test.MSE_ridge.min = avg.cv.errors

# Figure.12, fig.cap="Figure 12: Plot of Best lambda(min) vs. folds (left), and plot of Test MSE vs. folds (right)"
pp1 <- ggplot(aes(x=folds, y=lambda), data=cv.output_ridge) +
   geom_point(color="red", size=3) +
   geom_line(color="red", linetype="solid") +
   geom_hline(yintercept = avg.bestlam_ridge) +
   annotate("text", x=(k+1)/2, y=avg.bestlam_ridge+0.0002,
            label=paste("Mean of lambda =", round(avg.bestlam_ridge,4))) +
   labs(x="Folds", y="Best Lambda (min)") +
   ggtitle("Plot of best lambda vs. folds")
   
   
pp2 <- ggplot(aes(x=folds, y=Test_MSE), data=cv.output_ridge) +
   geom_point(color="blue", size=3) +
   geom_line(color="blue", linetype="solid") +
   geom_hline(yintercept = avg.cv.errors) +
   annotate("text", x=(k+1)/2, y=avg.cv.errors+0.01,
            label=paste("Mean of Test MSE =", round(avg.cv.errors,4))) +
   labs(x="Folds", y="Test MSE") +
   ggtitle("Plot of test MSE vs. folds")

grid.arrange(pp1, pp2, ncol=2)

# Figure 13: MSE vs. log(lambda) from Ridge Regression of the 10th fold
plot(ridge.cv.out)

###
### for lambda.1se
###

avg.bestlam_ridge.1se = mean(bestlam_ridge.1se)
avg.cv.errors.1se = mean(cv.errors.1se)

# Test MSE from Ridge Regression with lambda.1se 
test.MSE_ridge.1se = avg.cv.errors.1se

pp3 <- ggplot(aes(x=folds, y=lambda), data=cv.output_ridge.1se) +
   geom_point(color="red", size=3) +
   geom_line(color="red", linetype="solid") +
   geom_hline(yintercept = avg.bestlam_ridge.1se) +
   annotate("text", x=(k+1)/2, y=avg.bestlam_ridge.1se+0.005,
            label=paste("Mean of lambda =", round(avg.bestlam_ridge.1se,4))) +
   labs(x="Folds", y="Best Lambda (1SE)") +
   ggtitle("Plot of best lambda vs. folds")
   
   
pp4 <- ggplot(aes(x=folds, y=Test_MSE), data=cv.output_ridge.1se) +
   geom_point(color="blue", size=3) +
   geom_line(color="blue", linetype="solid") +
   geom_hline(yintercept = avg.cv.errors.1se) +
   annotate("text", x=(k+1)/2, y=avg.cv.errors.1se+0.02,
            label=paste("Mean of Test MSE =", round(avg.cv.errors.1se,4))) +
   labs(x="Folds", y="Test MSE") +
   ggtitle("Plot of test MSE vs. folds")

grid.arrange(pp3, pp4, ncol=2)

###########################################
### Fit Ridge Regression to full dataset

out_ridge = glmnet(x, y, alpha=0, lambda=grid)

#Figure.15, fig.ap="Figure 15: Coefficients vs. log(lambda) for Ridge Regression with full dataset
plot(out_ridge, xvar="lambda", label=T,
     sub=paste("(Vertical lines at lambda .min =", round(avg.bestlam_ridge, 4),
               ", and lambda.1se =", round(avg.bestlam_ridge.1se, 4), ")"))
abline(v=c(log(avg.bestlam_ridge), log(avg.bestlam_ridge.1se) ))

###
### Calculate MSE of Ride Regression using the full data
###

# for lambda(min)
pred = predict(out_ridge, s=avg.bestlam_ridge, newx=x, exact=T, x=x, y=y)
MSE_ridge.min = mean((pred - y)^2)
cat("Ridge regression MSE with full data:", MSE_ridge.min,
    "(lambda.min =", round(avg.bestlam_ridge,4),")")

coef.bestlam_ridge = coef(out_ridge, s=avg.bestlam_ridge, exact=T, x=x, y=y)[,1]

# for lambda(1se)
pred = predict(out_ridge, s=avg.bestlam_ridge.1se, newx=x, exact=T, x=x, y=y)
MSE_ridge.1se = mean((pred - y)^2)
cat("Ridge regression MSE with full data:", MSE_ridge.1se,
    "(lambda =", round(avg.bestlam_ridge.1se,4),")")

coef.bestlam_ridge.1se = coef(out_ridge, s=avg.bestlam_ridge.1se,
                              exact=T, x=x, y=y)[,1]

### Sanity check1 (both should equal)
mean(full.lm2$residuals^2)

pred_0 = predict(out_ridge, s=0, newx=x, exact=T, x=x, y=y)
mean((pred_0 - y)^2)

###################################
### Lasso Rgression
###################################

grid = 10^seq(1, -5, length=100)

cv.errors = rep(NA, k)
bestlam_lasso = rep(NA, k)

cv.errors.1se = rep(NA, k)
bestlam_lasso.1se = rep(NA, k)

for(j in 1:k) {
   set.seed(12)
   lasso.cv.out = cv.glmnet(x[folds!=j,], y[folds!=j], alpha=1, nfolds=10, lambda=grid)
   bestlam_min_lasso = lasso.cv.out$lambda.min
   bestlam_1se_lasso = lasso.cv.out$lambda.1se
   
   bestlam_lasso[j] = bestlam_min_lasso
   lasso.cv.pred = predict(lasso.cv.out, s=bestlam_lasso[j], newx=x[folds==j,])
   cv.errors[j] = mean((lasso.cv.pred - y[folds==j])^2)
   
   bestlam_lasso.1se[j] = bestlam_1se_lasso
   lasso.cv.pred.1se = predict(lasso.cv.out, s=bestlam_lasso.1se[j], newx=x[folds==j,])
   cv.errors.1se[j] = mean((lasso.cv.pred.1se - y[folds==j])^2)
}

cv.output_lasso = data.frame("folds"=c(1:k), "lambda"=bestlam_lasso, "Test_MSE"=cv.errors)

cv.output_lasso.1se = data.frame("folds"=c(1:k), "lambda"=bestlam_lasso.1se, "Test_MSE"=cv.errors.1se)

##############################
### Lasso Regression Plots

###
### for lambda.min
###

avg.bestlam_lasso = mean(bestlam_lasso)
avg.cv.errors = mean(cv.errors)

# Test MSE from Lasso Regression with lambda.min
test.MSE_lasso.min = avg.cv.errors

# Figure 16: Plot of Best lambda(min) vs. folds (left), and plot of Test MSE vs. folds (right) for Lasso Regression
pp1 <- ggplot(aes(x=folds, y=lambda), data=cv.output_lasso) +
   geom_point(color="red", size=3) +
   geom_line(color="red", linetype="solid") +
   geom_hline(yintercept = avg.bestlam_lasso) +
   annotate("text", x=(k+1)/2, y=avg.bestlam_lasso+0.00007,
            label=paste("Mean of lambda =", round(avg.bestlam_lasso,4))) +
   labs(x="Folds", y="Best Lambda (min)") +
   ggtitle("Plot of best lambda vs. folds")
   
pp2 <- ggplot(aes(x=folds, y=Test_MSE), data=cv.output_lasso) +
   geom_point(color="blue", size=3) +
   geom_line(color="blue", linetype="solid") +
   geom_hline(yintercept = avg.cv.errors) +
   annotate("text", x=(k+1)/2, y=avg.cv.errors+0.015,
            label=paste("Mean of Test MSE =", round(avg.cv.errors,4))) +
   labs(x="Folds", y="Test MSE") +
   ggtitle("Plot of test MSE vs. folds")

grid.arrange(pp1, pp2, ncol=2)

# Figure 17: MSE vs. log(lambda) from Lasso Regression of the 10th fold
plot(lasso.cv.out)

###
### for lambda.1se
###

avg.bestlam_lasso.1se = mean(bestlam_lasso.1se)
avg.cv.errors.1se = mean(cv.errors.1se)

# Test MSE from Lasso Regression with lambda.1se
test.MSE_lasso.1se = avg.cv.errors.1se

pp3 <- ggplot(aes(x=folds, y=lambda), data=cv.output_lasso.1se) +
   geom_point(color="red", size=3) +
   geom_line(color="red", linetype="solid") +
   geom_hline(yintercept = avg.bestlam_lasso.1se) +
   annotate("text", x=(k+1)/2, y=avg.bestlam_lasso.1se+0.0008,
            label=paste("Mean of lambda =", round(avg.bestlam_lasso.1se,4))) +
   labs(x="Folds", y="Best Lambda (1SE)") +
   ggtitle("Plot of best lambda vs. folds")
   
   
pp4 <- ggplot(aes(x=folds, y=Test_MSE), data=cv.output_lasso.1se) +
   geom_point(color="blue", size=3) +
   geom_line(color="blue", linetype="solid") +
   geom_hline(yintercept = avg.cv.errors.1se) +
   annotate("text", x=(k+1)/2, y=avg.cv.errors.1se+0.015,
            label=paste("Mean of Test MSE =", round(avg.cv.errors.1se,4))) +
   labs(x="Folds", y="Test MSE") +
   ggtitle("Plot of test MSE vs. folds")

grid.arrange(pp3, pp4, ncol=2)

###########################################
### Fit Lasso Regression to full dataset

out_lasso = glmnet(x, y, alpha=1, lambda=grid)

# Figure 19: Coefficients vs. log(lambda) for Ridge Regression with full dataset
plot(out_lasso, xvar="lambda", label=T,
     sub=paste("(Vertical lines at lambda.min =", round(avg.bestlam_lasso, 4),
               ", and lambda.1se =", round(avg.bestlam_lasso.1se, 4), ")"))
abline(v=c(log(avg.bestlam_lasso), log(avg.bestlam_lasso.1se) ))

###
### Calculate MSE of Lasso Regression using the full data
###

# for lambda(min)
pred = predict(out_lasso, s=avg.bestlam_lasso, newx=x, exact=T, x=x, y=y)
MSE_lasso.min = mean((pred - y)^2)
cat("Lasso regression MSE with full data:", MSE_lasso.min,
    "(lambda =", round(avg.bestlam_lasso,4),")")

coef.bestlam_lasso = coef(out_lasso, s=avg.bestlam_lasso, exact=T, x=x, y=y)[,1]

# for lambda(1se)
pred = predict(out_lasso, s=avg.bestlam_lasso.1se, newx=x, exact=T, x=x, y=y)
MSE_lasso.1se = mean((pred - y)^2)
cat("Lasso regression MSE with full data:", MSE_lasso.1se,
    "(lambda =", round(avg.bestlam_lasso.1se,4),")")

coef.bestlam_lasso.1se = coef(out_lasso, s=avg.bestlam_lasso.1se,
                              exact=T, x=x, y=y)[,1]

### Sanity check2 (both should equal)
mean(full.lm2$residuals^2)

pred_0 = predict(out_ridge, s=0, newx=x, exact=T, x=x, y=y)
mean((pred_0 - y)^2)

########################################################################
### Calculate Test and Training MSE for Full and Manual models

cv.errors_full = c(NA, k)
train.errors_full = c(NA, k)

cv.errors_manual = c(NA, k)
train.errors_manual = c(NA, k)

for(j in 1:k) {
   OLS.full = lm(log.crim ~ ., data=mydata3[folds!=j,])
   train.errors_full[j] = sum(OLS.full$residuals^2) / length(OLS.full$residuals)
   
   OLS.manual = lm(log.crim ~ zn + nox + rad + ptratio + exp.black + age + lstat,
                   data=mydata3[folds!=j,])
   train.errors_manual[j] = sum(OLS.manual$residuals^2) / length(OLS.manual$residuals)
   
   y.test = mydata3$log.crim[folds==j]
   
   pred_full = predict(OLS.full, newdata=mydata3[folds==j,])
   pred_manual = predict(OLS.manual, newdata=mydata3[folds==j,])
   
   cv.errors_full[j] = mean((pred_full - y.test)^2)
   cv.errors_manual[j] = mean((pred_manual - y.test)^2)
}

test.MSE_full = mean(cv.errors_full)
train.MSE_full = mean(train.errors_full)
test.MSE_manual = mean(cv.errors_manual)
train.MSE_manual = mean(train.errors_manual)

###################################
### Make summary table
###################################

# Table of model coefficients
coef.table = matrix(NA, 21, 7, dimnames=list(names(coef(full.lm1)), c("Full_model", "Manual", "Best_sub", "Ridge(min)", "Ridge(1se)", "Lasso(min)", "Lasso(1se)")))

coef.table[, 1] = full.lm1$coefficients
coef.table[names(manual.pick$coefficients),2 ] = manual.pick$coefficients
coef.table[names(coef(reg.best, index.best)), 3] = coef(reg.best, index.best)
coef.table[names(coef.bestlam_ridge), 4] = coef.bestlam_ridge
coef.table[names(coef.bestlam_ridge.1se), 5] = coef.bestlam_ridge.1se
coef.table[names(coef.bestlam_lasso), 6] = coef.bestlam_lasso
coef.table[names(coef.bestlam_lasso.1se), 7] = coef.bestlam_lasso.1se

coef.table[coef.table == 0] = NA

# MSE with all data
MSE_full = sum(full.lm1$residuals^2) / nrow(mydata3)
MSE_manual = sum(manual.pick$residuals^2) / nrow(mydata3)
MSE_bestsub = summary(reg.best)$rss[index.best]/reg.best$nn

MSE_all.data = c(MSE_full, MSE_manual, MSE_bestsub, MSE_ridge.min, MSE_ridge.1se,
                MSE_lasso.min, MSE_lasso.1se)

# Test MSE
test.MSE = c(test.MSE_full, test.MSE_manual, test.MSE_bestsub, test.MSE_ridge.min,
             test.MSE_ridge.1se, test.MSE_lasso.min, test.MSE_lasso.1se)

# Join coefficient table and metrics
summary.table = rbind(coef.table, MSE_all.data, test.MSE)
rownames(summary.table)[22:23] = c("MSE(all_data)","Test_MSE")

# Table 8: Summary table of model metric and coefficients
summary.table = round(summary.table, digits=4)
kable(summary.table, caption="Table 8: Summary table of model metric and coefficients", format="html", table.attr = "style='width:90%;'") 

#######################################
### DISCUSSION: Table for Best subsets

BS.table = matrix(0, 20, 21, dimnames=list(c(1:20), names(coef(full.lm1))))
BS.coef = coef(reg.best, c(1:npred))

for(i in c(1:20)) {
   BS.table[i,names(BS.coef[[i]])] = BS.coef[[i]]
}

BS.table_df = data.frame(BS.table)
BS.table_df$np = c(1:20)
colnames(BS.table_df)[1] = "intercept"
colnames(BS.table_df)[4] = "chasOth"

melt.BS.table = melt(BS.table_df, id.vars=c("np"))

# Figure 20: Coefficients vs. number of predictors for Best subsets

ggplot(data=melt.BS.table) +
   geom_line(aes(x=np, y=value, color=variable)) +
   scale_x_continuous(trans="reverse", breaks=c(1:20)) +
   labs(x="Number of predictors", y="Coefficients")

############################################################
### Top 5 pick by Best Subsets and Lasso Regression

# Best Subsets
BS.best5.coef = coef(reg.best, c(1:5))
BS.best5.MSE = summary.BS$rss[1:5] / reg.best$nn

# Lasso Regression
grid = 10^seq(0.3, -0.3, length=100)
lambda.pick = grid[c(15, 19, 83, 87, 88)] # Picked lambda for each predictor number

out_lasso5 = glmnet(x, y, alpha=1, lambda=grid)
out_lasso5.summary = summary(out_lasso5)

Lasso.best5 = list()
Lasso.best5.MSE = rep(NA, length(lambda.pick))

for(i in c(1:length(lambda.pick))) {
   Lasso.best5[[i]] = coef(out_lasso5, s=lambda.pick[i], exact=T, x=x, y=y)[,1]
   pred = predict(out_lasso5, s=lambda.pick[i], newx=x, exact=T, x=x, y=y)
   Lasso.best5.MSE[i] = mean((pred - y)^2)
}
# Table of model coefficients
feature.select = matrix(NA, 21, 10, dimnames=list(names(coef(full.lm1)), c("Best(n=1)","Lasso(n=1)","Best(n=2)","Lasso(n=2)","Best(n=3)","Lasso(n=3)","Best(n=4)","Lasso(n=4)","Best(n=5)","Lasso(n=5)")))

for(i in c(1:length(lambda.pick))) {
   feature.select[names(BS.best5.coef[[i]]),2*i-1] = BS.best5.coef[[i]]
   feature.select[names(Lasso.best5[[i]]),2*i] = Lasso.best5[[i]]
}

feature.select[feature.select == 0] = NA

kable(feature.select, caption="Table 9: Selected coefficients for first 5 predictors from Best subsets and Lasso regression", 
      format="html", table.attr = "style='width:100%;'")
