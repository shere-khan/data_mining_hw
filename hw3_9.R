library(ISLR)
set.seed(9)

# Training and Test Set
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
data.train = College[train, ]
data.test = College[test, ]

# Fit linear model using least squares
lm.fit = lm(Apps~., data=data.train)
lm.pred = predict(lm.fit, data.test)
mean((data.test[, "Apps"] - lm.pred)^2)

# Fit ridge regression
library(glmnet)
train.matrix = model.matrix(Apps~., data=data.train)
test.matrix = model.matrix(Apps~., data=data.test)
range = 10 ^ seq(4, -2, length=100)
model.ridge = cv.glmnet(train.matrix, data.train[, "Apps"], alpha=0, lambda=range, thresh=1e-12)
best.lambda = model.ridge$lambda.min
best.lambda
ridge.pred = predict(model.ridge, newx=test.matrix, s=best.lambda)
mean((data.test[, "Apps"] - ridge.pred)^2)

# Fit Lasso
model.lasso = cv.glmnet(train.matrix, data.train[, "Apps"], alpha=1, lambda=range, thresh=1e-12)
lambda.best = model.lasso$lambda.min
lambda.best
lasso.pred = predict(model.lasso, newx=test.matrix, s=lambda.best)
mean((data.test[, "Apps"] - lasso.pred)^2)

model.lasso = glmnet(model.matrix(Apps~., data=College), College[, "Apps"], alpha=1)
predict(mod.lasso, s=lambda.best, type="coefficients")

# PCR
library(pls)
pcr.fit = pcr(Apps~., data=data.train, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
pcr.pred = predict(pcr.fit, data.test, ncomp=10)
mean((data.test[, "Apps"] - data.frame(pcr.pred))^2)

# PLS
pls.fit = plsr(Apps~., data=data.train, scale=T, validation="CV")
validationplot(pls.fit, val.type="MSEP")
pls.pred = predict(pls.fit, data.test, ncomp=10)
mean((data.test[, "Apps"] - data.frame(pls.pred))^2)

# compare results
test.avg = mean(data.test[, "Apps"])
test_lm_r2 = 1 - mean((data.test[, "Apps"] - lm.pred)^2) /mean((data.test[, "Apps"] - test.avg)^2)
test_ridge_r2 = 1 - mean((data.test[, "Apps"] - ridge.pred)^2) /mean((data.test[, "Apps"] - test.avg)^2)
test_lasso_r2 = 1 - mean((data.test[, "Apps"] - lasso.pred)^2) /mean((data.test[, "Apps"] - test.avg)^2)
test_pcr_r2 = 1 - mean((data.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((data.test[, "Apps"] - test.avg)^2)
test_pls_r2 = 1 - mean((data.test[, "Apps"] - data.frame(pls.pred))^2) /mean((data.test[, "Apps"] - test.avg)^2)
barplot(c(test_lm_r2, test_ridge_r2, test_lasso_r2, test_pcr_r2, test_pls_r2), col="red", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")
