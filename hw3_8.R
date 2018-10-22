library(leaps)
set.seed(1)
X = rnorm(n=100)
e = rnorm(n=100)
b0 = 2
b1 = 3
b2 = 2
b3 = 0.2
Y = b0 + b1*X + b2*(X^2) + b3*(X^3) + e
data.xy = data.frame(y = Y, x = X)

# Best subset selection
subset = regsubsets(Y~poly(X, 10, raw=T), data=data.xy, nvmax=10)
best.summary = summary(subset)

# C_p
which.min(best.summary$cp)
plot(best.summary$cp, xlab="Size of Subset", ylab="C_p", pch=20, type="l")
points(3, best.summary$cp[3], pch=4, col="red", lwd=7)

# BIC
which.min(best.summary$bic)
plot(best.summary$bic, xlab="Size of Subset", ylab="BIC", pch=20, type="l")
points(3, best.summary$bic[3], pch=4, col="red", lwd=7)

# Adjusted R^2
which.max(best.summary$adjr2)
plot(best.summary$adjr2, xlab="Size of Subset", ylab="AR^2", pch=20, type="l")
points(3, best.summary$adjr2[3], pch=4, col="red", lwd=7)

# Forward Selection
forward = regsubsets(Y ~ poly(X, 10, raw = T), data = data.xy, nvmax = 10,  method = "forward")
forward.summary = summary(forward)
which.min(forward.summary$cp)
which.min(forward.summary$bic)
which.min(forward.summary$adjr2)

# Backward Selection
backward = regsubsets(Y ~ poly(X, 10, raw = T), data = data.xy, nvmax = 10,  method = "backward")
backward.summary = summary(backward)
which.min(backward.summary$cp)
which.min(backward.summary$bic)
which.min(backward.summary$adjr2)

# Combined graphs for Forward and Backward Selection
par(mfrow = c(3, 2))
plot(forward.summary$cp, xlab = "Subset Size", ylab = "Forward Cp", pch = 20, type = "l")
points(3, forward.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(backward.summary$cp, xlab = "Subset Size", ylab = "Backward Cp", pch = 20, type = "l")
points(3, backward.summary$cp[3], pch = 4, col = "red", lwd = 7)
plot(forward.summary$bic, xlab = "Subset Size", ylab = "Forward BIC", pch = 20,  type = "l")
points(3, forward.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(backward.summary$bic, xlab = "Subset Size", ylab = "Backward BIC", pch = 20,  type = "l")
points(3, backward.summary$bic[3], pch = 4, col = "red", lwd = 7)
plot(forward.summary$adjr2, xlab = "Subset Size", ylab = "Forward Adjusted R2",  pch = 20, type = "l")
points(3, forward.summary$adjr2[3], pch = 4, col = "red", lwd = 7)
plot(backward.summary$adjr2, xlab = "Subset Size", ylab = "Backward Adjusted R2",  pch = 20, type = "l")
points(4, backward.summary$adjr2[4], pch = 4, col = "red", lwd = 7)

# Lasso Regression
library(glmnet)
X.matrix = model.matrix(y~poly(X, 10, raw=T), data=data.xy)[, -1]
model.lasso = cv.glmnet(X.matrix, Y, alpha=1)
lam = model.lasso$lambda.min
lam
plot(model.lasso)

best.model = glmnet(X.matrix, Y, alpha=1)
predict(best.model, s=lam, type="coefficients")

# 8f
b0 = 2
b7 = 3
Y = b0 + b7*(X^7) + e
data.xy = data.frame(y = Y, x = X)
# Best subset
subset = regsubsets(Y~poly(X, 10, raw=T), data=data.xy, nvmax=10)
best.summary = summary(subset)
# C_p
which.min(best.summary$cp)
plot(best.summary$cp, xlab="Size of Subset", ylab="C_p", pch=20, type="l")
points(3, best.summary$cp[3], pch=4, col="red", lwd=7)
# BIC
which.min(best.summary$bic)
plot(best.summary$bic, xlab="Size of Subset", ylab="BIC", pch=20, type="l")
points(3, best.summary$bic[3], pch=4, col="red", lwd=7)
# Adjusted R^2
which.max(best.summary$adjr2)
plot(best.summary$adjr2, xlab="Size of Subset", ylab="AR^2", pch=20, type="l")
points(3, best.summary$adjr2[3], pch=4, col="red", lwd=7)
# Coefficients
coefficients(subset, id=1)
coefficients(subset, id=2)
coefficients(subset, id=6)
# Lasso
X.matrix = model.matrix(y~poly(X, 10, raw=T), data=data.xy)[, -1]
model.lasso = cv.glmnet(X.matrix, Y, alpha=1)
lam = model.lasso$lambda.min
lam
best.model = glmnet(X.matrix, Y, alpha=1)
predict(best.model, s=lam, type="coefficients")
