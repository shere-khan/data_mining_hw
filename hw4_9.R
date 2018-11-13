library(MASS)
attach(Boston)

# Cubic poly fit
lm.cubic=lm(nox~poly(dis, 3), data=Boston)
coef(summary(lm.cubic))

# Create dis vs nox plot
dislim = range(dis)
dis.grid = seq(from = dislim[1], to = dislim[2], by = 0.1)
lm.pred = predict(lm.cubic, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, lm.pred, col = "red", lwd = 2)

# Question 2b: Plot poly fits for range [1:10] and report RSS
rss.list = rep(NA, 10)
for (n in 1:10) {
  # Poly fit of degree n
  lm.fit =lm(nox~poly(dis, n), data=Boston)
  
  rss.list[n] = sum(lm.fit$residuals^2)
}
rss.list

library(boot)
err.list = rep(NA, 10)
for (n in 1:10) {
  # Poly fit of degree n
  glm.fit = glm(nox~poly(dis, n), data=Boston)
  err.list[n] = cv.glm(Boston, glm.fit, K=10)$delta[2]
}
plot(1:10, err.list, xlab = "degree", ylab = "Cross-validation error", type = "l", pch = 20, 
     lwd = 2)

# RENAME
 # Regression spline
library(splines)
sp.fit = lm(nox ~ bs(dis, df = 4, knots = c(4, 7, 11)), data = Boston)
summary(sp.fit)

sp.pred = predict(sp.fit, list(dis = dis.grid))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dis.grid, sp.pred, col = "red", lwd = 2)

# (e) Regression spline range dof
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = lm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = sum(lm.fit$residuals^2)
  sp.pred = predict(lm.fit, list(dis = dis.grid))
  plot(nox ~ dis, data = Boston, col = "darkgrey")
  lines(dis.grid, sp.pred, col = "red", lwd = 2)
}
all.cv[-c(1, 2)]

# (f) select best dof for regression spline
all.cv = rep(NA, 16)
for (i in 3:16) {
  lm.fit = glm(nox ~ bs(dis, df = i), data = Boston)
  all.cv[i] = cv.glm(Boston, lm.fit, K = 10)$delta[2]
}
plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
