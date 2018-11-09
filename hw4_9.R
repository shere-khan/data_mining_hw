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
rss.lsit