# Question 2g
P = function(n) return(1 - (1 - 1/n)^n)
x = 1:100000
plot(x, P(x))


# Question 2h
store=rep(NA, 10000)
for(i in 1:10000) {
	store[i] = sum(sample(1:100, rep=TRUE) == 4) > 0
}
mean(store)

# Question 8
set.seed(1)
y = rnorm(100)
x = rnorm(100)
y = x - 2*x ^2 + rnorm(100)

set.seed(90)
library(boot)
D = data.frame(x, y)

glm.fit = glm(y ~ x)
cv.glm(D, glm.fit)$delta
summary(glm.fit)

glm.fit = glm(y ~ poly(x, 2))
cv.glm(D, glm.fit)$delta
summary(glm.fit)

glm.fit = glm(y ~ poly(x, 3))
cv.glm(D, glm.fit)$delta
summary(glm.fit)

glm.fit = glm(y ~ poly(x, 4))
cv.glm(D, glm.fit)$delta
summary(glm.fit)

