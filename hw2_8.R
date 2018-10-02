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
