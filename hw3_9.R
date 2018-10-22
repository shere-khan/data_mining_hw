library(ISLR)
set.seed(9)

# Training and Test Set
train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
data.train = College[train, ]
data.test = College[test, ]
