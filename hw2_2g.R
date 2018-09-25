P = function(n) return(1 - (1 - 1/n)^n)
x = 1:100000
plot(x, P(x))
