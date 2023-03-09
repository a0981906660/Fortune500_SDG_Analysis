# R.V.s
# d, p, q, r
# d: density function; pdf
# p: distribution function; cdf
# q: quantile; inverse cdf
# r: random number

dunif(x = 0.5, 0, 1) # the height (the value) of the density function
punif(q = 0.25, 0, 1) # cdf
qunif(0.5, 0, 1) # quantile
runif(1)

# uniform distribution
# pdf
x <- seq(-0.5, 1.5, 0.01)
y <- dunif(x)
plot(x, y, type = 'l')

# cdf
y <- punif(x)
plot(x, y, type = 'l')

# quantile
probs <- seq(0, 1, 0.01)
y <- qunif(probs)
plot(probs, y, type = 'l')


# normal distribution
# pdf
x <- seq(-5, 5, 0.01)
y <- dnorm(x)
plot(x, y, type = 'l')

# cdf
y <- pnorm(x)
plot(x, y, type = 'l')

# quantile
probs <- seq(0, 1, 0.01)
y <- qnorm(probs)
plot(probs, y, type = 'l')


# chi-square distribution
# pdf
x <- seq(-3, 20, 0.01)
y <- dchisq(x, df = 5)
plot(x, y, type = 'l')

# cdf
y <- pchisq(x, df = 5)
plot(x, y, type = 'l')

# quantile
probs <- seq(0, 1, 0.01)
y <- qchisq(probs, df = 5)
plot(probs, y, type = 'l')
