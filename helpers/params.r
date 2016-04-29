# this function estimates the parameters of a gaussian distribution

Params <- function(X) {
    m = nrow(X)
    n = ncol(X)
    mu = c()
    sigma = c()
    for (i in 1:n) {
        mu = c(mu, sum(X[, i])/m)
        sigma = c(sigma, sum((X[,i]-mu[i])^2)/m)
    }
    return(list(mu, sigma))
}