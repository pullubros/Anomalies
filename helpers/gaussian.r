# computes the probability density function of the gaussian distribution.

Gaussian <- function(X, mu, sigma) {
    m = nrow(X)
    n = ncol(X)
    cv = diag(sigma)
    x = as.matrix(X - matrix(rep(mu, m), ncol = n, byrow = TRUE))
    p = (2*pi)^(-n/2) * det(cv)^(-0.5) * exp(-0.5*rowSums((x%*%solve(cv))*x))
    return (p)
}