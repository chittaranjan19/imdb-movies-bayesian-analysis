sample_post_beta = function(theta, T_mat, y_g, X_g, sigma2_g) {
    new_cov = solve(solve(T_mat) + t(X)%*%X/sigma2_g)
    new_mean = new_cov%*%(solve(T_mat)%*%theta + t(X)%*%y/sigma2_g)
    new_beta = rmvnorm(1, mean = new_mean, sigma = new_cov)
    return(new_beta)
}

log_dens_v = function(v, u2, sigma2_vec, delta = 2, lambda = 2) {
    log_unnorm_dens = dgamma(v, shape = delta, rate = lambda, log = TRUE) +
        sum(dgamma(1/sigma2_vec, shape = v/2, rate = v*u2/2, log = TRUE))
    return(log_unnorm_dens)
}

sample_post_v = function(old_v) {
    prop_value = rnorm(n = 1, mean = old_v, sd = 2)
    log_ratio = log_dens_v(old_v, u2, sigma2_vec) - 
        log_dens_v(prop_value, u2, sigma2_vec)
    check = log(runif(1)) < log_ratio
    new_v = ifelse(test = check, yes = prop_value, no = old_v)
    return(new_v)
}
