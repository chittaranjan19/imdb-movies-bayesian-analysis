library(LaplacesDemon)
library(mvtnorm)

# We need to define some parameters:
# - parameters "a" and "b" for "u2"
# - "delta" and "lambda" for "v"
# - "t_0" and "S_0" for T_mat
# We also need to define 
# - "m": the number of groups we want to compare
# - "n_1", ..., "n_m": the number of observations for each group
# - "

sample_post_beta = function(theta, T_mat, y_g, X_g, sigma2_g) {
    new_cov = solve(solve(T_mat) + t(X_g)%*%X_g/sigma2_g)
    new_mean = new_cov%*%(solve(T_mat)%*%theta + t(X_g)%*%y_g/sigma2_g)
    new_beta = rmvnorm(1, mean = new_mean, sigma = new_cov)
    return(new_beta)
}

log_dens_v = function(v, u2, sigma2_vec) {
    log_unnorm_dens = dgamma(v, shape = delta, rate = lambda, log = TRUE) +
        sum(dgamma(1/sigma2_vec, shape = v/2, rate = v*u2/2, log = TRUE))
    return(log_unnorm_dens)
}

sample_post_v = function(old_v, u2, sigma2_vec) {
    prop_value = rnorm(n = 1, mean = old_v, sd = 2)
    log_ratio = log_dens_v(old_v, u2, sigma2_vec) - 
        log_dens_v(prop_value, u2, sigma2_vec)
    check = log(runif(1)) < log_ratio
    new_v = ifelse(test = check, yes = prop_value, no = old_v)
    return(new_v)
}

sample_post_u2 = function(v, sigma2_vec) {
    new_u2 = rgamma(
        n = 1, 
        shape = a + m*v/2, 
        rate = b + (v/2)*sum(sigma2_vec)
    )
    return(new_u2)
}

get_SSR_beta_g = function(y_g, beta_g, X_g) {
    new_resids_g = y_g - beta_g%*%X_g
    new_SSR_beta_g = sum(new_resids_g^2)
    return(new_SSR_beta_g)
}

sample_post_sigma2_g = function(v, u2, n_g, SSR_beta_g) {
    new_shape = (v + n_j)/2
    new_rate = (v*u2 + SSR_beta_g)/2
    new_sigma2_g = rinvgamma(n = 1, shape = new_shape, scale = new_rate)
    return(new_sigma2_g)
}

sample_post_T_mat = function(beta_mat, theta_vec) {
    S_theta = matrix(NA_real_, nrow = ncol(beta_mat), ncol = ncol(beta_mat))
    for (i in 1:nrow(beta_mat)) {
        beta_g = beta_mat[i,]
        S_theta = S_theta + (beta_g - theta_vec)%*%t(beta_g - theta_vec)
    }
    # S_theta = (beta_g - theta_vec)%*%t(beta_g - theta_vec)
    new_S = solve(S_0 + S_theta) # Inverse of the sum of S_0 and S_theta
    new_T_mat = rinvwishart(nu = t_0 + m, S = new_S)
    return(new_T_mat)
}

sample_post_theta = function(beta_vec, X_g, sigma2_g) {
    new_cov = solve(solve(W_0) + m * solve(T))
    new_mean = new_cov * (solve(W_0)*mu_0 + solve(T)*colSums(beta_vec))
    new_theta = rmvnorm(1, mean = new_mean, sigma = new_cov)
    return(new_theta)
}




