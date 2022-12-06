m = 3 # number of genres
n_g = c(1,1,1) # vector of number of movies from each genre
mu_0 # Vector of means for distribution of theta
W_0 # Matrix of covariances for distribution of theta


n_sims = 100
for (s in 1:n_sims) {
    # Update u2
    u2 = sample_post_u2(v, sigma2_vec)
    # Update v
    v = sample_post_v(old_v = v, u2 = u2, sigma2_vec = sigma2_vec)
    # Update sigma2
    for (j in 1:m) {
        SSR_g = get_SSR_beta_g(y_g, beta_g, X_g)
        sigma2_vec[j] = sample_post_sigma2_g(v, u2, n_g[j], SSR_g)
    }
    # Update theta
    theta = sample_post_theta(beta)
    # Update T_mat
    
    # Update vectors of beta
    beta_1 = sample_post_beta(theta = theta, T_mat = T_mat, )
}