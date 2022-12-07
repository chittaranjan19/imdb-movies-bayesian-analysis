source("post_fns.R")

m = 3 # number of genres
n_g = c(1,1,1) # vector of number of movies from each genre
a # scalar for distribution of u2
b # scalar for distribution of u2
delta # Scalar for distribution of v
lambda # Scalar for distribution of v
mu_0 # Vector of means for distribution of theta
W_0 # Matrix of covariances for distribution of theta
# inv_W_0 = solve(W_0)
S_0 # Matrix for distribution of T
t_0 # Scalar degrees of freedom for distribution of T

# Sampling initial values from prior distributions
u2 = rgamma(1, shape = a, rate = b)
v = gamma(1, shape = delta, rate = lambda)
sigma2_vec = rinvgamma(3, shape = v/2, scale = v*u2/2)
theta = as.vector(rmvnorm(1, mean = mu_0, sigma = W_0))
T_mat = rinvwishartc(nu = t_0, W_0)
beta_mat = rmvnorm(3, mean = theta, sigma = T_mat)
colnames(beta_mat) = as.character(seq(1, ncol(beta_mat)))

# Places to store variables
u2_vec = rep(NA_real_, times = S)

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
    T_mat = sample_post_T_mat(beta_mat, theta)
    # Update vectors of beta
    for (j in 1:m) {
        # y_g = # filter only rows of gender j
        # X_g = # similarly
        beta_mat[j,] = sample_post_beta(theta, T_mat, y_g, X_g, sigma2_vec[j])
    }
    
}