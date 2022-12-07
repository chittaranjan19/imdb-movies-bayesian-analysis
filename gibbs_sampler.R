source("post_fns.R")

n_sims = 100 # Number of simulations
m = 3 # number of genres
q # number of explanatory variables (including intercept)
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
u2_and_v_mat = matrix(NA_real_, nrow = n_sims, ncol = 2)
sigma2_mat = matrix(NA_real_, nrow = n_sims, ncol = m)
theta_mat = matrix(NA_real_, nrow = n_sims, ncol = q)
T_mat_array = array(data = NA_real_, dim = c(q, q, n_sims)) # This is an
# array with one patrix per simulation, each matrix of dimension q by q
beta_array = array(data = NA_real_, dim = c(n_sims, q, m)) # This is an array
# of matrices where row (first dimension) number i  corresponds to the 
# simulation number, column (second dimension) number j corresponds to beta_j,
# and matrix (third dimension) number k corresponds to genre.

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
    theta = sample_post_theta(beta_mat, T_mat)
    # Update T_mat
    T_mat = sample_post_T_mat(beta_mat, theta)
    # Update vectors of beta
    for (j in 1:m) {
        # y_g = # filter only rows of gender j
        # X_g = # similarly
        beta_mat[j,] = sample_post_beta(theta, T_mat, y_g, X_g, sigma2_vec[j])
    }
    # Storing variables
    u2_and_v_mat[s] = c(u2, v)
    sigma2_mat[s] = sigma2_vec
    theta_mat[s] = theta
    T_mat_array[,, s] = T_mat
    for (j in 1:m) {
        beta_array[s, , j] = beta_mat[j,]
    }
    
}