library(tidyverse)
source("post_fns.R")

movies = read_csv("data-collection/data_clean.csv") %>%
    mutate(excess_duration = (duration - 120) * (duration > 120))

set.seed(52996)
training_data <- movies %>%
    group_by(genres) %>%
    slice_sample(prop = 0.7) %>%
    ungroup()

data_set = training_data

data_set = data_set %>%
    mutate(genres = as.factor(genres)) %>%
    mutate(intercept = 1) %>% 
    select(c(
        # "genres", "intercept", "revenue", "budget"
        "genres", "intercept", "revenue", "budget", "excess_duration"
        # "genres", "intercept", "revenue"
    ))
    # mutate(tmp_value = TRUE)

genres = levels(data_set$genres)

# data_set = data_set %>%
#     pivot_wider(names_from = genres, values_from = tmp_value) %>%
#     replace(is.na(.), FALSE)

# X = data_set %>% select(-revenue)
# X[, "intercept"] = 1

n_sims = 10000 # Number of simulations
m = length(genres) # number of genres
q = ncol(data_set) - 2 # number of explanatory variables (including intercept)
# n_g = c(1,1,1) # vector of number of data_set from each genre
a = 13e+6 # scalar for distribution of u2
b = 2 # scalar for distribution of u2
delta = 12 # Scalar for distribution of v
lambda = 2 # Scalar for distribution of v
mu_0 = c(mean(movies$revenue), 1, -50000)
# mu_0 = c(mean(movies$revenue), 1, -50000, 0) # Vector of means for distribution of theta
W_0 = matrix(0, nrow = q, ncol = q) # Matrix of covariances for distribution of theta
diag(W_0) = c(1e+12, 5^2, 25000^2)
# diag(W_0) = c(1e+12, 0.4^2, 25000^2, 1000000)

# inv_W_0 = solve(W_0)
t_0 = q + 2
S_0 = (t_0 - q - 1) * diag(x = 1e+12, nrow = q, ncol = q)# Matrix for distribution of T
# t_0 = q # Scalar degrees of freedom for distribution of T

set.seed(1210)
# Sampling initial values from prior distributions
u2 = rgamma(1, shape = a, rate = b)
v = rgamma(1, shape = delta, rate = lambda)
sigma2_vec = rinvgamma(m, shape = v/2, scale = v*u2/2)
theta = as.vector(rmvnorm(1, mean = mu_0, sigma = W_0))
T_mat = rinvwishart(nu = t_0, S_0)
beta_mat = rmvnorm(m, mean = theta, sigma = T_mat)
colnames(beta_mat) = as.character(seq(1, ncol(beta_mat)))

# Places to store variables
u2_and_v_mat = matrix(NA_real_, nrow = n_sims, ncol = 2)
sigma2_mat = matrix(NA_real_, nrow = n_sims, ncol = m)
theta_mat = matrix(NA_real_, nrow = n_sims, ncol = q)
T_mat_array = array(data = NA_real_, dim = c(q, q, n_sims)) # This is an
# array with one matrix per simulation, each matrix of dimension q by q
beta_array = array(data = NA_real_, dim = c(n_sims, q, m)) # This is an array
# of matrices where row (first dimension) number i  corresponds to the 
# simulation number, column (second dimension) number j corresponds to beta_j,
# and matrix (third dimension) number k corresponds to genre.
tmp = Sys.time()
for (s in 1:n_sims) {
    print(s)
    # Update u2
    u2 = sample_post_u2(v, sigma2_vec)
    # Update v
    v = sample_post_v(old_v = v, u2 = u2, sigma2_vec = sigma2_vec)
    # Update sigma2
    for (j in 1:m) {
        
        g = genres[j]
        # filter only rows of gender g
        y_g = filter(data_set, genres == g)[, "revenue", drop = TRUE]
        n_g = length(y_g)
        X_g = data_set %>% 
            filter(genres == g) %>% 
            select(-c("revenue", "genres"))
        X_g = as.matrix(X_g)
        # head(X_g)
        beta_g = unname(beta_mat[j,])
        # beta_g
        SSR_g = get_SSR_beta_g(y_g, beta_g, X_g)
        sigma2_vec[j] = sample_post_sigma2_g(v, u2, n_g, SSR_g)
    }
    # Update theta
    theta = sample_post_theta(beta_mat, T_mat)
    # Update T_mat
    T_mat = sample_post_T_mat(beta_mat, theta)
    # Update vectors of beta
    for (j in 1:m) {
        g = genres[j]
        # filter only rows of gender g
        y_g = filter(data_set, genres == g)[, "revenue", drop = TRUE]
        n_g = length(y_g)
        X_g = data_set %>% 
            filter(genres == g) %>% 
            select(-c("revenue", "genres"))
        X_g = as.matrix(X_g)
        beta_mat[j,] = sample_post_beta(theta, T_mat, y_g, X_g, sigma2_vec[j])
    }
    # Storing variables
    u2_and_v_mat[s,] = c(u2, v)
    sigma2_mat[s, ] = sigma2_vec
    theta_mat[s, ] = theta
    T_mat_array[,, s] = T_mat
    for (j in 1:m) {
        beta_array[s, , j] = beta_mat[j,]
    }
}
save.image("gibbs_env_chitt.RData")
