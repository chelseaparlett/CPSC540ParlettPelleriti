# s/o to Chat GPT for helping simulat the data
# libs
library(brms)
library(bayesplot)
library(tidybayes)

# sim data
set.seed(540)
n <- 200
parental_income <- rnorm(n, mean = 50, sd = 10) # income_in_k
z <- 1 + 0.05 * parental_income + rnorm(n, 0, 1)
p <- 1 / (1 + exp(-z))
passed_exam <- rbinom(n, 1, p) 
df <- data.frame(parental_income, passed_exam)

# priors
priors <- c(
  prior(normal(0, 5), class = "b"),
  prior(normal(0, 5), class = "Intercept")
)

# fit
fit <- brm(passed_exam ~ parental_income, data = df,
           family = bernoulli(), prior = priors,
           seed = 123, chains = 4, cores = 4, iter = 2000)

# summary
summary(fit)

# convergence
mcmc_trace(fit) 

# pp_check
pp_check(fit)

# plot params
mcmc_areas(fit,
           pars = c("b_parental_income"))

#-----

# Bayesian Mixed Effect Models
# s/o to Chat GPT for helping simulate the data
# Load required libraries
library(brms)
library(bayesplot)
library(tidybayes)

# sim
set.seed(123)
n_schools <- 10
n_students <- 20
total_n <- n_schools * n_students

school <- factor(rep(1:n_schools, each = n_students))
parental_income <- rnorm(total_n, mean = 50, sd = 10) # Parental income in thousands of dollars
school_effect <- rnorm(n_schools, 0, 5)[school]
individual_error <- rnorm(total_n, 0, 5)
exam_score <- 50 + 0.5 * parental_income + school_effect + individual_error

df <- data.frame(school, parental_income, exam_score)

# priors
priors <- c(
  prior(normal(0, 10), class = "b"),
  prior(normal(50, 10), class = "Intercept"),
  prior(exponential(1), class = "sd")
)

# fit
fit <- brm(exam_score ~ parental_income + (1 + parental_income | school),
           data = df, family = gaussian(), prior = priors,
           seed = 123, chains = 4, cores = 4, iter = 2000)

# summary
summary(fit)

# converge
mcmc_trace(fit) 

# pp_check
pp_check(fit)

# plot
mcmc_areas(fit, pars = c("b_parental_income"))

