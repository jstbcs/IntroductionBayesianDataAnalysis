
library(brms)
library(tidyverse)

set.seed(35) # to reproduce 

# brms needs a prior on the logit-scale intercept (since the model
# uses a bernoulli(link = "logit")), but you want to make your prior to be theta ~ Beta(alpha, beta)
# this converts a Beta(alpha, beta) prior into an approximating Normal prior on
# the logit scales: first we simulate n_sim draws from Beta(prior_alpha, prior_beta) --> p_sim
# then we transform this to the logit scale: theta_sim = log(p / (1-p))
# and then we return the mean and sd of the transformed draws
# This mean and sd pair is then used as the parameters of a Normal() prior on the b_Intercept
# this is the standard way to get a "Beta like" prior into a logistic regression in brms
# since there's no closed-form formular to switch between logit-Normal and Beta
get_logit_normal_prior <- function(prior_alpha, prior_beta, n_sim = 1e5) {
  p_sim     <- rbeta(n_sim, prior_alpha, prior_beta)
  theta_sim <- log(p_sim / (1 - p_sim))
  c(mean = mean(theta_sim), sd = sd(theta_sim))
}

# this function builds an Bernoulli (0,1) dataset from success/failure counts
# and fits a logistic regression with only intercept in brms with a prior that is
# taken from the function before --> get_logit_normal_prior()
# then it transforms the posterior draws of b_Intercept back to being a probabbility
# arguments for the function are successes, failures (binomial outcome), prior_alpha, prior_beta (hyperparameters of the conceptual Beta prior)
fit_brms_binomial <- function(successes, failures, prior_alpha, prior_beta,
                               label = NULL) {

  prior_pars <- get_logit_normal_prior(prior_alpha, prior_beta)
  # get the logit-Normal prior that is matching the Beta(prior_alpha, prior_beta)

  dat <- data.frame(y = c(rep(1, successes), rep(0, failures)))
  # makes counts into one row per trial (successes rows of y=1,failures rows of y=0)
  # this is the long format brms needs for a bernoulli family model
  # equivalent to a binomial(successes, n) model

  fit <- brm(
    data   = dat,
    family = bernoulli(link = "logit"),
    y ~ 0 + Intercept,
    prior = prior_string(
      paste0("normal(", round(prior_pars["mean"], 4), ", ",
                          round(prior_pars["sd"],   4), ")"),
      coef = "Intercept"
    ),
    iter    = 2000,
    warmup  = 700,
    chains  = 4,
    cores   = 4,
    silent  = 2,
    refresh = 0
  )
  # Intercept only logistic regression: y ~ 0 + Intercept --> removes the default intercept parameterization by brms
  # so that the prior you specify (coef = "Intercept") is applied to b_Intercept

  post <- as_draws_df(fit) %>%
    mutate(theta = exp(b_Intercept) / (1 + exp(b_Intercept)))
  # extracting posterior draws and transform the logit-scale intercept back 
  # to a probability theta using the inverse logit

  if (!is.null(label)) post$sample <- label

  # we return the fitted model, posterior draws (with theta added), the posterior mean of theta and a credible interval
  list(
    fit      = fit,
    post     = post,
    estimate = mean(post$theta),
    ci       = quantile(post$theta, c(0.025, 0.975))
  )
}


## prior theta ~ Beta(5, 15)  (25% soapy belief)
# fits three models with the informative prior (mean is about 5/(5+15) = 0.25)
# but increasing sample size
# this is holding the observed proportion of successes more or less constant at abozt 8% (4/50, 40/500, 400/5000)
# by this we isolate the effect of sample size on how much the informative prior gets "overruled" by the data
res1_brms <- fit_brms_binomial(successes = 4,   failures = 50   - 4,
                                prior_alpha = 5, prior_beta = 15,
                                label = "n = 50")

res2_brms <- fit_brms_binomial(successes = 40,  failures = 500  - 40,
                                prior_alpha = 5, prior_beta = 15,
                                label = "n = 500")

res3_brms <- fit_brms_binomial(successes = 400, failures = 5000 - 400,
                                prior_alpha = 5, prior_beta = 15,
                                label = "n = 5000")

# posterior mean estimates and CIs
res1_brms$estimate; res1_brms$ci
res2_brms$estimate; res2_brms$ci
res3_brms$estimate; res3_brms$ci



## Prior theta ~ Beta(1, 1)  (uniform)
# now with an uninformative (flat) prior
res1_brms_u <- fit_brms_binomial(successes = 4,   failures = 50   - 4,
                                  prior_alpha = 1, prior_beta = 1,
                                  label = "n = 50")

res2_brms_u <- fit_brms_binomial(successes = 40,  failures = 500  - 40,
                                  prior_alpha = 1, prior_beta = 1,
                                  label = "n = 500")

res3_brms_u <- fit_brms_binomial(successes = 400, failures = 5000 - 400,
                                  prior_alpha = 1, prior_beta = 1,
                                  label = "n = 5000")

# posterior mean estimate and CIs
res1_brms_u$estimate; res1_brms_u$ci
res2_brms_u$estimate; res2_brms_u$ci
res3_brms_u$estimate; res3_brms_u$ci

# putting the three posteriors (with flat prior) into one long data frame, ordered by "sample" 
# so you can plot this later 
# post_all_uniform <- bind_rows(res1_brms_u$post, res2_brms_u$post, res3_brms_u$post) %>%
 # mutate(sample = factor(sample, levels = c("n = 50", "n = 500", "n = 5000")))


# With the flat prior, the posterior mean is less biased toward the prior's belief even for the smallest sample (n = 50) but is
# also a  bit wider (less information from the prior)
# When n gets bigger, the two priors converge to the same posterior 


## sanity check: compare brms estimates to our results from bayes_binomial()
# for each of the 3 sample sizes x 2 priors we store the brms posterior mean
comparison <- tibble(
  sample       = rep(c("n=50", "n=500", "n=5000"), times = 2),
  prior        = rep(c("Beta(5,15)", "Beta(1,1)"), each = 3),
  brms_mean    = c(res1_brms$estimate,   res2_brms$estimate,   res3_brms$estimate,
                    res1_brms_u$estimate, res2_brms_u$estimate, res3_brms_u$estimate)
)

print(comparison)

