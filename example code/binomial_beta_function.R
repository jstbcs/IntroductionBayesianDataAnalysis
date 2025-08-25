library(ggplot2)

# Function to obtain the Posterior distribution for the Binomial-Beta Model
bayes_binomial <- function(successes, failures, prior_alpha, prior_beta){
  # Parameter of the Posterior
  aprime <- prior_alpha + successes
  bprime <- prior_beta + failures
  
  # Estimator for theta
  schaetzer <- aprime / (aprime + bprime)
  ci <- qbeta(c(0.025, 0.975), aprime, bprime)
  
  # Plot
  cols <- hcl(h = seq(15, 375
                      , length = 3)
              , l = 65, c = 100)[1:2]
  p <- ggplot(data.frame(x = 1), aes(x = x)) + 
    xlim(c(0, 1)) +
    stat_function(fun = dbeta
                  , args = list(prior_alpha, prior_beta)
                  , geom = "area", alpha = 0.35, aes(fill = 'Prior')) + 
    stat_function(fun = dbeta
                  , args = list(aprime, bprime)
                  , geom = "area", alpha = 0.35, aes(fill = 'Posterior')) + 
    scale_fill_manual(name='Distribution',
                      breaks=c('Prior', 'Posterior'),
                      values=c('Prior' = cols[1], 'Posterior' = cols[2])) +
    xlab(expression("Parameter" ~ theta)) +
    ylab("Probability Density") +
    theme_light(base_size = 14)
  
  return(list("estimate" = schaetzer, "ci" = ci, "p" = p))
}


posterior1 <- 
  bayes_binomial(successes = 32
                 , failures = 8
                 , prior_alpha = 4
                 , prior_beta = 4)

posterior1$estimate; posterior1$ci
posterior1$p