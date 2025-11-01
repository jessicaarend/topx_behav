# Dissertation prospectus power analysis
# Sept 16 2025

########################
# power analysis for aim 1 LMER
# Load libraries
library(lme4)
library(simr)

set.seed(123)  # reproducibility

# 1) Define study parameters
N <- 136           # number of participants
reps <- 3          # repeated measurements per participant
effect_size <- 0.5 # difference between groups (Cohen's d ~0.5)
resid_sd <- 1      # residual SD

# 2) Simulate participant IDs and group assignment
Subject <- factor(rep(1:N, each = reps))
Group <- factor(rep(sample(c("Control", "EP"), N, replace = TRUE), each = reps))

# 3) Simulate random intercept for participants
# Random intercept SD (ICC related)
rand_intercept_sd <- 0.3
u <- rnorm(N, mean = 0, sd = rand_intercept_sd)
u_long <- rep(u, each = reps)

# 4) Simulate outcome
Y <- effect_size*(Group == "EP") + u_long + rnorm(N*reps, sd = resid_sd)

dat <- data.frame(Y, Group, Subject)

# 5) Fit linear mixed model
model <- lmer(Y ~ Group + (1|Subject), data = dat)

# 6) Convert to simr object
model_sim <- extend(model, along = "Subject", n = N)

# 7) Run power simulation for fixed effect "GroupEP"
# Check summary(model) for exact fixed effect name
summary(model)
power_result <- powerSim(model_sim, "GroupEP", nsim = 500)  # start with 500 sims
power_result


########################
# power analysis for aim 2 path analysis

# Function to compute power for RMSEA
# N = sample size
# df = model degrees of freedom
# alpha = Type I error rate
# rmsea0 = null RMSEA (close fit)
# rmseaA = alternative RMSEA (worse fit)

rmsea_power <- function(N, df, alpha = 0.05, rmsea0 = 0.05, rmseaA = 0.08) {
  
  # Noncentrality parameters
  lambda0 <- N * df * rmsea0^2  # under null
  lambdaA <- N * df * rmseaA^2  # under alternative
  
  # Critical chi-square value for rejecting null
  chi_crit <- qchisq(1 - alpha, df)
  
  # Power = probability of exceeding chi-square crit under alternative
  power <- 1 - pchisq(chi_crit, df, ncp = lambdaA)
  
  return(power)
}

# Example usage:
df <- 20           # degrees of freedom of your SEM model
N <- 179           # planned sample size
alpha <- 0.05
rmsea0 <- 0.05     # close fit (null)
rmseaA <- 0.08     # not-close fit (alternative)

power <- rmsea_power(N, df, alpha, rmsea0, rmseaA)
power

###########
# find required N
# Simple numeric search to find sample size for desired power
required_N <- function(df, alpha = 0.05, rmsea0 = 0.05, rmseaA = 0.08, target_power = 0.8) {
  N_seq <- seq(50, 1000, by = 1)
  for (N in N_seq) {
    if (rmsea_power(N, df, alpha, rmsea0, rmseaA) >= target_power) return(N)
  }
  return(NA)
}

required_N(df = 10, alpha = 0.05, rmsea0 = 0.05, rmseaA = 0.08, target_power = 0.8)
