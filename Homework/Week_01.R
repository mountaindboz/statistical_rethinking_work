# Statistical Rethinking Homework - Week 1
# https://github.com/rmcelreath/stat_rethinking_2022/blob/main/homework/week01.pdf

library(tidyverse)
library(patchwork)
library(rethinking)


# Question 1 --------------------------------------------------------------
# Suppose the globe tossing data (Chapter 2) had turned out to be 4 water and 11
# land. Construct the posterior distribution, using grid approximation. Use the
# same flat prior as in the book.

# Define # of values to use in the grid
n_val_grid <- 100

# Define grid as a dataframe
df_globe <- tibble(p_grid = seq(from = 0, to = 1, length.out = n_val_grid))

# Compute the posterior with a flat prior and observations W = 4 and L = 11
df_globe_1 <- df_globe %>%
  mutate(
    # Using flat prior:
    prior = 1,
    # Compute likelihood at each value in grid
    likelihood = dbinom(4, size = 15, prob = p_grid),
    # Compute product of likelihood and prior
    unstd_posterior = likelihood * prior,
    # Standardize the posterior, so it sums to 1
    posterior = unstd_posterior / sum(unstd_posterior)
  )

# Plot the prior
clr_prior <- "grey60"
plt_prior_1 <- df_globe_1 %>%
  ggplot(aes(x = p_grid, y = prior)) +
  geom_point(color = clr_prior, fill = clr_prior) +
  geom_line(color = clr_prior) +
  labs(
    title = "Prior Distribution",
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Prior Probability"
  ) +
  theme_bw()

# Plot the posterior
plt_post_1 <- df_globe_1 %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Posterior with observations W = 4 and L = 11",
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Posterior Probability"
  ) +
  theme_bw()

# Combine plots
plt_prior_1 + plt_post_1


# Question 2 --------------------------------------------------------------
# Now suppose the data are 4 water and 2 land. Compute the posterior again, but
# this time use a prior that is zero below p = 0.5 and a constant above p = 0.5.
# This corresponds to prior information that a majority of the Earth’s surface
# is water.

# Compute the posterior with the new prior and observations W = 4 and L = 2
df_globe_2 <- df_globe %>%
  mutate(
    # Using prior that is zero below p = 0.5 and a constant above p = 0.5:
    prior = if_else(p_grid < 0.5, 0, 1),
    # Compute likelihood at each value in grid
    likelihood = dbinom(4, size = 6, prob = p_grid),
    # Compute product of likelihood and prior
    unstd_posterior = likelihood * prior,
    # Standardize the posterior, so it sums to 1
    posterior = unstd_posterior / sum(unstd_posterior)
  )

# Plot the prior
plt_prior_2 <- df_globe_2 %>%
  ggplot(aes(x = p_grid, y = prior)) +
  geom_point(color = clr_prior, fill = clr_prior) +
  geom_line(color = clr_prior) +
  labs(
    title = "Prior Distribution",
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Prior Probability"
  ) +
  theme_bw()

# Plot the posterior
plt_post_2 <- df_globe_2 %>%
  ggplot(aes(x = p_grid, y = posterior)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Posterior with observations W = 4 and L = 2",
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Posterior Probability"
  ) +
  theme_bw()

# Combine plots
plt_prior_2 + plt_post_2


# Question 3 --------------------------------------------------------------
# For the posterior distribution from 2, compute 89% percentile and HPDI
# intervals. Compare the widths of these intervals. Which is wider? Why? If you
# had only the information in the interval, what might you misunderstand about
# the shape of the posterior distribution?

# Sample the posterior distribution from #2
samp_globe_2 <- sample(df_globe_2$p_grid, size = 1e4, replace = TRUE, prob = df_globe_2$posterior)

# Calculate the 89% percentile interval
globe_2_PI_89 <- PI(samp_globe_2)
globe_2_PI_89

# Calculate the 89% HPDI interval
globe_2_HPDI_89 <- HPDI(samp_globe_2)
globe_2_HPDI_89

# Which interval is wider?
max(globe_2_PI_89) - min(globe_2_PI_89)
max(globe_2_HPDI_89) - min(globe_2_HPDI_89)
# The 89% percentile interval


# Question 4 --------------------------------------------------------------
# OPTIONAL CHALLENGE. Suppose there is bias in sampling so that Land is more
# likely than Water to be recorded. Specifically, assume that 1-in-5 (20%) of
# Water samples are accidentally recorded instead as ”Land”. First, write a
# generative simulation of this sampling process. Assuming the true proportion
# of Water is 0.70, what proportion does your simulation tend to produce
# instead? Second, using a simulated sample of 20 tosses, compute the unbiased
# posterior distribution of the true proportion of water.


