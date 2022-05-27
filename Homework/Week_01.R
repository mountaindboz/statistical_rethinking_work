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
df_globe <- tibble(PGrid = seq(from = 0, to = 1, length.out = n_val_grid))

# Compute the posterior with a flat prior and observations W = 4 and L = 11
df_globe_1 <- df_globe %>%
  mutate(
    # Using flat prior:
    Prior = 1,
    # Compute likelihood at each value in grid
    LH = dbinom(4, size = 15, prob = PGrid),
    # Compute product of likelihood and prior
    UnstdPost = LH * Prior,
    # Standardize the posterior, so it sums to 1
    Post = UnstdPost / sum(UnstdPost)
  )

# Plot the prior
clr_prior <- "grey60"
plt_prior_1 <- df_globe_1 %>%
  ggplot(aes(x = PGrid, y = Prior)) +
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
  ggplot(aes(x = PGrid, y = Post)) +
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
    Prior = if_else(PGrid < 0.5, 0, 1),
    # Compute likelihood at each value in grid
    LH = dbinom(4, size = 6, prob = PGrid),
    # Compute product of likelihood and prior
    UnstdPost = LH * Prior,
    # Standardize the posterior, so it sums to 1
    Post = UnstdPost / sum(UnstdPost)
  )

# Plot the prior
plt_prior_2 <- df_globe_2 %>%
  ggplot(aes(x = PGrid, y = Prior)) +
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
  ggplot(aes(x = PGrid, y = Post)) +
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
set.seed(100)
samp_globe_2 <- sample(df_globe_2$PGrid, size = 1e4, replace = TRUE, prob = df_globe_2$Post)

# Calculate the 89% percentile interval
globe_2_PI_89 <- PI(samp_globe_2)
globe_2_PI_89

# Calculate the 89% HPDI interval
globe_2_HPDI_89 <- HPDI(samp_globe_2)
globe_2_HPDI_89

# Which interval is wider?
max(globe_2_PI_89) - min(globe_2_PI_89)
max(globe_2_HPDI_89) - min(globe_2_HPDI_89)
# The 89% percentile interval - because the HDPI is always narrower than the
# percentile interval. Also the percentile interval assumes that the posterior
# distribution is symmetrical, but in this case it isn't. It's impossible to
# discern the drop to zero probability for the values less than 0.5 with just
# the information from the intervals.


# Question 4 --------------------------------------------------------------
# OPTIONAL CHALLENGE. Suppose there is bias in sampling so that Land is more
# likely than Water to be recorded. Specifically, assume that 1-in-5 (20%) of
# Water samples are accidentally recorded instead as ”Land”. First, write a
# generative simulation of this sampling process. Assuming the true proportion
# of Water is 0.70, what proportion does your simulation tend to produce
# instead? Second, using a simulated sample of 20 tosses, compute the unbiased
# posterior distribution of the true proportion of water.

# Simulate the biased sampling process
set.seed(100)
n_sims <- 1e5
n_tosses <- 20
true_n_water <- rbinom(n_sims, size = n_tosses, prob = 0.7)
obs_n_water <- rbinom(n_sims, size = true_n_water, prob = 0.8)

# Calculate the mean probabilities for the true and biased simulations
mean(true_n_water / n_tosses)
mean(obs_n_water / n_tosses)

# Simulate sample of 20 globe tosses with sampling bias
set.seed(100)
n_water <- rbinom(1, size = n_tosses, prob = 0.7 * 0.8)

#  Compute the posterior distributions with and without the sampling bias
df_globe_3 <- df_globe %>%
  mutate(
    # Using flat prior:
    Prior = 1,
    # Compute likelihood at each value in grid including sampling bias
    LH_bias = dbinom(n_water, size = n_tosses, prob = PGrid * 0.8),
    # Compute likelihood at each value in grid NOT including sampling bias
    LH_noBias = dbinom(n_water, size = n_tosses, prob = PGrid),
    # Compute product of likelihood and prior for both distributions
    across(starts_with("LH_"), ~ .x * Prior, .names = "UnstdPost{.col}"),
    # Standardize the posteriors, so they each sum to 1
    across(starts_with("Unstd"), ~ .x / sum(.x), .names = "Post_{.col}")
  )

# Plot the prior
plt_prior_3 <- df_globe_3 %>%
  ggplot(aes(x = PGrid, y = Prior)) +
  geom_point(color = clr_prior, fill = clr_prior) +
  geom_line(color = clr_prior) +
  labs(
    title = "Prior Distribution",
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Prior Probability"
  ) +
  theme_bw()

# Plot the posterior for both distributions, with and without sampling bias
plt_post_3 <- df_globe_3 %>%
  select(PGrid, starts_with("Post_")) %>%
  pivot_longer(
    cols = starts_with("Post_"),
    names_to = "Posterior",
    values_to = "Value"
  ) %>%
  mutate(Posterior = if_else(str_detect(Posterior, "_bias$"), "Bias", "No Bias")) %>%
  ggplot(aes(x = PGrid, y = Value, color = Posterior)) +
  geom_point() +
  geom_line() +
  labs(
    title = paste0("Posterior with observations W = ", n_water, " and L = ", 20 - n_water),
    subtitle = paste0(n_val_grid, " points in grid"),
    x = "Probability of Water",
    y = "Posterior Probability"
  ) +
  theme_bw()

# Combine plots
plt_prior_3 + plt_post_3

