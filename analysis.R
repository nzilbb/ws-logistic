# Let's do some logistic regression
library(tidyverse)

# Mixed effects modelling
library(lme4)
library(lmerTest)

# Predicting and plotting
library(modelbased)

# auc function comes from here.
library(ModelMetrics)

# Create random names in simulated data
library(randomNames)

## Logistic Regression

# The following code simulates some data.

sim_data_1 <- 
  tibble(
    rides_bike = sample(c(TRUE, FALSE), size = 70, replace = T),
    cool_points = if_else(rides_bike, rnorm(n=70), rnorm(n=70, mean=0.1)),
    eats_pasta = sample(c(TRUE, FALSE), size = 70, replace = T)
  ) |> 
  mutate(
    log_odds = -0.2 + 0.3*eats_pasta + 0.5*cool_points,
    prob = plogis(log_odds),
    y_binom = map_dbl(prob, ~ rbinom(n=1, size=1, prob=.x)),
    variant = factor(if_else(
        y_binom == 0, "Variant 1", "Variant_2"
      )
    )
  ) |> 
  select(rides_bike, cool_points, eats_pasta, variant)

# Have a look at the data
View(sim_data_1)

# Plot the variables. Which do you think are likely to explain variation?

sim_data_1 |> 
  ggplot(
    aes(
      x = # try some variables here.,
      y = variant
    )
  ) +
  geom_jitter(height = 0.1)
  # Maybe add some facets?

# Fit a multiple logistic regression using glm().

fit_1 <- glm(
  # FINISH ME.
  family = # What family? 
)

# What are the coefficients?

# Plot a prediction?

# Generate an AUC value for the model

auc(fit_1)

# Now we'll try with grouping. The following code simulates data with grouping
# (multiple observations for each participant). You'll only need to add 
# a random intercept for participants.

sim_participants <-  
  tibble(
    participant_name = randomNames(70),
    year_of_birth = sample(seq(1920:1999), size = 70, replace = T),
    rides_bike = sample(c(TRUE, FALSE), size = 70, replace = T),
    cool_points = if_else(rides_bike, rnorm(n=70), rnorm(n=70, mean=0.1)),
    eats_pasta = sample(c(TRUE, FALSE), size = 70, replace = T),
    idiosyncracy = rnorm(n=70, sd=0.5)
  ) 

sim_data_2 <- sim_participants |> 
  mutate(
    more_explanatory = map(
      participant_name,
      ~ {
        n_obs = sample(10:60, size = 1)
        tibble(
          preceeding_environment = factor(
            sample(
              c("env_1", "env_2", "env_3"), size = n_obs, replace = T
            )
          ),
          following_environment = factor(
            sample(
              c("env_1", "env_2", "env_3"), size = n_obs, replace = T
            )
          ),
          style = factor(
            sample(
              c("formal", "informal"), size = n_obs, replace = T
            )
          ),
        )
      }
    )
  ) |> 
  unnest(more_explanatory) |> 
  mutate(
    pe_coef = case_when(
      preceeding_environment == "env_1" ~ -0.2,
      preceeding_environment == "env_2" ~ 0,
      preceeding_environment == "env_3" ~ 0.1,
    ),
    fe_coef = case_when(
      preceeding_environment == "env_1" ~ 0.1,
      preceeding_environment == "env_2" ~ -0,
      preceeding_environment == "env_3" ~ -0.25,
    ), 
    style_coef = if_else(
      style == "formal", -0.9, 0.2
    ),
    yob_s = scale(year_of_birth),
    log_odds = 
      -0.2 + 0.01 * year_of_birth + 0.5*rides_bike + 0.3*cool_points + pe_coef + 
      fe_coef + style_coef + idiosyncracy,
    prob = plogis(log_odds),
    y_binom = map_dbl(prob, ~ rbinom(n=1, size=1, prob=.x)),
    variant = factor(if_else(
      y_binom == 0, "Variant 1", "Variant_2"
    ))
  )

# Let's look at the data
View(sim_data_2)

# Fit a mixed effects logistic regression using `glmer()`.

fit_2 <- glmer(
  # fomula goes here,
  data = sim_data_2,
  family = # what goes here?
)
