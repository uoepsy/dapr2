# RQ1 example code ----

# Follows workflow here: dapr2/2627/dapr2_flashcards/resources/workflow.html

## Phase 1: Before model fitting ----

### 1a: Set up code and data ----

# Load required R packages
library(tidyverse)   # for wrangling
library(psych)       # for summarising
library(sjPlot)      # for nice tables/plots
library(kableExtra)  # for nicely-formatted tables
library(patchwork)   # for combining plots into one graphic


# Read in data
fomo <- read_csv("https://uoepsy.github.io/data/FOMOdataset.csv")


# Tidy data if needed
summary(fomo)
# Ranges all look reasonable, and there are no NAs, so we're good to continue


### 1b: Set up the variables we'll model ----

# RQ: How is people's sense of Fear of Missing Out (FOMO) associated with their age and how many Instagram followers they have?

# Based on RQ, identify outcome variables and predictors.
# - Outcome: `FOMO`
# - Predictors: `Age`, `TotalFollowers`

# Based on RQ, decide whether to test hypotheses using coefficient significance tests or model comparison.
# - The RQ is about associations between variables, so we'll test it using significance test for coef estimates.

# Set up continuous predictors. (e.g., any transformations?)
# - One option might be to z-score Age and TotalFollowers.
#   This would be an important step if Age = 0 and TotalFollowers = 0 were totally meaningless quantities (imagine Height = 0 or ShoeSize = 0 – impossible and nonexistent).
#   But Age = 0 means an infant, and TotalFollowers = 0 means someone with no followers.
#   So they are reasonable quantities to generate predictions for.

# Explore patterns in the data by plotting outcome and predictor variables together.

p_age <- fomo |>
  ggplot(aes(x = Age, y = FOMO)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
p_age

p_followers <- fomo |>
  ggplot(aes(x = TotalFollowers, y = FOMO)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
p_followers

p_age + p_followers  # combine using patchwork


# Make a nice descriptives table

fomo |> 
  # Select only the variables we want in the table
  select(FOMO, Age, TotalFollowers) |>
  
  # Give the variables more readable names
  rename(
    "Fear of Missing Out" = FOMO, 
    "Age (in years)" = Age, 
    "Number of Instagram Followers" = TotalFollowers
  ) |>
  
  # Use describe() from the psych package to get descriptive stats
  describe() |>
  
  # Select only the summary stats we care about
  select(n, mean, sd, min, max) |>
  
  # Give these stats more readable names
  rename("N" = n, "Mean" = mean, "SD" = sd, "Minimum" = min, "Maximum" = max) |>
  
  # kable() from the kableExtra package formats the table nicely
  kable(
    caption = "Descriptive statistics for FOMO and socio-demographic factors", 
    digits = 2
  ) |>
  kable_styling()


## Phase 2: Model fitting ----

# Write the mathematical model formulation for your model(s).
#  $$
#    \text{FOMO} = \beta_0 + (\beta_1 \cdot \text{Age}) + (\beta_2 \cdot \text{Followers}) + \epsilon
#  $$


# Explicitly define the hypotheses that your RQ is aiming to test.
#  $$
#  \begin{align}
#  H_0 &: \text{ All } \beta_j = 0 \text{ for } j = 1, 2 \\
#  H_1 &: \text{ Any } \beta_j \neq 0 \\
#  \end{align}
#  $$

# Fit your model(s) using lm().
m1 <- lm(
  FOMO ~ Age + TotalFollowers,
  data = fomo
)


## Phase 3: After model fitting ----

### 3a: Plot and interpret model estimates ----

# Interpret the coefficient estimates (if appropriate for your RQ).
summary(m1)
confint(m1) |> round(2)
confint(m1) |> round(3)  # for TotalFollowers

# - `Intercept`: The estimated FOMO score for someone aged zero with zero total followers is 26.52 points.
# - `Age`: Increasing one year of age is associated with a decrease in FOMO score of 0.17 points (holding follower count constant). This estimate is significantly different from zero (b = –0.17, 95% CI [–0.18, –0.15], p < .001).
# - prediction borne out!
#   - `TotalFollowers`: Increasing one follower is associated with an increase in FOMO score of 0.018 points (holding age constant). This estimate is significantly different from zero (b = 0.018, 95% CI [0.016, 0.020], p < .001).
# - prediction borne out!
#   - (rounded to 3 dp because otherwise estim and CI bounds are all 0.02)


# Generate a nicely-formatted regression table (if appropriate for your RQ).
tab_model(
  m1,
  dv.labels = 'Fear of missing out (FOMO)',
  pred.labels = c("Age" = "Age (in years)"),
  title = "RQ1: Linear model coefficient estimates"
)

# Plot model-fitted values (if appropriate for your RQ).
plot_model(
  m1, 
  type = 'eff', 
  terms = c('Age', 'TotalFollowers'), 
  show.data = TRUE
)

### 3b: Write up methods and results – IN LABS ----


# Bonus challenge RQ2 example code ----

## Phase 1: Before model fitting ----

### 1a: Set up code and data – DONE ----

### 1b: Set up the variables we'll model ----

# RQ: Do the Big 5 personality traits significantly predict FOMO, in addition to the effects of age and total Instagram followers?

# Based on RQ, identify outcome variables and predictors.
# - Outcome: `FOMO`
# - Predictors: `Age`, `TotalFollowers`, and each OCEAN variable

# Based on RQ, decide whether to test hypotheses using coefficient significance tests or model comparison.
# - The RQ asks whether multiple predictors predict an outcome jointly, so it's a model comparison question.

# Set up continuous predictors. (e.g., any transformations?)
# - One option might be to z-score the OCEAN variables.
#   Zero is not very meaningful as-is.
#   But we actually don't care about interpreting any of those coefficients, just whether their inclusion helps the model.
#   So we can leave the OCEAN variables as-is too.
#   (Emphasise: this is a choice that could be made either way – just justify it!)

# Explore patterns in the data by plotting outcome and predictor variables together.
# - you could accomplish the same thing by making five plots and patchworking
#   them together, but this is a bit less repetitive

fomo |>
  rename(
    "Outgoingness" = O,
    "Conscientiousness" = C,
    "Extraversion" = E,
    "Agreeableness" = A,
    "Neuroticism" = N
  ) |>
  pivot_longer(
    cols = Neuroticism:Conscientiousness, 
    names_to = 'Trait', 
    values_to = 'Score'
  ) |>
  ggplot(aes(x = Score, y = FOMO)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = 'lm', se = F) +
  facet_wrap(~ Trait)


# Make a nice descriptives table
fomo |> 
  select(O, C, E, A, N) |>
  rename(
    "Outgoingness" = O,
    "Conscientiousness" = C,
    "Extraversion" = E,
    "Agreeableness" = A,
    "Neuroticism" = N
  ) |>
  describe() |>
  select(n, mean, sd, min, max) |>
  rename("N" = n, "Mean" = mean, "SD" = sd, "Minimum" = min, "Maximum" = max) |>
  kable(caption = "Descriptive statistics for Big 5 personality traits", digits = 2) |>
  kable_styling()


## Phase 2: Model fitting ----

# Write the mathematical model formulation for your model(s).
# - First model = model from RQ1. Second model:
#   $$
#     \text{FOMO} = \beta_0 + 
#     (\beta_1 \cdot \text{Age}) + 
#     (\beta_2 \cdot \text{Followers}) + 
#     (\beta_3 \cdot \text{O}) + 
#     (\beta_4 \cdot \text{C}) + 
#     (\beta_5 \cdot \text{E}) + 
#     (\beta_6 \cdot \text{A}) + 
#     (\beta_7 \cdot \text{N}) + 
#     \epsilon
#   $$

# Explicitly define the hypotheses that your RQ is aiming to test.
# - The null hypothesis that corresponds to this model comparison is that none of Big 5 predictors are significantly associated with FOMO.
#   $$
#   \begin{align}
#   H_0 &: \text{ All } \beta_j = 0 \text{ for } j = 3, 4, 5, 6, 7 \\
#   H_1 &: \text{ Any } \beta_j \neq 0 \\
#   \end{align}
#   $$

# Fit your model(s) using lm().
m2 <- lm(
  FOMO ~ Age + TotalFollowers + O + C + E + A + N,
  data = fomo
)

## Phase 3: After model fitting ----

### 3a: Plot and interpret model estimates ----

# Compare models and interpret model comparison statistics (if appropriate for your RQ).
# - Because these models are nested, we can use the incremental F-test via the `anova()` function.
anova(m1, m2)

### 3b: Write up methods and results – IN LABS ----
