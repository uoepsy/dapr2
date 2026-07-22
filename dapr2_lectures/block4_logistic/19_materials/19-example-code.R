# Logistic regression: Practice analysis
# Follows workflow here: dapr2/2627/dapr2_flashcards/resources/workflow-b4.html

# Phase 1: Before model fitting ---- 

## 1a: Set up code and data ---- 

# Load the required R packages
library(tidyverse)
library(psych)
library(sjPlot)
library(kableExtra)
library(car)
library(effects)

# Read in data
delay_data <- read_csv('https://uoepsy.github.io/data/mallow2_cleaned.csv')

# Tidy data (e.g., any missingness, any implausible values? are data types set correctly?).
glimpse(delay_data)

# - describe continuous variables
describe(delay_data$age)  # looks plausible – min 3.25, max 10, mean 6.56 years

# - get frequency distributions for categorical variables
table(delay_data$timeofday)   # fine, no weird values
table(delay_data$visibility)  # fine, no weird values
table(delay_data$taken)       # fine, no weird values


## 1b: Set up the variables you'll model ----

# Based on the RQ, decide whether you'll test your hypotheses using coefficient significance tests or model comparison.
# - RQ: Does the probability of delaying gratification change as a function of age, marshmallow visibility, and time of day?
# - RQ asks about how a probability changes as a function of some predictors – so that's coefficients

# Based on the RQ, decide whether your model requires an interaction between predictors.
# - RQ doesn't ask about whether one predictor's effect depends on another predictor
# - So we can answer the RQ with a multiple regression model only

# Based on the RQ, identify your outcome variable and the predictors.
# - Outcome: `taken`
# - Predictors: `age`, `visibility`, `timeofday`

# Based on the outcome variable, decide whether to use regular regression or logistic regression.
# - The outcome variable `taken` is binary (taken vs. waited), so we'll use logistic regression

# If logistic regression, set up the outcome variable so that desired level will be modelled as "success".
# - The RQ wants us to estimate the probability of a "waited" response.
# - Therefore "waited" needs to be the "success" level, coded as 1
delay_data <- delay_data |>
  mutate(
    taken = ifelse(taken == 'waited', 1, 0)
  )

# Set up categorical predictors (e.g., factor levels? which contrast coding scheme?)
# - No specific requirements for contrast coding scheme from RQ
# - So let's just do R's default treatment coding, and use the alphabetically first level for each variable
# - Can just convert to factors and check contrast matrices
delay_data <- delay_data |>
  mutate(
    visibility = factor(visibility),
    timeofday = factor(timeofday),
  )
contrasts(delay_data$visibility)  # ref level = hidden
contrasts(delay_data$timeofday)   # ref level = am


# Set up continuous predictors
# - It's reasonable to mean-centre age, because we have no observations of 0-year-olds in the data
# - It would also be reasonable not to, because 0 is a possible age, but let's do it for the practice
delay_data <- delay_data |>
  mutate(
    age_c = age - mean(age)
  )

# Explore patterns in the data by plotting outcome and predictor variables together
# - We'll have the continuous variable on the x axis, the responses on the y axis at 0 and 1,
#   and facets for each combination of visibility and timeofday
delay_data |>
  ggplot(aes(x = age_c, y = taken)) +
  
  # wiggle the points around a little bit so they don't all overlap
  geom_jitter(height = 0.01) +
  
  # facet by categorical predictors
  facet_grid(visibility ~ timeofday) +
  
  # label only 0 and 1 on the y axis
  scale_y_continuous(breaks = c(0, 1)) +
  
  # add logistic curve
  geom_smooth(
    method = "glm",                          # not a straight line
    method.args = list(family = binomial),   # specify logistic curve
    se = FALSE          # remove standard error ribbon around line
  ) +
  
  # add labels for clarity (currently taken = 1 looks like they took it, not waited!)
  labs(
    x = 'Age (in years, mean-centered)',
    y = 'Delayed gratification? (0 = took marshmallow, 1 = waited)'
  )



# Phase 2: Model fitting ----

# Write the mathematical model formulation for your model(s)
# - Refer back to contrast matrices to know what the reference levels will be.
#   $$
#   \text{logodds(waited)} = \beta_0 + 
#   (\beta_1 \cdot \text{visibility}_\text{visible}) + 
#   (\beta_2 \cdot \text{timeofday}_\text{pm}) + 
#   (\beta_3 \cdot \text{age})
#   $$
  
  
# Explicitly define the hypotheses that your RQ is aiming to test.
# - RQ asks whether any predictors affect the outcome, so the H0 is that none of them do
#   $$
#   \begin{align}
#   H_0 &: \text{All } \beta_j = 0 \text{ for } j = 2, 3, 4 \\
#   H_1 &: \text{Any } \beta_j \neq 0\\
#   \end{align}
#   $$

# Fit your model(s) using `lm()` or `glm()` as appropriate.
delay_mdl <- glm(
  taken ~ age_c + visibility + timeofday, 
  data = delay_data,
  family = binomial
)
delay_mdl

# Phase 3: After model fitting ----

## 3a: Check assumptions and diagnostics ----

# Check model assumptions
rstandard(delay_mdl, type = "deviance") |> plot() # nothing outside -/+3, good
  
# Run diagnostics for multicollinearity.
vif(delay_mdl)  # all close to 1, good

# Run diagnostics for influential observations.
plot(delay_mdl, which = 4)  # all well below 0.5 threshold

# If you find extreme influential observations: run sensitivity analysis.
# - NA
   

## 3b: Plot and interpret model estimates ----
   
# Interpret the coefficient estimates
summary(delay_mdl)

# - Intercept:
#   - For a child of average age in the morning when the marshmallow is hidden, 
#     the log-odds of delaying gratification are 0.63.
plogis( coef(delay_mdl)[[1]] )  # or 65% probability
#   - This estimate is significantly different from zero (p = .011), 
#     so significantly more likely than chance.

# age_c:
#   - Holding visibility and time of day constant, increasing one year in age
#     is associated with an increase of 0.58 in the log-odds of delaying gratification.
#   - This estimate is significantly different from zero (p < .001), 


# - visibilityvisible:
#   - Holding age and time of day constant, changing the marshmallow from hidden to visible 
#     is associated with a decrease of 1.20 in the log-odds of delaying gratification.
#   - This estimate is significantly different from zero (p < .001), 

# timeofdaypm:
#   - Holding age and visibility constant, changing the time of day from am to pm
#     is associated with an increase of 0.49 in the log-odds of delaying gratification.
#   - This estimate is not significantly different from zero (p = .096).


# Generate a nicely-formatted regression table.
tab_model(
  delay_mdl,
  transform = NULL,  # display coefficients as log-odds
  dv.labels = 'Log-odds of delaying gratification',
  pred.labels = c(
    'Intercept', 'Visibility [visible]', 'Time of day [pm]', 'Age (mean-centered)'
  ),
  title = "Logistic regression model coefficient estimates"
)

# Compare models and interpret model comparison statistics (if appropriate for your RQ) 
# - NA.

# Plot model-fitted values and/or simple slopes/simple effects.
# - We'll plot the data in probability space, since that's what more readers will understand.
# - To create swoopy-line plots in the 2x2 facet grid:
Effect(
  focal.predictors = c('age_c', 'visibility', 'timeofday'), 
  mod = delay_mdl,
  xlevels = 20  # how many values of `age_c` to estimate outcomes for
) |>
  as.data.frame() |>
  ggplot(aes(x = age_c, y = fit)) +
  # a line for the model-fitted estimates
  geom_line() +
  # a ribbon for the 95% CIs
  geom_ribbon( aes( ymin = lower,  ymax = upper ),  alpha = .3 ) +
  # facet by visibility and timeofday
  facet_grid(visibility ~ timeofday) +
  # axis labels for clarity
  labs(
    y = 'Probability of delaying gratification',
    x = 'Age (years, mean-centered)'
  ) 


## 3c: Write up -- in labs! ----
