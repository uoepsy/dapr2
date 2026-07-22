# Interactions: Practice analysis
# Follows workflow here: dapr2/2627/dapr2_flashcards/resources/workflow-b3.html

# Phase 1: Before model fitting ---- 

## 1a: Set up code and data ---- 

# Load the required R packages
library(tidyverse)
library(psych)
library(sjPlot)
library(kableExtra)
library(patchwork)
library(car)
library(interactions)

# Read in data
notes_df <- read_csv('https://uoepsy.github.io/data/laptop_vs_longhand.csv')

# Tidy data (e.g., any missingness, any implausible values? are data types set correctly?).
glimpse(notes_df) # one numeric, two categorical

# - Check numeric for weird values using describe()
describe(notes_df$test_score)  # min and max look reasonable

# Check each categorical for weird values using table()
table(notes_df$medium)         # fine
table(notes_df$study)          # fine

# - Check for NAs
table(is.na(notes_df))  # no NAs, good to continue


## 1b: Set up the variables you'll model ----

# Based on the RQ, decide whether you'll test your hypotheses using coefficient significance tests or model comparison.
# - RQ: Is the association between amount of studying and test scores affected by the note-taking medium that people used (notes either written longhand or typed on a laptop)?
# - This question asks about associations between predictors and outcomes (and whether other predictors affect this association), so test using coef significance tests.

# Based on the RQ, decide whether your model requires an interaction between predictors.
# - RQ asks whether an association between predictor and outcome is affected by the value of another predictor, so this is a classic interaction RQ.

# Based on the RQ, identify your outcome variable and the predictors.
# - Outcome: `test_score`
# - Predictors: `medium`, `study`, and their interaction

# Set up categorical predictors (e.g., factor levels? which contrast coding scheme?)
# - Both categ preds should be converted to factors
# - We've been using treatment coding for all interaction models so far, so we'll do that here
# - Reference level of `study` should be "No" (the smallest amount of studying, 
#   so coefs will tell us how much improvement)
# - Reference level of `medium` doesn't really matter, let's say "Laptop" so we can see
#   if "Longhand" is an improvement

notes_df <- notes_df |>
  mutate(
    medium = factor(medium, levels = c('Laptop', 'Longhand')),
    study = factor(study, levels = c('No', 'Minimal', 'Moderate', 'Extensive'))
  )

# - Check contrast coding
contrasts(notes_df$medium)
contrasts(notes_df$study)

# Set up continuous predictors –  NA.

# Explore patterns in the data by plotting outcome and predictor variables together
# - For multiple regression, we plotted predictors separately, but for interactions,
#   it's nice to combine them all into a single plot.
notes_df |>
  ggplot(aes(x = study, y = test_score, fill = study, colour = study)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', colour = 'black', size = 3) +
  theme(legend.position = 'none') +
  facet_wrap(~ medium)  +
  labs(
    y = 'Test score',
    x = 'Amount of study'
  )


# Phase 2: Model fitting ----

# Write the mathematical model formulation for your model(s)
# - Refer back to contrast matrices to know what the dummy variables will be.
#   $$
#   \begin{align}
#   \text{test score} ~=~& \beta_0 ~+ \\
#     & (\beta_1 \cdot \text{study}_\text{Minimal}) ~+ \\
#     & (\beta_2 \cdot \text{study}_\text{Moderate}) ~+ \\
#     & (\beta_3 \cdot \text{study}_\text{Extensive}) ~+ \\
#     & (\beta_4 \cdot \text{medium}_\text{Longhand}) ~+ \\
#     & (\beta_5 \cdot \text{study}_\text{Minimal}   \cdot \text{medium}_\text{Longhand}) ~+ \\
#     & (\beta_6 \cdot \text{study}_\text{Moderate}  \cdot \text{medium}_\text{Longhand}) ~+ \\
#     & (\beta_7 \cdot \text{study}_\text{Extensive} \cdot \text{medium}_\text{Longhand}) ~+ \\
#     & \epsilon
#   \end{align}
#   $$
  
# - NOTE: Incorrect model formula (because disregards dummy variables):
#   $$
#   \begin{align}
#   \text{test score} ~=~& \beta_0 ~+ \\
#     & (\beta_1 \cdot \text{study}) ~+ \\
#     & (\beta_2 \cdot \text{medium}) ~+ \\
#     & (\beta_3 \cdot \text{study} \cdot \text{medium}) ~+ \\
#     & \epsilon
#   \end{align}
#   $$
  
  
# Explicitly define the hypotheses that your RQ is aiming to test.
# - Are any of the interaction terms significant?
#   $$
#   \begin{align}
#   H_0 &: \text{All } \beta_j = 0 \text{ for } j = 5, 6, 7\\
#   H_1 &: \text{Any } \beta_j \neq 0\\
#   \end{align}
#   $$

# Fit your model(s) using `lm()`.
notes_mdl <- lm(test_score ~ study * medium, data = notes_df)
notes_mdl

# Phase 3: After model fitting ----

## 3a: Check assumptions and diagnostics ----

# Check model assumptions:
# - Linearity of association -- assume it's met bc all preds are categorical
# - Independence of errors -- assume it's met bc we have one obs per person
# - Normality of errors
plot(notes_mdl, which = 2)  # good!
# - Equal variance of errors
plot(notes_mdl, which = 1)  # good! 
#   (the indiv lines are bc we only observed a handful of diff test scores)
  
# Run diagnostics for multicollinearity.
vif(notes_mdl)  # obviously the interaction term is correlated with both, hence R's recommendation
vif(notes_mdl, type = 'predictor')  # all GVIF = 1, ideal!

# Run diagnostics for influential observations.
plot(notes_mdl, which = 4)  # no super extreme influential points, prob no need for sensitivity analysis

# If you find extreme influential observations: run sensitivity analysis. - NA

   
## 3b: Plot and interpret model estimates ----
   
# Interpret the coefficient estimates
summary(notes_mdl)
confint(notes_mdl) |> round(2)

# - `(Intercept)`
#   - The estimated mean test score for people who didn't study at all and who took notes on their laptop was 48.12 points.

# - `studyMinimal`                 
#   - When people took notes on their laptop and studied minimally, their test scores were estimated to increase by 7.45 points compared to laptop note-takers who didn't study at all.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `studyModerate`                
#   - When people took notes on their laptop and studied moderately, their test scores were estimated to increase by 11.18 points compared to laptop note-takers who didn't study at all.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `studyExtensive`               
#   - When people took notes on their laptop and studied extensively, their test scores were estimated to increase by 13.04 points compared to laptop note-takers who didn't study at all.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `mediumLonghand`               
#   - When people did not study at all and took longhand notes, their test scores were estimated to increase by 2.90 points compared to non-studiers who took notes on a laptop.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `studyMinimal:mediumLonghand`  
#   - For people who take notes longhand, the effect of minimal studying is estimated to increase by 2.44 points.
#   - Or: For people who study minimally, the effect of taking notes longhand is estimated to increase by 2.44 points.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `studyModerate:mediumLonghand` 
#   - For people who take notes longhand, the effect of moderate studying is estimated to increase by 18.49 points.
#   - Or: For people who study moderately, the effect of taking notes longhand is estimated to increase by 18.49 points.
#   - This estimate is significantly different from zero ($p$ < .001).

# - `studyExtensive:mediumLonghand`
#   - For people who take notes longhand, the effect of extensive studying is estimated to increase by 26.52 points.
#   - Or: For people who study extensively, the effect of taking notes longhand is estimated to increase by 26.52 points.
#   - This estimate is significantly different from zero ($p$ < .001).

# Generate a nicely-formatted regression table.
tab_model(
  notes_mdl,
  dv.labels = 'Test score',
  title = "Linear model coefficient estimates"
)

# Compare models and interpret model comparison statistics (if appropriate for your RQ) - NA.

# Plot model-fitted values and/or simple slopes/simple effects.
cat_plot(
  notes_mdl,
  pred = 'study',
  modx = 'medium',
  geom = 'line'  # clearly shows the different pattern in each medium
)

## 3c: Write up -- in labs! ----
