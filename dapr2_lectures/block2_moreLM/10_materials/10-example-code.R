# Categorical predictors: Practice analysis
# Follows workflow here: dapr2/2627/dapr2_flashcards/resources/workflow-b2.html

# Phase 1: Before model fitting ---- 

## 1a: Set up code and data ---- 

# Load the required R packages
library(tidyverse)
library(psych)
library(sjPlot)
library(kableExtra)
library(patchwork)
library(car)

# Read in data
attend_df <- read_csv("https://uoepsy.github.io/data/DapR2_S1B2_PracticalPart1.csv")

# rename Conscientiousness because it's a lot to type
attend_df <- attend_df |>
  rename(Consc = Conscientiousness)

# **Tidy data** (e.g., any missingness, any implausible values? are data types set correctly?).
summary(attend_df)  # no NAs, no wacky values
glimpse(attend_df)  # lets us see example values for categorical preds, looks OK


## 1b: Set up the variables you'll model ----

# Based on the RQ, decide whether you'll test your hypotheses using coefficient significance tests or model comparison.
# - RQ: Do people with greater conscientiousness and more advanced years of study attend university courses more?
# - This question is about associations between predictors and outcomes, so we can test it using coef significance tests.

# Based on the RQ, identify your outcome variable and the predictors.
# - Outcome: `Attendance`
# - Predictors: `Consc` and `Year`

# Set up categorical predictors (e.g., factor levels? which contrast coding scheme?)

# - First, `Consc` has three levels: `Low`, `Moderate`, and `High`.
# - This is not their alphabetical order, so let's specify this order using `mutate()`.
attend_df <- attend_df |>
  mutate(
    Consc = factor(Consc, levels = c('Low', 'Moderate', 'High'))
  )

# - We are interested in comparing people with higher conscientiousness to people with lower conscientiousness, so treatment coding is fine.
# - How will `Consc` be coded?
contrasts(attend_df$Consc)
# - Reference level = `Low`, and two dummy variables which compare `Low` to `Moderate` and `Low` to `High`.

# - Next, `Year` has many levels: Y1, Y2, Y3, Y4, MSc, and PhD.
# - This is also not alphabetical, so we'll specify manually.
attend_df <- attend_df |>
  mutate(
    Year = factor(Year, c('Y1', 'Y2', 'Y3', 'Y4', 'MSc', 'PhD'))
  )
contrasts(attend_df$Year)
# - `Y1` is the ref level, and we have five dummy variables which compare Y1 to each following year.

# Set up continuous predictors –  NA.

# Explore patterns in the data by plotting outcome and predictor variables together
# - Consc:
#   boxplots are fine for a quick look, but not as good because it doesn't show the data itself
attend_df |>
  ggplot(aes(x = Consc, y = Attendance)) +
  geom_boxplot()

#    violin plots are better for a report
p_consc <- attend_df |>
  ggplot(aes(x = Consc, y = Attendance, fill = Consc, colour = Consc)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', colour = 'black', size = 3) +
  theme(legend.position = 'none')
p_consc

# - Year:
#   boxplot (quick and dirty)
attend_df |>
  ggplot(aes(x = Year, y = Attendance)) +
  geom_boxplot()
#   violin plot (nicer)
p_year <- attend_df |>
  ggplot(aes(x = Year, y = Attendance, fill = Year, colour = Year)) +
  geom_violin(alpha = 0.5) +
  geom_jitter(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', colour = 'black', size = 3) +
  theme(legend.position = 'none')
p_year

# - Patchwork violin plots together:
p_consc + p_year


# Make a nice descriptives table of all relevant variables for the write-up
attend_df |>
  rename(Conscientiousness = Consc) |>
  group_by(Year, Conscientiousness) |>
  summarise(
    n = n(), 
    Mean = mean(Attendance), 
    SD = sd(Attendance),
    Minimum = min(Attendance),
    Maximum = max(Attendance)
  ) |>
  kable(
    caption = "Descriptive statistics about attendance by year of study and level of conscientiousness", 
    digits = 2
  ) |>
  kable_styling()   


# Phase 2: Model fitting ----

# Write the mathematical model formulation for your model(s)
# - Refer back to contrast matrices to know what the dummy variables will be.
#   $$
#     \text{Attendance} = \beta_0 + 
#     (\beta_1 \cdot \text{Consc}_\text{Moderate}) + 
#     (\beta_2 \cdot \text{Consc}_\text{High}) +  \\
#     (\beta_3 \cdot \text{Year}_\text{Y2}) + 
#     (\beta_4 \cdot \text{Year}_\text{Y3}) + 
#     (\beta_5 \cdot \text{Year}_\text{Y4}) + 
#     (\beta_6 \cdot \text{Year}_\text{MSc}) + 
#     (\beta_7 \cdot \text{Year}_\text{PhD}) + 
#     \epsilon
#   $$

# - A nicely aligned version:
#   $$
#   \begin{align}
#   \text{Attendance } =& \beta_0 + 
#     (\beta_1 \cdot \text{Consc}_\text{Moderate}) + 
#     (\beta_2 \cdot \text{Consc}_\text{High}) ~ + \\
#   & (\beta_3 \cdot \text{Year}_\text{Y2}) + 
#     (\beta_4 \cdot \text{Year}_\text{Y3}) + 
#     (\beta_5 \cdot \text{Year}_\text{Y4}) + \\
#   & (\beta_6 \cdot \text{Year}_\text{MSc}) + 
#     (\beta_7 \cdot \text{Year}_\text{PhD}) + 
#     \epsilon \\
#   \end{align}
#   $$
  
# - NOTE: Incorrect model formula (because disregards dummy variables):
#   $$
#    \text{Attendance} = \beta_0 + 
#    (\beta_1 \cdot \text{Consc}) + 
#    (\beta_2 \cdot \text{Year}) + 
#    \epsilon
#  $$
  
  
# Explicitly define the hypotheses that your RQ is aiming to test.
# - We want to know whether there are significant associations between any dummy variable and Attendance, so:
#   $$
#   \begin{align}
#   H_0 &: \text{ All } \beta_j = 0 \text{ for } j = 1, 2, 3, 4, 5, 6, 7\\
#   H_1 &: \text{ Any } \beta_j \neq 0\\
#   \end{align}
#   $$

# Fit your model(s) using `lm()`.
attend_mdl <- lm(
  Attendance ~ Consc + Year,
  data = attend_df
)
attend_mdl


# Phase 3: After model fitting ----

## 3a: Check assumptions and diagnostics ----

# Check model assumptions:
# - Linearity of association -- assume it's met bc all preds are categorical
# - Independence of errors -- assume it's met bc we have one obs per ppt (pid column)
# - Normality of errors
plot(attend_mdl, which = 2)  # good match to diagonal
# - Equal variance of errors
plot(attend_mdl, which = 1)  # vertical spread varies a little. judgement call. I say fine.
# - overall: looks fine!
  
# Run diagnostics for multicollinearity.
vif(attend_mdl)
# - GVIF (categorical version) close to 1, so we're good!

# Run diagnostics for influential observations.
plot(attend_mdl, which = 4)
# - There are a few data points with relatively larger Cook's Distance values.

# If you find extreme influential observations: run sensitivity analysis.
# - (NOTE: You'll need to look at main model first to compare results, so come back to this
#   if time allows!)
# - Three sensitivity analyses, one for each of the three most extreme points.

attend_mdl_no355 <- lm(
  Attendance ~ Consc + Year,
  data = attend_df[-355,]
)
summary(attend_mdl_no355)  # no change to significance pattern

attend_mdl_no365 <- lm(
  Attendance ~ Consc + Year,
  data = attend_df[-365,]
)
summary(attend_mdl_no365) # no change to significance pattern

attend_mdl_no161 <- lm(
  Attendance ~ Consc + Year,
  data = attend_df[-161,]
)
summary(attend_mdl_no161)  # no change to significance pattern
   
   
## 3b: Plot and interpret model estimates ----
   
# Interpret the coefficient estimates
summary(attend_mdl)
confint(attend_mdl) |> round(2)
# - `(Intercept)`
#   - The estimated mean attendance for people with low conscientiousness (Consc = 0 = Low) in first-year (Year = 0 = Y1) is 14.57 days.
# 
# - `ConscModerate`
#   - Holding year of study constant, moving from low conscientiousness to moderate conscientiousness is associated with an estimated increase in attendance of 10.80 days.
#   - This estimate is significantly different from zero (p < .001).
# 
# - `ConscHigh`
#   - Holding year of study constant, moving from low conscientiousness to high conscientiousness is associated with an estimated increase in attendance of 18.24 days.
#   - This estimate is significantly different from zero (p < .001).
# 
# - `YearY2`
#   - Holding conscientiousness constant, moving from Y1 to Y2 is associated with an estimated increase in attendance of 4.76 days.
#   - This estimate is significantly different from zero (p = 0.004).
# 
# - `YearY3`
#   - Holding conscientiousness constant, moving from Y1 to Y3 is associated with an estimated increase in attendance of 3.44 days.
#   - This estimate is not significantly different from zero (p = 0.07).
# 
# - `YearY4`
#   - Holding conscientiousness constant, moving from Y1 to Y4 is associated with an estimated increase in attendance of 4.60 days.
#   - This estimate is significantly different from zero (p = 0.013).
# 
# - `YearMSc`
#   - Holding conscientiousness constant, moving from Y1 to MSc is associated with an estimated increase in attendance of 5.96 days.
#   - This estimate is significantly different from zero (p = 0.004).
# 
# - `YearPhD`   
#   - Holding conscientiousness constant, moving from Y1 to PhD is associated with an estimated increase in attendance of 12.57 days.
#   - This estimate is significantly different from zero (p < 0.001).


# Generate a nicely-formatted regression table.
tab_model(
  attend_mdl,
  dv.labels = 'Attendance (days)',
  pred.labels = c(
    "ConscModerate" = "Conscientiousness [Moderate]",
    "ConscHigh" = "Conscientiousness [High]"
    ),
  title = "Linear model coefficient estimates"
)

# Compare models and interpret model comparison statistics (if appropriate for your RQ) - NA.

# Plot model-fitted values.
plot_model(
  attend_mdl, 
  type = 'eff', 
  terms = c('Year', 'Consc')   # first term is the one on x axis
) +
  labs(
    colour = 'Conscientiousness'
  )

## 3c: Write up -- in labs! ----
