

## Logistic Models

Because logistic regression models don’t have the same expected error distribution (we don’t expect residuals to be normally distributed around the mean, with constant variance), checking the assumptions of logistic regression is a little different.

`r optbegin("Deviance Residuals", olabel=FALSE,toggle=params$TOGGLE)`

Typically, we look at the "deviance residuals". But we __don't__ examine plots for patterns, we simply examine them for potentially outlying observations. If we use a standardised residual, it makes it easier to explore extreme values as we expect most residuals to be within -2, 2 or -3, 3 (depending on how strict we feel). 

__Deviance Residuals__  

There are three ways we can get out deviance residuals, each scaled differently: 

- $i$th residual = measure of deviance contributed from the $i$th observation
- $i$th standardized residual = residual / SD(residual)
- $i$th studentized residual = residual / SD(residual from model fitted without observation $i$)

:::blue

**In R**

```{r}
#| eval: false
# deviance residuals
residuals(sen_mdl1, type = 'deviance')

# studentised residuals
rstudent(sen_mdl1, type = 'deviance')

# standardised residuals
rstandard(sen_mdl1, type = 'deviance')
```

:::

We can visually assess whether any residuals are larger than 2 or 3 in absolute value:

:::blue

**In R**

```{r}
plot(rstandard(sen_mdl1, type = "deviance"), ylab = "Standardised Deviance Residuals")
```

:::

:::frame

[**Interpretation Guidance**]{style="color:red;"}

Most residuals should be within -2, 2 or -3, 3. 

You must specify your criteria (e.g., "we expected residuals to fall within the range of -2 to 2") before you conduct your analysis.

:::

`r optend()`

<br> 

`r optbegin("High Influence Cases", olabel=FALSE,toggle=params$TOGGLE)`

Just like in linear regression, to check for influential observations, we can use `cooks.distance()`, and plot this using `plot()`. Alternatively, you can specify `which = 4` when plotting your fitted model (i.e., `plot(model, which = ?)`.

:::blue

**In R**

```{r}
plot(sen_mdl1, which = 4)
```

:::

:::frame

[**Interpretation Guidance**]{style="color:red;"}

In logistic regression, we can use the arbitrary cut-offs of $> 0.5$ (moderately influential) or $>1$ (highly influential) to describe influential points.

:::

`r optend()`


<div class="divider div-transparent div-dot"></div>
