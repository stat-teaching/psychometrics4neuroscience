## Correcting PB - Selection Models (SM)

. . .

- SM assume a **relationship between the p-value and the probability of publishing**

. . .

- SM **estimate this relationship** from available studies and **correct the average effect**

. . .

:::{.cons}
- The models are complicated (number of parameters) and need a large $k$
:::

. . .

:::{.pros}
- They provide a very elegant framework to formalize the publication bias supporting simulations and methods development
:::

## SM - Publication Bias Function

- The publication bias can be formalized using a **weight function** that assign a probability to a certain study properties (e.g., p-value, sample size, z-score, etc.) representing the likelihood of that study being published.

. . .

- The general idea [e.g., @Citkowicz2017-ox] is to use a weighted probability density function (wPDF). In the presence of publication bias, the parameters of the wPDF will be different (i.e., adjusted) compared to unweighted PDF (i.e., assuming no publication bias)

## SM - Publication Bias Function

The random-effect meta-analysis PDF can be written as [e.g., @Citkowicz2017-ox]:

$$
f\left(y_i \mid \beta, \tau^2 ; \sigma_i^2\right)=\phi\left(\frac{y_i-\Delta_i}{\sqrt{\sigma_i^2+\tau^2}}\right) / \sqrt{\sigma_i^2+\tau^2},
$$

And adding the weight function:

$$
f\left(Y_i \mid \beta, \tau^2 ; \sigma_i^2\right)=\frac{\mathrm{w}\left(p_i\right) \phi\left(\frac{y_i-\Delta_i}{\sqrt{\sigma_i^2+\tau^2}}\right) / \sqrt{\sigma_i^2+\tau^2}}{\int_{-\infty}^{\infty} \mathrm{w}\left(p_i\right) \phi\left(\frac{Y_i-\Delta_i}{\sqrt{\sigma_i^2+\tau^2}}\right) / \sqrt{\sigma_i^2+\tau^2} d Y_i}
$$

## SM - Publication Bias Function {#sec-pub-bias-fun}

For example, Citkowicz and Vevea [-@Citkowicz2017-ox] proposed a model using a weight function based on the Beta distribution with two parameters $a$ and $b$^[[https://www.youtube.com/watch?v=ucmOCuyCk-c](https://www.youtube.com/watch?v=ucmOCuyCk-c)]
$w(p_i) = p_i^{a - 1} \times (1 - p_i)^{b - 1}$:

```{r}
#| echo: false
wbeta <- function(x, a, b) x^(a - 1) * (1 - x)^(b - 1)

pval <- seq(0, 1, 0.01)
plot(pval, wbeta(pval, 1, 1), type = "l", ylim = c(0, 1), col = 1, lwd = 2,
xlab = "P-value", ylab = "Relative Likelihood of Selection")
lines(pval, wbeta(pval, 1, 5), col = 2, lwd = 2)
lines(pval, wbeta(pval, 1, 20), col = 3, lwd = 2)
lines(pval, wbeta(pval, 1, 50), col = 4, lwd = 2)

legend("topright", legend = c("a = 1, b = 1", "a = 1, b = 5", "a = 1, b = 20", "a = 1, b = 50"), fill = 1:4)
```

## Selection Models

In R we can use the `metafor::selmodel()` function to implement several type of models. For example we can apply the Citkowicz and Vevea [-@Citkowicz2017-ox] model:

```{r}
sel_beta <- selmodel(fit, type = "beta")
```

::: {.panel-tabset}

### Results

```{r}
#| echo: false
sel_beta
```

### Plot

```{r}
plot(sel_beta)
```

:::

## Selection Models

Let's try the Beta selection model without publication bias:

```{r}
set.seed(2023)
dat <- sim_studies(30, 0.5, 0, 30)
fit <- rma(yi, vi, data = dat, method = "ML")
sel_beta <- selmodel(fit, type = "beta")
plot(sel_beta)
```

## More on SM and Publication Bias

- The SM documentation of `metafor::selmodel()` [https://wviechtb.github.io/metafor/reference/selmodel.html](https://www.youtube.com/watch?v=ucmOCuyCk-c)
- Wolfgang Viechtbauer overview of PB [https://www.youtube.com/watch?v=ucmOCuyCk-c](https://www.youtube.com/watch?v=ucmOCuyCk-c)
- @Harrer2021-go - Doing Meta-analysis in R - [Chapter 9](https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pub-bias.html)
- @McShane2016-bk for a nice introduction about publication bias and SM
- Another good overview by @Jin2015-ik
- See also @Guan2016-kn, @Maier2023-js and @Bartos2022-im for Bayesian approaches to PB

## More on SM and Publication Bias

Assessing, testing and developing sofisticated models for publication bias is surely important and interesting. But as Wolfgang Viechtbauer (the author of `metafor`) said:

> hopefully there won't be need for these models in the future [@Viechtbauer2021-od]

## Extra - Simulating Publication Bias {.extra}

I wrote two custom functions to generate a biased dataset based on different criteria:

- a simple step function with a threshold (e.g., $p \leq 0.05$ then 1 else 0)
- the weight function from slide [@sec-pub-bias-fun]
- a custom criterion (e.g., select only $|y_i| > 0.5$)

```{r}
#| echo: false
#| output: asis
filor::print_fun(c(funs$weigth_beta, funs$weigth_2step))
```

...continue

## Extra - Simulating Publication Bias {.extra}

```{r}
#| echo: false
#| output: asis
filor::print_fun(c(funs$sim_pub_bias))
```

## Extra - Simulating Publication Bias {.extra}

For example, let's simulate a pretty simple publication bias model where we included $k = 40$ studies. The biased criteria is that only $p \leq 0.05$ are included.

- $\theta = 0.5$
- $\tau^2 = 0$ i.e. an EE model
- $n \sim \mathcal{U}(10, 200)$

```{r}
#| echo: false

pval <- seq(0, 1, 0.01)
plot(pval, ifelse(pval <= 0.05, 1, 0), type = "l", xlab = "P Value", ylab = "Probability of Publishing", lwd = 2, col = "firebrick")
```

## Extra - Simulating Publication Bias {.extra}

```{r}
set.seed(2023)
k <- 100
theta <- 0.5
tau2 <- 0.1

dat <- sim_pub_bias(selmodel = list(method = "2step", param = "pval", th = 0.05, side = "<="), k = k, theta = theta, tau2 = tau2, nmin = 10, nmax = 200)

filor::trim_df(dat)
```

## Extra - Simulating Publication Bias {.extra}

```{r}
fit <- rma(yi, vi, data = dat)
summary(fit)
```

Clearly, $\theta$ is overestimated because we are systematic omitting non-significant p-values. These studies are more likely to be imprecise studies thus we are probably creating a funnel plot asymmetry.

## Extra - Simulating Publication Bias {.extra}

```{r}
egger <- regtest(fit)
funnel(fit, cex.lab = 1.2, cex = 1.2)
se <- seq(0,1.8,length=100)
lines(coef(egger$fit)[1] + coef(egger$fit)[2]*se, se, lwd=3)
```

## PB Sensitivity analysis

```{r}
#| code-fold: true
tab <- data.frame(
  steps = c(0.005, 0.01, 0.05, 0.10, 0.25, 0.35, 0.50, 0.65, 0.75, 0.90, 0.95, 0.99, 0.995, 1),
  delta.mod.1 = c(1, 0.99, 0.95, 0.80, 0.75, 0.65, 0.60, 0.55, 0.50, 0.50, 0.50, 0.50, 0.50, 0.50),
  delta.sev.1 = c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.40, 0.35, 0.30, 0.25, 0.10, 0.10, 0.10, 0.10),
  delta.mod.2 = c(1, 0.99, 0.95, 0.90, 0.80, 0.75, 0.60, 0.60, 0.75, 0.80, 0.90, 0.95, 0.99, 1.00),
  delta.sev.2 = c(1, 0.99, 0.90, 0.75, 0.60, 0.50, 0.25, 0.25, 0.50, 0.60, 0.75, 0.90, 0.99, 1.00))

tab |> 
  pivot_longer(2:ncol(tab), values_to = "weight") |> 
  mutate(side = ifelse(grepl("2", name), "2 tails", "1 tail"),
         bias = ifelse(grepl("sev", name), "severe", "moderate")) |> 
  select(-name) |> 
  ggplot(aes(x = steps, y = weight)) +
  geom_step() +
  facet_grid(side ~ bias) +
  ggtitle("Vevea and Woods (2005)")
```

## PB Sensitivity analysis

`metafor` easily implements this sensitivity analysis approach. Let's apply the approach with a biased and unbiased dataset.

```{r}
k <- 50 # more reasonable value
es <- 0.3
tau2 <- 0.1
n <- runif(k, 10, 100)
dat <- sim_studies(k, es, tau2, n)
dat_pb <- sim_biased_studies()
```
