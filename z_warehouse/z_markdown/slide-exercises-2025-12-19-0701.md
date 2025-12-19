# Slide Exercise Sections — Draft

Insert these sections near the end of each slide deck, before any Resources section.

---

## 1. eda-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Response vs. Predictor

For the father-son height data:

1. Which variable would you designate as the "response" and which as the "predictor"?
2. Could a reasonable argument be made for the reverse?
3. Describe a situation where this distinction would not apply.

## Team Exercise 2: Regression to the Mean

Galton observed that extremely tall or short fathers tend to have sons who are not quite so extreme.

1. Using the summary table of sons' heights by father's height, can you see evidence of this phenomenon?
2. How would you quantify it?
3. Why does this happen? (Hint: think about the role of the mother's height and random variation.)

## Team Exercise 3: Misleading Summaries

1. Find or construct an example where summary statistics (mean, SD, correlation) give a misleading impression.
2. What visualization would reveal the true structure?
3. What does this imply about the order of operations in EDA?

::: {.notes}
Exercise 1 connects to the dual aims theme. Exercise 2 is historically important and counterintuitive. Exercise 3 leads naturally to Anscombe's quartet in the next chapter.
:::

## Discussion Questions

1. A colleague says "I have 10 million rows—I don't need to look at the data." How would you respond?

2. When might prediction and understanding conflict? Give an example.

3. What would Tukey think about large language models doing data analysis?
```

---

## 2. conditioning-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Bivariate Normal Construction

Given independent standard normal $X$ and $Z$, and correlation $r$:

1. Construct $Y = rX + \sqrt{1 - r^2} Z$
2. What are the unconditional mean and SD of $Y$?
3. What is $\text{Cor}(X, Y)$?
4. How would you generalize to arbitrary means $(\mu_x, \mu_y)$ and SDs $(\sigma_x, \sigma_y)$?

## Team Exercise 2: Simpson's Paradox

The UC Berkeley admissions example showed an aggregate bias that reversed within departments.

1. As a team, construct a different example of Simpson's paradox (can be hypothetical).
2. What is the lurking variable in your example?
3. Which analysis gives the "correct" answer—aggregated or disaggregated?

## Team Exercise 3: Correlation vs. Independence

Construct an example where $X$ and $Y$ are statistically dependent but have $r = 0$.

1. Sketch the joint distribution of $(X, Y)$.
2. Why does correlation fail to detect the dependence?
3. What does this imply for feature selection in machine learning?

::: {.notes}
Exercise 1 builds simulation skills. Exercise 2 reinforces the importance of conditioning. Exercise 3 connects to mutual information (Chapter 6).
:::

## Discussion Questions

1. "Correlation does not imply causation." When does correlation *suggest* causation?

2. In what situations is an aggregated analysis appropriate despite Simpson's paradox?

3. How would you explain conditional expectation to a manager?
```

---

## 3. clustering-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Iris K-means

Using `datasets::iris`:

1. Apply K-means with $K = 3$ to the four numeric measurements.
2. Cross-tabulate clusters with `Species`. How well do they align?
3. Which species is hardest to separate? Why might this be?

## Team Exercise 2: College Data Exploration

Using `ISLR2::College`:

1. How many private vs. public schools? What is the range of expenditure per student?
2. Use `GGally::ggpairs()` on 4–5 variables. Which appear correlated?
3. What groupings would you *expect* to find—elite vs. non-elite? Large vs. small?
4. Run K-means with $K = 3$. Do the clusters match your expectations?

## Team Exercise 3: Choosing K

You have customer transaction data and want to segment customers.

1. How would you decide on the number of clusters?
2. What are the pros and cons of the elbow method vs. silhouette scores?
3. Your marketing team wants exactly 5 segments. How do you respond?

::: {.notes}
Exercise 1 is quick and concrete. Exercise 2 encourages domain thinking before algorithms. Exercise 3 addresses real-world constraints.
:::

## Discussion Questions

1. When is clustering exploratory vs. confirmatory?

2. K-means finds spherical clusters. What real-world data might violate this assumption?

3. How would you validate clusters when there are no true labels?
```

---

## 4. simulation-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Mean vs. Median Efficiency

Verify the variance ratio for normal data:

1. Set $n = 30$ and $R = 5000$ replications.
2. For each replication, generate a sample from `rnorm()`, compute mean and median.
3. Calculate `var(medians) / var(means)`. Compare to $\pi/2 \approx 1.57$.
4. What does this ratio tell us about the relative efficiency of the median?

## Team Exercise 2: Heavy Tails and the Cauchy

Repeat Exercise 1, but use `rcauchy()` instead of `rnorm()`.

1. Calculate the sample variance of your $R$ means. What do you observe?
2. Construct a histogram of sample means. Does it look normal?
3. What does this imply about the Central Limit Theorem?

## Team Exercise 3: Bootstrap Confidence Interval

Use the bootstrap to estimate a 95% CI for the median of `mtcars$mpg`:

1. Resample with replacement $B = 2000$ times.
2. Compute the median for each bootstrap sample.
3. Use the 2.5th and 97.5th percentiles as your CI.
4. How does the width compare to a bootstrap CI for the mean?

::: {.notes}
Exercises 1–2 contrast well-behaved vs. pathological distributions. Exercise 3 introduces nonparametric inference.
:::

## Discussion Questions

1. When is simulation more trustworthy than mathematical derivation?

2. A colleague sets `set.seed(42)` and runs one simulation. Is this reproducible research?

3. How many bootstrap replications are "enough"?
```

---

## 5. study-design-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Observational Studies at Work

Break into teams:

1. Identify 2–3 examples of observational studies in your work context.
2. Which would be most valuable? What questions would they answer?
3. What are the potential pitfalls (confounding, selection bias, etc.)?
4. Could any be converted to experiments? At what cost?

## Team Exercise 2: The Quiz Puzzle

A TA gives a 10-question quiz. After grading:

- Average number right: 6.4, SD: 2.0
- Average number wrong: [?], SD: [?]

Fill in the blanks—or do you need the raw data? Explain briefly.

## Team Exercise 3: Left-Handedness Puzzle

In a large health survey, the percentage of left-handed respondents decreased from 10% at age 20 to 4% at age 70.

"The data show that many people change from left-handed to right-handed as they get older."

1. True or false? Explain.
2. If false, what explains the pattern?
3. What study design would test your alternative hypothesis?

::: {.notes}
Exercise 1 grounds concepts in participants' experience. Exercises 2–3 are from FPP and test reasoning about study design.
:::

## Discussion Questions

1. What modern datasets might have Literary Digest-style selection bias?

2. When is a randomized experiment unethical or impractical?

3. How would you detect distribution shift between training and deployment?
```

---

## 6. info-theory-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Entropy of a Fair Die

1. Calculate the entropy $H$ for a fair six-sided die.
2. Compare to a loaded die with $P(6) = 0.5$ and others equal.
3. Which has higher entropy? Why does this make sense?
4. Generalize: what is $H$ for a discrete uniform distribution on $K$ outcomes?

## Team Exercise 2: UCB Admissions Revisited

Consider a box of tickets matching the UC Berkeley admissions data.

1. Restricting to just "Admitted" vs. "Rejected," calculate $H_{\text{decision}}$.
2. Now calculate the joint entropy $H_{\text{decision, sex}}$ and mutual information $MI$.
3. What does $MI \approx 0$ tell us about marginal sex bias?
4. How would you incorporate department to reveal Simpson's paradox?

## Team Exercise 3: Cross-Entropy Loss

A classifier predicts probabilities for 3 classes. True class is 1.

Compare cross-entropy loss for these predictions:

| Prediction | $(\hat{p}_1, \hat{p}_2, \hat{p}_3)$ |
|------------|-------------------------------------|
| Confident & correct | $(0.9, 0.05, 0.05)$ |
| Less confident | $(0.6, 0.2, 0.2)$ |
| Uniform | $(0.33, 0.33, 0.34)$ |

1. Calculate the loss for each.
2. What happens as $\hat{p}_1 \to 0$?
3. Why is this the standard loss for classification?

::: {.notes}
Exercise 1 builds intuition for entropy. Exercise 2 connects to conditioning chapter. Exercise 3 motivates cross-entropy in neural networks.
:::

## Discussion Questions

1. When would mutual information identify a useful feature that correlation misses?

2. Why is KL divergence not symmetric? Give an example where direction matters.

3. How does information theory connect to compression?
```

---

## 7. ts-data-slides.qmd

```markdown
# Exercises

## Team Exercise 1: Explore an `astsa` Dataset

Choose a time series from `astsa` not discussed today (e.g., `nyse`, `oil`, `prodn`):

1. Plot the series. What features do you observe?
2. Plot the ACF. Is there evidence of autocorrelation?
3. Does the series appear stationary? What would you do if not?

## Team Exercise 2: Simulate White Noise

Generate 500 observations of Gaussian white noise using `rnorm()`:

1. Plot the series, ACF, and spectrum.
2. How do they compare to theoretical signatures (flat spectrum, ACF = 0 at all lags)?
3. Repeat several times. How much sampling variation do you see?

## Team Exercise 3: Effective Sample Size

For an AR(1) process, $ESS \approx T \cdot (1 - \phi) / (1 + \phi)$.

1. Calculate ESS for $T = 200$ when $\phi = 0.5$, $0.8$, $0.95$.
2. What happens as $\phi \to 1$?
3. You have $T = 100$ observations with estimated $\phi = 0.7$. How wide should your confidence interval for the mean be, compared to the naive interval?

::: {.notes}
Exercise 1 develops pattern recognition. Exercise 2 establishes the baseline. Exercise 3 is critical for inference.
:::

## Discussion Questions

1. You fit a regression and the residuals have ACF(1) = 0.4. What are the implications?

2. When is first differencing appropriate? When might it remove too much signal?

3. "The spectrum and ACF contain the same information." True in theory—why might one be more useful in practice?
```

---

## 8. ts-time-domain-slides.qmd

```markdown
# Exercises

## Team Exercise 1: AR(1) vs. MA(1) Signatures

Using `stats::arima.sim()`:

1. Generate 200 observations from AR(1) with $\phi = 0.7$.
2. Generate 200 observations from MA(1) with $\theta = 0.7$.
3. Plot the ACF and PACF for each. Can you tell them apart?
4. Which cuts off sharply? Which decays exponentially?

## Team Exercise 2: Model Selection

For the recruitment data (`astsa::rec`):

1. Fit AR(1), AR(2), and AR(3) using `stats::ar.ols()`.
2. Compare their AIC values.
3. Does AIC agree with what the PACF suggests?
4. What are the tradeoffs of parsimony vs. fit?

## Team Exercise 3: Forecast Uncertainty

Using the global temperature series (`astsa::gtemp_land`):

1. Fit an ARIMA model to the first differences.
2. Generate a 20-year forecast with prediction intervals.
3. Why do the intervals widen over time?
4. What does this imply for long-range climate projection?

::: {.notes}
Exercise 1 is the key diagnostic skill. Exercise 2 practices model selection. Exercise 3 addresses forecast humility.
:::

## Discussion Questions

1. A model with great in-sample fit forecasts poorly. What went wrong?

2. When would you choose a simpler model despite higher AIC?

3. How do ARIMA models compare to machine learning for time series?
```

---

## Usage Notes

1. Each section is designed for ~15–20 minutes of class time.

2. "Team Exercises" work well for groups of 2–4.

3. "Discussion Questions" can be used as wrap-up or take-home reflection.

4. Speaker notes (`::: {.notes}`) provide instructor guidance.

5. Exercises are selected to reinforce core concepts without requiring extensive setup.
