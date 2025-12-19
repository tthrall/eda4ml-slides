# info-theory-slides.qmd — CORRECTED EXERCISE LEVELS

Replace lines 539-584 with the following:

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

## Summary of Changes

| Line | Before | After |
|------|--------|-------|
| 539 | `## Exercises` | `# Exercises` |
| 541 | `### Team Exercise 1: Entropy of a Fair Die` | `## Team Exercise 1: Entropy of a Fair Die` |
| 548 | `### Team Exercise 2: UCB Admissions Revisited` | `## Team Exercise 2: UCB Admissions Revisited` |
| 557 | `### Team Exercise 3: Cross-Entropy Loss` | `## Team Exercise 3: Cross-Entropy Loss` |

The `## Discussion Questions` (line 577) was already correct—it's level 2 under the `# Exercises` section.

This makes `info-theory-slides.qmd` consistent with all other slide decks.
