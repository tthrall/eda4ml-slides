# Slide Exercise Additions/Revisions

---

## 1. ts-freq-domain-slides.qmd — NEW EXERCISES

Insert after line 690 (after "Practical Checklist"):

```markdown
# Exercises

## Team Exercise 1: White Noise Spectrum

Generate 256 observations of Gaussian white noise using `stats::rnorm()`:

1. Compute and plot the raw periodogram using `stats::spec.pgram(..., spans = NULL)`.
2. The true spectrum is flat. How much does the periodogram deviate from flat?
3. Apply smoothing with `spans = c(7, 7)`. How does the estimate improve?
4. Why is the periodogram inconsistent even though it's unbiased?

## Team Exercise 2: Identifying Periodicities

Using the SOI data (`astsa::soi`):

1. Plot the spectrum with appropriate smoothing.
2. Identify the two dominant peaks. What periods do they correspond to?
3. One peak is at annual frequency. What physical phenomenon explains the other?
4. How does the spectrum reveal information that the ACF shows less clearly?

## Team Exercise 3: Bandwidth Trade-off

Using the sunspot data (`astsa::sunspotz`):

1. Estimate the spectrum with `spans = c(3, 3)` (narrow bandwidth).
2. Estimate again with `spans = c(15, 15)` (wide bandwidth).
3. How do the estimates differ? Which shows the ~11-year peak more clearly?
4. What would you choose for a final analysis, and why?

## Team Exercise 4: Coherence Interpretation

For the HNL-NYC temperature data:

1. At what frequency is coherence highest? Why?
2. At what frequencies is coherence near zero? What does this mean physically?
3. If you added a third city (e.g., London), what coherence pattern would you expect with NYC?
4. How is coherence related to correlation? When might they differ?

::: {.notes}
Exercise 1 builds intuition for periodogram variability. Exercise 2 practices reading spectra. Exercise 3 addresses the key practical decision. Exercise 4 extends to bivariate analysis.
:::

## Discussion Questions

1. "The spectrum and ACF contain the same information." Why might the spectrum be more useful for some questions?

2. You observe a spectral peak but aren't sure if it's real or noise. How would you assess this?

3. When would you prefer time domain methods over frequency domain methods for the same data?
```

---

## 2. graph-theory-slides.qmd — REFORMATTED EXERCISES

Replace lines 1026-1037 with:

```markdown
# Exercises

## Team Exercise 1: Karate Club Centrality

Load the Karate Club network using `igraph::make_graph("Zachary")`:

1. Compute betweenness centrality for all nodes.
2. Which node has the highest betweenness? 
3. Why does this make sense sociologically—what role does this node play?
4. Compare to degree centrality. Do the rankings agree?

## Team Exercise 2: Community Detection

Apply the Louvain algorithm (`igraph::cluster_louvain()`) to the Karate Club:

1. How many communities does it find?
2. Compare the detected communities to the known faction split (Mr. Hi vs. John A).
3. Which nodes, if any, are "misclassified"? Why might this be?
4. Try Walktrap (`igraph::cluster_walktrap()`). Do the results differ?

## Team Exercise 3: LearningGraph Exploration

Install `eda4mldata` from GitHub and load `learning_graph`:

1. Build the skill prerequisite graph from `learning_graph$edges$prerequisite`.
2. Verify that it is acyclic. What would a cycle mean pedagogically?
3. Find the skill with the most prerequisites (highest in-degree).
4. Find the skill that is prerequisite to the most others (highest out-degree).

## Team Exercise 4: Bipartite Projection

Create a bipartite graph: students × courses they've taken.

- Alice: Math, Stats, Programming
- Bob: Stats, Programming, ML
- Carol: Math, Stats
- Dan: Programming, ML

1. Project onto students (edges connect students sharing courses).
2. What are the edge weights? Which pair is most similar?
3. Project onto courses. What does an edge between two courses mean?
4. When is bipartite projection useful in practice?

::: {.notes}
Exercise 1 focuses on centrality interpretation. Exercise 2 practices community detection. Exercise 3 uses the textbook's LearningGraph. Exercise 4 covers bipartite graphs.
:::

## Discussion Questions

1. Social networks, citation networks, and neural networks are all called "networks." What do they share? How do they differ?

2. When would betweenness centrality be more informative than degree centrality?

3. How do graph-derived features differ from tabular features for machine learning?
```

---

## Notes

- Both now follow the consistent format: `# Exercises` (level 1), `## Team Exercise N: Title` (level 2), `## Discussion Questions` (level 2)
- Each has 3-4 team exercises suitable for in-class work
- Speaker notes provide instructor guidance
- Discussion questions work as wrap-up or take-home

**Still needed**: Upload `info-theory-slides.qmd` for the section level fixes you mentioned.

---

## 3. ts-freq-domain.qmd — CHAPTER EXERCISES

Insert as a new section before Resources:

```markdown
## Exercises {#sec-ts-freq-domain-exercises}

1. **White Noise Spectrum.** Generate 500 observations of Gaussian white noise using `stats::rnorm()`. (a) Compute and plot the raw periodogram using `stats::spec.pgram()` with no smoothing. (b) The true spectrum is flat (constant). How much does the periodogram deviate? (c) Apply smoothing with `spans = c(7, 7)` and compare. (d) Repeat the exercise several times with different random seeds. What do you observe about the variability of the raw periodogram vs. the smoothed estimate?

2. **Periodogram Distribution.** For the white noise simulations above, at a fixed Fourier frequency $\lambda_j$, the periodogram ordinate $I(\lambda_j)$ follows a $\chi_2^2/2$ distribution (scaled by the true spectrum). (a) Generate 1000 periodogram ordinates at a single frequency by repeating the simulation. (b) Plot a histogram and overlay the theoretical $\chi_2^2/2$ density. (c) What does this tell you about confidence intervals for spectral estimates?

3. **SOI Spectrum.** Using the Southern Oscillation Index data (`astsa::soi`): (a) Plot the smoothed spectrum with appropriate bandwidth. (b) Identify the two dominant peaks and convert their frequencies to periods in months. (c) The annual peak is expected. What physical phenomenon explains the other dominant peak? (d) Compare the spectrum to the ACF. Which diagnostic more clearly reveals the El Niño periodicity?

4. **Sunspot Cycle.** The sunspot series (`astsa::sunspotz`) exhibits a well-known approximately 11-year cycle. (a) Estimate the spectrum and identify the dominant peak. (b) The peak is broad rather than sharp. What does this indicate about the solar cycle? (c) Are there harmonics visible? What do they imply about the waveform shape? (d) Using the spectrum, estimate what fraction of total variance is attributable to the ~11-year cycle.

5. **Bandwidth Selection.** Using the recruitment series (`astsa::rec`): (a) Estimate the spectrum with three different bandwidths—narrow (`spans = c(3, 3)`), medium (`spans = c(7, 7)`), and wide (`spans = c(15, 15)`). (b) How do the estimates differ in terms of variance and resolution? (c) Which bandwidth would you choose for a final analysis? Justify your choice. (d) How does sample size affect the appropriate bandwidth choice?

6. **AR(2) Spectrum.** The recruitment series is well-modeled by an AR(2) process. (a) Fit an AR(2) model using `stats::ar.ols()`. (b) Compute the theoretical AR(2) spectrum from the fitted coefficients using the formula in the chapter. (c) Overlay the theoretical spectrum on the smoothed periodogram. How well do they match? (d) Where does the spectral peak occur, and what period does this correspond to?

7. **Coherence Analysis.** Using the SOI and recruitment data (both from `astsa`): (a) Estimate the coherence between the two series. (b) At what frequencies is coherence highest? (c) The SOI measures atmospheric pressure differences; recruitment measures fish population. What physical mechanism might explain their coherence? (d) Is there a phase relationship visible in the cross-spectrum?

8. **Temperature Coherence.** Using the HNL-NYC temperature data from `eda4mldata`: (a) Estimate and plot the coherence. (b) At what frequency is coherence essentially 1? Why? (c) At what frequencies is coherence near 0? What does this mean physically? (d) If you had temperature data from London, what coherence pattern would you expect with NYC? With Honolulu?
```
