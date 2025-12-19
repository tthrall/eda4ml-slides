# eda4ml-slides

Slide decks for *Exploratory Data Analysis for Machine Learning*, a textbook for Masters-level data science students.

## Overview

These [Quarto Reveal.js](https://quarto.org/docs/presentations/revealjs/) presentations accompany the online textbook. Each slide deck distills a chapter's key concepts into lecture-ready format, with speaker notes, discussion prompts, and exercises suitable for classroom use.

## Slide Decks

| File | Chapter | Title |
|------|---------|-------|
| `eda-slides.qmd` | 1 | Exploratory Data Analysis |
| `conditioning-slides.qmd` | 2 | Conditional Distributions |
| `clustering-slides.qmd` | 3 | Clustering |
| `simulation-slides.qmd` | 4 | Statistical Simulation |
| `study-design-slides.qmd` | 5 | Sampling and Study Design |
| `info-theory-slides.qmd` | 6 | Information Theory |
| `lin-reg-slides.qmd` | 7 | Linear Regression |
| `pca-slides.qmd` | 8 | Principal Component Analysis |
| `lin-discr-slides.qmd` | 9 | Linear Discriminant Analysis |
| `text-as-data-slides.qmd` | 10 | Text as Data |
| `topic-models-slides.qmd` | 11 | Topic Models |
| `ts-data-slides.qmd` | 12 | Time Series Data |
| `ts-time-domain-slides.qmd` | 13 | Time Domain Methods |
| `ts-freq-domain-slides.qmd` | 14 | Frequency Domain Methods |
| `graph-theory-slides.qmd` | 15 | Graph Theory for Machine Learning |

## Usage

### Rendering slides

To render a single slide deck:

```bash
quarto render eda-slides.qmd
```

To render all slide decks:

```bash
quarto render
```

### Viewing slides

Open the rendered `.html` file in a browser. Press `S` to open speaker notes in a separate window.

### Keyboard shortcuts (Reveal.js)

| Key | Action |
|-----|--------|
| `→` / `←` | Next / previous slide |
| `S` | Speaker notes |
| `O` | Slide overview |
| `F` | Fullscreen |
| `?` | Help |

## Dependencies

### R packages

The slide decks use the following packages:

```r
# Core
install.packages(c("tidyverse", "knitr"))

# Data
remotes::install_github("tthrall/eda4mldata")

# Chapter-specific (install as needed)
install.packages(c(
 "GGally",      # conditioning, clustering
 "palmerpenguins", # clustering, lin-discr
 "HistData",    # lin-reg (Galton)
  "tidytext",    # text-as-data, topic-models
  "igraph",      # graph-theory
  "astsa"        # ts-data, ts-time-domain, ts-freq-domain
))
```

### Stylesheet

All slide decks use a shared theme defined in `eda4ml-slides.scss`. This file must be present in the same directory as the `.qmd` files (or adjust the theme path in each file's YAML header).

## Customization

### Timing

Most slide decks include an instructor timing guide in the appendix. A typical 50-minute lecture covers roughly 25–30 slides with discussion.

### Speaker notes

Speaker notes appear in `:::  {.notes}` blocks. These are visible only in presenter view (press `S`).

### Exercises

Exercises at the end of each deck align with the corresponding textbook chapter. They are categorized as:

- **Conceptual** — Understanding core ideas
- **Calculations** — Working through examples by hand
- **Programming** — R implementation
- **Advanced** — Deeper exploration (optional)

## Related Resources

- **Textbook**: [eda4ml](https://github.com/tthrall/eda4ml) — Full book source
- **Data package**: [eda4mldata](https://github.com/tthrall/eda4mldata) — Datasets used in examples
- **Student workbook**: *(coming soon)* — Guided exercises for each chapter

## License

These materials are intended for educational use in conjunction with the textbook.

## Author

Tony Thrall  
tthrall@alumni.stanford.edu
