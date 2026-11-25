# grad-analytics-coursework

R code from my graduate analytics coursework (MS in Business Analytics). One folder per course.

| Folder | Course | Term | What's in it |
|---|---|---|---|
| [`math-543-linear-optimization`](math-543-linear-optimization) | MATH 543, Linear Optimization Methods | Fall 2024 | Textbook homework in R: LP and integer programs solved with `lpSolve` (including sensitivity and dual values), a kNN classifier with a gains chart, and two LINGO models |
| [`mngt-512-forecasting`](mngt-512-forecasting) | MNGT 512, Forecasting Models | Spring 2025 | One script per chapter of *Forecasting: Principles and Practice* (3rd ed.), from time series graphics through dynamic regression, plus two forecasting cases (cleaning, regression, ETS models) |
| [`mngt-504-multivariate-methods`](mngt-504-multivariate-methods) | MNGT 504, Multivariate Statistical Methods | Summer 2025 | R versions of the methods in each chapter (matrix algebra, multivariate plots, significance tests, distances, PCA, factor analysis, discriminant analysis, CART, clustering), a golf round-similarity analysis, and a fantasy football clustering project |

## Running the code

- R 4.x. Each folder uses these packages:
  - **math-543:** `lpSolve`, `readxl`, `class`, `gains`, `caret`, `ggplot2`, `lattice`
  - **mngt-512:** `fpp3`, `readr`, `GGally`, `broom`, `glue`, `latex2exp`
  - **mngt-504:** `tidyverse`, `vegan`, `ade4`, `cluster`, `factoextra`, `dendextend`, `FactoMineR`, `MASS`, `car`, `psych`, `rpart`, `rpart.plot`, `pheatmap`, `ffanalytics`, and a few more (see the `library()` calls at the top of each script)
- No data is included. The scripts read from a `data/` folder; [`data/README.md`](data/README.md) lists which files they expect and where they come from.
- These are class exercises, not a package. Some scripts call `View()` or run top to bottom in an interactive session.
