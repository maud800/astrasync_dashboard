# astrasync_dashboard

# AstraSync AI/ML Contract Decision Analysis

**Live dashboard:** https://maud800.github.io/astrasync_dashboard/

A decision analysis of how AstraSync, a software company offering AI/ML services, should respond to a client's counteroffers on an 18-month implementation contract. The project combines a decision tree, Monte Carlo simulation, and sensitivity analysis in R, and presents the results as an interactive HTML dashboard.

## The business problem

AstraSync proposed billing the client $160,000 per month for 18 months (present value of $626,902). The client rejected this and offered two alternatives. If AstraSync declines both, the client will open the project to a competitive request for proposal (RFP).

| Option | Terms | Uncertainty |
|---|---|---|
| A | $152,000 per month for 18 months | None |
| B | $125,000 per month plus an $800,000 bonus at month 18 if the implementation beats a benchmark | 70% chance of earning the bonus |
| C | RFP: bid $150,000 per month plus a gain-share on the client's savings at month 18 | 40% chance of winning; savings are uncertain |

## Model inputs

| Input | Value |
|---|---|
| Effort | 1,200 hours per month at $100 per hour |
| AstraSync monthly cost | $120,000 |
| Contract length | 18 months |
| Monthly discount rate | 1.5% |
| Annuity factor (AF) | 15.6726 |
| Month-18 discount factor (DF) | 0.7649 |
| Client savings (Option C) | Triangular: low $3.2M, most likely $5.6M, high $12.8M |

The Option C gain-share follows the client's schedule:

| Client savings | AstraSync share |
|---|---|
| Below $4M | $0 |
| $4M to $6M | 20% of savings above $4M |
| $6M to $8M | $400,000 plus 40% of savings above $6M |
| Above $8M | $1.2M plus 60% of savings above $8M |

## Method

1. **Present value.** Each option's monthly margin (price minus $120,000 cost) is discounted with the annuity factor. One-time payments at month 18 are discounted with the single-payment factor.
2. **Decision tree.** Chance outcomes (bonus earned or not, RFP won or lost) are rolled back to expected present values. The decision node selects the highest expected value.
3. **Monte Carlo simulation.** Client savings are drawn 1,000,000 times from the triangular distribution, using inverse-transform sampling. Each draw is converted to a gain-share payment. The simulated mean drives Option C's expected value.
4. **Sensitivity analysis.** The two most uncertain and influential inputs are varied: the probability of winning the RFP and the probability of earning the Option B bonus. Break-even points and a two-way map show where the best option changes.

## Results

| Option | Expected PV | Worst case | Best case |
|---|---|---|---|
| A: Equal payments | $501,521.9 | $501,521.9 | $501,521.9 |
| B: Payments + bonus | $506,713.3 | $78,362.8 | $690,292.1 |
| C: RFP + gain-share | $509,431.4 | $0 | about $3.59M |

| Break-even point | Value |
|---|---|
| P(win RFP) where C equals B | 39.8% |
| P(win RFP) where C equals A | 39.4% |
| P(bonus) where B equals A | 69.2% |
| P(bonus) where B equals C | 70.4% |

## Recommendation

On expected value alone, Option C ranks first, but only by about $2,700 over Option B. The base-case probabilities sit almost exactly on the break-even points, so small changes in either estimate change the ranking. Option C also carries a 60% chance of earning nothing and a standard deviation of about $766K, compared with about $280K for Option B.

The dashboard recommends **Option B**. It gives up less than 1% of expected value in exchange for a guaranteed contract, a far smaller spread of outcomes, and a bonus that rewards the implementation success AstraSync already expects.

## Repository contents

| File | Description |
|---|---|
| `index.html` | The rendered dashboard served by GitHub Pages |
| `index.Rmd` | R Markdown source for the dashboard |
| `astrasync_analysis.R` | All calculations, simulation, sensitivity analysis and plots |
| `decision_tree.png` | Decision tree with probabilities, payoffs and expected values |
| `histogram_option_c.png` | Distribution of the simulated gain-share with mean and median |

## How to reproduce

Requires R with the `ggplot2`, `scales` and `rmarkdown` packages.

```r
install.packages(c("ggplot2", "scales", "rmarkdown"))

# Print all results and save the plots as PNG files
source("astrasync_analysis.R")   # or run: Rscript astrasync_analysis.R

# Rebuild the dashboard
rmarkdown::render("index.Rmd")
```

Keep `index.Rmd` and `astrasync_analysis.R` in the same folder. The simulation uses `set.seed(123)` so results are reproducible.

## Author

Maud Anderson, MS in Business Analytics, William & Mary
