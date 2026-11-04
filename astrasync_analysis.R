# =============================================================
# AstraSync AI/ML contract decision
# Decision tree, Monte Carlo simulation and sensitivity analysis
# Author: Maud Anderson
# =============================================================

library(ggplot2)
library(scales)

# -------------------------------------------------------------
# 1. Inputs
# -------------------------------------------------------------
hours_per_month <- 1200
cost_per_hour   <- 100
months          <- 18
r               <- 0.015                         # monthly discount rate
monthly_cost    <- hours_per_month * cost_per_hour  # $120,000

# Discounting helpers
AF <- (1 - (1 + r)^-months) / r   # annuity factor, 18 equal monthly payments
DF <- (1 + r)^-months             # discount factor, single payment at month 18

# Original AstraSync proposal (rejected by the client)
original_price <- 160000
pv_original    <- (original_price - monthly_cost) * AF

# Option A: equal payments
price_A <- 152000

# Option B: lower payments plus success bonus
price_B <- 125000
bonus_B <- 800000
p_bonus <- 0.7

# Option C: RFP with gain-share
bid_C    <- 150000
p_win    <- 0.4
sav_min  <- 3.2e6
sav_mode <- 5.6e6
sav_max  <- 12.8e6
n_sim    <- 1000000

# -------------------------------------------------------------
# 2. Option A
# -------------------------------------------------------------
pv_A <- (price_A - monthly_cost) * AF
ev_A <- pv_A

# -------------------------------------------------------------
# 3. Option B
# -------------------------------------------------------------
pv_B_fail    <- (price_B - monthly_cost) * AF        # monthly margin only
pv_bonus     <- bonus_B * DF                          # bonus discounted 18 months
pv_B_success <- pv_B_fail + pv_bonus

ev_B_fun <- function(p) p * pv_B_success + (1 - p) * pv_B_fail
ev_B     <- ev_B_fun(p_bonus)

# -------------------------------------------------------------
# 4. Option C: simulate client savings and AstraSync's share
# -------------------------------------------------------------
# Triangular random draws by inverse transform (no extra package needed)
rtriangular <- function(n, a, c, b) {
  u  <- runif(n)
  Fc <- (c - a) / (b - a)
  ifelse(u < Fc,
         a + sqrt(u * (b - a) * (c - a)),
         b - sqrt((1 - u) * (b - a) * (b - c)))
}

# Gain-share schedule from the client's RFP
gain_share <- function(s) {
  ifelse(s < 4e6, 0,
  ifelse(s <= 6e6, 0.20 * (s - 4e6),
  ifelse(s <= 8e6, 4e5 + 0.40 * (s - 6e6),
                   1.2e6 + 0.60 * (s - 8e6))))
}

set.seed(123)
savings <- rtriangular(n_sim, sav_min, sav_mode, sav_max)
share   <- gain_share(savings)

mean_share   <- mean(share)
median_share <- median(share)

pv_C_bid <- (bid_C - monthly_cost) * AF
pv_C_win <- pv_C_bid + mean_share * DF

ev_C_fun <- function(p, s_mean = mean_share) p * (pv_C_bid + s_mean * DF)
ev_C     <- ev_C_fun(p_win)

# Full simulated PV of Option C (win/lose drawn too), used for risk metrics
win      <- runif(n_sim) < p_win
pv_C_sim <- ifelse(win, pv_C_bid + share * DF, 0)

# Risk metrics
sd_B          <- sqrt(p_bonus * (1 - p_bonus)) * pv_bonus
sd_C          <- sd(pv_C_sim)
p_C_zero      <- mean(pv_C_sim == 0)
p_C_below_A   <- mean(pv_C_sim < pv_A)
p_share_zero  <- mean(share == 0)

# -------------------------------------------------------------
# 5. Results table
# -------------------------------------------------------------
results <- data.frame(
  Option = c("A: Equal payments", "B: Payments + bonus", "C: RFP + gain-share"),
  Monthly_price = c(price_A, price_B, bid_C),
  Expected_PV = c(ev_A, ev_B, ev_C),
  Worst_case_PV = c(pv_A, pv_B_fail, 0),
  Best_case_PV = c(pv_A, pv_B_success, pv_C_bid + gain_share(sav_max) * DF)
)
best_option <- results$Option[which.max(results$Expected_PV)]

# -------------------------------------------------------------
# 6. Sensitivity analysis on the two key inputs
# -------------------------------------------------------------
# Break-even points
be_pwin_C_vs_B  <- ev_B / pv_C_win                         # C beats B above this
be_pwin_C_vs_A  <- ev_A / pv_C_win                         # C beats A above this
be_pbonus_B_vs_A <- (ev_A - pv_B_fail) / pv_bonus          # B beats A above this
be_pbonus_B_vs_C <- (ev_C - pv_B_fail) / pv_bonus          # B beats C above this

# One-way: probability of winning the RFP
sens_pwin <- expand.grid(p = seq(0.20, 0.60, by = 0.01))
sens_pwin <- rbind(
  data.frame(p = sens_pwin$p, Option = "A", EV = ev_A),
  data.frame(p = sens_pwin$p, Option = "B", EV = ev_B),
  data.frame(p = sens_pwin$p, Option = "C", EV = ev_C_fun(sens_pwin$p))
)

# One-way: probability of earning the bonus in Option B
p_seq <- seq(0.40, 1.00, by = 0.01)
sens_pbonus <- rbind(
  data.frame(p = p_seq, Option = "A", EV = ev_A),
  data.frame(p = p_seq, Option = "B", EV = ev_B_fun(p_seq)),
  data.frame(p = p_seq, Option = "C", EV = ev_C)
)

# Two-way: best option across both probabilities
grid2 <- expand.grid(p_win = seq(0.20, 0.60, by = 0.005),
                     p_bonus = seq(0.40, 1.00, by = 0.005))
grid2$A <- ev_A
grid2$B <- ev_B_fun(grid2$p_bonus)
grid2$C <- ev_C_fun(grid2$p_win)
grid2$Best <- c("A", "B", "C")[max.col(grid2[, c("A", "B", "C")], ties.method = "first")]

# -------------------------------------------------------------
# 7. Plots
# -------------------------------------------------------------
pal <- c(A = "#8A94A6", B = "#D98E04", C = "#2F5DE0")
ink <- "#14213D"

theme_astra <- function() {
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", colour = ink),
          plot.subtitle = element_text(colour = "#4A5568"),
          axis.title = element_text(colour = "#4A5568"),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          plot.background = element_rect(fill = "white", colour = NA))
}

# 7a. Histogram of simulated gain-share (Q2.4)
p_hist <- ggplot(data.frame(share = share), aes(x = share)) +
  geom_histogram(bins = 50, fill = "#2F5DE0", colour = "white", alpha = 0.85) +
  geom_vline(xintercept = mean_share, colour = "#C0392B", linewidth = 1) +
  geom_vline(xintercept = median_share, colour = "#1E8449", linewidth = 1,
             linetype = "dashed") +
  annotate("label", x = mean_share, y = Inf, vjust = 1.3, hjust = -0.05,
           label = paste0("Mean ", dollar(round(mean_share))),
           colour = "#C0392B", size = 3.6, label.size = 0) +
  annotate("label", x = median_share, y = Inf, vjust = 3, hjust = 1.05,
           label = paste0("Median ", dollar(round(median_share))),
           colour = "#1E8449", size = 3.6, label.size = 0) +
  scale_x_continuous(labels = dollar) +
  labs(title = "Option C: simulated gain-share paid to AstraSync",
       subtitle = paste0(comma(n_sim), " simulations of client savings, ",
                         "Triangular($3.2M, $5.6M, $12.8M)"),
       x = "Gain-share at month 18 (undiscounted)", y = "Number of simulations") +
  theme_astra()

# 7b. Decision tree
fmt <- function(x) dollar(round(x, 1), accuracy = 0.1)
seg <- data.frame(
  x    = c(0, 0.6, 0.6, 0.6, 0.6, 3.5, 4.1, 4.1, 4.1, 3.5, 4.1, 4.1, 4.1),
  y    = c(0, 3, 3, 0, -3, 0, 1, 1, -1, -3, -2, -2, -4),
  xend = c(0.6, 0.6, 7, 3.5, 3.5, 4.1, 4.1, 7, 7, 4.1, 4.1, 7, 7),
  yend = c(0, -3, 3, 0, -3, 0, -1, 1, -1, -3, -4, -2, -4)
)
nodes <- data.frame(
  x = c(0, 3.5, 3.5, 7, 7, 7, 7, 7),
  y = c(0, 0, -3, 3, 1, -1, -2, -4),
  shape = c("Decision", "Chance", "Chance", rep("Payoff", 5))
)
branch_lab <- data.frame(
  x = c(0.75, 0.75, 0.75, 4.25, 4.25, 4.25, 4.25),
  y = c(3.3, 0.3, -2.7, 1.3, -0.7, -1.7, -3.7),
  label = c("Option A: $152K/month", "Option B: $125K/month + bonus",
            "Option C: RFP, bid $150K/month",
            paste0("Bonus earned (p = ", p_bonus, ")"),
            paste0("No bonus (p = ", 1 - p_bonus, ")"),
            paste0("Win RFP (p = ", p_win, ")"),
            paste0("Lose RFP (p = ", 1 - p_win, ")"))
)
payoff_lab <- data.frame(
  x = 7.25, y = c(3, 1, -1, -2, -4),
  label = c(paste0(fmt(pv_A), "  ($32K x AF)"),
            paste0(fmt(pv_B_success), "  ($5K x AF + $800K x DF)"),
            paste0(fmt(pv_B_fail), "  ($5K x AF)"),
            paste0(fmt(pv_C_win), "  ($30K x AF + E[share] x DF)"),
            "$0  (no contract)")
)
ev_lab <- data.frame(
  x = c(3.45, 3.45, 6.9), y = c(-0.5, -3.5, 3.45),
  label = c(paste0("EV = ", fmt(ev_B)), paste0("EV = ", fmt(ev_C)),
            paste0("EV = ", fmt(ev_A))),
  hjust = c(1, 1, 1)
)

p_tree <- ggplot() +
  geom_segment(data = seg, aes(x = x, y = y, xend = xend, yend = yend),
               colour = "#4A5568", linewidth = 0.6) +
  geom_point(data = nodes, aes(x = x, y = y, shape = shape, fill = shape),
             size = 6, colour = ink) +
  scale_shape_manual(values = c(Decision = 22, Chance = 21, Payoff = 24), name = NULL) +
  scale_fill_manual(values = c(Decision = "#2F5DE0", Chance = "#D98E04",
                               Payoff = "#E2E8F0"), name = NULL) +
  geom_text(data = branch_lab, aes(x = x, y = y, label = label),
            hjust = 0, size = 3.5, colour = ink) +
  geom_text(data = payoff_lab, aes(x = x, y = y, label = label),
            hjust = 0, size = 3.5, colour = ink) +
  geom_text(data = ev_lab, aes(x = x, y = y, label = label, hjust = hjust),
            size = 3.6, fontface = "bold", colour = "#2F5DE0") +
  annotate("text", x = -0.15, y = 0, hjust = 1, size = 3.6, fontface = "bold",
           colour = ink, label = paste0("Choose max EV\n", sub(":.*", "", best_option),
                                        " = ", fmt(max(results$Expected_PV)))) +
  coord_cartesian(xlim = c(-2.2, 12.3), ylim = c(-4.4, 3.7), clip = "off") +
  labs(title = "AstraSync decision tree (present values at 1.5% per month)",
       subtitle = paste0("AF = ", round(AF, 4), " (18 monthly payments),  DF = ",
                         round(DF, 4), " (single payment at month 18)")) +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = ink),
        plot.subtitle = element_text(colour = "#4A5568"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = NA))

# 7c. Sensitivity: probability of winning the RFP
p_sens_win <- ggplot(sens_pwin, aes(p, EV, colour = Option)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = p_win, linetype = "dotted", colour = ink) +
  geom_vline(xintercept = be_pwin_C_vs_B, linetype = "dashed", colour = pal["C"]) +
  annotate("text", x = be_pwin_C_vs_B, y = 3.3e5, hjust = 1.05, size = 3.4,
           colour = pal["C"], label = paste0("C beats B above\np = ",
                                             round(be_pwin_C_vs_B, 3))) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Sensitivity 1: chance of winning the RFP",
       subtitle = "Dotted line = base case (0.40)",
       x = "Probability of winning the RFP", y = "Expected PV") +
  theme_astra()

# 7d. Sensitivity: probability of earning the bonus
p_sens_bonus <- ggplot(sens_pbonus, aes(p, EV, colour = Option)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = p_bonus, linetype = "dotted", colour = ink) +
  geom_vline(xintercept = be_pbonus_B_vs_C, linetype = "dashed", colour = pal["B"]) +
  annotate("text", x = be_pbonus_B_vs_C, y = 3.3e5, hjust = -0.05, size = 3.4,
           colour = pal["B"], label = paste0("B beats C above\np = ",
                                             round(be_pbonus_B_vs_C, 3))) +
  scale_colour_manual(values = pal) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Sensitivity 2: chance of earning the Option B bonus",
       subtitle = "Dotted line = base case (0.70)",
       x = "Probability of earning the bonus", y = "Expected PV") +
  theme_astra()

# 7e. Two-way map: which option wins
p_twoway <- ggplot(grid2, aes(p_win, p_bonus, fill = Best)) +
  geom_raster() +
  annotate("point", x = p_win, y = p_bonus, size = 3.5, colour = "white",
           fill = ink, shape = 21, stroke = 1.2) +
  annotate("label", x = p_win, y = p_bonus, label = "Base case", vjust = -0.8,
           size = 3.4, label.size = 0) +
  scale_fill_manual(values = pal, name = "Best option") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Two-way view: best option by both probabilities",
       x = "Probability of winning the RFP",
       y = "Probability of earning the bonus") +
  theme_astra()

# -------------------------------------------------------------
# 8. Print results when run directly (Rscript astrasync_analysis.R)
# -------------------------------------------------------------
if (sys.nframe() == 0) {
  cat("Annuity factor (AF):", round(AF, 6), "\n")
  cat("Discount factor (DF):", round(DF, 6), "\n")
  cat("PV original proposal:", dollar(pv_original, accuracy = 0.1), "\n\n")
  cat("Q2.2 EV Option A:", dollar(ev_A, accuracy = 0.1), "\n")
  cat("Q2.3 EV Option B:", dollar(ev_B, accuracy = 0.1), "\n")
  cat("Simulated mean share:", dollar(mean_share, accuracy = 0.1),
      " median share:", dollar(median_share, accuracy = 0.1), "\n")
  cat("Q2.5 EV Option C:", dollar(ev_C, accuracy = 0.1), "\n\n")
  print(results, row.names = FALSE)
  cat("\nBreak-even P(win) C vs B:", round(be_pwin_C_vs_B, 4), "\n")
  cat("Break-even P(bonus) B vs A:", round(be_pbonus_B_vs_A, 4), "\n")
  cat("Break-even P(bonus) B vs C:", round(be_pbonus_B_vs_C, 4), "\n")

  ggsave("histogram_option_c.png", p_hist, width = 9, height = 5, dpi = 150)
  ggsave("decision_tree.png", p_tree, width = 12, height = 6, dpi = 150)
  ggsave("sensitivity_pwin.png", p_sens_win, width = 8, height = 5, dpi = 150)
  ggsave("sensitivity_pbonus.png", p_sens_bonus, width = 8, height = 5, dpi = 150)
  ggsave("sensitivity_twoway.png", p_twoway, width = 8, height = 5, dpi = 150)
}
