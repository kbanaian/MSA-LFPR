# Shift-share decomposition over sex x age cells.
#
# For consecutive available years a -> b, with population shares w and rates r:
#   change in rate = sum(wbar * dr)  [behavior]  +  sum(rbar * dw)  [composition]
# using midpoint (Shapley) weights, so the two parts add exactly to the change.
# The age/sex-adjusted series accumulates only the behavior parts and is anchored
# to the latest actual rate (as on econvitals). The aging-only counterfactual
# holds each cell's rate at a base year and lets only the shares w move.

cell_arrays <- function(cells, geo = "msa") {
  d <- cells[cells$geo == geo, ]
  d$sex <- factor(d$sex, SEXES); d$age <- factor(d$age, AGES)
  d <- d[order(d$year, d$sex, d$age), ]
  years <- sort(unique(d$year))
  pick <- function(v) t(sapply(years, function(y) d[d$year == y, v]))
  P <- pick("pop"); L <- pick("lf"); E <- pick("emp")
  rownames(P) <- rownames(L) <- rownames(E) <- years
  list(years = years, w = P / rowSums(P), lfpr = L / P, epop = E / P, pop = P, lf = L, emp = E)
}

decompose <- function(A, measure = c("lfpr", "epop"), bases = integer()) {
  measure <- match.arg(measure)
  w <- A$w; r <- A[[measure]]; n <- length(A$years)
  actual <- 100 * rowSums(w * r)
  beh_cell <- matrix(0, n, ncol(w))
  comp <- numeric(n)
  for (i in seq_len(n)[-1]) {
    beh_cell[i, ] <- 100 * 0.5 * (w[i - 1, ] + w[i, ]) * (r[i, ] - r[i - 1, ])
    comp[i]       <- 100 * sum(0.5 * (r[i - 1, ] + r[i, ]) * (w[i, ] - w[i - 1, ]))
  }
  beh <- rowSums(beh_cell)
  cumb <- cumsum(beh)
  adjusted <- actual[n] - (cumb[n] - cumb)
  cf <- list()
  for (b in bases[bases %in% A$years]) {
    rb <- r[as.character(b), ]
    cf[[as.character(b)]] <- list(
      cf      = 100 * as.vector(w %*% rb),
      contrib = 100 * w * sweep(r, 2, rb))   # rows sum to actual - cf
  }
  list(years = A$years, actual = actual, adjusted = adjusted,
       behavior = beh, composition = comp, cum = apply(beh_cell, 2, cumsum), cf = cf)
}

totals <- function(cells) {
  agg <- stats::aggregate(cbind(pop, lf, emp) ~ geo + year, cells, sum)
  data.frame(geo = agg$geo, year = agg$year,
             lfpr = 100 * agg$lf / agg$pop, epop = 100 * agg$emp / agg$pop)
}

# Monthly LAUS rates over ACS civilian population 16+, interpolated geometrically
# between July points and extended past the last ACS year at its trailing
# five-year growth rate.
monthly_rates <- function(laus, A) {
  pop <- stats::setNames(rowSums(A$pop), A$years)
  yrs <- A$years
  last <- max(yrs); g <- (pop[[as.character(last)]] / pop[[as.character(max(yrs[yrs <= last - 5]))]])^
    (1 / (last - max(yrs[yrs <= last - 5])))
  ym <- as.integer(substr(laus$month, 1, 4)); mo <- as.integer(substr(laus$month, 6, 7))
  t <- ym + (mo - 7) / 12
  p <- vapply(t, function(tt) {
    if (tt >= last) return(pop[[as.character(last)]] * g^(tt - last))
    if (tt <= min(yrs)) return(pop[[as.character(min(yrs))]])
    y0 <- max(yrs[yrs <= tt]); y1 <- min(yrs[yrs > tt])
    pop[[as.character(y0)]] * (pop[[as.character(y1)]] / pop[[as.character(y0)]])^((tt - y0) / (y1 - y0))
  }, 0)
  ma12 <- function(x) vapply(seq_along(x), function(i) {
    if (i < 12) return(NA_real_)
    v <- x[(i - 11):i]; if (sum(!is.na(v)) < 11) NA_real_ else mean(v, na.rm = TRUE)
  }, 0)
  lfpr <- 100 * laus$lf / p; epop <- 100 * laus$emp / p
  data.frame(month = laus$month, lfpr = lfpr, epop = epop, lfpr12 = ma12(lfpr), epop12 = ma12(epop), pop = p)
}
