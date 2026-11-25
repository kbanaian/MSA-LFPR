# Offline checks for the PUMS module on synthetic microdata.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE); cat("ok -", msg, "\n") }
set.seed(42)
fake <- function(n, p_fb, p_lf_native, p_lf_fb, college_codes = c(15, 16), inst = 0.01) {
  age <- sample(16:85, n, TRUE); fb <- runif(n) < p_fb
  lf <- runif(n) < ifelse(fb, p_lf_fb, p_lf_native) * ifelse(age > 64, 0.3, 1)
  esr <- ifelse(lf, ifelse(runif(n) < 0.95, 1, 3), 6)
  sch <- ifelse(age <= 24 & runif(n) < 0.6, 2, 1)
  w <- round(runif(n, 10, 30))
  d <- data.frame(SERIALNO = seq_len(n), SPORDER = 1, PUMA = "00800", AGEP = age, SEX = sample(1:2, n, TRUE),
    ESR = esr, NATIVITY = ifelse(fb, 2, 1), POBP = ifelse(fb, sample(c(448, 416, 303), n, TRUE), 27),
    SCH = sch, SCHG = ifelse(sch == 2, college_codes[1], 0), RELSHIPP = ifelse(runif(n) < inst, 37, 20),
    SCHL = ifelse(age >= 25, sample(c(14, 16, 19, 21), n, TRUE), sample(c(16, 19), n, TRUE)),
    PWGTP = w)
  # replicate weights: SDR-style perturbations of the base weight
  for (r in 1:80) d[[paste0("PWGTP", r)]] <- round(w * sample(c(0.29, 1, 1.71), n, TRUE, prob = c(.25, .5, .25)))
  data.table::as.data.table(lapply(d, as.character))
}
d0 <- fake(8000, 0.03, 0.72, 0.70)
d1 <- fake(9000, 0.08, 0.72, 0.55)
p0 <- pums_prepare(d0); p1 <- pums_prepare(d1)
ok(all(p0$age %in% AGES), "ages mapped to the 8 groups")
ok(!any(as.numeric(d0$RELSHIPP[as.numeric(d0$AGEP) >= 16]) == 37) || nrow(p0) < sum(as.numeric(d0$AGEP) >= 16), "institutional GQ excluded")
g <- pums_groups(p0)
ok(all(g$rates$lfpr_se > 0 & g$rates$lfpr_se < 10), "group SEs are positive and plausible")
tot <- g$rates[g$rates$group == "total", ]
ok(abs(tot$lfpr_se - 100 * sqrt(tot$lfpr / 100 * (1 - tot$lfpr / 100) / tot$n)) < 2, "total-LFPR SE is the same order as the simple binomial SE")
for (m in c("lf", "emp")) {
  dd <- pums_decompose(p0, p1, m)
  e <- stats::setNames(dd$est, dd$term)
  ok(abs(e[["age_sex"]] + e[["nativity"]] + e[["enrollment"]] + e[["education"]] + e[["within"]] - e[["change"]]) < 1e-9,
     paste(m, ": nested terms add to the actual change"))
}
dd <- pums_decompose(p0, p1, "lf"); e <- stats::setNames(dd$est, dd$term)
ok(e[["nativity"]] < 0, "a rising share of a lower-participation group shows up as negative nativity composition")
# old SCHG coding (2010 file) picked up as college
d2 <- fake(3000, 0.03, 0.7, 0.7, college_codes = c(6, 7)); p2 <- pums_prepare(d2)
ok(any(p2$college), "college detected under the pre-2012 SCHG coding")
ok(all(p0$edu[p0$age %in% c("16-19", "20-24")] == "u25") && all(p0$edu[!p0$age %in% c("16-19", "20-24")] %in% c("lths", "hs", "somecol", "ba")), "education coded for 25+ only")
d3 <- fake(500, 0.03, 0.7, 0.7); d3$SCHL <- as.character(sample(c(8, 9, 11, 13, 16), 500, TRUE)); p3 <- pums_prepare(d3)
ok(all(table(p3$edu[p3$edu != "u25"]) > 0) && setequal(unique(p3$edu[p3$edu != "u25"]), c("lths", "hs", "somecol", "ba")), "old 1-16 SCHL coding maps to all four levels")
cat("PUMS tests passed\n")
