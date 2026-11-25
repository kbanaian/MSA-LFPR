# Offline checks: Rscript tests/run_tests.R  (from the repo root)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
ok <- function(cond, msg) { if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE); cat("ok -", msg, "\n") }

# ---- 1. B23001 label parser, both label styles (with and without colons) ----
make_b23001 <- function(colons) {
  ages_u65 <- c("16 to 19 years", "20 and 21 years", "22 to 24 years", "25 to 29 years", "30 to 34 years",
                "35 to 44 years", "45 to 54 years", "55 to 59 years", "60 and 61 years", "62 to 64 years")
  ages_65 <- c("65 to 69 years", "70 to 74 years", "75 years and over")
  lab <- "Total"
  for (s in SEXES) {
    lab <- c(lab, paste("Total", s, sep = "!!"))
    for (a in ages_u65) lab <- c(lab, paste("Total", s, a, c("", "!!In labor force", "!!In labor force!!In Armed Forces",
      "!!In labor force!!Civilian", "!!In labor force!!Civilian!!Employed", "!!In labor force!!Civilian!!Unemployed",
      "!!Not in labor force"), sep = "!!"))
    for (a in ages_65) lab <- c(lab, paste("Total", s, a, c("", "!!In labor force", "!!In labor force!!Employed",
      "!!In labor force!!Unemployed", "!!Not in labor force"), sep = "!!"))
  }
  lab <- sub("!!$", "", gsub("!!!!", "!!", lab))
  if (colons) lab <- gsub("(!!|^)([^!]+)(?=!!|$)", "\\1\\2:", lab, perl = TRUE)
  lab <- paste0("Estimate!!", lab)
  stats::setNames(lab, sprintf("B23001_%03dE", seq_along(lab)))
}
for (colons in c(FALSE, TRUE)) {
  labels <- make_b23001(colons)
  ok(length(labels) == 173, sprintf("synthetic B23001 has 173 lines (colons=%s)", colons))
  # values: pop 1000 per detailed age; LF 700; AF 10 (under 65); employed 650
  vals <- stats::setNames(as.list(rep(0, 173)), names(labels))
  for (i in seq_along(labels)) {
    l <- sub(":$", "", gsub(":!!", "!!", labels[i]))
    vals[[i]] <- if (grepl("years( and over)?$", l)) 1000 else if (grepl("In labor force$", l)) 700 else
      if (grepl("Armed Forces$", l)) 10 else if (grepl("Employed$", l)) 650 else 0
  }
  cells <- parse_b23001(labels, vals)
  ok(nrow(cells) == 16, "16 sex x age cells")
  ok(all(cells$pop[cells$age == "55-64"] == 3 * 990), "55-64 combines three detailed ages, net of armed forces")
  ok(all(cells$lf[cells$age == "75+"] == 700), "65+ groups have no armed-forces line")
  ok(all(cells$emp[cells$age == "20-24"] == 1300), "20-24 combines two detailed ages")
}

# ---- 2. Decomposition identities on the cached St. Cloud data ----------------
cells <- utils::read.csv("data/raw/acs_cells.csv", stringsAsFactors = FALSE)
A <- cell_arrays(cells, "msa")
for (m in c("lfpr", "epop")) {
  d <- decompose(A, m, bases = c(min(A$years), 2019))
  n <- length(d$actual)
  ok(abs(sum(d$behavior) + sum(d$composition) - (d$actual[n] - d$actual[1])) < 1e-9,
     paste(m, ": behavior + composition = actual change"))
  ok(abs(d$adjusted[n] - d$actual[n]) < 1e-12, paste(m, ": adjusted anchored to latest actual"))
  cf <- d$cf[["2019"]]
  ok(max(abs(rowSums(cf$contrib) - (d$actual - cf$cf))) < 1e-9, paste(m, ": group contributions sum to actual - counterfactual"))
}
# Reference values from the original (independent) Python implementation, 2010-2024 data
if (min(A$years) == 2010) {
  d <- decompose(A, "lfpr", 2019)
  ok(abs(sum(d$composition) - (-3.63)) < 0.01 && abs(sum(d$behavior) - 1.11) < 0.01, "LFPR matches reference (-3.63 mix, +1.11 behavior)")
}
cat("All tests passed\n")
