# Demographic drag ahead: hold recent sex x age participation rates fixed and
# apply the Minnesota State Demographic Center's 2024 county projections by
# age and sex (data/raw/sdc_projections.csv; Benton + Stearns rows of the
# "county age sex" sheet). The projection is used only for the *change* in the
# mix: projected shares are scaled from the last ACS year's observed shares
# by the ratio of projected to base-year shares, so the path starts exactly at
# the observed composition.
#
# To refresh: download the SDC workbook (mn.gov/demography, "Our Projections"),
# keep the county rows for the MSA counties from the "county age sex" sheet,
# and save them in the same layout.

PROJ_FILE <- "data/raw/sdc_projections.csv"
PROJ_COUNTIES <- c("Benton", "Stearns")
PROJ_RATE_YEARS <- 3   # pool this many latest ACS years for the base rates

# SDC 5-year groups -> our 8 groups. 16-19 takes 4/5 of 15-19 (uniform within group).
sdc_to_cells <- function(sdc) {
  yrs <- grep("^\\d{4}$", names(sdc), value = TRUE)
  share <- c("15_19" = 0.8)
  map <- c("15_19" = "16-19", "20_24" = "20-24", "25_29" = "25-34", "30_34" = "25-34",
           "35_39" = "35-44", "40_44" = "35-44", "45_49" = "45-54", "50_54" = "45-54",
           "55_59" = "55-64", "60_64" = "55-64", "65_69" = "65-74", "70_74" = "65-74",
           "75_79" = "75+", "80_84" = "75+", "85+" = "75+")
  d <- sdc[sdc$County %in% PROJ_COUNTIES & sdc$Age %in% names(map), ]
  f <- ifelse(d$Age %in% names(share), share[d$Age], 1)
  d$age <- map[d$Age]; d$sex <- d$Sex
  m <- as.matrix(d[, yrs]) * f
  agg <- stats::aggregate(m, by = list(sex = d$sex, age = d$age), FUN = sum)
  agg$sex <- factor(agg$sex, SEXES); agg$age <- factor(agg$age, AGES)
  agg <- agg[order(agg$sex, agg$age), ]
  out <- as.matrix(agg[, -(1:2)]); colnames(out) <- sub("^X", "", colnames(out))
  rownames(out) <- paste(agg$sex, agg$age)
  out   # 16 x years population matrix, same cell order as cell_arrays()
}

project_drag <- function(A, sdc = utils::read.csv(PROJ_FILE, check.names = FALSE, stringsAsFactors = FALSE)) {
  P <- sdc_to_cells(sdc)
  base <- max(A$years)
  if (!as.character(base) %in% colnames(P)) stop("projections do not cover the base year ", base)
  # base rates: pooled over the latest PROJ_RATE_YEARS ACS years
  ry <- utils::tail(A$years, PROJ_RATE_YEARS)
  pool <- function(M) colSums(M[as.character(ry), , drop = FALSE])
  r_lf <- pool(A$lf) / pool(A$pop); r_ep <- pool(A$emp) / pool(A$pop)
  # projected shares, anchored to the observed base-year composition
  sP <- sweep(P, 2, colSums(P), "/")
  ratio <- sP / sP[, as.character(base)]
  w0 <- A$w[as.character(base), ]
  W <- ratio * w0; W <- sweep(W, 2, colSums(W), "/")
  yrs <- as.integer(colnames(W)); keep <- yrs >= base
  W <- W[, keep, drop = FALSE]; yrs <- yrs[keep]
  lf <- 100 * colSums(W * r_lf); ep <- 100 * colSums(W * r_ep)
  # contributions by age band (both sexes) to the change from base to each horizon
  bands <- list("16-24" = c("16-19", "20-24"), "25-54" = c("25-34", "35-44", "45-54"),
                "55-64" = "55-64", "65+" = c("65-74", "75+"))
  age_of <- sub("^\\S+ ", "", rownames(W))
  # each band's share change times its rate *relative to the base-year average*,
  # so a growing low-participation group counts as a drag (terms sum to the total)
  contrib <- function(r, h) { rbar <- sum(W[, 1] * r); sapply(bands, function(b) {
    i <- age_of %in% b; 100 * sum((W[i, as.character(h)] - W[i, 1]) * (r[i] - rbar)) }) }
  horizons <- intersect(c(base + 5, base + 10, base + 20), yrs)
  ab <- c(lfpr = 100 * sum(A$lf[as.character(base), ]) / sum(A$pop[as.character(base), ]),
          epop = 100 * sum(A$emp[as.character(base), ]) / sum(A$pop[as.character(base), ]))
  # path anchored to the base-year actual rate: actual + (projected - projected at base)
  list(base = base, rate_years = ry, years = yrs,
       lfpr = round(unname(ab[["lfpr"]] + lf - lf[1]), 3), epop = round(unname(ab[["epop"]] + ep - ep[1]), 3),
       actual_base = as.list(round(ab, 3)),
       share65 = round(unname(100 * colSums(W[age_of %in% c("65-74", "75+"), ])), 2),
       contrib = lapply(stats::setNames(horizons, horizons), function(h)
         list(lfpr = as.list(round(contrib(r_lf, h), 3)), epop = as.list(round(contrib(r_ep, h), 3)))))
}
