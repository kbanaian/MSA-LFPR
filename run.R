# Update the cached data and rebuild docs/data/site.json.
#
#   Rscript run.R            # fetch anything new, then rebuild
#   Rscript run.R --offline  # rebuild from data/raw only
#
# Keys are read from the environment (CENSUS_API_KEY required for ACS pulls,
# BLS_API_KEY optional). Without CENSUS_API_KEY the ACS step is skipped and the
# cache is used as is.

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

offline <- "--offline" %in% commandArgs(trailingOnly = TRUE)
raw <- function(f) file.path("data", "raw", f)
read_cache <- function(f) if (file.exists(raw(f))) utils::read.csv(raw(f), stringsAsFactors = FALSE) else NULL
write_cache <- function(d, f) { dir.create(dirname(raw(f)), FALSE, TRUE); utils::write.csv(d, raw(f), row.names = FALSE) }

cells <- read_cache("acs_cells.csv")
bench <- read_cache("acs_bench.csv")
s2301 <- read_cache("acs_s2301.csv")
laus  <- read_cache("laus.csv")

this_year <- as.integer(format(Sys.Date(), "%Y"))
have_key <- nzchar(Sys.getenv("CENSUS_API_KEY"))

# ---- ACS 1-year cells (MSA) plus state and U.S. benchmark totals ------------
if (!offline && have_key) {
  want <- setdiff(seq(CONFIG$first_year, this_year - 1), CONFIG$skip_years)
  have <- if (is.null(cells)) integer() else unique(cells$year[cells$geo == "msa"])
  for (y in setdiff(want, have)) {
    labels <- tryCatch(census_labels(y, "B23001"), error = function(e) NULL)
    if (is.null(labels)) { message("ACS ", y, ": not released yet"); next }
    new <- lapply(c("msa", "state", "us"), function(g) fetch_b23001(y, g, labels))
    if (is.null(new[[1]])) { message("ACS ", y, ": MSA not available"); next }
    message("ACS ", y, ": fetched")
    cells <- rbind(cells, new[[1]])
    bt <- totals(do.call(rbind, new[-1]))
    bench <- rbind(bench[!(bench$year == y), , drop = FALSE], bt)
  }
  cells <- cells[order(cells$geo, cells$year), ]
  write_cache(cells, "acs_cells.csv"); write_cache(bench[order(bench$geo, bench$year), ], "acs_bench.csv")

  # S2301 rows for the by-group table
  latest <- max(cells$year[cells$geo == "msa"])
  prev <- max(setdiff(unique(cells$year), latest))
  need <- expand.grid(survey = c("acs1", "acs5"), year = unique(c(latest, prev, CONFIG$table_peak)),
                      stringsAsFactors = FALSE)
  # 5-year files come out a few months after the 1-year; also try the next 5-year vintage
  need <- rbind(need, data.frame(survey = "acs5", year = latest + 1))
  for (i in seq_len(nrow(need))) {
    sv <- need$survey[i]; y <- need$year[i]
    if (!is.null(s2301) && any(s2301$survey == sv & s2301$year == y)) next
    d <- tryCatch(fetch_s2301(y, sv), error = function(e) NULL)
    if (!is.null(d)) { message("S2301 ", sv, " ", y, ": fetched"); s2301 <- rbind(s2301, d) }
  }
  write_cache(s2301, "acs_s2301.csv")
} else if (!offline) {
  message("CENSUS_API_KEY not set: using cached ACS data")
}

# ---- LAUS: re-pull recent years to catch revisions -------------------------
if (!offline) {
  start <- if (is.null(laus)) CONFIG$laus_start else this_year - CONFIG$laus_refetch + 1
  new <- tryCatch(fetch_laus(start, this_year), error = function(e) { message("LAUS fetch failed: ", conditionMessage(e)); NULL })
  if (!is.null(new)) {
    laus <- rbind(laus[!laus$month %in% new$month, , drop = FALSE], new)
    laus <- laus[order(laus$month), ]
    write_cache(laus, "laus.csv")
    message("LAUS through ", utils::tail(laus$month[!is.na(laus$lf)], 1))
  }
}

# ---- the 5-year S2301 used in the table is the newest one available ----------
s5 <- sort(unique(s2301$year[s2301$survey == "acs5"]), decreasing = TRUE)
site <- build_site(cells, s2301, laus, bench)
site$meta$acs5_latest <- s5[1]
site$meta$acs5_prev <- if (length(s5) > 1) s5[2] else NULL

# ---- projected demographic drag (State Demographic Center county projections) ----
site$projection <- tryCatch(project_drag(cell_arrays(cells, "msa")), error = function(e) { message("Projection step failed: ", conditionMessage(e)); NULL })

# ---- PUMS module (5-year windows; downloads once, then uses data/pums extracts) ----
site$pums <- tryCatch(run_pums(offline), error = function(e) { message("PUMS step failed: ", conditionMessage(e)); NULL })

dir.create("docs/data", FALSE, TRUE)
jsonlite::write_json(site, "docs/data/site.json", auto_unbox = TRUE, null = "null", na = "null", digits = NA)
message("Wrote docs/data/site.json (ACS through ", site$meta$acs_latest, ", LAUS through ", site$meta$laus_last, ")")
