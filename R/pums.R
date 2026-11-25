# ACS PUMS module: rates by nativity and school enrollment, and a nested
# shift-share (age/sex -> + nativity -> + enrollment) between 5-year windows,
# with successive-difference replication (SDR) standard errors from the 80
# replicate weights:  SE = sqrt( 4/80 * sum_r (theta_r - theta)^2 ).

pums_url <- function(year) sprintf(
  "https://www2.census.gov/programs-surveys/acs/data/pums/%d/5-Year/csv_p%s.zip",
  as.integer(year), tolower(state_abbr()))
state_abbr <- function() c("27" = "MN")[[CONFIG$state_fips]]   # extend if you change states

PUMS_KEEP <- c("SERIALNO", "SPORDER", "PUMA", "AGEP", "SEX", "ESR", "NATIVITY", "POBP",
               "SCH", "SCHG", "RELP", "RELSHIPP", "YOEP", "SCHL", "RAC1P", "HISP",
               "PWGTP", paste0("PWGTP", 1:80))

# Download a 5-year person file and keep only the window's PUMAs and columns.
fetch_pums_extract <- function(year) {
  win <- PUMS_WINDOWS[[as.character(year)]]
  zip <- tempfile(fileext = ".zip"); on.exit(unlink(zip))
  utils::download.file(pums_url(year), zip, mode = "wb", quiet = TRUE)
  csv <- utils::unzip(zip, list = TRUE)$Name
  csv <- csv[grepl("\\.csv$", csv, ignore.case = TRUE)][1]
  tmp <- tempfile(); utils::unzip(zip, csv, exdir = tmp)
  path <- file.path(tmp, csv)
  hdr <- toupper(names(data.table::fread(path, nrows = 0)))
  # mixed-vintage files label PUMA and POBP with a suffix (PUMA10, POBP12, ...)
  pick <- function(stem) { h <- hdr[grepl(paste0("^", stem, "\\d*$"), hdr)]; if (length(h)) h[length(h)] else NA }
  cols <- unique(stats::na.omit(c(intersect(PUMS_KEEP, hdr), pick("PUMA"), pick("POBP"))))
  d <- data.table::fread(path, select = which(hdr %in% cols), colClasses = "character")
  names(d) <- toupper(names(d))
  names(d)[names(d) == pick("PUMA")] <- "PUMA"
  names(d)[names(d) == pick("POBP")] <- "POBP"
  d <- d[sprintf("%05d", as.integer(d$PUMA)) %in% win$pumas, ]
  unlink(tmp, recursive = TRUE)
  d
}

# Person records -> analysis frame (civilian 16+, optionally noninstitutional).
pums_prepare <- function(d) {
  num <- function(x) suppressWarnings(as.numeric(x))
  age <- num(d$AGEP); esr <- num(d$ESR)
  gq_inst <- if ("RELSHIPP" %in% names(d)) num(d$RELSHIPP) == 37 else if ("RELP" %in% names(d)) num(d$RELP) == 16 else FALSE
  keep <- !is.na(age) & age >= 16 & !is.na(esr) & !esr %in% c(4, 5)
  if (PUMS_EXCLUDE_INSTITUTIONAL) keep <- keep & !(gq_inst %in% TRUE)
  d <- d[keep, ]; age <- age[keep]; esr <- esr[keep]
  schg <- num(d$SCHG); college_codes <- if (max(schg, na.rm = TRUE) <= 7) c(6, 7) else c(15, 16)
  agegrp <- cut(age, c(15, 19, 24, 34, 44, 54, 64, 74, Inf), labels = AGES)
  fb <- num(d$NATIVITY) == 2
  ea <- fb & num(d$POBP) %in% PUMS_EAST_AFRICA
  enrolled <- num(d$SCH) %in% c(2, 3)
  W <- as.matrix(data.frame(lapply(d[, c("PWGTP", paste0("PWGTP", 1:80)), with = FALSE], num)))
  data.frame(
    age = as.character(agegrp), sex = ifelse(num(d$SEX) == 1, "Male", "Female"),
    nat = ifelse(!fb, "native", ifelse(ea, "eafrica", "otherfb")),
    enr = ifelse(age <= 24 & enrolled, "enrolled", "not"),
    college = age <= 24 & schg %in% college_codes,
    lf = esr %in% c(1, 2, 3), emp = esr %in% c(1, 2),
    stringsAsFactors = FALSE
  ) -> p
  attr(p, "W") <- W
  p
}

sdr_se <- function(full, reps) sqrt(4 / 80 * sum((reps - full)^2))

# Weighted totals for every weight column at once: returns 81-vectors.
wsum <- function(W, idx) colSums(W[idx, , drop = FALSE])

rate_est <- function(p, rows, num = c("lf", "emp")) {
  W <- attr(p, "W")
  den <- wsum(W, rows)
  num_v <- wsum(W, rows & p[[num]])
  100 * num_v / den
}
share_est <- function(p, rows, sub) { W <- attr(p, "W"); 100 * wsum(W, rows & sub) / wsum(W, rows) }

# Group table for one window: estimate, SE, unweighted n.
PUMS_GROUPS <- list(
  total = function(p) rep(TRUE, nrow(p)),
  men = function(p) p$sex == "Male", women = function(p) p$sex == "Female",
  native = function(p) p$nat == "native", foreignborn = function(p) p$nat != "native",
  eafrica = function(p) p$nat == "eafrica", otherfb = function(p) p$nat == "otherfb",
  y_enrolled = function(p) p$age %in% c("16-19", "20-24") & p$enr == "enrolled",
  y_not = function(p) p$age %in% c("16-19", "20-24") & p$enr == "not",
  prime = function(p) p$age %in% c("25-34", "35-44", "45-54"),
  prime_native = function(p) p$age %in% c("25-34", "35-44", "45-54") & p$nat == "native",
  prime_eafrica = function(p) p$age %in% c("25-34", "35-44", "45-54") & p$nat == "eafrica"
)

pums_groups <- function(p) {
  out <- lapply(names(PUMS_GROUPS), function(g) {
    rows <- PUMS_GROUPS[[g]](p)
    lf <- rate_est(p, rows, "lf"); ep <- rate_est(p, rows, "emp")
    data.frame(group = g, n = sum(rows), lfpr = lf[1], lfpr_se = sdr_se(lf[1], lf[-1]),
               epop = ep[1], epop_se = sdr_se(ep[1], ep[-1]),
               pop = wsum(attr(p, "W"), rows)[1])
  })
  shares <- list(
    share_fb = list(rep(TRUE, nrow(p)), p$nat != "native"),
    share_eafrica = list(rep(TRUE, nrow(p)), p$nat == "eafrica"),
    share_enrolled_1624 = list(p$age %in% c("16-19", "20-24"), p$enr == "enrolled"),
    share_college_1624 = list(p$age %in% c("16-19", "20-24"), p$college),
    share_1624 = list(rep(TRUE, nrow(p)), p$age %in% c("16-19", "20-24")))
  sh <- lapply(names(shares), function(s) {
    v <- share_est(p, shares[[s]][[1]], shares[[s]][[2]])
    data.frame(group = s, n = sum(shares[[s]][[1]] & shares[[s]][[2]]), share = v[1], share_se = sdr_se(v[1], v[-1]))
  })
  list(rates = do.call(rbind, out), shares = do.call(rbind, sh))
}

# ---- nested shift-share between two windows, with SDR SEs -------------------

cell_totals <- function(p, dims) {
  key <- do.call(paste, c(p[dims], sep = "|"))
  W <- attr(p, "W")
  agg <- function(m) rowsum(m, key, reorder = TRUE)   # rownames are the cell keys
  pop <- agg(W)
  list(keys = rownames(pop), pop = pop, lf = agg(W * p$lf), emp = agg(W * p$emp))
}

# Composition and behavior terms for weight column j, cells defined by dims.
shift_terms <- function(c0, c1, j, measure) {
  keys <- union(c0$keys, c1$keys)
  g <- function(c, what) { v <- stats::setNames(numeric(length(keys)), keys); m <- c[[what]]; v[rownames(m)] <- m[, j]; v }
  P0 <- g(c0, "pop"); P1 <- g(c1, "pop"); N0 <- g(c0, measure); N1 <- g(c1, measure)
  w0 <- P0 / sum(P0); w1 <- P1 / sum(P1)
  r0 <- ifelse(P0 > 0, N0 / P0, NA); r1 <- ifelse(P1 > 0, N1 / P1, NA)
  r0[is.na(r0)] <- r1[is.na(r0)]; r1[is.na(r1)] <- r0[is.na(r1)]   # one-sided cells: no behavior term
  c(comp = 100 * sum(0.5 * (r0 + r1) * (w1 - w0)), beh = 100 * sum(0.5 * (w0 + w1) * (r1 - r0)),
    change = 100 * (sum(N1) / sum(P1) - sum(N0) / sum(P0)))
}

PUMS_SPECS <- list(AS = c("sex", "age"), ASN = c("sex", "age", "nat"), ASNE = c("sex", "age", "nat", "enr"))

pums_decompose <- function(p0, p1, measure = c("lf", "emp")) {
  measure <- match.arg(measure)
  cells <- lapply(PUMS_SPECS, function(dims) list(cell_totals(p0, dims), cell_totals(p1, dims)))
  terms <- sapply(1:81, function(j) {
    t <- lapply(cells, function(cc) shift_terms(cc[[1]], cc[[2]], j, measure))
    c(change = t$AS[["change"]],
      age_sex = t$AS[["comp"]],
      nativity = t$ASN[["comp"]] - t$AS[["comp"]],
      enrollment = t$ASNE[["comp"]] - t$ASN[["comp"]],
      within = t$ASNE[["beh"]])
  })
  data.frame(term = rownames(terms), est = terms[, 1],
             se = apply(terms, 1, function(x) sdr_se(x[1], x[-1])), row.names = NULL)
}

# ---- driver ---------------------------------------------------------------

run_pums <- function(offline = FALSE) {
  dir.create("data/pums", FALSE, TRUE)
  P <- list()
  for (y in names(PUMS_WINDOWS)) {
    f <- sprintf("data/pums/extract_%s.csv.gz", y)
    if (!file.exists(f)) {
      if (offline) { message("PUMS ", y, ": no extract, skipping"); next }
      d <- tryCatch(fetch_pums_extract(y), error = function(e) { message("PUMS ", y, " download failed: ", conditionMessage(e)); NULL })
      if (is.null(d)) next
      data.table::fwrite(d, f)
      message("PUMS ", y, ": ", nrow(d), " person records in area")
    }
    P[[y]] <- pums_prepare(data.table::fread(f, colClasses = "character"))
  }
  if (!length(P)) return(NULL)
  ys <- names(P)
  wins <- lapply(ys, function(y) c(PUMS_WINDOWS[[y]], pums_groups(P[[y]])))
  names(wins) <- ys
  pairs <- if (length(ys) > 1) c(lapply(seq_len(length(ys) - 1), function(i) ys[i:(i + 1)]),
                                 if (length(ys) > 2) list(ys[c(1, length(ys))])) else list()
  dec <- lapply(pairs, function(pr) list(from = pr[1], to = pr[2],
    lfpr = pums_decompose(P[[pr[1]]], P[[pr[2]]], "lf"),
    epop = pums_decompose(P[[pr[1]]], P[[pr[2]]], "emp")))
  list(windows = wins, decomp = dec, east_africa_codes = PUMS_EAST_AFRICA,
       noninstitutional = PUMS_EXCLUDE_INSTITUTIONAL)
}
