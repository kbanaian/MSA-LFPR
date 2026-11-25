# ACS pulls from api.census.gov. Data calls need a key (CENSUS_API_KEY);
# the variable-label metadata does not.

census_key <- function() {
  k <- Sys.getenv("CENSUS_API_KEY")
  if (!nzchar(k)) stop("CENSUS_API_KEY is not set (see README).", call. = FALSE)
  k
}

geo_param <- function(geo) {
  switch(geo,
    msa   = paste0("metropolitan statistical area/micropolitan statistical area:", CONFIG$cbsa),
    state = paste0("state:", CONFIG$state_fips),
    us    = "us:1",
    stop("unknown geo: ", geo))
}

# Returns a named character vector (variable -> value) for one geography,
# or NULL if the table/year/geo is not available.
census_group <- function(year, table, geo, survey = "acs1", subject = FALSE) {
  path <- sprintf("https://api.census.gov/data/%d/acs/%s%s", year, survey,
                  if (subject) "/subject" else "")
  req <- httr2::request(path) |>
    httr2::req_url_query(get = sprintf("group(%s)", table), `for` = geo_param(geo),
                         key = census_key()) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200) return(NULL)
  body <- httr2::resp_body_string(resp)
  if (!nzchar(body)) return(NULL)
  m <- jsonlite::fromJSON(body)
  stats::setNames(m[2, ], m[1, ])
}

census_labels <- function(year, table, survey = "acs1", subject = FALSE) {
  url <- sprintf("https://api.census.gov/data/%d/acs/%s%s/groups/%s.json", year, survey,
                 if (subject) "/subject" else "", table)
  v <- jsonlite::fromJSON(url)$variables
  lab <- vapply(v, function(x) x$label, "")
  lab[grepl("E$", names(lab))]
}

# ---- B23001: sex x age x employment status -> 16 civilian cells ------------

# Pure function (tested offline): labels + values -> data.frame of cells.
parse_b23001 <- function(labels, values) {
  lab <- gsub(":", "", labels)
  lab <- sub("^Estimate!!Total!!", "", lab)
  parts <- strsplit(lab, "!!", fixed = TRUE)
  rows <- lapply(seq_along(parts), function(i) {
    p <- parts[[i]]
    if (length(p) < 2 || !p[1] %in% SEXES || !p[2] %in% names(AGE_MAP)) return(NULL)
    rest <- paste(p[-(1:2)], collapse = "!!")
    status <- if (rest == "") "pop" else switch(rest,
      "In labor force" = "lf",
      "In labor force!!In Armed Forces" = "af",
      "In labor force!!Civilian!!Employed" = "emp",
      "In labor force!!Employed" = "emp",
      NA_character_)
    if (is.na(status)) return(NULL)
    data.frame(sex = p[1], age = AGE_MAP[[p[2]]], status = status,
               value = as.numeric(values[[names(labels)[i]]]))
  })
  d <- do.call(rbind, rows)
  w <- stats::xtabs(value ~ sex + age + status, d)
  out <- expand.grid(age = AGES, sex = SEXES, stringsAsFactors = FALSE)[, c("sex", "age")]
  g <- function(s) mapply(function(x, a) w[x, a, s], out$sex, out$age)
  af <- if ("af" %in% dimnames(w)$status) g("af") else 0
  out$pop <- g("pop") - af   # civilian population
  out$lf  <- g("lf") - af    # civilian labor force
  out$emp <- g("emp")        # civilian employed
  out
}

fetch_b23001 <- function(year, geo, labels = NULL) {
  vals <- census_group(year, "B23001", geo)
  if (is.null(vals)) return(NULL)
  if (is.null(labels)) labels <- census_labels(year, "B23001")
  cells <- parse_b23001(labels, as.list(vals))
  cbind(geo = geo, year = year, cells)
}

# ---- S2301: published rates and MOEs for the by-group table ----------------

fetch_s2301 <- function(year, survey = "acs1", geo = "msa") {
  vals <- census_group(year, "S2301", geo, survey = survey, subject = TRUE)
  if (is.null(vals)) return(NULL)
  lab <- census_labels(year, "S2301", survey = survey, subject = TRUE)
  c02 <- lab[grepl("^S2301_C02_\\d{3}E$", names(lab)) &
             grepl("^Estimate!!Labor Force Participation Rate", lab)]
  num <- function(v) { x <- suppressWarnings(as.numeric(v)); x[!is.na(x) & x < -1e8] <- NA; x }
  rows <- lapply(names(S2301_ROWS), function(k) {
    hit <- names(c02)[grepl(S2301_ROWS[[k]], c02)]
    if (length(hit) != 1) { warning("S2301 row not matched uniquely: ", k); return(NULL) }
    line <- substr(hit, 11, 13)
    v <- function(col, em) num(vals[[sprintf("S2301_C0%d_%s%s", col, line, em)]])
    data.frame(survey = survey, year = year, key = k,
               pop = v(1, "E"), lfpr = v(2, "E"), epop = v(3, "E"), ur = v(4, "E"),
               moe_lfpr = v(2, "M"), moe_epop = v(3, "M"), moe_ur = v(4, "M"))
  })
  do.call(rbind, rows)
}
