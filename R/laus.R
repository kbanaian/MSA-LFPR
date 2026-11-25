# BLS LAUS monthly labor force and employment for the MSA (not seasonally adjusted).
# BLS_API_KEY is optional: without it the public API allows 25 queries a day
# and 10 years per query; with it, 500 a day and 20 years.

laus_ids <- function() {
  base <- sprintf("LAUMT%s%s000000", CONFIG$state_fips, CONFIG$cbsa)
  c(emp = paste0(base, "05"), lf = paste0(base, "06"))
}

fetch_laus <- function(start, end = as.integer(format(Sys.Date(), "%Y"))) {
  key <- Sys.getenv("BLS_API_KEY")
  span <- if (nzchar(key)) 20 else 10
  ids <- laus_ids()
  starts <- seq(start, end, by = span)
  out <- list()
  for (s in starts) {
    body <- list(seriesid = unname(ids), startyear = as.character(s),
                 endyear = as.character(min(s + span - 1, end)))
    if (nzchar(key)) body$registrationkey <- key
    resp <- httr2::request("https://api.bls.gov/publicAPI/v2/timeseries/data/") |>
      httr2::req_body_json(body) |>
      httr2::req_retry(max_tries = 3) |>
      httr2::req_perform()
    j <- httr2::resp_body_json(resp)
    if (!identical(j$status, "REQUEST_SUCCEEDED")) stop("BLS: ", paste(unlist(j$message), collapse = "; "))
    for (se in j$Results$series) {
      var <- names(ids)[ids == se$seriesID]
      for (d in se$data) {
        if (!grepl("^M(0[1-9]|1[0-2])$", d$period)) next
        out[[length(out) + 1]] <- data.frame(
          month = sprintf("%s-%s", d$year, substr(d$period, 2, 3)), var = var,
          value = suppressWarnings(as.numeric(d$value)))
      }
    }
  }
  d <- do.call(rbind, out)
  w <- stats::reshape(d, idvar = "month", timevar = "var", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  w <- w[order(w$month), c("month", "lf", "emp")]
  rownames(w) <- NULL
  w
}
