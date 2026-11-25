# Assemble docs/data/site.json, the single file the page reads.

build_site <- function(cells, s2301, laus, bench) {
  A <- cell_arrays(cells, "msa")
  bases <- sort(unique(c(min(A$years), CONFIG$peaks)))
  bases <- bases[bases %in% A$years]
  full <- seq(min(A$years), max(A$years))
  pad <- function(x) { names(x) <- A$years; unname(lapply(as.character(full), function(y) if (y %in% names(x)) round(x[[y]], 3) else NULL)) }
  padm <- function(m) { rownames(m) <- A$years; lapply(as.character(full), function(y) if (y %in% rownames(m)) round(unname(m[y, ]), 3) else NULL) }

  meas <- function(m) {
    d <- decompose(A, m, bases)
    list(actual = pad(d$actual), adjusted = pad(d$adjusted), cum = padm(d$cum),
         comp = round(sum(d$composition), 2), beh = round(sum(d$behavior), 2),
         cf = lapply(d$cf, function(x) list(cf = pad(x$cf), contrib = padm(x$contrib))))
  }
  latest <- max(A$years); prev <- max(A$years[A$years < latest])
  cell_years <- unique(c(latest, prev, CONFIG$table_peak))
  cells_out <- lapply(stats::setNames(cell_years, cell_years), function(y) {
    if (!y %in% A$years) return(NULL)
    unname(cbind(A$pop[as.character(y), ], A$lf[as.character(y), ], A$emp[as.character(y), ]))
  })

  s_out <- list()
  for (i in seq_len(nrow(s2301))) {
    r <- s2301[i, ]
    k <- sprintf("%s%d", r$survey, r$year)
    vals <- c(r$pop, r$lfpr, r$epop, r$ur, r$moe_lfpr, r$moe_epop, r$moe_ur)
    s_out[[k]][[r$key]] <- if (is.na(r$lfpr)) NULL else vals
  }

  b <- function(g, m) { x <- bench[bench$geo == g, ]; pad(stats::setNames(x[[m]], x$year)[as.character(A$years)]) }

  mr <- monthly_rates(laus, A)
  r2 <- function(x) ifelse(is.na(x), NA, round(x, 2))

  list(
    meta = list(msa_name = CONFIG$msa_name, msa_short = CONFIG$msa_short, counties = CONFIG$counties,
                state_name = CONFIG$state_name, generated = format(Sys.time(), "%Y-%m-%d"),
                acs_latest = latest, acs_prev = prev, table_peak = CONFIG$table_peak,
                bases = bases, peaks = CONFIG$peaks, moe_flag = CONFIG$moe_flag,
                laus_last = utils::tail(mr$month[!is.na(mr$lfpr)], 1)),
    years = full, ages = AGES,
    lfpr = meas("lfpr"), epop = meas("epop"),
    bench = list(state = list(lfpr = b("state", "lfpr"), epop = b("state", "epop")),
                 us = list(lfpr = b("us", "lfpr"), epop = b("us", "epop"))),
    cells = cells_out, s2301 = s_out,
    monthly = list(months = mr$month, lfpr = r2(mr$lfpr), epop = r2(mr$epop),
                   lfpr12 = r2(mr$lfpr12), epop12 = r2(mr$epop12))
  )
}
