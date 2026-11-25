# MSA-LFPR
Recreating Jason Furman's Econvitals' labor force participation analysis but with local data. Defaults to St. Cloud MSA (Benton and Stearns counties.)

The page lives in `docs/` and is served by GitHub Pages. A scheduled GitHub Action pulls new data, reruns the decomposition and commits the updated files. National original: <https://econvitals.org/labor/demographicdetail/>.

## What it shows

1. **Actual vs. age/sex-adjusted LFPR and EPOP.** Each year's change is split over 16 sex × age cells into a *behavior* part (rates within cells) and a *composition* part (population shares), using midpoint (Shapley) weights so the two parts add exactly to the change. The adjusted series accumulates only behavior and is anchored to the latest actual rate.
2. **Aging-only counterfactual.** Each cell's rate is held at a base year (the first ACS year or a business-cycle peak) while the population mix evolves. Bar charts split the change since the base into the aging mix plus each age × sex group's behavior.
3. **Labor market by group.** Levels and changes in UR, LFPR and EPOP by sex, age, race/ethnicity and education, with the sex × age cross-cut. Cells are flagged when the published margin of error is 4 pp or more.
4. **A monthly read** from BLS LAUS (NSA) divided by ACS population 16+, for timeliness between ACS releases.

## How the national page maps onto one MSA

| National page | Here | Cost |
|---|---|---|
| CPS monthly rates | ACS 1-year B23001 (annual), plus LAUS monthly | Monthly frequency; LAUS level differs from ACS |
| CPS microdata cells | B23001 collapsed to 8 ages × 2 sexes | The 2020 step spans two years (no standard 2020 release) |
| Peaks 2000–01, 2007, 2019–20 | 2007 and 2019 (ACS 1-year starts 2005; 2005 excluded group quarters, so the series starts in 2006) | The 2000–01 peak |
| Race/ethnicity from CPS | ACS 5-year S2301 (the 1-year file suppresses St. Cloud's smaller groups) | Timeliness, overlapping windows |
| Education, 25+ | ACS 1-year S2301, 25–64 | Comparability |
| Arbitrary cross-cuts (IPUMS) | Sex × age only | Race/education cross-cuts need ACS PUMS on PUMAs approximating the MSA |

## Layout

```
R/config.R        geography, peaks, age groups, S2301 rows  <- edit this to change MSA
R/acs.R           api.census.gov pulls; B23001 label parser; S2301 row matcher
R/laus.R          BLS API pull (LAUS employment and labor force)
R/decompose.R     shift-share, counterfactual, monthly rates
R/build_site.R    assembles docs/data/site.json
run.R             fetch what's new -> cache in data/raw -> rebuild site.json
tests/run_tests.R offline checks (parser and decomposition identities)
data/raw/*.csv    cached inputs (committed, so the page rebuilds without a key)
docs/index.html   the page; reads docs/data/site.json
.github/workflows/update.yml   scheduled update
```

## Setup

1. **Secrets** (Settings → Secrets and variables → Actions → New repository secret):
   - `CENSUS_API_KEY` (required for ACS pulls). Free from <https://api.census.gov/data/key_signup.html>.
   - `BLS_API_KEY` (optional; raises the BLS limit from 25 to 500 queries/day). Free from <https://data.bls.gov/registrationEngine/>.
2. **Pages** (Settings → Pages): Source "Deploy from a branch", branch `main`, folder `/docs`.
3. **Run it once by hand:** Actions → "Update data and page" → Run workflow. The first keyed run adds the 2006–2009 ACS years (and with them the 2007 peak) to the cache.

## Running locally

Put keys in `~/.Renviron` (never in the repo):

```
CENSUS_API_KEY=your_key
BLS_API_KEY=your_key
```

Then, from the project root in RStudio:

```r
install.packages(c("httr2", "jsonlite"))
source("run.R")                  # or: Rscript run.R --offline  (no downloads)
source("tests/run_tests.R")
```

To preview the page, serve `docs/` over http (opening the file directly blocks the data fetch). For example, `servr::httd("docs")` in R, or `python -m http.server -d docs` in a terminal.

## Changing the metro

Edit the geography block in `R/config.R` (CBSA code, state FIPS, names), delete `data/raw/*.csv`, and run `run.R` with a key. The page picks up names, years and peaks from `site.json`.

## Caveats

- One-year ACS estimates for a metro of about 200,000 people move by 1–2 points a year from sampling alone (MOE on LFPR is about ±1.5 pp), so year-to-year wiggles in the adjusted series are mostly noise. The cumulative 10-plus-year comparison is the robust part.
- The B23001 denominator is the civilian population 16+, including the institutionalized population. BLS rates use the civilian *noninstitutional* population.
- The LAUS monthly denominator is ACS population interpolated between July points and extrapolated past the last ACS year. Census PEP county estimates by age would be a better denominator (planned).
