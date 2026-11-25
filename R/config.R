# Project settings. To rebuild for another metro, change the geography block,
# delete data/raw/*.csv (the cache is specific to the MSA), and run run.R with
# a Census API key set.

CONFIG <- list(
  # --- geography -----------------------------------------------------------
  msa_name   = "St. Cloud, MN MSA",
  msa_short  = "St. Cloud",
  counties   = "Benton + Stearns counties",
  cbsa       = "41060",   # CBSA code
  state_fips = "27",      # state used for the benchmark line and the LAUS series id
  state_name = "Minnesota",

  # --- ACS -----------------------------------------------------------------
  first_year = 2006,      # 2005 ACS excluded group quarters, so start in 2006
  skip_years = 2020,      # no standard 1-year release for 2020
  peaks      = c(2007, 2019),  # business-cycle peaks offered as counterfactual bases
  table_peak = 2019,      # "change from peak" year in the by-group table
  moe_flag   = 4,         # flag (dagger) when the MOE of a rate or change >= this many pp

  # --- LAUS ----------------------------------------------------------------
  laus_start   = 2010,
  laus_refetch = 6        # years re-pulled each run to pick up BLS benchmark revisions
)

# B23001 age labels -> the 8 age groups used in the decomposition
AGE_MAP <- c(
  "16 to 19 years"    = "16-19",
  "20 and 21 years"   = "20-24", "22 to 24 years" = "20-24",
  "25 to 29 years"    = "25-34", "30 to 34 years" = "25-34",
  "35 to 44 years"    = "35-44",
  "45 to 54 years"    = "45-54",
  "55 to 59 years"    = "55-64", "60 and 61 years" = "55-64", "62 to 64 years" = "55-64",
  "65 to 69 years"    = "65-74", "70 to 74 years" = "65-74",
  "75 years and over" = "75+"
)
AGES  <- c("16-19", "20-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")
SEXES <- c("Male", "Female")

# S2301 rows used in the by-group table, matched on the LFPR (C02) label text.
S2301_ROWS <- c(
  total     = "!!Population 16 years and over$",
  white     = "!!White alone$",
  black     = "!!Black or African American alone$",
  asian     = "!!Asian alone$",
  two       = "!!Two or more races$",
  hisp      = "!!Hispanic or Latino origin \\(of any race\\)$",
  whitenh   = "!!White alone, not Hispanic or Latino$",
  men2064   = "!!Population 20 to 64 years!!SEX!!Male$",
  women2064 = "!!Population 20 to 64 years!!SEX!!Female$",
  lths      = "!!Less than high school graduate$",
  hs        = "!!High school graduate \\(includes equivalency\\)$",
  somecol   = "!!Some college or associate's degree$",
  ba        = "!!Bachelor's degree or higher$"
)

# --- PUMS (5-year files) ------------------------------------------------------
# Each window is a 5-year PUMS file, named by its last year. The PUMA vintage
# changes across files, so the area changes too; the label says what it covers.
PUMS_WINDOWS <- list(
  "2010" = list(label = "2006\u201310", pumas = "00800",
                area = "Benton + Stearns (2000 PUMA 00800; identical to the MSA)"),
  "2019" = list(label = "2015\u201319", pumas = "00900",
                area = "Stearns County only (2010 PUMA 00900; Benton was grouped with all of Sherburne)"),
  "2024" = list(label = "2020\u201324", pumas = c("00901", "01001"),
                area = "Benton + Stearns + NW Sherburne (2020 PUMAs 00901, 01001)")
)
PUMS_EXCLUDE_INSTITUTIONAL <- TRUE   # match BLS's civilian noninstitutional universe
# Place-of-birth codes for East Africa that are identical in the 2010 and 2024
# dictionaries: Ethiopia, Eritrea, Kenya, Somalia, Sudan
PUMS_EAST_AFRICA <- c(416, 417, 427, 448, 451)
