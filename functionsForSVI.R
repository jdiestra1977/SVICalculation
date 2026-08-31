# ============================================================
# SVI extraction + robust ranking for ACS (ZCTA / County / CBG)
# Works at CBG by: (1) mapping geography to "block group"
# (2) skipping tables not available at BG (strict_bg = TRUE)
# (3) safe divisions + NA-tolerant rank sums
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(tidycensus)
})

# --------------------------
# Helper: safe division
# --------------------------
safe_div <- function(num, den) {
  out <- ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
  out[!is.finite(out)] <- NA_real_
  out
}

# --------------------------
# Variables lists
# --------------------------
vars_full <- c(
  "B01003_001E","C17002_002E","C17002_003E","C17002_001E","B23025_005E","B23025_003E","B19301_001E",
  "B15003_016E","B15003_017E","B15003_018E","B15003_019E","B15003_020E","B15003_021E","B15003_022E",
  "B15003_023E","B15003_024E","B15003_025E","B15003_001E","B99163_005E","B99163_001E","B09021_022E",
  "B09021_001E","B01001_003E","B01001_004E","B01001_005E","B01001_006E","B01001_027E","B01001_028E",
  "B01001_029E","B01001_030E","B18101_026E","B18101_007E","C18130_010E","C18130_017E","B18101_025E",
  "B18101_006E","C18130_009E","C18130_016E","B23008_008E","B23008_021E","B23008_002E","B23008_015E",
  "B25024_007E","B25024_008E","B25024_009E","B25024_001E","B25033_006E","B25033_007E","B25033_012E",
  "B25033_013E","B25033_001E","B25014_005E","B25014_006E","B25014_007E","B25014_011E","B25014_012E",
  "B25014_013E","B25014_001E","B25044_003E","B25044_010E","B25044_001E","B26001_001E","B03002_003E",
  "B03002_001E","B02001_004E","B02001_001E","B02001_005E","B02001_003E","B03003_003E","B03003_001E",
  "B02001_006E","B02001_007E","B02001_008E","B03002_003E","B03002_001E",
  # Insurance (often NOT available at BG)
  "B27010_017E","B27010_033E","B27010_001E"
)

# Safer subset for block group (tables more likely to exist at BG)
vars_bg_safe <- c(
  "B01003_001E",
  "C17002_002E","C17002_003E","C17002_001E",
  "B23025_005E","B23025_003E",
  "B19301_001E",
  "B15003_016E","B15003_017E","B15003_018E","B15003_019E","B15003_020E",
  "B15003_021E","B15003_022E","B15003_023E","B15003_024E","B15003_025E","B15003_001E",
  "B99163_005E","B99163_001E",
  "B09021_022E","B09021_001E",
  "B01001_003E","B01001_004E","B01001_005E","B01001_006E",
  "B01001_027E","B01001_028E","B01001_029E","B01001_030E",
  "B18101_026E","B18101_007E","B18101_025E","B18101_006E",
  "B25024_007E","B25024_008E","B25024_009E","B25024_001E",
  "B25014_005E","B25014_006E","B25014_007E","B25014_011E","B25014_012E","B25014_013E","B25014_001E",
  "B03002_003E","B03002_001E",
  "B02001_004E","B02001_001E","B02001_005E","B02001_003E","B02001_006E","B02001_007E","B02001_008E"
)

# --------------------------
# Get vars for ALL US (ZCTA/County/Tract or; block group not supported nationwide in one call)
# --------------------------
getVariablesAllUS <- function(geo, year, strict_bg = TRUE) {
  geo_tidy <- dplyr::case_when(
    tolower(geo) %in% c("cbg","bg","blockgroup","block group") ~ "block group",
    tolower(geo) %in% c("zcta","zip") ~ "zcta",
    tolower(geo) %in% c("county") ~ "county",
    tolower(geo) %in% c("tract") ~ "tract",
    TRUE ~ geo
  )
  if (geo_tidy == "block group") {
    stop("Block-group nationwide pull is not supported in a single call. Use getVariables(state=...) per-state.")
  }
  vars <- if (geo_tidy == "block group" && strict_bg) vars_bg_safe else vars_full
  get_acs(
    geography = geo_tidy,
    variables = vars,
    year = year,
    survey = "acs5",
    output = "wide",
    geometry = FALSE
  )
}

# --------------------------
# Get vars by STATE (supports block group)
# --------------------------
getVariables <- function(geo, state, year, strict_bg = TRUE) {
  geo_tidy <- dplyr::case_when(
    tolower(geo) %in% c("cbg","bg","blockgroup","block group") ~ "block group",
    tolower(geo) %in% c("zcta","zip") ~ "zcta",
    tolower(geo) %in% c("county") ~ "county",
    tolower(geo) %in% c("tract") ~ "tract",
    TRUE ~ geo
  )
  vars <- if (geo_tidy == "block group" && strict_bg) vars_bg_safe else vars_full
  
  get_acs(
    geography = geo_tidy,
    variables = vars,
    year = year,
    state = state,
    survey = "acs5",
    output = "wide",
    geometry = FALSE
  )
}

# --------------------------
# Ranking + SVI (robust)
# --------------------------
rankingAndSvi <- function(x, min_components = 6) {
  df <- x
  
  # Check for missing estimate columns (diagnostic)
  all_est <- grep("_E$", names(df), value = TRUE)
  all_na_cols <- all_est[vapply(df[all_est], function(v) all(is.na(v)), logical(1))]
  if (length(all_na_cols)) {
    message("All-NA or missing estimate columns (likely unavailable at this geography):\n  ",
            paste(all_na_cols, collapse = ", "))
  }
  
  safe_div <- function(num, den) {
    out <- ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
    out[!is.finite(out)] <- NA_real_
    out
  }
  
  df <- df %>%
    mutate(
      TOTPOP = B01003_001E,
      POV    = safe_div(C17002_002E + C17002_003E, C17002_001E),
      UNEMP  = safe_div(B23025_005E, B23025_003E),
      PCI    = B19301_001E,
      NOHSDP = 1 - safe_div(
        B15003_016E + B15003_017E + B15003_018E + B15003_019E +
          B15003_020E + B15003_021E + B15003_022E + B15003_023E +
          B15003_024E + B15003_025E, B15003_001E
      ),
      LIMENG = safe_div(B99163_005E, B99163_001E),
      AGE65  = safe_div(B09021_022E, B09021_001E),
      AGE17  = safe_div(
        B01001_003E + B01001_004E + B01001_005E + B01001_006E +
          B01001_027E + B01001_028E + B01001_029E + B01001_030E, B01003_001E
      ),
      DISABL = safe_div(B18101_026E + B18101_007E, B18101_025E + B18101_006E),
      MUNIT  = safe_div(B25024_007E + B25024_008E + B25024_009E, B25024_001E),
      CROWD  = safe_div(
        B25014_005E + B25014_006E + B25014_007E +
          B25014_011E + B25014_012E + B25014_013E, B25014_001E
      ),
      MINORITY = 1 - safe_div(B03002_003E, B03002_001E),
      NTVAMRCN = safe_div(B02001_004E, B02001_001E),
      ASIAN    = safe_div(B02001_005E, B02001_001E),
      BLACK    = safe_div(B02001_003E, B02001_001E),
      PACISL   = safe_div(B02001_006E, B02001_001E),
      OTHRRACE = safe_div(B02001_007E, B02001_001E),
      MULTRACE = safe_div(B02001_008E, B02001_001E),
      WHITE    = safe_div(B03002_003E, B03002_001E)
    )
  
  # Only compute insurance if variables exist
  if (all(c("B27010_017E","B27010_033E","B27010_001E") %in% names(df))) {
    df <- df %>% mutate(UNINSU = safe_div(B27010_017E + B27010_033E, B27010_001E))
  } else {
    df$UNINSU <- NA_real_
  }
  
  # Ranking helper
  rfun <- function(v, desc = TRUE) {
    if (all(is.na(v))) return(rep(NA_real_, length(v)))
    rank(if (desc) -v else v, na.last = "keep", ties.method = "max")
  }
  
  df <- df %>%
    mutate(
      RNKPOV     = rfun(POV),
      RNKUNEMP   = rfun(UNEMP),
      RNKPCI     = rfun(PCI, desc = FALSE),
      RNKNOHSDP  = rfun(NOHSDP),
      RNKLIMENG  = rfun(LIMENG),
      RNKAGE65   = rfun(AGE65),
      RNKAGE17   = rfun(AGE17),
      RNKDISABL  = rfun(DISABL),
      RNKMUNIT   = rfun(MUNIT),
      RNKCROWD   = rfun(CROWD),
      RNKUNINSU  = rfun(UNINSU)
    )
  
  rank_cols <- c("RNKPOV","RNKUNEMP","RNKPCI","RNKNOHSDP","RNKLIMENG",
                 "RNKAGE65","RNKAGE17","RNKDISABL","RNKMUNIT","RNKCROWD","RNKUNINSU")
  
  have <- rank_cols[rank_cols %in% names(df)]
  df$N_RANKS <- apply(df[have], 1, function(z) sum(!is.na(z)))
  df$SUMRANK <- rowSums(df[have], na.rm = TRUE)
  df$SUMRANK[is.na(df$TOTPOP) | df$TOTPOP <= 0 | df$N_RANKS < min_components] <- NA_real_
  
  keep <- !is.na(df$SUMRANK)
  df$ADPTVCAPACITY <- NA_real_
  df$ADPTVCAPACITY[keep] <- dplyr::percent_rank(df$SUMRANK[keep])
  df$SVI <- 1 - df$ADPTVCAPACITY
  
  df %>% select(Zip = GEOID, SVI)
}
# ============================================================
# EXAMPLE USAGE
# ============================================================
# 1) Make sure you've set your Census key once per session (or once per machine with install=TRUE)
# census_api_key("YOUR_KEY_HERE", install = FALSE, overwrite = FALSE)

# 2) Texas, block groups (2022). strict_bg=TRUE uses BG-safe variable list.
#    This is the scenario that previously produced all-NA SVI for you.
# state <- "TX"
# socioEcoVars_cbgs <- getVariables("cbg", state, 2022, strict_bg = TRUE)
# sviTexas_cbgs     <- rankingAndSvi(socioEcoVars_cbgs)
# dplyr::glimpse(sviTexas_cbgs)
# head(sviTexas_cbgs)

# 3) ZCTA or County still works with the full variable set:
# socioEcoVars_zcta <- getVariables("zcta", state, 2022)
# rankingAndSvi(socioEcoVars_zcta)

# socioEcoVars_cty  <- getVariables("county", state, 2022)
# rankingAndSvi(socioEcoVars_cty)

# ============================================================
# CDC/ATSDR SVI -- EXACT 2020+ methodology replication
# ============================================================
# The functions above (getVariables / rankingAndSvi) are explicitly an
# "adapted" index (see README) -- a single-pass ranking over a variable
# set that differs from CDC's actual composition in several ways: it's
# missing 5 of the 16 official variables (SNGPNT, MOBILE, NOVEH, GROUPQ,
# and a correctly-sourced LIMENG), sums all variables in one pass instead
# of CDC's rank-within-theme-then-recombine structure, and pulls several
# variables from different ACS tables than CDC actually uses (e.g.
# NOHSDP from B15003 instead of the Subject table S0601; DISABL from
# B18101 instead of Data Profile DP02).
#
# getVariablesSVI2020() and computeSvi2020() below are a from-scratch
# replication of the ACTUAL CDC 2020+ methodology, built directly from
# CDC's own published data dictionary (CDC/ATSDR SVI 2020 Documentation,
# 8/5/2022 -- every ACS table/cell reference below was read off that
# document, not reconstructed from memory). This covers the 2020+ theme
# structure only (2020 and 2022 vintages) -- CDC changed several
# variables and theme names between the 2018 and 2020 vintages (Per
# Capita Income -> Housing Cost Burden, added No Health Insurance, moved
# English Language Proficiency from the Minority theme into the
# Household theme), so this does NOT correctly reproduce pre-2020
# vintages -- that's intentionally out of scope for now.
#
# Four themes, exactly as CDC defines them:
#   Theme 1 (Socioeconomic Status):        POV150, UNEMP, HBURD, NOHSDP, UNINSUR
#   Theme 2 (Household Characteristics):   AGE65, AGE17, DISABL, SNGPNT, LIMENG
#   Theme 3 (Racial & Ethnic Minority Status): MINRTY (one variable only)
#   Theme 4 (Housing Type & Transportation): MUNIT, MOBILE, CROWD, NOVEH, GROUPQ
#
# For each variable: EP_ (percentage) -> EPL_ (percentile rank of EP_,
# ascending -- every 2020+ variable is oriented so higher = more
# vulnerable, unlike the retired PCI variable, so no inversions needed)
# -> SPL_THEMEn (sum of that theme's EPL_'s) -> RPL_THEMEn (percentile
# rank of SPL_THEMEn) -> SPL_THEMES (sum of all 4 SPL_THEMEn, equivalent
# to summing all 16 EPL_'s directly -- CDC's own documentation notes this
# equivalence) -> RPL_THEMES (percentile rank of SPL_THEMES, the final
# overall SVI score).

# --------------------------
# Pull the exact ACS cells CDC uses -- three separate get_acs() calls
# (detailed tables / Data Profile / Subject tables) rather than one
# mixed call, since those are different underlying API endpoints and
# keeping them separate makes it obvious which source is unavailable if
# one fails at a given geography (Subject tables in particular are not
# always available below tract level).
# --------------------------
## geo/year as before. state is now used only to LABEL intent -- for zcta
## geography it is NOT passed to get_acs() at all (see below); for
## county/tract it's still passed through since state-filtering isn't
## documented as broken for those. zip_filter (optional): a character
## vector of 5-digit ZIPs to restrict a nationwide zcta pull down to --
## pass e.g. the ZIP column already sitting in Texas_ZCTA_2022.csv for
## this project's use, or leave NULL to get the whole country back.
getVariablesSVI2020 <- function(geo, state, year, zip_filter = NULL) {
  geo_tidy <- dplyr::case_when(
    tolower(geo) %in% c("cbg", "bg", "blockgroup", "block group") ~ "block group",
    tolower(geo) %in% c("zcta", "zip") ~ "zcta",
    tolower(geo) %in% c("county") ~ "county",
    tolower(geo) %in% c("tract") ~ "tract",
    TRUE ~ geo
  )

  ## Detailed tables ONLY -- no Data Profile (DP-prefix) tables at all.
  ## DP tables error with "unknown/unsupported geography hierarchy" at
  ## ZCTA geography, a known tidycensus/Census API limitation
  ## (https://github.com/walkerke/tidycensus/issues/116). Every variable
  ## that would have come from DP02/DP03/DP04/DP05 is rebuilt here from
  ## the underlying detailed table instead:
  ##   HH:      B11001_001E     (replaces DP02_0001E)
  ##   UNEMP:   B23025_003E/005E (replaces DP03_0009PE)
  ##   HU:      B25001_001E     (replaces DP04_0001E)
  ##   occ. HU: B25002_002E     (replaces DP04_0002E, CROWD denominator)
  ##   MOBILE:  B25024_010E     (replaces DP04_0014PE)
  ##   NOVEH:   B25044_001E/003E/010E (replaces DP04_0058PE)
  ##   MINRTY:  B03002_001E/003E (replaces the DP05 7-cell sum -- same
  ##            "1 - white-alone-non-Hispanic share" formula the existing
  ##            "adapted" rankingAndSvi() above already used for MINORITY)
  detailed_vars <- c(
    "B09001_001E",                                                          # AGE17 numerator
    "B11001_001E",                                                          # HH (SNGPNT denominator)
    "B16005_001E", "B16005_007E", "B16005_008E", "B16005_012E", "B16005_013E",
    "B16005_017E", "B16005_018E", "B16005_022E", "B16005_023E", "B16005_029E",
    "B16005_030E", "B16005_034E", "B16005_035E", "B16005_039E", "B16005_040E",
    "B16005_044E", "B16005_045E",                                            # LIMENG denominator + numerator
    "B26001_001E",                                                          # GROUPQ numerator
    "B23025_003E", "B23025_005E",                                            # UNEMP: civilian labor force, unemployed
    "B25001_001E",                                                          # HU (MUNIT denominator)
    "B25002_002E",                                                          # occupied HU (CROWD denominator)
    "B25024_007E", "B25024_008E", "B25024_009E",                            # MUNIT numerator (10-19, 20-49, 50+ unit structures)
    "B25024_010E",                                                          # MOBILE numerator
    "B25014_001E", "B25014_005E", "B25014_006E", "B25014_007E",
    "B25014_011E", "B25014_012E", "B25014_013E",                            # CROWD: same categories the "adapted" rankingAndSvi() above already used
    "B25044_001E", "B25044_003E", "B25044_010E",                            # NOVEH: total, owner no-vehicle, renter no-vehicle
    "B03002_001E", "B03002_003E",                                           # MINRTY: total, white-alone-non-Hispanic
    "B17001_001E", "B17001_002E",                                           # pre-2020 only: POV at 100% federal poverty level (POV150 above uses S1701 instead)
    "B19301_001E",                                                          # pre-2020 only: per capita income (PCI) -- replaces HBURD in the pre-2020 Theme 1
    "B11003_010E", "B11003_016E"                                            # pre-2020 SNGPNT: stable across all vintages including 2014-2018, confirmed via the Census API's own 2018-vintage group listing -- always pulled, unlike B11012 below
  )
  ## B11012 (2020+ SNGPNT) genuinely does not exist as an API variable
  ## before the 2015-2019 ACS release -- requesting it for an earlier
  ## year fails the API call itself, before computeSvi*() ever runs, no
  ## matter which formula the compute step uses afterward. This has to be
  ## conditional on the year actually being pulled, not just on which
  ## compute function is called downstream.
  if (year >= 2019) {
    detailed_vars <- c(detailed_vars, "B11012_010E", "B11012_015E")
  }
  ## Subject tables (S-prefix) appear to work fine at ZCTA geography
  ## (general tidycensus usage/documentation), unlike Data Profile tables
  ## -- kept as-is except DISABL, moved here from DP02_0072PE to
  ## S1810_C03_001E (percent with a disability). This one cell is lower-
  ## confidence than the rest, sourced from general knowledge of S1810's
  ## layout rather than the same CDC-documentation lookup used for
  ## everything else -- worth an extra look at the validation script's
  ## per-theme output (Theme 2) if that theme underperforms the others.
  subject_vars <- c(
    "S0601_C01_001E",                    # TOTPOP
    "S0601_C01_033E",                    # NOHSDP, direct percentage
    "S1701_C01_001E", "S1701_C01_040E",  # POV150 denominator + numerator
    "S2503_C01_001E", "S2503_C01_028E", "S2503_C01_032E", "S2503_C01_036E", "S2503_C01_040E", # HBURD denom + 4 income-band numerator cells
    "S2701_C05_001E",                    # UNINSUR, direct percentage
    "S0101_C02_030E",                    # AGE65, direct percentage
    "S1810_C03_001E"                     # DISABL, direct percentage
  )

  ## For zcta geography specifically: pull nationwide (no state=) and
  ## filter down afterward, rather than passing state= to get_acs().
  ## State-filtered ZCTA queries have their own separate, independently
  ## documented problem in some tidycensus/API combinations
  ## (https://github.com/walkerke/tidycensus/issues/78) -- pulling
  ## nationwide and filtering after the fact sidesteps that too, same
  ## workaround fetch_zcta_population.R (in the thcicpudf11 project)
  ## already uses for exactly this failure mode. Non-zcta geographies are
  ## unaffected by this issue, so state= is still passed through for them.
  pull <- function(vars) {
    if (geo_tidy == "zcta") {
      get_acs(geography = geo_tidy, variables = vars, year = year,
              survey = "acs5", output = "wide", geometry = FALSE)
    } else {
      get_acs(geography = geo_tidy, variables = vars, year = year,
              state = state, survey = "acs5", output = "wide", geometry = FALSE)
    }
  }

  detailed <- pull(detailed_vars)
  subject  <- pull(subject_vars)

  combined <- detailed %>%
    dplyr::left_join(subject %>% dplyr::select(-NAME), by = "GEOID")

  if (!is.null(zip_filter)) {
    combined <- combined %>%
      dplyr::mutate(.zip_tmp = sub(".*?([0-9]{5})$", "\\1", GEOID)) %>%
      dplyr::filter(.zip_tmp %in% zip_filter) %>%
      dplyr::select(-.zip_tmp)
  }

  combined
}

## Clearer name -- this pull now feeds both computeSvi2020() and
## computeSvi2018(), not just the 2020+ methodology, so the original name
## undersells what it does. Kept getVariablesSVI2020() itself unchanged
## (rather than renaming it outright) so validate_2020_methodology.R's
## existing call to it keeps working -- this is just an alias pointing at
## the same function.
getSviVariables <- getVariablesSVI2020

# --------------------------
# Compute EP_ / EPL_ / SPL_THEMEn / RPL_THEMEn / RPL_THEMES exactly
# following CDC's documented formulas.
# --------------------------
computeSvi2020 <- function(x) {
  df <- x

  safe_div <- function(num, den) {
    out <- ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
    out[!is.finite(out)] <- NA_real_
    out
  }

  df <- df %>%
    mutate(
      TOTPOP = S0601_C01_001E,

      ## ---- Theme 1: Socioeconomic Status ----
      EP_POV150  = safe_div(S1701_C01_040E, S1701_C01_001E) * 100,
      EP_UNEMP   = safe_div(B23025_005E, B23025_003E) * 100,     ## was DP03_0009PE
      E_HBURD    = S2503_C01_028E + S2503_C01_032E + S2503_C01_036E + S2503_C01_040E,
      EP_HBURD   = safe_div(E_HBURD, S2503_C01_001E) * 100,
      EP_NOHSDP  = S0601_C01_033E,
      EP_UNINSUR = S2701_C05_001E,

      ## ---- Theme 2: Household Characteristics ----
      EP_AGE65  = S0101_C02_030E,
      EP_AGE17  = safe_div(B09001_001E, TOTPOP) * 100,
      EP_DISABL = S1810_C03_001E,                                ## was DP02_0072PE -- lower confidence, see comment above getVariablesSVI2020()
      E_SNGPNT  = B11012_010E + B11012_015E,
      EP_SNGPNT = safe_div(E_SNGPNT, B11001_001E) * 100,         ## denominator was DP02_0001E
      E_LIMENG  = B16005_007E + B16005_008E + B16005_012E + B16005_013E +
                  B16005_017E + B16005_018E + B16005_022E + B16005_023E +
                  B16005_029E + B16005_030E + B16005_034E + B16005_035E +
                  B16005_039E + B16005_040E + B16005_044E + B16005_045E,
      EP_LIMENG = safe_div(E_LIMENG, B16005_001E) * 100,

      ## ---- Theme 3: Racial & Ethnic Minority Status ----
      EP_MINRTY = (1 - safe_div(B03002_003E, B03002_001E)) * 100, ## was the DP05 7-cell sum; same "1 - white-alone-non-Hispanic share" formula as the "adapted" rankingAndSvi() above

      ## ---- Theme 4: Housing Type & Transportation ----
      E_MUNIT   = B25024_007E + B25024_008E + B25024_009E,
      EP_MUNIT  = safe_div(E_MUNIT, B25001_001E) * 100,          ## denominator was DP04_0001E
      EP_MOBILE = safe_div(B25024_010E, B25001_001E) * 100,      ## was DP04_0014PE
      E_CROWD   = B25014_005E + B25014_006E + B25014_007E + B25014_011E + B25014_012E + B25014_013E,
      EP_CROWD  = safe_div(E_CROWD, B25002_002E) * 100,          ## numerator categories + denominator both changed from DP04 versions -- see comment above getVariablesSVI2020()
      E_NOVEH   = B25044_003E + B25044_010E,
      EP_NOVEH  = safe_div(E_NOVEH, B25044_001E) * 100,          ## was DP04_0058PE
      EP_GROUPQ = safe_div(B26001_001E, TOTPOP) * 100
    )

  ## Percentile rank matching CDC's Excel PERCENTRANK.INC -- dplyr's
  ## percent_rank() (0 = lowest, 1 = highest) is the standard R
  ## equivalent. Tie-handling differs very slightly (percent_rank uses
  ## "min" ranking for exact ties; PERCENTRANK.INC linearly interpolates
  ## among them) but for continuous ACS percentage estimates, exact ties
  ## are rare enough that this doesn't meaningfully matter in practice.
  pct_rank_na <- function(v) {
    out <- rep(NA_real_, length(v))
    keep <- !is.na(v)
    out[keep] <- dplyr::percent_rank(v[keep])
    out
  }

  df <- df %>%
    mutate(
      EPL_POV150  = pct_rank_na(EP_POV150),
      EPL_UNEMP   = pct_rank_na(EP_UNEMP),
      EPL_HBURD   = pct_rank_na(EP_HBURD),
      EPL_NOHSDP  = pct_rank_na(EP_NOHSDP),
      EPL_UNINSUR = pct_rank_na(EP_UNINSUR),
      EPL_AGE65   = pct_rank_na(EP_AGE65),
      EPL_AGE17   = pct_rank_na(EP_AGE17),
      EPL_DISABL  = pct_rank_na(EP_DISABL),
      EPL_SNGPNT  = pct_rank_na(EP_SNGPNT),
      EPL_LIMENG  = pct_rank_na(EP_LIMENG),
      EPL_MINRTY  = pct_rank_na(EP_MINRTY),
      EPL_MUNIT   = pct_rank_na(EP_MUNIT),
      EPL_MOBILE  = pct_rank_na(EP_MOBILE),
      EPL_CROWD   = pct_rank_na(EP_CROWD),
      EPL_NOVEH   = pct_rank_na(EP_NOVEH),
      EPL_GROUPQ  = pct_rank_na(EP_GROUPQ),

      SPL_THEME1 = EPL_POV150 + EPL_UNEMP + EPL_HBURD + EPL_NOHSDP + EPL_UNINSUR,
      SPL_THEME2 = EPL_AGE65 + EPL_AGE17 + EPL_DISABL + EPL_SNGPNT + EPL_LIMENG,
      SPL_THEME3 = EPL_MINRTY,
      SPL_THEME4 = EPL_MUNIT + EPL_MOBILE + EPL_CROWD + EPL_NOVEH + EPL_GROUPQ,

      RPL_THEME1 = pct_rank_na(SPL_THEME1),
      RPL_THEME2 = pct_rank_na(SPL_THEME2),
      RPL_THEME3 = pct_rank_na(SPL_THEME3),
      RPL_THEME4 = pct_rank_na(SPL_THEME4),

      SPL_THEMES = SPL_THEME1 + SPL_THEME2 + SPL_THEME3 + SPL_THEME4,
      RPL_THEMES = pct_rank_na(SPL_THEMES)
    )

  df %>%
    select(GEOID, NAME, TOTPOP, starts_with("EP_"), starts_with("EPL_"),
           starts_with("RPL_THEME"), SPL_THEMES, RPL_THEMES)
}

# --------------------------
# Pre-2020 CDC methodology (2014/2016/2018 vintages). Same input shape as
# computeSvi2020() -- both consume the same getVariablesSVI2020() pull,
# since almost every underlying ACS table is unchanged between vintages
# (confirmed from the same CDC documentation used to build computeSvi2020:
# most rows are marked "Field name changed since 2018? No"). Only the
# handful of real differences are implemented here:
#   - Theme 1 (still called "Socioeconomic Status" pre-2020) has 4
#     variables, not 5: POV (100% federal poverty level, not 150%), UNEMP,
#     PCI (Per Capita Income -- replaced by Housing Cost Burden in 2020),
#     NOHSDP. No Health Insurance (UNINSUR) did not exist as a ranked
#     variable pre-2020 -- it was an unranked "adjunct" variable, so it's
#     excluded from SPL_THEME1 here entirely, not just recomputed.
#   - Theme 2 ("Household Composition & Disability" pre-2020) has 4
#     variables: AGE65, AGE17, DISABL, SNGPNT. English Language
#     Proficiency lived in Theme 3 before 2020, not here.
#   - Theme 3 ("Minority Status & Language" pre-2020) has 2 variables:
#     MINRTY and LIMENG (moved to Theme 2 starting in 2020).
#   - Theme 4 (Housing Type & Transportation) is unchanged: MUNIT, MOBILE,
#     CROWD, NOVEH, GROUPQ, same as computeSvi2020().
#   - PCI needs an INVERTED percentile rank -- unlike every other
#     variable, higher per capita income means LESS vulnerable, so its
#     rank runs the opposite direction (1 - percent_rank(PCI), matching
#     CDC's own documented formula: "1-(PERCENTRANK.INC on EP_PCI array)").
# 15 total ranked variables (vs. 16 in the 2020+ methodology).
computeSvi2018 <- function(x) {
  df <- x

  safe_div <- function(num, den) {
    out <- ifelse(is.na(num) | is.na(den) | den <= 0, NA_real_, num / den)
    out[!is.finite(out)] <- NA_real_
    out
  }
  pct_rank_na <- function(v, invert = FALSE) {
    out <- rep(NA_real_, length(v))
    keep <- !is.na(v)
    r <- dplyr::percent_rank(v[keep])
    out[keep] <- if (invert) 1 - r else r
    out
  }

  df <- df %>%
    mutate(
      TOTPOP = S0601_C01_001E,

      ## ---- Theme 1: Socioeconomic Status (pre-2020: 4 variables) ----
      EP_POV   = safe_div(B17001_002E, B17001_001E) * 100,
      EP_UNEMP = safe_div(B23025_005E, B23025_003E) * 100,
      EP_PCI   = B19301_001E,
      EP_NOHSDP = S0601_C01_033E,

      ## ---- Theme 2: Household Composition & Disability (4 variables) ----
      EP_AGE65  = S0101_C02_030E,
      EP_AGE17  = safe_div(B09001_001E, TOTPOP) * 100,
      EP_DISABL = S1810_C03_001E,
      E_SNGPNT  = B11003_010E + B11003_016E,   ## B11012 (used in computeSvi2020) doesn't exist before the 2015-2019 ACS release
      EP_SNGPNT = safe_div(E_SNGPNT, B11001_001E) * 100,

      ## ---- Theme 3: Minority Status & Language (2 variables) ----
      EP_MINRTY = (1 - safe_div(B03002_003E, B03002_001E)) * 100,
      E_LIMENG  = B16005_007E + B16005_008E + B16005_012E + B16005_013E +
                  B16005_017E + B16005_018E + B16005_022E + B16005_023E +
                  B16005_029E + B16005_030E + B16005_034E + B16005_035E +
                  B16005_039E + B16005_040E + B16005_044E + B16005_045E,
      EP_LIMENG = safe_div(E_LIMENG, B16005_001E) * 100,

      ## ---- Theme 4: Housing Type & Transportation (5 variables, unchanged) ----
      E_MUNIT   = B25024_007E + B25024_008E + B25024_009E,
      EP_MUNIT  = safe_div(E_MUNIT, B25001_001E) * 100,
      EP_MOBILE = safe_div(B25024_010E, B25001_001E) * 100,
      E_CROWD   = B25014_005E + B25014_006E + B25014_007E + B25014_011E + B25014_012E + B25014_013E,
      EP_CROWD  = safe_div(E_CROWD, B25002_002E) * 100,
      E_NOVEH   = B25044_003E + B25044_010E,
      EP_NOVEH  = safe_div(E_NOVEH, B25044_001E) * 100,
      EP_GROUPQ = safe_div(B26001_001E, TOTPOP) * 100
    )

  df <- df %>%
    mutate(
      EPL_POV    = pct_rank_na(EP_POV),
      EPL_UNEMP  = pct_rank_na(EP_UNEMP),
      EPL_PCI    = pct_rank_na(EP_PCI, invert = TRUE),   ## higher income = LESS vulnerable
      EPL_NOHSDP = pct_rank_na(EP_NOHSDP),
      EPL_AGE65  = pct_rank_na(EP_AGE65),
      EPL_AGE17  = pct_rank_na(EP_AGE17),
      EPL_DISABL = pct_rank_na(EP_DISABL),
      EPL_SNGPNT = pct_rank_na(EP_SNGPNT),
      EPL_MINRTY = pct_rank_na(EP_MINRTY),
      EPL_LIMENG = pct_rank_na(EP_LIMENG),
      EPL_MUNIT  = pct_rank_na(EP_MUNIT),
      EPL_MOBILE = pct_rank_na(EP_MOBILE),
      EPL_CROWD  = pct_rank_na(EP_CROWD),
      EPL_NOVEH  = pct_rank_na(EP_NOVEH),
      EPL_GROUPQ = pct_rank_na(EP_GROUPQ),

      SPL_THEME1 = EPL_POV + EPL_UNEMP + EPL_PCI + EPL_NOHSDP,
      SPL_THEME2 = EPL_AGE65 + EPL_AGE17 + EPL_DISABL + EPL_SNGPNT,
      SPL_THEME3 = EPL_MINRTY + EPL_LIMENG,
      SPL_THEME4 = EPL_MUNIT + EPL_MOBILE + EPL_CROWD + EPL_NOVEH + EPL_GROUPQ,

      RPL_THEME1 = pct_rank_na(SPL_THEME1),
      RPL_THEME2 = pct_rank_na(SPL_THEME2),
      RPL_THEME3 = pct_rank_na(SPL_THEME3),
      RPL_THEME4 = pct_rank_na(SPL_THEME4),

      SPL_THEMES = SPL_THEME1 + SPL_THEME2 + SPL_THEME3 + SPL_THEME4,
      RPL_THEMES = pct_rank_na(SPL_THEMES)
    )

  df %>%
    select(GEOID, NAME, TOTPOP, starts_with("EP_"), starts_with("EPL_"),
           starts_with("RPL_THEME"), SPL_THEMES, RPL_THEMES)
}

# --------------------------
# Dispatcher: the "ifelse" step -- picks the right methodology for a
# given ACS 5-year vintage automatically, so calling code doesn't need to
# know or track which formula applies to which year. year here is the
# same "year" passed to getVariablesSVI2020() (2020+ uses computeSvi2020,
# earlier years use computeSvi2018 -- the CDC methodology boundary is the
# 2018-to-2020 SVI vintage transition, which lines up with ACS 5-year
# vintage 2020 = the first to use the new variables).
computeSvi <- function(x, year) {
  if (year >= 2020) computeSvi2020(x) else computeSvi2018(x)
}

# ============================================================
# EXAMPLE USAGE + VALIDATION AGAINST THE REAL CDC DOWNLOAD
# ============================================================
# state <- "TX"
# vars2020 <- getVariablesSVI2020("zcta", state, 2020)
# svi2020  <- computeSvi2020(vars2020)
# dplyr::glimpse(svi2020)
#
# ---- Real validation: compare against the genuine CDC download ----
# Texas_ZCTA_2022.csv (in the thcicpudf11 project) is a confirmed actual
# CDC download, not a derived product -- this is the real ground truth
# to check against (use year = 2022 above to match it exactly).
#
# svi2020's GEOID comes back from tidycensus in whatever ZCTA label format
# that version/vintage uses (a plain 5-digit code in some versions, a
# longer "86000USxxxxx"-style label in others -- this varies and isn't
# safe to assume). Normalize both sides down to a bare 5-digit ZIP string
# instead of guessing a prefix format -- same regex-based approach
# fetch_zcta_population.R already uses for exactly this problem.
# svi2020 <- svi2020 %>% dplyr::mutate(ZIP = sub(".*?([0-9]{5})$", "\\1", GEOID))
#
# cdc_official <- readr::read_csv(
#   "/Users/jherreradiestra/Projects/HAIsTexas/thcicpudf11/Data/Texas_ZCTA_2022.csv"
# ) %>%
#   dplyr::mutate(ZIP = formatC(as.integer(FIPS), width = 5, flag = "0")) %>%
#   dplyr::select(ZIP, RPL_THEMES_cdc = RPL_THEMES)
#
# compare <- svi2020 %>%
#   dplyr::inner_join(cdc_official, by = "ZIP") %>%
#   dplyr::filter(RPL_THEMES >= 0, RPL_THEMES_cdc >= 0)   # drop -999 sentinels
#
# cor(compare$RPL_THEMES, compare$RPL_THEMES_cdc)
# summary(lm(RPL_THEMES_cdc ~ RPL_THEMES, data = compare))
# compare %>% ggplot2::ggplot(ggplot2::aes(x = RPL_THEMES, y = RPL_THEMES_cdc)) +
#   ggplot2::geom_point(alpha = 0.3) + ggplot2::geom_abline(slope = 1, intercept = 0, color = "red") +
#   ggplot2::labs(x = "Custom (computeSvi2020)", y = "CDC official (Texas_ZCTA_2022.csv)")