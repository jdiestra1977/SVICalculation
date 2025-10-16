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
# Get vars for ALL US (ZCTA/County/Tract ok; block group not supported nationwide in one call)
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