## Validates computeSvi2018() (in functionsForSVI.R) against a genuine CDC
## download -- 2018_county_TEXAS.csv (misnamed; it's actually TRACT-level
## data, per its own LOCATION/FIPS columns) sitting in the thcicpudf11
## project. CDC didn't publish ZCTA-level SVI before the 2020 vintage, so
## this validates at tract geography instead -- same idea as
## validate_2020_methodology.R, just a different geography and a
## different compute function.
##
## Not run automatically by anything else.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidycensus)
})

setwd("~/Documents/GitHub/SVICalculation/")
source("functionsForSVI.R")

api_key <- Sys.getenv("CENSUS_API_KEY")
if (nchar(api_key) == 0) {
  stop("CENSUS_API_KEY is not set. Run Sys.setenv(CENSUS_API_KEY = \"your-key\") ",
       "or tidycensus::census_api_key(\"your-key\", install = TRUE) first.")
}
census_api_key(api_key, overwrite = FALSE, install = FALSE)

official_svi_file <- "/Users/jherreradiestra/Projects/HAIsTexas/thcicpudf11/Data/2018_county_TEXAS.csv"

## ---- Step 1: load the real CDC download ----
## FIPS must be read as character explicitly -- an 11-digit tract FIPS
## (e.g. 48001950100) exceeds the 32-bit integer range read_csv() uses
## when it guesses a column is an integer, so letting it auto-detect
## silently overflows the whole column to NA instead of erroring. Same
## col_types pattern already used for Texas_ZCTA_2022.csv elsewhere in
## this project, for the same reason.
svi_official <- read_csv(official_svi_file,
                          col_types = cols(FIPS = col_character(), .default = col_guess())) %>%
  mutate(FIPS_11 = formatC(FIPS, width = 11, flag = "0")) %>%
  select(FIPS_11,
         RPL_THEMES_cdc = RPL_THEMES,
         RPL_THEME1_cdc = RPL_THEME1, RPL_THEME2_cdc = RPL_THEME2,
         RPL_THEME3_cdc = RPL_THEME3, RPL_THEME4_cdc = RPL_THEME4)
cat("CDC tract rows loaded:", nrow(svi_official), "\n")

## ---- Step 2: pull the raw ACS variables and compute the custom SVI ----
## Tract geography (unlike zcta) supports state-filtered pulls directly
## via get_acs(), so no zip_filter/nationwide-pull workaround is needed
## here -- getSviVariables()'s pull() already keeps state= for any
## geography other than "zcta".
cat("Pulling 2018 ACS 5-year tract data for Texas...\n")
vars2018 <- getSviVariables("tract", "TX", 2018)
cat("Rows pulled:", nrow(vars2018), "\n")

svi_custom <- computeSvi(vars2018, year = 2018)

## GEOID for tract geography is an 11-digit FIPS code (state+county+tract),
## possibly with a longer prefix label depending on tidycensus version --
## normalize the same way as the ZCTA validation, just an 11-digit regex
## instead of 5.
svi_custom <- svi_custom %>%
  mutate(FIPS_11 = sub(".*?([0-9]{11})$", "\\1", GEOID))

## ---- Diagnostics: check each step separately instead of assuming ----
## which one is empty. GEOID format assumptions have already broken this
## kind of join once before (the ZCTA validation) -- checking directly
## here instead of guessing again.
cat("\n========== Diagnostics ==========\n")
cat("svi_custom rows:", nrow(svi_custom), "| non-NA RPL_THEMES:", sum(is.finite(svi_custom$RPL_THEMES)), "\n")
cat("Sample svi_custom GEOID -> FIPS_11:\n")
print(head(svi_custom %>% select(GEOID, FIPS_11, RPL_THEMES), 5))
cat("\nsvi_official rows:", nrow(svi_official), "\n")
cat("Sample svi_official FIPS_11:\n")
print(head(svi_official %>% select(FIPS_11, RPL_THEMES_cdc), 5))
cat("\nFIPS_11 values in BOTH (before dropping -999/NA):",
    length(intersect(svi_custom$FIPS_11, svi_official$FIPS_11)), "\n")

## ---- Step 3: join + drop CDC's -999 "no data" sentinels ----
joined <- svi_custom %>% inner_join(svi_official, by = "FIPS_11")
cat("\nRows after join (before -999/NA filter):", nrow(joined), "\n")

compare <- joined %>%
  filter(RPL_THEMES_cdc >= 0, RPL_THEME1_cdc >= 0, RPL_THEME2_cdc >= 0,
         RPL_THEME3_cdc >= 0, RPL_THEME4_cdc >= 0,
         is.finite(RPL_THEMES), is.finite(RPL_THEME1), is.finite(RPL_THEME2),
         is.finite(RPL_THEME3), is.finite(RPL_THEME4))

cat("Tracts matched between custom calculation and CDC official (after dropping -999/NA):", nrow(compare), "\n")

if (nrow(compare) == 0) {
  stop("compare has 0 rows -- see the Diagnostics section above to see whether the join itself ",
       "matched nothing (check the FIPS_11 samples for a format mismatch) or matched rows that ",
       "were then all dropped by the -999/NA filter (check joined's RPL_THEME columns directly, ",
       "e.g. summary(joined$RPL_THEMES) and summary(joined$RPL_THEMES_cdc)).")
}

## ---- Step 4: how close is the overall score, and each theme? ----
check_one <- function(custom_col, cdc_col, label) {
  fit <- lm(compare[[cdc_col]] ~ compare[[custom_col]])
  tibble::tibble(
    Component     = label,
    N             = nrow(compare),
    Correlation   = cor(compare[[custom_col]], compare[[cdc_col]]),
    R_Squared     = summary(fit)$r.squared,
    Mean_Abs_Diff = mean(abs(compare[[custom_col]] - compare[[cdc_col]]))
  )
}

results <- dplyr::bind_rows(
  check_one("RPL_THEMES", "RPL_THEMES_cdc", "Overall (RPL_THEMES)"),
  check_one("RPL_THEME1", "RPL_THEME1_cdc", "Theme 1: Socioeconomic (POV/UNEMP/PCI/NOHSDP)"),
  check_one("RPL_THEME2", "RPL_THEME2_cdc", "Theme 2: Household Composition & Disability"),
  check_one("RPL_THEME3", "RPL_THEME3_cdc", "Theme 3: Minority Status & Language"),
  check_one("RPL_THEME4", "RPL_THEME4_cdc", "Theme 4: Housing/Transportation")
)

cat("\n========== Custom (computeSvi2018) vs. CDC official 2018 tract: overall + per-theme ==========\n")
print(results, n = Inf, width = Inf)

write_csv(results, "svi2018_validation_results.csv")
write_csv(compare, "svi2018_validation_full_comparison.csv")

## ---- Step 5: scatter plots ----
plot_one <- function(custom_col, cdc_col, label) {
  ggplot(compare, aes(x = .data[[custom_col]], y = .data[[cdc_col]])) +
    geom_point(alpha = 0.15, size = 0.6) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Custom (computeSvi2018)", y = "CDC official (2018 tract)", title = label) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1))
}

p_overall <- plot_one("RPL_THEMES", "RPL_THEMES_cdc", "Overall SVI (2018): custom vs. CDC official")
p_theme1  <- plot_one("RPL_THEME1", "RPL_THEME1_cdc", "Theme 1: Socioeconomic")
p_theme2  <- plot_one("RPL_THEME2", "RPL_THEME2_cdc", "Theme 2: Household Composition & Disability")
p_theme3  <- plot_one("RPL_THEME3", "RPL_THEME3_cdc", "Theme 3: Minority Status & Language")
p_theme4  <- plot_one("RPL_THEME4", "RPL_THEME4_cdc", "Theme 4: Housing/Transportation")

ggsave("svi2018_validation_overall.png", p_overall, width = 6, height = 6, dpi = 150)
ggsave("svi2018_validation_themes.png",
       gridExtra::grid.arrange(p_theme1, p_theme2, p_theme3, p_theme4, ncol = 2),
       width = 10, height = 10, dpi = 150)

print(p_overall)

message("\nValidation written: svi2018_validation_results.csv, svi2018_validation_full_comparison.csv, ",
        "svi2018_validation_overall.png, svi2018_validation_themes.png")
