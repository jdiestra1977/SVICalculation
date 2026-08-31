## Validates computeSvi2020()/getVariablesSVI2020() (in functionsForSVI.R)
## against a genuine CDC download -- Texas_ZCTA_2022.csv, sitting in the
## thcicpudf11 project, confirmed to be an actual CDC file (not a derived
## product). Pulls 2022 ACS ZCTA data for Texas, computes the custom SVI,
## joins it to the real CDC numbers by ZIP, and reports how closely they
## match -- correlation and R^2 for the overall score AND each of the 4
## themes separately (so if something's off, this points at WHICH theme,
## not just "the overall score is a bit different").
##
## Not run automatically by anything else -- run this interactively or
## via Rscript whenever you want to (re-)check the methodology.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(tidycensus)
})

setwd("~/Documents/GitHub/SVICalculation/")
source("functionsForSVI.R")

## Same Census API key setup pattern as fetch_zcta_population.R in the
## thcicpudf11 project -- never hardcode a key in this file.
api_key <- Sys.getenv("CENSUS_API_KEY")
if (nchar(api_key) == 0) {
  stop("CENSUS_API_KEY is not set. Run Sys.setenv(CENSUS_API_KEY = \"your-key\") ",
       "or tidycensus::census_api_key(\"your-key\", install = TRUE) first.")
}
census_api_key(api_key, overwrite = FALSE, install = FALSE)

official_svi_file <- "/Users/jherreradiestra/Projects/HAIsTexas/thcicpudf11/Data/Texas_ZCTA_2022.csv"

## ---- Step 1: load the real CDC download first -- its ZIP list is what ----
## ---- we filter the (nationwide) ACS pull down to ----
svi_official <- read_csv(official_svi_file, show_col_types = FALSE) %>%
  mutate(ZIP = formatC(as.integer(FIPS), width = 5, flag = "0")) %>%
  select(ZIP,
         RPL_THEMES_cdc = RPL_THEMES,
         RPL_THEME1_cdc = RPL_THEME1, RPL_THEME2_cdc = RPL_THEME2,
         RPL_THEME3_cdc = RPL_THEME3, RPL_THEME4_cdc = RPL_THEME4)
texas_zips <- svi_official$ZIP

## ---- Step 2: pull the raw ACS variables and compute the custom SVI ----
## getVariablesSVI2020() pulls ZCTA data nationwide (no state= filter --
## that's unreliable for zcta geography, see the function's comments) and
## filters to texas_zips afterward via zip_filter. This is slower than a
## true state-filtered pull would be, but it's the reliable option.
cat("Pulling 2022 ACS 5-year ZCTA data (nationwide pull, filtered to", length(texas_zips), "Texas ZIPs)...\n")
vars2022 <- getVariablesSVI2020("zcta", "TX", 2022, zip_filter = texas_zips)
cat("Rows pulled:", nrow(vars2022), "\n")

svi_custom <- computeSvi2020(vars2022)

## GEOID comes back from tidycensus in whatever ZCTA label format that
## version uses (a plain 5-digit code in some versions, a longer
## "86000USxxxxx"-style label in others) -- normalize to a bare 5-digit
## ZIP string with a regex rather than assuming a specific prefix, same
## approach fetch_zcta_population.R already uses for this exact problem.
svi_custom <- svi_custom %>%
  mutate(ZIP = sub(".*?([0-9]{5})$", "\\1", GEOID))

## ---- Step 3: join + drop CDC's -999 "no data" sentinels ----
compare <- svi_custom %>%
  inner_join(svi_official, by = "ZIP") %>%
  filter(RPL_THEMES_cdc >= 0, RPL_THEME1_cdc >= 0, RPL_THEME2_cdc >= 0,
         RPL_THEME3_cdc >= 0, RPL_THEME4_cdc >= 0,
         is.finite(RPL_THEMES), is.finite(RPL_THEME1), is.finite(RPL_THEME2),
         is.finite(RPL_THEME3), is.finite(RPL_THEME4))

cat("\nZCTAs matched between custom calculation and CDC official (after dropping -999/NA):", nrow(compare), "\n")

## ---- Step 4: how close is the overall score, and each theme? ----
check_one <- function(custom_col, cdc_col, label) {
  fit <- lm(compare[[cdc_col]] ~ compare[[custom_col]])
  tibble::tibble(
    Component  = label,
    N          = nrow(compare),
    Correlation = cor(compare[[custom_col]], compare[[cdc_col]]),
    R_Squared   = summary(fit)$r.squared,
    Mean_Abs_Diff = mean(abs(compare[[custom_col]] - compare[[cdc_col]]))
  )
}

results <- dplyr::bind_rows(
  check_one("RPL_THEMES", "RPL_THEMES_cdc", "Overall (RPL_THEMES)"),
  check_one("RPL_THEME1", "RPL_THEME1_cdc", "Theme 1: Socioeconomic"),
  check_one("RPL_THEME2", "RPL_THEME2_cdc", "Theme 2: Household Characteristics"),
  check_one("RPL_THEME3", "RPL_THEME3_cdc", "Theme 3: Racial/Ethnic Minority"),
  check_one("RPL_THEME4", "RPL_THEME4_cdc", "Theme 4: Housing/Transportation")
)

cat("\n========== Custom vs. CDC official: overall + per-theme ==========\n")
print(results, n = Inf, width = Inf)
cat("\n(Correlation/R^2 near 1 and Mean_Abs_Diff near 0 = the custom calculation matches CDC closely.\n",
    "If one theme is notably worse than the others, that theme's variable definitions are the place to check first.)\n")

write_csv(results, "svi2020_validation_results.csv")
write_csv(compare, "svi2020_validation_full_comparison.csv")

## ---- Step 5: scatter plots, overall + all 4 themes ----
plot_one <- function(custom_col, cdc_col, label) {
  ggplot(compare, aes(x = .data[[custom_col]], y = .data[[cdc_col]])) +
    geom_point(alpha = 0.25, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
    labs(x = "Custom (computeSvi2020)", y = "CDC official (Texas_ZCTA_2022.csv)", title = label) +
    coord_equal(xlim = c(0, 1), ylim = c(0, 1))
}

p_overall <- plot_one("RPL_THEMES", "RPL_THEMES_cdc", "Overall SVI: custom vs. CDC official")
p_theme1  <- plot_one("RPL_THEME1", "RPL_THEME1_cdc", "Theme 1: Socioeconomic Status")
p_theme2  <- plot_one("RPL_THEME2", "RPL_THEME2_cdc", "Theme 2: Household Characteristics")
p_theme3  <- plot_one("RPL_THEME3", "RPL_THEME3_cdc", "Theme 3: Racial & Ethnic Minority Status")
p_theme4  <- plot_one("RPL_THEME4", "RPL_THEME4_cdc", "Theme 4: Housing Type & Transportation")

ggsave("svi2020_validation_overall.png", p_overall, width = 6, height = 6, dpi = 150)
ggsave("svi2020_validation_themes.png",
       gridExtra::grid.arrange(p_theme1, p_theme2, p_theme3, p_theme4, ncol = 2),
       width = 10, height = 10, dpi = 150)

print(p_overall)

message("\nValidation written: svi2020_validation_results.csv, svi2020_validation_full_comparison.csv, ",
        "svi2020_validation_overall.png, svi2020_validation_themes.png")
