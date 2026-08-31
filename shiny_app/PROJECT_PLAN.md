# Custom SVI Calculator -- Shiny App Project Plan

Status: **idea / not started**. This is a planning document only -- no app
code exists yet. Written so this can be picked up cold in a future
session without needing to reconstruct the reasoning from scratch.

## The pitch

A public tool that lets anyone compute the CDC/ATSDR Social Vulnerability
Index at whatever geography and whatever custom region they actually need
-- not just the fixed tract/county geographies and single-state or
single-country scope CDC's own official releases are limited to.

The computational engine already exists and is already validated: this
repo's `functionsForSVI.R` (`getSviVariables()` / `computeSvi()` /
`computeSvi2020()` / `computeSvi2018()`) reproduces CDC's real methodology
almost exactly -- R^2 > 0.997 on every theme, > 0.999 overall, checked
against genuine CDC downloads at both ZCTA (2022 vintage) and tract (2018
vintage) geography. See `validate_2020_methodology.R` and
`validate_2018_methodology.R` for the receipts. The app is "just" a UI
wrapper around functions that are already done and already trustworthy --
that's the whole reason this is worth building now rather than a distant
someday.

## What CDC's own tool can't do, that this should

- **CDC only ships tract and county geography** (plus ZCTA starting with
  the 2020+ vintage). No block group, no arbitrary aggregation.
- **CDC's percentile ranks are computed within one fixed universe** -- the
  whole US, or one state, chosen in advance. There's no way to rank a
  custom combined region against itself.
- **No custom multi-jurisdiction pooling at all.** The motivating example:
  someone wants SVI for two adjacent counties (or two countries, or a
  metro area that crosses a state line) treated as ONE ranked pool -- a
  tract in County A should be ranked relative to County A+B combined, not
  relative to County A alone or the whole state/nation. CDC has no
  equivalent of this, and it's a completely reasonable thing to want for
  any regional health/emergency-planning analysis that doesn't respect
  county or state lines.
- **No year flexibility at ZCTA geography.** CDC never published ZCTA SVI
  before 2020 at all -- this project already solved that gap for its own
  needs (`computeSvi2018()`), and the same capability is generally useful
  to anyone else with the same problem (exactly how this project ended up
  needing it in the first place).

## Core use cases

1. **Single region, single geography, single year** -- the simple case:
   "give me tract-level SVI for Texas, 2020." Already fully solved by the
   existing functions; the app just needs a form for it.
2. **Multi-jurisdiction custom pooling** -- "give me SVI for these two
   counties (or states, or countries) combined, ranked against each
   other, not against anything outside that combined region." This is the
   one CDC has no equivalent of, and the main reason to build this at
   all.
3. **Upload-your-own-region** -- user uploads a list of FIPS codes / ZIPs
   / a shapefile defining an arbitrary boundary, gets SVI computed and
   ranked within exactly that boundary.
4. **Time series for a fixed region** -- "give me this county's SVI for
   every year from 2014 to 2022," using the year-dispatching already built
   (`computeSvi(x, year)`), so a user can see how vulnerability moved
   over time in a way CDC's static per-vintage downloads don't make easy.

## Technical foundation already in place (don't rebuild this)

- `getVariablesSVI2020()` / `getSviVariables()` -- pulls the exact ACS
  cells needed, geography-aware (zcta/tract/county/block group), with the
  ZCTA-specific Data-Profile-table and state-filter workarounds already
  solved.
- `computeSvi2020()` -- 2020+ CDC methodology, validated (R^2=0.999 vs.
  real CDC ZCTA data).
- `computeSvi2018()` -- pre-2020 methodology, validated (R^2=0.9997 vs.
  real CDC tract data).
- `computeSvi(x, year)` -- dispatches to the right one automatically.

## The one real technical gap: custom multi-jurisdiction pooling

Right now, `getVariablesSVI2020()` takes a single `state` and an optional
`zip_filter` (used to restrict a nationwide ZCTA pull down to one state's
worth of ZIPs, working around the ZCTA state-filter bug). Multi-state /
multi-country pooling needs more than that:

- Pulling ACS data for an arbitrary SET of states (or the whole country,
  then filtering to an arbitrary FIPS/ZIP list spanning multiple states)
  -- mostly already possible today via `zip_filter` for zcta, but needs
  the same treatment for tract/county geography (currently those always
  pass a single `state=` straight to `get_acs()`).
- Making sure the RANKING step (`pct_rank_na()` inside `computeSvi2020()`/
  `computeSvi2018()`) operates over the FULL custom-pooled data frame, not
  per-state slices -- this should already be correct as long as all the
  pulled rows are combined into one data frame before `computeSvi()` is
  called, but needs an explicit test with a real two-state pool before
  trusting it (nothing today has exercised this path).
- "Countries" specifically -- everything built so far is Census-API-only,
  which is US-only by construction. Multi-country support would need a
  completely different data source per country (this is a much bigger
  lift than anything else on this list, worth treating as a stretch goal
  / separate phase, not part of an MVP).

## Rough architecture sketch (not decided, just a starting point)

- **Inputs:** geography level (tract/ZCTA/county/block group), region
  definition (pick states/counties from a list, OR upload a FIPS/ZIP
  list, OR upload a shapefile), year/vintage, HAI-project-style narrow/
  broad-equivalent options if relevant to other users (probably not --
  that's specific to the HAIsTexas project, not a general SVI need).
- **Server:** calls `getSviVariables()` then `computeSvi()` on the
  resulting combined pull; shows a progress indicator (ACS pulls can be
  slow, especially nationwide ones).
- **Output:** downloadable CSV (full EP_/EPL_/RPL_ table), a results table
  in-app, and ideally a choropleth map (`leaflet` or `ggplot2` + `sf`) so
  people can actually see what they computed, not just download a CSV
  blind.
- **Where it lives:** this `shiny_app/` folder, once actually started --
  `app.R` (or `ui.R`/`server.R`) alongside the existing `functionsForSVI.R`
  it depends on.

## Open questions to resolve before/while building

- Hosting: shinyapps.io, a self-hosted Shiny Server, or something else --
  affects how much the Census API rate limits/costs matter and whether a
  shared API key is workable or each user needs their own.
- How to keep ACS pulls fast enough for a live web app -- nationwide
  pulls are slow; may need caching of common regions/years rather than
  hitting the API fresh every time.
- Whether/how to expose the narrow-vs-CDC-exact distinction to a general
  audience -- this tool's whole value proposition is "this matches CDC
  exactly," so that needs to stay true and be clearly communicated, not
  just true today by accident.

## Suggested phasing (not commitments, just a sane order)

1. MVP: single-state, single-geography, single-year, CSV download only.
   No map, no multi-jurisdiction pooling yet -- just prove the wrapper
   works end to end.
2. Add multi-jurisdiction custom pooling (the actual differentiator).
3. Add the map/visualization layer.
4. Add file upload for custom region definitions.
5. Polish + host it publicly + write it up somewhere shareable ("advertise
   it").
