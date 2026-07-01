# Star Wars AI assistant example

A `cohortBuilder` + `shinyCohortBuilder` app over four related Star Wars tables
(people, planets, species, films) with an LLM assistant mounted in a right
sidebar. The assistant inspects and applies filters through cohortBuilder's
registered AI tools, so the filter panel, the assistant, and the data tables all
operate on the same cohort.

## Files

- `app.R` — the Shiny app.
- `starwars.rds` — the dataset: a named list of four related tibbles.

## Dataset

Sourced from the public Star Wars API (SWAPI, https://swapi.dev/): 82 people,
60 planets, 37 species, 6 films. Each table has an `id`, foreign-key columns
(`homeworld_id`, `species_id`) used for binding keys, and a mix of discrete,
numeric-range and date-range variables that exercise the different cohortBuilder
filter types.

## Requirements

Requires `cohortBuilder` (>=1.0.0) and `shinyCohortBuilder` (>=1.0.0)
installed. On R < 4.4 the app injects the base `%||%` operator into the package
namespaces, since both packages target R >= 4.4.

The assistant uses ellmer's Anthropic client, configured entirely from
environment variables (nothing is hard-coded):

| Variable                   | Purpose                                            |
| -------------------------- | -------------------------------------------------- |
| `ANTHROPIC_BASE_URL`       | Base url of an Anthropic-compatible endpoint/proxy |
| `ANTHROPIC_AUTH_TOKEN`     | Bearer token                                       |
| `ANTHROPIC_CUSTOM_HEADERS` | Optional extra `Key: value` header (e.g. a proxy)  |
| `ANTHROPIC_MODEL`          | Optional model override                            |
| `STARWARS_APP_PORT`        | Optional port (default `3838`)                     |

## Run

```r
shiny::runApp("inst/examples/starwars-assistant")
```

or from this directory:

```sh
Rscript app.R
```

The app opens with a few predefined filters (male characters, height 150–220 cm,
mammalian species). Ask the assistant things like *"Show only human characters
taller than 180 cm"* or *"Clear all filters and show planets with population
over 1 billion"*.
