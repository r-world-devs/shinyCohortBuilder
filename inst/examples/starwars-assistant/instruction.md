# useR!2026 Lightning Talk — Slide Build Guide

**Title:** Enabling Data Exploration Through a Metadata Layer: LLM Integration in cohortBuilder
**Authors:** Krystian Igras, Adam Foryś
**Duration:** 5 minutes (lightning talk)
**Target format:** Google Slides / PPTX (build slides manually from this guide)
**Demo app:** `shinyCohortBuilder/inst/examples/starwars-assistant/app.R`

This guide describes, slide by slide, the exact on-slide content and a detailed
shot list for each animated GIF. Build the deck manually in Google Slides / PPTX
and drop in the recorded GIFs and screenshots as noted.

---

## Timing budget (~5 min)

| Slide | Topic | Time |
|------:|-------|-----:|
| 1 | Title | 0:10 |
| 2 | About (shiny)cohortBuilder | 0:45 |
| 3 | The problem | 0:35 |
| 4 | Demo — warm-up | 0:45 |
| 5 | Demo — iterate | 0:55 |
| 6 | Reproduce | 0:20 |
| 7 | Share | 0:20 |
| 8 | From barrier to FAIR exploration | 0:30 |
| 9 | Real-time GUI — why it matters | 0:30 |
| 10 | Thank you + links | 0:10 |
| | **Total** | **~5:00** |

Demo slides (4–6) carry the weight; keep static slides tight.

---

## Recording setup (read once)

The demo GIFs are recorded from the `starwars-assistant` app. Each GIF is
**self-contained**: it starts from a known app state so it can be recorded and
looped independently of the others.

**Launch the app**

```bash
cd shinyCohortBuilder/inst/examples/starwars-assistant

# Slide 3 — start UNFILTERED (empty filtering panel):
STARWARS_PREDEFINED_FILTERS=false STARWARS_APP_PORT=3838 Rscript app.R

# Slides 4–6 — start unfiltered, then pre-drive to the required state
# (see each slide's "Precondition") before recording:
STARWARS_PREDEFINED_FILTERS=false STARWARS_APP_PORT=3838 Rscript app.R
```

Env vars: `STARWARS_PREDEFINED_FILTERS` (truthy `1/true/yes/on`, default `true`)
toggles whether the app opens pre-filtered; `STARWARS_APP_PORT` (default `3838`)
sets the port. The app requires an LLM chat backend to be configured for the
assistant to respond.

**App layout** (fixed reference for all "focus region" beats):

- **Left sidebar** — the shinyCohortBuilder **filtering panel**.
- **Center** — tabbed data tables: **people · planets · species · films** (each with a row count).
- **Right sidebar** — the **AI assistant chat**.

**Capture conventions (tool-agnostic):**

- Record at a stable ~720×720 window; then crop each GIF to the focus
  region called out in its beats (left panel / right chat / center table / modal).
- Keep each GIF ≤ ~(expected-slide-time)-10s; loop cleanly (end frame ≈ a natural resting state). Do not take the suggested timebreaks as a necessary assumption.
- Move the cursor deliberately; pause ~1 s on each click target so viewers can follow.
- Prefer a clean theme, hidden bookmarks/browser chrome, and a legible font size.
- Any screen recorder works (e.g. built-in OS recorder, LICEcap, ScreenToGif, ffmpeg); the beats below are the source of truth, not the tool.

**Asset naming** (used throughout):

- `gif-1-explore.gif`, `gif-2-warmup.gif`, `gif-3-iterate.gif`
- `shot-panel-filters.png` (slide 2 panel screenshot)
- `shot-repro-code.png`  (slide 6 panel screenshot)
- `shot-get-state.png`  (slide 7 panel screenshot)

---

## Slide 1 — Title

**Layout:** single, centered.

**Content:**

- **Title:** Enabling Data Exploration Through a Metadata Layer: LLM Integration in cohortBuilder
- **Authors:** Krystian Igras, Adam Foryś
- **Venue line (optional):** useR!2026 · Lightning talk

---

## Slide 2 — About (shiny)cohortBuilder

**Layout:** two columns (roughly equal).

**Left column — minimal code snippet** (compact; syntax-highlighted):

```r
library(cohortBuilder)

src <- set_source(
  tblist(people = starwars$people, species = starwars$species),
  binding_keys = bind_keys(
    bind_key(update = data_key("people", "species_id"), data_key("species", "id"))
  )
) |>
  autofilter()

coh <- cohort(
  src,
  filter("discrete", dataset = "people", name = "Gender",
         variable = "gender", value = "male"),
  filter("range",    dataset = "people", name = "Height",
         variable = "height", range = c(150, 220)),
  filter("discrete", dataset = "species", name = "Classification",
         variable = "classification", value = "mammal")
) |>
  run()
```

*(One line to describe the data, a few filters, and you have a running pipeline.)*

**Right column — screenshot:** `shot-panel-filters.png` — the shinyCohortBuilder
filtering panel from the demo app, showing the **Gender**, **Height** and
**Classification** filters enrolled. Capture the left sidebar only, cropped tight.

**Speaker note:** cohortBuilder builds reproducible filtering pipelines;
shinyCohortBuilder gives them an interactive GUI.

---

## Slide 3 — The problem

**Layout:** two columns — **left thinner**, right wider.

**Left column (thinner) — the friction:**

- The **starwars** dataset: **4 tables** (people · planets · species · films), **35 columns** total.
- With that many columns, it's tiresome to even *explore* them and decide which to filter on.
- *"…instead, I'll just ask."*

**Right column (wider) — `gif-1-explore.gif`:**

- **Precondition:** app launched **without** initial filters
  (`STARWARS_PREDEFINED_FILTERS=false`) — the filtering panel is empty.
- **Beats:**
  1. (0.0–3.0 s) Focus the **left filtering panel**; show it empty (no filters yet).
  2. (3.0–5.0 s) Click **"Add Step"** in the panel.
  3. (5.0–7.0 s) Open the **enroll (add filter) dropdown**; camera moves toward the **configuration modal**.
  4. (7–12 s) The **configuration modal** opens; slowly **scroll through the list of available filters** across the datasets, conveying "lots of columns to sift through."
- **End state:** configuration modal open, filter list mid-scroll (resting frame).

**Speaker note:** this manual browsing is exactly the friction the assistant removes.

---

## Slide 4 — Demo — warm-up

**Layout:** two columns.

**Left column:**

- **Quote:** *"Keep only female characters."*
- **Description:** One natural-language request → one discrete filter applied on
  the **people** table. The filter appears in the panel and the results refresh instantly.

**Right column — `gif-2-warmup.gif`:**

- **Precondition:** app running **unfiltered** (empty panel), assistant chat ready.
- **Beats:**
  1. (0.0–5.0 s) Focus the **right chat panel**; type/submit *"Keep only male characters."*
  2. (5.0–7.0 s) Assistant responds; a tool call fires.
  3. (7.0–10.0 s) Pan to the **left filtering panel** as the **Gender = male** filter renders; the center **people** count refreshes.
- **End state:** Gender filter visible in the panel, people table updated.

**Speaker note:** one sentence, one filter — the GUI reacts as if clicked by hand.

---

## Slide 5 — Demo — iterate

**Layout:** two columns.

**Left column:**

- **Quote:** *"Keep all the mammals that originate from high-temperature planets."*
- **Description:** Multiple conditions across **species** and **planets** — resolved in a single step, respecting each column's valid range.

**Right column — `gif-3-iterate.gif`:**

- **Precondition:** continuation of slide 4's state (Gender = female already applied
  in step 1). Pre-drive the app to this state before recording.
- **Prompt (type in chat):** *"Keep all the mammals that originate from high-temperature planets. — **add all these filters in the same
  step (step 1)**."*
- **Beats:**
  1. (0.0–5.0 s) Focus the **right chat panel**; submit the prompt above.
  2. (5.0–8.0 s) Assistant processes; multiple tool calls fire.
  3. (8.0–12.0 s) Pan to the **left filtering panel** as several filters render **in step 1**; center counts refresh.
  4. (12.0–15.0 s) Briefly refocus the **chat** to show the assistant's short explanation.
- **End state:** multiple filters in step 1; chat explanation visible.

**Speaker note:** cross-table conditions in one step, each within valid ranges — no schema lookup needed.

---

## Slide 6 — Reproduce

**Layout:** two columns.

**Left column:**

- **Title:** *Reproducible code*
- **Description:** See exactly how filtering works through reproducible code. Given the source data, apply it to reproduce the same results and validate every step yourself.


**Right column — screenshot:** `shot-repro-code.png` — the shinyCohortBuilder
modal from the demo app (at slide 5 state) that shows after click at **"Show Reproducible Code"** button.
Screenshot modal view only.

**Speaker note:** every LLM edit is inspectable and reproducible — as shareable R code.

---

## Slide 7 — Share

**Layout:** two columns.

**Left column:**

- **Title:** *Cohort state*
- **Description:** Get State saves your cohort as portable JSON — one file that travels anywhere. Hand it off, restore it, and pick up right where you left off.


**Right column — screenshot:** `shot-get-state.png` — the shinyCohortBuilder
modal from the demo app (at slide 5 state) that shows after click at **"Get State"** button.
Screenshot modal view only.

---

## Slide 8 — From barrier to FAIR exploration

**Layout:** single content slide (three short blocks). Compacted copy:

**The barrier**
Filtering requires knowing the schema — exact variable names and valid ranges.
A high entry threshold even for technical users: the knowledge lives in people's
heads, not with the data.

**Our proposal**
A metadata layer connecting filtering pipelines to LLMs via tool calling.
Describe the data once; the app and the LLM discover filters, read constraints,
and apply valid values — in natural language *or* the GUI.

**FAIR by design**
- **Findable + Accessible** — metadata for humans and machines.
- **Interoperable** — across backends.
- **Reusable** — describe once, reproduce as shareable R code or Shiny bookmarks.

---

## Slide 9 — Real-time GUI — and why it matters

**Layout:** single content slide (two short blocks). Compacted copy:

**The chat drives the GUI**
- A chat panel lets users talk to the LLM; tool calls mutate the shared **Cohort**.
- Post-action hooks fire an internal *"data updated"* signal.
- The GUI reacts in real time: filters appear, values change, results refresh.
- LLM edits and manual clicks are indistinguishable to the app.

**Why it matters**
- A fully metadata-driven solution with reproducible output.
- The filtered data is always represented in the UI.
- Technical users keep full, precise control of the pipeline.
- Describe the data model once — you decide what the LLM can access.

---

## Slide 10 — Thank you

**Layout:** single, centered.

**Content:**

- **Thank you!**
- cohortBuilder docs — https://r-world-devs.github.io/cohortBuilder/
- shinyCohortBuilder docs — https://r-world-devs.github.io/shinyCohortBuilder/
- Authors: Krystian Igras, Adam Foryś

---

## Asset checklist

**GIFs (record per the beats above):**

- [ ] `gif-1-explore.gif` — slide 3 (unfiltered launch → Add Step → config modal scroll)
- [ ] `gif-2-warmup.gif` — slide 4 ("Keep only female characters." → Gender filter renders)
- [ ] `gif-3-iterate.gif` — slide 5 (multi-condition, same-step prompt → filters render → explanation)

**Screenshots:**

- [ ] `shot-panel-filters.png` — slide 2 (filtering panel with Gender/Species/Planets)
- [ ] `shot-repro-code.png`— slide 6 (reproducible code modal)
- [ ] `shot-get-state.png`— slide 7 (get state modal)

**Consistency check before finalizing:**

- [ ] Tab names read *people · planets · species · films*.
- [ ] Action labels read exactly *Add Step*, *Show Reproducible Code*, *Get State*.
- [ ] Slide 3 states *4 tables / 35 columns*.
- [ ] Slides 5 prompts request all filters in the **same step (step 1)**.
- [ ] Slide 10 links resolve.
