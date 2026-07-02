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
| 2 | About (shiny)cohortBuilder | 0:30 |
| 3 | The problem | 0:35 |
| 4 | Demo — warm-up | 0:45 |
| 5 | Demo — the payoff | 0:55 |
| 6 | Iterate, explain, reproduce | 0:55 |
| 7 | From barrier to FAIR exploration | 0:30 |
| 8 | Real-time GUI — why it matters | 0:25 |
| 9 | Thank you + links | 0:10 |
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

- Record at a stable ~1280×720 (16:9) window; then crop each GIF to the focus
  region called out in its beats (left panel / right chat / center table / modal).
- Keep each GIF ≤ ~12 s; loop cleanly (end frame ≈ a natural resting state).
- Move the cursor deliberately; pause ~0.5 s on each click target so viewers can follow.
- Prefer a clean theme, hidden bookmarks/browser chrome, and a legible font size.
- Any screen recorder works (e.g. built-in OS recorder, LICEcap, ScreenToGif, ffmpeg); the beats below are the source of truth, not the tool.

**Asset naming** (used throughout):

- `gif-1-explore.gif`, `gif-2-warmup.gif`, `gif-3-payoff.gif`, `gif-4-iterate.gif`
- `shot-panel-filters.png` (slide 2 panel screenshot)

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

src <- set_source(tblist(people = starwars$people)) |>
  autofilter()

coh <- cohort(
  src,
  filter("discrete", dataset = "people", name = "Gender",
         variable = "gender", value = "male"),
  filter("range",    dataset = "people", name = "Height",
         variable = "height", range = c(150, 220)),
  filter("discrete", dataset = "people", name = "Species",
         variable = "species", value = "Human")
) |>
  run()
```

*(One line to describe the data, a few filters, and you have a running pipeline.)*

**Right column — screenshot:** `shot-panel-filters.png` — the shinyCohortBuilder
filtering panel from the demo app, showing the **Gender**, **Height** and
**Species** filters enrolled. Capture the left sidebar only, cropped tight.

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
  1. (0.0–1.5 s) Focus the **left filtering panel**; show it empty (no filters yet).
  2. (1.5–3.0 s) Click **"Add Step"** in the panel.
  3. (3.0–4.5 s) Open the **enroll (add filter) dropdown**; camera moves toward the **configuration modal**.
  4. (4.5–8.5 s) The **configuration modal** opens; slowly **scroll through the list of available filters** across the datasets, conveying "lots of columns to sift through."
- **End state:** configuration modal open, filter list mid-scroll (resting frame).

**Speaker note:** this manual browsing is exactly the friction the assistant removes.

---

## Slide 4 — Demo — warm-up

**Layout:** two columns.

**Left column:**

- **Quote:** *"Keep only male characters."*
- **Description:** One natural-language request → one discrete filter applied on
  the **people** table. The filter appears in the panel and the results refresh instantly.

**Right column — `gif-2-warmup.gif`:**

- **Precondition:** app running **unfiltered** (empty panel), assistant chat ready.
- **Beats:**
  1. (0.0–3.0 s) Focus the **right chat panel**; type/submit *"Keep only male characters."*
  2. (3.0–4.5 s) Assistant responds; a tool call fires.
  3. (4.5–8.0 s) Pan to the **left filtering panel** as the **Gender = male** filter renders; the center **people** count refreshes.
- **End state:** Gender filter visible in the panel, people table updated.

**Speaker note:** one sentence, one filter — the GUI reacts as if clicked by hand.

---

## Slide 5 — Demo — the payoff

**Layout:** two columns.

**Left column:**

- **Quote:** *"Find human characters taller than 180 cm from planets with a population over 1 billion."*
- **Description:** Multiple conditions across **people**, **species** and
  **planets** — resolved in a single step, respecting each column's valid range.

**Right column — `gif-3-payoff.gif`:**

- **Precondition:** continuation of slide 4's state (Gender = male already applied
  in step 1). Pre-drive the app to this state before recording.
- **Prompt (type in chat):** *"Find human characters taller than 180 cm from
  planets with a population over 1 billion — **add all these filters in the same
  step (step 1)**."*
- **Beats:**
  1. (0.0–3.5 s) Focus the **right chat panel**; submit the prompt above.
  2. (3.5–5.0 s) Assistant processes; multiple tool calls fire.
  3. (5.0–9.0 s) Pan to the **left filtering panel** as several filters render **in step 1** (species/human, height ≥ 180, planet population > 1e9); center counts refresh.
  4. (9.0–11.0 s) Briefly refocus the **chat** to show the assistant's short explanation.
- **End state:** multiple filters in step 1; chat explanation visible.

**Speaker note:** cross-table conditions in one step, each within valid ranges — no schema lookup needed.

---

## Slide 6 — Iterate, explain, reproduce

**Layout:** two columns.

**Left column:**

- **Quote:** *"Now narrow to mammalian species, and tell me what you filtered."*
- **Description:** Fully reproducible output via the **state** and
  **reproducible-code** features — inspect the state manually and customize
  further yourself.

**Right column — `gif-4-iterate.gif`:**

- **Precondition:** continuation of slide 5's state (multi-condition step 1
  applied). Pre-drive the app to this state before recording.
- **Prompt (type in chat):** *"Now narrow to mammalian species, and tell me what
  you filtered — **keep it in the same step (step 1)**."*
- **Beats:**
  1. (0.0–3.0 s) Focus the **right chat panel**; submit the prompt above.
  2. (3.0–5.0 s) Pan to the **left filtering panel** as the mammalian-species filter renders in step 1.
  3. (5.0–6.5 s) Pan to the **center data table**; briefly **highlight the filtered rows**.
  4. (6.5–8.0 s) Refocus the **chat** showing the assistant's short explanation of what it filtered.
  5. (8.0–10.0 s) Click **"Show Reproducible Code"**; the **reproducible-code modal** appears (basic source + `dplyr::filter()` calls).
  6. (10.0–12.0 s) Close it, click **"Get State"**; the **state modal** appears.
- **End state:** state modal open (resting frame).

**Speaker note:** every LLM edit is inspectable and reproducible — as shareable R code or a saved state.

---

## Slide 7 — From barrier to FAIR exploration

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

## Slide 8 — Real-time GUI — and why it matters

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

## Slide 9 — Thank you

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
- [ ] `gif-2-warmup.gif` — slide 4 ("Keep only male characters." → Gender filter renders)
- [ ] `gif-3-payoff.gif` — slide 5 (multi-condition, same-step prompt → filters render → explanation)
- [ ] `gif-4-iterate.gif` — slide 6 (mammalian narrow → highlight → explanation → Show Reproducible Code → Get State)

**Screenshots:**

- [ ] `shot-panel-filters.png` — slide 2 (filtering panel with Gender/Height/Species)

**Consistency check before finalizing:**

- [ ] Tab names read *people · planets · species · films*.
- [ ] Action labels read exactly *Add Step*, *Show Reproducible Code*, *Get State*.
- [ ] Slide 3 states *4 tables / 35 columns*.
- [ ] Slides 5 & 6 prompts request all filters in the **same step (step 1)**.
- [ ] Slide 9 links resolve.
