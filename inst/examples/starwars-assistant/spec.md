# Spec: useR!2026 Lightning Talk — instruction.md

## Problem statement

We need a build guide (`instruction.md`) for a 5-minute lightning talk at useR!2026,
titled **"Enabling Data Exploration Through a Metadata Layer: LLM Integration in
cohortBuilder"** (authors: Krystian Igras, Adam Foryś).

The guide is authored for **manual slide building in Google Slides / PPTX**. For each
of the 9 slides it must specify:
- the exact on-slide content (titles, columns, quotes, bullet copy, code snippets), and
- a **detailed shot list** for every GIF (timed beats, clicks, focus/zoom regions),
  so a person can record and assemble the assets deterministically.

The talk demonstrates the `starwars-assistant` demo app (this folder), where an LLM
chat drives the shinyCohortBuilder GUI in real time via a metadata layer.

`instruction.md` is the sole deliverable of the build phase. This `spec.md` only
describes what that file must contain.

## Grounding facts (verified from the codebase)

- **Demo app:** `shinyCohortBuilder/inst/examples/starwars-assistant/app.R`.
- **Layout:** `bslib::page_sidebar` with three regions:
  - **Left sidebar (width 420):** the shinyCohortBuilder filter panel (`cb_ui("starwars", assistant = FALSE, new_step = "configure")`).
  - **Right sidebar (width 400):** the AI assistant chat (`cb_chat_ui`).
  - **Center:** `navset_card_tab` with four tabs — **people, planets, species, films** — each showing a row count + table.
- **Data (`starwars.rds`):** 4 tables, **35 columns total** — people (11 cols, 82 rows), planets (10 cols, 60 rows), species (8 cols, 37 rows), films (6 cols, 6 rows).
- **Predefined-filter toggle:** env var `STARWARS_PREDEFINED_FILTERS` (truthy `1/true/yes/on`, default `true`). When **off**, the app opens unfiltered — used for slide 3's GIF. When **on**, it opens pre-filtered (gender=male, height 150–220, species classification=mammal).
- **App port:** env var `STARWARS_APP_PORT` (default `3838`); app binds `0.0.0.0`.
- **UI action labels/icons (exact):** "Add Step" (`plus`), "Show Reproducible Code" (`code`), "Get State" (`sliders-h`).
- **Reproducible code modal** shows basic-only code (source + `dplyr::filter()` calls; no method/action definitions, no step markers).
- **Docs URLs (slide 9):**
  - cohortBuilder: https://r-world-devs.github.io/cohortBuilder/
  - shinyCohortBuilder: https://r-world-devs.github.io/shinyCohortBuilder/

## Requirements

### R1 — File
- Create `shinyCohortBuilder/inst/examples/starwars-assistant/instruction.md`.
- Single Markdown file, one clearly delimited section per slide (9 total), in order.

### R2 — Per-slide content block
Each slide section contains:
- Slide number + short title.
- **Layout** note (single / two-column with column widths, matching the user's brief).
- **Content** — the literal text to place on the slide (headings, bullets, quotes, code).
- Proofread/compacted copy where the user requested it (slides 6, 7, 8).

### R3 — GIF shot lists (detailed)
For every GIF referenced (slides 3, 4, 5, 6):
- A named GIF asset (suggested filename).
- **Precondition** — the exact app state to start from (each GIF is **independent / self-contained**, started from a known state; state reached by pre-driving the app or via the predefined-filter env var).
- **Ordered beats** — numbered steps with the concrete action (click "Add Step", type prompt X, wait for render), the **focus/zoom region** (left filter panel / right chat / center table), and approximate timing.
- **End state** — the final frame.

### R4 — Recording setup (once, near top of file)
- A short "Recording setup" section: how to launch the app for each GIF (relevant env vars — `STARWARS_PREDEFINED_FILTERS`, `STARWARS_APP_PORT`), viewport/aspect guidance, and general capture conventions (crop-to-region, loop, duration budget). Recording-tool choice left to the presenter; note it is tool-agnostic.

### R5 — Timing budget
- Include a suggested per-slide time allocation summing to ~5 minutes, reflecting a lightning talk (heavy weight on the three demo slides 4–6).

### R6 — Fidelity to the app
- All prompts, tab names, column references, action labels, and the slide-2 snippet must be consistent with the actual app and packages (use the grounding facts above). Prompts for slides 5 and 6 must instruct the LLM to attach all filters **in the same step (step 1)**.

## Slide-by-slide content outline (to be expanded in instruction.md)

1. **Title** — talk title + authors.
2. **About (shiny)cohortBuilder** — two columns:
   - Left: **minimal generic** code snippet (`set_source(tblist(...))` + `cohort(...)` + 3 configured filters: gender, height, species).
   - Right: screenshot of the shinyCohortBuilder filter panel showing those filters.
3. **The problem** — two columns (left thinner):
   - Left: starwars data (4 tables, **35 columns**); exploring/choosing columns is tiresome; "…instead, I'll just ask."
   - Right (wider): **GIF-1** — app launched **without** initial filters (`STARWARS_PREDEFINED_FILTERS=false`); focus filter panel; click "Add Step" → open the enroll dropdown; move to configuration modal; scroll through the available filters.
4. **Demo — warm-up** — two columns:
   - Left: quote *"Keep only male characters."*; description (one NL request → one discrete filter on people; appears in panel; results refresh).
   - Right: **GIF-2** — focus chat while question asked; then pan to filter panel as the filter renders. Independent GIF started from an unfiltered app.
5. **Demo — the payoff** — two columns:
   - Left: quote *"Find human characters taller than 180 cm from planets with a population over 1 billion."*; description (multiple conditions across people/species/planets in a single step, respecting valid ranges).
   - Right: **GIF-3** — chat focus (prompt notes "same step" so all land in step 1); pan to filter panel as filters render; refocus chat briefly showing the explanation. Independent GIF (precondition = state after slide-4 action).
6. **Iterate, explain, reproduce** — two columns:
   - Left: quote *"Now narrow to mammalian species, and tell me what you filtered."*; **proofread** description (fully reproducible via state / reproducible-code features; manual verification + further customization).
   - Right: **GIF-4** — chat focus (same-step prompt); pan to filter panel as filter renders; pan to highlighted data; refocus chat showing explanation; click **"Show Reproducible Code"** (modal shown); click **"Get State"** (modal shown). Independent GIF (precondition = state after slide-5 action).
7. **From barrier to FAIR exploration** — single content slide, **compacted** copy: The barrier / Our proposal / FAIR by design (Findable+Accessible, Interoperable, Reusable).
8. **Real-time GUI — and why it matters** — single content slide, **compacted** copy: "The chat drives the GUI" + "Why it matters".
9. **Thank you** — closing + doc links (cohortBuilder + shinyCohortBuilder URLs above).

## Acceptance criteria

- [ ] `instruction.md` exists in `shinyCohortBuilder/inst/examples/starwars-assistant/`.
- [ ] Exactly 9 slide sections, in order, each with Layout + Content.
- [ ] Two-column slides specify which column is thinner/wider per the brief (2, 3, 4, 5, 6).
- [ ] Slide 2 includes a runnable **minimal generic** snippet with gender/height/species filters.
- [ ] Slide 3 states 4 tables / 35 columns and includes the "…instead, I'll just ask." line.
- [ ] GIFs on slides 3, 4, 5, 6 each have: named asset, precondition (independent/self-contained start state), ordered timed beats with focus regions, and an end state.
- [ ] Slides 5 and 6 prompts explicitly request all filters in the **same step (step 1)**.
- [ ] Slide 6 GIF includes both "Show Reproducible Code" and "Get State" modal beats.
- [ ] Slides 6, 7, 8 copy is proofread/compacted.
- [ ] A "Recording setup" section documents env vars (`STARWARS_PREDEFINED_FILTERS`, `STARWARS_APP_PORT`) and capture conventions.
- [ ] A ~5-minute timing budget is included.
- [ ] Slide 9 links to both pkgdown docs sites.
- [ ] All app references (tabs, labels, prompts) match the actual app.

## Implementation approach (ordered)

1. Create `instruction.md` with a top header (title, authors, 5-min budget) and a "Recording setup" section (env vars, launch commands, viewport/crop/loop conventions, tool-agnostic note).
2. Write slide 1 (title/authors).
3. Write slide 2 (two-column: minimal generic snippet + panel screenshot note).
4. Write slide 3 (two-column: data/problem copy + GIF-1 detailed shot list, unfiltered launch).
5. Write slides 4–6 (two-column each: quote + description + detailed GIF shot lists 2/3/4; same-step prompts for 5 & 6; repro-code + get-state beats for 6).
6. Write slide 7 (compacted FAIR copy) and slide 8 (compacted real-time GUI copy).
7. Write slide 9 (thank you + doc links).
8. Add the per-slide timing table and a final asset checklist (GIF filenames + screenshots to capture).
9. Proofread the whole file for consistency with the grounding facts (tabs, labels, column counts, URLs).
