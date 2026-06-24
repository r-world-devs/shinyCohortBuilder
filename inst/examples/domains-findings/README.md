# Reproducing the domain-propagation findings

This folder contains a self-contained Shiny app (`app.R`) that lets you manually
reproduce the non-obvious behaviours of cohortBuilder + shinyCohortBuilder domain
propagation. Each finding has click-by-click steps below.

## How to launch

From an installed package:

```r
shiny::runApp(
  system.file("examples/domains-findings", package = "shinyCohortBuilder")
)
```

From a source checkout (two sibling packages `cohortBuilder/` and
`shinyCohortBuilder/`):

```r
pkgload::load_all("cohortBuilder")
pkgload::load_all("shinyCohortBuilder")
shiny::runApp("shinyCohortBuilder/inst/examples/domains-findings")
```

## What you see

- **Left** — the cohortBuilder filtering panel (the thing under test).
- **Right, top** — a config box: pick `propagate_domains`, `render_source`,
  `run_button`, `cache`, then click **Apply** to (re)build the panel.
- **Right, bottom** — a live **inspector** that polls the cohort every 0.5s and
  prints, per step: `pending`, the **group domain**, and the **age domain**;
  plus the step-2 **age slider** LIVE widget bounds vs its STATIC HTML
  attributes.

The inspector is the key instrument: it shows the cohort's *internal* state, so
you can watch domains narrow (or not) as you act.

## The data (why narrowing is visible)

12 patients. Group `"A"` occurs **only** with gender `"M"`. So once a step
restricts gender to `"F"`, every `"A"` row is gone — the remaining groups are
`{B, C}` and ages span `35–50`. That is the signal you are looking for.

---

## Finding 1 — Adding a step narrows it immediately from a resolved parent (cache/data)

A newly added step starts from the parent's **remaining** data. If the parent
already restricts gender to `F` (which removes every `A` row), the new step's
`group` domain opens as `{B, C}` and its `age` range as the F-rows' range — not
the full declared `{A, B, C}` / `{18, 80}`.

Why: domain propagation in `data`/`cache` mode recomputes a step's domains from
its parent's snapshot at the **start of `run_step`** (step n from step n-1). So
running the newly added step narrows it from the already-resolved parent.

> Historical note: this used to be a bug. Propagation fired at the *end* of
> `run_step` and targeted the *next* step (`run → propagate_domains_to(n+1)`).
> Adding+running a step runs only the *new* step, never the parent, so the new
> step kept the parent's un-narrowed domain. The fix moved propagation to the
> start of `run_step`, targeting the step being run (`n` from `n-1`). This is
> simpler (one propagation point, no special add-step handling) and also fixes
> partial runs `run_flow(min_step = m)`, which previously skipped re-propagating
> step `m` itself. Regression tests: `test-cohort_methods.R` ("narrows a step
> added after a resolved parent", "defers narrowing when added step is not run")
> and the add-step UI matrix.

Steps:

1. Config: `propagate_domains = data`, `render_source = domain`,
   `run_button = none`, `cache = checked`. Click **Apply**.
2. Leave **Gender = `F`** (the app's default). All `A` rows are gender `M`, so the
   remaining data contains no `A`. Step 1 is run on startup, so it is resolved
   (not pending).
3. Click **+ (add step)** in the panel header to clone step 1.
4. **Observe** the inspector:

   ```
   step 1 | pending=FALSE | group domain={A,B,C} | age domain={18,80}
   step 2 | pending=FALSE | group domain={B,C}   | age domain={35,50}
   ```

   Step 2 opens already narrowed to `{B,C}` / `{35,50}`, matching the parent's
   remaining (gender `F`) data. (Step 1 itself is never narrowed — nothing
   propagates into the first step.)

> Exception — a **pending** parent defers: under a run button, step 1 is not run
> at startup, so adding a step does *not* eagerly narrow. The new step inherits
> the full domain until you run the flow, at which point propagation narrows it.
> `filter` mode also keeps `{A,B,C}` here, because it narrows from a filter's own
> upstream *value*, not from the parent's remaining data (see Finding 4).

---

## Finding 2 — Updating a NON-LAST step re-narrows downstream (cache/data modes)

With `propagate_domains = cache` or `data`, *editing* (re-running) an upstream
(non-last) step recomputes the downstream step's domain from the remaining data.

Continue from Finding 1 (steps 1 and 2 exist). To see the domain move, first
**widen** then **narrow** the parent:

1. In **step 1**, tick `M` so gender = `{F, M}`. Step 2 widens — the inspector
   shows `group domain={A,B,C}` and `age domain={28,61}` (the full-data range).
2. Untick `M` so gender is **only `F`** again. Step 2 re-narrows:

   ```
   step 2 | pending=FALSE | group domain={B,C} | age domain={35,50}
   ```

   and the rendered step-2 **Group** checkboxes again offer only `B` and `C`.

The key point: each change to step 1 re-runs it and re-propagates to step 2,
keeping the downstream domain in sync with the upstream remaining data.

> Contrast: with `propagate_domains = none` step 2 never changes from its added
> state. `filter` mode does not narrow from upstream data either (Finding 4).

---

## Finding 3 — Range slider: LIVE widget bounds change, STATIC HTML attrs do not

`updateSliderInput` re-bounds the live ionRangeSlider widget but does **not**
rewrite the slider's static `data-min` / `data-max` HTML attributes. So if you
assert on the HTML attributes you will miss the narrowing.

Right after Finding 2, look at the bottom of the inspector:

```
LIVE widget bounds : min=35 max=50 (handles 35..50)
STATIC HTML attrs  : data-min=18 data-max=80
>>> LIVE and STATIC differ -> Finding 3 reproduced.
```

The age domain narrowed to `35–50` in the live widget, while the HTML attributes
still read the originally-rendered `18–80`. (In tests, read the live bounds via
`$('…-age-slider').data('ionRangeSlider').options`, not the DOM attributes.)

---

## Finding 4 — `propagate_domains = "filter"` does NOT narrow from upstream data

`filter` mode narrows a filter from **its own** post-filter values, not from an
upstream step's remaining data — so an upstream *gender* restriction does not
shrink a downstream *group* domain.

Steps:

1. Config: `propagate_domains = filter`, `render_source = domain`,
   `run_button = none`. Click **Apply**.
2. Set **Gender** to `{F, M}`, click **+ (add step)**, then set step 1 **Gender**
   to only `F` (as in Finding 2).
3. **Observe**: step 2 group domain stays `{A,B,C}` (no upstream narrowing),
   unlike the `cache`/`data` result.

---

## Finding 5 — Run buttons defer the run and mark steps pending

With a run button, edits/additions mark a step **pending** (`pending=TRUE`); the
domains/stats recompute only when you click run.

Steps:

1. Config: `propagate_domains = data`, `render_source = auto`,
   `run_button = global`. Click **Apply**.
2. **Observe**: `step 1 | pending=TRUE` (it awaits a run).
3. Click the panel's **global run** button. `pending` flips to `FALSE`.
4. Click **+ (add step)**. The new step shows `step 2 | pending=TRUE` until you
   run again.

> `run_button = local` behaves the same but with a run button per step.

---

## Finding 6 — (fixed bug) Factor columns zeroed the downstream post-stats

Originally, propagating a domain in `data` mode for a **factor** column stored the
narrowed domain as a *factor*. The discrete filter then ran
`column %in% c(value, NA)`, and because `c(factor, NA)` coerces the factor to its
integer codes, the match failed and step 2's post-data collapsed to **0 / 0%** —
even though no filter in step 2 excluded anything. (In the screenshot that
triggered this: step 2 showed `0 / 7 (0%)` with every level at `(0 / N)`.)

Fix: `domain_from_data_discrete_impl` now returns a **character** domain
(consistent with `cache` mode), and `cb_filter_data` defensively coerces a factor
value to character. Regression tests:

- `test-source_tblist.R` — "discrete filter handles a factor-valued effective value"
- `test-cohort_methods.R` — "propagate_domains = 'data' keeps a factor column's post-data intact"

To confirm it stays fixed in the app: the `group` column is a factor, so reproduce
Findings 1–2 and check the panel header for step 2 shows the full row count
(`7 / 7`), not `0 / 7`.

## Quick reference: expected inspector lines

| Action / config | Inspector result |
|---|---|
| Add step (any mode) | new step shows declared full domain `{A,B,C}` / `{18,80}` |
| Update non-last step, `propagate=data`/`cache` | downstream `group={B,C}`, `age={35,50}` |
| Update non-last step, `propagate=none`/`filter` | downstream stays `{A,B,C}` |
| Range narrowed | LIVE bounds change, STATIC `data-min`/`data-max` unchanged |
| `run_button=global`/`local` | edited/added step `pending=TRUE` until run |

> Tip: the discrete filters are created with `value = NA` / `value = "F"` (a
> concrete value), **not** `NULL`. Initialising a discrete filter with `NULL`
> changes its update semantics and can collapse the step to zero rows — if you
> adapt this app, keep `NA` for "no selection".
