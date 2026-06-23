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

## Finding 1 — Adding a step does NOT narrow on its own

A cloned (added) step shows the **declared full domain**; propagation narrows a
downstream step only when the **parent** step is later updated/re-run, not at the
moment of cloning.

Steps:

1. Config: `propagate_domains = data`, `render_source = domain`,
   `run_button = none`, `cache = checked`. Click **Apply**.
2. In the panel, set the **Gender** filter to **both** `F` and `M` (check both
   boxes). The inspector still shows `step 1 | group domain={A,B,C}`.
3. Click **+ (add step)** in the panel header to clone step 1.
4. **Observe** the inspector:

   ```
   step 1 | pending=FALSE | group domain={A,B,C} | age domain={18,80}
   step 2 | pending=FALSE | group domain={A,B,C} | age domain={18,80}
   ```

   The new step 2 shows the **full** `{A,B,C}` / `{18,80}` — no narrowing
   happened just from adding the step.

---

## Finding 2 — Updating a NON-LAST step narrows downstream (cache/data modes)

With `propagate_domains = cache` or `data`, editing an upstream (non-last) step
re-runs it and narrows the downstream step's domain from the remaining data.

Continue from Finding 1 (you have steps 1 and 2, gender = `{F, M}`):

1. In **step 1**, set **Gender** to **only `F`** (uncheck `M`).
2. **Observe** the inspector — step 2 narrows:

   ```
   step 2 | pending=FALSE | group domain={B,C} | age domain={35,50}
   ```

   and the rendered step-2 **Group** checkboxes now offer only `B` and `C`.

> Contrast: set `propagate_domains = none` (Apply, redo the steps) and step 2
> stays `{A,B,C}` after the same edit.

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
