# Alone — by the numbers (website)

A static site generated from the `alone` package datasets.

## Rebuild

From the package root:

```r
source("website/build.R")   # or: Rscript website/build.R
```

Needs `glue` and `magick`. The script:

- loads `data/*.rda` (no install needed)
- resizes photos from `dev/images/survivalists/<VER><SS><ID>.(png|jpg)` into `images/survivalists/` (640px JPEG, only when new/changed)
- writes `index.html`, `us.html`, `au.html` and `seasons/<us|au|us-frozen>-<SS>.html`

Adding a season = update the package data, add photos, re-run. Survivalists without a photo get an initials placeholder.

## Files

- `assets/style.css`: all styling (colour tokens at the top)
- `assets/site.js`: hover/focus tooltips for chart marks
- `build.R`: page templates and data summaries
