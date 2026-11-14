# =============================================================================
# Build the "Alone — by the numbers" static website
#
#   Rscript website/build.R          (run from the package root)
#
# Reads the package datasets in data/*.rda, resizes survivalist photos from
# dev/images/survivalists into website/images/survivalists, and writes:
#   website/index.html               home / navigation
#   website/us.html, website/au.html all-season summaries
#   website/seasons/<slug>.html      one standardised page per season
#
# Re-run whenever new data is added. Dependencies: glue, magick.
# =============================================================================

suppressPackageStartupMessages({
  library(glue)
  library(magick)
})

root <- if (file.exists("DESCRIPTION")) "." else if (file.exists("../DESCRIPTION")) ".." else
  stop("Run this script from the alone package root (or the website folder).")
site <- file.path(root, "website")

for (f in list.files(file.path(root, "data"), pattern = "[.]rda$", full.names = TRUE)) load(f)

# -----------------------------------------------------------------------------
# helpers
# -----------------------------------------------------------------------------

h <- function(..., .envir = parent.frame()) as.character(glue(..., .envir = .envir))
cat0 <- function(x) paste(x, collapse = "\n")

esc <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

ordinal <- function(n) {
  suf <- ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  paste0(n, suf)
}

fmt <- function(x, d = 1, na = "&mdash;") ifelse(is.na(x), na, formatC(x, format = "f", digits = d, big.mark = ","))

fmt_date <- function(d) {
  d <- as.Date(d)
  ifelse(is.na(d), "&mdash;",
         paste(as.integer(format(d, "%d")), month.abb[as.integer(format(d, "%m"))], format(d, "%Y")))
}

days_lbl <- function(n) ifelse(n == 1, "1 day", paste(n, "days"))

initials <- function(name) {
  p <- strsplit(name, " ")[[1]]
  toupper(paste0(substr(p[1], 1, 1), if (length(p) > 1) substr(p[length(p)], 1, 1) else ""))
}

write_page <- function(html, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(html, con, useBytes = FALSE)
}

mean_na <- function(x) if (all(is.na(x))) NA else mean(x, na.rm = TRUE)

# -----------------------------------------------------------------------------
# data prep
# -----------------------------------------------------------------------------

sv <- as.data.frame(survivalists); sv$version <- as.character(sv$version)
ep <- as.data.frame(episodes);     ep$version <- as.character(ep$version)
lo <- as.data.frame(loadouts);     lo$version <- as.character(lo$version)
se <- as.data.frame(seasons);      se$version <- as.character(se$version)

ver_slug  <- c("US" = "us", "AU" = "au", "US Frozen" = "us-frozen")
ver_show  <- c("US" = "Alone", "AU" = "Alone Australia", "US Frozen" = "Alone: Frozen")
home_ctry <- c("US" = "United States", "AU" = "Australia", "US Frozen" = "United States")

se$group <- ifelse(se$version == "AU", "AU", "US")
se$slug  <- sprintf("%s-%02d", ver_slug[se$version], se$season)
se$file  <- paste0("seasons/", se$slug, ".html")
se$short <- ifelse(se$version == "US Frozen", "Frozen", paste0("S", se$season))
se$title <- ifelse(se$version == "US Frozen", "Alone: Frozen", paste("Season", se$season))
se$show  <- unname(ver_show[se$version])
se$loc_short <- ifelse(is.na(se$location), se$region, se$location)
se$place <- vapply(seq_len(nrow(se)), function(i) {
  p <- unique(na.omit(c(se$location[i], se$region[i], se$country[i])))
  paste(p, collapse = ", ")
}, "")
se <- se[order(se$group, se$version == "US Frozen", se$season), ]
rownames(se) <- NULL

# outcome categories (Loss of inventory / Disqualified are rare -> "Other")
sv$reason_category[sv$reason_category %in% "Disqaulified"] <- "Disqualified"
sv$outcome <- ifelse(sv$result == 1, "Winner",
                     ifelse(sv$reason_category %in% c("Health", "Personal"), sv$reason_category, "Other"))
sv$outcome_lbl <- ifelse(sv$outcome == "Other" & !is.na(sv$reason_category), sv$reason_category, sv$outcome)
sv$ocls <- paste0("o-", tolower(sv$outcome))
sv$skey <- paste(sv$version, sv$season)
se$skey <- paste(se$version, se$season)
sv$sfile  <- se$file[match(sv$skey, se$skey)]
sv$sshort <- se$short[match(sv$skey, se$skey)]

sv$hometown <- vapply(seq_len(nrow(sv)), function(i) {
  p <- c(sv$city[i], sv$state[i], if (!identical(sv$country[i], home_ctry[[sv$version[i]]])) sv$country[i])
  p <- unique(p[!is.na(p) & p != ""])
  if (length(p)) paste(p, collapse = ", ") else NA_character_
}, "")

# -----------------------------------------------------------------------------
# images: dev/images/survivalists/<VER><SS><ID>.(png|jpg) -> website jpg 640px
# -----------------------------------------------------------------------------

src_imgs <- list.files(file.path(root, "dev/images/survivalists"), full.names = TRUE)
src_key  <- sub("[.][A-Za-z]+$", "", basename(src_imgs))
img_dir  <- file.path(site, "images/survivalists")
dir.create(img_dir, recursive = TRUE, showWarnings = FALSE)

sv$img <- NA_character_
for (i in seq_len(nrow(sv))) {
  key <- sprintf("%s%02d%s", sv$version[i], sv$season[i], sv$id[i])
  k <- match(key, src_key)
  if (is.na(k)) next
  out <- file.path(img_dir, paste0(key, ".jpg"))
  src_time <- suppressWarnings(file.mtime(src_imgs[k]))
  if (!file.exists(out) || isTRUE(src_time > file.mtime(out))) {
    tryCatch(
      image_read(src_imgs[k]) |>
        image_background("#1c201e") |>
        image_flatten() |>
        image_scale("640x") |>
        image_write(out, format = "jpeg", quality = 80),
      error = function(err) warning("Could not read ", src_imgs[k], ": ", conditionMessage(err), call. = FALSE)
    )
  }
  if (file.exists(out)) sv$img[i] <- paste0("images/survivalists/", key, ".jpg")
}
message(sum(!is.na(sv$img)), " of ", nrow(sv), " survivalists have photos")

# -----------------------------------------------------------------------------
# shared components
# -----------------------------------------------------------------------------

avatar <- function(r, P, cls = "av") {
  if (!is.na(r$img)) h('<img class="{cls}" src="{P}{r$img}" alt="" loading="lazy">')
  else h('<span class="{cls} av-fallback" aria-hidden="true">{esc(initials(r$name))}</span>')
}

photo <- function(r, P) {
  if (!is.na(r$img)) h('<img src="{P}{r$img}" alt="Photo of {esc(r$name)}" loading="lazy">')
  else h('<div class="ph" aria-hidden="true">{esc(initials(r$name))}</div>')
}

legend_html <- function(dot = FALSE) {
  k <- if (dot) "key round" else "key"
  h('<ul class="legend" aria-label="Legend">
  <li class="o-winner"><span class="{k}"></span>Winner</li>
  <li class="o-health"><span class="{k}"></span>Tapped out: health / medical</li>
  <li class="o-personal"><span class="{k}"></span>Tapped out: personal</li>
  <li class="o-other"><span class="{k}"></span>Other (loss of inventory, disqualified)</li>
</ul>')
}

ticks_html <- function() '<div class="ticks" aria-hidden="true"><span style="left:0">0</span><span style="left:25%">25</span><span style="left:50%">50</span><span style="left:75%">75</span><span style="left:100%">100</span></div>'

stat <- function(v, k, s = NULL) {
  s <- if (is.null(s)) "" else h('<span class="s">{s}</span>')
  h('<div class="stat"><span class="v">{v}</span><span class="k">{k}</span>{s}</div>')
}

page <- function(title, body, P = "", active = "", desc = "") {
  nav <- cat0(c(
    h('<a href="{P}index.html"{if (active == "home") \' aria-current="page"\' else ""}>Home</a>'),
    h('<a href="{P}us.html"{if (active == "US") \' aria-current="page"\' else ""}>Alone US</a>'),
    h('<a href="{P}au.html"{if (active == "AU") \' aria-current="page"\' else ""}>Alone Australia</a>')
  ))
  h('<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{esc(title)}</title>
<meta name="description" content="{esc(desc)}">
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link href="https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@600;700;800&family=Barlow:ital,wght@0,400;0,600;0,700;1,400&display=swap" rel="stylesheet">
<link rel="stylesheet" href="{P}assets/style.css">
</head>
<body>
<header class="topbar"><div class="wrap topbar-in">
<a class="brand" href="{P}index.html"><span class="brand-mark">ALONE</span><span class="brand-sub">by the numbers</span></a>
<nav class="nav" aria-label="Main">{nav}</nav>
</div></header>
<main>
{body}
</main>
<footer class="footer"><div class="wrap">
<p>Data from the <a href="https://github.com/doehm/alone">alone</a> R package &mdash; tidy datasets on the survival TV series <em>Alone</em> and <em>Alone Australia</em>.</p>
<p>Sources: Wikipedia, IMDb and the show&#39;s official cast pages. Built {format(Sys.Date(), "%d %b %Y")}.</p>
<p class="disclaimer"><strong>This is an unofficial, fan-built website.</strong> It is not affiliated with or endorsed by HISTORY, A&amp;E Networks, SBS or the producers of <em>Alone</em>. All survivalist images are the property of their respective owners: <em>Alone</em> images &copy; HISTORY / A&amp;E Networks; <em>Alone Australia</em> images &copy; SBS.</p>
</div></footer>
<script src="{P}assets/site.js" defer></script>
</body>
</html>')
}

# -----------------------------------------------------------------------------
# season page (one template for every season)
# -----------------------------------------------------------------------------

build_season <- function(i) {
  s <- se[i, ]
  P <- "../"
  x <- sv[sv$skey == s$skey, ]; x <- x[order(x$result, x$name), ]
  e <- ep[ep$version == s$version & ep$season == s$season, ]; e <- e[order(e$episode), ]
  l <- lo[lo$version == s$version & lo$season == s$season, ]
  win <- x[x$result == 1, ]
  sib <- se[se$group == s$group, ]
  pos <- match(s$skey, sib$skey)

  # --- hero
  hero_img <- win$img[!is.na(win$img)][1]
  hero_cls <- if (is.na(hero_img)) "hero no-photo" else "hero"
  hero_bg  <- if (is.na(hero_img)) "" else h('<div class="hero-bg" style="background-image:url({P}{hero_img})"></div>')
  win_names <- if (nrow(win) > 1) paste(win$name, collapse = " &amp; ") else esc(win$name)
  sub_bits <- c(
    if (!is.na(s$subtitle)) h("<strong>{esc(s$subtitle)}</strong>"),
    esc(s$place)
  )
  drop <- if (!is.na(s$date_drop_off)) h(" &middot; Dropped off {fmt_date(s$date_drop_off)}") else ""
  premiere <- if (nrow(e) && !is.na(e$air_date[1])) h(" &middot; Premiered {fmt_date(e$air_date[1])}") else ""

  hero <- h('<section class="{hero_cls}">{hero_bg}<div class="wrap"><div class="hero-in">
<p class="eyebrow">{s$show}</p>
<h1 class="display">{s$title}</h1>
<p class="hero-sub">{paste(sub_bits, collapse = " &middot; ")}{drop}{premiere}</p>
<div class="hero-winner">{avatar(win[1, ], P)}<div><span class="k">Winner</span><span class="v">{win_names}</span><span class="d">{days_lbl(win$days_lasted[1])} alone</span></div></div>
</div></div></section>')

  # --- season switcher
  pills <- cat0(vapply(seq_len(nrow(sib)), function(j) {
    cur <- if (j == pos) ' aria-current="page"' else ""
    h('<a href="{basename(sib$file[j])}"{cur} title="{esc(sib$show[j])} {esc(sib$title[j])}">{sib$short[j]}</a>')
  }, ""))
  switcher <- h('<nav class="switcher" aria-label="Seasons"><div class="wrap"><span class="lbl">{if (s$group == "US") "Alone US" else "Alone AU"} seasons</span><div class="pills">{pills}</div></div></nav>')

  # --- stat tiles
  n_lbl <- if (any(!is.na(x$team))) h('{nrow(x)}<small>/ {length(unique(na.omit(x$team)))} teams</small>') else nrow(x)
  runner <- x[x$result == 2, ][1, ]
  stats <- cat0(c(
    stat(n_lbl, "Survivalists"),
    stat(h('{win$days_lasted[1]}<small>days</small>'), "Winning time"),
    stat(h('{fmt(mean(x$days_lasted))}<small>days</small>'), "Average stay"),
    stat(h('{fmt(median(x$days_lasted), 0)}<small>days</small>'), "Median stay"),
    stat(sum(x$medically_evacuated, na.rm = TRUE), "Medical evacuations"),
    stat(nrow(e), "Episodes", if (!all(is.na(e$imdb_rating))) h("avg IMDb {fmt(mean_na(e$imdb_rating))}") else NULL)
  ))

  # --- results timeline
  rows <- cat0(vapply(seq_len(nrow(x)), function(j) {
    r <- x[j, ]
    w <- min(100, r$days_lasted)
    meta <- c(if (!is.na(r$age)) paste("Age", r$age), if (!is.na(r$hometown)) esc(r$hometown))
    if (!is.na(r$team)) meta <- c(meta, h("Team: {esc(r$team)}"))
    evac <- if (isTRUE(r$medically_evacuated)) ' <span class="badge badge-evac">&#10010; Med-evac</span>' else ""
    reason <- if (r$outcome == "Winner") "Last one standing" else esc(r$reason_tapped_out)
    ep_tap <- if (!is.na(r$episode_tapped)) h(' <span class="muted">(ep. {r$episode_tapped})</span>') else ""
    h('<li class="tl-row {r$ocls}{if (r$result == 1) " is-winner" else ""}">
<span class="tl-place">{r$result}</span>
{avatar(r, P)}
<div class="tl-who"><a class="tl-name" href="#sv-{r$id}">{esc(r$name)}</a><span class="tl-meta">{paste(meta, collapse = " &middot; ")}</span></div>
<div class="tl-bar"><div class="scale" style="--w:{w}%"><span class="fill"></span><span class="val">{r$days_lasted} <small>days</small></span></div>
<div class="tl-reason"><span class="tag">{esc(r$outcome_lbl)}</span>{evac} &mdash; {reason}{ep_tap}</div></div>
</li>')
  }, ""))

  results <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>The results</h2><p class="lede">Days each survivalist lasted, in finishing order, and why they tapped out. Every season page uses the same 0&ndash;100 day scale so seasons can be compared at a glance.</p></div></div>
<div class="panel">{legend_html()}
<div class="tl-axis"><span></span><span></span><span></span><div class="tl-bar">{ticks_html()}</div></div>
<ol class="tl">{rows}</ol></div>
</div></section>')

  # --- survivalist cards
  cards <- cat0(vapply(seq_len(nrow(x)), function(j) {
    r <- x[j, ]
    items <- l[l$id == r$id, ]; items <- items[order(items$item_number), ]
    load <- if (nrow(items)) h('<p class="sv-load-h">Loadout</p><ul class="chips">{cat0(paste0("<li>", esc(items$item_detailed), "</li>"))}</ul>') else ""
    facts <- c(
      h("<dt>Lasted</dt><dd>{days_lbl(r$days_lasted)}{if (isTRUE(r$medically_evacuated)) ' <span class=\"badge badge-evac\">&#10010; Med-evac</span>' else ''}</dd>"),
      if (!is.na(r$age)) h("<dt>Age</dt><dd>{r$age}</dd>"),
      if (!is.na(r$hometown)) h("<dt>From</dt><dd>{esc(r$hometown)}</dd>"),
      if (!is.na(r$team)) h("<dt>Team</dt><dd>{esc(r$team)}</dd>")
    )
    tag <- if (r$result == 1) "Winner" else "place"
    h('<article class="sv-card{if (r$result == 1) " is-winner" else ""}" id="sv-{r$id}">
<div class="sv-photo">{photo(r, P)}<span class="place-badge">{ordinal(r$result)}<small>{tag}</small></span></div>
<div class="sv-body"><h3>{esc(r$name)}</h3>
{if (!is.na(r$profession)) h(\'<p class="sv-prof">{esc(r$profession)}</p>\') else ""}
<dl class="facts">{cat0(facts)}</dl>
{load}</div></article>')
  }, ""))
  people <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>The survivalists</h2><p class="lede">Who they are, where they&#39;re from and the ten items they carried in.</p></div></div>
<div class="sv-grid">{cards}</div>
</div></section>')

  # --- loadout summary
  if (nrow(l)) {
    n_people <- length(unique(l$id))
    tab <- sort(table(l$item), decreasing = TRUE)
    bars <- cat0(vapply(names(tab), function(it) {
      n <- as.integer(tab[[it]])
      h('<li class="hbar"><div class="hbar-lbl"><b>{esc(it)}</b></div><div class="scale" style="--w:{round(100 * n / n_people, 1)}%"><span class="fill"></span><span class="val">{n} <small>of {n_people}</small></span></div></li>')
    }, ""))
    gap <- if (n_people < nrow(x)) h('<p class="note">Loadout data available for {n_people} of {nrow(x)} survivalists.</p>') else ""
    load_body <- h('<div class="panel">{gap}<ul class="hbars">{bars}</ul></div>')
  } else {
    load_body <- '<div class="note-box">Loadout data isn&#39;t available for this season yet.</div>'
  }
  loadout <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>The loadout</h2><p class="lede">How many survivalists chose each item (items grouped into simplified categories).</p></div></div>
{load_body}
</div></section>')

  # --- episodes
  if (nrow(e)) {
    erows <- cat0(vapply(seq_len(nrow(e)), function(j) {
      r <- e[j, ]
      q <- if (!is.na(r$quote)) h('<span class="ep-quote">&ldquo;{esc(r$quote)}&rdquo; <span>&mdash; {esc(r$author)}</span></span>') else ""
      rating <- if (is.na(r$imdb_rating)) "&mdash;" else
        h('<span class="minibar" title="{r$n_ratings} ratings"><i style="--w:{r$imdb_rating * 10}%"></i>{fmt(r$imdb_rating)}</span>')
      h('<tr><td class="num">{r$episode}</td><td><span class="ep-title">{esc(r$title)}</span>{q}</td><td class="nowrap">{fmt_date(r$air_date)}</td><td class="num">{fmt(r$n_remaining, 0)}</td><td class="num">{fmt(r$viewers, 2)}</td><td class="num">{rating}</td></tr>')
    }, ""))
    ep_body <- h('<div class="table-wrap"><table>
<thead><tr><th class="num">Ep</th><th>Title &amp; opening quote</th><th>Aired</th><th class="num">Remaining</th><th class="num">Viewers (M)</th><th class="num">IMDb</th></tr></thead>
<tbody>{erows}</tbody></table></div>')
  } else ep_body <- '<div class="note-box">No episode data for this season.</div>'
  episodes_sec <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>Episodes</h2><p class="lede">Air dates, survivalists remaining at the start of each episode, viewers and IMDb rating.</p></div></div>
{ep_body}
</div></section>')

  # --- prev / next
  prev <- if (pos > 1) h('<a class="prev" href="{basename(sib$file[pos - 1])}"><span class="dir">&larr; Previous</span><span class="ttl">{sib$title[pos - 1]}</span></a>') else h('<a class="prev" href="{P}{tolower(s$group)}.html"><span class="dir">&larr; Back to</span><span class="ttl">All {s$group} seasons</span></a>')
  nxt <- if (pos < nrow(sib)) h('<a class="next" href="{basename(sib$file[pos + 1])}"><span class="dir">Next &rarr;</span><span class="ttl">{sib$title[pos + 1]}</span></a>') else h('<a class="next" href="{P}{tolower(s$group)}.html"><span class="dir">Back to &rarr;</span><span class="ttl">All {s$group} seasons</span></a>')
  pager <- h('<div class="wrap"><nav class="pager" aria-label="Season pager">{prev}{nxt}</nav></div>')

  body <- cat0(c(hero, switcher,
                 h('<div class="wrap"><div class="stats" style="margin-top:1.6rem">{stats}</div></div>'),
                 results, people, loadout, episodes_sec, pager))
  write_page(page(h("{s$show} {s$title} | Alone by the numbers"), body, P, s$group,
                  h("Results, survivalists, loadouts and episodes for {s$show} {s$title}.")),
             file.path(site, s$file))
}

# -----------------------------------------------------------------------------
# summary page (all seasons of one show)
# -----------------------------------------------------------------------------

build_summary <- function(g) {
  P <- ""
  S <- se[se$group == g, ]
  X <- sv[sv$skey %in% S$skey, ]
  E <- ep[paste(ep$version, ep$season) %in% S$skey, ]
  L <- lo[paste(lo$version, lo$season) %in% S$skey, ]
  W <- X[X$result == 1, ]
  name <- if (g == "US") "Alone" else "Alone Australia"
  winners_by_season <- lapply(S$skey, function(k) W[W$skey == k, ])

  # --- hero + tiles
  longest <- X[which.max(X$days_lasted), ]
  win_days <- vapply(winners_by_season, function(w) w$days_lasted[1], 0)
  people <- length(unique(X$id))
  hero_img <- W$img[!is.na(W$img)]; hero_img <- hero_img[length(hero_img)]
  hero_bg <- if (length(hero_img)) h('<div class="hero-bg" style="background-image:url({hero_img})"></div>') else ""
  hero <- h('<section class="hero">{hero_bg}<div class="wrap"><div class="hero-in">
<p class="eyebrow">All seasons</p>
<h1 class="display">{name}</h1>
<p class="hero-sub"><strong>{nrow(S)} seasons</strong> &middot; {nrow(X)} survivalists &middot; {nrow(E)} episodes &middot; {fmt(sum(X$days_lasted), 0)} days survived between them</p>
</div></div></section>')

  stats <- cat0(c(
    stat(nrow(S), "Seasons"),
    stat(nrow(X), "Survivalists", if (people < nrow(X)) h("{people} different people") else NULL),
    stat(h('{longest$days_lasted}<small>days</small>'), "Longest stay", h('{esc(longest$name)}, {longest$sshort}')),
    stat(h('{fmt(mean(win_days))}<small>days</small>'), "Average winning time"),
    stat(h('{fmt(100 * mean(X$medically_evacuated, na.rm = TRUE), 0)}<small>%</small>'), "Medically evacuated", h("{sum(X$medically_evacuated, na.rm = TRUE)} survivalists")),
    stat(sum(W$gender == "Female"), "Female winners", h("of {length(unique(W$id))} winners"))
  ))

  # --- seasons table
  trows <- cat0(vapply(seq_len(nrow(S)), function(j) {
    s <- S[j, ]; w <- winners_by_season[[j]]; xs <- X[X$skey == s$skey, ]; es <- E[paste(E$version, E$season) == s$skey, ]
    wn <- if (nrow(w) > 1) paste(esc(w$name), collapse = " &amp; ") else esc(w$name)
    h('<tr><td class="nowrap"><a href="{s$file}">{s$title}</a></td><td>{esc(ifelse(is.na(s$subtitle), "", s$subtitle))}<span class="ep-quote" style="font-style:normal">{esc(s$place)}</span></td>
<td><div class="person">{avatar(w[1, ], P)}<span>{wn}</span></div></td><td class="num">{w$days_lasted[1]}</td><td class="num">{fmt(mean(xs$days_lasted))}</td><td class="num">{sum(xs$medically_evacuated, na.rm = TRUE)}</td><td class="num">{nrow(es)}</td><td class="num">{fmt(mean_na(es$imdb_rating))}</td></tr>')
  }, ""))
  season_table <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>Every season</h2><p class="lede">Select a season for the full results, survivalist profiles, loadouts and episode guide.</p></div></div>
<div class="table-wrap"><table><thead><tr><th>Season</th><th>Subtitle &amp; location</th><th>Winner</th><th class="num">Winning days</th><th class="num">Avg days</th><th class="num">Med-evacs</th><th class="num">Episodes</th><th class="num">Avg IMDb</th></tr></thead>
<tbody>{trows}</tbody></table></div>
</div></section>')

  # --- winning time by season
  wbars <- cat0(vapply(seq_len(nrow(S)), function(j) {
    s <- S[j, ]; w <- winners_by_season[[j]]
    wn <- if (nrow(w) > 1) paste(paste(esc(w$first_name), collapse = " &amp; "), esc(w$last_name[1])) else esc(w$name)
    h('<li class="hbar o-winner"><div class="hbar-lbl"><a href="{s$file}">{s$title}</a><span class="sub">{wn}</span></div><div class="scale" style="--w:{min(100, w$days_lasted[1])}%;--c:var(--winner)"><span class="fill"></span><span class="val">{w$days_lasted[1]} <small>days</small></span></div></li>')
  }, ""))
  win_chart <- h('<div class="panel"><h3 class="sv-load-h" style="margin:0 0 .8rem">Winning time by season</h3>
<div class="hbar" style="border:0;padding-bottom:0"><span></span>{ticks_html()}</div>
<ul class="hbars">{wbars}</ul></div>')

  # --- every survivalist strip plot
  strip_rows <- cat0(vapply(seq_len(nrow(S)), function(j) {
    s <- S[j, ]; xs <- X[X$skey == s$skey, ]; xs <- xs[order(xs$days_lasted, -xs$result), ]
    offs <- c(0, -11, 11, -22, 22, -5, 5, -16, 16)
    k <- integer(nrow(xs))
    for (m in seq_len(nrow(xs))) k[m] <- sum(abs(xs$days_lasted[seq_len(m - 1)] - xs$days_lasted[m]) <= 2)
    dots <- cat0(vapply(seq_len(nrow(xs)), function(m) {
      r <- xs[m, ]
      tip <- esc(paste0(days_lbl(r$days_lasted), "|", r$name, "|", ordinal(r$result), " place · ", r$outcome_lbl,
                        if (isTRUE(r$medically_evacuated)) " · med-evac" else ""))
      h('<span class="dot {r$ocls}" style="--x:{min(100, r$days_lasted)}%;--y:{offs[(k[m] %% length(offs)) + 1]}px" tabindex="0" data-tip="{tip}" aria-label="{tip}"></span>')
    }, ""))
    h('<div class="strip-row"><div class="lbl"><a href="{s$file}">{s$short}</a><small>{esc(ifelse(is.na(s$subtitle), s$loc_short, s$subtitle))}</small></div><div class="strip">{dots}</div></div>')
  }, ""))
  strip <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>How long they lasted</h2><p class="lede">Every survivalist in every season, plotted by days lasted. Hover or tab to a dot for details.</p></div></div>
<div class="panel">{legend_html(TRUE)}
{strip_rows}
<div class="strip-axis"><span></span>{ticks_html()}</div></div>
</div></section>')

  # --- why they tapped out
  cats <- c("Health", "Personal", "Other")
  reason_counts <- t(vapply(S$skey, function(k) {
    xs <- X[X$skey == k & X$outcome != "Winner", ]
    vapply(cats, function(cc) sum(xs$outcome == cc), 0)
  }, numeric(3)))
  stack_row <- function(lbl, cnt, cls = "") {
    segs <- cat0(vapply(seq_along(cats), function(c) {
      n <- cnt[c]; if (n == 0) return("")
      pct <- round(100 * n / sum(cnt))
      tip <- esc(paste0(n, " (", pct, "%)|", cats[c], "|", gsub("<[^>]+>", "", lbl)))
      h('<span class="seg o-{tolower(cats[c])}" style="--n:{n}" tabindex="0" data-tip="{tip}" aria-label="{tip}">{if (n / sum(cnt) >= .08) n else ""}</span>')
    }, ""))
    h('<div class="stack-row {cls}"><div class="lbl">{lbl}</div><div class="stack">{segs}</div><div class="n">{sum(cnt)}</div></div>')
  }
  srows <- cat0(c(
    vapply(seq_len(nrow(S)), function(j) stack_row(h('<a href="{S$file[j]}">{S$short[j]}</a>'), reason_counts[j, ]), ""),
    stack_row("All", colSums(reason_counts), "total")
  ))
  rtab <- cat0(vapply(seq_len(nrow(S)), function(j)
    h('<tr><td>{S$short[j]}</td><td class="num">{reason_counts[j, 1]}</td><td class="num">{reason_counts[j, 2]}</td><td class="num">{reason_counts[j, 3]}</td></tr>'), ""))
  reasons <- h('<div class="panel"><h3 class="sv-load-h" style="margin:0 0 .8rem">Why they tapped out (excluding winners)</h3>
<ul class="legend"><li class="o-health"><span class="key"></span>Health / medical</li><li class="o-personal"><span class="key"></span>Personal</li><li class="o-other"><span class="key"></span>Other</li></ul>
{srows}
<details class="table-view"><summary>Show as table</summary><div class="table-wrap"><table><thead><tr><th>Season</th><th class="num">Health</th><th class="num">Personal</th><th class="num">Other</th></tr></thead><tbody>{rtab}</tbody></table></div></details>
</div>')

  charts <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>Winners &amp; tap-outs</h2><p class="lede">How long it took to win each season, and what ended everyone else&#39;s run.</p></div></div>
<div class="cols">{win_chart}{reasons}</div>
</div></section>')

  # --- hall of winners
  tiles <- cat0(vapply(seq_len(nrow(W)), function(j) {
    r <- W[j, ]
    img <- if (!is.na(r$img)) h('<img src="{r$img}" alt="" loading="lazy">') else '<div class="ph"></div>'
    h('<a class="tile" href="{r$sfile}#sv-{r$id}">{img}<span class="t-days">{r$days_lasted} days</span><div class="tile-in"><span class="t-eyebrow">{r$sshort}</span><span class="t-title">{esc(r$name)}</span><span class="t-sub">{if (!is.na(r$age)) paste0("Age ", r$age, " &middot; ") else ""}{esc(ifelse(is.na(r$hometown), "", r$hometown))}</span></div></a>')
  }, ""))
  hall <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>The winners</h2><p class="lede">Last one standing, every season.</p></div></div>
<div class="tile-grid">{tiles}</div>
</div></section>')

  # --- leaderboard
  top <- X[order(-X$days_lasted, X$result), ][1:10, ]
  lrows <- cat0(vapply(seq_len(nrow(top)), function(j) {
    r <- top[j, ]
    h('<tr class="{if (r$result == 1) "is-winner" else ""}"><td class="num">{j}</td><td><div class="person">{avatar(r, P)}<a href="{r$sfile}#sv-{r$id}">{esc(r$name)}</a></div></td><td><a href="{r$sfile}">{r$sshort}</a></td><td>{ordinal(r$result)}</td><td class="num">{r$days_lasted}</td></tr>')
  }, ""))
  board <- h('<div><h3 class="sv-load-h" style="margin:0 0 .8rem">Longest stays</h3><div class="table-wrap"><table><thead><tr><th class="num">#</th><th>Survivalist</th><th>Season</th><th>Place</th><th class="num">Days</th></tr></thead><tbody>{lrows}</tbody></table></div></div>')

  # --- returning survivalists
  dup <- unique(X$id[duplicated(X$id)])
  if (length(dup)) {
    rr <- cat0(vapply(dup, function(id) {
      xs <- X[X$id == id, ]; xs <- xs[order(match(xs$skey, S$skey)), ]
      runs <- paste(h('<a href="{xs$sfile}">{xs$sshort}</a> {ordinal(xs$result)} ({xs$days_lasted}d)'), collapse = " &rarr; ")
      h('<tr><td><div class="person">{avatar(xs[nrow(xs), ], P)}<span>{esc(xs$name[1])}</span></div></td><td>{runs}</td></tr>')
    }, ""))
    returning <- h('<div><h3 class="sv-load-h" style="margin:0 0 .8rem">Returning survivalists</h3><div class="table-wrap"><table><thead><tr><th>Survivalist</th><th>Appearances</th></tr></thead><tbody>{rr}</tbody></table></div></div>')
  } else {
    ages <- X[!is.na(X$age), ]
    returning <- h('<div><h3 class="sv-load-h" style="margin:0 0 .8rem">Who goes out there</h3><div class="stats" style="margin:0;grid-template-columns:repeat(2,1fr)">
{stat(fmt(mean(ages$age)), "Average age")}{stat(sum(X$gender == "Female"), "Women", h("of {nrow(X)} survivalists"))}
{stat(min(ages$age), "Youngest", esc(ages$name[which.min(ages$age)]))}{stat(max(ages$age), "Oldest", esc(ages$name[which.max(ages$age)]))}</div></div>')
  }
  records <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>Records</h2></div></div>
<div class="cols">{board}{returning}</div>
</div></section>')

  # --- loadouts across seasons
  n_people <- nrow(unique(L[, c("version", "season", "id")]))
  if (n_people >= 10) {
    tab <- tapply(paste(L$version, L$season, L$id), L$item, function(z) length(unique(z)))
    tab <- sort(tab, decreasing = TRUE)[1:min(15, length(tab))]
    bars <- cat0(vapply(names(tab), function(it) {
      pct <- 100 * tab[[it]] / n_people
      h('<li class="hbar"><div class="hbar-lbl"><b>{esc(it)}</b></div><div class="scale" style="--w:{round(pct, 1)}%"><span class="fill"></span><span class="val">{fmt(pct, 0)}% <small>({tab[[it]]})</small></span></div></li>')
    }, ""))
    load_body <- h('<div class="panel"><p class="note" style="margin-bottom:.6rem">Share of survivalists who packed each item &mdash; top 15 of {length(unique(L$item))} item types, across {n_people} loadouts.</p><ul class="hbars">{bars}</ul></div>')
  } else {
    load_body <- h('<div class="note-box">Loadout data is only available for {n_people} {name} survivalist{if (n_people == 1) "" else "s"} so far &mdash; not enough for a meaningful summary.</div>')
  }
  loadouts_sec <- h('<section class="section"><div class="wrap">
<div class="section-head"><div><h2>Most popular loadout items</h2><p class="lede">Each survivalist picks ten items to take into the wilderness.</p></div></div>
{load_body}
</div></section>')

  body <- cat0(c(hero, h('<div class="wrap"><div class="stats">{stats}</div></div>'),
                 season_table, charts, strip, hall, records, loadouts_sec))
  write_page(page(h("{name}: all seasons | Alone by the numbers"), body, P, g,
                  h("Summary of every {name} season: winners, survival times, tap-out reasons and loadouts.")),
             file.path(site, paste0(tolower(g), ".html")))
}

# -----------------------------------------------------------------------------
# home page
# -----------------------------------------------------------------------------

build_home <- function() {
  P <- ""
  show_card <- function(g) {
    S <- se[se$group == g, ]; X <- sv[sv$skey %in% S$skey, ]; W <- X[X$result == 1, ]
    W <- W[!duplicated(W$skey), ]
    faces <- cat0(vapply(seq_len(nrow(W)), function(j) avatar(W[j, ], P), ""))
    name <- if (g == "US") "Alone" else "Alone Australia"
    blurb <- if (g == "US") "The original History Channel series &mdash; plus the all-star spin-off <em>Alone: Frozen</em>." else "SBS&#39;s Australian edition, from lutruwita / Tasmania to Finnish Lapland."
    h('<a class="show-card" href="{tolower(g)}.html">
<p class="eyebrow">{if (g == "US") "United States" else "Australia"}</p>
<h3>{name}</h3>
<p>{blurb}</p>
<div class="faces" aria-hidden="true">{faces}</div>
<div class="mini-stats"><div><b>{nrow(S)}</b><span>Seasons</span></div><div><b>{nrow(X)}</b><span>Survivalists</span></div><div><b>{max(X$days_lasted)}</b><span>Longest stay</span></div></div>
<span class="cta">All {g} seasons &rarr;</span>
</a>')
  }

  season_tiles <- function(g) {
    S <- se[se$group == g, ]
    cat0(vapply(seq_len(nrow(S)), function(j) {
      s <- S[j, ]; w <- sv[sv$skey == s$skey & sv$result == 1, ]
      img <- if (!is.na(w$img[1])) h('<img src="{w$img[1]}" alt="" loading="lazy">') else '<div class="ph"></div>'
      wn <- if (nrow(w) > 1) paste(paste(w$first_name, collapse = " & "), w$last_name[1]) else w$name
      h('<a class="tile" href="{s$file}">{img}<span class="t-days">{w$days_lasted[1]} days</span><div class="tile-in"><span class="t-eyebrow">{esc(ifelse(is.na(s$subtitle), s$loc_short, s$subtitle))}</span><span class="t-title">{s$title}</span><span class="t-sub">Winner: {esc(wn)}</span></div></a>')
    }, ""))
  }

  longest <- sv[which.max(sv$days_lasted), ]
  hero <- h('<section class="hero"><div class="hero-bg" style="background-image:url({longest$img})"></div><div class="wrap"><div class="hero-in">
<p class="eyebrow">The survival series, by the numbers</p>
<h1 class="display">Alone<br><span class="thin">in the wild</span></h1>
<p class="hero-sub">Dropped into the wilderness with ten items and a camera. No crew, no contact &mdash; the last one standing wins. <strong>{nrow(se)} seasons</strong>, <strong>{nrow(sv)} survivalists</strong> and <strong>{fmt(sum(sv$days_lasted), 0)} days</strong> of results.</p>
</div></div></section>')

  body <- cat0(c(
    hero,
    h('<div class="wrap"><div class="show-grid">{show_card("US")}{show_card("AU")}</div></div>'),
    h('<section class="section"><div class="wrap"><div class="section-head"><div><h2>Alone US seasons</h2></div><a class="cta" href="us.html">Season summary &rarr;</a></div><div class="tile-grid">{season_tiles("US")}</div></div></section>'),
    h('<section class="section"><div class="wrap"><div class="section-head"><div><h2>Alone Australia seasons</h2></div><a class="cta" href="au.html">Season summary &rarr;</a></div><div class="tile-grid">{season_tiles("AU")}</div></div></section>'),
    h('<section class="section"><div class="wrap"><div class="section-head"><div><h2>Get the data</h2><p class="lede">Everything on this site comes from the <code>alone</code> R package: four tidy datasets &mdash; <code>survivalists</code>, <code>loadouts</code>, <code>episodes</code> and <code>seasons</code>.</p></div></div>
<pre>install.packages("alone")
library(alone)
survivalists</pre></div></section>')
  ))
  write_page(page("Alone by the numbers", body, P, "home",
                  "Results, survivalists and loadouts for every season of Alone and Alone Australia."),
             file.path(site, "index.html"))
}

# -----------------------------------------------------------------------------
# build
# -----------------------------------------------------------------------------

for (i in seq_len(nrow(se))) build_season(i)
build_summary("US")
build_summary("AU")
build_home()
message("Built ", nrow(se), " season pages, 2 summaries and the home page in ", normalizePath(site))
