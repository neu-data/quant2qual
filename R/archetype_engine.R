# ---------------------------------------------------------------------------
# The classification engine.
#
# Three steps, kept separate so that each can be inspected:
#
#   1. Scale   - every dimension is standardised against the cohort it came
#                from (mean/SD, median/MAD or rank-based normal scores), so a
#                "high" friction reading means high *for this dataset*.
#   2. Band    - standardised scores are cut into low / mid / high, either at
#                +/- `threshold` or at raw-unit `cutpoints` the analyst names.
#   3. Match   - each archetype is a set of allowed bands per dimension. Every
#                case gets a fuzzy membership in every archetype (a smooth
#                version of "inside the allowed bands"), a strict rule match,
#                an assigned archetype, a confidence and a runner-up.
#
# Membership is what lets the narrative hedge honestly: a case sitting on the
# boundary between two signatures is described as such, rather than being
# reported with the same certainty as a textbook example.
# ---------------------------------------------------------------------------

# ---- mock data ----------------------------------------------------------------

# Latent behavioral signatures used to simulate data, in UX units:
# mean session minutes, mean actions, mean errors, and share of the cohort.
mock_signatures <- tibble::tribble(
  ~signature,   ~minutes, ~actions, ~errors, ~share,
  "baseline",       25,       75,     3.0,   0.27,
  "deliberate",     48,       42,     1.0,   0.18,
  "fluent",         14,      150,     1.0,   0.15,
  "obstructed",     46,      165,    13.0,   0.12,
  "early_exit",     11,       36,     9.0,   0.08,
  "shallow",         7,       16,     0.8,   0.20
)

# Per-domain rescaling of the UX units: minutes, actions, errors
mock_domain_scale <- list(
  ux       = c(1.0, 1.0, 1.0),
  learning = c(4.0, 0.6, 1.0),
  survey   = c(0.7, 0.8, 0.5)
)

#' Simulate a behavioral dataset
#'
#' Generates a synthetic cohort from a known mixture of behavioral
#' signatures, so that the classification engine can be demonstrated and
#' tested against a ground truth. Time is log-normal, activity is negative
#' binomial and friction is Poisson, which reproduces the right skew and the
#' excess of zeros found in real clickstream, learning and paradata logs.
#'
#' Column names follow the domain's lexicon (see [q2q_lexicon()]): for the
#' default `"ux"` domain they are `user_id`, `active_minutes`,
#' `action_clicks` and `error_count`.
#'
#' @param n Number of cases to simulate (at least 10).
#' @param domain One of `"ux"`, `"learning"` or `"survey"`.
#' @param seed Random seed, or `NULL` to use the current RNG state. The
#'   caller's RNG state is restored afterwards either way.
#' @param include_truth If `TRUE`, adds a `true_archetype` column holding
#'   the simulated signature, named in the domain's terms.
#'
#' @return A [tibble][tibble::tibble] with an integer id column (starting at
#'   101) and three numeric behavioral columns.
#' @export
#' @family archetype engine
#' @examples
#' mock_behavioral_data(10)
#' mock_behavioral_data(10, domain = "learning")
#'
#' d <- mock_behavioral_data(200, include_truth = TRUE)
#' table(d$true_archetype)
mock_behavioral_data <- function(n = 200, domain = "ux", seed = 2026,
                                 include_truth = FALSE) {
  check_number(n, min = 10)
  check_flag(include_truth)
  lx <- q2q_lexicon(domain)
  if (!is.null(seed)) {
    check_number(seed)
    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
    } else {
      on.exit(rm(".Random.seed", envir = globalenv()), add = TRUE)
    }
    set.seed(seed)
  }
  n <- as.integer(n)
  sig <- mock_signatures
  mult <- mock_domain_scale[[domain]]
  who <- sample(seq_len(nrow(sig)), n, replace = TRUE, prob = sig$share)

  minutes <- stats::rlnorm(n, meanlog = log(sig$minutes[who] * mult[1]), sdlog = 0.28)
  actions <- stats::rnbinom(n, mu = sig$actions[who] * mult[2], size = 12)
  errors  <- stats::rpois(n, lambda = sig$errors[who] * mult[3])

  am <- default_archetypes(domain)
  key_to_name <- c(
    baseline   = am$archetype[am$fallback],
    stats::setNames(am$archetype[!am$fallback],
                    c("obstructed", "early_exit", "deliberate", "fluent", "shallow"))
  )

  out <- tibble::tibble(
    id = 100L + seq_len(n),
    velocity = round(minutes, 1),
    intensity = as.integer(actions),
    friction = as.integer(errors)
  )
  names(out) <- unname(lx$columns[c("id", "velocity", "intensity", "friction")])
  if (include_truth) {
    out$true_archetype <- factor(unname(key_to_name[sig$signature[who]]),
                                 levels = am$archetype)
  }
  out
}

# ---- archetype definitions ------------------------------------------------------

#' Parse a band specification into allowed levels
#' @noRd
parse_levels <- function(x, dim = "dimension", call = rlang::caller_env()) {
  if (!is.character(x) || length(x) == 0L || anyNA(x)) {
    q2q_abort("The {.field {dim}} rule must be a character vector of bands.",
              class = "quant2qual_value_error", call = call)
  }
  parts <- trimws(tolower(unlist(strsplit(x, "|", fixed = TRUE))))
  parts <- unlist(lapply(parts, function(p) {
    switch(p,
      any = , all = c("low", "mid", "high"),
      not_high = , `not high` = c("low", "mid"),
      not_low = , `not low` = c("mid", "high"),
      medium = , average = , typical = , moderate = "mid",
      p)
  }))
  bad <- setdiff(parts, c("low", "mid", "high"))
  if (length(bad) > 0L) {
    q2q_abort(c("{cli::qty(length(bad))}Unknown band{?s} {.val {bad}} in the {.field {dim}} rule.",
                i = "Use {.val low}, {.val mid}, {.val high}, combinations such as {.val low|mid}, or {.val any}."),
              class = "quant2qual_value_error", call = call)
  }
  lv <- intersect(c("low", "mid", "high"), parts)
  if (identical(lv, c("low", "high"))) {
    q2q_abort(c("The {.field {dim}} rule {.val low|high} is not a contiguous range.",
                i = "Define two archetypes instead, one for each tail."),
              class = "quant2qual_value_error", call = call)
  }
  lv
}

#' Define a behavioral archetype
#'
#' An archetype is a behavioral signature -- the bands that each dimension
#' must fall in -- together with the language used to describe it. Combine
#' several with [archetype_matrix()] and pass the result to
#' [identify_archetype()].
#'
#' Bands are relative to the cohort: with the default `threshold = 0.5`,
#' `"high"` means more than half a standard deviation above the cohort mean.
#' A rule may allow several adjacent bands (`"low|mid"`, or equivalently
#' `c("low", "mid")` or `"not_high"`) and `"any"` leaves a dimension
#' unconstrained.
#'
#' Text fields may use the placeholders `{subject}` and `{Subject}`, which are
#' replaced by the lexicon's subject noun (for example "the participant").
#'
#' @param name Archetype name, e.g. `"Frustrated Fighter"`.
#' @param velocity,intensity,friction Allowed bands for time, activity and
#'   friction.
#' @param persona One-sentence qualitative persona description.
#' @param headline Opening sentence of a case narrative. Defaults to a
#'   sentence built from `theme`.
#' @param interpretation Interpretive paragraph used in case narratives.
#' @param recommendations Character vector of implications or actions.
#' @param theme Short thematic label, e.g. `"obstructed persistence"`.
#' @param icon Optional emoji or symbol shown in console output only.
#' @param colour Hex colour used in plots.
#' @param priority Integer priority for rule matching (lower wins). Defaults
#'   to the archetype's position in the matrix.
#' @param fallback Is this the archetype for cases that match no rule? Each
#'   matrix has exactly one.
#'
#' @return A one-row [tibble][tibble::tibble] of class `q2q_archetype`.
#' @export
#' @family archetype engine
#' @examples
#' define_archetype(
#'   "Night Owl Power User",
#'   velocity = "high", intensity = "high", friction = "low|mid",
#'   persona = "A committed expert who works long, dense sessions.",
#'   recommendations = "Offer automation for repetitive long-session tasks."
#' )
define_archetype <- function(name, velocity = "any", intensity = "any",
                             friction = "any", persona = "", headline = NULL,
                             interpretation = "", recommendations = character(),
                             theme = NULL, icon = "", colour = NA_character_,
                             priority = NA_integer_, fallback = FALSE) {
  check_string(name)
  check_flag(fallback)
  for (arg in c("persona", "interpretation", "icon")) {
    v <- get(arg)
    if (!is.character(v) || length(v) != 1L || is.na(v)) {
      q2q_abort("{.arg {arg}} must be a single string.", class = "quant2qual_type_error")
    }
  }
  if (!is.character(recommendations)) {
    q2q_abort("{.arg recommendations} must be a character vector.",
              class = "quant2qual_type_error")
  }
  if (is.null(theme)) theme <- tolower(name)
  check_string(theme)
  if (is.null(headline)) {
    headline <- paste0("{Subject} presented a profile of ", theme, ".")
  }
  check_string(headline)
  if (!is.na(colour) && !grepl("^#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{8})$", colour) &&
      !colour %in% grDevices::colors()) {
    q2q_abort("{.arg colour} must be a hex colour or an R colour name.",
              class = "quant2qual_value_error")
  }
  rules <- list(velocity = velocity, intensity = intensity, friction = friction)
  rules <- purrr::imap_chr(rules, function(r, d) paste(parse_levels(r, d), collapse = "|"))

  out <- tibble::tibble(
    archetype = name,
    velocity = rules[["velocity"]],
    intensity = rules[["intensity"]],
    friction = rules[["friction"]],
    theme = theme,
    persona = persona,
    headline = headline,
    interpretation = interpretation,
    recommendations = list(recommendations),
    icon = icon,
    colour = as.character(colour),
    priority = as.integer(priority),
    fallback = fallback
  )
  class(out) <- c("q2q_archetype", class(out))
  out
}

#' Combine archetypes into a classification matrix
#'
#' @param ... Archetypes from [define_archetype()], or data frames of them.
#' @param domain Optional domain label recorded on the matrix.
#'
#' @return A tibble of class `q2q_archetype_matrix` with one row per
#'   archetype. Exactly one row is the fallback; if none was defined, a
#'   neutral `"Baseline"` fallback is appended. Missing priorities follow row
#'   order and missing colours are filled from [neudata_palette()].
#' @export
#' @family archetype engine
#' @examples
#' m <- archetype_matrix(
#'   define_archetype("Sprinter", velocity = "low", intensity = "high"),
#'   define_archetype("Lingerer", velocity = "high", intensity = "low"),
#'   define_archetype("Everyone else", fallback = TRUE)
#' )
#' m
archetype_matrix <- function(..., domain = NULL) {
  rows <- rlang::list2(...)
  if (length(rows) == 0L) {
    q2q_abort("Supply at least one archetype.", class = "quant2qual_value_error")
  }
  ok <- vapply(rows, is.data.frame, logical(1))
  if (!all(ok)) {
    q2q_abort("Every argument must come from {.fn define_archetype}.",
              class = "quant2qual_type_error")
  }
  m <- dplyr::bind_rows(lapply(rows, function(r) {
    tibble::as_tibble(unclass_tbl(r))
  }))
  need <- c("archetype", q2q_dimensions(), "persona", "headline",
            "interpretation", "recommendations", "fallback")
  check_columns(m, need, arg = "archetypes")
  if (anyDuplicated(m$archetype)) {
    dup <- unique(m$archetype[duplicated(m$archetype)])
    q2q_abort("Archetype names must be unique; {.val {dup}} appears more than once.",
              class = "quant2qual_value_error")
  }
  if (sum(m$fallback) > 1L) {
    q2q_abort("Only one archetype may be the fallback; found {.val {m$archetype[m$fallback]}}.",
              class = "quant2qual_value_error")
  }
  if (sum(m$fallback) == 0L) {
    m <- dplyr::bind_rows(m, unclass_tbl(define_archetype(
      "Baseline", fallback = TRUE, theme = "cohort-typical behavior",
      persona = "A case whose behavior sits within the ordinary range of the cohort.",
      headline = "{Subject} followed a pattern broadly typical of the cohort.",
      interpretation = "No dimension departs far enough from the cohort norm to define a distinct behavioral signature."
    )))
  }
  m$priority <- ifelse(is.na(m$priority), seq_len(nrow(m)), m$priority)
  m$priority[m$fallback] <- .Machine$integer.max
  missing_col <- is.na(m$colour)
  if (any(missing_col)) {
    pal <- rep(c(neudata_palette(), "#E0A340", "#C0504D"), length.out = nrow(m))
    m$colour[missing_col] <- pal[missing_col]
  }
  # The fallback always goes last: it is what is left over, not a signature.
  m <- dplyr::bind_rows(m[!m$fallback, ], m[m$fallback, ])
  structure(m, class = c("q2q_archetype_matrix", "tbl_df", "tbl", "data.frame"),
            domain = domain)
}

#' @noRd
unclass_tbl <- function(x) {
  class(x) <- setdiff(class(x), c("q2q_archetype", "q2q_archetype_matrix"))
  x
}

#' @export
print.q2q_archetype_matrix <- function(x, ...) {
  cli_print({
    dom <- attr(x, "domain")
    cli::cli_text("{.strong Archetype matrix} ({nrow(x)} archetype{?s}{if (!is.null(dom)) paste0(', domain ', dom) else ''})")
    for (i in seq_len(nrow(x))) {
      r <- x[i, ]
      rule <- if (r$fallback) "{.emph fallback: matches no other rule}" else
        sprintf("V: %s | I: %s | F: %s", r$velocity, r$intensity, r$friction)
      nm <- r$archetype
      th <- r$theme
      cli::cli_bullets(c("*" = paste0("{.strong {nm}} ", rule, " {.emph -- {th}}")))
    }
  })
  invisible(x)
}

# ---- scaling ------------------------------------------------------------------------

#' Build a scaler for one dimension
#'
#' Returns forward (raw -> standardised) and inverse functions, so that raw
#' cutpoints can be placed on the standardised scale and band edges can be
#' reported back in raw units.
#' @noRd
make_scaler <- function(x, method, transform, dim, call = rlang::caller_env()) {
  fwd_t <- if (transform == "log1p") log1p else identity
  inv_t <- if (transform == "log1p") expm1 else identity
  tx <- fwd_t(x)
  n <- length(tx)
  if (length(unique(tx)) < 2L) {
    q2q_abort(c("The {.field {dim}} dimension has no variation.",
                i = "Every complete case has the value {.val {x[1]}}; it cannot be standardised."),
              class = "quant2qual_value_error", call = call)
  }
  if (method == "percentile") {
    sx <- sort(tx)
    p <- (rank(sx, ties.method = "average") - 0.5) / n
    keep <- !duplicated(sx)
    ux <- sx[keep]
    up <- p[keep]
    fn <- function(v) {
      pv <- stats::approx(ux, up, xout = fwd_t(v), rule = 2, ties = mean)$y
      stats::qnorm(pmin(pmax(pv, 0.5 / n), 1 - 0.5 / n))
    }
    inv <- function(z) unname(stats::quantile(x, stats::pnorm(z), type = 7, names = FALSE))
    return(list(fn = fn, inverse = inv, center = stats::median(tx), scale = NA_real_))
  }
  if (method == "z") {
    center <- mean(tx)
    scale <- stats::sd(tx)
  } else {
    center <- stats::median(tx)
    scale <- stats::mad(tx)
    if (!is.finite(scale) || scale == 0) scale <- stats::IQR(tx) / 1.349
    if (!is.finite(scale) || scale == 0) scale <- stats::sd(tx)
  }
  list(
    fn = function(v) (fwd_t(v) - center) / scale,
    inverse = function(z) inv_t(center + z * scale),
    center = center, scale = scale
  )
}

#' Smooth membership of z in the interval [a, b]
#' @noRd
interval_membership <- function(z, a, b, k) {
  lo <- if (is.finite(a)) stats::plogis(k * (z - a)) else 1
  hi <- if (is.finite(b)) stats::plogis(k * (b - z)) else 1
  lo * hi
}

#' Interval on the standardised scale covered by a set of bands
#' @noRd
bands_to_interval <- function(levels, edges) {
  lower <- c(low = -Inf, mid = edges[1], high = edges[2])
  upper <- c(low = edges[1], mid = edges[2], high = Inf)
  c(min(lower[levels]), max(upper[levels]))
}

# ---- classification -------------------------------------------------------------------

#' Identify behavioral archetypes
#'
#' The classification engine. Standardises the three behavioral dimensions
#' against the cohort, bands them into low / mid / high and assigns every
#' case to an archetype, with a membership-based confidence and the closest
#' competing archetype.
#'
#' @section Scaling:
#' * `"z"` -- mean and standard deviation (the classical choice).
#' * `"robust"` -- median and MAD, falling back to IQR/1.349 when the MAD is
#'   zero. Recommended for skewed counts such as errors, where a few extreme
#'   cases inflate the SD and hide everyone else.
#' * `"percentile"` -- rank-based normal scores: each value is replaced by
#'   the normal quantile of its mid-rank percentile. Fully distribution-free.
#'
#' `transform = "log1p"` is applied before scaling and suits durations and
#' counts with a long right tail.
#'
#' @section Bands and cutpoints:
#' By default a dimension is `"high"` above `+threshold` and `"low"` below
#' `-threshold` on the standardised scale. `cutpoints` overrides this with
#' fixed raw-unit edges for any dimension, for example
#' `cutpoints = list(friction = c(2, 8))` makes fewer than 2 errors low and
#' more than 8 high regardless of the cohort.
#'
#' @section Assignment:
#' Every case receives a fuzzy membership in every archetype: for each
#' constrained dimension, a logistic ramp equal to 0.5 at a band edge and
#' rising towards 1 inside the allowed range (steepness `sharpness`),
#' combined across dimensions by geometric mean. The fallback archetype's
#' membership is the membership of all three dimensions in the mid band.
#'
#' * `method = "rules"` assigns the highest-priority archetype whose rule is
#'   met exactly, and the fallback when none is. This is the transparent,
#'   auditable choice, equivalent to a `case_when()` cascade.
#' * `method = "fuzzy"` assigns the archetype with the highest membership,
#'   so every case goes to its nearest signature.
#'
#' In both cases `confidence` is the membership of the assigned archetype,
#' and `runner_up` is the archetype with the next-highest membership.
#'
#' @param data A data frame with one row per case.
#' @param velocity,intensity,friction Column names (strings) holding time,
#'   activity and friction. Default to the domain lexicon's column names.
#' @param id Column holding case identifiers. Defaults to the lexicon's id
#'   column when present, otherwise a `case_id` column is created.
#' @param domain Domain pack: `"ux"`, `"learning"` or `"survey"`.
#' @param archetypes An [archetype_matrix()]. Defaults to the domain's
#'   built-in matrix.
#' @param method `"rules"` or `"fuzzy"`; see *Assignment*.
#' @param scaling `"z"`, `"robust"` or `"percentile"`; see *Scaling*.
#' @param transform `"none"` or `"log1p"`.
#' @param threshold Band edge on the standardised scale: a single positive
#'   number, or a named vector with one value per dimension.
#' @param cutpoints Optional named list of raw-unit band edges, each a
#'   length-2 increasing numeric vector.
#' @param sharpness Steepness of the membership ramps (positive number).
#' @param lexicon A [q2q_lexicon()] used later by the narrative engine.
#'   Defaults to the domain's lexicon.
#'
#' @return A tibble of class `q2q_profile`: the input columns plus
#'   `archetype` (factor), `confidence`, `runner_up`,
#'   `runner_up_membership`, `z_*`, `pct_*` (percentile rank, 0-100) and
#'   `level_*` for each dimension, `pace` (activity per unit time),
#'   `friction_rate` (friction per 100 activity units) and `outlier`
#'   (beyond a Tukey fence on any dimension). Classification settings,
#'   cohort reference statistics and the full membership matrix are stored
#'   in the `"q2q"` attribute.
#' @export
#' @family archetype engine
#' @examples
#' users <- mock_behavioral_data(200)
#' profile <- identify_archetype(users)
#' profile
#' summary(profile)
#'
#' # Robust scaling and a fixed definition of "high friction"
#' identify_archetype(users, scaling = "robust",
#'                    cutpoints = list(friction = c(2, 8)))
#'
#' # Learning analytics with the matching domain pack
#' identify_archetype(mock_behavioral_data(150, domain = "learning"),
#'                    domain = "learning")
identify_archetype <- function(data, velocity = NULL, intensity = NULL,
                               friction = NULL, id = NULL, domain = "ux",
                               archetypes = default_archetypes(domain),
                               method = c("rules", "fuzzy"),
                               scaling = c("z", "robust", "percentile"),
                               transform = c("none", "log1p"),
                               threshold = 0.5, cutpoints = NULL,
                               sharpness = 6, lexicon = q2q_lexicon(domain)) {
  check_data_frame(data)
  method <- rlang::arg_match(method)
  scaling <- rlang::arg_match(scaling)
  transform <- rlang::arg_match(transform)
  check_number(sharpness, min = 0.1)
  if (!inherits(archetypes, "q2q_archetype_matrix")) {
    q2q_abort("{.arg archetypes} must be built with {.fn archetype_matrix} or {.fn default_archetypes}.",
              class = "quant2qual_type_error")
  }
  if (!inherits(lexicon, "q2q_lexicon")) {
    q2q_abort("{.arg lexicon} must be built with {.fn q2q_lexicon}.",
              class = "quant2qual_type_error")
  }
  dims <- q2q_dimensions()

  # Re-classifying a profile: drop the columns this function adds.
  if (inherits(data, "q2q_profile")) {
    data <- strip_profile(data)
  }
  data <- tibble::as_tibble(data)

  cols <- c(velocity = velocity %||% lexicon$columns[["velocity"]],
            intensity = intensity %||% lexicon$columns[["intensity"]],
            friction = friction %||% lexicon$columns[["friction"]])
  for (d in dims) check_string(cols[[d]], arg = d)
  if (anyDuplicated(cols)) {
    q2q_abort("Each dimension must come from a different column.",
              class = "quant2qual_value_error")
  }
  check_columns(data, unname(cols))
  check_numeric_columns(data, unname(cols))

  if (is.null(id)) {
    id <- if (lexicon$columns[["id"]] %in% names(data)) lexicon$columns[["id"]] else "case_id"
    if (identical(id, "case_id") && !"case_id" %in% names(data)) {
      data$case_id <- seq_len(nrow(data))
    }
  }
  check_string(id)
  check_columns(data, id)
  if (anyDuplicated(data[[id]])) {
    q2q_abort(c("Case identifiers in {.field {id}} must be unique.",
                x = "{sum(duplicated(data[[id]]))} duplicate{?s} found."),
              class = "quant2qual_value_error")
  }

  added <- profile_columns()
  clash <- intersect(added, names(data))
  if (length(clash) > 0L) {
    q2q_warn("Overwriting existing column{?s} {.field {clash}}.")
    data <- data[setdiff(names(data), clash)]
  }

  thr <- resolve_threshold(threshold, dims)
  cutpoints <- check_cutpoints(cutpoints, dims)

  X <- as.matrix(data[unname(cols)])
  colnames(X) <- dims
  complete <- stats::complete.cases(X)
  n_ok <- sum(complete)
  if (n_ok < 3L) {
    q2q_abort("At least 3 complete cases are needed to standardise a cohort; found {n_ok}.",
              class = "quant2qual_value_error")
  }
  if (n_ok < nrow(X)) {
    q2q_warn(c("{nrow(X) - n_ok} case{?s} with missing values could not be classified.",
               i = "Their {.field archetype} is {.val {NA}}."),
             class = "quant2qual_missing_values")
  }
  if (n_ok < 20L) {
    q2q_warn("Only {n_ok} complete cases: cohort-relative bands are unstable below about 20.",
             class = "quant2qual_small_cohort")
  }
  neg <- dims[colSums(X[complete, , drop = FALSE] < 0) > 0]
  if (length(neg) > 0L) {
    if (transform == "log1p") {
      q2q_abort("{.code transform = \"log1p\"} needs non-negative values; {.field {cols[neg]}} {?has/have} negatives.",
                class = "quant2qual_value_error")
    }
    q2q_warn("Negative values in {.field {cols[neg]}}: time, activity and friction are usually non-negative.")
  }

  n <- nrow(X)
  Z <- P <- matrix(NA_real_, n, 3, dimnames = list(NULL, dims))
  L <- matrix(NA_character_, n, 3, dimnames = list(NULL, dims))
  reference <- list()
  edges <- list()
  for (d in dims) {
    x <- X[complete, d]
    sc <- make_scaler(x, scaling, transform, cols[[d]])
    e <- if (!is.null(cutpoints[[d]])) sc$fn(cutpoints[[d]]) else c(-thr[[d]], thr[[d]])
    if (!(e[1] < e[2])) {
      q2q_abort("The band edges for {.field {d}} collapse onto each other on the standardised scale.",
                class = "quant2qual_value_error")
    }
    edges[[d]] <- e
    z <- sc$fn(x)
    Z[complete, d] <- z
    P[complete, d] <- 100 * (rank(x, ties.method = "average") - 0.5) / length(x)
    L[complete, d] <- ifelse(z < e[1], "low", ifelse(z > e[2], "high", "mid"))
    q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    iqr <- q[3] - q[1]
    reference[[d]] <- list(
      column = cols[[d]], n = length(x), mean = mean(x), sd = stats::sd(x),
      median = q[2], q1 = q[1], q3 = q[3], min = min(x), max = max(x),
      fence_low = q[1] - 1.5 * iqr, fence_high = q[3] + 1.5 * iqr,
      edges_z = e,
      edges_raw = if (!is.null(cutpoints[[d]])) cutpoints[[d]] else sc$inverse(e),
      center = sc$center, scale = sc$scale
    )
  }

  # Memberships ---------------------------------------------------------------
  am <- archetypes
  M <- matrix(NA_real_, n, nrow(am),
              dimnames = list(as.character(data[[id]]), am$archetype))
  strict <- matrix(FALSE, n, nrow(am), dimnames = list(NULL, am$archetype))
  zc <- Z[complete, , drop = FALSE]
  for (k in seq_len(nrow(am))) {
    if (am$fallback[k]) {
      rules <- stats::setNames(rep("mid", 3), dims)
      constrained <- dims
    } else {
      rules <- vapply(dims, function(d) am[[d]][k], character(1))
      constrained <- dims[rules != "low|mid|high"]
    }
    if (length(constrained) == 0L) {
      M[complete, k] <- 1
      strict[complete, k] <- TRUE
      next
    }
    parts <- vapply(constrained, function(d) {
      iv <- bands_to_interval(strsplit(rules[[d]], "|", fixed = TRUE)[[1]], edges[[d]])
      interval_membership(zc[, d], iv[1], iv[2], sharpness)
    }, numeric(nrow(zc)))
    parts <- matrix(parts, nrow = nrow(zc))
    M[complete, k] <- exp(rowMeans(log(pmax(parts, 1e-12))))
    inside <- vapply(constrained, function(d) {
      lv <- strsplit(rules[[d]], "|", fixed = TRUE)[[1]]
      L[complete, d] %in% lv
    }, logical(nrow(zc)))
    strict[complete, k] <- rowSums(matrix(inside, nrow = nrow(zc))) == length(constrained)
  }

  # Assignment -----------------------------------------------------------------
  fb <- which(am$fallback)
  ord <- order(am$priority)
  assigned <- rep(NA_integer_, n)
  for (i in which(complete)) {
    if (method == "rules") {
      hit <- ord[strict[i, ord] & !am$fallback[ord]]
      assigned[i] <- if (length(hit) > 0L) hit[1] else fb
    } else {
      assigned[i] <- which.max(M[i, ])
    }
  }
  confidence <- runner_up <- runner_m <- rep(NA, n)
  for (i in which(complete)) {
    confidence[i] <- M[i, assigned[i]]
    others <- M[i, ]
    others[assigned[i]] <- -Inf
    j <- which.max(others)
    runner_up[i] <- am$archetype[j]
    runner_m[i] <- M[i, j]
  }

  vel <- X[, "velocity"]
  int <- X[, "intensity"]
  fri <- X[, "friction"]
  pace <- ifelse(vel > 0, int / vel, NA_real_)
  friction_rate <- ifelse(int > 0, 100 * fri / int, NA_real_)
  outlier <- rep(NA, n)
  for (i in which(complete)) {
    outlier[i] <- any(vapply(dims, function(d) {
      X[i, d] < reference[[d]]$fence_low || X[i, d] > reference[[d]]$fence_high
    }, logical(1)))
  }

  out <- data
  out$archetype <- factor(am$archetype[assigned], levels = am$archetype)
  out$confidence <- as.numeric(confidence)
  out$runner_up <- as.character(runner_up)
  out$runner_up_membership <- as.numeric(runner_m)
  for (d in dims) out[[paste0("z_", d)]] <- Z[, d]
  for (d in dims) out[[paste0("pct_", d)]] <- P[, d]
  for (d in dims) out[[paste0("level_", d)]] <- factor(L[, d], levels = c("low", "mid", "high"))
  out$pace <- pace
  out$friction_rate <- friction_rate
  out$outlier <- as.logical(outlier)

  reference$pace <- summarise_ref(pace[complete])
  reference$friction_rate <- summarise_ref(friction_rate[complete])

  attr(out, "q2q") <- list(
    id = id, columns = cols, archetypes = am, lexicon = lexicon,
    domain = lexicon$domain, method = method, scaling = scaling,
    transform = transform, threshold = thr, cutpoints = cutpoints,
    sharpness = sharpness, reference = reference, membership = M,
    n_complete = n_ok
  )
  class(out) <- c("q2q_profile", "tbl_df", "tbl", "data.frame")
  out
}

#' @noRd
summarise_ref <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    return(list(n = 0L, median = NA_real_, mean = NA_real_, q1 = NA_real_, q3 = NA_real_))
  }
  list(n = length(x), median = stats::median(x), mean = mean(x),
       q1 = stats::quantile(x, 0.25, names = FALSE),
       q3 = stats::quantile(x, 0.75, names = FALSE))
}

#' @noRd
profile_columns <- function() {
  d <- q2q_dimensions()
  c("archetype", "confidence", "runner_up", "runner_up_membership",
    paste0("z_", d), paste0("pct_", d), paste0("level_", d),
    "pace", "friction_rate", "outlier")
}

#' @noRd
strip_profile <- function(x) {
  x <- tibble::as_tibble(unclass_profile(x))
  x[setdiff(names(x), profile_columns())]
}

#' @noRd
unclass_profile <- function(x) {
  attr(x, "q2q") <- NULL
  class(x) <- setdiff(class(x), "q2q_profile")
  x
}

#' @noRd
resolve_threshold <- function(threshold, dims, call = rlang::caller_env()) {
  if (!is.numeric(threshold) || anyNA(threshold) || any(threshold <= 0)) {
    q2q_abort("{.arg threshold} must contain positive numbers.",
              class = "quant2qual_value_error", call = call)
  }
  if (length(threshold) == 1L && is.null(names(threshold))) {
    return(stats::setNames(rep(threshold, 3), dims))
  }
  if (is.null(names(threshold)) || !all(names(threshold) %in% dims)) {
    q2q_abort("A vector {.arg threshold} must be named with {.val {dims}}.",
              class = "quant2qual_value_error", call = call)
  }
  out <- stats::setNames(rep(0.5, 3), dims)
  out[names(threshold)] <- threshold
  out
}

#' @noRd
check_cutpoints <- function(cutpoints, dims, call = rlang::caller_env()) {
  if (is.null(cutpoints)) {
    return(list())
  }
  if (!is.list(cutpoints) || is.null(names(cutpoints)) ||
      !all(names(cutpoints) %in% dims)) {
    q2q_abort("{.arg cutpoints} must be a list named with any of {.val {dims}}.",
              class = "quant2qual_value_error", call = call)
  }
  for (d in names(cutpoints)) {
    cp <- cutpoints[[d]]
    if (!is.numeric(cp) || length(cp) != 2L || anyNA(cp) || cp[1] >= cp[2]) {
      q2q_abort("{.code cutpoints${d}} must be two increasing numbers (low/mid edge, mid/high edge).",
                class = "quant2qual_value_error", call = call)
    }
  }
  cutpoints
}

# ---- methods ----------------------------------------------------------------------------

#' Summarise archetypes in a profile
#'
#' One row per archetype: how many cases it holds, its share of the
#' classified cohort, the mean confidence and the median raw value and mean
#' standardised score of each dimension.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#'
#' @return A [tibble][tibble::tibble] with columns `archetype`, `n`,
#'   `share`, `mean_confidence`, `median_<dimension>` and `mean_z_<dimension>`.
#' @export
#' @family archetype engine
#' @examples
#' archetype_summary(identify_archetype(mock_behavioral_data(200)))
archetype_summary <- function(profile) {
  check_profile(profile)
  meta <- attr(profile, "q2q")
  cols <- meta$columns
  d <- unclass_profile(profile)
  d <- d[!is.na(d$archetype), ]
  d$.velocity <- d[[cols[["velocity"]]]]
  d$.intensity <- d[[cols[["intensity"]]]]
  d$.friction <- d[[cols[["friction"]]]]
  out <- d |>
    dplyr::group_by(.data$archetype, .drop = FALSE) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_confidence = if (dplyr::n() > 0) mean(.data$confidence) else NA_real_,
      median_velocity = if (dplyr::n() > 0) stats::median(.data$.velocity) else NA_real_,
      median_intensity = if (dplyr::n() > 0) stats::median(.data$.intensity) else NA_real_,
      median_friction = if (dplyr::n() > 0) stats::median(.data$.friction) else NA_real_,
      mean_z_velocity = if (dplyr::n() > 0) mean(.data$z_velocity) else NA_real_,
      mean_z_intensity = if (dplyr::n() > 0) mean(.data$z_intensity) else NA_real_,
      mean_z_friction = if (dplyr::n() > 0) mean(.data$z_friction) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(share = .data$n / sum(.data$n), .after = "n")
  tibble::as_tibble(out)
}

#' @export
summary.q2q_profile <- function(object, ...) {
  archetype_summary(object)
}

#' @export
print.q2q_profile <- function(x, ..., n = 6) {
  meta <- attr(x, "q2q")
  if (is.null(meta)) {
    print(tibble::as_tibble(unclass_profile(x)), n = n, ...)
    return(invisible(x))
  }
  cli_print({
    s <- archetype_summary(x)
    cli::cli_text("{.strong quant2qual profile}: {nrow(x)} case{?s}, domain {.val {meta$domain}}, ",
                  "{meta$method} assignment on {meta$scaling} scores")
    w <- max(nchar(as.character(s$archetype)))
    for (i in seq_len(nrow(s))) {
      bar <- strrep("\u2588", round(20 * s$share[i]))
      lab <- formatC(as.character(s$archetype[i]), width = -w)
      cli::cli_verbatim(sprintf("  %s %4d (%5.1f%%) %s", lab, s$n[i], 100 * s$share[i], bar))
    }
  })
  print(tibble::as_tibble(unclass_profile(x)), n = n, ...)
  invisible(x)
}

#' @export
`[.q2q_profile` <- function(x, i, j, ..., drop = FALSE) {
  meta <- attr(x, "q2q")
  out <- NextMethod()
  if (is.data.frame(out) && all(profile_columns() %in% names(out))) {
    attr(out, "q2q") <- meta
    class(out) <- unique(c("q2q_profile", class(out)))
  } else if (is.data.frame(out)) {
    out <- unclass_profile(out)
  }
  out
}

#' Convert a profile to a plain data frame
#'
#' @param x A `q2q_profile`.
#' @param row.names,optional,... Passed to [as.data.frame()].
#' @return A data frame without the `q2q` metadata.
#' @export
#' @examples
#' p <- identify_archetype(mock_behavioral_data(50))
#' head(as.data.frame(p))
as.data.frame.q2q_profile <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(tibble::as_tibble(unclass_profile(x)), row.names = row.names,
                optional = optional, ...)
}

#' Archetype membership matrix
#'
#' Returns the fuzzy membership of every case in every archetype, the basis
#' of `confidence` and `runner_up`.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @return A tibble with the id column and one numeric column per archetype.
#' @export
#' @family archetype engine
#' @examples
#' p <- identify_archetype(mock_behavioral_data(50))
#' archetype_membership(p)
archetype_membership <- function(profile) {
  check_profile(profile)
  meta <- attr(profile, "q2q")
  ids <- profile[[meta$id]]
  key <- as.character(ids)
  if (!all(key %in% rownames(meta$membership))) {
    q2q_abort("Some cases are not in the stored membership matrix; re-run {.fn identify_archetype}.",
              class = "quant2qual_value_error")
  }
  M <- meta$membership[key, , drop = FALSE]
  out <- tibble::as_tibble(as.data.frame(M, check.names = FALSE, row.names = NULL))
  tibble::add_column(out, !!meta$id := ids, .before = 1)
}
