# ---------------------------------------------------------------------------
# Internal helpers: condition signalling, argument checks and the small
# language toolkit (numbers, ordinals, percentiles, ratios) that the narrative
# engine stitches into prose. Nothing here is exported.
# ---------------------------------------------------------------------------

# ---- conditions -------------------------------------------------------------

#' Signal a classed quant2qual error
#' @noRd
q2q_abort <- function(message, ..., class = NULL, call = rlang::caller_env(),
                      .envir = parent.frame()) {
  cli::cli_abort(message, ..., class = c(class, "quant2qual_error"),
                 call = call, .envir = .envir)
}

#' Signal a classed quant2qual warning
#' @noRd
q2q_warn <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_warn(message, ..., class = c(class, "quant2qual_warning"),
                .envir = .envir)
}

# ---- argument checks --------------------------------------------------------

#' @noRd
check_data_frame <- function(x, arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!is.data.frame(x)) {
    q2q_abort("{.arg {arg}} must be a data frame, not {.obj_type_friendly {x}}.",
              class = "quant2qual_type_error", call = call)
  }
  if (nrow(x) == 0L) {
    q2q_abort("{.arg {arg}} has no rows.", class = "quant2qual_value_error",
              call = call)
  }
  invisible(x)
}

#' @noRd
check_string <- function(x, arg = rlang::caller_arg(x), allow_null = FALSE,
                         call = rlang::caller_env()) {
  if (allow_null && is.null(x)) {
    return(invisible(x))
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    q2q_abort("{.arg {arg}} must be a single non-empty string.",
              class = "quant2qual_type_error", call = call)
  }
  invisible(x)
}

#' @noRd
check_number <- function(x, arg = rlang::caller_arg(x), min = -Inf, max = Inf,
                         call = rlang::caller_env()) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    q2q_abort("{.arg {arg}} must be a single number.",
              class = "quant2qual_type_error", call = call)
  }
  if (x < min || x > max) {
    q2q_abort("{.arg {arg}} must be between {min} and {max}, not {x}.",
              class = "quant2qual_value_error", call = call)
  }
  invisible(x)
}

#' @noRd
check_flag <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    q2q_abort("{.arg {arg}} must be {.code TRUE} or {.code FALSE}.",
              class = "quant2qual_type_error", call = call)
  }
  invisible(x)
}

#' @noRd
check_columns <- function(data, cols, arg = "data", call = rlang::caller_env()) {
  missing <- setdiff(cols, names(data))
  if (length(missing) > 0L) {
    q2q_abort(c("{cli::qty(length(missing))}Column{?s} {.field {missing}} not found in {.arg {arg}}.",
                i = "Available columns: {.field {names(data)}}."),
              class = "quant2qual_missing_column", call = call)
  }
  invisible(data)
}

#' @noRd
check_numeric_columns <- function(data, cols, call = rlang::caller_env()) {
  bad <- cols[!vapply(data[cols], is.numeric, logical(1))]
  if (length(bad) > 0L) {
    types <- vapply(data[bad], function(v) class(v)[1], character(1))
    q2q_abort(c("Behavioral dimensions must be numeric.",
                x = "{.field {bad}} {?is/are} {.cls {types}}."),
              class = "quant2qual_type_error", call = call)
  }
  invisible(data)
}

#' @noRd
check_profile <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!inherits(x, "q2q_profile") || is.null(attr(x, "q2q"))) {
    q2q_abort(c("{.arg {arg}} must be the output of {.fn identify_archetype}.",
                i = "Row-subsetting with {.code [} keeps the class; most {.pkg dplyr} verbs drop it."),
              class = "quant2qual_type_error", call = call)
  }
  invisible(x)
}

# ---- language toolkit -------------------------------------------------------

#' Deterministic variant choice: the same key always picks the same phrasing,
#' different keys spread across the options. No global RNG state is touched.
#' @noRd
pick_variant <- function(options, key) {
  if (length(options) <= 1L) {
    return(options[1])
  }
  code <- utf8ToInt(enc2utf8(as.character(key)))
  h <- sum(code * seq_along(code) * 31L) %% length(options)
  options[[h + 1L]]
}

#' Format a number for prose: thousands separators, sensible decimals
#' @noRd
fmt_num <- function(x, digits = 1) {
  vapply(x, function(v) {
    if (is.na(v)) {
      return("NA")
    }
    d <- if (abs(v - round(v)) < 1e-9) 0 else digits
    formatC(round(v, d), format = "f", digits = d, big.mark = ",")
  }, character(1))
}

#' @noRd
ordinal <- function(n) {
  n <- as.integer(round(n))
  suffix <- ifelse(n %% 100L %in% 11:13, "th",
                   c("th", "st", "nd", "rd", rep("th", 6))[n %% 10L + 1L])
  paste0(n, suffix)
}

#' Percentile rank (0-1) -> prose
#' @noRd
percentile_phrase <- function(p, key = "") {
  if (is.na(p)) {
    return("at an undetermined position in the cohort")
  }
  top <- max(1L, as.integer(ceiling((1 - p) * 100)))
  bottom <- max(1L, as.integer(ceiling(p * 100)))
  if (p >= 0.80) {
    pick_variant(c(sprintf("within the upper %s percentile", ordinal(top)),
                   sprintf("in the top %d%% of the cohort", top),
                   sprintf("above %d%% of the cohort", as.integer(floor(p * 100)))),
                 key)
  } else if (p <= 0.20) {
    pick_variant(c(sprintf("within the lowest %d%% of the cohort", bottom),
                   sprintf("in the bottom %d%% of the cohort", bottom),
                   sprintf("below %d%% of the cohort", 100L - bottom)),
                 key)
  } else {
    sprintf("at the %s percentile", ordinal(p * 100))
  }
}

#' Standardised score -> intensity adverb
#' @noRd
magnitude_word <- function(z) {
  a <- abs(z)
  if (is.na(a)) "indeterminately"
  else if (a >= 2.5) "far"
  else if (a >= 1.5) "markedly"
  else if (a >= 1.0) "substantially"
  else if (a >= 0.5) "moderately"
  else "only marginally"
}

#' @noRd
times_word <- function(r) {
  if (r < 2.25) "roughly twice"
  else if (r < 2.75) sprintf("about %s times", fmt_num(r, 1))
  else if (r < 3.5) "roughly three times"
  else if (r < 4.5) "roughly four times"
  else sprintf("about %s times", fmt_num(r, 0))
}

#' A value relative to a cohort reference -> prose
#' @noRd
ratio_phrase <- function(x, ref, ref_label = "the cohort median") {
  if (is.na(x) || is.na(ref)) {
    return(paste("not comparable with", ref_label))
  }
  if (ref == 0) {
    return(if (x == 0) paste("in line with", ref_label, "of zero")
           else paste("against", ref_label, "of zero"))
  }
  r <- x / ref
  if (r >= 1.8) paste(times_word(r), ref_label)
  else if (r >= 1.12) sprintf("about %d%% above %s", as.integer(round((r - 1) * 100)), ref_label)
  else if (r > 0.88) paste("close to", ref_label)
  else if (r > 0.42) sprintf("about %d%% below %s", as.integer(round((1 - r) * 100)), ref_label)
  else if (r > 0.15) {
    fr <- c(`roughly a third of` = 1 / 3, `about a quarter of` = 1 / 4,
            `about a fifth of` = 1 / 5, `about a sixth of` = 1 / 6)
    paste(names(fr)[which.min(abs(fr - r))], ref_label)
  }
  else if (r > 0) paste("a small fraction of", ref_label)
  else paste("far below", ref_label)
}

#' "a", "a and b", "a, b and c"
#' @noRd
collapse_and <- function(x, last = " and ") {
  x <- as.character(x)
  n <- length(x)
  if (n == 0L) return("")
  if (n == 1L) return(x)
  paste0(paste(x[-n], collapse = ", "), last, x[n])
}

#' @noRd
capitalise <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#' Replace {placeholders} in a template with values from a named list
#' @noRd
interpolate <- function(template, values) {
  out <- template
  for (nm in names(values)) {
    out <- gsub(paste0("{", nm, "}"), as.character(values[[nm]]), out, fixed = TRUE)
  }
  out
}

#' Unit in the right number: "1 error", "2 errors", "1 activity"
#' @noRd
unit_for <- function(value, unit) {
  if (is.na(value) || value != 1) {
    return(unit)
  }
  words <- strsplit(unit, " ", fixed = TRUE)[[1]]
  last <- words[length(words)]
  last <- if (grepl("ies$", last)) sub("ies$", "y", last) else sub("s$", "", last)
  paste(c(words[-length(words)], last), collapse = " ")
}

#' "was" or "were" for a dimension noun
#' @noRd
be <- function(dim_lexicon) if (isTRUE(dim_lexicon$plural)) "were" else "was"

#' Print cli output to stdout, so print methods behave like print methods
#' @noRd
cli_print <- function(expr) {
  lines <- cli::cli_fmt(expr)
  cat(lines, sep = "\n")
  invisible(lines)
}

#' Indefinite article for a noun phrase
#' @noRd
article <- function(word) {
  if (grepl("^[aeiouAEIOU]", word)) "an" else "a"
}

#' Escape characters that pandoc Markdown would otherwise interpret
#' @noRd
md_escape <- function(x) {
  gsub("([*_`<>\\\\])", "\\\\\\1", x)
}
