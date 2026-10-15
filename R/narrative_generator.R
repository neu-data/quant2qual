# ---------------------------------------------------------------------------
# The narrative generator.
#
# Prose is assembled, not generated: every sentence comes from a template
# whose slots are filled from the case's own statistics, and whose phrasing
# variant is chosen deterministically from the case id. The same profile
# therefore always yields the same text, different cases do not read like
# copies of one another, and nothing leaves the machine.
#
# A case narrative has five parts, in the order a clinical or UX case study
# is read:
#
#   presentation   - the headline and the archetype it aligns with
#   evidence       - one sentence per dimension, most salient first, stitched
#                    with connectors that follow the direction of the data,
#                    then rates and outliers
#   interpretation - what the signature means in this domain
#   implications   - actions or considerations
#   confidence     - how cleanly the case fits, and what it was compared with
# ---------------------------------------------------------------------------

#' Generate a qualitative case narrative
#'
#' Translates one or more classified profiles into case-study prose that
#' cites its own evidence (raw values, percentiles, cohort ratios, outliers)
#' and states its own uncertainty.
#'
#' @section Styles:
#' * `"clinical"` -- formal third-person case study with standardised
#'   scores, a methodological note and "Considerations".
#' * `"ux"` -- practitioner register without z-scores, ending in
#'   "Recommended actions".
#' * `"brief"` -- a single analytical paragraph, for tables, dashboards or
#'   e-mail summaries.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param id One or more case identifiers (values of the profile's id
#'   column). Use [select_cases()] to pick exemplars automatically.
#' @param style `"clinical"`, `"ux"` or `"brief"`; see *Styles*.
#' @param lexicon Optional [q2q_lexicon()] overriding the one stored in the
#'   profile.
#'
#' @return For a single `id`, an object of class `q2q_narrative`: a list with
#'   `id`, `archetype`, `confidence`, `style`, `title`, `subtitle`,
#'   `sections` (named list of paragraphs; `implications` is a character
#'   vector) and `evidence` (a tibble of the statistics cited). For several
#'   ids, a `q2q_narrative_set` (a list of narratives). Use [format()] or
#'   [as.character()] for Markdown and [print()] for the console.
#' @export
#' @family narrative
#' @examples
#' profile <- identify_archetype(mock_behavioral_data(200))
#'
#' generate_narrative(profile, id = 104)
#' generate_narrative(profile, id = 104, style = "ux")
#' generate_narrative(profile, id = 104, style = "brief")
#'
#' # Markdown for a report
#' cat(format(generate_narrative(profile, id = 104)))
#'
#' # One exemplar per archetype
#' generate_narrative(profile, id = select_cases(profile), style = "brief")
generate_narrative <- function(profile, id, style = c("clinical", "ux", "brief"),
                               lexicon = NULL) {
  check_profile(profile)
  style <- rlang::arg_match(style)
  meta <- attr(profile, "q2q")
  lx <- lexicon %||% meta$lexicon
  if (!inherits(lx, "q2q_lexicon")) {
    q2q_abort("{.arg lexicon} must be built with {.fn q2q_lexicon}.",
              class = "quant2qual_type_error")
  }
  if (missing(id) || is.null(id) || length(id) == 0L) {
    q2q_abort(c("Supply at least one case {.arg id}.",
                i = "{.code select_cases(profile)} picks one exemplar per archetype."),
              class = "quant2qual_value_error")
  }
  ids <- profile[[meta$id]]
  unknown <- id[!as.character(id) %in% as.character(ids)]
  if (length(unknown) > 0L) {
    q2q_abort(c("{cli::qty(length(unknown))}Case{?s} {.val {unknown}} not found in {.field {meta$id}}.",
                i = "The profile holds {length(ids)} cases, e.g. {.val {utils::head(ids, 3)}}."),
              class = "quant2qual_value_error")
  }
  out <- lapply(id, function(one) {
    row <- which(as.character(ids) == as.character(one))[1]
    build_narrative(profile, row, style, lx, meta)
  })
  if (length(out) == 1L) {
    return(out[[1]])
  }
  structure(out, class = "q2q_narrative_set")
}

#' Pick cases to narrate
#'
#' Chooses exemplar cases from a profile, so that a report can illustrate
#' every archetype without the analyst picking cases by hand.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param n Number of cases per archetype.
#' @param strategy
#'   * `"representative"` -- highest classification confidence: the clearest
#'     examples of each signature.
#'   * `"extreme"` -- largest distance from the cohort centre (sum of
#'     absolute standardised scores): the most pronounced examples.
#'   * `"borderline"` -- lowest confidence: the cases that sit between
#'     signatures, useful for auditing the matrix.
#' @param archetypes Optional subset of archetype names.
#'
#' @return A vector of case identifiers, ordered by archetype.
#' @export
#' @family narrative
#' @examples
#' profile <- identify_archetype(mock_behavioral_data(200))
#' select_cases(profile)
#' select_cases(profile, n = 2, strategy = "extreme")
select_cases <- function(profile, n = 1, strategy = c("representative", "extreme", "borderline"),
                         archetypes = NULL) {
  check_profile(profile)
  check_number(n, min = 1)
  strategy <- rlang::arg_match(strategy)
  meta <- attr(profile, "q2q")
  d <- tibble::as_tibble(unclass_profile(profile))
  d <- d[!is.na(d$archetype), ]
  if (!is.null(archetypes)) {
    bad <- setdiff(archetypes, levels(d$archetype))
    if (length(bad) > 0L) {
      q2q_abort("Unknown archetype{?s} {.val {bad}}.", class = "quant2qual_value_error")
    }
    d <- d[d$archetype %in% archetypes, ]
  }
  d$.score <- switch(strategy,
    representative = d$confidence,
    extreme = abs(d$z_velocity) + abs(d$z_intensity) + abs(d$z_friction),
    borderline = -d$confidence
  )
  picked <- d |>
    dplyr::group_by(.data$archetype) |>
    dplyr::slice_max(.data$.score, n = as.integer(n), with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$archetype, dplyr::desc(.data$.score))
  picked[[meta$id]]
}

# ---- case facts ---------------------------------------------------------------

#' Everything the templates need about one case
#' @noRd
case_facts <- function(profile, row, lx, meta) {
  dims <- q2q_dimensions()
  am <- meta$archetypes
  arch <- as.character(profile$archetype[row])
  id <- profile[[meta$id]][row]
  if (is.na(arch)) {
    q2q_abort("Case {.val {id}} has missing values and was not classified.",
              class = "quant2qual_value_error")
  }
  a <- am[am$archetype == arch, ]
  ev <- purrr::map_dfr(dims, function(d) {
    ref <- meta$reference[[d]]
    lexd <- lx$dimensions[[d]]
    tibble::tibble(
      dimension = d,
      noun = lexd$noun,
      unit = lexd$unit,
      value = profile[[meta$columns[[d]]]][row],
      median = ref$median,
      z = profile[[paste0("z_", d)]][row],
      percentile = profile[[paste0("pct_", d)]][row],
      level = as.character(profile[[paste0("level_", d)]][row]),
      outlier = profile[[meta$columns[[d]]]][row] < ref$fence_low ||
        profile[[meta$columns[[d]]]][row] > ref$fence_high
    )
  })
  ev <- ev[order(-abs(ev$z)), ]
  list(
    id = id, key = paste0(id, "|", arch), archetype = arch, a = a,
    evidence = ev,
    pace = profile$pace[row], pace_median = meta$reference$pace$median,
    rate = profile$friction_rate[row], rate_median = meta$reference$friction_rate$median,
    confidence = profile$confidence[row],
    runner_up = profile$runner_up[row],
    runner_up_membership = profile$runner_up_membership[row],
    n = meta$n_complete, method = meta$method, scaling = meta$scaling
  )
}

#' @noRd
subject_values <- function(lx) {
  list(subject = lx$subject, Subject = capitalise(lx$subject),
       subject_short = lx$subject_short, subject_plural = lx$subject_plural)
}

#' @noRd
confidence_label <- function(x) {
  if (is.na(x)) "undetermined" else if (x >= 0.75) "high" else if (x >= 0.5) "moderate" else "low"
}

# ---- sentence builders ----------------------------------------------------------

#' One evidence sentence for one dimension (no leading capital, no connector)
#' @noRd
evidence_sentence <- function(e, f, lx, show_stats) {
  key <- paste0(f$key, e$dimension)
  reading <- pick_variant(lx$dimensions[[e$dimension]]$readings[[e$level]], key)
  pct <- percentile_phrase(e$percentile / 100, key)
  val <- paste(fmt_num(e$value), unit_for(e$value, e$unit))
  was <- be(lx$dimensions[[e$dimension]])
  zt <- if (show_stats) sprintf("; z = %+.2f", e$z) else ""
  ratio <- ratio_phrase(e$value, e$median)
  dir <- if (e$z >= 0) "above" else "below"
  if (e$level == "mid") {
    tpl <- pick_variant(c(
      "{noun} {was} unremarkable at {val}, {ratio}{zt_paren}, reflecting {reading}",
      "{noun} ({val}{zt}) sat {pct}, in line with {reading}"
    ), key)
  } else {
    tpl <- pick_variant(c(
      "{noun} sat {pct} ({val} against a cohort median of {median}{zt}), indicating {reading}",
      "{noun} {was} {magnitude} {dir} the cohort norm at {val} ({ratio}{zt}), consistent with {reading}",
      "at {val}, {noun} placed {subject} {pct} ({ratio}{zt}), a pattern that points to {reading}"
    ), key)
  }
  interpolate(tpl, list(
    noun = e$noun, val = val, ratio = ratio, reading = reading, pct = pct,
    median = paste(fmt_num(e$median), e$unit), zt = zt,
    zt_paren = if (show_stats) sprintf(" (z = %+.2f)", e$z) else "",
    magnitude = magnitude_word(e$z), dir = dir, subject = lx$subject, was = was
  ))
}

#' Connector between consecutive evidence sentences
#' @noRd
connector <- function(prev, cur, key, i = 2L) {
  if (cur$level == "mid") {
    if (prev$level != "mid") {
      return("Meanwhile, ")
    }
    return(if (i <= 2L) "Similarly, " else "Likewise, ")
  }
  same <- prev$level != "mid" && sign(prev$z) == sign(cur$z)
  if (same && cur$dimension == "friction" && cur$level == "high") {
    return("Compounding this, ")
  }
  if (same) {
    return(pick_variant(c("Alongside this, ", "In parallel, ", "Likewise, "), key))
  }
  if (prev$level == "mid") {
    return(pick_variant(c("Against that backdrop, ", "Of greater note, "), key))
  }
  pick_variant(c("By contrast, ", "Conversely, ", "In contrast, "), key)
}

#' @noRd
evidence_paragraph <- function(f, lx, show_stats, max_sentences = 3L) {
  ev <- f$evidence
  k <- min(nrow(ev), max_sentences)
  sentences <- character(k)
  for (i in seq_len(k)) {
    body <- evidence_sentence(ev[i, ], f, lx, show_stats)
    if (i == 1L) {
      sentences[i] <- paste0(capitalise(body), ".")
    } else {
      sentences[i] <- paste0(connector(ev[i - 1L, ], ev[i, ], paste0(f$key, i), i), body, ".")
    }
  }
  sentences
}

#' @noRd
rates_sentence <- function(f, lx) {
  parts <- character()
  if (!is.null(lx$pace) && is.finite(f$pace) && is.finite(f$pace_median)) {
    parts <- c(parts, sprintf("%s %s of %s %s (%s)", article(lx$pace$noun), lx$pace$noun,
                              fmt_num(f$pace, 1), lx$pace$unit,
                              ratio_phrase(f$pace, f$pace_median)))
  }
  if (!is.null(lx$rate) && is.finite(f$rate) && is.finite(f$rate_median)) {
    parts <- c(parts, sprintf("%s %s of %s %s (%s)", article(lx$rate$noun), lx$rate$noun,
                              fmt_num(f$rate, 1), lx$rate$unit,
                              ratio_phrase(f$rate, f$rate_median)))
  }
  if (length(parts) == 0L) {
    return(character())
  }
  lead <- pick_variant(c("Expressed as rates, the record shows ",
                         "Normalising for exposure, the record shows ",
                         "In relative terms, the record shows "), f$key)
  paste0(lead, collapse_and(parts), ".")
}

#' @noRd
outlier_sentence <- function(f, lx) {
  ev <- f$evidence[f$evidence$outlier %in% TRUE, ]
  if (nrow(ev) == 0L) {
    return(character())
  }
  nouns <- collapse_and(ev$noun)
  sprintf(paste("On %s, %s lies beyond the Tukey fence of the cohort distribution and is a",
                "statistical outlier; values of this kind merit verification against the raw",
                "logs before interpretation."), nouns, lx$subject)
}

#' @noRd
alignment_sentence <- function(f, lx) {
  a <- f$a
  persona <- a$persona
  if (nzchar(persona)) {
    persona <- interpolate(persona, subject_values(lx))
    persona <- paste0(tolower(substring(persona, 1, 1)), substring(persona, 2))
    persona <- sub("\\.$", "", persona)
  }
  lead <- pick_variant(c(
    "The overall pattern aligns with the {A} archetype",
    "Taken as a whole, the record is most consistent with the {A} archetype",
    "The profile maps onto the {A} archetype"
  ), f$key)
  s <- interpolate(lead, list(A = a$archetype))
  if (nzchar(persona)) paste0(s, " \u2014 ", persona, ".") else paste0(s, ".")
}

#' @noRd
confidence_paragraph <- function(f, lx, style) {
  lab <- confidence_label(f$confidence)
  A <- f$archetype
  R <- f$runner_up
  cm <- sprintf("%.2f", f$confidence)
  rm <- sprintf("%.2f", f$runner_up_membership)
  is_fallback <- isTRUE(f$a$fallback)
  s <- switch(lab,
    high = sprintf("Classification confidence is high (membership %s): the record is a clear example of the %s signature.", cm, A),
    moderate = sprintf("Classification confidence is moderate (membership %s).", cm),
    low = if (is_fallback && f$method == "rules") {
      sprintf(paste("Classification confidence is low (membership %s): the record meets none of the",
                    "defined signatures exactly and was assigned to %s by default, so the label should be",
                    "treated as provisional."), cm, A)
    } else {
      sprintf(paste("Classification confidence is low (membership %s): the record does not fit any defined",
                    "signature cleanly, and the %s label should be treated as provisional."), cm, A)
    },
    sprintf("Classification confidence could not be determined.")
  )
  if (!is.na(f$runner_up_membership) && f$runner_up_membership >= 0.4) {
    s <- paste(s, sprintf(paste("The profile also shows partial affinity with the %s archetype",
                                "(membership %s), and the narrative should be read with that overlap in mind."),
                          R, rm))
  }
  if (style == "clinical") {
    s <- paste(s, sprintf(paste("All comparisons are relative to the present cohort of %s %s (%s scaling);",
                                "the same raw values could read differently against another reference group."),
                          fmt_num(f$n), lx$subject_plural, f$scaling))
  }
  s
}

# ---- assembly --------------------------------------------------------------------

#' @noRd
build_narrative <- function(profile, row, style, lx, meta) {
  f <- case_facts(profile, row, lx, meta)
  sv <- subject_values(lx)
  a <- f$a
  headline <- interpolate(a$headline, sv)
  title <- sprintf("%s ID: #%s", lx$case_label, f$id)
  subtitle <- sprintf("%s \u2014 %s", f$archetype, capitalise(a$theme))

  show_stats <- style == "clinical"
  evidence <- evidence_paragraph(f, lx, show_stats)
  interpretation <- interpolate(a$interpretation, sv)
  recs <- a$recommendations[[1]]
  conf <- confidence_paragraph(f, lx, style)

  if (style == "brief") {
    lead <- evidence_paragraph(f, lx, FALSE, max_sentences = 2L)
    para <- paste(c(headline, alignment_sentence(f, lx), lead,
                    sprintf("Confidence: %s (%.2f).", confidence_label(f$confidence), f$confidence)),
                  collapse = " ")
    sections <- list(summary = para)
  } else {
    sections <- list(
      presentation = paste(headline, alignment_sentence(f, lx)),
      evidence = paste(c(evidence, rates_sentence(f, lx), outlier_sentence(f, lx)), collapse = " "),
      interpretation = interpretation,
      implications = recs,
      confidence = conf
    )
    if (length(recs) == 0L) sections$implications <- NULL
  }
  ev <- f$evidence
  ev$label <- vapply(ev$dimension, function(d) lx$dimensions[[d]]$label, character(1))
  structure(list(
    id = f$id, archetype = f$archetype, confidence = f$confidence,
    runner_up = f$runner_up, style = style, title = title, subtitle = subtitle,
    sections = sections, evidence = ev, domain = lx$domain
  ), class = "q2q_narrative")
}

#' @noRd
section_titles <- function(style) {
  list(
    presentation = "Presenting pattern",
    evidence = "Behavioral evidence",
    interpretation = "Interpretation",
    implications = if (style == "clinical") "Considerations" else "Recommended actions",
    confidence = "Confidence and caveats",
    summary = NULL
  )
}

#' Format narratives as Markdown or plain text
#'
#' @param x A `q2q_narrative`, `q2q_narrative_set` or
#'   `q2q_cohort_narrative`.
#' @param format `"markdown"` (default) or `"text"`.
#' @param heading_level Markdown heading level for the case title.
#' @param ... Unused.
#' @return A single character string.
#' @export
#' @family narrative
#' @examples
#' p <- identify_archetype(mock_behavioral_data(100))
#' n <- generate_narrative(p, id = 104)
#' cat(format(n))
#' cat(format(n, format = "text"))
format.q2q_narrative <- function(x, format = c("markdown", "text"), heading_level = 2, ...) {
  format <- rlang::arg_match(format)
  st <- section_titles(x$style)
  md <- format == "markdown"
  hd <- strrep("#", heading_level)
  out <- if (md) {
    c(paste(hd, x$title), "", sprintf("*%s \u00B7 confidence %s (%.2f)*", x$subtitle,
                                      confidence_label(x$confidence), x$confidence), "")
  } else {
    c(x$title, x$subtitle, "")
  }
  for (nm in names(x$sections)) {
    body <- x$sections[[nm]]
    ttl <- st[[nm]]
    if (nm == "implications") {
      out <- c(out, if (md) paste0("**", ttl, "**") else paste0(ttl, ":"), "",
               paste0(if (md) "- " else "  * ", md_text(body, md)), "")
    } else if (is.null(ttl)) {
      out <- c(out, md_text(body, md), "")
    } else {
      lead <- if (md) paste0("**", ttl, ".** ") else paste0(ttl, ". ")
      out <- c(out, paste0(lead, md_text(body, md)), "")
    }
  }
  paste(out, collapse = "\n")
}

#' @noRd
md_text <- function(x, md) if (md) md_escape(x) else x

#' @rdname format.q2q_narrative
#' @export
format.q2q_narrative_set <- function(x, format = c("markdown", "text"), heading_level = 2, ...) {
  paste(vapply(x, format, character(1), format = format, heading_level = heading_level),
        collapse = "\n\n")
}

#' @export
as.character.q2q_narrative <- function(x, ...) format(x, ...)

#' @export
as.character.q2q_narrative_set <- function(x, ...) format(x, ...)

#' @export
print.q2q_narrative <- function(x, ...) {
  cli_print({
    st <- section_titles(x$style)
    cli::cli_h1(x$title)
    sub <- x$subtitle
    cl <- sprintf("confidence %s (%.2f)", confidence_label(x$confidence), x$confidence)
    cli::cli_text("{.emph {sub}} \u00B7 {.field {cl}}")
    for (nm in names(x$sections)) {
      body <- x$sections[[nm]]
      ttl <- st[[nm]]
      if (!is.null(ttl)) cli::cli_h3(ttl)
      if (nm == "implications") {
        cli::cli_ul()
        for (b in body) cli::cli_li("{b}")
        cli::cli_end()
      } else {
        cli::cli_text("{body}")
      }
    }
  })
  invisible(x)
}

#' @export
print.q2q_narrative_set <- function(x, ...) {
  for (n in x) print(n)
  invisible(x)
}

#' @export
`[.q2q_narrative_set` <- function(x, i) {
  structure(unclass(x)[i], class = "q2q_narrative_set")
}

# ---- cohort narrative -----------------------------------------------------------------

#' Generate a cohort-level narrative
#'
#' Writes the synthesis that precedes individual cases in a report: how the
#' cohort divides across archetypes, how much of it is friction-led, how the
#' three dimensions relate to one another, how concentrated friction is,
#' how many cases are outliers, and how confidently the cohort was
#' classified -- followed by a short portrait of every archetype.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param lexicon Optional [q2q_lexicon()] overriding the stored one.
#'
#' @return An object of class `q2q_cohort_narrative`: a list with `title`,
#'   `sections` (named paragraphs), `portraits` (one paragraph per
#'   archetype present) and `summary` (the [archetype_summary()] table).
#' @export
#' @family narrative
#' @examples
#' profile <- identify_archetype(mock_behavioral_data(200))
#' generate_cohort_narrative(profile)
generate_cohort_narrative <- function(profile, lexicon = NULL) {
  check_profile(profile)
  meta <- attr(profile, "q2q")
  lx <- lexicon %||% meta$lexicon
  am <- meta$archetypes
  cols <- meta$columns
  dn <- function(d) lx$dimensions[[d]]$noun
  du <- function(d) lx$dimensions[[d]]$unit
  s <- archetype_summary(profile)
  s <- s[s$n > 0, ]
  s <- s[order(-s$n), ]
  d <- tibble::as_tibble(unclass_profile(profile))
  d <- d[!is.na(d$archetype), ]
  n_all <- nrow(profile)
  n_ok <- nrow(d)
  pct <- function(x) sprintf("%.0f%%", 100 * x)

  # overview
  ov <- sprintf("The cohort comprised %s %s%s.", fmt_num(n_all), lx$subject_plural,
                if (n_ok < n_all) sprintf(", of whom %s had complete records and were classified", fmt_num(n_ok)) else "")
  top <- s[1, ]
  ov <- paste(ov, sprintf(paste("Their behavior resolved into %d recognisable signatures. The most common was",
                                "the %s profile (%s of the cohort), characterised by %s."),
                          nrow(s), top$archetype, pct(top$share),
                          am$theme[am$archetype == top$archetype]))
  if (nrow(s) > 1L) {
    rest <- sprintf("%s (%s)", s$archetype[-1], pct(s$share[-1]))
    ov <- paste(ov, sprintf("It was followed by %s.", collapse_and(rest)))
  }

  # friction-led share
  fr_names <- am$archetype[!am$fallback & am$friction == "high"]
  fr_share <- sum(s$share[s$archetype %in% fr_names])
  friction <- if (length(fr_names) > 0L && fr_share > 0) {
    sprintf(paste("Taken together, %s of %s fall into friction-led profiles (%s): the group in which",
                  "design or support intervention is most likely to change outcomes."),
            pct(fr_share), lx$subject_plural, collapse_and(fr_names[fr_names %in% s$archetype]))
  } else {
    sprintf("No %s fell into a friction-led profile.", lx$subject_short)
  }

  # concentration of friction
  fx <- d[[cols[["friction"]]]]
  conc <- character()
  if (sum(fx) > 0) {
    top_n <- max(1L, ceiling(0.1 * length(fx)))
    share_top <- sum(sort(fx, decreasing = TRUE)[seq_len(top_n)]) / sum(fx)
    zero <- mean(fx == 0)
    conc <- sprintf(paste("The distribution of %s was uneven: the top 10%% of %s accounted for %s",
                          "of all %s, while %s recorded none at all."),
                    dn("friction"), lx$subject_plural, pct(share_top), du("friction"),
                    pct(zero))
  }

  # relationships between dimensions
  pairs <- list(c("velocity", "intensity"), c("velocity", "friction"), c("intensity", "friction"))
  rel <- vapply(pairs, function(p) {
    r <- suppressWarnings(stats::cor(d[[cols[[p[1]]]]], d[[cols[[p[2]]]]], method = "spearman"))
    if (is.na(r) || abs(r) < 0.1) return(NA_character_)
    strength <- if (abs(r) >= 0.5) "strongly" else if (abs(r) >= 0.3) "moderately" else "weakly"
    plural2 <- isTRUE(lx$dimensions[[p[2]]]$plural)
    verb <- if (r > 0) (if (plural2) "more" else "greater") else (if (plural2) "fewer" else "lower")
    lead <- if (isTRUE(lx$dimensions[[p[1]]]$plural)) "more" else "greater"
    sprintf("%s %s %s %s associated with %s %s (Spearman's rho = %.2f)",
            lead, dn(p[1]), be(lx$dimensions[[p[1]]]), strength, verb, dn(p[2]), r)
  }, character(1))
  rel <- rel[!is.na(rel)]
  relationships <- if (length(rel) == 0L) {
    "Across the cohort the three dimensions were largely independent of one another."
  } else {
    paste0("Across the cohort, ", paste(rel, collapse = "; "), ".")
  }

  # outliers and confidence
  n_out <- sum(d$outlier, na.rm = TRUE)
  low <- d[d$confidence < 0.5, ]
  conf <- sprintf("Median classification confidence was %.2f.", stats::median(d$confidence))
  if (nrow(low) > 0L) {
    pr <- paste(low$archetype, low$runner_up, sep = "|")
    common <- names(sort(table(pr), decreasing = TRUE))[1]
    ab <- strsplit(common, "|", fixed = TRUE)[[1]]
    conf <- paste(conf, sprintf(paste("%s %s (%s) were classified with low confidence and sit between",
                                      "signatures, most often between %s and %s."),
                                fmt_num(nrow(low)), if (nrow(low) == 1L) lx$subject_short else lx$subject_plural,
                                pct(nrow(low) / n_ok), ab[1], ab[2]))
  }
  if (n_out > 0L) {
    conf <- paste(conf, sprintf("%s %s (%s) lie beyond a Tukey fence on at least one dimension.",
                                fmt_num(n_out), if (n_out == 1L) lx$subject_short else lx$subject_plural,
                                pct(n_out / n_ok)))
  }

  # portraits
  portraits <- purrr::map_chr(seq_len(nrow(s)), function(i) {
    r <- s[i, ]
    a <- am[am$archetype == r$archetype, ]
    persona <- interpolate(a$persona, subject_values(lx))
    sprintf("%s (n = %s, %s). %s Median %s %s %s, %s %s %s and %s %s %s; mean confidence %.2f.",
            r$archetype, fmt_num(r$n), pct(r$share), persona,
            dn("velocity"), fmt_num(r$median_velocity), unit_for(r$median_velocity, du("velocity")),
            dn("intensity"), fmt_num(r$median_intensity), unit_for(r$median_intensity, du("intensity")),
            dn("friction"), fmt_num(r$median_friction), unit_for(r$median_friction, du("friction")),
            r$mean_confidence)
  })
  names(portraits) <- as.character(s$archetype)

  structure(list(
    title = "Cohort synthesis",
    sections = list(
      overview = ov,
      friction = paste(c(friction, conc), collapse = " "),
      relationships = relationships,
      confidence = conf
    ),
    portraits = portraits,
    summary = archetype_summary(profile),
    domain = lx$domain
  ), class = "q2q_cohort_narrative")
}

#' @rdname format.q2q_narrative
#' @export
format.q2q_cohort_narrative <- function(x, format = c("markdown", "text"), heading_level = 2, ...) {
  format <- rlang::arg_match(format)
  md <- format == "markdown"
  hd <- strrep("#", heading_level)
  out <- c(if (md) paste(hd, x$title) else x$title, "",
           unname(vapply(x$sections, md_text, character(1), md = md)), "")
  out <- paste(out, collapse = "\n\n")
  sub <- if (md) paste(strrep("#", heading_level + 1L), "Archetype portraits") else "Archetype portraits"
  port <- vapply(names(x$portraits), function(nm) {
    body <- sub(paste0(nm, " "), "", x$portraits[[nm]], fixed = TRUE)
    if (md) paste0("- **", nm, "** ", md_escape(body)) else paste0("* ", x$portraits[[nm]])
  }, character(1))
  paste(gsub("\n{3,}", "\n\n", out), sub, "", paste(port, collapse = "\n"), sep = "\n")
}

#' @export
as.character.q2q_cohort_narrative <- function(x, ...) format(x, ...)

#' @export
print.q2q_cohort_narrative <- function(x, ...) {
  cli_print({
    cli::cli_h1(x$title)
    for (p in x$sections) cli::cli_text("{p}")
    cli::cli_h3("Archetype portraits")
    cli::cli_ul()
    for (p in x$portraits) cli::cli_li("{p}")
    cli::cli_end()
  })
  invisible(x)
}
