# ---------------------------------------------------------------------------
# LLM-assisted narratives.
#
# The rule-based engine stays the source of truth. For each case it produces
# a fact sheet -- every number the narrative is allowed to cite, the archetype
# definition and the uncertainty statement -- plus its own draft. The model is
# asked to rewrite that draft into more fluent prose *using only those facts*,
# and returns the same five sections as generate_narrative() through
# structured output, so the result prints, formats and drops into reports
# exactly like a rule-based narrative.
#
# Two safeguards keep the prose honest:
#   * a grounding check lists every number in the generated text that does
#     not appear in the fact sheet, and warns when there are any;
#   * if the call fails or is refused, the rule-based narrative is returned
#     instead (fallback = TRUE), so a report never ends up with a hole in it.
# ---------------------------------------------------------------------------

#' Generate a case narrative with a large language model
#'
#' An alternative to [generate_narrative()] that passes each case's
#' statistics to a large language model through the
#' [ellmer](https://ellmer.tidyverse.org) package for more varied, fluent
#' prose. The model is grounded in a fact sheet built by the rule-based
#' engine and is instructed to cite nothing else; every number in its output
#' is then checked against that fact sheet.
#'
#' @section Model and provider:
#' By default a Claude chat is created with [ellmer::chat_anthropic()],
#' model `"claude-opus-5-5"`, adaptive thinking at `reasoning_effort` and
#' Anthropic's server-side refusal fallback enabled. This needs an API key in
#' the `ANTHROPIC_API_KEY` environment variable. Any other ellmer chat can be
#' passed as `chat` instead -- for example [ellmer::chat_openai()] or a local
#' model through [ellmer::chat_ollama()] -- and its system prompt is replaced
#' with the narrator's.
#'
#' @section Privacy:
#' Only aggregate, derived statistics for the case and its cohort are sent:
#' the case id, raw values of the three dimensions, percentiles, cohort
#' medians and the archetype definition. No other columns of `profile` leave
#' the machine. Use a local model if even that is not acceptable.
#'
#' @inheritParams generate_narrative
#' @param chat An ellmer `Chat` object, or `NULL` to create a Claude chat.
#' @param model Model used when `chat` is `NULL`.
#' @param reasoning_effort Thinking effort used when `chat` is `NULL`:
#'   `"low"`, `"medium"`, `"high"`, `"xhigh"` or `"max"`.
#' @param instructions Optional extra guidance for the writer, e.g.
#'   `"Write for a product manager audience."`.
#' @param fallback If the model call fails, return the rule-based narrative
#'   with a warning (`TRUE`) or raise the error (`FALSE`)?
#'
#' @return As [generate_narrative()]: a `q2q_narrative` for one id, a
#'   `q2q_narrative_set` for several. Each narrative also carries `engine`
#'   (`"llm"`, or `"rules"` after a fallback), `model`, `draft` (the
#'   rule-based narrative the model rewrote) and `ungrounded` (numbers in the
#'   generated text that are not in the fact sheet; empty when all are).
#' @export
#' @family narrative
#' @seealso [narrative_facts()] for the fact sheet sent to the model.
#' @examples
#' \dontrun{
#' profile <- identify_archetype(mock_behavioral_data(200))
#'
#' # Claude (needs ANTHROPIC_API_KEY)
#' generate_llm_narrative(profile, id = 104)
#'
#' # Any ellmer provider, e.g. a local model
#' generate_llm_narrative(profile, id = 104, style = "ux",
#'                        chat = ellmer::chat_ollama(model = "llama3.2"))
#'
#' # In a report
#' write_behavioral_report(profile, format = "pdf", narrator = "llm")
#' }
generate_llm_narrative <- function(profile, id, style = c("clinical", "ux", "brief"),
                                   chat = NULL, model = "claude-opus-5-5",
                                   reasoning_effort = "high", instructions = NULL,
                                   fallback = TRUE, lexicon = NULL) {
  rlang::check_installed("ellmer", "to generate narratives with a language model")
  check_profile(profile)
  style <- rlang::arg_match(style)
  check_string(model)
  check_string(instructions, allow_null = TRUE)
  check_flag(fallback)
  reasoning_effort <- rlang::arg_match(reasoning_effort,
                                       c("low", "medium", "high", "xhigh", "max"))
  # The rule-based narratives double as validation of `id` and as the drafts.
  drafts <- generate_narrative(profile, id, style = style, lexicon = lexicon)
  if (inherits(drafts, "q2q_narrative")) {
    drafts <- structure(list(drafts), class = "q2q_narrative_set")
  }
  if (is.null(chat)) {
    chat <- default_llm_chat(model, reasoning_effort)
  }
  check_chat(chat)
  meta <- attr(profile, "q2q")
  lx <- lexicon %||% meta$lexicon
  system_prompt <- llm_system_prompt(style, lx, instructions)
  type <- llm_output_type(style)

  out <- lapply(drafts, function(draft) {
    facts <- narrative_facts(profile, draft$id, lexicon = lx)
    llm_one(chat, draft, facts, system_prompt, type, style, fallback)
  })
  if (length(out) == 1L) {
    return(out[[1]])
  }
  structure(out, class = "q2q_narrative_set")
}

#' The fact sheet behind a narrative
#'
#' Collects every statistic a narrative about one case may cite -- raw
#' values, percentiles, standardised scores, cohort medians, ratios, rates,
#' outliers, the archetype definition and the classification confidence --
#' as plain text. This is exactly what [generate_llm_narrative()] sends to
#' the model, so it is also the place to check what would leave the machine.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param id A single case identifier.
#' @param lexicon Optional [q2q_lexicon()] overriding the stored one.
#'
#' @return A character string of class `q2q_facts`, with the numbers it
#'   contains in the `"numbers"` attribute.
#' @export
#' @family narrative
#' @examples
#' p <- identify_archetype(mock_behavioral_data(200))
#' narrative_facts(p, 104)
narrative_facts <- function(profile, id, lexicon = NULL) {
  check_profile(profile)
  meta <- attr(profile, "q2q")
  lx <- lexicon %||% meta$lexicon
  ids <- profile[[meta$id]]
  if (length(id) != 1L || !as.character(id) %in% as.character(ids)) {
    q2q_abort("{.arg id} must be a single case in {.field {meta$id}}.",
              class = "quant2qual_value_error")
  }
  row <- which(as.character(ids) == as.character(id))[1]
  f <- case_facts(profile, row, lx, meta)
  a <- f$a
  sv <- subject_values(lx)
  ev <- f$evidence
  dim_lines <- vapply(seq_len(nrow(ev)), function(i) {
    e <- ev[i, ]
    sprintf("- %s: %s %s; cohort median %s; percentile rank %s; z = %+.2f; band %s; ratio to median %s%s",
            e$noun, fmt_num(e$value), unit_for(e$value, e$unit), fmt_num(e$median),
            fmt_num(e$percentile, 0), e$z, e$level,
            if (is.finite(e$median) && e$median > 0) fmt_num(e$value / e$median, 1) else "n/a",
            if (isTRUE(e$outlier)) "; beyond the Tukey fence (statistical outlier)" else "")
  }, character(1))
  rate_lines <- c(
    if (!is.null(lx$pace) && is.finite(f$pace))
      sprintf("- %s: %s %s (cohort median %s)", lx$pace$noun, fmt_num(f$pace, 1),
              lx$pace$unit, fmt_num(f$pace_median, 1)),
    if (!is.null(lx$rate) && is.finite(f$rate))
      sprintf("- %s: %s %s (cohort median %s)", lx$rate$noun, fmt_num(f$rate, 1),
              lx$rate$unit, fmt_num(f$rate_median, 1))
  )
  recs <- a$recommendations[[1]]
  text <- paste(c(
    sprintf("Case: %s %s (one of %s %s in the cohort; domain: %s)",
            lx$case_label, f$id, fmt_num(f$n), lx$subject_plural, lx$label),
    sprintf("Subject noun: %s", lx$subject),
    "",
    sprintf("Archetype: %s (%s)", f$archetype, a$theme),
    sprintf("Persona: %s", interpolate(a$persona, sv)),
    sprintf("Interpretation of this archetype: %s", interpolate(a$interpretation, sv)),
    if (length(recs) > 0L) c("Implications of this archetype:", paste0("- ", recs)),
    "",
    "Dimensions, most salient first:",
    dim_lines,
    if (length(rate_lines) > 0L) c("Rates:", rate_lines),
    "",
    sprintf("Classification: %s assignment on %s scores; confidence %.2f (%s); runner-up %s (membership %.2f)",
            f$method, f$scaling, f$confidence, confidence_label(f$confidence),
            f$runner_up, f$runner_up_membership)
  ), collapse = "\n")
  structure(text, class = c("q2q_facts", "character"), numbers = extract_numbers(text))
}

#' @export
print.q2q_facts <- function(x, ...) {
  cat(unclass(x), "\n", sep = "")
  invisible(x)
}

# ---- internals ------------------------------------------------------------------

#' @noRd
default_llm_chat <- function(model, reasoning_effort) {
  ellmer::chat_anthropic(
    model = model,
    # Thinking tokens count against max_tokens; ellmer's default of 4096 can
    # truncate a five-section narrative at higher effort.
    params = ellmer::params(reasoning_effort = reasoning_effort, max_tokens = 16000),
    # If a safety classifier declines a request, let the API retry it on a
    # suitable model server-side instead of returning a refusal.
    beta_headers = "server-side-fallback-2026-07-01",
    api_args = list(fallbacks = "default"),
    echo = "none"
  )
}

#' @noRd
check_chat <- function(chat, call = rlang::caller_env()) {
  ok <- (is.environment(chat) || inherits(chat, "R6")) &&
    all(c("chat_structured", "clone", "set_system_prompt") %in% names(chat))
  if (!ok) {
    q2q_abort(c("{.arg chat} must be an ellmer chat, e.g. from {.fn ellmer::chat_anthropic}.",
                i = "Leave it {.code NULL} to create a Claude chat."),
              class = "quant2qual_type_error", call = call)
  }
  invisible(chat)
}

#' @noRd
llm_system_prompt <- function(style, lx, instructions) {
  register <- switch(style,
    clinical = paste("Write in the register of a clinical or psychological case study: third person,",
                     "formal, precise, cautious. Standardised scores may be cited in parentheses."),
    ux = paste("Write in the register of a UX research readout: clear, practical, addressed to a",
               "product team. Do not cite z-scores; prefer percentiles and plain comparisons."),
    brief = paste("Write one tight analytical paragraph of four to six sentences, suitable for a",
                  "dashboard or an e-mail summary. No z-scores.")
  )
  paste(c(
    sprintf(paste("You are a mixed-methods behavioral researcher writing qualitative case narratives",
                  "from quantitative behavioral data in the domain of %s. The subject of each case is",
                  "%s."), tolower(lx$label), lx$subject),
    "",
    "You will receive a fact sheet and a draft written by a rule-based engine. Rewrite the draft as",
    "fluent, varied prose that a researcher would be glad to put in a report.",
    "",
    "Rules that matter more than style:",
    "- Cite only numbers that appear in the fact sheet, exactly as written there. Do not compute new",
    "  statistics, round differently, or introduce counts, dates or percentages of your own.",
    "- Interpret the pattern; do not speculate about causes the data cannot show, and do not",
    "  diagnose. Describe behavior, not the person's character.",
    "- Keep the uncertainty: state the classification confidence, and if the runner-up membership is",
    "  0.40 or more, say that the case also resembles the runner-up archetype. Note that comparisons",
    "  are relative to this cohort.",
    "- Mention any statistical outliers flagged in the fact sheet.",
    "- Plain text only inside each field: no Markdown, headings or bullet characters.",
    "",
    register,
    if (!is.null(instructions)) c("", "Additional guidance from the analyst:", instructions)
  ), collapse = "\n")
}

#' @noRd
llm_output_type <- function(style) {
  if (style == "brief") {
    return(ellmer::type_object(
      summary = ellmer::type_string("The single-paragraph case summary.")
    ))
  }
  ellmer::type_object(
    presentation = ellmer::type_string(
      "Opening paragraph: the presenting pattern and the archetype it aligns with."),
    evidence = ellmer::type_string(
      "Paragraph citing the behavioral evidence, most salient dimension first, then rates and outliers."),
    interpretation = ellmer::type_string(
      "Paragraph interpreting what the signature means for this case."),
    implications = ellmer::type_array(
      ellmer::type_string("One concrete implication or action."),
      "Two to four implications or recommended actions."),
    confidence = ellmer::type_string(
      "Paragraph stating classification confidence, any overlap with the runner-up, and that comparisons are cohort-relative.")
  )
}

#' @noRd
llm_one <- function(chat, draft, facts, system_prompt, type, style, fallback) {
  prompt <- paste(
    "FACT SHEET", unclass(facts), "",
    "DRAFT", format(draft, format = "text"), "",
    "Rewrite the draft following the rules. Return each section separately.",
    sep = "\n"
  )
  result <- tryCatch({
    worker <- chat$clone()
    worker$set_system_prompt(system_prompt)
    worker$chat_structured(prompt, type = type)
  }, error = function(e) e)

  model <- tryCatch(chat$get_model(), error = function(e) NA_character_)
  if (inherits(result, "error") || !is.list(result)) {
    msg <- if (inherits(result, "error")) conditionMessage(result) else "an unexpected response"
    if (!fallback) {
      q2q_abort(c("The language model call failed for case {.val {draft$id}}.", x = msg),
                class = "quant2qual_llm_error")
    }
    q2q_warn(c("The language model call failed for case {.val {draft$id}}; using the rule-based narrative.",
               x = msg), class = "quant2qual_llm_fallback")
    draft$engine <- "rules"
    draft$model <- model
    draft$draft <- NULL
    draft$ungrounded <- character()
    return(draft)
  }

  sections <- if (style == "brief") {
    list(summary = clean_llm_text(result$summary))
  } else {
    list(
      presentation = clean_llm_text(result$presentation),
      evidence = clean_llm_text(result$evidence),
      interpretation = clean_llm_text(result$interpretation),
      implications = vapply(unlist(result$implications), clean_llm_text, character(1),
                            USE.NAMES = FALSE),
      confidence = clean_llm_text(result$confidence)
    )
  }
  generated <- paste(unlist(sections), collapse = " ")
  # The draft is derived deterministically from the same facts, so its
  # numbers (ratios, "top 5%") are grounded too.
  allowed <- union(attr(facts, "numbers"), extract_numbers(format(draft, format = "text")))
  ungrounded <- setdiff(extract_numbers(generated), allowed)
  if (length(ungrounded) > 0L) {
    q2q_warn(c("The narrative for case {.val {draft$id}} cites number{?s} not in its fact sheet: {.val {ungrounded}}.",
               i = "Check them before use; {.code $draft} holds the rule-based version."),
             class = "quant2qual_llm_ungrounded")
  }
  out <- draft
  out$sections <- sections
  out$engine <- "llm"
  out$model <- model
  out$draft <- draft
  out$ungrounded <- ungrounded
  out
}

#' Strip Markdown and stray whitespace from model text
#' @noRd
clean_llm_text <- function(x) {
  x <- paste(as.character(x), collapse = " ")
  x <- gsub("\\*\\*|__|`", "", x)
  x <- gsub("^\\s*[-*\u2022]\\s+", "", x)
  trimws(gsub("\\s+", " ", x))
}

#' Every number in a text, normalised for comparison
#'
#' Thousands separators and signs are dropped and trailing zeros removed, so
#' "1,200", "+2.70" and "2.7" compare as 1200, 2.7 and 2.7. Integers from 0 to
#' 10 and 100 are ignored: they occur naturally in prose ("two of the three
#' dimensions", "per 100 actions") and carry no claim about the case.
#' @noRd
extract_numbers <- function(text) {
  m <- regmatches(text, gregexpr("[0-9][0-9,]*(\\.[0-9]+)?", text))[[1]]
  if (length(m) == 0L) {
    return(character())
  }
  v <- suppressWarnings(as.numeric(gsub(",", "", m)))
  v <- v[!is.na(v)]
  v <- v[!(v == round(v) & (v <= 10 | v == 100))]
  unique(vapply(v, function(x) format(x, scientific = FALSE, drop0trailing = TRUE, trim = TRUE),
                character(1)))
}
