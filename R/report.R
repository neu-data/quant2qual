# ---------------------------------------------------------------------------
# The reporting interface.
#
# A report is assembled as plain pandoc Markdown -- key findings, the cohort
# synthesis with its figures, one case study per selected case, and a
# methods section generated from the classification settings -- and only
# then wrapped for a target: a Markdown string, an R Markdown or Quarto
# source, or a rendered HTML or PDF file. Keeping the body format-free means
# every target says exactly the same thing.
#
# PDF output goes through the Neudata LaTeX class (inst/neudata/neudata.cls),
# the same class used for Neudata reports outside R, via a pandoc template.
# ---------------------------------------------------------------------------

#' Neudata-styled PDF output format
#'
#' An R Markdown output format that typesets documents with the Neudata
#' LaTeX class: cover page, branded headers and footers, navy table headers
#' and the closing copyright page. Used by [write_behavioral_report()] and by
#' the package's pipeline guide, and usable in any R Markdown document:
#'
#' ```yaml
#' output:
#'   quant2qual::neudata_pdf:
#'     toc: true
#' ```
#'
#' Extra YAML fields understood by the template: `subtitle`, `client`,
#' `affiliation`, `reference`, `version`, `confidentiality` and `abstract`.
#' Inside the document, the LaTeX environments `keyfindings` and
#' `neudatanote` are available.
#'
#' @param style `"style1"` (Neudata Quarto-report look) or `"style2"`
#'   (Neudata PowerPoint look).
#' @param doc_type `"report"`, `"proposal"` or `"note"`, shown on the cover.
#' @param toc Include a table of contents?
#' @param number_sections Number section headings?
#' @param copyright_page Append the Neudata copyright page?
#' @param draft Add a DRAFT watermark?
#' @param cover Use the full cover page (`TRUE`) or a compact title block?
#' @param keep_tex Keep the intermediate `.tex` file?
#' @param ... Further arguments passed to [rmarkdown::pdf_document()].
#'
#' @return An R Markdown output format object.
#' @export
#' @family report
#' @examples
#' fmt <- neudata_pdf(toc = TRUE)
#' class(fmt)
neudata_pdf <- function(style = c("style1", "style2"),
                        doc_type = c("report", "proposal", "note"),
                        toc = FALSE, number_sections = TRUE,
                        copyright_page = TRUE, draft = FALSE, cover = TRUE,
                        keep_tex = FALSE, ...) {
  rlang::check_installed("rmarkdown", "to render PDF output")
  style <- rlang::arg_match(style)
  doc_type <- rlang::arg_match(doc_type)
  for (flag in list(toc, number_sections, copyright_page, draft, cover, keep_tex)) {
    check_flag(flag, arg = "flag")
  }
  latex_dir <- system.file("neudata", package = "quant2qual")
  template <- file.path(latex_dir, "neudata-template.tex")
  if (!file.exists(template)) {
    q2q_abort("The Neudata LaTeX template is missing from the installed package.",
              class = "quant2qual_install_error")
  }
  opts <- c(style, doc_type, if (draft) "draft", if (!cover) "nocover")
  # A wide --columns keeps pipe tables at their natural widths instead of
  # wrapping every cell to a fixed share of the line.
  args <- c("--columns=160", as.vector(rbind("-V", paste0("classoption=", opts))),
            if (copyright_page) c("-V", "copyright-page=true"))

  base <- rmarkdown::pdf_document(
    template = template, toc = toc, number_sections = number_sections,
    keep_tex = keep_tex, latex_engine = "pdflatex", fig_caption = TRUE,
    dev = "pdf", pandoc_args = args, ...
  )
  copied <- character()
  pre <- function(metadata, input_file, runtime, knit_meta, files_dir, output_dir) {
    dest <- dirname(normalizePath(input_file, mustWork = FALSE))
    for (f in c("neudata.cls", "neudata-logo.png")) {
      target <- file.path(dest, f)
      if (!file.exists(target)) {
        file.copy(file.path(latex_dir, f), target)
        copied <<- c(copied, target)
      }
    }
    invisible(NULL)
  }
  rmarkdown::output_format(
    knitr = NULL, pandoc = NULL, pre_processor = pre,
    on_exit = function() {
      unlink(copied)
      copied <<- character()
    },
    base_format = base
  )
}

#' Neudata-styled HTML output format
#'
#' A self-contained HTML document with the Neudata palette, for use in R
#' Markdown documents and by [write_behavioral_report()].
#'
#' @param toc Include a floating table of contents?
#' @param ... Further arguments passed to [rmarkdown::html_document()].
#' @return An R Markdown output format object.
#' @export
#' @family report
#' @examples
#' fmt <- neudata_html()
#' class(fmt)
neudata_html <- function(toc = TRUE, ...) {
  rlang::check_installed("rmarkdown", "to render HTML output")
  rmarkdown::html_document(
    toc = toc, toc_float = toc, theme = "default", highlight = "tango",
    css = system.file("css", "neudata.css", package = "quant2qual"),
    self_contained = TRUE, ...
  )
}

#' Write a behavioral narrative report
#'
#' Compiles the cohort synthesis and a set of case narratives into a
#' structured report: key findings, cohort synthesis with figures and an
#' archetype table, one case study per selected case (with its profile
#' chart), and an automatically written methods section that records the
#' scaling, the band edges in raw units and the archetype matrix.
#'
#' @section Formats:
#' * `"markdown"` -- pandoc Markdown. With `file = NULL` the text is
#'   returned without figures; with a file, figures are written to a
#'   `<name>_figures/` folder beside it.
#' * `"rmd"` -- an R Markdown source that renders with [neudata_pdf()].
#' * `"qmd"` -- a Quarto source with HTML and PDF formats declared.
#' * `"html"` -- rendered with [neudata_html()].
#' * `"pdf"` -- rendered with [neudata_pdf()] (needs a LaTeX installation,
#'   e.g. `tinytex::install_tinytex()`).
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param file Output path. Optional for `"markdown"`; for other formats a
#'   file in [tempdir()] is used when `NULL`.
#' @param format One of `"markdown"`, `"rmd"`, `"qmd"`, `"html"` or `"pdf"`.
#' @param ids Case identifiers to narrate. Defaults to [select_cases()].
#' @param n_per_archetype,strategy Passed to [select_cases()] when `ids` is
#'   `NULL`.
#' @param style Narrative style; see [generate_narrative()].
#' @param title,subtitle,author,client,reference,confidentiality Document
#'   metadata. `client`, `reference` and `confidentiality` appear on the PDF
#'   cover.
#' @param include_cohort,include_figures,include_methods Include the cohort
#'   synthesis, the figures and the methods section?
#' @param toc Include a table of contents (HTML and PDF)?
#' @param pdf_style `"style1"` or `"style2"`; see [neudata_pdf()].
#' @param quiet Suppress rendering output?
#' @param narrator `"rules"` (default) writes case narratives with
#'   [generate_narrative()]; `"llm"` rewrites them with
#'   [generate_llm_narrative()]. The cohort synthesis and methods are always
#'   rule-based, and the methods section states which narrator was used.
#' @param llm_args A list of further arguments for [generate_llm_narrative()]
#'   when `narrator = "llm"`, e.g. `list(chat = ellmer::chat_ollama(...))`.
#'
#' @return For `format = "markdown"` with `file = NULL`, the Markdown text
#'   (a character string). Otherwise the path to the written file,
#'   invisibly.
#' @export
#' @family report
#' @examples
#' profile <- identify_archetype(mock_behavioral_data(200))
#'
#' md <- write_behavioral_report(profile, n_per_archetype = 1, style = "brief")
#' cat(substr(md, 1, 600))
#'
#' path <- write_behavioral_report(profile, file = tempfile(fileext = ".Rmd"),
#'                                 format = "rmd")
#' file.exists(path)
#'
#' \donttest{
#' if (rmarkdown::pandoc_available() && nzchar(Sys.which("pdflatex"))) {
#'   pdf <- write_behavioral_report(profile, format = "pdf")
#' }
#' }
write_behavioral_report <- function(profile, file = NULL,
                                    format = c("markdown", "rmd", "qmd", "html", "pdf"),
                                    ids = NULL, n_per_archetype = 1,
                                    strategy = c("representative", "extreme", "borderline"),
                                    style = c("clinical", "ux", "brief"),
                                    title = "Behavioral Narrative Report",
                                    subtitle = NULL, author = "Neudata Consulting Ltd",
                                    client = NULL, reference = NULL,
                                    confidentiality = NULL,
                                    include_cohort = TRUE, include_figures = TRUE,
                                    include_methods = TRUE, toc = TRUE,
                                    pdf_style = c("style1", "style2"), quiet = TRUE,
                                    narrator = c("rules", "llm"), llm_args = list()) {
  check_profile(profile)
  narrator <- rlang::arg_match(narrator)
  if (!is.list(llm_args)) {
    q2q_abort("{.arg llm_args} must be a list.", class = "quant2qual_type_error")
  }
  format <- rlang::arg_match(format)
  strategy <- rlang::arg_match(strategy)
  style <- rlang::arg_match(style)
  pdf_style <- rlang::arg_match(pdf_style)
  check_string(title)
  check_string(subtitle, allow_null = TRUE)
  check_string(author)
  check_string(client, allow_null = TRUE)
  check_string(reference, allow_null = TRUE)
  check_string(confidentiality, allow_null = TRUE)
  check_string(file, allow_null = TRUE)
  for (f in c("include_cohort", "include_figures", "include_methods", "toc", "quiet")) {
    check_flag(get(f), arg = f)
  }
  meta <- attr(profile, "q2q")
  lx <- meta$lexicon
  if (is.null(ids)) {
    ids <- select_cases(profile, n = n_per_archetype, strategy = strategy)
  }
  if (is.null(subtitle)) {
    subtitle <- sprintf("%s case studies from %s %s", capitalise(lx$label),
                        fmt_num(meta$n_complete), lx$subject_plural)
  }

  render_target <- format %in% c("html", "pdf")
  if (is.null(file) && format != "markdown") {
    ext <- c(rmd = ".Rmd", qmd = ".qmd", html = ".html", pdf = ".pdf")[[format]]
    file <- file.path(tempdir(), paste0("behavioral-report", ext))
  }

  # Where the figures go, and how the Markdown refers to them.
  figures <- include_figures && !is.null(file)
  if (render_target) {
    workdir <- tempfile("q2q-report-")
    dir.create(workdir)
    fig_dir <- file.path(workdir, "figures")
    fig_ref <- "figures"
  } else if (figures) {
    stem <- tools::file_path_sans_ext(basename(file))
    fig_ref <- paste0(stem, "_figures")
    fig_dir <- file.path(dirname(file), fig_ref)
  }
  fig_ext <- if (format == "pdf") "pdf" else "png"
  callout_target <- if (format %in% c("pdf", "rmd")) "latex" else "div"

  body <- report_body(profile, ids, style, lx, meta,
                      include_cohort = include_cohort,
                      include_methods = include_methods,
                      figures = figures, fig_dir = if (figures) fig_dir,
                      fig_ref = if (figures) fig_ref, fig_ext = fig_ext,
                      callout_target = callout_target,
                      narrator = narrator, llm_args = llm_args)

  if (format == "markdown") {
    md <- paste(c(paste("#", title), "", paste0("*", subtitle, "*"), "", body), collapse = "\n")
    if (is.null(file)) {
      return(md)
    }
    write_utf8(md, file)
    return(invisible(normalizePath(file, winslash = "/")))
  }

  front <- yaml_front_matter(format, title, subtitle, author, client, reference,
                             confidentiality, toc, pdf_style)
  doc <- paste(c(front, "", body), collapse = "\n")

  if (!render_target) {
    write_utf8(doc, file)
    return(invisible(normalizePath(file, winslash = "/")))
  }

  rlang::check_installed("rmarkdown", "to render reports")
  if (!rmarkdown::pandoc_available()) {
    q2q_abort(c("Rendering needs pandoc, which was not found.",
                i = "Install it, or use {.code format = \"rmd\"} or {.code \"markdown\"} to write the source only."),
              class = "quant2qual_dependency_error")
  }
  input <- file.path(workdir, "behavioral-report.Rmd")
  write_utf8(doc, input)
  out_fmt <- if (format == "pdf") {
    neudata_pdf(style = pdf_style, toc = toc)
  } else {
    neudata_html(toc = toc)
  }
  out_dir <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  res <- rmarkdown::render(input, output_format = out_fmt,
                           output_file = basename(file), output_dir = out_dir,
                           quiet = quiet, envir = new.env(parent = globalenv()))
  unlink(workdir, recursive = TRUE)
  if (!quiet) cli::cli_alert_success("Report written to {.file {res}}")
  invisible(normalizePath(res, winslash = "/"))
}

# ---- body assembly -----------------------------------------------------------------

#' @noRd
report_body <- function(profile, ids, style, lx, meta, include_cohort, include_methods,
                        figures, fig_dir, fig_ref, fig_ext, callout_target,
                        narrator = "rules", llm_args = list()) {
  out <- character()
  if (figures) dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
  fig <- function(plot, name, caption, width = 7, height = 4.6, out_width = "95%") {
    path <- file.path(fig_dir, paste0(name, ".", fig_ext))
    save_plot(plot, path, width, height)
    c(sprintf("![%s](%s/%s.%s){width=%s}", caption, fig_ref, name, fig_ext, out_width), "")
  }

  cohort <- generate_cohort_narrative(profile, lexicon = lx)
  out <- c(out, "# Key findings", "", key_findings(profile, cohort, lx, callout_target), "")

  if (include_cohort) {
    out <- c(out, "# Cohort synthesis", "",
             unname(vapply(cohort$sections, md_escape, character(1))), "")
    if (figures) {
      out <- c(out, fig(plot_archetype_map(profile), "archetype-map",
                        sprintf("Archetype map: standardised %s against %s; point size shows %s.",
                                lx$dimensions$velocity$noun, lx$dimensions$intensity$noun,
                                lx$dimensions$friction$noun)))
      out <- c(out, fig(plot_archetype_signatures(profile), "archetype-signatures",
                        "Archetype signatures: mean standardised score on each dimension.",
                        height = 3.6))
    }
    s <- cohort$summary
    s <- s[s$n > 0, ]
    tab <- data.frame(
      Archetype = as.character(s$archetype),
      N = s$n,
      Share = sprintf("%.0f%%", 100 * s$share),
      V = fmt_num(s$median_velocity),
      I = fmt_num(s$median_intensity),
      F = fmt_num(s$median_friction),
      Confidence = sprintf("%.2f", s$mean_confidence),
      check.names = FALSE
    )
    names(tab)[4:6] <- vapply(q2q_dimensions(), function(d) {
      sprintf("Median %s", lx$dimensions[[d]]$unit)
    }, character(1))
    out <- c(out, md_table(tab), "",
             ": Archetypes in the cohort, with median raw values and mean classification confidence.", "",
             "## Archetype portraits", "",
             paste0("- **", names(cohort$portraits), "** ",
                    md_escape(substring(cohort$portraits, nchar(names(cohort$portraits)) + 2L))),
             "")
  }

  out <- c(out, if (style == "brief") "# Case summaries" else "# Case studies", "",
           sprintf(paste("The cases below were selected to illustrate each archetype.",
                         "Each narrative is generated from the case's own statistics and",
                         "compares it with the cohort of %s %s."),
                   fmt_num(meta$n_complete), lx$subject_plural), "")
  narr <- if (narrator == "llm") {
    do.call(generate_llm_narrative,
            c(list(profile = profile, id = ids, style = style, lexicon = lx), llm_args))
  } else {
    generate_narrative(profile, ids, style = style, lexicon = lx)
  }
  if (inherits(narr, "q2q_narrative")) narr <- structure(list(narr), class = "q2q_narrative_set")
  llm_models <- unique(stats::na.omit(unlist(lapply(narr, function(n) {
    if (identical(n$engine, "llm")) n$model else NA_character_
  }))))
  for (k in seq_along(narr)) {
    n <- narr[[k]]
    text <- format(n, heading_level = 2)
    if (figures && style != "brief") {
      # The fingerprint goes directly under the case heading and subtitle.
      parts <- strsplit(text, "\n\n", fixed = TRUE)[[1]]
      figure <- fig(plot_case_profile(profile, n$id), paste0("case-", k),
                    sprintf("Behavioral fingerprint of case %s against the cohort.", n$id),
                    width = 7, height = 2.6, out_width = "85%")
      text <- paste(c(parts[1:2], paste(figure, collapse = "\n"), parts[-(1:2)]),
                    collapse = "\n\n")
    }
    out <- c(out, text, "")
  }

  if (include_methods) {
    out <- c(out, methods_section(profile, meta, lx, callout_target, llm_models))
  }
  out
}

#' @noRd
key_findings <- function(profile, cohort, lx, target) {
  meta <- attr(profile, "q2q")
  s <- cohort$summary
  s <- s[s$n > 0, ]
  s <- s[order(-s$n), ]
  am <- meta$archetypes
  d <- profile[!is.na(profile$archetype), ]
  fr <- am$archetype[!am$fallback & am$friction == "high"]
  fr_share <- sum(s$share[s$archetype %in% fr])
  lines <- c(
    sprintf("- The dominant signature is **%s**, covering %.0f%% of %s.",
            s$archetype[1], 100 * s$share[1], lx$subject_plural),
    if (length(fr) > 0L)
      sprintf("- %.0f%% of %s show a friction-led profile (%s).",
              100 * fr_share, lx$subject_plural, collapse_and(fr)),
    sprintf("- Median classification confidence is %.2f; %d case%s classified with low confidence.",
            stats::median(d$confidence), sum(d$confidence < 0.5),
            if (sum(d$confidence < 0.5) == 1L) " was" else "s were"),
    sprintf("- %d case%s lie beyond a Tukey fence on at least one dimension.",
            sum(d$outlier, na.rm = TRUE), if (sum(d$outlier, na.rm = TRUE) == 1L) "" else "s")
  )
  callout("Key findings", lines, target, "keyfindings")
}

#' @noRd
callout <- function(title, lines, target, env = "keyfindings") {
  if (target == "latex") {
    c("```{=latex}", sprintf("\\begin{%s}[%s]", env, title), "```", "", lines, "",
      "```{=latex}", sprintf("\\end{%s}", env), "```")
  } else {
    c(sprintf("::: {.%s}", env), sprintf("**%s**", title), "", lines, ":::")
  }
}

#' @noRd
methods_section <- function(profile, meta, lx, target, llm_models = character()) {
  dims <- q2q_dimensions()
  cols <- meta$columns
  scaling_txt <- switch(meta$scaling,
    z = "as z-scores (deviation from the cohort mean in standard-deviation units)",
    robust = "as robust z-scores (deviation from the cohort median in units of the scaled median absolute deviation)",
    percentile = "as rank-based normal scores (the normal quantile of each case's mid-rank percentile)"
  )
  if (meta$transform == "log1p") scaling_txt <- paste(scaling_txt, "after a log(1 + x) transformation")
  method_txt <- if (meta$method == "rules") {
    paste("each case was assigned to the highest-priority archetype whose band rule it met exactly,",
          "and to the fallback archetype when it met none")
  } else {
    "each case was assigned to the archetype in which its fuzzy membership was highest"
  }
  dim_txt <- collapse_and(vapply(dims, function(d) {
    sprintf("%s (%s, column `%s`)", d, lx$dimensions[[d]]$noun, cols[[d]])
  }, character(1)))
  version <- tryCatch(as.character(utils::packageVersion("quant2qual")), error = function(e) "dev")

  bands <- data.frame(
    Dimension = vapply(dims, function(d) lx$dimensions[[d]]$label, character(1)),
    Column = md_escape(unname(cols)),
    Low = vapply(dims, function(d) sprintf("< %s", fmt_num(meta$reference[[d]]$edges_raw[1], 1)), character(1)),
    High = vapply(dims, function(d) sprintf("> %s", fmt_num(meta$reference[[d]]$edges_raw[2], 1)), character(1)),
    Median = vapply(dims, function(d) fmt_num(meta$reference[[d]]$median, 1), character(1)),
    Source = vapply(dims, function(d) if (!is.null(meta$cutpoints[[d]])) "fixed cutpoints"
                    else sprintf("+/- %s on the standardised scale", meta$threshold[[d]]), character(1)),
    check.names = FALSE, row.names = NULL
  )
  am <- meta$archetypes
  matrix_tab <- data.frame(
    Archetype = am$archetype,
    Velocity = ifelse(am$fallback, "--", am$velocity),
    Intensity = ifelse(am$fallback, "--", am$intensity),
    Friction = ifelse(am$fallback, "--", am$friction),
    Theme = am$theme,
    check.names = FALSE
  )
  c("# Methods", "",
    md_escape(sprintf(paste("Each %s was described on three behavioral dimensions: %s.",
                            "Every dimension was standardised against the cohort %s, and cut into low, mid and high",
                            "bands. Cases were then matched against a matrix of %d archetypes: %s.",
                            "Classification confidence is the fuzzy membership of a case in its assigned archetype:",
                            "a logistic ramp of steepness %s at each band edge (0.5 exactly on the edge),",
                            "combined across constrained dimensions by geometric mean."),
                      lx$subject_short, gsub("`", "", dim_txt), scaling_txt, nrow(am), method_txt,
                      meta$sharpness)), "",
    md_table(bands), "",
    ": Band edges expressed in raw units.", "",
    md_table(matrix_tab), "",
    ": The archetype matrix. Rules list the bands each dimension may fall in; the last row is the fallback.", "",
    callout("Reproducibility", md_escape(reproducibility_note(version, llm_models)),
            target, "neudatanote"), "")
}

#' @noRd
reproducibility_note <- function(version, llm_models = character()) {
  if (length(llm_models) == 0L) {
    return(sprintf(paste(
      "Narratives were assembled by rule-based templating from each case's own statistics with",
      "quant2qual %s. No generative language model was used: the same data and settings always produce",
      "the same text, and every number quoted in a narrative can be traced to the profile table."), version))
  }
  sprintf(paste(
    "Case narratives were drafted by rule-based templating with quant2qual %s and then rewritten by",
    "%s through the ellmer package. The model received only a fact sheet of the case's derived",
    "statistics and the draft, was instructed to cite no other numbers, and every number in its output",
    "was checked against that fact sheet. The cohort synthesis and this methods section are rule-based.",
    "Rewritten text can vary between runs; the rule-based drafts are reproducible."),
    version, collapse_and(llm_models))
}

# ---- small writers -------------------------------------------------------------------

#' @noRd
yaml_front_matter <- function(format, title, subtitle, author, client, reference,
                              confidentiality, toc, pdf_style) {
  q <- function(x) paste0('"', gsub('"', '\\\\"', x), '"')
  meta <- c(
    paste("title:", q(title)),
    if (!is.null(subtitle)) paste("subtitle:", q(subtitle)),
    paste("author:", q(author)),
    if (!is.null(client)) paste("client:", q(client)),
    if (!is.null(reference)) paste("reference:", q(reference)),
    if (!is.null(confidentiality)) paste("confidentiality:", q(confidentiality)),
    paste("date:", q(format(Sys.Date(), "%d %B %Y")))
  )
  tf <- if (toc) "true" else "false"
  out <- switch(format,
    rmd = c("output:", "  quant2qual::neudata_pdf:", paste0("    style: ", pdf_style),
            paste0("    toc: ", tf)),
    qmd = c("format:", "  html:", paste0("    toc: ", tf), "  pdf:", paste0("    toc: ", tf)),
    character()
  )
  c("---", meta, out, "---")
}

#' @noRd
md_table <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  esc <- function(x) gsub("|", "\\|", as.character(x), fixed = TRUE)
  num <- vapply(df, is.numeric, logical(1))
  header <- paste0("| ", paste(esc(names(df)), collapse = " | "), " |")
  align <- paste0("|", paste(ifelse(num, "---:", ":---"), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) paste0("| ", paste(esc(trimws(r)), collapse = " | "), " |"))
  c(header, align, rows)
}

#' @noRd
write_utf8 <- function(text, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(text, con, useBytes = FALSE)
  invisible(path)
}

#' @noRd
save_plot <- function(plot, path, width, height) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "pdf") {
    dev <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf
    ggplot2::ggsave(path, plot, width = width, height = height, device = dev)
  } else {
    dev <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
    ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, device = dev)
  }
  invisible(path)
}
