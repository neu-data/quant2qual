# ---------------------------------------------------------------------------
# The quant2qual pipeline, as data and as a drawing. The table is the single
# definition: the diagram, the README and the PDF pipeline guide all read it.
# ---------------------------------------------------------------------------

#' The quant2qual workflow, as data
#'
#' One row per function in the workflow, with the stage it belongs to, its
#' order within the stage, whether it is optional and what it does.
#'
#' @return A [tibble][tibble::tibble] with columns `stage`, `step`, `fn`,
#'   `optional` and `caption`.
#' @export
#' @family pipeline
#' @examples
#' q2q_pipeline()
q2q_pipeline <- function() {
  tibble::tribble(
    ~stage,      ~step, ~fn,                           ~optional, ~caption,
    "Configure", 1L,    "q2q_lexicon()",               FALSE,     "name the subject, dimensions and units",
    "Configure", 2L,    "default_archetypes()",        FALSE,     "start from a domain's archetype matrix",
    "Configure", 3L,    "define_archetype()",          TRUE,      "add or replace behavioral signatures",
    "Classify",  1L,    "identify_archetype()",        FALSE,     "scale, band and match every case",
    "Classify",  2L,    "archetype_summary()",         FALSE,     "size and signature of each archetype",
    "Classify",  3L,    "archetype_membership()",      TRUE,      "fuzzy membership behind the confidence",
    "Narrate",   1L,    "select_cases()",              FALSE,     "pick exemplar or borderline cases",
    "Narrate",   2L,    "generate_narrative()",        FALSE,     "case-study prose for each case",
    "Narrate",   3L,    "generate_cohort_narrative()", FALSE,     "the cohort-level synthesis",
    "Report",    1L,    "plot_archetype_map()",        TRUE,      "figures for the report",
    "Report",    2L,    "write_behavioral_report()",   FALSE,     "Markdown, R Markdown, Quarto, HTML or PDF",
    "Report",    3L,    "neudata_pdf()",               TRUE,      "the Neudata PDF house style"
  )
}

#' Break a long function name at the underscore nearest its middle
#' @noRd
wrap_fn_label <- function(x, max_chars = 22L) {
  if (nchar(x) <= max_chars) {
    return(x)
  }
  pos <- gregexpr("_", x, fixed = TRUE)[[1]]
  if (identical(as.integer(pos), -1L)) {
    return(x)
  }
  cut <- pos[which.min(abs(pos - nchar(x) / 2))]
  paste0(substr(x, 1, cut), "\n", substr(x, cut + 1L, nchar(x)))
}

#' Draw the quant2qual pipeline
#'
#' A four-stage flow diagram: configure, classify, narrate, report. Optional
#' steps are outlined rather than filled.
#'
#' @param pipeline A table in the shape of [q2q_pipeline()].
#' @param base_size Base font size in points.
#' @param title,subtitle Labels; `NULL` for none.
#' @return A ggplot object.
#' @export
#' @family pipeline
#' @examples
#' plot_q2q_pipeline()
plot_q2q_pipeline <- function(pipeline = q2q_pipeline(), base_size = 10,
                              title = "The quant2qual pipeline",
                              subtitle = "From numeric distributions to qualitative case studies") {
  d <- as.data.frame(pipeline)
  check_columns(d, c("stage", "step", "fn", "optional", "caption"), arg = "pipeline")
  stages <- unique(d$stage)
  n_stage <- length(stages)
  d$row <- match(d$stage, stages)
  d$y <- n_stage - d$row + 1
  widest <- max(table(d$stage))
  d$x <- unlist(lapply(split(d, d$row), function(g) {
    seq_len(nrow(g)) + (widest - nrow(g)) / 2
  }), use.names = FALSE)
  off <- 0.85
  d$x <- d$x + off
  bw <- 0.46
  bh <- 0.30
  fills <- stats::setNames(rep(c("#055F56", "#0B376C", "#5FA39B", "#04242F"),
                               length.out = n_stage), stages)
  d$fill <- ifelse(d$optional, "white", unname(fills[d$stage]))
  d$border <- unname(fills[d$stage])
  d$text <- ifelse(d$optional, unname(fills[d$stage]), "white")
  # Long names break at the underscore nearest their middle so they stay
  # inside the box at any reasonable base_size.
  d$label <- vapply(d$fn, wrap_fn_label, character(1), USE.NAMES = FALSE)
  d$label <- ifelse(d$optional, paste0(d$label, "\n(optional)"), d$label)

  stage_lab <- data.frame(stage = stages, y = n_stage - seq_len(n_stage) + 1,
                          colour = unname(fills))
  arrows <- data.frame(
    x = (widest + 1) / 2 + off, xend = (widest + 1) / 2 + off,
    y = stage_lab$y[-n_stage] - bh - 0.2, yend = stage_lab$y[-1] + bh + 0.03
  )
  within <- do.call(rbind, lapply(split(d, d$row), function(g) {
    if (nrow(g) < 2L) return(NULL)
    data.frame(x = g$x[-nrow(g)] + bw, xend = g$x[-1] - bw, y = g$y[-1], yend = g$y[-1])
  }))

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(data = arrows, ggplot2::aes(x = .data$x, xend = .data$xend,
                                                      y = .data$y, yend = .data$yend),
                          arrow = grid::arrow(length = grid::unit(2.2, "mm"), type = "closed"),
                          colour = "#8A99A3", linewidth = 0.6) +
    ggplot2::geom_rect(data = d, ggplot2::aes(xmin = .data$x - bw, xmax = .data$x + bw,
                                              ymin = .data$y - bh, ymax = .data$y + bh),
                       fill = d$fill, colour = d$border, linewidth = 0.7) +
    ggplot2::geom_text(data = d, ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
                       colour = d$text, size = base_size / 3.7, fontface = "bold",
                       lineheight = 0.9, family = "mono") +
    ggplot2::geom_text(data = d, ggplot2::aes(x = .data$x, y = .data$y - bh - 0.12,
                                              label = .data$caption),
                       colour = "#1F2D33", size = base_size / 4.1, lineheight = 0.9) +
    ggplot2::geom_text(data = stage_lab, ggplot2::aes(x = 0.1, y = .data$y, label = .data$stage),
                       colour = stage_lab$colour, size = base_size / 2.6, fontface = "bold",
                       hjust = 0) +
    ggplot2::scale_x_continuous(limits = c(0.05, widest + off + 0.55), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.45, n_stage + 0.45), expand = c(0, 0)) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", colour = "#04242F"),
      plot.subtitle = ggplot2::element_text(colour = "#055F56"),
      plot.margin = ggplot2::margin(6, 6, 6, 6)
    )
  if (!is.null(within)) {
    p <- p + ggplot2::geom_segment(data = within,
                                   ggplot2::aes(x = .data$x, xend = .data$xend,
                                                y = .data$y, yend = .data$yend),
                                   arrow = grid::arrow(length = grid::unit(1.6, "mm"), type = "closed"),
                                   colour = "#8A99A3", linewidth = 0.4)
  }
  p
}
