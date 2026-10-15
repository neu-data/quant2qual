# ---------------------------------------------------------------------------
# Figures. Each one answers a question a reader of a narrative report asks:
#
#   plot_archetype_map()        where does every case sit, and how do the
#                               archetypes partition the space?
#   plot_archetype_signatures() what makes each archetype distinctive?
#   plot_case_profile()         where does this one case sit against the
#                               cohort on every dimension?
#
# All plots work on the standardised scale, so the band edges used for
# classification can be drawn directly onto them.
# ---------------------------------------------------------------------------

#' Colours for the archetypes of a profile or matrix
#'
#' @param x A `q2q_profile` or a `q2q_archetype_matrix`.
#' @return A named character vector of colours, one per archetype.
#' @export
#' @family plots
#' @examples
#' archetype_palette(default_archetypes("ux"))
archetype_palette <- function(x) {
  am <- if (inherits(x, "q2q_profile")) attr(x, "q2q")$archetypes else x
  if (!inherits(am, "q2q_archetype_matrix")) {
    q2q_abort("{.arg x} must be a {.cls q2q_profile} or {.cls q2q_archetype_matrix}.",
              class = "quant2qual_type_error")
  }
  stats::setNames(am$colour, am$archetype)
}

#' @noRd
dim_label <- function(lx, d) {
  sprintf("%s (z)", capitalise(lx$dimensions[[d]]$noun))
}

#' Map cases onto two behavioral dimensions
#'
#' A scatter plot of standardised scores coloured by archetype, with the band
#' edges used for classification drawn as dashed lines. The third dimension
#' is shown by point size.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param x,y Dimensions for the axes: any two of `"velocity"`,
#'   `"intensity"` and `"friction"`.
#' @param highlight Optional case identifiers to label.
#' @param base_size Base font size.
#'
#' @return A ggplot object.
#' @export
#' @family plots
#' @examples
#' p <- identify_archetype(mock_behavioral_data(200))
#' plot_archetype_map(p)
#' plot_archetype_map(p, x = "intensity", y = "friction", highlight = 104)
plot_archetype_map <- function(profile, x = "velocity", y = "intensity",
                               highlight = NULL, base_size = 10) {
  check_profile(profile)
  dims <- q2q_dimensions()
  x <- rlang::arg_match(x, dims)
  y <- rlang::arg_match(y, dims)
  if (identical(x, y)) {
    q2q_abort("{.arg x} and {.arg y} must be different dimensions.",
              class = "quant2qual_value_error")
  }
  size_dim <- setdiff(dims, c(x, y))
  meta <- attr(profile, "q2q")
  lx <- meta$lexicon
  d <- tibble::as_tibble(unclass_profile(profile))
  d <- d[!is.na(d$archetype), ]
  d$.x <- d[[paste0("z_", x)]]
  d$.y <- d[[paste0("z_", y)]]
  d$.size <- d[[meta$columns[[size_dim]]]]
  ex <- meta$reference[[x]]$edges_z
  ey <- meta$reference[[y]]$edges_z

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$.x, y = .data$.y)) +
    ggplot2::geom_vline(xintercept = ex, linetype = "dashed", colour = "#8A99A3", linewidth = 0.35) +
    ggplot2::geom_hline(yintercept = ey, linetype = "dashed", colour = "#8A99A3", linewidth = 0.35) +
    ggplot2::geom_point(ggplot2::aes(colour = .data$archetype, size = .data$.size), alpha = 0.75) +
    ggplot2::scale_colour_manual(values = archetype_palette(profile), name = NULL, drop = TRUE) +
    ggplot2::scale_size(range = c(0.8, 4.5), name = capitalise(lx$dimensions[[size_dim]]$noun)) +
    ggplot2::labs(x = dim_label(lx, x), y = dim_label(lx, y)) +
    theme_neudata(base_size = base_size) +
    ggplot2::theme(legend.box = "vertical") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 2, override.aes = list(size = 3)))
  if (!is.null(highlight)) {
    h <- d[as.character(d[[meta$id]]) %in% as.character(highlight), ]
    h$.label <- paste0("#", h[[meta$id]])
    p <- p +
      ggplot2::geom_point(data = h, shape = 21, size = 5, stroke = 1, colour = "#04242F", fill = NA) +
      ggplot2::geom_text(data = h, ggplot2::aes(label = .data$.label), nudge_y = 0.28,
                         size = base_size / 3.2, fontface = "bold", colour = "#04242F")
  }
  p
}

#' Plot the signature of every archetype
#'
#' A tile chart of the mean standardised score of each archetype on each
#' dimension -- the empirical counterpart of the archetype matrix. It shows
#' whether the archetypes the rules define are distinct in the data.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param base_size Base font size.
#' @return A ggplot object.
#' @export
#' @family plots
#' @examples
#' plot_archetype_signatures(identify_archetype(mock_behavioral_data(200)))
plot_archetype_signatures <- function(profile, base_size = 10) {
  check_profile(profile)
  lx <- attr(profile, "q2q")$lexicon
  s <- archetype_summary(profile)
  s <- s[s$n > 0, ]
  long <- tidyr::pivot_longer(s, dplyr::starts_with("mean_z_"),
                              names_to = "dimension", values_to = "z",
                              names_prefix = "mean_z_")
  long$dimension <- factor(long$dimension, levels = q2q_dimensions(),
                           labels = vapply(q2q_dimensions(), function(d) {
                             capitalise(lx$dimensions[[d]]$noun)
                           }, character(1)))
  long$archetype <- factor(long$archetype, levels = rev(levels(s$archetype)))
  long$label <- sprintf("%+.1f", long$z)
  lim <- max(1.5, ceiling(max(abs(long$z), na.rm = TRUE) * 2) / 2)
  ggplot2::ggplot(long, ggplot2::aes(x = .data$dimension, y = .data$archetype, fill = .data$z)) +
    ggplot2::geom_tile(colour = "white", linewidth = 1.2) +
    ggplot2::geom_text(ggplot2::aes(label = .data$label,
                                    colour = abs(.data$z) > lim * 0.55),
                       size = base_size / 2.9, show.legend = FALSE) +
    ggplot2::scale_fill_gradient2(low = "#0B376C", mid = "#F4F7F8", high = "#C0504D",
                                  midpoint = 0, limits = c(-lim, lim), name = "Mean z") +
    ggplot2::scale_colour_manual(values = c(`FALSE` = "#1F2D33", `TRUE` = "white")) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = NULL, y = NULL) +
    theme_neudata(base_size = base_size) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "right")
}

#' Plot one case against the cohort
#'
#' The case's standardised score on each dimension, drawn over the cohort's
#' distribution (box plot) and the low / mid / high bands. A compact visual
#' companion to a case narrative.
#'
#' @param profile A `q2q_profile` from [identify_archetype()].
#' @param id A single case identifier.
#' @param base_size Base font size.
#' @return A ggplot object.
#' @export
#' @family plots
#' @examples
#' p <- identify_archetype(mock_behavioral_data(200))
#' plot_case_profile(p, 104)
plot_case_profile <- function(profile, id, base_size = 10) {
  check_profile(profile)
  meta <- attr(profile, "q2q")
  lx <- meta$lexicon
  if (length(id) != 1L || !as.character(id) %in% as.character(profile[[meta$id]])) {
    q2q_abort("{.arg id} must be a single case in {.field {meta$id}}.",
              class = "quant2qual_value_error")
  }
  dims <- q2q_dimensions()
  labels <- vapply(dims, function(d) capitalise(lx$dimensions[[d]]$noun), character(1))
  d <- tibble::as_tibble(unclass_profile(profile))
  cohort <- tidyr::pivot_longer(d[!is.na(d$archetype), paste0("z_", dims)],
                                dplyr::everything(), names_to = "dimension",
                                values_to = "z", names_prefix = "z_")
  cohort$dimension <- factor(cohort$dimension, levels = rev(dims), labels = rev(labels))
  row <- d[as.character(d[[meta$id]]) == as.character(id), ]
  case <- tibble::tibble(
    dimension = factor(dims, levels = rev(dims), labels = rev(labels)),
    z = vapply(dims, function(dd) row[[paste0("z_", dd)]], numeric(1)),
    value = vapply(dims, function(dd) {
      sprintf("%s %s", fmt_num(row[[meta$columns[[dd]]]]),
              unit_for(row[[meta$columns[[dd]]]], lx$dimensions[[dd]]$unit))
    }, character(1))
  )
  edges <- do.call(rbind, lapply(dims, function(dd) {
    e <- meta$reference[[dd]]$edges_z
    data.frame(dimension = factor(labels[[dd]], levels = rev(labels)),
               lo = e[1], hi = e[2])
  }))
  colour <- archetype_palette(profile)[[as.character(row$archetype)]]
  ggplot2::ggplot(cohort, ggplot2::aes(x = .data$z, y = .data$dimension)) +
    ggplot2::geom_rect(data = edges, inherit.aes = FALSE,
                       ggplot2::aes(xmin = .data$lo, xmax = .data$hi,
                                    ymin = as.numeric(.data$dimension) - 0.42,
                                    ymax = as.numeric(.data$dimension) + 0.42),
                       fill = "#EAF2F5") +
    ggplot2::geom_boxplot(width = 0.38, outlier.size = 0.6, outlier.colour = "#8A99A3",
                          colour = "#8A99A3", fill = NA) +
    ggplot2::geom_point(data = case, size = 4, colour = colour) +
    ggplot2::geom_text(data = case, ggplot2::aes(label = .data$value), vjust = -1.1,
                       size = base_size / 3.3, colour = "#04242F") +
    ggplot2::geom_vline(xintercept = 0, colour = "#04242F", linewidth = 0.3) +
    ggplot2::labs(x = "Standardised score (z); shaded: mid band", y = NULL,
                  # ASCII only: the base pdf device on Linux and macOS uses
                  # Latin-1, which has no em dash, and warns on every draw.
                  title = sprintf("#%s: %s", id, row$archetype)) +
    theme_neudata(base_size = base_size) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(colour = colour, face = "bold"))
}
