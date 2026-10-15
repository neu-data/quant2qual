#' Neudata colour palette
#'
#' Brand colours sampled from the Neudata logo, ordered for use as a
#' categorical palette.
#'
#' @param n Number of colours to return. Defaults to all six.
#' @param named Return a named vector?
#'
#' @return A character vector of hex colours.
#' @export
#' @examples
#' neudata_palette()
#' neudata_palette(3)
neudata_palette <- function(n = 6, named = FALSE) {
  cols <- c(
    teal       = "#055F56",
    blue       = "#0B376C",
    navy       = "#04242F",
    teal_light = "#5FA39B",
    blue_light = "#6C8DB8",
    grey       = "#8A99A3"
  )
  if (n > length(cols)) {
    stop("The Neudata palette has ", length(cols), " colours; requested ", n, ".", call. = FALSE)
  }
  out <- cols[seq_len(n)]
  if (named) out else unname(out)
}

#' Neudata ggplot2 theme and scales
#'
#' A clean minimal theme and discrete colour/fill scales using
#' [neudata_palette()].
#'
#' @param base_size Base font size.
#' @param ... Passed to [ggplot2::scale_colour_manual()] or
#'   [ggplot2::scale_fill_manual()].
#'
#' @return A ggplot2 theme or scale.
#' @export
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
#'   geom_point(size = 2) +
#'   scale_colour_neudata(name = "Cylinders") +
#'   theme_neudata()
theme_neudata <- function(base_size = 11) {
  navy <- "#04242F"
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold", colour = navy),
      plot.subtitle    = ggplot2::element_text(colour = "#055F56"),
      axis.title       = ggplot2::element_text(colour = navy),
      strip.text       = ggplot2::element_text(face = "bold", colour = navy),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    )
}

#' @rdname theme_neudata
#' @export
scale_colour_neudata <- function(...) {
  ggplot2::scale_colour_manual(values = neudata_palette(), ...)
}

#' @rdname theme_neudata
#' @export
scale_fill_neudata <- function(...) {
  ggplot2::scale_fill_manual(values = neudata_palette(), ...)
}
