# ---------------------------------------------------------------------------
# Builds the quant2qual hex logo and the pkgdown favicons.
#
# The motif reads left to right, the way the package works: three bars (the
# numeric distribution) pass through a chevron and come out as a speech
# bubble holding lines of text (the narrative). The hex follows the Neudata
# sticker family: navy fill, teal border, white name, neu-data.com beneath.
#
# Run from the package root:  source("data-raw/make_logo.R")
# ---------------------------------------------------------------------------

library(grid)

navy <- "#04242F"
teal <- "#055F56"
teal_mid <- "#2E8B7F"
teal_light <- "#5FA39B"
seafoam <- "#9FD3CB"
blue <- "#6C8DB8"
white <- "#FFFFFF"

family <- if ("Segoe UI" %in% systemfonts::system_fonts()$family) "Segoe UI" else "sans"

hex_xy <- function(r, cx = 0.5, cy = 0.5) {
  a <- (seq(90, 390, by = 60)[1:6]) * pi / 180
  list(x = cx + r * cos(a) * (600 / 518), y = cy + r * sin(a))
}

draw_logo <- function() {
  grid.newpage()
  outer <- hex_xy(0.5)
  inner <- hex_xy(0.468)
  grid.polygon(outer$x, outer$y, gp = gpar(fill = teal, col = NA))
  grid.polygon(inner$x, inner$y, gp = gpar(fill = navy, col = NA))

  # --- numbers: three rising bars on a baseline
  base_y <- 0.445
  bars <- data.frame(x = c(0.215, 0.285, 0.355), h = c(0.085, 0.14, 0.205),
                     col = c(teal_light, teal_mid, seafoam))
  for (i in seq_len(nrow(bars))) {
    grid.roundrect(x = bars$x[i], y = base_y, width = 0.052, height = bars$h[i],
                   just = c("centre", "bottom"), r = unit(0.006, "npc"),
                   gp = gpar(fill = bars$col[i], col = NA))
  }
  grid.lines(c(0.175, 0.395), c(base_y, base_y), gp = gpar(col = blue, lwd = 5, lineend = "round"))

  # --- transformation: chevron
  grid.lines(c(0.44, 0.475, 0.44), c(0.585, 0.54, 0.495),
             gp = gpar(col = seafoam, lwd = 11, lineend = "round", linejoin = "round"))

  # --- narrative: speech bubble with lines of text
  bx <- 0.665
  by <- 0.565
  grid.roundrect(x = bx, y = by, width = 0.29, height = 0.205, r = unit(0.035, "npc"),
                 gp = gpar(fill = white, col = NA))
  grid.polygon(x = c(0.565, 0.6, 0.548), y = c(0.47, 0.47, 0.425),
               gp = gpar(fill = white, col = NA))
  line_w <- c(0.205, 0.17, 0.205, 0.12)
  line_y <- by + c(0.06, 0.02, -0.02, -0.06)
  line_col <- c(teal, teal_mid, teal, teal_light)
  for (i in seq_along(line_w)) {
    x0 <- bx - 0.1025
    grid.lines(c(x0, x0 + line_w[i]), c(line_y[i], line_y[i]),
               gp = gpar(col = line_col[i], lwd = 7, lineend = "round"))
  }

  # --- name and address
  grid.text("quant2qual", x = 0.5, y = 0.285,
            gp = gpar(col = white, fontsize = 64, fontface = "bold", fontfamily = family))
  grid.text("neu-data.com", x = 0.5, y = 0.175,
            gp = gpar(col = seafoam, fontsize = 22, fontfamily = family))
}

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
ragg::agg_png("man/figures/logo.png", width = 518, height = 600, background = "transparent", res = 72)
draw_logo()
invisible(grDevices::dev.off())

svglite_ok <- requireNamespace("svglite", quietly = TRUE)
if (svglite_ok) {
  svglite::svglite("man/figures/logo.svg", width = 518 / 72, height = 600 / 72, bg = "transparent")
  draw_logo()
  invisible(grDevices::dev.off())
}

# Favicons, cut from the PNG with magick so they match it exactly.
dir.create("pkgdown/favicon", showWarnings = FALSE, recursive = TRUE)
logo <- magick::image_read("man/figures/logo.png")
square <- function(size) {
  img <- magick::image_resize(logo, paste0("x", size))
  magick::image_extent(img, paste0(size, "x", size), color = "none", gravity = "center")
}
magick::image_write(square(180), "pkgdown/favicon/apple-touch-icon.png", format = "png")
magick::image_write(square(96), "pkgdown/favicon/favicon-96x96.png", format = "png")
magick::image_write(square(192), "pkgdown/favicon/web-app-manifest-192x192.png", format = "png")
magick::image_write(square(512), "pkgdown/favicon/web-app-manifest-512x512.png", format = "png")
magick::image_write(magick::image_join(square(16), square(32), square(48)),
                    "pkgdown/favicon/favicon.ico", format = "ico")
b64 <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}
writeLines(sprintf(paste0(
  '<svg xmlns="http://www.w3.org/2000/svg" width="96" height="96" viewBox="0 0 96 96">',
  '<image width="96" height="96" href="%s"/></svg>'), b64("pkgdown/favicon/favicon-96x96.png")),
  "pkgdown/favicon/favicon.svg")
writeLines(c(
  "{",
  '  "name": "quant2qual",',
  '  "short_name": "quant2qual",',
  '  "icons": [',
  '    {"src": "/web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "maskable"},',
  '    {"src": "/web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "maskable"}',
  "  ],",
  '  "theme_color": "#04242F",',
  '  "background_color": "#04242F",',
  '  "display": "standalone"',
  "}"
), "pkgdown/favicon/site.webmanifest")
