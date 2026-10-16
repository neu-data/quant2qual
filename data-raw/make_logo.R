# ---------------------------------------------------------------------------
# quant2qual hex logo: four concepts, one chosen for the package.
#
# Every concept tells the package's story -- numbers become narrative -- in a
# different visual voice, after the tidyverse sticker family:
#
#   confetti  midnight navy, multicolour data confetti that stacks into a
#             histogram and flows into a speech bubble          (tidyverse)
#   sunset    warm orange gradient, bar chart above, paragraph below, the
#             name held between two turning arrows               (tidyr)
#   paper     clean white, a blue connected-dot series that ends in a
#             speech bubble                                       (ggplot2)
#   quill     cream paper, a fountain pen whose line starts as a jagged data
#             trace and ends as cursive handwriting               (magrittr)
#
# Drawn with grid at 1039 x 1200 px (pointy-top hex, 2x the usual sticker
# size) on ragg, which supports the gradients and clipping used here.
#
# Run from the package root:   source("data-raw/make_logo.R")
# Choose the concept with:      Sys.setenv(Q2Q_LOGO = "confetti") before sourcing
# ---------------------------------------------------------------------------

library(grid)

W <- 1039
H <- 1200
CX <- W / 2
CY <- H / 2
R <- 600

have_font <- function(f) f %in% systemfonts::system_fonts()$family
pick_font <- function(...) {
  for (f in c(...)) if (have_font(f)) return(f)
  "sans"
}
SANS <- pick_font("Segoe UI", "Source Sans 3", "Helvetica")
GEO <- pick_font("Century Gothic", "Futura", "Segoe UI")
SERIF <- pick_font("Constantia", "Georgia", "serif")
HAND <- pick_font("Lucida Handwriting", "Segoe Script", "Segoe Print", "serif")

hex_xy <- function(r = R, cx = CX, cy = CY) {
  a <- (90 + 60 * 0:5) * pi / 180
  list(x = cx + r * cos(a), y = cy + r * sin(a))
}

# Open a pixel-coordinate viewport, draw the border, and return a clipped
# viewport for the interior. `border` is a fill (colour or gradient).
start_hex <- function(background, border, border_width = 34) {
  grid.newpage()
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))
  o <- hex_xy()
  grid.polygon(o$x, o$y, default.units = "native", gp = gpar(fill = border, col = NA))
  i <- hex_xy(R - border_width)
  inner <- polygonGrob(i$x, i$y, default.units = "native")
  pushViewport(viewport(xscale = c(0, W), yscale = c(0, H), clip = inner))
  grid.rect(gp = gpar(fill = background, col = NA))
}

end_hex <- function() {
  popViewport(2)
}

# Small address along the lower-right edge, as on the RStudio stickers.
edge_url <- function(label = "neu-data.com", col = "white", alpha = 0.75, size = 26) {
  grid.text(label, x = CX + 255, y = CY - 395, default.units = "native", rot = 30,
            gp = gpar(col = col, alpha = alpha, fontsize = size, fontfamily = SANS))
}

# A rounded speech bubble with a tail at the lower left.
speech_bubble <- function(x, y, w, h, fill, col = NA, lwd = 1, r = 34, tail = c(0.18, 0.06)) {
  grid.roundrect(x, y, w, h, r = unit(r / W, "snpc"), default.units = "native",
                 gp = gpar(fill = fill, col = col, lwd = lwd))
  tx <- x - w / 2 + w * tail[1]
  ty <- y - h / 2
  grid.polygon(c(tx - 6, tx + 44, tx - 34), c(ty + 4, ty + 4, ty - 52),
               default.units = "native", gp = gpar(fill = fill, col = NA))
  if (!is.na(col)) {
    grid.lines(c(tx - 6, tx - 34, tx + 44), c(ty + 1, ty - 52, ty + 1),
               default.units = "native", gp = gpar(col = col, lwd = lwd, linejoin = "round"))
  }
}

text_lines <- function(x0, y0, widths, cols, gap = 36, lwd = 13) {
  for (i in seq_along(widths)) {
    yy <- y0 - (i - 1) * gap
    grid.lines(c(x0, x0 + widths[i]), c(yy, yy), default.units = "native",
               gp = gpar(col = cols[(i - 1) %% length(cols) + 1], lwd = lwd, lineend = "round"))
  }
}

# ---- 1. confetti ------------------------------------------------------------

draw_confetti <- function() {
  bg <- radialGradient(c("#1D3557", "#0B1626"), cx1 = 0.5, cy1 = 0.62, r2 = 0.75)
  border <- linearGradient(c("#2EC4B6", "#0B6E73"), x1 = 0, y1 = 1, x2 = 1, y2 = 0)
  start_hex(bg, border)
  pal <- c("#FF6B6B", "#FFD93D", "#6BCB77", "#4D96FF", "#C77DFF", "#FF9F45", "#2EC4B6")

  set.seed(42)
  n <- 340
  grid.points(stats::runif(n, 0, W), stats::runif(n, 0, H), default.units = "native",
              pch = 16, size = unit(stats::runif(n, 3, 10), "native"),
              gp = gpar(col = sample(pal, n, TRUE), alpha = stats::runif(n, 0.25, 0.75)))

  # data: dots stacking into a histogram
  counts <- c(2, 4, 6, 5, 3)
  for (j in seq_along(counts)) {
    for (k in seq_len(counts[j])) {
      grid.circle(170 + (j - 1) * 44, 690 + (k - 1) * 40, r = 17, default.units = "native",
                  gp = gpar(fill = pal[(j + k) %% length(pal) + 1], col = NA))
    }
  }
  grid.lines(c(140, 380), c(662, 662), default.units = "native",
             gp = gpar(col = "white", alpha = 0.6, lwd = 6, lineend = "round"))
  # the flow: a trail of shrinking dots arcing into the bubble
  t <- seq(0, 1, length.out = 8)
  grid.circle(405 + 135 * t, 860 + 55 * sin(pi * t), r = 15 - 8 * t, default.units = "native",
              gp = gpar(fill = pal[c(5, 4, 7, 3, 2, 6, 1, 5)], col = NA, alpha = 0.95))
  # narrative: the bubble
  speech_bubble(735, 820, 330, 240, fill = "white", r = 44)
  text_lines(605, 895, c(260, 205, 260, 150), c("#4D96FF", "#FF6B6B", "#6BCB77", "#C77DFF"),
             gap = 40, lwd = 14)

  grid.text("quant2qual", CX, 480, default.units = "native",
            gp = gpar(col = "white", fontsize = 158, fontfamily = SANS, fontface = "bold"))
  grid.text("numbers into narrative", CX, 372, default.units = "native",
            gp = gpar(col = "#9FE3DC", fontsize = 40, fontfamily = SANS))
  edge_url()
  end_hex()
}

# ---- 2. sunset ------------------------------------------------------------------

draw_sunset <- function() {
  bg <- linearGradient(c("#FFC24B", "#F7871E", "#E8561B"), x1 = 0.5, y1 = 1, x2 = 0.5, y2 = 0)
  start_hex(bg, "#B9420F")
  white <- "#FFFFFF"

  # top: bar chart with one highlighted bar
  base <- 830
  hs <- c(70, 120, 175, 105)
  for (i in seq_along(hs)) {
    grid.rect(395 + (i - 1) * 82, base, 56, hs[i], just = c("centre", "bottom"),
              default.units = "native",
              gp = gpar(fill = if (i == 3) "#7A2A07" else white, col = NA, alpha = if (i == 3) 0.85 else 1))
  }
  grid.lines(c(345, 695), c(base - 6, base - 6), default.units = "native",
             gp = gpar(col = white, lwd = 8, lineend = "round"))

  # middle: name between two turning arrows
  arc <- function(cx, from, to, flip) {
    a <- seq(from, to, length.out = 60) * pi / 180
    x <- cx + flip * 150 * cos(a) * 0.55
    y <- 545 + 150 * sin(a)
    grid.lines(x, y, default.units = "native",
               gp = gpar(col = "#7A2A07", lwd = 9, lineend = "round"),
               arrow = arrow(angle = 28, length = unit(30, "native"), type = "closed"))
  }
  arc(CX - 365, 230, 130, 1)
  arc(CX + 365, 50, -50, 1)
  grid.text("quant2qual", CX, 545, default.units = "native",
            gp = gpar(col = white, fontsize = 128, fontfamily = SANS, fontface = "bold"))

  # bottom: the paragraph it becomes
  grid.roundrect(CX, 300, 300, 170, r = unit(18 / W, "snpc"), default.units = "native",
                 gp = gpar(fill = white, col = NA))
  grid.text("Aa", CX - 95, 300, default.units = "native",
            gp = gpar(col = "#E8561B", fontsize = 70, fontfamily = SERIF, fontface = "bold"))
  text_lines(CX - 25, 345, c(140, 115, 140), c("#E8561B", "#F7871E", "#B9420F"), gap = 45, lwd = 14)
  edge_url(col = "white", alpha = 0.85)
  end_hex()
}

# ---- 3. paper -------------------------------------------------------------------

draw_paper <- function() {
  start_hex("#FFFFFF", "#3C3C3C", border_width = 38)
  for (gx in seq(60, W, by = 95)) {
    grid.lines(c(gx, gx), c(0, H), default.units = "native", gp = gpar(col = "#ECECEC", lwd = 2))
  }
  for (gy in seq(40, H, by = 95)) {
    grid.lines(c(0, W), c(gy, gy), default.units = "native", gp = gpar(col = "#ECECEC", lwd = 2))
  }
  px <- c(95, 235, 375, 500, 615)
  py <- c(705, 790, 735, 880, 820)
  blues <- c("#BBDDF5", "#79B7EA", "#3C8DD8", "#1C5FAE", "#0B3474")
  grid.lines(px, py, default.units = "native", gp = gpar(col = "#1F2A38", lwd = 9, linejoin = "round"))
  grid.lines(c(px[5], 690), c(py[5], 865), default.units = "native",
             gp = gpar(col = "#1F2A38", lwd = 9, lty = "22", lineend = "round"))
  grid.circle(px, py, r = 32, default.units = "native", gp = gpar(fill = blues, col = NA))

  speech_bubble(800, 900, 250, 190, fill = "#0B3474", r = 36, tail = c(0.12, 0.06))
  text_lines(705, 955, c(190, 150, 190), c("#BBDDF5", "#79B7EA", "#FFFFFF"), gap = 42, lwd = 13)

  grid.text("quant2qual", CX, 470, default.units = "native",
            gp = gpar(col = "#3C3C3C", fontsize = 142, fontfamily = GEO))
  grid.text("numbers into narrative", CX, 365, default.units = "native",
            gp = gpar(col = "#1C5FAE", fontsize = 38, fontfamily = GEO))
  edge_url(col = "#3C3C3C", alpha = 0.7)
  end_hex()
}

# ---- 4. quill -------------------------------------------------------------------

# A fountain-pen nib, tip at (0, 0), pointing down, scaled by s and rotated.
nib <- function(x, y, s, rot, body = "#2B2B2B", metal = "#C9A227") {
  rot_xy <- function(px, py) {
    a <- rot * pi / 180
    list(x = x + s * (px * cos(a) - py * sin(a)), y = y + s * (px * sin(a) + py * cos(a)))
  }
  shape <- rot_xy(c(0, 0.30, 0.42, 0.40, -0.40, -0.42, -0.30),
                  c(0, 0.55, 1.00, 1.30, 1.30, 1.00, 0.55))
  grid.polygon(shape$x, shape$y, default.units = "native", gp = gpar(fill = metal, col = "#7A5C0E", lwd = 4))
  slit <- rot_xy(c(0, 0), c(0.02, 0.78))
  grid.lines(slit$x, slit$y, default.units = "native", gp = gpar(col = "#5A430A", lwd = 4))
  hole <- rot_xy(0, 0.8)
  grid.circle(hole$x, hole$y, r = 0.06 * s, default.units = "native", gp = gpar(fill = "#5A430A", col = NA))
  barrel <- rot_xy(c(-0.44, 0.44, 0.40, -0.40), c(1.30, 1.30, 2.6, 2.6))
  grid.polygon(barrel$x, barrel$y, default.units = "native", gp = gpar(fill = body, col = NA))
  band <- rot_xy(c(-0.45, 0.45, 0.45, -0.45), c(1.28, 1.28, 1.42, 1.42))
  grid.polygon(band$x, band$y, default.units = "native", gp = gpar(fill = metal, col = NA))
}

draw_quill <- function() {
  bg <- radialGradient(c("#FBF4E2", "#EAD9B3"), cx1 = 0.5, cy1 = 0.55, r2 = 0.8)
  start_hex(bg, "#3B2A1A", border_width = 36)
  ink <- "#2E3A59"

  # The line: a jagged data trace that relaxes into cursive loops.
  base <- 770
  data_x <- c(95, 135, 170, 210, 245, 285, 320)
  data_y <- c(730, 815, 705, 850, 755, 825, base)
  grid.lines(data_x, data_y, default.units = "native",
             gp = gpar(col = ink, lwd = 9, linejoin = "mitre"))
  grid.circle(data_x[-7], data_y[-7], r = 13, default.units = "native",
              gp = gpar(fill = "#C0392B", col = NA))
  # A prolate cycloid loops whenever its radius exceeds its rolling step,
  # which gives the "e e e" of joined-up handwriting.
  u <- seq(0, 2 * pi * 3, length.out = 600)
  step <- 350 / (2 * pi * 3)
  grid.lines(320 + step * u - 38 * sin(u), base + 42 * (1 - cos(u)),
             default.units = "native", gp = gpar(col = ink, lwd = 9, lineend = "round"))
  # The barrel deliberately runs off the edge, as the tools do on the
  # dplyr sticker.
  nib(x = 670, y = base, s = 110, rot = -40)

  grid.text("quant2qual", CX, 490, default.units = "native",
            gp = gpar(col = "#3B2A1A", fontsize = 150, fontfamily = SERIF, fontface = "italic"))
  grid.text("Ceci n'est pas un tableau", CX, 375, default.units = "native",
            gp = gpar(col = "#6B4F2E", fontsize = 40, fontfamily = HAND))
  edge_url(col = "#3B2A1A", alpha = 0.7)
  end_hex()
}

# ---- render ----------------------------------------------------------------------

concepts <- list(confetti = draw_confetti, sunset = draw_sunset,
                 paper = draw_paper, quill = draw_quill)

render_logo <- function(fn, path, scale = 1) {
  ragg::agg_png(path, width = W * scale, height = H * scale, background = "transparent",
                res = 72 * scale)
  fn()
  invisible(grDevices::dev.off())
}

out_dir <- Sys.getenv("Q2Q_LOGO_DIR", "")
if (nzchar(out_dir)) {
  # Preview mode: every concept to a scratch folder, nothing in the package.
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  for (nm in names(concepts)) render_logo(concepts[[nm]], file.path(out_dir, paste0(nm, ".png")))
} else {
  chosen <- Sys.getenv("Q2Q_LOGO", "confetti")
  stopifnot(chosen %in% names(concepts))
  dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
  render_logo(concepts[[chosen]], "man/figures/logo.png", scale = 0.5)
  render_logo(concepts[[chosen]], "man/figures/logo-hires.png", scale = 1)

  # Favicons, cut from the high-resolution logo so they match it exactly.
  dir.create("pkgdown/favicon", showWarnings = FALSE, recursive = TRUE)
  logo <- magick::image_read("man/figures/logo-hires.png")
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
}
