test_that("neudata_palette returns valid hex colours", {
  cols <- neudata_palette()
  expect_length(cols, 6)
  expect_true(all(grepl("^#[0-9A-F]{6}$", cols)))
  expect_equal(neudata_palette(1), "#055F56")
  expect_named(neudata_palette(2, named = TRUE), c("teal", "blue"))
})

test_that("neudata_palette errors when too many colours are requested", {
  expect_error(neudata_palette(7), "six|6")
})

test_that("theme and scales build", {
  expect_s3_class(theme_neudata(), "theme")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
    ggplot2::geom_point() +
    scale_colour_neudata() +
    theme_neudata()
  expect_silent(ggplot2::ggplot_build(p))
})
