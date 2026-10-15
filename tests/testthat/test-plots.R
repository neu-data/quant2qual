test_that("plots build without warnings", {
  p <- fixture_profile()
  # Draw onto the base pdf device, which is what R CMD check uses.
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (g in list(plot_archetype_map(p),
                 plot_archetype_map(p, "intensity", "friction", highlight = p$user_id[1:2]),
                 plot_archetype_signatures(p),
                 plot_case_profile(p, p$user_id[1]),
                 plot_q2q_pipeline())) {
    expect_s3_class(g, "ggplot")
    expect_silent(print(g))
  }
})

test_that("plot arguments are validated", {
  p <- fixture_profile()
  expect_error(plot_archetype_map(p, "velocity", "velocity"), class = "quant2qual_value_error")
  expect_error(plot_case_profile(p, "missing"), class = "quant2qual_value_error")
  expect_error(archetype_palette(mtcars), class = "quant2qual_type_error")
  pal <- archetype_palette(p)
  expect_named(pal, levels(p$archetype))
})

test_that("the pipeline table is complete", {
  pl <- q2q_pipeline()
  expect_setequal(unique(pl$stage), c("Configure", "Classify", "Narrate", "Report"))
  exported <- getNamespaceExports("quant2qual")
  expect_true(all(sub("\\(\\)$", "", pl$fn) %in% exported))
})
