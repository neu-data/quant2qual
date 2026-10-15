test_that("plots build without warnings", {
  p <- fixture_profile()
  # Draw onto the base pdf device, which is what R CMD check uses.
  path <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit(grDevices::dev.off(), add = TRUE)
  plots <- list(
    map = plot_archetype_map(p),
    map_highlight = plot_archetype_map(p, "intensity", "friction", highlight = p$user_id[1:2]),
    signatures = plot_archetype_signatures(p),
    case = plot_case_profile(p, p$user_id[1]),
    pipeline = plot_q2q_pipeline()
  )
  for (nm in names(plots)) {
    g <- plots[[nm]]
    expect_s3_class(g, "ggplot")
    expect_no_warning(print(g), message = NULL)
    # The base pdf device on Linux and macOS is Latin-1 and warns on text it
    # cannot encode, while Windows (CP1252) draws the same text silently, so
    # the warning check above cannot catch it here. Keep labels ASCII.
    labs <- unlist(g$labels[vapply(g$labels, is.character, logical(1))])
    expect_false(any(grepl("[^\\x01-\\x7F]", labs, perl = TRUE)),
                 label = paste(nm, "has non-ASCII labels"))
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
