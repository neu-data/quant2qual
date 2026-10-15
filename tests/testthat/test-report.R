test_that("markdown reports carry every section", {
  p <- fixture_profile()
  md <- write_behavioral_report(p, style = "brief")
  expect_type(md, "character")
  for (h in c("# Behavioral Narrative Report", "# Key findings", "# Cohort synthesis",
              "# Case summaries", "# Methods", "::: {.keyfindings}")) {
    expect_match(md, h, fixed = TRUE)
  }
  md2 <- write_behavioral_report(p, include_cohort = FALSE, include_methods = FALSE)
  expect_false(grepl("# Methods", md2, fixed = TRUE))
  expect_false(grepl("# Cohort synthesis", md2, fixed = TRUE))
})

test_that("source formats are written with figures beside them", {
  p <- fixture_profile()
  dir <- withr::local_tempdir()
  rmd <- write_behavioral_report(p, file = file.path(dir, "r.Rmd"), format = "rmd")
  txt <- readLines(rmd)
  expect_true(any(grepl("quant2qual::neudata_pdf", txt, fixed = TRUE)))
  expect_true(any(grepl("\\begin{keyfindings}", txt, fixed = TRUE)))
  expect_true(file.exists(file.path(dir, "r_figures", "archetype-map.png")))

  qmd <- write_behavioral_report(p, file = file.path(dir, "q.qmd"), format = "qmd")
  expect_true(any(grepl("^format:", readLines(qmd))))

  md <- write_behavioral_report(p, file = file.path(dir, "m.md"), format = "markdown",
                                ids = p$user_id[1:2])
  expect_true(any(grepl("q_figures|m_figures", readLines(md))))
})

test_that("bad arguments are caught before any work", {
  p <- fixture_profile()
  expect_error(write_behavioral_report(p, format = "docx"))
  expect_error(write_behavioral_report(p, title = 3), class = "quant2qual_type_error")
  expect_error(write_behavioral_report(mtcars), class = "quant2qual_type_error")
})

test_that("output formats build", {
  skip_if_not_installed("rmarkdown")
  expect_s3_class(neudata_pdf(), "rmarkdown_output_format")
  expect_s3_class(neudata_html(), "rmarkdown_output_format")
  expect_true(file.exists(system.file("neudata", "neudata.cls", package = "quant2qual")))
})

test_that("the PDF report renders end to end", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available("2.0"), "pandoc not available")
  skip_if(!nzchar(Sys.which("pdflatex")), "pdflatex not available")
  p <- fixture_profile()
  dir <- withr::local_tempdir()
  out <- write_behavioral_report(p, file = file.path(dir, "report.pdf"), format = "pdf",
                                 n_per_archetype = 1)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 20000)
})
