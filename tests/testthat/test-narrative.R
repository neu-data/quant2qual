test_that("a clinical narrative cites the case and its statistics", {
  p <- fixture_profile()
  id <- p$user_id[1]
  n <- generate_narrative(p, id)
  expect_s3_class(n, "q2q_narrative")
  expect_equal(n$id, id)
  expect_named(n$sections, c("presentation", "evidence", "interpretation",
                             "implications", "confidence"))
  md <- format(n)
  expect_match(md, paste0("## Case Study ID: #", id), fixed = TRUE)
  expect_match(md, as.character(p$archetype[1]), fixed = TRUE)
  expect_match(n$sections$evidence, "z = [+-]")
  expect_match(n$sections$confidence, "relative to the present cohort")
  expect_false(grepl("{", md, fixed = TRUE))
})

test_that("narratives are deterministic", {
  p <- fixture_profile()
  id <- p$user_id[7]
  expect_identical(format(generate_narrative(p, id)), format(generate_narrative(p, id)))
})

test_that("styles differ in register", {
  p <- fixture_profile()
  id <- p$user_id[2]
  ux <- generate_narrative(p, id, style = "ux")
  brief <- generate_narrative(p, id, style = "brief")
  expect_false(grepl("z = ", ux$sections$evidence))
  expect_match(format(ux), "Recommended actions")
  expect_named(brief$sections, "summary")
  expect_match(brief$sections$summary, "Confidence:")
})

test_that("several ids give a narrative set", {
  p <- fixture_profile()
  ids <- select_cases(p)
  expect_length(ids, nlevels(droplevels(p$archetype)))
  set <- generate_narrative(p, ids, style = "brief")
  expect_s3_class(set, "q2q_narrative_set")
  expect_length(set, length(ids))
  expect_s3_class(set[1:2], "q2q_narrative_set")
  expect_type(as.character(set), "character")
})

test_that("select_cases strategies pick what they promise", {
  p <- fixture_profile()
  rep <- select_cases(p, strategy = "representative")
  bord <- select_cases(p, strategy = "borderline")
  conf <- stats::setNames(p$confidence, p$user_id)
  expect_gte(mean(conf[as.character(rep)]), mean(conf[as.character(bord)]))
  expect_length(select_cases(p, n = 2, archetypes = "Power Navigator"), 2)
  expect_error(select_cases(p, archetypes = "Nobody"), class = "quant2qual_value_error")
})

test_that("unknown ids and plain data frames are rejected", {
  p <- fixture_profile()
  expect_error(generate_narrative(p, "no-such-id"), class = "quant2qual_value_error")
  expect_error(generate_narrative(p), class = "quant2qual_value_error")
  expect_error(generate_narrative(as.data.frame(p), 101), class = "quant2qual_type_error")
})

test_that("every domain narrates in its own vocabulary", {
  for (d in c("learning", "survey")) {
    users <- mock_behavioral_data(150, domain = d, seed = 5)
    p <- identify_archetype(users, domain = d)
    n <- generate_narrative(p, select_cases(p)[1])
    expect_match(format(n), q2q_lexicon(d)$subject, fixed = TRUE)
    expect_match(format(n), q2q_lexicon(d)$case_label, fixed = TRUE)
  }
})

test_that("lexicon overrides reach the prose", {
  p <- fixture_profile()
  lx <- q2q_lexicon("ux", subject = "the clinician", subject_short = "clinician",
                    subject_plural = "clinicians")
  n <- generate_narrative(p, p$user_id[1], lexicon = lx)
  expect_match(format(n), "clinician")
  expect_error(q2q_lexicon("nope"), class = "quant2qual_value_error")
})

test_that("the cohort narrative describes the distribution", {
  p <- fixture_profile()
  cn <- generate_cohort_narrative(p)
  expect_s3_class(cn, "q2q_cohort_narrative")
  expect_match(cn$sections$overview, "240 participants")
  expect_match(cn$sections$relationships, "Spearman|independent")
  expect_length(cn$portraits, sum(archetype_summary(p)$n > 0))
  expect_match(format(cn), "### Archetype portraits", fixed = TRUE)
  expect_output(print(cn), "Cohort synthesis")
})

test_that("language helpers read naturally", {
  expect_equal(quant2qual:::ordinal(c(1, 2, 3, 4, 11, 12, 13, 21, 22, 101)),
               c("1st", "2nd", "3rd", "4th", "11th", "12th", "13th", "21st", "22nd", "101st"))
  expect_equal(quant2qual:::unit_for(1, "errors"), "error")
  expect_equal(quant2qual:::unit_for(1, "activities"), "activity")
  expect_equal(quant2qual:::unit_for(1, "failed attempts"), "failed attempt")
  expect_equal(quant2qual:::unit_for(2, "errors"), "errors")
  expect_match(quant2qual:::ratio_phrase(20, 10), "twice")
  expect_match(quant2qual:::ratio_phrase(10, 10), "close to")
  expect_match(quant2qual:::ratio_phrase(2, 10), "fifth")
  expect_match(quant2qual:::percentile_phrase(0.9, "a"), "10")
  expect_equal(quant2qual:::collapse_and(c("a", "b", "c")), "a, b and c")
})
