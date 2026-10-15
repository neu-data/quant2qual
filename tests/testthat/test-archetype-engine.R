test_that("mock_behavioral_data is reproducible and leaves the RNG alone", {
  set.seed(1)
  before <- stats::runif(1)
  set.seed(1)
  a <- mock_behavioral_data(50, seed = 3)
  after <- stats::runif(1)
  expect_identical(before, after)
  expect_identical(a, mock_behavioral_data(50, seed = 3))
  expect_named(a, c("user_id", "active_minutes", "action_clicks", "error_count"))
  expect_equal(a$user_id[1:3], 101:103)
  expect_true(all(a$active_minutes > 0))
})

test_that("mock data follows each domain's column names", {
  for (d in q2q_domains()$domain) {
    lx <- q2q_lexicon(d)
    x <- mock_behavioral_data(20, domain = d)
    expect_named(x, unname(lx$columns))
  }
  truth <- mock_behavioral_data(30, domain = "learning", include_truth = TRUE)$true_archetype
  expect_true(all(levels(truth) %in% default_archetypes("learning")$archetype))
})

test_that("identify_archetype recovers the simulated signatures", {
  users <- fixture_users()
  truth <- users$true_archetype
  p_rules <- identify_archetype(users)
  p_fuzzy <- identify_archetype(users, method = "fuzzy")
  expect_s3_class(p_rules, "q2q_profile")
  expect_gt(mean(p_rules$archetype == truth), 0.75)
  expect_gt(mean(p_fuzzy$archetype == truth), 0.75)
  expect_true(all(p_rules$confidence >= 0 & p_rules$confidence <= 1))
  expect_true(all(c("z_velocity", "pct_friction", "level_intensity", "pace",
                    "friction_rate", "outlier", "runner_up") %in% names(p_rules)))
})

test_that("rules mode honours the band rules exactly", {
  p <- fixture_profile()
  am <- attr(p, "q2q")$archetypes
  for (k in which(!am$fallback)) {
    rows <- p[p$archetype == am$archetype[k], ]
    for (d in q2q_dimensions()) {
      allowed <- strsplit(am[[d]][k], "|", fixed = TRUE)[[1]]
      expect_true(all(as.character(rows[[paste0("level_", d)]]) %in% allowed))
    }
  }
})

test_that("scaling options all produce a valid profile", {
  users <- fixture_users()[, 1:4]
  for (s in c("z", "robust", "percentile")) {
    p <- identify_archetype(users, scaling = s)
    expect_false(anyNA(p$archetype))
  }
  p <- identify_archetype(users, scaling = "robust", transform = "log1p")
  expect_equal(attr(p, "q2q")$transform, "log1p")
})

test_that("raw cutpoints set the friction bands", {
  users <- fixture_users()[, 1:4]
  p <- identify_archetype(users, cutpoints = list(friction = c(2, 8)))
  expect_true(all(p$level_friction[p$error_count > 8] == "high"))
  expect_true(all(p$level_friction[p$error_count < 2] == "low"))
  expect_equal(attr(p, "q2q")$reference$friction$edges_raw, c(2, 8))
})

test_that("custom matrices and column names work", {
  d <- data.frame(who = paste0("c", 1:30), t = c(1:29, 200), a = rev(1:30), e = rep(0:2, 10))
  m <- archetype_matrix(
    define_archetype("Sprinter", velocity = "low", intensity = "high"),
    define_archetype("Lingerer", velocity = "high", intensity = "low")
  )
  expect_equal(m$archetype[nrow(m)], "Baseline")
  p <- identify_archetype(d, velocity = "t", intensity = "a", friction = "e",
                          id = "who", archetypes = m)
  expect_setequal(levels(p$archetype), c("Sprinter", "Lingerer", "Baseline"))
  expect_equal(as.character(p$archetype[1]), "Sprinter")
})

test_that("defensive checks give classed errors", {
  users <- fixture_users()[, 1:4]
  expect_error(identify_archetype("x"), class = "quant2qual_type_error")
  expect_error(identify_archetype(users, velocity = "nope"), class = "quant2qual_missing_column")
  users_chr <- users
  users_chr$error_count <- as.character(users_chr$error_count)
  expect_error(identify_archetype(users_chr), class = "quant2qual_type_error")
  flat <- users
  flat$error_count <- 3L
  expect_error(identify_archetype(flat), class = "quant2qual_value_error")
  dup <- users
  dup$user_id[2] <- dup$user_id[1]
  expect_error(identify_archetype(dup), class = "quant2qual_value_error")
  expect_error(identify_archetype(users, cutpoints = list(friction = c(5, 1))),
               class = "quant2qual_value_error")
  expect_error(identify_archetype(users, threshold = -1), class = "quant2qual_value_error")
  expect_error(define_archetype("x", velocity = "low|high"), class = "quant2qual_value_error")
  expect_error(define_archetype("x", velocity = "huge"), class = "quant2qual_value_error")
  expect_error(archetype_matrix(define_archetype("a"), define_archetype("a")),
               class = "quant2qual_value_error")
})

test_that("missing values are reported and left unclassified", {
  users <- fixture_users()[, 1:4]
  users$active_minutes[c(3, 9)] <- NA
  expect_warning(p <- identify_archetype(users), class = "quant2qual_missing_values")
  expect_true(all(is.na(p$archetype[c(3, 9)])))
  expect_equal(sum(archetype_summary(p)$n), nrow(users) - 2)
})

test_that("re-classifying a profile replaces the derived columns", {
  p <- fixture_profile()
  p2 <- identify_archetype(p, method = "fuzzy")
  expect_equal(ncol(p2), ncol(p))
  expect_equal(attr(p2, "q2q")$method, "fuzzy")
})

test_that("summary, membership and subsetting keep the metadata", {
  p <- fixture_profile()
  s <- summary(p)
  expect_equal(sum(s$n), nrow(p))
  expect_equal(sum(s$share), 1)
  sub <- p[1:5, ]
  expect_s3_class(sub, "q2q_profile")
  m <- archetype_membership(sub)
  expect_equal(nrow(m), 5)
  expect_equal(m$user_id, sub$user_id)
  expect_output(print(p), "quant2qual profile")
  expect_s3_class(as.data.frame(p), "data.frame")
  expect_null(attr(as.data.frame(p), "q2q"))
})
