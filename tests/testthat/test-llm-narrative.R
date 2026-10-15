# A stand-in for an ellmer chat: records what it is sent and returns a
# canned structured response, so the LLM path is tested without a network.
fake_chat <- function(response = NULL, fail = FALSE) {
  e <- new.env()
  e$system_prompt <- NULL
  e$prompts <- character()
  e$clone <- function() e
  e$set_system_prompt <- function(value) {
    e$system_prompt <- value
    invisible(e)
  }
  e$get_model <- function() "fake-model"
  e$chat_structured <- function(prompt, type) {
    e$prompts <- c(e$prompts, prompt)
    if (fail) stop("connection refused")
    if (is.function(response)) response(prompt) else response
  }
  e
}

# A response built from the rule-based draft: every number in it is grounded.
echo_draft <- function(profile, id) {
  d <- generate_narrative(profile, id)
  list(presentation = d$sections$presentation, evidence = d$sections$evidence,
       interpretation = d$sections$interpretation,
       implications = as.list(d$sections$implications),
       confidence = d$sections$confidence)
}

test_that("the fact sheet carries the case's statistics", {
  p <- fixture_profile()
  id <- p$user_id[1]
  f <- narrative_facts(p, id)
  expect_s3_class(f, "q2q_facts")
  expect_match(f, paste0("Case Study ", id), fixed = TRUE)
  expect_match(f, as.character(p$archetype[1]), fixed = TRUE)
  expect_match(f, "percentile rank")
  expect_true(format(p$action_clicks[1]) %in% attr(f, "numbers") ||
                p$action_clicks[1] <= 10)
  expect_output(print(f), "Dimensions, most salient first")
  expect_error(narrative_facts(p, c(1, 2)), class = "quant2qual_value_error")
})

test_that("number extraction normalises and ignores small integers", {
  ex <- quant2qual:::extract_numbers
  expect_setequal(ex("z = +2.70, 1,200 actions, 3 errors, per 100, 45.5 minutes"),
                  c("2.7", "1200", "45.5"))
  expect_length(ex("no numbers here"), 0)
})

test_that("a grounded LLM narrative keeps the narrative shape", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  id <- p$user_id[3]
  chat <- fake_chat(echo_draft(p, id))
  n <- generate_llm_narrative(p, id, chat = chat, instructions = "Audience: product team.")
  expect_s3_class(n, "q2q_narrative")
  expect_equal(n$engine, "llm")
  expect_equal(n$model, "fake-model")
  expect_length(n$ungrounded, 0)
  expect_s3_class(n$draft, "q2q_narrative")
  expect_named(n$sections, c("presentation", "evidence", "interpretation",
                             "implications", "confidence"))
  expect_match(chat$system_prompt, "Cite only numbers that appear in the fact sheet")
  expect_match(chat$system_prompt, "Audience: product team.", fixed = TRUE)
  expect_match(chat$prompts[1], "FACT SHEET")
  expect_match(chat$prompts[1], "DRAFT")
  expect_match(format(n), paste0("Case Study ID: #", id), fixed = TRUE)
  expect_output(print(n), "fake-model")
})

test_that("invented numbers are flagged", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  id <- p$user_id[3]
  resp <- echo_draft(p, id)
  resp$evidence <- paste(resp$evidence, "The participant also abandoned 4242 carts.")
  expect_warning(n <- generate_llm_narrative(p, id, chat = fake_chat(resp)),
                 class = "quant2qual_llm_ungrounded")
  expect_equal(n$ungrounded, "4242")
})

test_that("Markdown in model output is cleaned", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  id <- p$user_id[2]
  resp <- echo_draft(p, id)
  resp$interpretation <- paste0("**", resp$interpretation, "**")
  resp$implications <- list(paste("-", resp$implications[[1]]))
  n <- generate_llm_narrative(p, id, chat = fake_chat(resp))
  expect_false(grepl("**", n$sections$interpretation, fixed = TRUE))
  expect_false(startsWith(n$sections$implications[1], "-"))
})

test_that("failures fall back to the rule-based narrative", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  id <- p$user_id[1]
  expect_warning(n <- generate_llm_narrative(p, id, chat = fake_chat(fail = TRUE)),
                 class = "quant2qual_llm_fallback")
  expect_equal(n$engine, "rules")
  expect_identical(n$sections, generate_narrative(p, id)$sections)
  expect_error(generate_llm_narrative(p, id, chat = fake_chat(fail = TRUE), fallback = FALSE),
               class = "quant2qual_llm_error")
})

test_that("brief style and several ids work", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  ids <- p$user_id[1:2]
  chat <- fake_chat(function(prompt) list(summary = "A short summary."))
  set <- generate_llm_narrative(p, ids, style = "brief", chat = chat)
  expect_s3_class(set, "q2q_narrative_set")
  expect_length(chat$prompts, 2)
  expect_equal(set[[1]]$sections$summary, "A short summary.")
})

test_that("arguments are validated", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  expect_error(generate_llm_narrative(p, p$user_id[1], chat = list()),
               class = "quant2qual_type_error")
  expect_error(generate_llm_narrative(p, "nope", chat = fake_chat()),
               class = "quant2qual_value_error")
  expect_error(generate_llm_narrative(p, p$user_id[1], chat = fake_chat(),
                                      reasoning_effort = "extreme"))
})

test_that("reports can use the LLM narrator and say so", {
  skip_if_not_installed("ellmer")
  p <- fixture_profile()
  ids <- p$user_id[1:2]
  chat <- fake_chat(function(prompt) {
    id <- sub(".*Case: Case Study ([^ ]+) .*", "\\1", prompt)
    echo_draft(p, as.integer(id))
  })
  md <- write_behavioral_report(p, ids = ids, narrator = "llm",
                                llm_args = list(chat = chat))
  expect_match(md, "rewritten by fake-model", fixed = TRUE)
  rules <- write_behavioral_report(p, ids = ids)
  expect_match(rules, "No generative language model was used", fixed = TRUE)
  expect_error(write_behavioral_report(p, narrator = "llm", llm_args = "x"),
               class = "quant2qual_type_error")
})
