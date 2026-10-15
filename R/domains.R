# ---------------------------------------------------------------------------
# Domain packs.
#
# The engine is domain-free: it sees three standardised dimensions and a rule
# matrix. What makes a narrative read like a usability case study rather than
# a learning-analytics note or a survey-quality audit is vocabulary and
# interpretation, and both live here. A domain pack is two things:
#
#   * a lexicon   - what the subject and each dimension are called, the units,
#                   the default column names, and how a low / mid / high
#                   reading of each dimension is described;
#   * archetypes  - the matrix of behavioral signatures, each with a persona,
#                   an opening line, an interpretive paragraph and implications.
#
# The three dimensions are fixed by design and named for what they measure:
#   velocity  - time invested            (session minutes, time on task)
#   intensity - volume of activity       (clicks, activities, words)
#   friction  - signs of difficulty      (errors, failed attempts, QA flags)
# ---------------------------------------------------------------------------

#' The behavioral dimensions used by quant2qual
#'
#' @return A character vector: `velocity`, `intensity` and `friction`.
#' @export
#' @examples
#' q2q_dimensions()
q2q_dimensions <- function() {
  c("velocity", "intensity", "friction")
}

#' Domains shipped with quant2qual
#'
#' Lists the built-in domain packs with the subject noun, the default column
#' names and the archetypes each one defines.
#'
#' @return A [tibble][tibble::tibble] with one row per domain.
#' @export
#' @examples
#' q2q_domains()
q2q_domains <- function() {
  purrr::map_dfr(names(domain_registry), function(d) {
    lx <- q2q_lexicon(d)
    am <- default_archetypes(d)
    tibble::tibble(
      domain     = d,
      label      = lx$label,
      subject    = lx$subject,
      velocity   = lx$columns[["velocity"]],
      intensity  = lx$columns[["intensity"]],
      friction   = lx$columns[["friction"]],
      archetypes = paste(am$archetype, collapse = ", ")
    )
  })
}

#' Vocabulary for a behavioral domain
#'
#' A lexicon tells the narrative engine how to talk about a dataset: what the
#' subject is called, what each dimension measures and in which units, and
#' how a low, typical or high reading of each dimension should be described.
#' Every element can be overridden, so a built-in domain can be adapted to a
#' specific product or study without writing a new one.
#'
#' @param domain One of [q2q_domains()]`$domain`: `"ux"`, `"learning"` or
#'   `"survey"`.
#' @param ... Named overrides, merged recursively into the lexicon. For
#'   example `subject = "the patient"` or
#'   `dimensions = list(friction = list(unit = "alarms"))`.
#'
#' @return An object of class `q2q_lexicon`: a named list with elements
#'   `domain`, `label`, `subject`, `subject_short`, `subject_plural`,
#'   `case_label`, `columns`, `dimensions`, `pace` and `rate`.
#' @export
#' @examples
#' lx <- q2q_lexicon("ux")
#' lx$dimensions$friction$noun
#'
#' # Adapt the UX pack to a clinical-software study
#' q2q_lexicon("ux", subject = "the clinician", subject_short = "clinician",
#'             dimensions = list(intensity = list(unit = "orders entered")))
q2q_lexicon <- function(domain = "ux", ...) {
  check_string(domain)
  if (!domain %in% names(domain_registry)) {
    q2q_abort(c("Unknown domain {.val {domain}}.",
                i = "Available domains: {.val {names(domain_registry)}}."),
              class = "quant2qual_value_error")
  }
  lx <- domain_registry[[domain]]$lexicon
  over <- rlang::list2(...)
  if (length(over) > 0L) {
    if (is.null(names(over)) || any(!nzchar(names(over)))) {
      q2q_abort("Lexicon overrides must be named.", class = "quant2qual_value_error")
    }
    lx <- utils::modifyList(lx, over)
  }
  structure(lx, class = "q2q_lexicon")
}

#' @export
print.q2q_lexicon <- function(x, ...) {
  cli_print({
    cli::cli_text("{.strong quant2qual lexicon} {.val {x$domain}} {.emph ({x$label})}")
    cli::cli_bullets(c(
      "*" = "Subject: {x$subject}",
      "*" = "Velocity:  {x$dimensions$velocity$noun} ({x$dimensions$velocity$unit}) <- {.field {x$columns[['velocity']]}}",
      "*" = "Intensity: {x$dimensions$intensity$noun} ({x$dimensions$intensity$unit}) <- {.field {x$columns[['intensity']]}}",
      "*" = "Friction:  {x$dimensions$friction$noun} ({x$dimensions$friction$unit}) <- {.field {x$columns[['friction']]}}"
    ))
  })
  invisible(x)
}

#' Built-in archetype matrix for a domain
#'
#' Returns the archetype matrix shipped with a domain pack. All three packs
#' share one rule geometry -- friction-led signatures are checked first, then
#' the time/activity quadrants, then a cohort-typical fallback -- but each
#' names and interprets the signatures in its own terms.
#'
#' | Signature | Velocity | Intensity | Friction | UX | Learning | Survey |
#' |---|---|---|---|---|---|---|
#' | Obstructed persistence | mid/high | high | high | Frustrated Fighter | Struggling Persister | Effortful Struggler |
#' | Early exit under friction | low/mid | low/mid | high | Frustrated Novice | At-Risk Withdrawer | Careless Speeder |
#' | Deliberate depth | high | low/mid | low/mid | Methodical Explorer | Deep Learner | Deliberative Responder |
#' | Fluent expertise | low/mid | high | low/mid | Power Navigator | Efficient Achiever | Efficient Responder |
#' | Shallow contact | low | low | low/mid | Passive Disengaged | Surface Engager | Minimal Responder |
#' | Cohort-typical | (fallback) | | | Baseline User | Steady Learner | Typical Responder |
#'
#' @param domain One of `"ux"`, `"learning"` or `"survey"`.
#'
#' @return A `q2q_archetype_matrix`; see [archetype_matrix()].
#' @export
#' @seealso [define_archetype()] to build your own.
#' @examples
#' default_archetypes("ux")
#' default_archetypes("learning")$archetype
default_archetypes <- function(domain = "ux") {
  check_string(domain)
  if (!domain %in% names(domain_registry)) {
    q2q_abort(c("Unknown domain {.val {domain}}.",
                i = "Available domains: {.val {names(domain_registry)}}."),
              class = "quant2qual_value_error")
  }
  specs <- domain_registry[[domain]]$archetypes
  rows <- lapply(specs, function(s) do.call(define_archetype, s))
  archetype_matrix(!!!rows, domain = domain)
}

# ---- shared rule geometry ----------------------------------------------------

rule_geometry <- list(
  obstructed = list(velocity = "mid|high", intensity = "high",    friction = "high"),
  early_exit = list(velocity = "low|mid",  intensity = "low|mid", friction = "high"),
  deliberate = list(velocity = "high",     intensity = "low|mid", friction = "low|mid"),
  fluent     = list(velocity = "low|mid",  intensity = "high",    friction = "low|mid"),
  shallow    = list(velocity = "low",      intensity = "low",     friction = "low|mid")
)

#' @noRd
spec <- function(geometry, ...) {
  g <- if (is.null(geometry)) list() else rule_geometry[[geometry]]
  c(list(...), g)
}

# ---- UX / product analytics ------------------------------------------------

ux_lexicon <- list(
  domain = "ux",
  label = "Product and UX analytics",
  subject = "the participant",
  subject_short = "participant",
  subject_plural = "participants",
  case_label = "Case Study",
  columns = c(id = "user_id", velocity = "active_minutes",
              intensity = "action_clicks", friction = "error_count"),
  dimensions = list(
    velocity = list(
      noun = "session duration", unit = "minutes", label = "Velocity (time)",
      readings = list(
        high = c("a sustained willingness to stay with the task",
                 "an extended investment of time in the product",
                 "a prolonged period of engagement"),
        mid  = c("a time investment typical of the cohort",
                 "an ordinary session length"),
        low  = c("a brief, compressed period of engagement",
                 "limited time spent in the product",
                 "a short-lived visit")
      )
    ),
    intensity = list(
      noun = "interaction volume", unit = "actions", label = "Intensity (actions)",
      readings = list(
        high = c("a dense, rapid stream of interface actions",
                 "heavy, sustained interaction with the interface",
                 "a high-throughput interaction style"),
        mid  = c("an interaction volume in keeping with peers",
                 "a conventional amount of interface activity"),
        low  = c("sparse, deliberate interaction",
                 "a restrained interaction footprint",
                 "minimal hands-on activity")
      )
    ),
    friction = list(
      noun = "error events", plural = TRUE, unit = "errors", label = "Friction (errors)",
      readings = list(
        high = c("repeated collisions with systemic roadblocks",
                 "a recurring pattern of failed or rejected actions",
                 "persistent friction at the level of the interface"),
        mid  = c("an ordinary level of interaction friction",
                 "the occasional slip expected of any session"),
        low  = c("smooth, largely error-free progress",
                 "an unusually clean interaction record",
                 "near-frictionless movement through the product")
      )
    )
  ),
  pace = list(noun = "interaction cadence", unit = "actions per minute"),
  rate = list(noun = "error rate", unit = "errors per 100 actions")
)

ux_archetypes <- list(
  spec("obstructed",
    name = "Frustrated Fighter", icon = "\U0001F9D7", colour = "#C0504D",
    theme = "obstructed persistence",
    persona = "An engaged but deeply stuck user struggling against system barriers, exhibiting high friction and repetitive loops.",
    headline = "{Subject} exhibited a state of high interaction friction.",
    interpretation = paste(
      "Taken together, the profile describes effortful persistence under obstruction rather than disengagement:",
      "{subject} kept working at the task while the interface repeatedly pushed back.",
      "Rapid, consecutive actions in the presence of frequent errors are characteristic of interface confusion",
      "and rising cognitive load, and the length of the session suggests that the goal mattered enough to keep trying.",
      "Left unaddressed, this pattern is a leading indicator of churn among otherwise motivated users."),
    recommendations = c(
      "Replay sessions from this group around the first error to locate the step where the loop begins.",
      "Audit error messages on that path for actionability: each should state what went wrong and what to do next.",
      "Add an in-context recovery aid (inline validation, undo, or a guided fallback) that triggers after repeated errors.")
  ),
  spec("early_exit",
    name = "Frustrated Novice", icon = "\U0001F423", colour = "#E0A340",
    theme = "early exit under friction",
    persona = "A newcomer who meets friction early and leaves before finding a footing: short sessions, little exploration and a disproportionate share of errors.",
    headline = "{Subject} presented an early-exit pattern under friction.",
    interpretation = paste(
      "Brief engagement, limited exploration and a high error load together suggest a user who encountered difficulty",
      "before building a working mental model of the product. Unlike the persistent struggler, this profile does not fight",
      "the barrier; it withdraws. The errors are better read as unfamiliarity with the conventions of the interface",
      "than as a defect on a single path, which makes them a design problem rather than a support problem."),
    recommendations = c(
      "Review first-run and onboarding flows for unexplained conventions and unforgiving inputs.",
      "Prefer error prevention (sensible defaults, constrained inputs, progressive disclosure) over error messages.",
      "Follow up with a short exit survey or a re-engagement message that offers a concrete first success.")
  ),
  spec("deliberate",
    name = "Methodical Explorer", icon = "\U0001F575", colour = "#0B376C",
    theme = "deliberate depth",
    persona = "A careful, systematic user who reads thoroughly and moves intentionally without getting lost.",
    headline = "{Subject} demonstrated a deliberate, low-friction engagement style.",
    interpretation = paste(
      "Long sessions combined with a restrained number of actions and few errors describe a reader rather than a clicker:",
      "time is spent absorbing content and weighing options before acting.",
      "The low error count indicates that this deliberation is productive rather than confused.",
      "Users with this signature are often the most reliable source of considered qualitative feedback."),
    recommendations = c(
      "Invest in content depth and information scent; this group rewards well-structured detail.",
      "Recruit from this group for interviews, diary studies and concept tests.",
      "Avoid speed optimisations that strip away the context this group relies on.")
  ),
  spec("fluent",
    name = "Power Navigator", icon = "\U0001F32A", colour = "#055F56",
    theme = "fluent expertise",
    persona = "A highly proficient, deeply familiar power user who sweeps through workflows with surgical precision.",
    headline = "{Subject} displayed a fluent, high-velocity pattern of expert use.",
    interpretation = paste(
      "A dense stream of actions delivered in comparatively little time, with few errors, is the signature of practised mastery:",
      "{subject} knows where things are and moves directly between them.",
      "Friction for this group is rarely about comprehension; it is about the cost of every additional step,",
      "and redesigns that reorganise familiar paths are felt most acutely here."),
    recommendations = c(
      "Offer accelerators: keyboard shortcuts, bulk actions and saved views.",
      "Protect established workflows during redesigns and announce changes in advance.",
      "Invite this group into early access programmes for advanced features.")
  ),
  spec("shallow",
    name = "Passive Disengaged", icon = "\U0001FAE5", colour = "#8A99A3",
    theme = "shallow contact",
    persona = "A disengaged or distracted user who skims the surface and exits quickly without deep interaction.",
    headline = "{Subject} showed a shallow, low-commitment engagement pattern.",
    interpretation = paste(
      "Short sessions and minimal activity, without a matching rise in errors, suggest that {subject} was not obstructed",
      "but simply not drawn in. The value of the product may not have registered, or the visit may have been incidental.",
      "The absence of friction here should not be mistaken for satisfaction."),
    recommendations = c(
      "Test whether the first screen communicates a concrete benefit within seconds.",
      "Tie re-engagement nudges to a single, specific next step rather than a general reminder.",
      "Segment by acquisition source to separate incidental visits from genuine disinterest.")
  ),
  spec(NULL,
    name = "Baseline User", icon = "\u2696", colour = "#5FA39B", fallback = TRUE,
    theme = "cohort-typical behavior",
    persona = "A user whose time, activity and friction all sit within the ordinary range of the cohort.",
    headline = "{Subject} followed a pattern broadly typical of the cohort.",
    interpretation = paste(
      "No dimension departs far enough from the cohort norm to define a distinct behavioral signature.",
      "That is informative in its own right: {subject} represents the mainstream experience of the product,",
      "and changes that shift this group's metrics will move the overall picture most."),
    recommendations = c(
      "Use this group as the reference population when evaluating design changes.",
      "Monitor for drift: movement out of the baseline band is an early signal of a changing experience.")
  )
)

# ---- Learning analytics ------------------------------------------------------

learning_lexicon <- list(
  domain = "learning",
  label = "Learning analytics",
  subject = "the learner",
  subject_short = "learner",
  subject_plural = "learners",
  case_label = "Learner Profile",
  columns = c(id = "learner_id", velocity = "minutes_on_task",
              intensity = "learning_actions", friction = "failed_attempts"),
  dimensions = list(
    velocity = list(
      noun = "time on task", unit = "minutes", label = "Velocity (time on task)",
      readings = list(
        high = c("a substantial investment of study time",
                 "sustained persistence with the material",
                 "extended periods of focused study"),
        mid  = c("a study time typical of the cohort",
                 "an ordinary allocation of time to the course"),
        low  = c("limited time devoted to the material",
                 "brief, intermittent contact with the course",
                 "a compressed study window")
      )
    ),
    intensity = list(
      noun = "learning activity", unit = "activities", label = "Intensity (activities)",
      readings = list(
        high = c("broad, active engagement with resources and exercises",
                 "a high volume of practice and resource use",
                 "an energetic, hands-on learning style"),
        mid  = c("a level of activity in keeping with peers",
                 "a conventional pattern of resource use"),
        low  = c("selective, economical use of learning resources",
                 "a narrow footprint of learning activity",
                 "few active learning events")
      )
    ),
    friction = list(
      noun = "failed attempts", plural = TRUE, unit = "failed attempts", label = "Friction (failed attempts)",
      readings = list(
        high = c("repeated difficulty in demonstrating mastery",
                 "a persistent pattern of unsuccessful attempts",
                 "recurring conceptual or procedural obstacles"),
        mid  = c("an ordinary rate of productive failure",
                 "the occasional unsuccessful attempt expected while learning"),
        low  = c("consistent success on assessed work",
                 "secure command of the material",
                 "few setbacks on the way to mastery")
      )
    )
  ),
  pace = list(noun = "activity rate", unit = "activities per minute"),
  rate = list(noun = "failure rate", unit = "failed attempts per 100 activities")
)

learning_archetypes <- list(
  spec("obstructed",
    name = "Struggling Persister", icon = "\U0001F9D7", colour = "#C0504D",
    theme = "obstructed persistence",
    persona = "A committed learner investing heavily in time and practice but meeting repeated failure, at risk of burnout without targeted support.",
    headline = "{Subject} showed high effort coupled with persistent difficulty.",
    interpretation = paste(
      "Effort is not the constraint for {subject}: time and activity are both well above the cohort,",
      "yet attempts continue to fail. This pattern typically signals a gap in prerequisite knowledge or a misconception",
      "that further practice of the same kind will not resolve. Motivation is currently an asset,",
      "but sustained effort without visible progress is a recognised route to disengagement."),
    recommendations = c(
      "Diagnose the specific concept behind the failed attempts before assigning further practice.",
      "Offer worked examples or a short tutorial intervention rather than additional unscaffolded exercises.",
      "Acknowledge the effort explicitly; feedback should separate persistence from performance.")
  ),
  spec("early_exit",
    name = "At-Risk Withdrawer", icon = "\U0001F6A9", colour = "#E0A340",
    theme = "early exit under friction",
    persona = "A learner whose limited time and activity are dominated by failure: early setbacks appear to be driving withdrawal.",
    headline = "{Subject} presented an early-warning pattern of withdrawal after setbacks.",
    interpretation = paste(
      "Low engagement on its own may reflect competing commitments, but low engagement concentrated around failed attempts",
      "is a well-documented precursor of dropout. The record suggests that {subject} met difficulty early",
      "and reduced contact with the course rather than persisting through it."),
    recommendations = c(
      "Prioritise a personal check-in from a tutor or advisor within the current week.",
      "Provide a low-stakes route back in, such as a short diagnostic with immediate feedback.",
      "Review whether early assessments are pitched above the entry level of this group.")
  ),
  spec("deliberate",
    name = "Deep Learner", icon = "\U0001F4DA", colour = "#0B376C",
    theme = "deliberate depth",
    persona = "A reflective learner who spends long periods with the material, acts selectively and rarely fails.",
    headline = "{Subject} demonstrated a reflective, depth-oriented approach to study.",
    interpretation = paste(
      "High time on task with economical activity and few failures is consistent with a deep approach to learning:",
      "{subject} appears to read, consider and consolidate before acting.",
      "Success rates suggest the time is being converted into understanding rather than lost to confusion."),
    recommendations = c(
      "Offer extension material and open-ended tasks that reward depth.",
      "Invite this learner to peer-support or study-group roles.",
      "Avoid metrics that equate activity counts with engagement for this group.")
  ),
  spec("fluent",
    name = "Efficient Achiever", icon = "\U0001F3AF", colour = "#055F56",
    theme = "fluent expertise",
    persona = "A capable learner who completes a high volume of work quickly and accurately.",
    headline = "{Subject} displayed fluent, efficient mastery of the material.",
    interpretation = paste(
      "A high rate of successful activity in comparatively little time indicates prior knowledge or rapid acquisition.",
      "For {subject}, the risk is not difficulty but under-challenge: the course may be consolidating what is already known",
      "rather than extending it."),
    recommendations = c(
      "Offer accelerated pathways or credit-by-assessment where available.",
      "Introduce stretch tasks that require transfer to unfamiliar problems.",
      "Monitor for disengagement driven by boredom rather than difficulty.")
  ),
  spec("shallow",
    name = "Surface Engager", icon = "\U0001F4A4", colour = "#8A99A3",
    theme = "shallow contact",
    persona = "A learner with minimal time and activity but no marked failure: present, but not yet invested.",
    headline = "{Subject} showed minimal, surface-level engagement with the course.",
    interpretation = paste(
      "Low time and low activity without elevated failure suggest that {subject} is not struggling with the material",
      "so much as not yet engaging with it. This can reflect external constraints, unclear expectations",
      "or a perception that the course offers little beyond what can be achieved with minimal contact."),
    recommendations = c(
      "Clarify expectations and the link between activity and assessment outcomes.",
      "Use short, high-value tasks to establish a routine of contact.",
      "Check for external constraints (workload, access, connectivity) before assuming low motivation.")
  ),
  spec(NULL,
    name = "Steady Learner", icon = "\u2696", colour = "#5FA39B", fallback = TRUE,
    theme = "cohort-typical study behavior",
    persona = "A learner whose time, activity and success rate all sit within the ordinary range of the cohort.",
    headline = "{Subject} followed a study pattern broadly typical of the cohort.",
    interpretation = paste(
      "No dimension of the record departs far enough from the cohort to define a distinct learning signature.",
      "{Subject} represents the mainstream course experience, and is the natural reference point when judging",
      "whether changes to the course help or hinder learners as a whole."),
    recommendations = c(
      "Use this group as the reference population when evaluating course changes.",
      "Watch for movement into a friction-led profile as assessments become more demanding.")
  )
)

# ---- Survey paradata and response quality -----------------------------------

survey_lexicon <- list(
  domain = "survey",
  label = "Survey paradata and response quality",
  subject = "the respondent",
  subject_short = "respondent",
  subject_plural = "respondents",
  case_label = "Respondent Profile",
  columns = c(id = "respondent_id", velocity = "completion_minutes",
              intensity = "open_text_words", friction = "quality_flags"),
  dimensions = list(
    velocity = list(
      noun = "completion time", unit = "minutes", label = "Velocity (completion time)",
      readings = list(
        high = c("an unhurried, considered pace through the instrument",
                 "a sustained investment of time in the questionnaire",
                 "extended deliberation over the items"),
        mid  = c("a completion time typical of the sample",
                 "an ordinary pace through the instrument"),
        low  = c("a rapid passage through the instrument",
                 "a completion time compatible with speeding",
                 "minimal time spent per item")
      )
    ),
    intensity = list(
      noun = "open-text elaboration", unit = "words", label = "Intensity (open text)",
      readings = list(
        high = c("rich, elaborated open-ended answers",
                 "a generous volume of free-text detail",
                 "substantive engagement with open questions"),
        mid  = c("open-text answers of typical length",
                 "an ordinary level of elaboration"),
        low  = c("terse or absent open-ended answers",
                 "minimal elaboration where free text was invited",
                 "little investment in open questions")
      )
    ),
    friction = list(
      noun = "data-quality flags", plural = TRUE, unit = "flags", label = "Friction (quality flags)",
      readings = list(
        high = c("repeated failures of attention and consistency checks",
                 "a concentration of data-quality flags",
                 "response patterns that cast doubt on data validity"),
        mid  = c("an ordinary number of data-quality flags",
                 "the occasional inconsistency expected in any long instrument"),
        low  = c("clean, internally consistent responding",
                 "no meaningful evidence of careless responding",
                 "attentive responding throughout")
      )
    )
  ),
  pace = list(noun = "elaboration rate", unit = "words per minute"),
  rate = list(noun = "flag rate", unit = "flags per 100 words")
)

survey_archetypes <- list(
  spec("obstructed",
    name = "Effortful Struggler", icon = "\U0001F9D7", colour = "#C0504D",
    theme = "obstructed persistence",
    persona = "A respondent who invests time and words yet accumulates quality flags, pointing to comprehension difficulty rather than carelessness.",
    headline = "{Subject} showed high effort alongside elevated quality flags.",
    interpretation = paste(
      "Careless responding is rarely slow and elaborate. When time and open-text effort are high but quality checks still fail,",
      "the more plausible reading is that {subject} found items hard to interpret: ambiguous wording, unfamiliar terms,",
      "translation issues or an instrument that does not fit their situation. These flags say as much about the questionnaire",
      "as about the respondent."),
    recommendations = c(
      "Inspect which items generated the flags and review their wording through cognitive interviewing.",
      "Do not exclude these cases automatically; consider sensitivity analyses with and without them.",
      "Check for language or mode effects concentrated in this group.")
  ),
  spec("early_exit",
    name = "Careless Speeder", icon = "\u26A1", colour = "#E0A340",
    theme = "early exit under friction",
    persona = "A respondent who moves quickly, writes little and fails quality checks: the classic signature of insufficient-effort responding.",
    headline = "{Subject} presented a pattern consistent with insufficient-effort responding.",
    interpretation = paste(
      "Short completion time, minimal elaboration and elevated quality flags together match the satisficing profile",
      "described in the survey methodology literature: answers are given to finish rather than to inform.",
      "Data from {subject} should be treated with caution, although no single indicator is conclusive",
      "and the classification is relative to this sample."),
    recommendations = c(
      "Flag this case for review against your pre-registered exclusion rules before analysis.",
      "Report results with and without insufficient-effort cases.",
      "Review instrument length and incentive structure if this group is large.")
  ),
  spec("deliberate",
    name = "Deliberative Responder", icon = "\U0001F9D0", colour = "#0B376C",
    theme = "deliberate depth",
    persona = "A careful respondent who takes time over the instrument, answers consistently and writes concisely.",
    headline = "{Subject} demonstrated careful, deliberative responding.",
    interpretation = paste(
      "Above-average completion time with clean quality checks indicates that the extra time was spent considering answers",
      "rather than struggling with them. Open-text answers are economical rather than expansive,",
      "suggesting {subject} favours precision over elaboration."),
    recommendations = c(
      "Treat this group as a high-quality reference when assessing measurement properties.",
      "Consider this group for follow-up qualitative interviews.")
  ),
  spec("fluent",
    name = "Efficient Responder", icon = "\u2705", colour = "#055F56",
    theme = "fluent expertise",
    persona = "A fluent respondent who completes quickly yet writes substantively and passes quality checks.",
    headline = "{Subject} displayed fast, engaged and high-quality responding.",
    interpretation = paste(
      "Speed alone is not a marker of poor quality. {Subject} moved through the instrument quickly while still providing",
      "substantive open-text answers and passing consistency checks, which suggests familiarity with the topic",
      "or with survey formats rather than satisficing."),
    recommendations = c(
      "Avoid speed-only exclusion rules, which would wrongly remove this group.",
      "Use this group's open-text answers as a source of well-articulated themes.")
  ),
  spec("shallow",
    name = "Minimal Responder", icon = "\U0001FAE5", colour = "#8A99A3",
    theme = "shallow contact",
    persona = "A respondent who completes quickly with little elaboration but without failing quality checks.",
    headline = "{Subject} showed minimal but internally consistent engagement.",
    interpretation = paste(
      "Brief completion and sparse open-text answers suggest low involvement, yet the absence of quality flags indicates",
      "that the closed-ended answers are likely valid. {Subject} answered what was asked and little more:",
      "the data are usable, but thin where elaboration was invited."),
    recommendations = c(
      "Retain closed-ended data; treat missing open text as non-response rather than evidence of carelessness.",
      "If open-ended insight matters, consider fewer, more focused open questions.")
  ),
  spec(NULL,
    name = "Typical Responder", icon = "\u2696", colour = "#5FA39B", fallback = TRUE,
    theme = "cohort-typical responding",
    persona = "A respondent whose completion time, elaboration and data quality all sit within the ordinary range of the sample.",
    headline = "{Subject} followed a response pattern broadly typical of the sample.",
    interpretation = paste(
      "No paradata dimension departs far enough from the sample norm to define a distinct response style.",
      "{Subject} represents the mainstream response experience of the instrument."),
    recommendations = c(
      "Use this group as the reference when calibrating quality thresholds.")
  )
)

domain_registry <- list(
  ux       = list(lexicon = ux_lexicon,       archetypes = ux_archetypes),
  learning = list(lexicon = learning_lexicon, archetypes = learning_archetypes),
  survey   = list(lexicon = survey_lexicon,   archetypes = survey_archetypes)
)
