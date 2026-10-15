# Shared fixtures: one simulated cohort with its ground truth, classified once.
fixture_users <- function(n = 240, domain = "ux") {
  mock_behavioral_data(n, domain = domain, seed = 11, include_truth = TRUE)
}

fixture_profile <- function(...) {
  users <- fixture_users()
  identify_archetype(users[setdiff(names(users), "true_archetype")], ...)
}
