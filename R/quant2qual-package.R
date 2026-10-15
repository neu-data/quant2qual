#' quant2qual: The Automated Behavioral Narrator
#'
#' `quant2qual` turns numeric behavioral distributions into qualitative
#' case-study prose. The workflow has three steps, each a function you can run
#' and inspect on its own:
#'
#' 1. **Classify** -- [identify_archetype()] standardises three behavioral
#'    dimensions (*velocity*: time, *intensity*: actions, *friction*: errors)
#'    against the cohort, bands them into low / mid / high, and maps every
#'    case onto an [archetype_matrix()] with a membership-based confidence.
#' 2. **Narrate** -- [generate_narrative()] writes a clinical, UX or brief
#'    case study for a chosen profile; [generate_cohort_narrative()] writes the
#'    cohort-level synthesis.
#' 3. **Report** -- [write_behavioral_report()] compiles both into Markdown,
#'    R Markdown or Quarto sources, or renders HTML and Neudata-styled PDF.
#'
#' Everything is rule-based and local: no text leaves the machine and the same
#' input always produces the same prose.
#'
#' @section Domains:
#' Three domain packs ship with the package, each with its own archetypes and
#' vocabulary: `"ux"` (product and usability analytics), `"learning"`
#' (learning analytics) and `"survey"` (survey paradata and response quality).
#' See [q2q_domains()], [default_archetypes()] and [q2q_lexicon()].
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data %||% :=
## usethis namespace: end
NULL
