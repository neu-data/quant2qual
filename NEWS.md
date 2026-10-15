# quant2qual 0.1.0

First release.

## Classification engine

* `identify_archetype()` standardises time, activity and friction against the
  cohort (`"z"`, `"robust"` or `"percentile"` scaling, optional `log1p`),
  bands them at a threshold or at raw-unit `cutpoints`, and assigns every case
  by exact rule cascade or by fuzzy membership, with a confidence, a runner-up
  archetype, percentile ranks, rates and outlier flags.
* `define_archetype()`, `archetype_matrix()` and `default_archetypes()` build
  archetype matrices; `archetype_summary()` and `archetype_membership()`
  inspect the result.
* `mock_behavioral_data()` simulates cohorts with a known ground truth.

## Narrative generator

* `generate_narrative()` writes clinical, UX or brief case studies;
  `generate_cohort_narrative()` writes the cohort synthesis.
  `select_cases()` picks representative, extreme or borderline cases.
* Three domain packs -- UX, learning analytics and survey paradata -- with
  their own vocabulary and interpretations, adaptable with `q2q_lexicon()`.

## Reporting

* `write_behavioral_report()` produces Markdown, R Markdown, Quarto, HTML or
  PDF reports with key findings, figures, case studies and a generated methods
  section.
* `neudata_pdf()` and `neudata_html()` output formats in the Neudata house
  style, and an R Markdown template.
* `plot_archetype_map()`, `plot_archetype_signatures()`,
  `plot_case_profile()` and `plot_q2q_pipeline()`.
* A PDF pipeline guide in `inst/pipeline/`.
