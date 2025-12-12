# Randomised controlled trial data

A simulated dataset

## Usage

``` r
qol
```

## Format

### `qol`

A data frame with 1000 rows and 6 columns:

- group:

  Randomisation group: 1 = Placebo, 2 = Active treatment

- age0:

  Participant's age at randomisation (baseline), in years

- qol0:

  Participant's quality of life at randomisation (baseline), measured
  using the EuroQol Visual Analogue Scale (EQ-VAS)

- qol3:

  Participant's quality of life at 3 months post-randomisation, measured
  using EQ-VAS

- qol12:

  Participant's quality of life at 12 months post-randomisation,
  measured using EQ-VAS

- r_qol12:

  Missingness indicator: whether qol12 is reported or not
