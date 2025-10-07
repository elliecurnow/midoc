#' Child body mass index data
#'
#' A simulated dataset
#'
#' @format ## `bmi` A data frame with 1000 rows and 6 columns:
#' \describe{
#'   \item{bmi7}{Child's body mass index at age 7 years}
#'   \item{matage}{Mother's age at first pregnancy, standardised relative to a
#'   mean age of 30 years}
#'   \item{mated}{Mother's educational level: post-16 years qualification or
#'   not}
#'   \item{pregsize}{Mother's pregnancy size: singleton or twins}
#'   \item{bwt}{Child's birth weight in kilograms}
#'   \item{r}{Missingness indicator: whether bmi7 is reported or not}
#' }
"bmi"

#' Randomised controlled trial data
#'
#' A simulated dataset
#'
#' @format ## `qol`
#' A data frame with 1000 rows and 6 columns:
#' \describe{
#'   \item{group}{Randomisation group: 0 = Placebo, 1 = Active treatment}
#'   \item{age0}{Participant's age at randomisation (baseline), in years}
#'   \item{qol0}{Participant's quality of life at randomisation (baseline),
#'   measured using the EuroQol Visual Analogue Scale (EQ-VAS)}
#'   \item{qol3}{Participant's quality of life at 3 months post-randomisation,
#'    measured using EQ-VAS}
#'   \item{qol12}{Participant's quality of life at 12 months post-randomisation,
#'   measured using EQ-VAS}
#'   \item{r_qol12}{Missingness indicator: whether qol12 is reported or not}
#' }
"qol"

#' Administrative data
#'
#' A simulated dataset
#' @format ## `adr`
#' A data frame with 1000 rows and 5 columns:
#' \describe{
#'   \item{gcse_score}{Child's GCSE score (at approximately age 16 years), as a
#'   percentage, calculated from the total of an individual’s top eight GCSE
#'   qualifications, ranked in terms of points}
#'   \item{log_income}{Logarithm of the family income per annum, in UK pounds,
#'   when the child is aged 5 years}
#'   \item{mated}{Mother's educational level: post-16 years qualification or
#'   not}
#'   \item{ks2_score}{Child's Key Stage 2 score (at approximately age 11 years),
#'   as a percentage, calculated from the total of an individual's English,
#'   maths and science scores}
#'   \item{r_log_income}{Missingness indicator: whether log_income is reported
#'   or not}
#' }
"adr"

