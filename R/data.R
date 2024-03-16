#' The pbc dataset
#'
#' @format A data frame with 418 observations and 20 variables.
#' \describe{
#'  \item{id}{case number}
#'  \item{time}{number of days between registration and the earlier of death, transplantation, or study analysis time}
#'  \item{status}{status at endpoint, 0/1/2 for censored, transplant, dead}
#'  \item{trt}{1/2/NA for D-penicillamine, placebo, or not randomized}
#'  \item{age}{age in years}
#'  \item{sex}{m/f}
#'  \item{ascites}{presence of ascites}
#'  \item{hepato}{presence of hepatomegaly or enlarged liver}
#'  \item{spiders}{blood vessel malformations in the skin}
#'  \item{edema}{0 no edema, 0.5 untreated or successfully treated, 1 edema despite diuretic therapy}
#'  \item{bili}{serum bilirubin (mg/dl)}
#'  \item{chol}{serum cholesterol (mg/dl)}
#'  \item{albumin}{serum albumin (g/dl)}
#'  \item{copper}{urine copper (ug/day)}
#'  \item{alk.phos}{alkaline phosphotase (U/liter)}
#'  \item{ast}{aspartate aminotransferase (U/ml)}
#'  \item{trig}{triglycerides (mg/dl)}
#'  \item{platelet}{platelet count}
#'  \item{protime}{standardised blood clotting time}
#'  \item{stage}{histologic stage of disease (1, 2, 3, or 4)}
#' }
#' @source {https://cran.r-project.org/web/packages/survival/survival.pdf}
"pbc"

#' Metadata for the pbc dataset using the DCIM standard
#'
#' @format A list containing DCMI metadata:
#' \describe{
#' \item{title}{title}
#' \item{creator}{creator}
#' \item{subject}{subject}
#' \item{description}{description}
#' \item{publisher}{publisher}
#' \item{date}{date}
#' \item{type}{type}
#' \item{format}{format}
#' \item{identifier}{identifier}
#' \item{source}{source}
#' \item{language}{language}
#' \item{rights}{rights}
#' \item{references}{references}
#' }
"pbc_metadata"

#' Variables sheet for the pbc dataset
#'
#' @format A data frame with 24 rows and 11 columns:
#' \describe{
#' \item{variable}{variable name}
#' \item{label}{variable label}
#' \item{labelLong}{variable label long}
#' \item{subject}{subject}
#' \item{section}{section}
#' \item{variableType}{variable type}
#' \item{databaseStart}{database start}
#' \item{units}{units}
#' \item{variableStart}{variable start}
#' \item{notes}{logical indicating presence of notes}
#' \item{description}{logical indicating presence of description}
#' }
"pbc_variables"

#' Variable details sheet for the pbc dataset
#'
#' @format A data frame with 69 rows and 16 columns:
#' \describe{
#' \item{variable}{variable name}
#' \item{dummyVariable}{dummy variable name}
#' \item{typeEnd}{end type}
#' \item{databaseStart}{database start}
#' \item{variableStart}{variable start}
#' \item{typeStart}{start type}
#' \item{recEnd}{record end}
#' \item{recStart}{record start}
#' \item{catLabel}{category label}
#' \item{catLabelLong}{category long label}
#' \item{nubValidCat}{number of valid categories (numeric)}
#' \item{units}{logical indicating presence of units}
#' \item{notes}{logical indicating presence of notes}
#' \item{catStartLabel}{category start label}
#' \item{variableStartShortLabel}{variable start short label}
#' \item{variableStartLabel}{variable start label}
#' }
"pbc_variable_details"
