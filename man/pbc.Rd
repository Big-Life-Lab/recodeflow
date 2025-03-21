% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{pbc}
\alias{pbc}
\title{The pbc dataset}
\format{
A data frame with 418 observations and 20 variables.
\describe{
 \item{id}{case number}
 \item{time}{number of days between registration and the earlier of death, transplantation, or study analysis time}
 \item{status}{status at endpoint, 0/1/2 for censored, transplant, dead}
 \item{trt}{1/2/NA for D-penicillamine, placebo, or not randomized}
 \item{age}{age in years}
 \item{sex}{m/f}
 \item{ascites}{presence of ascites}
 \item{hepato}{presence of hepatomegaly or enlarged liver}
 \item{spiders}{blood vessel malformations in the skin}
 \item{edema}{0 no edema, 0.5 untreated or successfully treated, 1 edema despite diuretic therapy}
 \item{bili}{serum bilirubin (mg/dl)}
 \item{chol}{serum cholesterol (mg/dl)}
 \item{albumin}{serum albumin (g/dl)}
 \item{copper}{urine copper (ug/day)}
 \item{alk.phos}{alkaline phosphotase (U/liter)}
 \item{ast}{aspartate aminotransferase (U/ml)}
 \item{trig}{triglycerides (mg/dl)}
 \item{platelet}{platelet count}
 \item{protime}{standardised blood clotting time}
 \item{stage}{histologic stage of disease (1, 2, 3, or 4)}
}
}
\source{
{https://cran.r-project.org/web/packages/survival/survival.pdf}
}
\usage{
pbc
}
\description{
The pbc dataset
}
\keyword{datasets}
