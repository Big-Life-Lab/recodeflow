#' Worksheet for mtcars dataset
#' @description 
#' The variable and variable_details worksheets that holds metadata for the pbc dataset that comes with the survival package.
#' 
#' Use `?pbc` to view the contents of the pbc dataset.
#' 
#' Each row the pbc_variables worksheet that contains metadata for a variable in the pbc dataset. There are 20 variables in the pbc database. The pbc_variable_details worksheet contains additional metadata for each variable.
#'
#' @format ## `pbc_database`
#' A list that contains metadata for the pbc database using the Dublin Core Metadata Initiative (DCMI) vocabulary.
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
#' @source <https://cran.r-project.org/web/packages/survival/survival.pdf>
#' "pbc_database"
#' 
#' @rdname pbc_database
#' @format ## `pbc_variables`
#' A data frame with 24 rows and 11 columns:
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
#' \item{notes}{notes}
#' \item{description}{description}
"pbc_variables"

#' @rdname pbc_database
#' @format ## `pbc_variable_details`
#' A data frame with 31 rows and 16 columns:
"","variable","dummyVariable","toType","databaseStart","variableStart","fromType","recTo","catLabel","catLabelLong","recFrom","catStartLabel","variableStartShortLabel","variableStartLabel","units","notes"
#' \describe{
#' \item{variable}{variable name}
#' \item{dummyVariable}{dummy variable name}
#' \item{toType}{to type}
#' \item{databaseStart}{database start}
#' \item{variableStart}{variable start}
#' \item{fromType}{from type}
#' \item{recTo}{rec to}
#' \item{catLabel}{category label}
#' \item{catLabelLong}{category long label}
#' \item{recFrom}{rec from}
#' \item{catStartLabel}{category start label}
#' \item{variableStartShortLabel}{variable start short label}
#' \item{variableStartLabel}{variable start label}
#' \item{units}{units}
#' \item{notes}{notes}
"pbc_variable_details"