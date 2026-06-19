#' @title Victorian taxation revenue data
#' @description Tidy data set of Victorian taxation revenue data
#' @format A data frame with `r nrow(victax_tbl)` rows and `r ncol(victax_tbl)` variables:
#' \describe{
#'   \item{\code{tax_line}}{The name of the tax line}
#'   \item{\code{tax_sub}}{Sub taxes where tax levies reported separetly}
#'   \item{\code{publication_type}}{The budget, budget update or pre-election budget update publication type}
#'   \item{\code{publication_year}}{The year the budget, budget update or pre-election budget update was for}
#'   \item{\code{financial_year}}{The financial year of the data, as a character variable.}
#'   \item{\code{fy_date}}{The financial year of the data, as a date variable.}
#'   \item{\code{estimate}}{A numeric being the amount of revenue in $ million.}
#'   \item{\code{estimate_type}}{Whether the data is an estimate or an actual value.}
#'   
#'}
#' @source \url{https://www.dtf.vic.gov.au/state-taxation-revenue}
"victax_tbl"

#' @title Victorian output data
#' @description Tidy data set of Victorian output performance measures data
#' @format A data frame with `r nrow(vicoutput_tbl)` rows and `r ncol(vicoutput_tbl)` variables:
#' \describe{
#'   \item{\code{sheet}}{The name of Excel sheet the data is extracted from}
#'   \item{\code{output}}{The name of the output}
#'   \item{\code{sub_ouput}}{The disaggregation of the output, if applicable}
#'   \item{\code{measure_type}}{The type or performance measure: being quantity, quality, timeliness or cost.}
#'   \item{\code{unit_of_measure}}{The unit of measure used.}
#'   \item{\code{financial_year}}{The financial year of the data, as a character variable.}
#'   \item{\code{value}}{A numeric being the amount of the measure.}
#'   \item{\code{evalue_type}}{Whether the data is a target, expected or an actual value.}
#'   
#'}
#' @source \url{https://www.dtf.vic.gov.au/state-taxation-revenue}
"vicoutput_tbl"