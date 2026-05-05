
<!-- README.md is generated from README.Rmd. Please edit that file -->

# victax

<!-- badges: start -->

<!-- badges: end -->

The goal of victax is to provide Victorian Government budget data in
tidy format.

## Installation

You can install the development version of ***victax*** from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("zerogetsamgow/victax")
```

## Taxation data

***victax*** converts [state taxation
revenue](https://www.dtf.vic.gov.au/state-taxation-revenue) data
published by the Victorian Department of Treasury and Finance in .xlsx
format into tidy data.

The data, saved and exported as `victax_tbl` contains actuals from
1996-97 - the first year accrual-based “whole of government” financial
statements - and budget, budget update, and pre-election budget update
estimates back to the 2013-14 budget. Data for the current financial
year and most recent budget is shown below.

``` r
library(victax)
## basic example code
head(
  victax_tbl |> 
    dplyr::filter(
      financial_year == fy::date2fy(Sys.Date())),12 
  )
#> # A tibble: 12 × 8
#>    tax_line    tax_sub publication_type publication_year financial_year estimate
#>    <chr>       <chr>   <fct>            <chr>            <chr>             <dbl>
#>  1 Emergency … ""      Budget           2025-26          2025-26           1623.
#>  2 Gambling t… ""      Budget           2025-26          2025-26           2744.
#>  3 Insurance … ""      Budget           2025-26          2025-26           2302.
#>  4 Land tax    ""      Budget           2025-26          2025-26           6377.
#>  5 Land tax    "COVID… Budget           2025-26          2025-26           1177.
#>  6 Land trans… ""      Budget           2025-26          2025-26           9562.
#>  7 Motor vehi… ""      Budget           2025-26          2025-26           3802.
#>  8 Other taxes ""      Budget           2025-26          2025-26           2211.
#>  9 Payroll tax ""      Budget           2025-26          2025-26           9585.
#> 10 Payroll tax "Menta… Budget           2025-26          2025-26           1147.
#> 11 Payroll tax "COVID… Budget           2025-26          2025-26           1147.
#> 12 Total taxa… ""      Budget           2025-26          2025-26          41677.
#> # ℹ 2 more variables: estimate_type <fct>, fy_date <date>
```

The package builds an [interactive shiny
app](https://zerogetsamgow.shinyapps.io/victax/) which can be used to
explore the tax data.
