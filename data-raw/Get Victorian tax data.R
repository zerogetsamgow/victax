## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(rvest)
library(arrow)

dtf.url = "https://www.dtf.vic.gov.au/"
tax.url = str_c(dtf.url,"state-financial-data-sets/state-taxation-revenue")

tax.file.url =
  tax.url |> 
  read_html() |> 
  html_elements('div [href$="xlsx"]') |>
  html_attr("href") |> 
  as_tibble_col("link") |> 
  filter(!str_detect(link,"Qtr")) |> 
  as_tibble_col() |> 
  mutate(value = str_c(dtf.url,value))

tax.temp = tempfile(fileext = "xlsx")

download.file(tax.file.url$value[1],tax.temp,mode="wb")

victax =
  tibble(
    sheet=map(tax.temp,readxl::excel_sheets)
  ) |> 
  unnest(sheet) |> 
  filter(!str_detect(sheet,"Introduction|Overview")) |> 
  mutate(data=pmap(list(tax.temp,sheet), readxl::read_excel, skip=4)) |> 
  unnest(data) |> 
  rename(financial_year = ...1) |> 
  select(sheet, financial_year, Revenue, contains("20")) |> 
  pivot_longer(-sheet:-financial_year, names_to = "estimate_type", values_to = "estimate") |> 
  mutate(
    estimate = as.numeric(estimate),
    estimate_type = str_replace(estimate_type,"Revenue","Actual"),
    publication_year = str_extract(estimate_type, "[0-9]{4}-[0-9]{2}"),
    publication_type = str_remove(estimate_type,  publication_year) |> str_trim(),
    estimate_type = factor(estimate_type, levels = c("Actual","Estimate")),
    estimate_type = coalesce(estimate_type,"Estimate") |> factor(),
    publication_type = coalesce(publication_type,estimate_type,) |> factor(),
    tax_sub = str_extract(sheet,".*levy"),
    tax_line = str_remove(sheet, tax_sub) |> str_trim(),
    tax_line = coalesce(tax_line,sheet),
    tax_line = 
      if_else(
        str_detect(sheet, "payroll|wellbeing"),
        "Payroll tax", 
        tax_line),
    tax_line = str_replace(tax_line, ".*landholding.*","Land tax")) |> 
  filter(!is.na(estimate)) |> 
  mutate(fy_date = fy::fy2date(financial_year)) 

write_parquet(
  victax,
  sink = "./data/victax.parquet"
)

