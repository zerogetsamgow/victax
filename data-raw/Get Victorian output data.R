## code to prepare `DATASET` dataset goes here
library(tidyverse)
library(rvest)
library(arrow)

dtf.url = "https://www.dtf.vic.gov.au"
output.url = str_c(dtf.url,"/departmental-statements#departmental-performance-measures")

output.file.url =
  output.url |> 
  read_html() |> 
  html_elements('div [href$="xlsx"]') |>
  html_attr("href") |> 
  as_tibble_col("link") |> 
  filter(str_detect(link,"performance")) |> 
#  as_tibble_col() |> 
  mutate(value = str_c(dtf.url,link)) 

vicoutput =
  output.file.url |> 
  rowwise() |> 
  mutate(download= tempfile(fileext = "xlsx"))  |> 
  mutate(x = download.file(value,download))  |>
  #select(-x) |> 
  mutate(sheet=map(download,readxl::excel_sheets)) |> 
  unnest(sheet)  |> 
  filter(str_detect(sheet,"(P|p)erform")) |> 
  mutate(data=pmap(list(download,sheet), readxl::read_excel, skip=1, col_types = "text")) |> 
  select(sheet, data) |> 
  unnest(data) |> 
  rename("output_measure"=2,
         "unit_of_measure"=3) |>
  filter(!is.na(output_measure),!str_detect(output_measure,"^This|^The|^New|^No target|renamed|unable")) |> 
  mutate(output_type = str_extract(output_measure, "Quantity|Quality|Timeliness|Cost")) |> 
  fill(output_type) |> 
  mutate(output_name = if_else(is.na(unit_of_measure)&!str_detect(output_measure, "Quantity|Quality|Timeliness|Cost"),output_measure,NA_character_)) |> 
  fill(output_name) |> 
  filter(!is.na(unit_of_measure)) |> 
  pivot_longer(contains("20")) |> 
  mutate(name = str_replace(name,"2025-2025","2024-2025")) |> 
  mutate(financial_year = str_extract(name,"[0-9]{4}-[0-9]{4}") |> fy::fy2date() |> fy::date2fy(),
         value_type = str_extract(name,"[a-z]+\\s*[a-z]*")) |> 
  filter(!is.na(value)) |> 
  select(sheet, contains("output"),financial_year,contains("value"),unit_of_measure)
 

write_parquet(
  vicoutput,
  sink = "./data/vicoutput.parquet"
)
