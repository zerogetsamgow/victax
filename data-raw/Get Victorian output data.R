## code to prepare `DATASET` dataset goes here
dtf.url = "https://www.dtf.vic.gov.au"

output.url = 
  stringr::str_c(
    dtf.url,
    "/departmental-statements#departmental-performance-measures")

output.file.url =
  output.url |> 
  rvest::read_html() |> 
  rvest::html_elements('div [href$="xlsx"]') |>
  rvest::html_attr("href") |> 
  tibble::as_tibble_col("link") |> 
  dplyr::filter(
    stringr::str_detect(
      link,
      "performance")) |> 
  dplyr::mutate(
    value = 
      stringr::str_c(dtf.url,link)) 

vicoutput_tbl =
  output.file.url |> 
  dplyr::rowwise() |> 
  dplyr::mutate(
    download = tempfile(fileext = "xlsx"))  |> 
  dplyr::mutate(
    x = download.file(value,download, mode = "wb"))  |>
  #select(-x) |> 
  dplyr::mutate(
    sheet = 
      purrr::map(
        download,
        readxl::excel_sheets)) |> 
  tidyr::unnest(sheet)  |> 
  dplyr::filter(
    stringr::str_detect(
      sheet,
      "(P|p)erform|(D|d)epart")) |> 
  dplyr::mutate(
    data = 
      purrr::pmap(
        list(download,sheet), 
        readxl::read_excel, 
        skip=0, 
        col_types = "text")) |> 
  dplyr::select(sheet, data) |> 
  tidyr::unnest(data) |> 
  janitor::clean_names() |> 
  dplyr::filter(
    !is.na(output),
    !stringr::str_detect(
      output,
      "^This|^The|^New|^No target|renamed|unable")) |> 
  dplyr::filter(!is.na(unit_of_measure)) |> 
  tidyr::pivot_longer(
    tidyselect::contains("20")) |> 
  dplyr::mutate(
    name = 
      stringr::str_replace(
        name,
        "2025-2025",
        "2024-2025")) |> 
  dplyr::mutate(
    financial_year = 
      stringr::str_extract(
        name,
        "[0-9]{4}_[0-9]{2,4}") |> 
      stringr::str_replace("_","-") |> 
      fy::fy2date() |> 
      fy::date2fy(),
    value_type = 
      stringr::str_extract(
        stringr::str_remove(
          name, "x"),
        "[a-z]+(\\s|_)*[a-z]*") |> 
      stringr::str_replace("_"," ")) |> 
  dplyr::filter(!is.na(value)) |> 
  dplyr::select(
    sheet, 
    tidyselect::contains("output"),
    tidyselect::contains("measure"),
    financial_year,
    tidyselect::contains("value"))
 


# Combine with existing data
vicoutput_tbl =
  dplyr::bind_rows(
    vicoutput_tbl, 
    victax::vicoutput_tbl) |> 
  unique()

# Save for export
usethis::use_data(vicoutput_tbl, overwrite = TRUE)

# Save for app
arrow::write_parquet(
  vicoutput_tbl,
  sink = "./inst/extdata/vicoutput_tbl.parquet"
)
