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

vicoutput =
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
      "(P|p)erform")) |> 
  dplyr::mutate(
    data = 
      purrr::pmap(
        list(download,sheet), 
        readxl::read_excel, 
        skip=1, 
        col_types = "text")) |> 
  dplyr::select(sheet, data) |> 
  tidyr::unnest(data) |> 
  dplyr::rename(
    "output_measure"=2,
    "unit_of_measure"=3) |>
  dplyr::filter(
    !is.na(output_measure),
    !stringr::str_detect(
      output_measure,
      "^This|^The|^New|^No target|renamed|unable")) |> 
  dplyr::mutate(
    output_type = 
      stringr::str_extract(
        output_measure, 
        "Quantity|Quality|Timeliness|Cost")) |> 
  tidyr::fill(output_type) |> 
  dplyr::mutate(
    output_name = 
      dplyr::if_else(
        is.na(unit_of_measure) & 
          !stringr::str_detect(
            output_measure,
            "Quantity|Quality|Timeliness|Cost"),
        output_measure,NA_character_)) |> 
  tidyr::fill(output_name) |> 
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
        "[0-9]{4}-[0-9]{4}") |> 
      fy::fy2date() |> 
      fy::date2fy(),
    value_type = 
      stringr::str_extract(
        name,
        "[a-z]+\\s*[a-z]*")) |> 
  dplyr::filter(!is.na(value)) |> 
  dplyr::select(
    sheet, 
    tidyselect::contains("output"),
    financial_year,
    tidyselect::contains("value"),
    unit_of_measure)
 

arrow::write_parquet(
  vicoutput,
  sink = ".inst/extdata/vicoutput_tbl.parquet"
)
