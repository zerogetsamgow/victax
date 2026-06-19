## code to prepare `DATASET` dataset goes here

# Define url for DTF website
dtf.url = 
  "https://www.dtf.vic.gov.au"

# Append to create url for State Taxation Revenue page
tax.url =
  stringr::str_c(
    dtf.url,
    "/state-financial-data-sets/state-taxation-revenue")

# Get url for download
tax.file.url =
  tax.url |> 
  rvest::read_html() |> 
  rvest::html_elements('div [href$="xlsx"]') |>
  rvest::html_attr("href") |> 
  tibble::as_tibble_col("link") |> 
  dplyr::filter(
    !stringr::str_detect(link,"Qtr")) |> 
  tibble::as_tibble_col() |> 
  dplyr::mutate(
    file_url = stringr::str_c(dtf.url,value), 
    file_name = stringr::str_c("./data-raw/",basename(file_url)),
    download = download.file(file_url, file_name, mode = "wb"))

# Get and clean data
victax_tbl =
  tax.file.url |> 
  dplyr::mutate(
    sheet =
      purrr::map(
        file_name,
        readxl::excel_sheets)) |> 
  tidyr::unnest(sheet) |>
  # Get data from all sheets other than intro and overview
  # Each tax line is a separate sheet.
  dplyr::filter(
    !stringr::str_detect(
      sheet,
      "Introduction|Overview")) |> 
  # Read data from each sheet
  dplyr::mutate(
    data = 
      purrr::pmap(
        list(file_name,sheet), 
        readxl::read_excel, 
        skip=4)) |> 
  tidyr::unnest(data) |> 
  # Rename first column, containing financial years.
  dplyr::rename(financial_year = ...1) |> 
  # select data columns.
  dplyr::select(
    sheet, 
    financial_year, 
    Revenue, 
    tidyselect::contains("20")) |> 
  # Pivot to long data
  tidyr::pivot_longer(
    -sheet:-financial_year, 
    names_to = "estimate_type", 
    values_to = "estimate") |> 
  # Extract and clean variables.
  dplyr::mutate(
    # Estimate should be a numeric
    estimate = as.numeric(estimate),
    # Extract publication year from estimate_type
    publication_year = 
      stringr::str_extract(
        estimate_type, 
        "[0-9]{4}-[0-9]{2}") |> 
      dplyr::coalesce(""),
    # Extract publication type
    publication_type = stringr::str_remove(
      estimate_type,  
      publication_year) |> 
      stringr::str_trim(),
    # Estimate type is a factor, rename Revenue to Actual
    estimate_type = 
      stringr::str_replace(
        estimate_type,
        "Revenue",
        "Actual"),
    estimate_type = 
      dplyr::coalesce(
        estimate_type,
        "Estimate") |> 
      factor(
        levels = 
          c("Actual",
            "Estimate")),
    publication_type = 
      dplyr::coalesce(
        publication_type,
        estimate_type) |> 
      factor(),
    # Get tax_sub,
    tax_sub = 
      stringr::str_extract(
        sheet,
        ".*levy") |> 
      dplyr::coalesce(""),
    # Then use to clean tax_line name
    tax_line = 
      stringr::str_remove(
        sheet, 
        tax_sub) |> stringr::str_trim(),
    tax_line = 
      dplyr::coalesce(
        tax_line,
        sheet),
    tax_line = 
      dplyr::if_else(
        stringr::str_detect(
          sheet, 
          "payroll|wellbeing"),
        "Payroll tax", 
        tax_line),
    tax_line = 
      stringr::str_replace(
        tax_line, 
        ".*landholding.*",
        "Land tax"),
    tax_line = 
      stringr::str_replace(
        tax_line, 
        "Voluntee.*",
        "Volunteer Levy")) |> 
  dplyr::filter(!is.na(estimate)) |> 
  dplyr::mutate(
    fy_date = 
      fy::fy2date(financial_year)) 

# Select and arrange data
victax_tbl =
  victax_tbl |> 
  dplyr::select(
    tax_line,
    tax_sub,
    publication_type,
    publication_year,
    financial_year,
    estimate, 
    estimate_type,
    fy_date
  ) |> 
  dplyr::arrange(
    
    desc(estimate_type),
    desc(publication_year),
    desc(financial_year),
    tax_line
  )

# Combine with existing data
victax_tbl =
  dplyr::bind_rows(
    victax_tbl, 
    victax::victax_tbl) |> 
  unique()

# Save for export
usethis::use_data(victax_tbl, overwrite = TRUE)
  
# Save for app
arrow::write_parquet(
  victax_tbl,
  sink = "./inst/extdata/victax_tbl.parquet"
)



# Remove download
file.remove(tax.file.url$file_name)


