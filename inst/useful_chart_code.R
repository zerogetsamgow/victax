library(vpstheme)
library(tidyverse)
library(arrow)

# Read tax data 
revenue = 
  arrow::read_parquet(
    "./inst/extdata/victax_tbl.parquet") |> 
  dplyr::filter(
    stringr::str_detect(tax_line,"Payroll"),
    stringr::str_detect(tax_sub,"Mental")) |> 
  dplyr::filter(
    (estimate_type == "Actual"|publication_year == "2026-27"))

output_cost = 
  arrow::read_parquet( "./inst/extdata/vicoutput_tbl.parquet") |> 
  dplyr::filter(
    stringr::str_detect(output,"Mental"),
    stringr::str_detect(measure_type,"Cost"),
    ((financial_year == "2026-27") | !stringr::str_detect(value_type,"target")))  |> 
  dplyr::mutate(
    value = 
      as.numeric(
        stringr::str_remove(value,"\\s")))

combined =
  dplyr::bind_rows(
    revenue |> 
      dplyr::select(
        financial_year,
        "value"=estimate) |> 
      dplyr::mutate(
        financial_year = 
          financial_year |> 
            fy::fy2date() |> 
            fy::date2fy(),
        measure = "Levy"),
    output_cost |> 
      dplyr::group_by(financial_year) |> 
      dplyr::summarise(value = sum(value)) |> 
      dplyr::mutate(measure = "Cost")
  ) |> 
  tidyr::pivot_wider(
    names_from = measure,
    values_from = value
  ) |> 
  dplyr::mutate(Base = Cost - Levy) |> 
  tidyr::pivot_longer(Levy:Base) |> 
  dplyr::mutate(
    name = 
      factor(
        name, 
        levels = c("Cost","Levy","Base")),
    fy_date = 
      fy::fy2date(financial_year)) |> 
  dplyr::filter(
    fy_date < ymd("2028-1-1"))



text.size = 10

output_plot =
  ggplot() +
  geom_col(
    data = 
      combined |> 
      dplyr::filter(name == "Cost"),
    aes(
      x = fy_date,
      y = value),
    fill = bv.navy
  ) +
  geom_text(
    data = 
      combined |> 
      dplyr::filter(name == "Cost"),
    aes(
      x = fy_date,
      y = value,
      label = round(value/1e3,1),
     
    ),
    colour = bv.navy,
    vjust = 0,
    size = text.size,
    position = position_stack(vjust = 1.01)
  ) +
  geom_col(
    data = 
      combined |> 
      dplyr::filter(name != "Cost"),
    aes(
      x = fy_date,
      y = value,
      fill = name
    ),
  ) +
  geom_text(
    data = 
      combined |> 
      dplyr::filter(name != "Cost"),
    aes(
      x = fy_date,
      y = value,
      label = format(round(value/1e3, digits = 1),nsmall = 1),
      colour = name
    ), 
    size = text.size,
    vjust = -.5,
    position = position_stack(vjust = 0)
  ) +
  geom_text(
    data = 
      combined |> 
      dplyr::filter(
        name != "Cost",
        fy_date == max(fy_date)) |> 
      mutate(
        fy_date = 
          fy_date + months(6)),
    aes(
      x = fy_date,
      y = value,
      label = stringr::str_c(name,"\nfunded"),
      colour = name
    ), 
    size = text.size,
    lineheight = .4,
    hjust = 0,
    position = position_stack(vjust = 0.5)
  ) +
  scale_x_date(
    name = "Financial year ending 30 June",
    date_labels = "%Y",
    breaks = seq.Date(from = ymd("2016-6-30"), to = ymd("2025-6-30"), by = "3 years"),
    limits = ymd(c("2015-1-1","2029-1-31")),
    expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("Levy"=bv.pink,"Base"=bv.royal), guide = "none")+
  scale_colour_manual(
    values = c("Levy" = bv.musk,
               "Base" = bv.sky), guide = "none")+
  vpstheme::scale_y_continuvps(
    limits = c(0,4e3),
    name = "Mental health funding, $ billion",
    labels = scales::label_number(scale = 1/1e3)
  ) +
  labs(title = "Victorian Government mental health output funding",
       caption = "Source: dtf.vic.gov.au/state-financial-data-sets")+
  vpstheme::theme_vps_dh(
    base_size = 30); output_plot

ggplot2::ggsave(
  filename = "./inst/Victorian Government mental health output funding.png",
  plot = output_plot)


regulation_costs = 
  arrow::read_parquet( "./inst/extdata/vicoutput_tbl.parquet") |> 
  dplyr::filter(
    (stringr::str_detect(stringr::str_to_lower(output),"regulation|statutory|resources|public health|fishing|building")|
       stringr::str_detect(stringr::str_to_lower(sub_output),"regulation|statutory|public health|fishing|building")),
    stringr::str_detect(measure_type,"Cost"),
    ((financial_year == "2026-27") | !stringr::str_detect(value_type,"target")))  |> 
  dplyr::mutate(
    value = 
      as.numeric(
        stringr::str_remove(value,"\\s"))) |> 
  dplyr::mutate(
    fy_date = 
      fy::fy2date(financial_year)) |> 
  dplyr::filter(
    fy_date < ymd("2028-1-1")) |> 
  arrange(desc(value))


regulation_plot =
  ggplot(
    data = 
      regulation_costs,
    aes(
      x = fy_date,
      y = value,
      fill = output,
      label = round(value,1))) +
  geom_col(show.legend = FALSE) +
  geom_text(
    position = position_stack(vjust = .5),
    show.legend = FALSE,
    size = 4
  ) +
  geom_text(
    data = 
      regulation_costs |> 
      filter(fy_date == max(fy_date)) |> 
      mutate(fy_date = fy_date + months(6)),
    aes(
      x = fy_date,
      y = value,
      colour = output,
      label = output),
    hjust = 0,
    position = position_stack(vjust = .5),
    show.legend = FALSE,
    size = 4
  ) +
  scale_x_date(
    name = "Financial year ending 30 June",
    date_labels = "%Y",
    breaks = seq.Date(from = ymd("2016-6-30"), to = ymd("2027-6-30"), by = "1 years"),
    limits = ymd(c("2024-1-1","2029-1-31")),
    expand = c(0.01,0.01)) +
  vpstheme::scale_y_continuvps(
    limits = c(0,2.7e3),
    name = "Mental health funding, $ billion",
    labels = scales::label_number(scale = 1/1e3)
  ) +
  labs(title = "Victorian Government mental health output funding",
       caption = "Source: dtf.vic.gov.au/state-financial-data-sets")+
  vpstheme::theme_vps_dh(
    base_size = 30); regulation_plot
