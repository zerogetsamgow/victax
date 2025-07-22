library(vpstheme)
library(tidyverse)
library(arrow)

# Read tax data 
revenue = 
  read_parquet( "./data/victax.parquet") |> 
  filter(str_detect(tax_line,"Payroll"),
         str_detect(tax_sub,"Mental")) |> 
  filter(
    (estimate_type == "Actual"|publication_year == "2025-26"))

output_cost = 
  read_parquet( "./data/vicoutput.parquet") |> 
  filter(str_detect(output_name,"Mental"),
         str_detect(output_type,"Cost"),
         !str_detect(value_type,"target"))  |> 
  mutate(value = as.numeric(str_remove(value,"\\s")))

combined =
  bind_rows(
    revenue |> 
      select(financial_year,"value"=estimate) |> 
      mutate(financial_year = 
               financial_year |> 
               fy::fy2date() |> 
               fy::date2fy()
               ,
             measure = "Levy"),
    output_cost |> 
      group_by(financial_year) |> 
      summarise(value = sum(value)) |> 
      mutate(measure = "Cost")
  ) |> 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |> 
  mutate(Base = Cost - Levy) |> 
  pivot_longer(Levy:Base) |> 
  mutate(
    name = factor(name, levels = c("Cost","Levy","Base")),
    fy_date = fy::fy2date(financial_year)) |> 
  filter(fy_date < ymd("2026-1-1"))



text.size = 10

output_plot =
  ggplot() +
  geom_col(
    data = combined |> filter(name == "Cost"),
    aes(
      x = fy_date,
      y = value),
    fill = bv.navy
  ) +
  geom_text(
    data = combined |> filter(name == "Cost"),
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
    data = combined |> filter(name != "Cost"),
    aes(
      x = fy_date,
      y = value,
      fill = name
    ),
  ) +
  geom_text(
    data = combined |> filter(name != "Cost"),
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
    data = combined |> 
      filter(name != "Cost",
             fy_date == max(fy_date)
             ) |> 
      mutate(fy_date = fy_date+months(6)),
    aes(
      x = fy_date,
      y = value,
      label = str_c(name,"\nfunded"),
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
    limits = ymd(c("2015-1-1","2027-1-31")),
    expand = c(0.01,0.01)) +
  scale_fill_manual(values = c("Levy"=bv.pink,"Base"=bv.royal), guide = "none")+
  scale_colour_manual(
    values = c("Levy" = bv.musk,
               "Base" = bv.sky), guide = "none")+
  scale_y_continuvps(
    limits = c(0,4e3),
    name = "Mental health funding, $ billion",
    labels = scales::label_number(scale = 1/1e3)
  ) +
  labs(title = "Victorian Government mental health output funding",
       caption = "Source: dtf.vic.gov.au/state-financial-data-sets")+
  theme_vps_dh(base_size = 30); output_plot

ggsave(
  filename = "~/documents/github/victax/Victorian Government mental health output funding.png",
  plot = output_plot)
