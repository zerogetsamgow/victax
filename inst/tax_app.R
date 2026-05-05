#
#
library(systemfonts)
library(shiny)

#db =  dbConnect(duckdb())

# Read data 
victax.app = 
  arrow::read_parquet(
    "./extdata/victax_tbl.parquet") |> 
  dplyr::filter(
    stringr::str_detect(
      tax_line,
      ("Total|Payroll|Land ")))

#victax.app =  copy_to(db,   df = victax.app, overwrite = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Victorian Government tax revenue dashboard"),
    br(),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # Input: Select the random distribution type ----
            radioButtons(
              ".taxline", 
              "Tax line:", 
              choices = 
                unique(
                  victax.app |> 
                   dplyr::pull(tax_line))),
            br(),
            radioButtons(
              ".addlevy", 
              "Add levy revenue:", 
              choices = c("Yes","No"),
              selected = "No"),
            textOutput("explainer"),
            br(),
            checkboxGroupInput(
              ".budget","Budget", 
              choices = c("2019-20","2020-21","2021-22","2022-23","2023-24","2024-25","2025-26","2026-27"),
              selected =  c("2022-23","2025-26", "2026-27"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("taxplot"),
          br(),
          tableOutput("taxtable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Get data 
  .victax.app <<-
    reactive({
      if(input$.addlevy == "No" & 
         input$.taxline %in% c("Payroll tax","Land tax")){
        dplyr::filter(
          victax.app,
          tax_sub == "") |>  
          dplyr::collect() 
      } else {
        victax.app |> 
          dplyr::group_by(
            financial_year,
            tax_line,
            estimate_type,
            publication_year,
            publication_type,
            fy_date) |> 
          dplyr::summarise(
            estimate = sum(estimate),
            .groups = 'drop') 
      }
      })
      
  
  .estimate.data <<-
    reactive({
      dplyr::filter(
        .victax.app(), 
        tax_line == input$.taxline,
        publication_type == "Budget",
        publication_year %in% input$.budget
     ) # |> collect()
    })
  
  .actual.data <<- 
    reactive({ 
      dplyr::filter(
        .victax.app(), 
        tax_line == input$.taxline,
        publication_type == "Actual",
        fy_date > as.Date("2019-1-1")
     ) # |> collect()
    })
  
  .min = reactive({max(0,floor(min(.actual.data()$estimate)/1e3)-1)})
  .max = reactive({ceiling(max(.estimate.data()$estimate)/1e3)})
  
  
  .colours =
    c(
      "2019-20" = vpstheme::bv.navy,
      "2020-21" = vpstheme::bv.teal,
      "2021-22" = vpstheme::bv.royal,
      "2022-23" = vpstheme::bv.amber,
      "2023-24" = vpstheme::bv.pink,
      "2024-25" = vpstheme::bv.purple,
      "2025-26" = vpstheme::bv.chartreuse,
      "2026-27" = vpstheme::bv.rose
    ) 
  
  output$explainer =
    renderText({
      stringr::str_c(
        "Choosing Yes adds COVID and ",
        "Mental health and wellbeing", 
        "levies to Payroll tax and ",
        "Land tax estimates.", sep = '\n')
    })
  
  output$taxtable =
    renderTable({
      dplyr::bind_rows(
        .estimate.data(),
        .actual.data()
      ) |> 
      dplyr::ungroup() |> 
      dplyr::filter(
        financial_year %in% c("2022-23","2025-26","2026-27")) |> 
      dplyr::arrange(publication_year, financial_year) |>
      dplyr::select(
        "Source" = publication_type,
        "Year" = publication_year,
        financial_year,
        estimate
      ) |> 
      tidyr::pivot_wider(
        names_from = financial_year,
        values_from = estimate) |> 
      data.table::data.table()},
      spacing = "l",
      digits = 0,
      width = "80%",
      na = "-",
      caption = "Estimates in table shown in $ millions."
    )
  
  
  output$taxplot =
    renderPlot({
      req(input$.taxline)
      req(input$.budget)
      suppressWarnings(
        ggplot2::ggplot() +
          ggplot2::geom_line(
            data = .estimate.data(),
            size = 1.2,
            ggplot2::aes(
              x = fy_date,
              y = estimate/1000,
              colour = publication_year,
              group = publication_year,
            )
            )+
          ggplot2::geom_text(
            size = 5,
            data = 
              .estimate.data() |> 
                dplyr::group_by(
                  publication_type,
                  publication_year) |>
               dplyr::filter(fy_date == max(fy_date)),
          ggplot2::aes(
            x = fy_date + lubridate::days(14),
            y = estimate/1000,
            label = publication_year,
            colour = publication_year),
            vjust = 0,
            hjust = 0)+
          ggplot2::geom_line(
            size = 1.2,
            data = .actual.data(),
            ggplot2::aes(
              x = fy_date,
              y = estimate/1000,
            ),
            colour = vpstheme::bv.charcoal) +
        ggplot2::scale_y_continuous(
          name = "Revenue, $ billion",
          limits = c(.min(),.max()),
          breaks = seq(0,50, by = 2)
          )+
        ggplot2::scale_x_date(
          name = "Financial year ending 30 June 20*",
          breaks = 
            seq.Date(
              from = lubridate::dmy("30-6-2019"),
              to = lubridate::dmy("30-6-2031"),
              by = "years"),
          date_labels = "%y",
          limits = lubridate::dmy("1-6-2019","1-1-2031")) +
        ggplot2::guides(colour = "none") +
          ggplot2::labs(
            title = 
              stringr::str_glue(
                "Budget {stringr::str_to_lower(input$.taxline)} vs actual revenue (grey line)")) +
        ggplot2::scale_color_manual(values = .colours) +
        vpstheme::theme_vps_dh()
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

