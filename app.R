#
#

library(systemfonts)
library(shiny)
library(vagotheme)
library(tidyverse)
library(arrow)
# library(duckdb)
# library(DBI)

#db =  dbConnect(duckdb())

# Read data 
victax.app = read_parquet( "./data/victax.parquet") |> 
  filter(str_detect(tax_line,("Total|Payroll tax|Land ")))

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
            radioButtons(".taxline", "Tax line:", choices = unique(victax.app |> pull(tax_line))),
            br(),
            radioButtons(".addlevy", "Add levy revenue:", choices = c("Yes","No"),
                          selected = "No"),
            textOutput("explainer"),
            br(),
            checkboxGroupInput(
              ".budget","Budget", 
              choices = c("2019-20","2020-21","2021-22","2022-23","2023-24","2024-25"),
              selected =  c("2021-22","2024-25"))
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
        filter(
          victax.app,
          is.na(tax_sub)) |>  collect()
      } else {
        victax.app |> 
          group_by(
            financial_year,
            tax_line,
            estimate_type,
            publication_year,
            publication_type,
            fy_date) |> 
          summarise(estimate = sum(estimate)) 
      }
      })
      
  
  .estimate.data <<-
    reactive({
      filter(
        .victax.app(), 
        tax_line == input$.taxline,
        publication_type %in% c("Budget"),
        publication_year %in% input$.budget
     ) # |> collect()
    })
  
  .actual.data <<- 
    reactive({ 
      filter(
        .victax.app(), 
        tax_line == input$.taxline,
        publication_type == "Actual",
        fy_date > as.Date("2019-1-1")
     ) # |> collect()
    })
  
  .min = reactive({max(0,floor(min(.actual.data()$estimate)/1e3)-1)})
  .max = reactive({ceiling(max(.estimate.data()$estimate)/1e3)})
  
  
  .colours =
    c("2019-20" = vago_colours$theme[1],
      "2020-21" = vago_colours$theme[2],
      "2021-22" = vago_colours$theme[3],
      "2022-23" = vago_colours$theme[4],
      "2023-24" = vago_colours$theme[5],
      "2024-25" = vago_colours$theme[6]
    )
  
  output$explainer =
    renderText({
      str_c(
        "Choosing Yes adds COVID and ",
        "Mental health and wellbeing", 
        "levies to Payroll tax and ",
        "Land tax estimates.", sep = '\n')
    })
  
  output$taxtable =
    renderTable({
      bind_rows(
        .estimate.data(),
        .actual.data()
      ) |> 
      ungroup() |> 
      filter(financial_year %in% c("2022-23","2023-24")) |> 
      arrange(publication_year, financial_year) |>
      select(
        "Source" = publication_type,
        "Year" = publication_year,
        financial_year,
        estimate
      ) |> 
      pivot_wider(names_from = financial_year,values_from = estimate) |> 
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
        ggplot() +
          geom_line(
            data = .estimate.data(),
            size = 1.2,
            aes(
              x = fy_date,
              y = estimate/1000,
              colour = publication_year,
              group = publication_year,
            )
          )+
        geom_text(
          size = 5,
          data = 
            .estimate.data() |> 
              group_by(publication_type,publication_year) |>
              filter(fy_date == max(fy_date)),
          aes(
            x = fy_date + days(1),
            y = estimate/1000,
            label = publication_year,
            colour = publication_year),
          vjust = 0,
          hjust = 0)+
        geom_line(
          size = 1.2,
          data = .actual.data(),
          aes(
            x = fy_date,
            y = estimate/1000,
          ),
          colour = vago.grey) +
        scale_y_continuous(
          name = "Revenue, $ billion",
          limits = c(.min(),.max()),
          #breaks = seq(0,40, by = 2)
        )+
        scale_x_date(
          name = "Financial year ending 30 June",
          breaks = 
            seq.Date(
              from = dmy("30-6-2019"),
              to = dmy("30-6-2028"),
              by = "years"),
          date_labels = "%Y",
          limits = dmy("1-6-2019","1-1-2030")) +
        guides(colour = "none") +
        labs(title = str_glue("Budget {str_to_lower(input$.taxline)} vs actual revenue (grey line)")) +
        scale_color_manual(values = .colours) +
        theme_vago_white()
      
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
