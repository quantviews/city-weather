shinyUI(
   fluidPage(
      theme = shinytheme("yeti"),
      tags$br(),
      fluidRow(column(4, offset = 1, selectInput("city", NULL, choices = cities$city, selectize = TRUE, width = "100%"))),
      fluidRow(
         column(7, highchartOutput("hc1", height = 800))
      ),
      fluidRow(column(5, offset = 1, gsub("<p><img src=\".*\"/></p>", "", includeMarkdown("README.md"))))
   )
)