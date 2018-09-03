shinyUI(
   fluidPage(
      theme = shinytheme("yeti"),
      titlePanel("Погода в российских городах-миллионниках"),
      tags$br(),
      fluidRow(column(4, offset = 1, selectInput("city", NULL, choices = cities$city, selectize = TRUE, width = "100%"))),
      fluidRow(
         column(7,  offset = 1, highchartOutput("hc1", height = 648))
      ),
      fluidRow(column(5, offset = 1, gsub("<p><img src=\".*\"/></p>", "", includeMarkdown("README.md"))))
   )
)