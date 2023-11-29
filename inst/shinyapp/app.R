#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("ct-util.R")
max_num_studies = 1000

#' @import shiny
# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Clinical Trials Query"),

  # Sidebar with several input for keywords
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      selectInput("source_class", "Sponsor Type",
                  choices = list("Federal" = "FED",
                                 "Individual" = "INDIV",
                                 "Industry" = "INDUSTRY",
                                 "Network" = "NETWORK",
                                 "NIH" = "NIH",
                                 "Other" = "OTHER",
                                 "Other gov" = "OTHER_GOV",
                                 "Unknown" = "UNKNOWN")),
      numericInput("num_conditions", "Number of the Most Frequent Conditions", value = 10),
      checkboxGroupInput("allocation", "Allocation",
                         choices = list("Not Applicable (Single Arm Trial)" = "N/A",
                                        "Randomized" = "Randomized",
                                        "Nonrandomized" = "Nonrandomized")),
      sliderInput("max", "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100)
    ),

    # Show plots of the generated distribution
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_Plot")),
        tabPanel("Conditions", plotOutput("condition_Plot")),
        tabPanel("Designs", plotOutput("design_Plot")),
        tabPanel("Word Cloud", plotOutput("word_Plot"))
      )
    )
  )
)

# Define server logic required to draw graphs
server <- function(input, output) {

  get_studies = reactive({
    if (input$brief_title_kw != "") {
      si = input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      ret = query_kwds(studies, si, "brief_title", match_all = TRUE)
    } else {
      ret = studies
    }

    ret = ret |>
      filter(source_class %in% !!input$source_class)

    ret |>
      head(max_num_studies) |>
      collect()
  })

  # output histogram for phases that clinical trials are categorized
  output$phase_Plot = renderPlot({
    get_studies() |>
      create_phase_histogram_plot(input$allocation)
  })

  # output histogram for conditions that trials in a query are examining
  output$condition_Plot <- renderPlot({
    get_studies() |>
      create_condition_histogram(input$num_conditions)
  })

  # output histogram for intervention model
  output$design_Plot = renderPlot({
    get_studies() |>
      create_design_histogram(input$allocation)
  })

  # output word cloud
  output$word_Plot = renderPlot({
      wordcloud_rep <- repeatable(wordcloud)
      v = get_studies() |>
        getTermMatrix()
      wordcloud_rep(names(v), v, scale=c(4,0.5), max.words = input$max,
                    colors = brewer.pal(8, "Dark2"))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
