library(shiny)
library(dplyr)
library(DT)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(bootswatch = 'minty'),
  # App title
  titlePanel("Recipe Generator"),
  # Sidebar layout with input and output definitions
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      # Input: Slider for the number of bins
      numericInput(inputId = "servings",
                  label = h3("Desired Servings"), 
                  value = 10)
      
    ),
    # Main panel for displaying outputs
    mainPanel(
      fluidRow(
        column(3,
               uiOutput("links"),
        column(5,
               dataTableOutput("ingredients")))
      ),
      fluidRow(
        column(4,
               radioButtons("radio", h3("Radio buttons"),
                            choices = list("Choice 1" = 1, "Choice 2" = 2,
                                           "Choice 3" = 3),selected = 1)),
        column(4,
               selectInput("select", h3("Select box"), 
                           choices = list("Choice 1" = 1, "Choice 2" = 2,
                                          "Choice 3" = 3), selected = 1))
        )
    )
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  recipe_data = reactive(recipe_generator(input$servings))
  
  output$ingredients = renderDataTable(
    data.frame(amount = a$amount,
                         unit = a$unit,
                         name = a$name) %>% 
                arrange(name),
              options = list(pageLength = 10, bLengthChange=0, bFilter=0,
                             autoWidth = TRUE),
              rownames= FALSE)
  
}

shinyApp(ui = ui, server = server)

