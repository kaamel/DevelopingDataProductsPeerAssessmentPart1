require(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Crime in the US"),
  sidebarPanel(
    width = 3,
    h3("Filters"),
    h4("Crime Types"),
#    checkboxInput(inputId = "pageable", label = "Pageable"),
    checkboxInput(inputId = "murder", label = "Murder", value = TRUE),
    checkboxInput(inputId = "assault", label = "Assault", value = TRUE),
    checkboxInput(inputId = "rape", label = "Rape", value = TRUE),
    sliderInput("Perc", "Set the Percentile", 
                min=0, max=100, value=50,  step=1,
                animate=TRUE)

  ),
  mainPanel(
    h3(textOutput("perc")),
    h4(textOutput("percsub1")),
    h5(textOutput("percsub2")),
    h5(textOutput("percsub3")),
    htmlOutput("gvis"),
    htmlOutput("myTable")
  )
)
)

