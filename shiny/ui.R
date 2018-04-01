library(shinydashboard)
library(leaflet)

header <- dashboardHeader(
  title = "Local Toxicity Data"
)

body <- dashboardBody(
  fluidRow(
    column(width = 8, 
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 700)
           )
    ),
    column(width = 4,
           box(width = NULL, status = "warning",
               textInput("addressInput", label = "Enter your address:", value = "Duke University, Durham, NC"),
               actionButton("search", "Search")
           ),
           box(width = NULL, 
               plotOutput("national", height = 200)
           ),
           box(width = NULL,
               plotOutput("race", height = 200)
           ),
           p(
             class = "text-muted", "Enter text here so I can give context."
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)