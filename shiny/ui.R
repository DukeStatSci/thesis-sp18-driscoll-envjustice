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
               actionButton("search", "Search"), 
               span(textOutput("warning"), style="color:red")
           ),
           p(
             class = "text-muted", "This plot shows the distribution of toxicity experienced across the nation, the state, and within your county."
           ),
           box(width = NULL, 
               plotOutput("national", height = 200)
           ),
           p(
             class = "text-muted", "This plot shows the distribution of toxicity experienced by black and white individuals within your county. This gives an idea of the enviromental inequality in your local area."
           ),
           box(width = NULL,
               plotOutput("race", height = 200)
           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)