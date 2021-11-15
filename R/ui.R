
ui <- shiny::fluidPage(

  theme = bslib::bs_theme(
    bg = "white",
    fg = "black",
    primary =  "#077C7C",
    secondary = "#069A5F"
    ),

  shiny::titlePanel("Survival Analysis"),
  shiny::tabsetPanel(
    shiny::tabPanel("Upload Data",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(shiny::fileInput(inputId = 'file1', 'Select csv file to Analyze',
                               accept=c('text/csv','text/comma-separated-values,text/plain','.csv', ".tsv")),
                               shiny::uiOutput("StartDate"),
                               shiny::uiOutput("EndDate"),
                               shiny::uiOutput("Event"),
                               shiny::uiOutput("Splits"), # new
                               shiny::selectInput("date_format", "Select Date Format", choices = c("yyyy-mm-dd", "mm-dd-yyyy", "dd-mm-yyyy")),
                               shiny::actionButton("submit", label = "Submit"),
                     ),

                     shiny::mainPanel(DT::dataTableOutput("SurvivalTbl"),
                  plotly::plotlyOutput("SurvPlot"),
                  shiny::tableOutput("Cox")
                  )
      )
    ),
    shiny::tabPanel("Filter Data",

                 DT::DTOutput("filter_table")

             ),
    shiny::tabPanel("Model Data",
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(
                        shiny::actionButton("submit2", label = "Submit"),
                        shiny::numericInput("churn", "Enter Percent Attrition", value = 0.5, min = 0.001, max = 0.999)
               ),
               shiny::mainPanel(
                 plotly::plotlyOutput("SurvPlot2"),
                 shiny::textOutput("churn_output")
               )
             ))
  )
)
