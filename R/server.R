

#' Shiny app server function
#'
#' @param input Provided by shiny.
#' @param output Provided by shiny.
#' @param session Provided by shiny
#'
#' @export
server <- function(input, output, session){
  thematic::thematic_shiny()

  vals <- shiny::reactiveValues()

  rawData <- shiny::reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    df <- utils::read.csv(inFile$datapath)
    return(df)
  })

  output$StartDate <- shiny::renderUI({
    shiny::selectInput("sdate", "Select Start Date Variable",
                choices = colnames(rawData()))
  })

  output$EndDate <- shiny::renderUI({
    shiny::selectInput("edate", "Select End Date Variable",
                choices = colnames(rawData()))
  })

  output$Event <- shiny::renderUI({
    shiny::selectInput("event", "Select Event Variable",
                choices = colnames(rawData()))
  })

  output$Splits <- renderUI({
    shiny::checkboxGroupInput('splits', "Select Group Subcategories",
                        choices = c("none", colnames(rawData())[!colnames(rawData()) %in% c(input$sdate, input$edate, input$event)]),
                       selected = "none")
  })


  output$SurvivalTbl <- DT::renderDataTable({
    req(input$file1)
    DT::datatable(rawData(), options = list(pageLength = 5))
    })


  NewTbl <- shiny::eventReactive( input$submit, {
    clean_data <- date_reformat(data = rawData(),
                                startdate = input$sdate,
                                endate = input$edate,
                                date_format = input$date_format,
                                splits = input$splits,
                                event = input$event)
    return(clean_data)
    })


  output$SurvPlot <- plotly::renderPlotly({
    create_SurvPlot(data = NewTbl(), splits = input$splits)
  })

  CoxFit <- shiny::eventReactive(input$submit, {
    cox_fit(data = NewTbl(), splits = input$splits)
      })

  output$Cox <- shiny::renderTable({
    if(is.null(CoxFit())){
      validate("Subset the data to compare groups")
    }
    else{
      df <- as.data.frame(summary(CoxFit())$coefficients)
      var <- rownames(df)
      df2 <- cbind(var, df)
      return(df2)
    }
  })


#############
## Page 2 ###
#############
  NewTbl2 <- shiny::eventReactive(input$submit, {
    cd <- date_reformat(data = rawData(),
                        startdate = input$sdate,
                        endate = input$edate,
                        date_format = input$date_format,
                        splits = input$splits,
                        event = input$event)
    return(cd)
  })

  output$filter_table <- DT::renderDT({
    DT::datatable(NewTbl2(), filter = 'top',
              options = list(pageLength = 5))
  })

  filtered_tbl <- shiny::reactive({
    tbl <- NewTbl2()[input[['filter_table_rows_all']],]
    return(tbl)
  })


  ##############
  ### page 3 ###
  ##############


  modelData <- shiny::eventReactive(input$submit2, {
    cd <- filtered_tbl()
    cd <- dplyr::select(cd, c(input$sdate, input$edate, input$event, tenure))
    return(cd)
  })


  output$SurvPlot2 <-plotly::renderPlotly({
    create_model(data = modelData())
  })


  output$churn_output <- shiny::renderText({
    wb_ <- eval(parse(text = paste0("survival::survreg(survival::Surv(modelData()[, 4], modelData()[, 3]) ~ 1, data = modelData())")))
    t <- stats::predict(wb_, type = "quantile", surv = surv, p = input$churn, newdata = data.frame(1))
    return(paste(round(t, 2), "days to reach ", input$churn * 100, "% attrition", sep = ""))
  })

  ######## end of server function ########


}










