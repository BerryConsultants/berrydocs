library(shiny)
library(DT)
library(htmltools)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .my_col_class {
         align-content: center;
      }")
    )
  ),
  
  fluidRow(
    titlePanel(h1("Dropout Per Dose Per Visit", align = "center")),
    alignCenter(sliderInput("numVisits", "Number of visits:", 
            min = 1, max = 20, value = 5)),
    DTOutput("dataInputTable")
  )
)

server <- function(input, output) {
  
  df = data.frame(Visits = 1:5,
                       ConditionalDropoutRate = 0.05)
  df$TotalDropoutRate = 1-cumprod(1-df$ConditionalDropoutRate)
  df$MarginalDropoutRate = c(df$TotalDropoutRate[1], df$TotalDropoutRate[-1] - df$TotalDropoutRate[-nrow(df)])
  df = df[,c("Visits", "ConditionalDropoutRate", "MarginalDropoutRate", "TotalDropoutRate")]
  colnames(df) = c("Visit number",
                   "Conditional Visit Dropout Rate",
                   "Marginal Visit Dropout Rate",
                   "Cumulative Dropout Prob.")
  
  ## Render DF to actually change
  output$dataInputTable = renderDT(datatable(df, options = list(
    digits = 4,
    dom = "t",
    autoWidth = TRUE,
    columnDefs = list(list(width = '150px', targets = 1:4))), selection = 'none', editable = "cell") |> formatRound(2:4, digits = 4))
  
  ## Update from Conditional
  
  proxy = dataTableProxy('dataInputTable')
  
  observeEvent(input$dataInputTable_cell_edit, {
    info = input$dataInputTable_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    df <<- editData(df, info)
    if(j == 2) {
      ## Update other columns from Conditional
      df[,4] <<- 1-cumprod(1-df[,2])
      df[,3] <<- c(df[1,4], df[-1,4] - df[-nrow(df),4])
    } else if(j == 3) {
      ## Update other columns from Marginal
    } else if(j == 4) {
      ## Update other columns from Cumulative
    }
    replaceData(proxy, df) 
  })
  
  observe({
    input$numVisits
    nv = input$numVisits
    if(nv > nrow(df)) {
      if(nrow(df) == 0) {
        df <<- data.frame(Visits = 1,
                   ConditionalDropoutRate = 1-(1-.1)^(1))
        df$TotalDropoutRate = 1-cumprod(1-df$ConditionalDropoutRate)
        df$MarginalDropoutRate = c(df$TotalDropoutRate[1], df$TotalDropoutRate[-1] - df$TotalDropoutRate[-nrow(df)])
        df = df[,c("Visits", "ConditionalDropoutRate", "MarginalDropoutRate", "TotalDropoutRate")]
        colnames(df) = c("Visit number",
                         "Conditional Visit Dropout Rate",
                         "Marginal Visit Dropout Rate",
                         "Cumulative Dropout Prob.")
      } else {
        for(i in 1:(nv-nrow(df))) {
          df <<- rbind(df, setNames(data.frame(matrix(c(i+nrow(df), 0.05, 0, 0), nrow = 1)), names(df)))
        }
        df[,4] <<- 1-cumprod(1-df[,2])
        df[,3] <<- c(df[1,4], df[-1,4] - df[-nrow(df),4])
      }
    } else if(nv < nrow(df)) {
      df <<- df[1:nv,]
    }
    replaceData(proxy, df) 
  })
  
  ## Update from marginal
  ## Update from cumulative
}

shinyApp(ui = ui, server = server)
