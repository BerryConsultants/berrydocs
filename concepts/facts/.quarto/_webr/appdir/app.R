library(shiny)
library(DT)
library(ggplot2)
library(htmltools)

alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(rowspan = 2, style = "border-right: solid 1px;",'Observed Visit Data'),
      th(colspan = 2, class="  dt-center", style = "border-right: solid 1px;",'\u3B1 priors'),
      th(colspan = 2, class="  dt-center", style = "border-right: solid 1px;",'\u3B2 priors'),
      th(colspan = 2, class="  dt-center", style = "border-right: solid 1px;",'\u3BB priors')
    ),
    tr(
      th("mean"),
      th(style = "border-right: solid 1px;", "SD"),
      th("mean"),
      th(style = "border-right: solid 1px;", "SD"),
      th("center"),
      th(style = "border-right: solid 1px;", "weight"),
    )
  )
))

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      .my_col_class {
         align-content: center;
      }")
    )
  ),
  
  titlePanel(h1("Linear Regression LM Priors", align = "center")),
  alignCenter(sliderInput("numVisits", "Number of visits:", 
                          min = 1, max = 20, value = 5, step = 1)),
  DTOutput("dataInputTable"),
  br(),
  titlePanel(h2("Plot a subject's prior predictive", align = "center")),
  fluidRow(
    #column(5, offset = 1, uiOutput("slider")),
    column(5, offset = 1, sliderInput(inputId = "lastVisitWithData", 
                                      label = "Last complete visit:", 
                                      min = 1, max = 5,
                                      value = 3, step = 1)),
    column(6, fluidRow(
      column(6, offset = 2, checkboxInput("fixAlpha", "Fix alpha at its mean?", value = FALSE, width = "100%")),
      column(6, offset = 2, checkboxInput("fixBeta", "Fix beta at its mean?", value = FALSE, width = "100%"))
    ))
  ),
  plotOutput("priorPredictive")
)

getLowerMedianUpper = function(earlyVisitVal, alpha = c(0,1), beta = c(0,1), lambda = c(1,1)) {
  distMeanFinal = c(alpha[1] + beta[1]*earlyVisitVal,
                    sqrt(alpha[2]^2 + beta[2]^2*earlyVisitVal^2))
  
  deviates = rnorm(10000)
  deviates = (deviates - mean(deviates))/(sd(deviates))

  samps = (deviates*distMeanFinal[2] + distMeanFinal[1]) + 1/rgamma(10000, lambda[2]/2, lambda[1]^2*lambda[2]/2)
  
  distValueFinal = c(mean(samps), sd(samps))
  
  return(list("meanFinal" = data.frame(lower = distMeanFinal[1] + qnorm(.025)*distMeanFinal[2],
                                       median = distMeanFinal[1],
                                       upper = distMeanFinal[1] + qnorm(0.975)*distMeanFinal[2]),
              "predictionFinal" = data.frame(lower = distValueFinal[1] + qnorm(.025)*distValueFinal[2],
                                             median = distValueFinal[1],
                                             upper = distValueFinal[1] + qnorm(0.975)*distValueFinal[2])))
}



server <- function(input, output, session) {
  
  df = data.frame(VisitResponse = rep(0, 5),
                  alphaPriorMean = 0,
                  alphaPriorSD = 10,
                  betaPriorMean = 0,
                  betaPriorSD = 10,
                  lambdaPriorCenter = 10, 
                  lambdaPriorWeight = 1)
  
  row.names(df) = paste("Visit", 1:5)
  
  ## Render DF to actually change
  output$dataInputTable = renderDT(datatable(df, 
                                             options = list(
                                               pageLength = 20,
                                               dom = "t",
                                               autoWidth = TRUE,
                                               columnDefs = list(list(className = 'dt-center', orderable = FALSE, width = '150px', targets = 0:7))
                                             ),
                                             container = sketch,
                                             rownames = TRUE,
                                             # escape = FALSE,
                                             selection = 'none',
                                             editable = list(target = "cell")
  ) |> formatStyle(c(1,3,5,7), `border-right` = "solid 1px") |>
       formatRound(1, digits = 4) |> formatRound(2:7, digits = 2)
  )
  
  ## Update from Conditional
  proxy = dataTableProxy('dataInputTable')
  
  observeEvent(input$dataInputTable_cell_edit, {
    info = input$dataInputTable_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    
    df <<- editData(df, info)
    
    replaceData(proxy, df) 
  })
  
  observe({
    nv = input$numVisits
    if(nv > nrow(df)) {
      tempd = df
      for(i in 1:(nv-nrow(df))) {
        tempd = rbind(tempd, setNames(data.frame(c(tempd[nrow(tempd),])), names(tempd)))
        rownames(tempd)[nrow(tempd)] = paste("Visit", nrow(tempd))
      }
      df <<- tempd
    } else if(nv < nrow(df)) {
      df <<- df[1:nv,]
    }
    replaceData(proxy, df) 
    
    updateSliderInput(session, "lastVisitWithData", max = nv, value = min(nv, input$lastVisitWithData))
  })
  
  output$priorPredictive = renderPlot({
    lvIndex = input$lastVisitWithData
    finalVisitIndex = input$numVisits
    tempDF = df
    dataToPlot = getLowerMedianUpper(tempDF[lvIndex,1],
                                     alpha = c(tempDF[lvIndex,2], ifelse(input$fixAlpha, 0, tempDF[lvIndex,3])),
                                     beta = c(tempDF[lvIndex,4], ifelse(input$fixBeta, 0, tempDF[lvIndex,5])),
                                     lambda = c(tempDF[lvIndex,6], tempDF[lvIndex,7]))
    
    tempDF$RowVisitIndex = 1:nrow(tempDF)
    tempDF$visitKnown = "included"
    tempDF$visitKnown[tempDF$RowVisitIndex > lvIndex] = "excluded"
    
    p1 = ggplot() + 
      geom_point(dat = tempDF, aes(x = RowVisitIndex, y = VisitResponse, color = visitKnown)) + 
      scale_color_manual(breaks = c("included", "excluded"), values = c("black", "gray70"), guide = "none") +
      coord_cartesian(xlim = c(1, finalVisitIndex)) + 
      xlab("Visit Index") + ylab("Response") +
      theme_bw() + 
      theme(text = element_text(size = 18))
    
    if(lvIndex < finalVisitIndex) {
      p1 = p1 + 
        geom_segment(data = dataToPlot[[2]], aes(x = finalVisitIndex, y = lower, yend = upper), color = "darkgreen", linewidth = 2.5) +
        annotate(geom = "point", x = finalVisitIndex, y = dataToPlot[[2]]$median, color = "darkgreen", size = 3, shape = 18) +
        annotate(geom = "ribbon", x = c(lvIndex, finalVisitIndex),
                 ymin = c(tempDF$VisitResponse[lvIndex], dataToPlot[[2]]$lower),
                 ymax = c(tempDF$VisitResponse[lvIndex], dataToPlot[[2]]$upper),
                 fill = "darkgreen", color = NA, alpha = .15) +
        geom_segment(data = dataToPlot[[1]], aes(x = finalVisitIndex, y = lower, yend = upper), color = "darkblue", linewidth = 1.5) +
        annotate(geom = "point", x = finalVisitIndex, y = dataToPlot[[1]]$median, color = "darkblue", size = 3, shape = 18) +
        annotate(geom = "ribbon", x = c(lvIndex, finalVisitIndex),
                 ymin = c(tempDF$VisitResponse[lvIndex], dataToPlot[[1]]$lower),
                 ymax = c(tempDF$VisitResponse[lvIndex], dataToPlot[[1]]$upper),
                 fill = "darkblue", color = NA, alpha = .4)
    } else {
      p1 = p1 + annotate(geom="text", label = "Final Visit Value Known",
                         alpha = .5, size = 10, x = (finalVisitIndex+1)/2, y = Inf, vjust = 1.3)
    }
    
    p1
  })
}

shinyApp(ui = ui, server = server)
