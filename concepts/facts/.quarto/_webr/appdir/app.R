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
                          min = 2, max = 20, value = 5, step = 1)),
  DTOutput("dataInputTable"),
  h5("Double click on a cell to edit.", align = "center"),
  br(),
  titlePanel(h2("Plot a subject's prior predictive", align = "center")),
  fluidRow(
    #column(5, offset = 1, uiOutput("slider")),
    column(5, offset = 1, uiOutput("slider")),
    column(6, fluidRow(
      column(6, offset = 2, checkboxInput("fixAlpha", "Fix alpha at its mean?", value = FALSE, width = "100%")),
      column(6, offset = 2, checkboxInput("fixBeta", "Fix beta at its mean?", value = FALSE, width = "100%")),
      column(6, offset = 2, checkboxInput("removePredictive", "Remove endpoint prior predictive?", value = FALSE, width = "100%"))
    ))
  ),
  fluidRow(
    column(6, plotOutput("visitToFinalPlot")),
    column(6, plotOutput("priorPredictive"))
  )

)

getLowerMedianUpper = function(earlyVisitVal, alpha = c(0,1), beta = c(0,1), lambda = c(1,1)) {
  distMeanFinal = c(alpha[1] + beta[1]*earlyVisitVal,
                    sqrt(alpha[2]^2 + beta[2]^2*earlyVisitVal^2))

  deviates = rnorm(10000)
  deviates = (deviates - mean(deviates))/(sd(deviates))

  if(any(is.na(lambda))) {
    samps = (deviates*distMeanFinal[2] + distMeanFinal[1])
  } else {
    samps = (deviates*distMeanFinal[2] + distMeanFinal[1]) + rnorm(10000, 0, sd = sqrt(1/rgamma(10000, lambda[2]/2, lambda[1]^2*lambda[2]/2)))
  }

  distValueFinal = c(mean(samps), sd(samps))

  return(list("meanFinal" = data.frame(lower = distMeanFinal[1] + qnorm(.025)*distMeanFinal[2],
                                       median = distMeanFinal[1],
                                       upper = distMeanFinal[1] + qnorm(0.975)*distMeanFinal[2]),
              "predictionFinal" = data.frame(lower = quantile(samps,.025),
                                             median = median(samps),
                                             upper = quantile(samps,.975))))
}



server <- function(input, output, session) {

  df = data.frame(VisitResponse = c(2,5,3,7,11),
                  alphaPriorMean = 0,
                  alphaPriorSD = 2,
                  betaPriorMean = 1,
                  betaPriorSD = 2,
                  lambdaPriorCenter = 5,
                  lambdaPriorWeight = 3)
  df[5,] = c(5, NA, NA, NA, NA, NA, NA)
  row.names(df) = paste("Visit", 1:5)

  ## Render DF to actually change
  output$dataInputTable = renderDT(datatable(df,
                                             options = list(
                                               pageLength = 20,
                                               dom = "t",
                                               autoWidth = TRUE,
                                               columnDefs = list(list(className = 'dt-center', orderable = FALSE, width = '75px', targets = 0:7),
                                                                 list(width = "150px", targets = 0:1))
                                             ),
                                             container = sketch,
                                             rownames = TRUE,
                                             # escape = FALSE,
                                             selection = 'none',
                                             editable = list(target = "cell")
  ) |> formatStyle(c(1,3,5,7), `border-right` = "solid 1px") |>
    formatRound(1, digits = 4) |> formatRound(2:7, digits = 2) |>
    formatStyle(0,
                target = "row",
                backgroundColor = styleEqual(paste("Visit",input$lastVisitWithData),
                                             "lightblue",
                                             'white'))
  )

  ## Update from Conditional
  proxy = dataTableProxy('dataInputTable')

  observeEvent(input$dataInputTable_cell_edit, {
    info = input$dataInputTable_cell_edit
    i = info$row
    j = info$col
    v = info$value

    if(i < nrow(df) | j == 1) {
      df <<- editData(df, info)
    } else {
      df[i,j] <<- NA
    }
    replaceData(proxy, df)
  })

  observe({
    nv = input$numVisits
    if(nv > nrow(df)) {
      tempd = df
      for(i in 1:(nv-nrow(df))) {
        tempd = rbind(tempd, setNames(data.frame(c(tempd[nrow(tempd),])), names(tempd)))
        rownames(tempd)[nrow(tempd)] = paste("Visit", nrow(tempd))
        tempd[nrow(tempd)-1,-1] = tempd[nrow(tempd)-2,-1]
      }
      df <<- tempd
    } else if(nv < nrow(df)) {
      df <<- df[1:nv,]
      df[nv,-1] <<- NA
    }
    replaceData(proxy, df)
  })

  sliderParams <- reactiveValues(max = 5, value = 3)
  output$slider <- renderUI({
    sliderInput("lastVisitWithData", "Last complete visit:", min = 1, max = sliderParams$max, value = sliderParams$value, step = 1)
  })
  observeEvent(input$numVisits, {
    sliderParams$max = input$numVisits
    if(!is.null(input$lastVisitWithData)) {
      sliderParams$value <- min(input$lastVisitWithData, input$numVisits)
    } else {
      sliderParams$value = 3
    }
  })

  output$priorPredictive = renderPlot({
    req(input$lastVisitWithData)
    input$dataInputTable_cell_edit

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

    # tempDF = rbind(setNames(data.frame(c(tempDF[1,])), names(tempDF)), tempDF)
    # tempDF[1,1] = 0
    # tempDF$RowVisitIndex[1] = 0
    # rownames(tempDF)[1] = "Baseline"

    p1 = ggplot() +
      geom_point(dat = tempDF, aes(x = RowVisitIndex, y = VisitResponse, color = visitKnown), size = 3) +
      scale_color_manual(breaks = c("included", "excluded"), values = c("black", "gray70"), guide = "none") +
      coord_cartesian(xlim = c(0, finalVisitIndex)) +
      scale_x_continuous(breaks = 0:finalVisitIndex, labels = c("Baseline", 1:finalVisitIndex)) +
      xlab("Visit") + ylab("Response") + ggtitle("Predicting Final Endpoint of a Subject") +
      theme_bw() +
      theme(plot.title = element_text(size = 20), text = element_text(size = 16), legend.justification = "left", legend.position = "bottom", legend.direction = "vertical")

    if(lvIndex < finalVisitIndex) {
      if(!input$removePredictive) {
        p1 = p1 +
          geom_segment(data = dataToPlot[[2]], aes(x = finalVisitIndex, y = lower, yend = upper), color = "darkgreen", linewidth = 2.5) +
          annotate(geom = "point", x = finalVisitIndex, y = dataToPlot[[2]]$median, color = "darkgreen", size = 3, shape = 18) +
          geom_ribbon(data = NULL, aes(x = c(lvIndex, finalVisitIndex),
                                       ymin = c(tempDF$VisitResponse[lvIndex], dataToPlot[[2]]$lower),
                                       ymax = c(tempDF$VisitResponse[lvIndex], dataToPlot[[2]]$upper),
                                       fill = "preds"),  color = NA, alpha = .2)
      }
      p1 = p1 +
        geom_segment(data = dataToPlot[[1]], aes(x = finalVisitIndex, y = lower, yend = upper), color = "darkblue", linewidth = 1.5) +
        annotate(geom = "point", x = finalVisitIndex, y = dataToPlot[[1]]$median, color = "darkblue", size = 3, shape = 18) +
        annotate(geom = "segment", x = lvIndex, xend = finalVisitIndex, y = tempDF$VisitResponse[lvIndex], yend = dataToPlot[[1]]$median, linetype = "dashed", color = "darkblue")+
        geom_ribbon(data = NULL, aes(x = c(lvIndex, finalVisitIndex),
                                     ymin = c(tempDF$VisitResponse[lvIndex], dataToPlot[[1]]$lower),
                                     ymax = c(tempDF$VisitResponse[lvIndex], dataToPlot[[1]]$upper)),
                    fill = "darkblue",  color = NA, alpha = .4)
    } else {
      p1 = p1 + annotate(geom="text", label = "Final Visit Value Known",
                         alpha = .5, size = 10, x = (finalVisitIndex)/2, y = Inf, vjust = 1.3)
    }

    p1 = p1 + scale_fill_manual(NULL, breaks = c("preds"), limits = c("preds"), values = c("darkgreen"), labels = c("Prior predictive distribution for final endpoint of subject."))
    #  guides(fill = guide_legend(override.aes = list(limits = c("darkgreen", "darkblue"), labels = c("Prior distribution for mean of imputed value.", "Prior predictive distribution for final endpoint of subject."))))

    p1
  })

  output$visitToFinalPlot = renderPlot({
    input$dataInputTable_cell_edit
    req(input$lastVisitWithData)

    lvIndex = input$lastVisitWithData
    finalVisitIndex = input$numVisits

    tempDF = df

    min_s = ifelse(min(tempDF$VisitResponse, na.rm = TRUE) < 0, (min(tempDF$VisitResponse, na.rm = TRUE)+1)*1.1, 0)
    max_s = ifelse(max(tempDF$VisitResponse, na.rm = TRUE) > 0, (max(tempDF$VisitResponse, na.rm = TRUE)+1)*1.1, 0)

    s = seq(min_s-5, max_s+5, length.out = 101)

    meanDist_mean = tempDF$alphaPriorMean[lvIndex] + tempDF$betaPriorMean[lvIndex]*s
    if(!input$fixAlpha & !input$fixBeta) {
      meanDist_sd = sqrt(tempDF$alphaPriorSD[lvIndex]^2 + tempDF$betaPriorSD[lvIndex]^2*s^2)
    } else if(input$fixAlpha & !input$fixBeta) {
      meanDist_sd = sqrt(tempDF$betaPriorSD[lvIndex]^2*s^2)
    } else if(!input$fixAlpha & input$fixBeta) {
      meanDist_sd = rep(sqrt(tempDF$alphaPriorSD[lvIndex]^2), length(s))
    } else {
      meanDist_sd = rep(0, length(meanDist_mean))
    }

    plotDF = data.frame(earlyVis = s,
                        lower = meanDist_mean + qnorm(0.025)*meanDist_sd,
                        median= meanDist_mean,
                        upper = meanDist_mean + qnorm(0.975)*meanDist_sd)

    if(lvIndex < finalVisitIndex) {
      if(!input$removePredictive) {
        numSamps = 2500

        deviates = rnorm(numSamps)
        deviates = (deviates - mean(deviates))/(sd(deviates))
        normigsamps = rnorm(numSamps, 0, sd = sqrt(1/rgamma(numSamps, tempDF$lambdaPriorWeight[lvIndex]/2, tempDF$lambdaPriorCenter[lvIndex]^2*tempDF$lambdaPriorWeight[lvIndex]/2)))

        distVals = matrix(NA, ncol = 2, nrow = length(s))
        for(i in 1:length(s)) {
          distVals[i,] = quantile((deviates*meanDist_sd[i] + meanDist_mean[i] + normigsamps), c(0.025, 0.975))
        }

        plotDF$lowerPred = distVals[,1]
        plotDF$upperPred = distVals[,2]
      }

      p2 = ggplot(data = plotDF) + geom_abline(aes(slope = tempDF$betaPriorMean[lvIndex], intercept = tempDF$alphaPriorMean[lvIndex]), color = "darkblue", linewidth = 1.5) +
        geom_ribbon(aes(x = s, ymin = lower, ymax = upper, fill = "means"), color = NA, alpha = 0.4)

      if(!input$removePredictive) {
        p2 = p2 + geom_ribbon(aes(x = s, ymin = lowerPred, ymax = upperPred), fill = "darkgreen", color = NA, alpha = 0.2)
      }
      p2 = p2 +
        coord_cartesian(xlim = c(min_s, max_s)) +
        xlab("Early Visit Known Value") + ylab("Expectation of Final Visit Response") + ggtitle("Expectation of Final Visit Given Early Visit") +
        theme_bw() +
        theme(plot.title = element_text(size = 20), text = element_text(size = 16), legend.justification = "right", legend.position = "bottom", legend.direction = "vertical")
    } else {
      p2 = ggplot(data = NULL) + geom_abline(aes(fill = "means"), slope = 1, intercept = 0, color = "darkblue", linewidth = 1.5) +
        coord_cartesian(xlim = c(min_s, max_s), ylim = c(min_s, max_s)) +
        annotate(geom="text", label = "Final Visit Value Known",
                 alpha = .5, size = 10, x = (max_s + min_s)/2, y = Inf, vjust = 1.3) +
        xlab("Early Visit Known Value") + ylab("Expectation of Final Visit Response") + ggtitle("Expectation of Final Visit Given Early Visit") +
        theme_bw() +
        theme(plot.title = element_text(size = 20), text = element_text(size = 16), legend.justification = "right", legend.position = "bottom", legend.direction = "vertical")
    }
    p2 = p2 + scale_fill_manual(NULL, breaks = c("means"), limits = c("means"), values = c("darkblue"), labels = c("Prior distribution for mean of imputed value."))
    p2
  })
}

shinyApp(ui = ui, server = server)
