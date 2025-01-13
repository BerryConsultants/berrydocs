library(shiny)
library(bslib)
library(ggplot2)

qinvgamma = function (p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, 
                      log.p = FALSE) 
{
  if (missing(rate) && !missing(scale)) 
    rate <- 1/scale
  qgamma(1 - p, shape, rate, lower.tail = lower.tail, log.p = log.p)^(-1)
}
pinvgamma = function (q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, 
                      log.p = FALSE) 
{
  if (missing(rate) && !missing(scale)) 
    rate <- 1/scale
  pgamma(1/q, shape, rate, lower.tail = !lower.tail, log.p = log.p)
}
dinvgamma = function (x, shape, rate = 1, scale = 1/rate, log = FALSE) 
{
  if (missing(rate) && !missing(scale)) 
    rate <- 1/scale
  log_f <- dgamma(1/x, shape, rate, log = TRUE) - 2 * log(x)
  if (log) 
    return(log_f)
  exp(log_f)
}
rinvgamma = function (n, shape, rate = 1, scale = 1/rate) 
{
  if (missing(rate) && !missing(scale)) 
    rate <- 1/scale
  1/rgamma(n, shape, rate)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #radioButtonDiv {
      display: flex;
      justify-content: center;
      }"
    ))
  ),
  withMathJax(),
  titlePanel(h1("Inverse Gamma Distribution in FACTS", align = "center")),
  h5('$$\\sigma^2 \\sim \\text{IG}\\left(\\alpha=\\frac{\\text{weight}}{2}, \\beta=\\frac{\\text{center}^2\\;*\\;\\text{weight}}{2}\\right)$$'),
  sidebarLayout(
    sidebarPanel(width = 3,
                 style = "border: 1px solid #000000",
                 titlePanel(h4("Center/Weight Parameterization")),
                 fluidRow(
                   column(width = 10, offset = 1, numericInput(inputId = "center", label = "Center of SD:", value = 5, min = 0, max = Inf, step = "any")),
                   column(width = 10, offset = 1, numericInput(inputId = "weight", label = "Weight:", value = 2, min = 0.001, max = Inf, step = "any")),
                 ),
                 titlePanel(h4("Alpha/Beta Parameterization")),
                 fluidRow(
                   column(width = 10, offset = 1, numericInput(inputId = "alpha", label = "Alpha:", value = 1, min = 0.0005, max = Inf, step = "any")),
                   column(width = 10, offset = 1, numericInput(inputId = "beta", label = "Beta:", value = 25, min = 0, max = Inf, step = "any"))
                 )
    ),
    mainPanel(width = 9,
              wellPanel(style = "background: white; border: 1px solid #000000",
                        fluidRow(
                          column(12,
                                 div(
                                   radioButtons("whichParam", 
                                                "Which parameter should be summarized?", 
                                                choiceNames = c("Variance \\((\\sigma^2)\\)", "Std. Dev. \\((\\sigma)\\)"), 
                                                choiceValues = c("sigma2", "sigma"), 
                                                selected = "sigma2", 
                                                inline = TRUE),
                                   id = "radioButtonDiv")
                          )
                        ),
                        uiOutput("sectionTitle"),
                        fluidRow(
                          column(width = 4, value_box("Mode", value= uiOutput("mode"), theme = value_box_theme(bg = "#0b2545"))),
                          column(width = 4, value_box("Median", value= uiOutput("median"), theme = value_box_theme(bg = "#ba5a31"))),
                          column(width = 4, value_box("Mean", value= uiOutput("mean"), theme = value_box_theme(bg = "#06402b")))
                        ),
                        br(),
                        plotOutput("igDistributionPlot")
              )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Set delay for re-calculation after input changes
  delayMs <- 1000

  # Track which parameter set (alpha/beta or center/weight) changed last
  lastChange <- reactiveVal(NULL)
  # Track the last time any parameter set changed
  lastChangeTime <- reactiveVal(Sys.time())

  # Create reactive containters for input changes
  desiredAlpha <- reactiveVal(NULL)
  desiredBeta <- reactiveVal(NULL)
  desiredCenter <- reactiveVal(NULL)
  desiredWeight <- reactiveVal(NULL)
  finalAlpha <- reactiveVal(NULL)
  finalBeta <- reactiveVal(NULL)
  
  # When center/weight changes, calculate and then store the needed alpha/beta changes
  observeEvent(c(input$center, input$weight), {
    ctr <- input$center
    wgt <- input$weight

    if(!is.null(ctr) && !is.null(wgt) && !is.na(ctr) && !is.na(wgt) && ctr > 0 && wgt > 0) {
      desiredAlpha(wgt / 2)
      desiredBeta((ctr^2 * wgt) / 2)
      lastChange("center_weight")
      lastChangeTime(Sys.time())
    }
  }, ignoreInit = TRUE)

  # When alpha/beta changes, calculate and then store the needed center/weight changes
  observeEvent(c(input$alpha, input$beta), {
    a <- input$alpha
    b <- input$beta

    if(!is.null(a) && !is.null(b) && !is.na(a) && !is.na(b) && a > 0 && b > 0) {
      desiredCenter(sqrt(b / a))
      desiredWeight(2 * a)
      lastChange("alpha_beta")
      lastChangeTime(Sys.time())
    }
  }, ignoreInit = TRUE)

  # Periodically check if input updates should be copied
  observe({
    invalidateLater(delayMs, session)
    # If enough time has passed since the last change of the input sets
    if (difftime(Sys.time(), lastChangeTime(), units = "secs") > (delayMs / 1000)) {
      isolate({
        if (lastChange() == "center_weight" && !is.null(desiredAlpha()) && !is.null(desiredBeta())) {
          updateNumericInput(session, "alpha", value = desiredAlpha())
          updateNumericInput(session, "beta", value = desiredBeta())
          finalAlpha(desiredAlpha())
          finalBeta(desiredBeta())
          # Reset containers
          desiredAlpha(NULL)
          desiredBeta(NULL)
        } else if (lastChange() == "alpha_beta" && !is.null(desiredCenter()) && !is.null(desiredWeight())) {
          updateNumericInput(session, "center", value = desiredCenter())
          updateNumericInput(session, "weight", value = desiredWeight())
          # Reset containers
          desiredCenter(NULL)
          desiredWeight(NULL)
        }
      })
    }
  })
  
  meanHolder = reactiveVal(NA)
  
  output$mean = renderText({
    cat("CalcMean\n")
    a = finalAlpha()
    b = finalBeta()
    
    if(a > 1) {
      if(input$whichParam == "sigma2") {
        meanHolder(b/(a-1))
        return(round(b/(a-1), 2))
      } else {
        tmp = mean(sqrt(rinvgamma(10000, a, b)))
        meanHolder(tmp)
        return(round(tmp, 2))
      }
    } else {
      meanHolder(NA)
      return("-")
    }
  })
  output$median = renderText({
    cat("CalcMedian\n")
    a = finalAlpha()
    b = finalBeta()
    
    if(input$whichParam == "sigma2") {
      return(round(qinvgamma(0.5, shape = a, rate = b), 2))
    } else {
      return(round(sqrt(qinvgamma(0.5, shape = a, rate = b)),2))
    }
  })
  
  output$mode = renderText({
    cat("CalcMode\n")
    a = finalAlpha()
    b = finalBeta()
    
    if(input$whichParam == "sigma2") {
      return(round(b/(a+1),2))
    } else {
      lmode = 0
      hmode = max(b/(a+1), 2)
      smode = seq(lmode, hmode, length.out = 100001)
      dmode = dinvgamma(smode, a, b)*(1/(2*sqrt(smode)))
      calcMode = sqrt(smode[which.max(dmode)])
      
      return(round(calcMode, 2))
    }
  })
  
  output$sectionTitle = renderUI({
    cat("ChangeHeader\n")
    if(input$whichParam == "sigma2") {
      return(h4("Characteristics of the Variance"))
    } else {
      return(h4("Characteristics of the Standard Deviation"))
    }
  })
  
  output$igDistributionPlot = renderPlot({
    cat("MakePlot\n")
    a = finalAlpha()
    b = finalBeta()
    wchParam = input$whichParam
    isolate({
      if(!is.null(a) & !is.null(b) & !is.null(input$center) & !is.null(input$weight) &
         a > 0 & b > 0 & input$center > 0 & input$weight > 0) {
        if(wchParam == "sigma2") {
          lowerbound = 0
          upperbound = qinvgamma(.995, a, b)
          upperbound = min(1e15, upperbound)
          
          sq0to1 = seq(1e-10, 1-(1e-10), length.out = 1000)
          sq = qinvgamma(sq0to1, a, b)
          #  sq = seq(1e-10,upperbound, length.out = 1001)
          
          df = data.frame(sq = sq,
                          density = dinvgamma(sq, a, b))
          
          modeDensity = dinvgamma(b/(a+1), a, b)
          
          if(df$density[sum(is.finite(df$density))] > modeDensity*.01) {
            maxPlot = sq[length(sq)]
          } else {
            maxPlot = sq[min(which(df$density < modeDensity*.01 & sq > sq[which.max(df$density)]))]
          }
          maxPlot = max(maxPlot, 2*(b/(a+1)))
          
          sq0to1 = c(10^seq(-17, -2, length.out = 51), seq(.0101, pinvgamma(maxPlot*1.1, a, b), length.out = 1001))
          sq = qinvgamma(sq0to1, a, b)
          
          df = data.frame(sq = sq,
                          density = dinvgamma(sq, a, b))
          
          p1 = ggplot(data = df) + geom_line(aes(x = sq, y = density)) + geom_area(aes(x = sq, y = density), fill = "gray80") +
            theme_bw() +
            xlab("Variance") + ylab("Density") +
            scale_y_continuous(expand = c(0, 0)) +
            ggtitle("Probability Distribution of the Variance") +
            coord_cartesian(xlim = c(0, maxPlot), ylim = c(0, modeDensity*1.3)) +
            theme(text = element_text(size = 18))
          
          if(a > 1) {
            meantmp = meanHolder()
            p1 = p1 + geom_vline(xintercept = meantmp, color = "#06402b") + annotate(geom="label", color = "white", fill = "#06402b", x = meantmp, y = modeDensity*1.15, label = "mean", angle=90, size = 4.5)
          }
          p1 = p1 + geom_vline(xintercept = b/(a+1), color = "#0b2545") + annotate(geom="label", color = "white", fill = "#0b2545", x = b/(a+1), y = modeDensity*1.15, label = "mode", angle=90, size = 4.5)
          
          if(is.finite(qinvgamma(.5, a, b))) {
            p1 = p1 + geom_vline(xintercept = qinvgamma(.5, a, b), color = "#ba5a31") + annotate(geom="label", color = "white", fill = "#ba5a31", x = qinvgamma(.5, a, b), y = modeDensity*1.15, label = "median", angle=90, size = 4.5)
          }
          
          p1 = p1 + geom_vline(xintercept = b/a) + annotate(geom="label", x = b/a, y = modeDensity*1.15, label = "center", angle=90, size = 4.5)
          
          p1
        }
        else {
          #df = data.frame(sq = sq,
          #density = dinvgamma(sq, a, b)*(1/(2*sqrt(sq))))
          
          lowerbound = 0
          upperbound = qinvgamma(.995, a, b)
          upperbound = min(1e15, upperbound)
          
          sq0to1 = seq(1e-10, 1-(1e-10), length.out = 1000)
          sq = qinvgamma(sq0to1, a, b)
          df = data.frame(sq = sq,
                          density = dinvgamma(sq, a, b)*(2*sqrt(sq)))#(1/(2*sqrt(sq))))
          
          lmode = 0
          hmode = max(b/(a), 2)
          smode = seq(lmode, hmode, length.out = 1001)
          dmode = dinvgamma(smode, a, b)*(2*sqrt(smode))#(1/(2*sqrt(smode)))
          wchMax = which.max(dmode)
          if(wchMax > 1) {
            smode2 = seq(smode[wchMax-1], smode[wchMax + 1], length.out = 1001)
            dmode2 = dinvgamma(smode2, a, b)*(2*sqrt(smode2))#(1/(2*sqrt(smode2)))
            wchMax = which.max(dmode2)
          } else {
            smode2 = seq(0, smode[2], length.out = 1000)
            dmode2 = dinvgamma(smode2, a, b)*(2*sqrt(smode2))#(1/(2*sqrt(smode2)))
            wchMax = which.max(dmode2)
          }
          calcMode = sqrt(smode2[wchMax])
          
          modeDensity = dinvgamma(calcMode^2, a, b)*(2*calcMode)#(1/(2*calcMode))
          
          if(df$density[sum(is.finite(df$density))] > modeDensity*.01) {
            maxPlot = sq[length(sq)]
          } else {
            maxPlot = sq[min(which(df$density < modeDensity*.01 & sq > sq[which.max(df$density)]))]
          }
          maxPlot = max(maxPlot, 2*smode2[wchMax])
          
          sq = (seq(1e-10,sqrt(maxPlot*1.1), length.out = 1001))^2
          
          df = data.frame(sq = sq,
                          density = dinvgamma(sq, a, b)*(2*sqrt(sq)))#(1/(2*sqrt(sq))))
          
          p1 = ggplot(data = df) + geom_line(aes(x = sqrt(sq), y = density)) + geom_area(aes(x = sqrt(sq), y = density), fill = "gray80") +
            theme_bw() +
            xlab("Standard Deviation") + ylab("Density") +
            scale_y_continuous(expand = c(0, 0)) +
            ggtitle("Probability Distribution of the Standard Deviation") +
            coord_cartesian(xlim = c(0, sqrt(maxPlot)), ylim = c(0, modeDensity*1.3)) +
            theme(text = element_text(size = 18))
          
          if(a > 1) {
            meantmp = meanHolder()
            p1 = p1 + geom_vline(xintercept = meantmp, color = "#06402b") + annotate(geom="label", color = "white", fill = "#06402b", x = meantmp, y = modeDensity*1.15, label = "mean", angle=90, size = 4.5)
          }
          
          p1 = p1 + geom_vline(xintercept = calcMode, color = "#0b2545") + annotate(geom="label", color = "white", fill = "#0b2545", x = calcMode, y = modeDensity*1.15, label = "mode", angle=90, size = 4.5)
          if(is.finite(sqrt(qinvgamma(.5, a, b)))) {
            p1 = p1 + geom_vline(xintercept = sqrt(qinvgamma(.5, a, b)), color = "#ba5a31") + annotate(geom="label", color = "white", fill = "#ba5a31", x = sqrt(qinvgamma(.5, a, b)), y = modeDensity*1.15, label = "median", angle=90, size = 4.5)
          }
          p1 = p1 + geom_vline(xintercept = sqrt(b/a)) + annotate(geom="label", x = sqrt(b/a), y = modeDensity*1.15, label = "center", angle=90, size = 4.5)
          
          p1
        }
      }
    })
  })
}

shinyApp(ui = ui, server = server)
