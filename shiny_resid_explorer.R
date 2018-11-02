library(shiny)
library(ggplot2)


library(mgcv)
library(Distance)
library(dsm)
load("~/current/multiddfdsm/example/spermwhale.RData")
dfa <- ds(obs, truncation=8000)
adsm <- dsm(count~s(x,y), dfa, segs, obs, family=tw())


make_data <- function(model, type = "deviance"){

  # get model terms
  model_terms <- attr(terms(model), "term.labels")

  # get deviance residuals and their order
  resids <- residuals(model, type=type)
  ind <- order(resids)

  # build the data
  # cannibalised from qq.gam
  n <- length(resids)
  fam <- fix.family.rd(model$family)
  if (!is.null(fam$rd)) {
  rep<-50
  dm <- matrix(0, n, rep)
  for (i in 1:rep) {
    yr <- fam$rd(model$fitted.values, model$prior.weights, model$sig2)
    model$y <- yr
    dm[, i] <- sort(residuals(model, type = type))
  }
  Dq <- quantile(as.numeric(dm), (1:n - 0.5)/n)
  }
  rly_qq_dat <- data.frame(resids    = sort(resids),
                           quantiles = sort(unname(Dq)),
                           count     = model$y[ind],
                           fitted    = fitted(model)[ind])
  rly_qq_dat <- cbind(adsm$data[, model_terms][ind, ], rly_qq_dat)

  attr(rly_qq_dat, "model_terms") <- model_terms
  return(rly_qq_dat)
}

rly_qq_dat <- make_data(adsm)

# interface
ui <- fluidPage(
  h1("Deviance residuals"),
  h3("Select a subset of the residuals to explore their properties"),
  fluidRow(
    column(width = 3,
      plotOutput("plot1", height = 400, width=400,
        # Equivalent to: click = clickOpts(id = "plot_click")
        click = "plot1_click",
        brush = brushOpts(
          id = "plot1_brush"
        )
      )
    ),
    column(width = 6, offset=1,
      plotOutput("plot2", height = 400, width=800)
    )
  ),
  fluidRow(
    column(width = 6,
      h4("Data"),
      verbatimTextOutput("brush_info")
    ),
    column(width = 1,
      h4("Number of points:"),
      h4(textOutput("brush_info_n"))
    )
  )
)

# processing
server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(x=rly_qq_dat$quantiles, y=rly_qq_dat$resids, pch=".",
         xlab="Theoretical quantile", ylab="Deviance residual",
         main="Quantile-quantile plot")
  })

  output$plot2 <- renderPlot({
    par(mfrow=c(1,2))

    # plot of resids
    hist(rly_qq_dat$resids, main="Histogram of deviance residuals",
         breaks=seq(min(rly_qq_dat$resids), max(rly_qq_dat$resids), len=100),
         xlab="Deviance residuals")
    pd <- brushedPoints(rly_qq_dat, input$plot1_brush, xvar="resids",
                       yvar="quantiles")$resids
    if(length(pd)!=0){
      rug(pd, col="red")
    }

    # plot of counts
    hist(rly_qq_dat$count, main="Histogram of observed counts",
         breaks=seq(min(rly_qq_dat$count), max(rly_qq_dat$count), len=100),
         xlab="Counts")
    pd <- brushedPoints(rly_qq_dat, input$plot1_brush, xvar="count",
                        yvar="resids")$count
    if(length(pd)!=0){
      rug(pd, col="red")
    }

  })

  # responsive bits
  output$click_info <- renderPrint({
    nearPoints(rly_qq_dat, input$plot1_click, addDist = TRUE,
               xvar="quantiles", yvar="resids")
  })

  output$brush_info_n <- renderPrint({
    nrow(brushedPoints(rly_qq_dat, input$plot1_brush, xvar="quantiles", yvar="resids"))})

  output$brush_info <- renderPrint({
    brushedPoints(rly_qq_dat, input$plot1_brush, xvar="quantiles", yvar="resids")})
}

shinyApp(ui, server)



