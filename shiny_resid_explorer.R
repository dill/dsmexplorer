library(shiny)
library(ggplot2)


library(mgcv)

# abstract this!
load("mexdolphins_models.RData")
M <- dsm.xy.depth



resids <- residuals(M, type="deviance")

# all qqplot does is sort both inputs and plot
#rly_qq_dat <- qqplot(x=unname(qq.gam(M)), y=resids, plot=FALSE)
rly_qq_dat <- qq.gam(M)
rly_qq_dat$count <- M$y[order(resids)]
rly_qq_dat$fitted <- fitted(M)[order(resids)]

rly_qq_dat$dev_res <- rly_qq_dat$y
rly_qq_dat <- as.data.frame(rly_qq_dat)


# interface
ui <- fluidPage(
  h1("Deviance residuals"),
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
    column(width = 1,
      h4("Number of points"),
      verbatimTextOutput("brush_info_n")
    ),
    column(width = 4,
      h4("Data"),
      verbatimTextOutput("brush_info")
    )
  )
)

# processing
server <- function(input, output) {
  output$plot1 <- renderPlot({
    plot(x=rly_qq_dat$x, y=rly_qq_dat$y, pch=".",
      xlab="Theoretical quantile", ylab="Deviance residual")
  })

  output$plot2 <- renderPlot({
    par(mfrow=c(1,2))

    # plot of resids
    hist(rly_qq_dat$dev_res, main="Histogram of deviance residuals",
         breaks=seq(min(rly_qq_dat$dev_res), max(rly_qq_dat$dev_res), len=100),
         xlab="Deviance residuals")
    pd <- brushedPoints(rly_qq_dat, input$plot1_brush, xvar="x",
                       yvar="y")$dev_res
    if(length(pd)!=0){
      abline(v=pd, col="red")
    }

    # plot of counts
    hist(rly_qq_dat$count, main="Histogram of observed counts",
         breaks=seq(min(rly_qq_dat$count), max(rly_qq_dat$count), len=100),
         xlab="Counts")
    pd <- brushedPoints(rly_qq_dat, input$plot1_brush, xvar="x",
                        yvar="y")$count
    if(length(pd)!=0){
      abline(v=pd, col="red")
    }

  })

  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(rly_qq_dat, input$plot1_click, addDist = TRUE,
               xvar="x", yvar="y")
  })

  output$brush_info_n <- renderPrint({
    nrow(brushedPoints(rly_qq_dat, input$plot1_brush, xvar="x", yvar="y"))})

  output$brush_info <- renderPrint({
    brushedPoints(rly_qq_dat, input$plot1_brush, xvar="x", yvar="y")})
}

shinyApp(ui, server)



