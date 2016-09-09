library(sp)
library(rgdal)
library(rgeos)
library(shiny)
library(maptools)
library(plyr)
library(ggplot2)

library(mgcv)
library(dsm)


# given the argument fill (the covariate vector to use as the fill) and a name,
# return a geom_polygon object
# fill must be in the same order as the polygon data
grid_plot_obj <- function(fill, name, sp){

  # what was the data supplied?
  names(fill) <- NULL
  row.names(fill) <- NULL
  data <- data.frame(fill)
  names(data) <- name

  spdf <- SpatialPolygonsDataFrame(sp, data)
  spdf@data$id <- rownames(spdf@data)
  spdf.points <- fortify(spdf, region="id")
  spdf.df <- join(spdf.points, spdf@data, by="id")

  # seems to store the x/y even when projected as labelled as
  # "long" and "lat"
  spdf.df$x <- spdf.df$long
  spdf.df$y <- spdf.df$lat

  geom_polygon(aes_string(x="x",y="y",fill=name, group="group"), data=spdf.df)
}



# abstract this!
load("mexdolphins_models.RData")
load("mexpolys.RData")
M <- dsm.xy.depth


preds <- mexdolphins$preddata

pp <- predict(dsm.xy.depth, preds, preds$area)

# interface
ui <- fluidPage(
  h1("Prediction explorer"),
  fluidRow(
    column(width = 6,
      plotOutput("plot1", height = 400, width=800,
        # Equivalent to: click = clickOpts(id = "plot_click")
        click = "plot1_click"#,
        #brush = brushOpts(
        #  id = "plot1_brush"
        #)
      )
    ),
    column(width = 3, offset=1,
      plotOutput("plot2", height = 400, width=400)
    )
  ),
  fluidRow(
    column(width = 6,
      h4("Data"),
      verbatimTextOutput("click_info")
    )
  )
)

# processing
server <- function(input, output) {
  output$plot1 <- renderPlot({

    isolate(pd <<- nearPoints(preds, input$plot1_click, xvar="x", yvar="y"))
    if(nrow(pd)==0){
      ggplot(preds) +
#        geom_point(aes(x=x, y=y))# +
      grid_plot_obj(pp, "Abundance", pred.polys) +
      coord_equal() +
      theme_minimal()
#      geom_path(aes(x=x, y=y), data=survey.area)
#      labs(fill="Abundance")
    }else{
      ggplot(preds) +
        grid_plot_obj(pp, "Abundance", pred.polys) +
        geom_point(aes(x=x, y=y), data=pd, col="red")
        coord_equal() +
        theme_minimal()
    }
  })

  output$plot2 <- renderPlot({
    plot(M, select=2)

    pd <- nearPoints(preds, input$plot1_click, xvar="x", yvar="y")
    if(nrow(pd)>0){
      abline(v=pd$depth[1], col="red")
    }
  })

  output$click_info <- renderPrint({
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(preds, input$plot1_click)
  })


}

shinyApp(ui, server)



