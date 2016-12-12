# facet by variables in the model, colour values in
# linear predictor vs. q resids

# probably more effective with user-defined cutpoints on the covars

library(ggplot2)
library(statmod)
library(reshape2)
library(gridExtra)

soruce("multiplot.R")

rq_facet <- function(model){

  # import code here from rqgam.check to deal with nb and tw

  dat <- model$data

  # get the covars we know and love
  dat <- dat[,attr(terms(model), "term.labels")]
  dat <- dat[apply(dat, 1, function(x) !any(is.na(x))),]

  # get linear pred and quantile residuals
  dat$linear_pred <- napredict(model$na.action, model$linear.predictors)
  dat$qres <- qresiduals(model)

  # omg viewports...

  plot_list <- list()

  for(tt in attr(terms(model), "term.labels")){

    p <- ggplot(dat) +
      geom_point(aes_string(x="linear_pred", y="qres", col=tt)) +
      labs(x="Linear predictor", y="Rand quant resids") +
      theme_minimal()

    p <- p + scale_color_viridis(discrete=is.factor(dat[[tt]]))
    plot_list[[tt]] <- p
  }

  multiplot(plot_list[[1]], plot_list[[2]], plot_list[[3]], ncols=2)

  invisible(plot_list)

}
