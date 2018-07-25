AICc1 <- function(model1, ndata) {
  ssr<-model1$value
  npar<-length(as.parvec(model1))
  ndata*log(ssr/ndata)+2*npar+2*npar*(npar+1)/(ndata-npar-1)
  #2*npar + ndata*log(ssr/ndata)
} 

myPlotResiduals <- function(data=out){
 
  P<- ggplot2::ggplot(data, aes(x = value, y =prediction, colour = as.factor(condition), group = as.factor(condition))) +geom_abline(slope = 1, intercept = 0,linetype = "dashed") + geom_point() +theme_dMod(base_size = 20) +scale_color_dMod(name = "condition") 
  
  P_r<-ggplot2::ggplot(data, aes(x = value, y =weighted.residual, colour = as.factor(condition), group = as.factor(condition)))+geom_hline(yintercept = 0, linetype = "dashed") + geom_point() +theme_dMod(base_size = 20) +scale_color_dMod(name = "condition") 

  return(gridExtra::grid.arrange(P, P_r, ncol = 1))
}

myCalculateResiduals <- function(parframe, x, data, split = "condition", errmodel = NULL, ...){
  timesD <- sort(unique(c(0, do.call(c, lapply(data, function(d) d$time)))))
  if(!("index" %in% colnames(parframe)))
    parframe$index <- 1:nrow(parframe)
  
  out <- do.call(rbind,lapply(1:nrow(parframe), function(j){
    pred <- x(timesD, as.parvec(parframe,j), deriv = FALSE, ...)
    
    out_con <- do.call(rbind,lapply(names(pred), function(con){
      err <- NULL
      if (!is.null(errmodel)) {
        err <- errmodel(out = pred[[con]], pars = getParameters(pred[[con]]), conditions = con)
      }
      out <- res(data[[con]], pred[[con]], err[[con]]) 
      return(cbind(out,condition = con))
    })
    )
    
    out_par <- cbind(index = as.character(parframe[j,"index"]), out_con)
    return(out_par)
  })
  )
  return(out)
}

#plotResiduals2(pars[1,], g*x*p, data)
