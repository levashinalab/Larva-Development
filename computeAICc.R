AICc1 <- function(model1) {
  ssr<-model1$ssr
  npar<-length(model1$par)
  ndata<-length(model1$residuals)
  ndata*log(ssr/ndata)+2*npar+2*npar*(npar+1)/(ndata-npar-1)
} 

plotResiduals2 <- function(parframe, x, data, split = "condition", errmodel = NULL, ...){
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
#  if (!is.null(errmodel)) {
#    out <- plyr::ddply(out, split, summarise, res = sum(weighted.residual^2 + log(sigma^2))) 
#  } else{
#    out <- plyr::ddply(out, split, summarise, res = sum(weighted.residual^2)) 
#  }
#  groupvar <- split[1]
#  if(length(split) > 1){
#    groupvar <- split[2]
#  }
  
  P<- ggplot2::ggplot(out, aes(x = value, y =prediction, colour = as.factor(condition), group = as.factor(condition))) +geom_abline(slope = 1, intercept = 0,linetype = "dashed") + geom_point() +theme_dMod(base_size = 20) +scale_color_dMod(name = "condition") 
  
  P_r<-ggplot2::ggplot(out, aes(x = value, y =weighted.residual, colour = as.factor(condition), group = as.factor(condition)))+geom_hline(yintercept = 0, linetype = "dashed") + geom_point() +theme_dMod(base_size = 20) +scale_color_dMod(name = "condition") 
  #P <- ggplot2::ggplot(out, aes_string(x = split[1], y = "res", color = groupvar, group = groupvar)) + theme_dMod() + geom_point() + geom_line()
  
 # if(length(split) > 2)
  #  P <- P + facet_wrap(split[3:length(split)]) 
  
  #attr(P,"out") <- out
  
  return(gridExtra::grid.arrange(P, P_r, ncol = 1))
}

#plotResiduals2(pars[1,], g*x*p, data)
