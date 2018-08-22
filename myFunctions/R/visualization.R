#'
#'Plot logit curve
#'
#'Plot the logit curve along with dot histogram of the actual target values. 
#'
#' @param df	Dataframe containing the target as well as variable to plot
#' @param varName	Name of the variable that has to be plotted
#' @param target	Name of the target variable 
#' @return Logit plot as output
#' @export 
#' 

plot_logit_curve <- function(df,varName,target){
  require("ggplot2")
  require("dplyr")
  plt_dat <- df[,c(varName,target)]
  names(plt_dat) <- c("idv","dv")
  glm_model <- glm(dv ~ idv, data=plt_dat, family=binomial(link="logit"))
  plt_dat <-  cbind(plt_dat,as.data.frame(predict(glm_model, newdata = plt_dat, type="link", se=TRUE)))
  plt_dat <- plt_dat[!is.na(plt_dat$idv),]
  
  ones <- plt_dat[plt_dat$dv == 1,]
  ones$breaks <- cut(ones$idv,  breaks=30,include.lowest=TRUE)
  ones$breaks <- gsub(("\\["),x = gsub(("\\]"),x = gsub(("\\("),x = as.character(ones$breaks), replacement = ""), replacement = ""), replacement = "")
  ones$break1 <- sapply(1:nrow(ones), function(x) as.numeric(strsplit(ones$breaks[x],",")[[1]][1]))
  ones$break2 <- sapply(1:nrow(ones), function(x) as.numeric(strsplit(ones$breaks[x],",")[[1]][2]))
  ones$breaks <- (ones$break1 + ones$break2)/2
  ones$break1 <- NULL
  ones$break2 <- NULL
  
  zeros <- plt_dat[plt_dat$dv == 0,]
  zeros$breaks <- cut(zeros$idv,  breaks=30,include.lowest=TRUE)
  zeros$breaks <- gsub(("\\["),x = gsub(("\\]"),x = gsub(("\\("),x = as.character(zeros$breaks), replacement = ""), replacement = ""), replacement = "")
  zeros$break1 <- sapply(1:nrow(zeros), function(x) as.numeric(strsplit(zeros$breaks[x],",")[[1]][1]))
  zeros$break2 <- sapply(1:nrow(zeros), function(x) as.numeric(strsplit(zeros$breaks[x],",")[[1]][2]))
  zeros$breaks <- (zeros$break1 + zeros$break2)/2
  zeros$break1 <- NULL
  zeros$break2 <- NULL
  
  alls <- rbind(ones, zeros)
  
  
  h <- alls %>%  group_by(dv, breaks) %>% 
    summarise(n = n()) %>%
    mutate(pct = ifelse(dv==0, n/sum(n), 1 - n/sum(n)))
  
  # Calculate confidence intervals
  std <- qnorm(0.95 / 2 + 0.5)
  plt_dat$ymin <- glm_model$family$linkinv(plt_dat$fit - std * plt_dat$se.fit)
  plt_dat$ymax <- glm_model$family$linkinv(plt_dat$fit + std * plt_dat$se.fit)
  plt_dat$fit <- glm_model$family$linkinv(plt_dat$fit)  # Rescale to 0-1
  
  # Plot everything
  logit_plot <- ggplot(plt_dat, aes(x=idv, y=dv)) +
    geom_segment(data=h, size=4, show.legend=FALSE,
                 aes(x=breaks, xend=breaks, y=dv, yend=pct, colour=factor(dv))) +
    #geom_point(color = "green", alpha = 0.1) + 
    geom_ribbon(data=plt_dat, fill = "blue", aes(y=fit, ymin=ymin, ymax=ymax), alpha=0.5) + 
    #geom_dotplot(
    #  aes(fill = factor(dv)), method = "histodot", binpositions = "all", 
    #  stackgroups = TRUE, stackdir = "centerwhole", binwidth = 1, alpha = 0.25,stackratio = 1,position="identity"
    #) + 
    scale_fill_discrete(name = target) + 
    scale_y_continuous(labels = scales::percent) +
    geom_line(data=plt_dat, aes(y=fit), color = "red") + 
    labs(x=varName, y=paste0(target)) +
    ggtitle(paste0(varName,' -  Logit plot'))
  
  plot(logit_plot)
}