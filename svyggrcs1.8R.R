svyggrcs<-function(fit,boot=T,log=F,n.boot=200,ref=NULL,plot=T,xlab=NULL,ylab=NULL,
                   ggtitle=NULL,yhatlimit=NULL,dec=2,P.Nonlinear=TRUE,px=NULL,py=NULL) {
  options(warn=-1)
  require(stringr)
  require(ggplot2)
  require(rms)
  require(cowplot)
  require(survey)
  fit<-fit
  out<-predatarcs(fit)
  fit<-out[["fit"]];data<-out[["data"]];x=out[["x"]]
  x1<-as.name(x)
  if (!is.null(ref)) {
    if (ref=="median") {
      ref<-median(data[,x])
    } else {ref<-ref}
  } 
  ddist <- rms::datadist(data)
  options(datadist = ddist)
  if (!is.null(ref)) {
    ddist$limits["Adjust to",x] <- ref  
    fit <- update(fit) 
  }
  an<-anova(fit)
  an<-as.data.frame(an)
  if (any(class(fit)=="cph"|class(fit)=="lrm")==T) {
    p.overall  <- an[x, 3]
    p.value <- an[" Nonlinear", 3]
  } else {
    p.overall<- an[x, 5]
    p.value <- an[" Nonlinear", 5]
  }
  p.value<-pvformat(p.value,dec)
  p.overall<-pvformat(p.overall,dec)
  p.value1<-p.value <- paste0("P for nonlinear", " = ",
                              p.value)
  p.overall1<-p.overall <- paste0("P for overall", " = ",
                                  p.overall)
  text<- paste(p.overall1, p.value1, sep = "\n")
  cat("P for nonlinear", " = ", p.value, "\n", "P for overall", " = ",p.overall);
  tmpdt<-eval(parse(text = sprintf("Predict(fit,%s,fun=exp,ref.zero=T)", 
                                   x)))
  if (deparse(fit[["call"]][[1]])=="ols") {
    tmpdt<-eval(parse(text = sprintf("Predict(fit,%s,ref.zero=T)", 
                                     x)))
  }
  yy<-NULL
  if (boot==T) {
    for(i in 1:n.boot) {
      n = round(runif(1,nrow(data)/5,nrow(data)))
      dat <-data[sample(nrow(data),size=n,replace = F),]
      fit1 <- suppressWarnings(update(fit, data = dat))
      if (deparse(fit[["call"]][[1]])=="lrm" | deparse(fit[["call"]][[1]])=="cph") {
        tmpdt1<-eval(parse(text = sprintf("Predict(fit1,%s,fun=exp,ref.zero=T)", 
                                          x)))
      }
      if (deparse(fit[["call"]][[1]])=="ols") {
        tmpdt1<-eval(parse(text = sprintf("Predict(fit1,%s,ref.zero=T)", 
                                          x)))
      }
      yy<-cbind(yy,tmpdt1$yhat)
      yy
    }
    newdat<-tmpdt
    newdat$bootll<-apply(yy, 1, quantile,probs=.025,na.rm=T)
    newdat$bootul<-apply(yy, 1, quantile,probs=.975,na.rm=T)
  }
  if (is.null(xlab)) {xlab<-x} else {xlab<-xlab}
  if (is.null(ylab)) {ylab<-"predictive value"} else {ylab<-ylab}
  if (is.null(ggtitle)) {ggtitle<-"Observe the relationship between variables and outcomes"
  } else {
    ggtitle<-ggtitle}
  newdat<-as.data.frame(newdat)
  if(!is.null(yhatlimit)) {
    yhatlimit<-yhatlimit
    newdat$yhat<-ifelse(newdat$yhat>yhatlimit,NA,newdat$yhat)
  }
  vx<-newdat[,x]
  p1<-ggplot(newdat,aes(vx,yhat))+
    geom_ribbon(aes(ymin=bootll,ymax=bootul),fill="red",alpha=.2)+
    geom_line(linewidth=1,col="red",alpha=.2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    xlab(xlab)+
    ylab(ylab)+
    ggtitle(ggtitle)
  p2<-ggplot(newdat,aes(vx,yhat))+
    geom_ribbon(aes(ymin=lower,ymax=upper),fill="red",alpha=.2)+
    geom_line(linewidth=1,col="red",alpha=.2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    xlab(xlab)+
    ylab(ylab)+
    ggtitle(ggtitle)
  if (P.Nonlinear==TRUE) {
    if (is.null(py)) {py<-max(newdat$upper)*0.9} else {py<-py}
    if (is.null(px)) {px<-(max(newdat[,x])-min(newdat[,x]))*0.02+min(newdat[,x])} else {px<-px}
    p1<-p1+draw_label(text, x = px, y = py, hjust = 0,
                      vjust = 1)
    p2<-p2+draw_label(text, x = px, y = py, hjust = 0,
                      vjust = 1)
  }
  out<-list(newdat=newdat,boot.p=p1,p2=p2)
}



predatarcs<-function(fit,group=NULL) {
  fit<-fit;group<-group
  call <- fit[["call"]]
  data <- fit$survey.design$variables
  allvar<-all.vars(fit$terms)
  yv<-allvar[1];xv<-allvar[-1]
  if (deparse(fit[["call"]][[1]])=="svycoxph") {
    call[[1]] <- as.name("cph")
  } 
  else if (fit[["family"]][["family"]]=="quasibinomial") {call[[1]] <- as.name("lrm")} 
  else if (fit[["family"]][["family"]]=="gaussian") {call[[1]] <- as.name("ols")}
  call$family <- NULL
  call$design <- NULL
  pat<-pan.rcs(fit)
  xval<-pat[["x"]];k<-pat[["k"]]
  cov<-setdiff(xv, xval)
  allvalname<-c(xval,yv,cov)
  inspect.data(data,allvalname)
  call$weights <- as.name("weights")
  datnames<-fit[["survey.design"]][["call"]][["data"]]
  call$data <- as.name("data")
  if (any(deparse(cov)=="charact,er(0)")) {originalmd<-TRUE} else {covname<-paste0(cov, collapse=" + ")}
  data$weights <- (1/fit$survey.design$prob)/mean(1/fit$survey.design$prob)
  formula<-call$formula
  options(datadist = suppressWarnings(rms::datadist(data)))
  fit = suppressWarnings(eval(call))
  out<-list(fit=fit,data=data,k=k,x=xval,cov=cov)
  out
}



pan.rcs<-function(fit) {
  call<-as.character(fit[["call"]][["formula"]])[3]
  text<-str_extract(call,"(?<=\\().+?(?=\\))")
  #regmatches(call, gregexpr( "(?<=\\().+?(?=\\))", call, perl = T))[[1]]
  x<-str_extract(text,".+?(?=\\,)")
  k<-as.numeric(str_extract(text,"(?<=\\,).+"))
  out<-list(x=x,k=k)
  out
}


inspect.data<-function(data,var) {
  n<-length(var)
  for (i in 1:n) {
    var1<-var[i]
    x<-data[,var1]
    if (length(levels(factor(x))) < 2L) {
      text<-sprintf("The variable %s only has one level, and the model cannot fit it.",var1)
      stop(text)
    }
  }
}

pvformat<-function(p,dec) {
  pp <- sprintf(paste("%.",dec,"f",sep=""),as.numeric(p))
  if (is.matrix(p)) {pp<-matrix(pp, nrow=nrow(p)); colnames(pp)<-colnames(p);rownames(pp)<-rownames(p);}
  lw <- paste("<",substr("0.00000000000",1,dec+1),"1",sep="");
  pp[as.numeric(p)<(1/10^dec)]<-lw
  return(pp)
}
