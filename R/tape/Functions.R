dff<-function(x) x-dat$stroke_rate
rng<-function(x) max(x)-min(x)
mn<-function(x) tapply(x,df$name,mean)
time_rate<-function(x) x/(df$swim.secs)

diffs<-function(x) x-predictions$stroke_rate
ranges<-function(x) max(x)-min(x)
means<-function(x) tapply(x,dat$name,mean)
time_rates<-function(x) x/(fd$time_dive)

find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}


panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}

assumptions<-function(model){
  modresid<-resid(model)
  modpredict<-predict(model)
  modstres<-(modresid-mean(modresid)/sd(modresid))
  modstpre<-(modpredict-mean(modpredict)/sd(modpredict))
  par(mfrow=c(2,2))
  qqnorm(modstres)
  qqline(resid(model))
  acf(resid(model))
  plot(modstpre, modstres, main = "Standardized Residuals Plot", 
       xlab = "Standardized Predicted Values", ylab = "Standardized Residuals")
  abline(0,0)
  hist(modstres, freq = FALSE,
       main="Histogram of Standardised Residuals")
  curve(dnorm(x, mean = mean(modstres), sd = sd(modstres)),
        add=TRUE, col = "red")
}

rsquared<-function(model,variable){
  1-(deviance(model)/sum((variable-mean(variable,na.rm=TRUE))^2,na.rm=TRUE))}


lm.results <-
  function(lm)
  {
    out <- c(lm$coefficients[1],
             lm$coefficients[2],
             summary(lm)$coefficients[2,2],
             pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
                summary(lm)$fstatistic[3], lower.tail = FALSE),
             summary(lm)$r.squared,
             logLik(lm))
    names(out) <- c("intercept","slope","slope.SE","p.value","r.squared","logLik")
    return(out)}


##Test differences between two slopes

slopeDiff<-function(model1,model2){
  b1<-model1$coefficients[2]
  b2<-model2$coefficients[2]
  seb1<-summary(model1)$coefficients[2,2]
  seb2<-summary(model2)$coefficients[2,2]
  z<-(b1-b2)/sqrt(seb1^2+seb2^2)
  p<-(2*pnorm(-abs(z)))
  out<-c(z,p)
  names(out)<-c("z","p")
  return(out)
}

# Define a function to compute R-squared in linear space 

rsq <- function(data, fit) {
  + ssres <- sum((data - fit)^2)
  + sstot <- sum((data - mean(data))^2)
  + rsq <- 1 - (ssres / sstot)
  + return(rsq)
}


my.summary <- function(x,...){
  c(mean=mean(x, ...),
    sd=sd(x, ...),
    min=min(x, ...),
    max=max(x,...), 
    n=length(x))
}


tmp <- function(df,x) {
  do.call(data.frame, 
               list(mean = tapply(df, x, mean),
                    sd = tapply(df, x, sd),
                    min = tapply(df, x, min),
                    max = tapply(df, x, max),
                    n = tapply(df, x, length)))
}
  

rsquared<-function(model,variable){
  1-(deviance(model)/sum((variable-mean(variable,na.rm=TRUE))^2,na.rm=TRUE))}

two.t.tests<-function(var1,var2){
  btest<-bartlett.test(var1,var2)
  ttest<-t.test(var1~var2)
  wtest<-wilcox.test(var1~var2)
  variance<-c(btest$statistic,btest$p.value)
  t.test<-c(ttest$statistic,ttest$p.value)
  wilcox<-c(wtest$statistic,wtest$p.value)
  out<-rbind(variance,t.test,wilcox)
  return(out)
}

statmod <- function(x) {
  z <- table(as.vector(x))
  names(z)[z == max(z)]
}

#Changing from volts to fractions ### Depends on how it is set on FMS!!!!
##Currently is set to values used in pre-lim trial
transform.o2<-function(x){
  (x*0.5+19)/100
}


transform.co2<-function(x){
  (x*0.2)/100
}

transform.fr<-function(x){
  x*200
}

transform.bp<-function(x){
  x*24
}

transform.wv<-function(x){
  x*4
}

create.wv<-function(x){
  (x*0.4)/10
}



##Flow rate corrected for BP and WVP####

correct.fr<-function(fr,bp,wvp){
  (fr*(bp-wvp)/bp)
}

calc.vo2<-function(data){
  (data$fr_c*(.209476-data$o2_v))/(1-0.209476+0.8*(.209476-data$o2_v))
}

recalc.vo2<-function(data){
  (data$fr_c*(.209476-data$o2_v))/(1-data$o2_v)
}

mean.o2<-function(x){
  mean(x$o2_v_1,na.rm=TRUE)
}

mean.bp<-function(x){
  mean(x$bp,na.rm=TRUE)
}

mean.wvp<-function(x){
  mean(x$wv,na.rm=TRUE)
}

mean.frc<-function(x){
  mean(x$fr_c,na.rm=TRUE)
}

mean.fr<-function(x){
  mean(x$fr,na.rm=TRUE)
}

mean.temp<-function(x){
  mean(x$temp,na.rm=TRUE)
}

mean.vo2<-function(x){
  mean(x$vo2,na.rm=TRUE)
}

sd.vo2<-function(x){
  sd(x$vo2,na.rm=TRUE)
}

min.vo2<-function(x){
  min(x$vo2,na.rm=TRUE)
}


max.vo2<-function(x){
  max(x$vo2,na.rm=TRUE)
}

nrow.vo2<-function(x){
  nrow(x$vo2)
}

mean.active<-function(x){
  mean(x$active,na.rm=TRUE)
}

sd.active<-function(x){
  sd(x$active,na.rm=TRUE)
}

min.active<-function(x){
  min(x$active,na.rm=TRUE)
}


max.active<-function(x){
  max(x$active,na.rm=TRUE)
}

nrow.active<-function(x){
  nrow(x$active)
}
###Stuff to figure out########

minute.summary <- function(x) tapply(x$vo2, as.numeric(paste(x$hour, x$minute, 
                                                  sep=".")), mean)

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This is does the summary; it's not easy to understand...
  browser()
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

se<-function(x){
  sd(x)/sqrt(length(x))
}

error<-function(n){
  qnorm(0.975)*sd/sqrt(n)}


data.by.hrmin<-function(data){
  means<-tapply(x$vo2, as.numeric(paste(x$hour, x$minute, 
                                               sep=".")), mean)
  stdev<-tapply(x$vo2, as.numeric(paste(x$hour, x$minute, 
                                        sep=".")), sd)
  count<-tapply(x$vo2, as.numeric(paste(x$hour, x$minute, 
                                        sep=".")), length)
  cbind(count,means,stdev)
}

data.by.minute<-function(data,var){
 x<- aggregate(var~minute+Marker,data,mean)
 y<- aggregate(var~minute+Marker,data,sd)
 z<- aggregate(var~minute+Marker,data,length)
 ID<-x$Marker
 AVERAGE<-x$vo2
 SD<-y$vo2
 N<-z$vo2
 data.frame(cbind(ID,AVERAGE,SD,N))
}


lm.results <-
  function(lm)
  {
    out <- c(lm$coefficients[1],
             lm$coefficients[2],
             summary(lm)$coefficients[2,2],
             pf(summary(lm)$fstatistic[1], summary(lm)$fstatistic[2],
                summary(lm)$fstatistic[3], lower.tail = FALSE),
             summary(lm)$r.squared,
             logLik(lm))
    names(out) <- c("intercept","slope","slope.SE","p.value","r.squared","logLik")
    return(out)}

##Ask thompson - how to add in name of model
#x <- eval(lm$call[[2]])[3]

lme.results <-
  function(lm)
  {  
    out <- c(summary(lm)$AIC,
             summary(lm)$logLik,
             r.squared.lme(lm)$Marginal,
             r.squared.lme(lm)$Conditional)
    names(out) <- c("AIC","LogLik","r.squared(fixed)","r.squared(all)")
    return(out)}


##Test differences between two slopes

slopeDiff<-function(model1,model2){
  b1<-model1$coefficients[2]
  b2<-model2$coefficients[2]
  seb1<-summary(model1)$coefficients[2,2]
  seb2<-summary(model2)$coefficients[2,2]
  z<-(b1-b2)/sqrt(seb1^2+seb2^2)
  p<-(2*pnorm(-abs(z)))
  out<-c(z,p)
  names(out)<-c("z","p")
  return(out)
}

# Define a function to compute R-squared in linear space 

rsq <- function(data, fit) {
  + ssres <- sum((data - fit)^2)
  + sstot <- sum((data - mean(data))^2)
  + rsq <- 1 - (ssres / sstot)
  + return(rsq)
}


my.summary <- function(x,...){
  c(mean=mean(x, ...),
    sd=sd(x, ...),
    min=min(x, ...),
    max=max(x,...), 
    n=length(x))
}


tmp <- function(df,x) {
  do.call(data.frame, 
               list(mean = tapply(df, x, mean),
                    sd = tapply(df, x, sd),
                    min = tapply(df, x, min),
                    max = tapply(df, x, max),
                    n = tapply(df, x, length)))
}
  

rsquared<-function(model,variable){
  1-(deviance(model)/sum((variable-mean(variable,na.rm=TRUE))^2,na.rm=TRUE))}

two.t.tests<-function(var1,var2){
  btest<-bartlett.test(var1,var2)
  ttest<-t.test(var1~var2)
  wtest<-wilcox.test(var1~var2)
  variance<-c(btest$statistic,btest$p.value)
  t.test<-c(ttest$statistic,ttest$p.value)
  wilcox<-c(wtest$statistic,wtest$p.value)
  out<-rbind(variance,t.test,wilcox)
  return(out)
}

#' R-squared and pseudo-rsquared for a list of (generalized) linear (mixed) models
#'
#' This function calls the generic \code{\link{r.squared}} function for each of the
#' models in the list and rbinds the outputs into one data frame
#'
#' @param a list of fitted (generalized) linear (mixed) model objects
#' @return a dataframe with one row per model, and "Class",
#'         "Family", "Marginal", "Conditional" and "AIC" columns
rsquared.glmm <- function(modlist) {
  # Iterate over each model in the list
  do.call(rbind, lapply(modlist, r.squared))
}

#' R-squared and pseudo-rsquared for (generalized) linear (mixed) models
#'
#' This generic function calculates the r squared and pseudo r-squared for
#' a variety of(generalized) linear (mixed) model fits.
#' Currently implemented for \code{\link{lm}}, \code{\link{lmerTest::merMod}},
#' and \code{\link{nlme::lme}} objects.
#' Implementing methods usually call \code{\link{.rsquared.glmm}}
#'
#' @param mdl a fitted (generalized) linear (mixed) model object
#' @return Implementing methods usually return a dataframe with "Class",
#'         "Family", "Marginal", "Conditional", and "AIC" columns
r.squared <- function(mdl){
  UseMethod("r.squared")
}

#' Marginal r-squared for lm objects
#'
#' This method uses r.squared from \code{\link{summary}} as the marginal.
#' Contrary to other \code{\link{r.squared}} methods, 
#' this one doesn't call \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lm object (usually fit using \code{\link{lm}},
#' @return a dataframe with with "Class" = "lm", "Family" = "gaussian",
#'        "Marginal" = unadjusted r-squared, "Conditional" = NA, and "AIC" columns
r.squared.lm <- function(mdl){
  data.frame(Class=class(mdl), Family="gaussian", Link="identity",
             Marginal=summary(mdl)$r.squared,
             Conditional=NA, AIC=AIC(mdl))
}

#' Marginal and conditional r-squared for merMod objects
#'
#' This method extracts the variance for fixed and random effects, residuals,
#' and the fixed effects for the null model (in the case of Poisson family),
#' and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an merMod model (usually fit using \code{\link{lme4::lmer}},
#'        \code{\link{lme4::glmer}}, \code{\link{lmerTest::lmer}},
#'        \code{\link{blme::blmer}}, \code{\link{blme::bglmer}}, etc)
r.squared.merMod <- function(mdl){
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$X)))
  # Get variance of random effects by extracting variance components
  # Omit random effects at the observation level, variance is factored in later
  VarRand <- sum(
    sapply(
      VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
      function(Sigma) {
        X <- model.matrix(mdl)
        Z <- X[,rownames(Sigma)]
        sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } ) )
  # Get the dispersion variance
  VarDisp <- unlist(VarCorr(mdl)[sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))])
  if(is.null(VarDisp)) VarDisp = 0 else VarDisp = VarDisp
  if(inherits(mdl, "lmerMod")){
    # Get residual variance
    VarResid <- attr(lme4::VarCorr(mdl), "sc")^2
    # Get ML model AIC
    mdl.aic <- AIC(update(mdl, REML=F))
    # Model family for lmer is gaussian
    family <- "gaussian"
    # Model link for lmer is identity
    link <- "identity"
  }
  else if(inherits(mdl, "glmerMod")){
    # Get the model summary
    mdl.summ <- summary(mdl)
    # Get the model's family, link and AIC
    family <- mdl.summ$family
    link <- mdl.summ$link
    mdl.aic <- AIC(mdl)
    # Pseudo-r-squared for poisson also requires the fixed effects of the null model
    if(family=="poisson") {
      # Get random effects names to generate null model
      rand.formula <- reformulate(sapply(findbars(formula(mdl)),
                                         function(x) paste0("(", deparse(x), ")")),
                                  response=".")
      # Generate null model (intercept and random effects only, no fixed effects)
      null.mdl <- update(mdl, rand.formula)
      # Get the fixed effects of the null model
      null.fixef <- as.numeric(lme4::fixef(null.mdl))
    }
  }
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, VarDisp, family = family, link = link,
                 mdl.aic = mdl.aic,
                 mdl.class = class(mdl),
                 null.fixef = null.fixef)
}

#' Marginal and conditional r-squared for lme objects
#'
#' This method extracts the variance for fixed and random effects,
#' as well as residuals, and calls \code{\link{.rsquared.glmm}}
#'
#' @param mdl an lme model (usually fit using \code{\link{nlme::lme}})
r.squared.lme <- function(mdl){
  # Get design matrix of fixed effects from model
  Fmat <- model.matrix(eval(mdl$call$fixed)[-2], mdl$data)
  # Get variance of fixed effects by multiplying coefficients by design matrix
  VarF <- var(as.vector(nlme::fixef(mdl) %*% t(Fmat)))
  # Get variance of random effects by extracting variance components
  VarRand <- sum(suppressWarnings(as.numeric(nlme::VarCorr(mdl)
                                             [rownames(nlme::VarCorr(mdl)) != "Residual",
                                              1])), na.rm=T)
  # Get residual variance
  VarResid <- as.numeric(nlme::VarCorr(mdl)[rownames(nlme::VarCorr(mdl))=="Residual", 1])
  # Call the internal function to do the pseudo r-squared calculations
  .rsquared.glmm(VarF, VarRand, VarResid, family = "gaussian", link = "identity",
                 mdl.aic = AIC(update(mdl, method="ML")),
                 mdl.class = class(mdl))
}

#' Marginal and conditional r-squared for glmm given fixed and random variances
#'
#' This function is based on Nakagawa and Schielzeth (2013). It returns the marginal
#' and conditional r-squared, as well as the AIC for each glmm.
#' Users should call the higher-level generic "r.squared", or implement a method for the
#' corresponding class to get varF, varRand and the family from the specific object
#'
#' @param varF Variance of fixed effects
#' @param varRand Variance of random effects
#' @param varResid Residual variance. Only necessary for "gaussian" family
#' @param family family of the glmm (currently works with gaussian, binomial and poisson)
#' @param link model link function. Working links are: gaussian: "identity" (default);
#'        binomial: "logit" (default), "probit"; poisson: "log" (default), "sqrt"
#' @param mdl.aic The model's AIC
#' @param mdl.class The name of the model's class
#' @param null.fixef Numeric vector containing the fixed effects of the null model.
#'        Only necessary for "poisson" family
#' @return A data frame with "Class", "Family", "Marginal", "Conditional", and "AIC" columns
.rsquared.glmm <- function(varF, varRand, varResid = NULL, varDisp = NULL, family, link,
                           mdl.aic, mdl.class, null.fixef = NULL){
  if(family == "gaussian"){
    # Only works with identity link
    if(link != "identity")
      family_link.stop(family, link)
    # Calculate marginal R-squared (fixed effects/total variance)
    Rm <- varF/(varF+varRand+varResid)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varResid)
  }
  else if(family == "binomial"){
    # Get the distribution-specific variance
    if(link == "logit")
      varDist <- (pi^2)/3
    else if(link == "probit")
      varDist <- 1
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else if(family == "poisson"){
    # Get the distribution-specific variance
    if(link == "log")
      varDist <- log(1+1/exp(null.fixef))
    else if(link == "sqrt")
      varDist <- 0.25
    else
      family_link.stop(family, link)
    # Calculate marginal R-squared
    Rm <- varF/(varF+varRand+varDist+varDisp)
    # Calculate conditional R-squared (fixed effects+random effects/total variance)
    Rc <- (varF+varRand)/(varF+varRand+varDist+varDisp)
  }
  else
    family_link.stop(family, link)
  # Bind R^2s into a matrix and return with AIC values
  data.frame(Class=mdl.class, Family = family, Link = link,
             Marginal=Rm, Conditional=Rc, AIC=mdl.aic)
}

#' stop execution if unable to calculate variance for a given family and link
family_link.stop <- function(family, link){
  stop(paste("Don't know how to calculate variance for",
             family, "family and", link, "link."))
}


z.test=function(a, mu, var){
  zeta = (mean(a) - mu)/
    (sqrt(var/length(a)))
  return(zeta)
}

mode<-function(x){
  temp<-table(as.vector(x))
  names(temp[temp==max(temp)])
}
