# clear workspace
rm(list = ls())

pc.wd = "Q:\\Research\\Data\\Data Files\\03 SCFPTX"
github.dir = "Q:\\Research\\Projects\\03 Payment terms extensions under RF (SCFPTX)\\03 R\\"
artwork.dir = paste(github.dir,"03 Figures\\",sep="")
models.dir = paste(github.dir,"02 Models\\",sep="")

inArtDir <- function(file.name) return(paste(artwork.dir,file.name,sep=""))
inModelsDir <- function(file.name) return(paste(models.dir,file.name,sep=""))

if(dir.exists(file.path(pc.wd, "."))) setwd(pc.wd)





#load packages

library(plm)
library(lmtest)
library(dplyr)
library(tidyr)
library(tibble)


# https://vincentarelbundock.github.io/modelsummary/articles/modelsummary.html#customizing-existing-models-part-ii
library(modelsummary)
library(broom)


# load data 

# load data 
load("04 RData\\scf.RData")
load("04 RData\\buyers.RData")


load(file = "plm.controls.fm.RData" %>% inModelsDir() )
load(file = "plm.basic.fm.RData" %>% inModelsDir())
load(file = "plm.main.fm.RData" %>% inModelsDir())
load(file = "plm.full.fm.RData" %>% inModelsDir())


load(file = "harmonization.probit.controls.fm.RData" %>% inModelsDir())
load(file = "harmonization.probit.full.fm.RData" %>% inModelsDir())


load(file = "lm.IV.country.step.1.lm.fm.RData" %>% inModelsDir())
load(file = "lm.IV.country.step.2.plm.fm.RData" %>% inModelsDir())
load(file = "lm.IV.ind.step.1.lm.fm.RData" %>% inModelsDir())
load(file = "lm.IV.ind.step.2.plm.fm.RData" %>% inModelsDir())


load(file = "lsdv.effects.select.step1.fm.RData" %>% inModelsDir())
load(file = "lsdv.effects.select.step2.fm.RData" %>% inModelsDir())


load(file = "plm.main.re.fm.RData" %>% inModelsDir())
load(file = "plm.full.re.fm.RData" %>% inModelsDir())
load(file = "plm.old.main.fm.RData" %>% inModelsDir())
load(file = "plm.old.full.fm.RData" %>% inModelsDir())


load(file = "plm.did.controls.fm.RData" %>% inModelsDir())
load(file = "plm.did.main.fm.RData" %>% inModelsDir())
load(file = "plm.did.full.fm.RData" %>% inModelsDir())



############################
#
#
# Adapt MSUMMARY for my latex style
#
#############################

msummary.tex <- function(m_list){
  MSUMMARY <- msummary(m_list, stars=TRUE,output="data.frame")
  
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\+", "$^{+}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*\\*", "$^{xxx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*\\*", "$^{xx}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\*", "$^{x}$", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xxx\\}", "{***}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{xx\\}", "{**}", x)}))
  MSUMMARY <- data.frame(lapply(MSUMMARY, function(x) {gsub("\\{x\\}", "{*}", x)}))
  
  N.models <- dim(MSUMMARY)[2]-3
  N.all.rows <- dim(MSUMMARY)[1]
  N.rows <- sum(MSUMMARY$part=="estimates")
  out = ""
  
  for(i in 1:N.models)
    #out = paste(out,"&& \\muc{1.5cm}{(",i,")}", sep="")
    out = paste(out,"&& \\suc{(",i,")}", sep="")
  
  out = paste(out,"\\\\",sep="")
  
  for(i in 1:N.models)
    out = paste(out,"\\cline{",2*i+1,"-",2*i+1,"}", sep="")
  
  out = paste(out,"\n",sep="")
  
  for(i in 1:N.rows){
    if(MSUMMARY[i,"statistic"]=="estimate"){
      #if(MSUMMARY[i,"statistic"]=="modelsummary_tmp1"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        out = paste(out,"&&", MSUMMARY[i,j], sep="")
      }
      out = paste(out,"\\\\\n", sep="")
    }
    if(MSUMMARY[i,"statistic"]=="std.error"){
      # if(MSUMMARY[i,"statistic"]=="modelsummary_tmp2"){
      
      for(j in 4:(N.models+3)){
        out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
      }
      out = paste(out,"\\\\", sep="")
    }
  }
  out = paste(out,"\\midrule\n", sep="")
  for(i in 1:N.all.rows){
    if(MSUMMARY[i,"part"]=="gof"){
      out = paste(out,MSUMMARY[i,"term"], sep="")
      for(j in 4:(N.models+3)){
        if(MSUMMARY[i,"term"]=="Num.Obs."){
          out = paste(out,"&&\\suc{", MSUMMARY[i,j],"}", sep="")
        }
        else{
          out = paste(out,"&&", MSUMMARY[i,j], sep="")
        }
      }
      out = paste(out,"\\\\\n", sep="")
    }
  }
  cat(out)
}



############################
#
#
# Convert models to list for MSUMMARY
#
#############################


plm.to.list <- function(PLM.FM){
  
  N = nobs(PLM.FM) %>% as.numeric
  rSquared = summary(PLM.FM)$r.squared[1] %>% as.numeric
  
  robust.df <- (PLM.FM %>% coeftest(.,vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))[] %>% 
    as.data.frame() %>%
    rownames_to_column(var="coefficient") %>%
    mutate(drop = grepl("as.factor", coefficient) | grepl("S_", coefficient) | grepl("Intercept", coefficient) ) %>%
    subset(!drop) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "as.factor\\(time\\)", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_IND__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_COUNTRY__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "ln.", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, ".std", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]count", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]", " "))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "libor", "risk free rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "priorTerms", "initial payment terms"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "mode", "mode (1=manual)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "posteriorTermsAreModeTRUE", "standard term (1=yes)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "revenue", "supplier revenue"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "discount rate", "RF interest rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "mean initial payment terms.othersry", "average payment terms (supplier country)")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "mean initial payment terms.others.ind", "average payment terms (supplier industry)")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "terms.others.ind ", "average payment terms (supplier industry)")) %>%
    dplyr::select(!contains("t value")) %>%
    plyr::rename(c("Estimate"="estimate","Std. Error"="std.error","Pr(>|t|)"="p.value","coefficient"="term"))
  
  fit.df <- data.frame(N, rSquared) 
  out <- list(estimates = robust.df, fit = fit.df)
  class(out) <- c("custom_plm",class(out))
  return(out)
}


glm.to.list <- function(GLM.FM){
  
  N = nobs(GLM.FM) %>% as.numeric
  AIC = summary(GLM.FM)$aic %>% as.numeric
  
  robust.df <- (GLM.FM  %>% coeftest(.,vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))[]%>% 
    as.data.frame() %>%
    rownames_to_column(var="coefficient") %>%
    mutate(drop = grepl("B_", coefficient) | grepl("as.factor", coefficient) | grepl("Intercept", coefficient) )%>%
    subset(!drop) %>%
    dplyr::select(!contains("z value")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "buyer[.]cogs", "buyer costs of goods sold"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "buyer[.]revenue", "buyer revenue"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "n[.]suppliers", "RF program size"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "as.factor\\(time\\)", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_IND__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_COUNTRY__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "ln.", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, ".std", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]count", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]", " "))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "libor", "risk free rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "discount rate", "RF interest rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "priorTerms", "initial payment terms"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "modemanual discount", "mode (1=manual)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "posteriorTermsAreModeTRUE", "standard term (1=yes)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "revenue", "supplier revenue"))%>%
    plyr::rename(c("Estimate"="estimate","Std. Error"="std.error","Pr(>|z|)"="p.value","coefficient"="term"))
  
  fit.df <- data.frame(N, AIC) 
  out <- list(estimates = robust.df, fit = fit.df)
  class(out) <- c("custom_glm",class(out))
  return(out)
}


select.to.list <- function(SELECT.FM){
  
  N = nobs(SELECT.FM) %>% as.numeric
  rSquared = summary(SELECT.FM)$rSquared[1] %>% as.numeric()
  
  robust.df <- summary(SELECT.FM)$estimate %>% 
    as.data.frame() %>%
    rownames_to_column(var="coefficient") %>%
    mutate(drop = grepl("S_", coefficient) |
             grepl("as.factor", coefficient) | 
             grepl("Intercept", coefficient) | 
             grepl("sigma", coefficient) | 
             grepl("rho", coefficient) | 
             grepl("invMillsRatio", coefficient) |
             grepl("priorTerms[.]std", coefficient) & !grepl("priorTerms[.]std[.]1", coefficient)|
             grepl("ln[.]revenue[.]std", coefficient) & !grepl("ln.revenue.std.1", coefficient)|
             grepl("ln[.]discount[.]rate[.]std", coefficient) & !grepl("ln[.]discount[.]rate.std[.]1", coefficient)|
             grepl("ln[.]annual[.]spend[.]std", coefficient) & !grepl("ln[.]annual[.]spend[.]std[.]1", coefficient)) %>%
    subset(!drop) %>%
    dplyr::select(!contains("t value")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "ln[.]", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]1", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]std", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]count", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]", " "))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "libor", "risk free rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "revenue", "supplier revenue"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "priorTerms", "initial payment terms"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "discount rate", "RF interest rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "modemanual discount", "mode (1=manual)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "posteriorTermsAreModeTRUE", "standard term (1=yes)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, ":", " x ")) %>%
    plyr::rename(c("Estimate"="estimate","Std. Error"="std.error","Pr(>|t|)"="p.value","coefficient"="term"))
  
  fit.df <- data.frame(N, rSquared) 
  out <- list(estimates = robust.df, fit = fit.df)
  class(out) <- c("custom_plm",class(out))
  return(out)
}


re.to.list <- function(PLM.FM){
  
  N = nobs(PLM.FM) %>% as.numeric
  rSquared = summary(PLM.FM)$r.squared[1] %>% as.numeric
  
  robust.df <- (PLM.FM %>% coeftest(.,vcov.=function(x) vcovHC(x, cluster="group", type="HC0")))[] %>% 
    as.data.frame() %>%
    rownames_to_column(var="coefficient") %>%
    mutate(drop = grepl("as.factor", coefficient) | grepl("S_", coefficient) | grepl("Intercept", coefficient) | grepl("B_", coefficient) ) %>%
    subset(!drop) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "as.factor\\(time\\)", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_IND__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "S_COUNTRY__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "B_IND__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "B_COUNTRY__", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "ln.", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, ".std", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]count", ""))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "[.]", " "))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "libor", "risk free rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "priorTerms", "initial payment terms"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "mode", "mode (1=manual)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "posteriorTermsAreModeTRUE", "standard term (1=yes)"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "revenue", "supplier revenue"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "discount rate", "RF interest rate"))%>%
    mutate(coefficient = stringr::str_replace(coefficient, "mean initial payment terms.othersry", "average payment terms (supplier country)")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "mean initial payment terms.others.ind", "average payment terms (supplier industry)")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "terms.others.ind ", "average payment terms (supplier industry)")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "adoptedTRUE", "adopted")) %>%
    mutate(coefficient = stringr::str_replace(coefficient, "treatmentTRUE", "treatment")) %>%
    dplyr::select(!contains("t value")) %>%
    plyr::rename(c("Estimate"="estimate","Std. Error"="std.error","Pr(>|t|)"="p.value","coefficient"="term"))
  
  fit.df <- data.frame(N, rSquared) 
  out <- list(estimates = robust.df, fit = fit.df)
  class(out) <- c("custom_re",class(out))
  return(out)
}


############################
#
#
# Tidy and glance fuctions for customized lists
#
#############################



tidy.custom_plm <- function(in.list, ...){
  x <- in.list$estimates
  data.frame(
    term =  x$term,
    estimate = x$estimate,
    std.error = x$std.error,
    p.value = x$p.value
  )
}
glance.custom_plm <-function(in.list,...){
  x <- in.list$fit
  data.frame(
    "Num. obs."=as.character(x$N),
    "Rsq"=round(x$rSquared,2),
    "model"="\\suc{fixed effects}"
  )
}

tidy.custom_glm <- function(in.list, ...){
  x <- in.list$estimates
  data.frame(
    term =  x$term,
    estimate = x$estimate,
    std.error = x$std.error,
    p.value = x$p.value
  )
}
glance.custom_glm <-function(in.list,...){
  x <- in.list$fit
  data.frame(
    "Num. obs."=as.character(x$N),
    "AIC"=paste("\\suc{",round(x$AIC,2),"}",sep=""),
    "model"="\\suc{probit}"
  )
}

tidy.custom_re <- function(in.list, ...){
  x <- in.list$estimates
  data.frame(
    term =  x$term,
    estimate = x$estimate,
    std.error = x$std.error,
    p.value = x$p.value
  )
}
glance.custom_re <-function(in.list,...){
  x <- in.list$fit
  data.frame(
    "Num. obs."=as.character(x$N),
    "Rsq"=round(x$rSquared,2),
    "model"="\\suc{random effects}"
  )
}
############################
#
#
# Export models to latex
#
#############################


main_list <- list(plm.controls = plm.to.list(plm.controls.fm),
                  plm.basic = plm.to.list(plm.basic.fm),
                  plm.main = plm.to.list(plm.main.fm),
                  plm.full = plm.to.list(plm.full.fm)
)

# msummary(main_list, stars=TRUE,output="data.frame")
msummary.tex(main_list)


probit_list <- list(probit.controls = glm.to.list(harmonization.probit.controls.fm),
                    probit.full = glm.to.list(harmonization.probit.full.fm)
)

msummary.tex(probit_list)


robustness_1_list <- list(IV.country.step.1 = plm.to.list(lm.IV.country.step.1.lm.fm),
                          IV.country.step.2 = plm.to.list(lm.IV.country.step.2.plm.fm),
                          IV.ind.step.1 = plm.to.list(lm.IV.ind.step.1.lm.fm),
                          IV.ind.step.2 = plm.to.list(lm.IV.ind.step.2.plm.fm),
                          select.step.1 = glm.to.list(lsdv.effects.select.step1.fm),
                          select.step.2 = select.to.list(lsdv.effects.select.step2.fm))
                          
msummary.tex(robustness_1_list)



robustness_2_list <- list(plm.main.re.fm = re.to.list(plm.main.re.fm),
                          plm.full.re.fm = re.to.list(plm.full.re.fm),
                          plm.old.main.fm = plm.to.list(plm.old.main.fm),
                          plm.old.full.fm = plm.to.list(plm.old.full.fm)
)

msummary.tex(robustness_2_list)



DiD_list <- list(plm.did.controls.fm = re.to.list(plm.did.controls.fm),
                 plm.did.main.fm = re.to.list(plm.did.main.fm),
                 plm.did.full.fm = re.to.list(plm.did.full.fm)
)

msummary.tex(DiD_list)




