read.xlsx.fn <- function(file, sheetName, rowIndex,colIndex){
  rowIndex <- rowIndex[1:length(rowIndex)-1]
  df <- read_excel(file, sheet=sheetName)[rowIndex,colIndex]
  names(df) <- gsub(" ", ".",names(df))
  names(df) <- gsub("\\(", ".",names(df))
  names(df) <- gsub("\\)", ".",names(df))
  for(c in 1:length(df))
    if(class(df[,c])=="character")
      df[,c] <- as.factor(df[,c])
  return(df)
}


star.symbol<-function(p){
	Stars <- vector(mode="character", length=length(p))
	Stars<-rep("",length(p))
	Stars <- ifelse(p <= 0.1, "+" , Stars)
	Stars <- ifelse(p <= 0.05, "*" , Stars)
	Stars <- ifelse(p <= 0.01, "**" , Stars)
	Stars <- ifelse(p <= 0.001, "***" , Stars)
	return(Stars)
	}

star.symbol.corr<-function(p){
  Stars <- vector(mode="character", length=length(p))
  Stars<-rep("",length(p))
  Stars <- ifelse(p <= 0.05, "*" , Stars)
  return(Stars)
}

star.symbol.tex<-function(p){
  Stars <- vector(mode="character", length=length(p))
  Stars<-rep("",length(p))
  Stars <- ifelse(p <= 0.1, "$^+$" , Stars)
  Stars <- ifelse(p <= 0.05, "$^*$" , Stars)
  Stars <- ifelse(p <= 0.01, "$^**$" , Stars)
  Stars <- ifelse(p <= 0.001, "$^***$" , Stars)
  return(Stars)
}


getEstimates.george<-function(MODEL){
  
  if(sum((class(MODEL) %in% c("MLE.ESTIMATE"))*1)>0){
    # t-tests use Satterthwaite's method
    # https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/
    
    
    df <- data.frame(MODEL[,"coef"],MODEL[,"b"], MODEL[,"p"], sqrt(MODEL[,"var"]))
    colnames(df) <- c("var","estimate","p","se")
    rownames(df) <- NULL
  }
  if(class(MODEL) %in% c("glmerMod","lme4")){
    # t-tests use Satterthwaite's method
    # https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/
    
    MODEL <- data.frame(summary(MODEL)$coefficients)
    df <- data.frame(rownames(MODEL),MODEL[,"Estimate"], MODEL[,"Pr...z.."], MODEL[,"Std..Error"])
    colnames(df) <- c("var","estimate","p","se")
    rownames(df) <- NULL
  }
  if(class(MODEL) %in% "coeftest"){
    # t-tests use Satterthwaite's method
    # https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/
    
    MODEL <- data.frame(MODEL[,])
    
    rownames(MODEL)
    
    
    df <- data.frame(rownames(MODEL),MODEL[,"Estimate"],MODEL[,"Pr...t.."],MODEL[,"Std..Error"])
    colnames(df) <- c("var","estimate","p","se")
    rownames(df) <- NULL
  }
  
  df.out <- data.frame(covariates=character(),cells=character())
  
  for(row in 1:length(df[,1])){
    top.cell <- as.character(paste( sprintf("%.3f", round(df[row,"estimate"],3)),star.symbol.tex(df[row,"p"]),sep=""))
    bottom.cell <- as.character(paste("(", sprintf("%.3f", round(df[row,"se"],3)),")",sep=""))
    top.df <- data.frame(df[row,"var"],top.cell)
    bottom.df <- data.frame(paste("__D__",df[row,"var"],sep=""),bottom.cell)
    colnames(top.df) <- colnames(df.out)
    colnames(bottom.df) <- colnames(df.out)
    df.out <- rbind(df.out,top.df)
    df.out <- rbind(df.out,bottom.df)
  }
  return(df.out)
}


getEstimates<-function(COXPH){
	n<-COXPH$n
	coeff<-summary(COXPH)$coef[,1]
	varNames<-gsub("__","-->",names(coeff),ignore.case=T)
	varNames<-gsub("_"," ",names(coeff),ignore.case=T)
	coeff<-unname(coeff)	
	names(coeff)<-varNames
	se<-unname(summary(COXPH)$coef[,2])
	z<-coeff/se
	Chisq<-unname(summary(COXPH)$coef[,4])
	p<-2*(pnorm(-abs(z)))
	sig<-star.symbol(p)
	estimates<-data.frame(varNames,coeff,se,z,p,sig)
	return(estimates)
}

getEstimatesFixed<-function(COXPH){
	n<-COXPH$n
	varNames<-names(summary(COXPH)$coef[,3])
	coeff<-summary(COXPH)$coef[,1]
	se<-unname(summary(COXPH)$coef[,3])
	z<-unname(summary(COXPH)$coef[,4])
	p<-unname(summary(COXPH)$coef[,5])
	sig<-star.symbol(p)
	estimates<-data.frame(varNames,coeff,se,z,p,sig)	
	return(estimates)
}

getEstimatesCoxAFT<-function(COXAFT){

	z<-summary(COXAFT)$table[,3]
	p<-summary(COXAFT)$table[,4]
	# transfrom from AFT to PH, cf. http://www.statisticsmentor.com/2012/12/25/r-relationship-between-accelerated-failure-model-and-the-proportional-hazard-model-for-weibull/
	ph_coeff<-summary(COXAFT)$table[,1]*(-1)*(1/summary(COXAFT)$scale)

	estimates<-data.frame(ph_coeff,z,p)
	estimates<-estimates[(!grepl(glob2rx('gamma:*'), rownames(estimates))) & (!grepl(glob2rx('(Intercept)'), rownames(estimates))),]
	return(estimates)
}

#getFit<-function(COXPH,COXNULL){
	#logLikelihood <- COXPH$loglik[2]
	#logLikelihood_NullModel <- COXNULL$loglik[2]
	#LR_chiSq<-2*(logLikelihood-logLikelihood_NullModel)
	#No_of_suppliers<-COXPH$n
	#No_of_completed_implementations<-COXPH$nevent
	#Formula<-Reduce(paste, deparse(COXPH$formula))
	#num_vars_in_model<-length(summary(COXPH)$coef[,1])-length(summary(COXNULL)$coef[,1])
	#Prob_chi2<-1-pchisq(LR_chiSq,num_vars_in_model)
	#fit<-data.frame(No_of_suppliers,No_of_completed_implementations,logLikelihood,logLikelihood_NullModel,LR_chiSq,Prob_chi2,num_vars_in_model,Formula)
	
#	return(fit)
#}

writeSummary<-function(FIXED,SHARED,STRATA,FIXEDNULL,SHAREDNULL,STRATANULL,NAME,DATA){

	# setup xlsx file
	
	
	fileName <- sprintf("Results\\COX\\%s.%s.xlsx", NAME,DATA)
	wb<-createWorkbook(type="xlsx")
	sheet_estimates <-createSheet(wb, sheetName = "Estimates")
	sheet_fit <-createSheet(wb, sheetName = "Model_Fit")
	CellStyle(wb, dataFormat=NULL, alignment=NULL, border=NULL, fill=NULL, font=NULL)
	TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
	TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) + 
							Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
							Border(color="black", position=c("TOP", "BOTTOM"),pen=c("BORDER_THIN", "BORDER_THICK")) 	
	TITLE_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=16, color="blue", isBold=TRUE, underline=1)
	SUB_TITLE_STYLE <- CellStyle(wb) +  Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
	
	setColumnWidth(sheet_estimates, colIndex=c(1:1), colWidth=30)			 
	
	setRowHeight(createRow(sheet_estimates,rowIndex=2),5)	
	setRowHeight(createRow(sheet_estimates,rowIndex=4),5)
	
	# add title
	rows <-createRow(sheet_estimates,rowIndex=1)
	sheetTitle <-createCell(rows, colIndex=1)
	setCellValue(sheetTitle[[1,1]], "Cox Proportional Hazard Rate Models")
	setCellStyle(sheetTitle[[1,1]], TITLE_STYLE)
	rows <-createRow(sheet_estimates,rowIndex=3)
	
	sheetSubTitle1 <-createCell(rows, colIndex=1)
	sheetSubTitle8 <-createCell(rows, colIndex=8)
	sheetSubTitle15 <-createCell(rows, colIndex=15)
	
	setCellValue(sheetSubTitle1[[1,1]], "Shared" )
	setCellValue(sheetSubTitle8[[1,1]], "Strata" )
	setCellValue(sheetSubTitle15[[1,1]], "Fixed" )
	
	rows_fit <-createRow(sheet_fit,rowIndex=2)
	nameOfModelCol <-createCell(rows_fit, colIndex=1)
	setCellValue(nameOfModelCol[[1,1]], "Shared" )
	
	rows_fit <-createRow(sheet_fit,rowIndex=3)
	nameOfModelCol <-createCell(rows_fit, colIndex=1)
	setCellValue(nameOfModelCol[[1,1]], "Strata" )
	
	rows_fit <-createRow(sheet_fit,rowIndex=4)
	nameOfModelCol <-createCell(rows_fit, colIndex=1)
	setCellValue(nameOfModelCol[[1,1]], "Fixed" )
	
	
	#obtain all estimates	
	estimates.fixed<-getEstimatesFixed(FIXED)		
	estimates.fixed<-estimates.fixed[!grepl(glob2rx('BID*'), rownames(estimates.fixed)),]	
	estimates.shared<-getEstimates(SHARED)
	estimates.shared<-estimates.shared[!grepl(glob2rx('frailty*'), rownames(estimates.shared)),]
	estimates.strata<-getEstimatesFixed(STRATA)
	# estimates.weibull.shared<-getEstimatesCoxAFT(cox.weibull)

	#shortFormula <-getFit(COXPH)$Formula
	#shortFormula<-gsub(" ","",shortFormula,ignore.case=T)
	#shortFormula<-gsub("~"," = ",shortFormula,ignore.case=T)
	#shortFormula<-gsub("__","",shortFormula,ignore.case=T)
	#shortFormula<-gsub("_"," ",shortFormula,ignore.case=T)
	
	addDataFrame(estimates.shared, sheet_estimates, startRow=5, startColumn=1, 
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)
				 
	addDataFrame(estimates.strata, sheet_estimates, startRow=5, startColumn=8, 
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)
				 
	addDataFrame(estimates.fixed, sheet_estimates, startRow=5, startColumn=15, 
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)

	addDataFrame(getFit(SHARED,SHAREDNULL), sheet_fit, startRow=1, startColumn=2, 
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)
	
	addDataFrame(getFit(STRATA,STRATANULL), sheet_fit, startRow=3, startColumn=2, col.names = FALSE,
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)
				 
	addDataFrame(getFit(FIXED,FIXEDNULL), sheet_fit, startRow=4, startColumn=2, col.names = FALSE,
				 colnamesStyle = TABLE_COLNAMES_STYLE,
				 rownamesStyle = TABLE_ROWNAMES_STYLE)			 
				 
	setColumnWidth(sheet_fit, colIndex=c(2:length(getFit(SHARED,SHAREDNULL))), colWidth=20)
	setColumnWidth(sheet_fit, colIndex=c(2:2), colWidth=0)	
	saveWorkbook(wb, fileName)			
}

writeSingle<-function(MODEL,NAME,DATA){
  
  # setup xlsx file
  
  fileName <- sprintf("..\\Results\\COX\\%s.%s.xlsx", NAME,DATA)
  wb<-createWorkbook(type="xlsx")
  sheet_estimates <-createSheet(wb, sheetName = "Estimates")

  
  n<-MODEL$n
  coeff<-summary(MODEL)$coefficients[,1]
  varNames<-gsub("__","-->",names(coeff),ignore.case=T)
  varNames<-gsub("_"," ",names(coeff),ignore.case=T)
  coeff<-unname(coeff)	
  names(coeff)<-varNames
  se<-unname(summary(COXPH)$coef[,2])
  z<-coeff/se
  Chisq<-unname(summary(COXPH)$coef[,4])
  p<-2*(pnorm(-abs(z)))
  sig<-star.symbol(p)
  estimates<-data.frame(coeff,se,z,p,sig)
  
  addDataFrame(MODEL, sheet_estimates, startRow=5, startColumn=1, 
               colnamesStyle = TABLE_COLNAMES_STYLE,
               rownamesStyle = TABLE_ROWNAMES_STYLE)
  
  saveWorkbook(wb, fileName)			
}


genRunWriteCox<-function(name,regressors,dat){		


#	regressorsFixed = paste(supplierControls,effects,sep="+")
	formulaAsString.fixed <-paste("Surv(time=duration,event=hasImplemented) ~  BID + " , regressors,sep="")
	formulaAsString.shared<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,"+frailty(BID,dist='gamma',sparse=FALSE,method='em')",sep="")
	formulaAsString.strata<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,"+strata(BID)",sep="")
	formulaAsString.weibull<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,sep="")

	fixedCox<-coxph( as.formula(formulaAsString.fixed), data=dat, method="breslow")
	strataCox<-coxph( as.formula(formulaAsString.strata), data=dat, method="breslow")		
	sharedCox<-coxph( as.formula(formulaAsString.shared), data=dat, method="breslow")	
	
	fixedCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~  BID, data=dat, method="breslow")
	strataCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~ 1 + strata(BID), data=dat, method="breslow")		
	sharedCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~ frailty(BID,dist='gamma',sparse=FALSE,method='em'), data=dat, method="breslow")	
	
	# cox.weibull<-survreg(as.formula(formulaAsString.shared),dist='weibull',data=scf)
	
	# model<-coxph( form , data=dat, method="breslow")
	writeSummary(fixedCox,sharedCox,strataCox,fixedCoxNull,sharedCoxNull,strataCoxNull,name,deparse(substitute(dat)))
	# warning(cat("completed writing",name,"\n",sep=" "))
}


genRunWriteCoxRegressors<-function(name,regressors,dat){		
  
  #	regressorsFixed = paste(supplierControls,effects,sep="+")
  formulaAsString.fixed <-paste("Surv(time=duration,event=hasImplemented) ~  BID + " , regressors,sep="")
  formulaAsString.shared<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,"+frailty(BID,dist='gamma',sparse=FALSE,method='em')",sep="")
  formulaAsString.strata<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,"+strata(BID)",sep="")
  formulaAsString.weibull<-paste("Surv(time=duration,event=hasImplemented) ~ " , regressors,sep="")
  
  fixedCox<-coxph( as.formula(formulaAsString.fixed), data=dat, method="breslow")
  strataCox<-coxph( as.formula(formulaAsString.strata), data=dat, method="breslow")		
  sharedCox<-coxph( as.formula(formulaAsString.shared), data=dat, method="breslow")	
  
  fixedCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~  BID, data=dat, method="breslow")
  strataCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~ 1 + strata(BID), data=dat, method="breslow")		
  sharedCoxNull<-coxph( Surv(time=duration,event=hasImplemented) ~ frailty(BID,dist='gamma',sparse=FALSE,method='em'), data=dat, method="breslow")	
  
  # cox.weibull<-survreg(as.formula(formulaAsString.shared),dist='weibull',data=scf)
  
  # model<-coxph( form , data=dat, method="breslow")
  writeSummary(fixedCox,sharedCox,strataCox,fixedCoxNull,sharedCoxNull,strataCoxNull,name,deparse(substitute(dat)))
  # warning(cat("completed writing",name,"\n",sep=" "))
}

genRunWriteSpecial<-function(name,model,dat){		
  writeSingle(model,name,deparse(substitute(dat)))
  # warning(cat("completed writing",name,"\n",sep=" "))
}

drop_all_levels<-function(dframe){
	dframe$BuyerCreditRating<-droplevels(dframe$BuyerCreditRating)
	dframe$S_SU_SEC__<-droplevels(dframe$S_SU_SEC__)
	dframe$S_SECTOR__<-droplevels(dframe$S_SECTOR__)
	dframe$B_SEC__<-droplevels(dframe$B_SEC__)
	dframe$B_SU_SEC__<-droplevels(dframe$B_SU_SEC__)
	dframe$B_COUNTRY__<-droplevels(dframe$B_COUNTRY__)
	dframe$S_COUNTRY__<-droplevels(dframe$S_COUNTRY__)
	return(dframe)
}

winsor.fn <- function(value,p){
  MAX = quantile(value,p)
  MIN = quantile(value,1-p)
  value<-ifelse(value>MAX,MAX,value)
  value<-ifelse(value<MIN,MIN,value)
  return(value)
}


print.tex.cnt <- function(cnt.df,m){
  for (i in 1:length(cnt.df[,1])){
    cat(paste(gsub("&", "\\\\&", as.character(cnt.df[i,]$x)),"& & ",cnt.df[i,]$freq, "& & ",round(cnt.df[i,]$freq/m*10000)/100,"\\%\n"))
  }
}

get.cnt <- function(var){
  tmp_cnt <- plyr::count(var)
  isMode = (sum(grepl("discount",tmp_cnt$x)*1)>0) # to make Mode be sorted non alphabetically
  if(!isMode){
    tmp_cnt<-tmp_cnt[order(as.character(tmp_cnt$x)),]
  }
  return(tmp_cnt)
}


print.tex.effectVar<-function(localVar,varName){
  localVar<-as.numeric(localVar)
  l.min <- round(unname( stat.desc(localVar)[4]),3)
  l.max <- round(unname( stat.desc(localVar)[5]),3)
  l.mean <-round(unname( stat.desc(localVar)[9]),3)
  l.std <- round(unname( stat.desc(localVar)[13]),3)
  l.cnt <- round(unname( stat.desc(localVar)[1]),3)
  
  return(paste(gsub("&", "\\\\&", varName) ,"&&",
            sprintf("%.3f",l.min),"&&",
            sprintf("%.3f",l.max),"&&",
            sprintf("%.3f",l.mean),"&&",
            sprintf("%.3f",l.std),"&&",
            l.cnt,"\\\\[3pt]\n"))
}


star.symbol<-function(p){
  #  if(is.na(p)){
  #    return("");
  #  }
  #  else{
  Stars <- vector(mode="character", length=length(p))
  Stars<-rep("",length(p))
  Stars <- ifelse(p <= 0.1, "+" , Stars)
  Stars <- ifelse(p <= 0.05, "*" , Stars)
  Stars <- ifelse(p <= 0.01, "**" , Stars)
  Stars <- ifelse(p <= 0.001, "***" , Stars)
  return(Stars)
  #  }
}

star.symbol.tex<-function(p){
  #  if(is.na(p)){
  #    return("");
  #  }
  #  else{
  Stars <- vector(mode="character", length=length(p))
  Stars<-rep("",length(p))
  Stars <- ifelse(p <= 0.1, "^{+}" , Stars)
  Stars <- ifelse(p <= 0.05, "^{*}" , Stars)
  Stars <- ifelse(p <= 0.01, "^{**}" , Stars)
  Stars <- ifelse(p <= 0.001, "^{***}" , Stars)
  return(Stars)
  #  }
}

customRound<-function(val){
  return(ifelse(abs(val)>0.1,round(val,3),round(val*10,3)/10))
}


modelFromCox<-function(COXPH,name){
  
  is.cox<-grepl("coxph",class(COXPH))[1]
  
  if(is.cox){
    model.df<-data.frame(summary(COXPH)$coef)
    coef <- model.df$coef
    z <- model.df$coef/model.df$se.coef.
    p <- model.df[,length(model.df)]
  }
  else{
    model.df<-data.frame(summary(COXPH)$table)
    coef <- -model.df$Value
    z <- -model.df$z
    p <- model.df$p
  }
  
  
  model.df$covariates <- row.names(model.df)
  row.names(model.df) <-NULL
  
  model.df$paper <- paste(customRound(coef),star.symbol.tex(p),"\\;(",customRound(z),")",sep="")
  model.df$paper <- gsub(" ", "", model.df$paper)
  model.df$paper <- gsub("\\(", " \\(", model.df$paper)
  model.df<-model.df[c("covariates","paper")]
  model.df<-rename(model.df,c("paper"=name))
  return(model.df)
}

texify.df <- function(df){
  for (row in 1:length(df[,1])){
    hasGap <- F
    if(df[row,1]==""){
      hasGap=T
    }
    for (col in 1:length(df)){
      if(!is.na(df[row,col]) && grepl("NA",df[row,col])==0 && grepl("(NaN)",df[row,col])==0){
        cat(df[row,col])
      }
      if(col<length(df)){
        cat(" && ")
      }
    }
    if(hasGap){
      cat("\\\\[3pt]\n")
    }
    else{
      cat("\\\\\n")
    }
  }
}

lr.test.fn <- function(COX1,COX2){
  LR_chiSq<-2*(COX2$loglik[2]-COX1$loglik[2])
  delta.df <- length(COX2$coefficients) - length(COX1$coefficients)
  Prob_chi2<-1-pchisq(LR_chiSq,delta.df)
  return(Prob_chi2)
}

get.boot.summary <- function(BOOT){
  results.summary <- data.frame(BOOT$t0)
  results.summary <- rename(results.summary,c("results.t0"="original"))
  results.summary$mean<-apply(BOOT$t, 2, mean,na.rm=T)
  results.summary$se<-apply(BOOT$t, 2, sd,na.rm=T)
  results.summary$z <-results.summary$mean/results.summary$se
  results.summary$p <- (1-pnorm(abs(results.summary$z),0,1))*2
  return(results.summary)
}

getEstimates<-function(FITTED.MODEL){
  
  if("boot.summary" %in% class(FITTED.MODEL)){
    df <- data.frame(rownames(FITTED.MODEL),FITTED.MODEL$mean, FITTED.MODEL$p,FITTED.MODEL$z)
  }
  
  if("coxph" %in% class(FITTED.MODEL) & !("coxph.penal" %in% class(FITTED.MODEL)))
  {
    df <- data.frame(names(summary(FITTED.MODEL)$coef[,3]),
                     summary(FITTED.MODEL)$coef[,1],
                     unname(summary(FITTED.MODEL)$coef[,5]),
                     unname(summary(FITTED.MODEL)$coef[,4]))
  }
  
  if("survreg" %in% class(FITTED.MODEL)){
    model.df<-data.frame(summary(FITTED.MODEL)$table)
    df <- data.frame(row.names(model.df),
                     -model.df$Value,
                     model.df$p,
                     -model.df$z)
  }
  
  if("coxph.penal" %in% class(FITTED.MODEL))
  {
    df <- data.frame(names(summary(FITTED.MODEL)$coef[,3]),
                     summary(FITTED.MODEL)$coef[,1],
                     unname(summary(FITTED.MODEL)$coef[,6]),
                     unname(summary(FITTED.MODEL)$coef[,1]/summary(FITTED.MODEL)$coef[,2]))
  }
  
  colnames(df) <- c("var","estimate","p","t")
  rownames(df) <- NULL
  
  df<-df[!grepl("BID",df$var),]
  
  df.out <- data.frame(covariates=character(),cells=character())
  
  for(row in 1:length(df[,1])){
    top.cell <- as.character(paste(sprintf("%.3f",df[row,"estimate"]),star.symbol.tex(df[row,"p"]),sep=""))
    bottom.cell <- as.character(paste("(",sprintf("%.3f",df[row,"t"]),")",sep=""))
    top.df <- data.frame(df[row,"var"],top.cell)
    bottom.df <- data.frame(paste("__D__",df[row,"var"],sep=""),bottom.cell)
    colnames(top.df) <- colnames(df.out)
    colnames(bottom.df) <- colnames(df.out)
    df.out <- rbind(df.out,top.df)
    df.out <- rbind(df.out,bottom.df)
  }
  return(df.out)
}



stackColumn<-function(col.in,names.in){
  for(i in 1:length(col.in)){
    cnt.df<-get.cnt(col.in[[i]])
    cnt.df$x<-gsub("&","\\\\&",as.character(cnt.df$x))
    m<-sum(cnt.df$freq)
    cnt.df$rel <- paste(sprintf("%.2f",round(cnt.df$freq/m*10000)/100),"\\%",sep="")
    cnt.df<-rbind(c(names.in[i],"{count}","{percentage}"),cnt.df,c("\\textbf{total}",m,"100\\%" ),c("","",""))
    if(i==1){
      col <- cnt.df
    }
    else{
      col<-rbind(col,cnt.df)
    }
  }
  return(col)
}

get.finish.lines<-function(col.in){
  line = 1
  line.vec = c()
  for(i in 1:length(col.in)){
    cnt = get.cnt(col.in[[i]])
    line = line + length(cnt$freq)
    line.vec = c(line.vec,line)
    line=line+3
  }
  return(line.vec)
}

get.start.lines<-function(col.in){
  line = 1
  line.vec = c(line)
  for(i in 1:(length(col.in))){
    if(i<length(col.in)){
      cnt = get.cnt(col.in[[i]])
      line = line + length(cnt$freq)+3
      line.vec = c(line.vec,line)
    }
  }
  return(line.vec)
}

createTable<-function(col.1.in,names.1.in,col.2.in,names.2.in){
  col.1 <- stackColumn(col.1.in,names.1.in)
  col.2 <- stackColumn(col.2.in,names.2.in)
  while(length(col.1[,1])<length(col.2[,1])){
    col.1<-rbind(col.1,c("","",""))
  }
  while(length(col.2[,1])<length(col.1[,1])){
    col.2<-rbind(col.2,c("","",""))
  }
  col.1.start.lines<-get.start.lines(col.1.in)
  col.2.start.lines<-get.start.lines(col.2.in)
  col.1.finish.lines<-get.finish.lines(col.1.in)
  col.2.finish.lines<-get.finish.lines(col.2.in)
  
  df<-(unname(cbind(col.1,col.2)))
  
  num.cols <- length(df) 
  num.rows <- length(df[,1])
  for(r in 1:num.rows){
    string=""
    for(c in 1:num.cols){
      if(c<num.cols){
        string = paste(string,df[r,c],"&&")
      }
      else{
        string= paste(string,df[r,c])
      }
    }
    if(r %in% col.1.start.lines | r %in% col.2.start.lines){
       string = paste(string,"\\\\")}
    else{
      string = paste(string,"\\\\[3pt]")
    }
    if(r %in% col.1.start.lines){
      string=paste(string,"\\cline{1-5}\\\\[-12pt]")
    }
    if(r %in% col.2.start.lines){
      string=paste(string,"\\cline{7-11}\\\\[-12pt]")
    }
    if(r %in% col.1.finish.lines){
      string=paste(string,"\\cline{1-5}\\\\[-12pt]")
    }
    if(r %in% col.2.finish.lines){
      string=paste(string,"\\cline{7-11}\\\\[-12pt]")
    }
    cat(string,"\n")
  }
}


df.to.tex <- function(df){
  for(row in 1:length(df[,1])){
    for(col in 1:length(df)){
      cat(paste("&&\\muc{2.5cm}{",round(df[row,col],2),"}",sep=""))
    }
    cat("\\\\\n")
  }
}


getFit <- function(MODEL,WIDTH=1.5){
  stats <- c("$R^2$","$F$","$N$","$p$")
  
  if(grepl("plm|panelmodel",class(MODEL))[1]){
    rsq <-  sprintf("%0.2f",(round(summary(MODEL)$r.squared[1]*100)/100))
    p.score <- summary(MODEL)$fstatistic$p.value
    if(p.score<0.001){
      p.score <- "<.001"
    }
    else{
      p.score<-round(p.score*1E3)/1E3
    }
    cells <- c(rsq,
               round(summary(MODEL)$fstatistic$statistic*100)/100,
               paste("\\muc{",WIDTH,"cm}{",length(summary(MODEL)$residuals),"}",sep=""),
               p.score)
    df <- data.frame(stats,cells)
  }
  else{
    cells <- c("","","","")
    df <- data.frame(stats,cells)
  }
  #https://stackoverflow.com/questions/45327217/r-squared-of-lmer-model-fit
  # R-squared: marginal R^2 value
 # if(grepl("merModLmerTest",class(MODEL))){
#    stats <- c("$R^2$")
#    cells<-sprintf("%0.2f",(r.squaredGLMM(MODEL)[1]*100)/100)
 #   df <- data.frame(stats,cells)
#  }
  return(df)
}



mergeFits <- function(...){
  n.args <- length(list(...))
  cols <- c("stats")
  if(n.args>0){
    fits.df <- list(...)[1]
    cols <- c(cols,"Fit 1")
    for(i in 2:n.args){
      fits.df<-merge(fits.df, list(...)[i],by="stats",all=T)
      cols <- c(cols,paste("Fit ",i,sep=""))
    }
    colnames(fits.df)<-cols
    # fits.df<-fits.df[c(4,1:3),]
    return(fits.df)
  }
}

texify.fits.df <- function(df){
  for (row in 1:length(df[,1])){
    for (col in 1:length(df)){
      if(!is.na(df[row,col]) && grepl("NA",df[row,col])==0){
        cat(as.character(df[row,col]))
      }
      if(col<length(df)){
        cat(paste(" && "))
      }
    }
    if(df[row,"stats"]==""){
      cat(paste("\\\\[3pt]\n"))
    }
    else{
      cat(paste("\\\\\n"))
    }
  }
}


# collapse small categories
aggregate.category.fn <- function(var,CUTOFF = 5){
  small.groups <- var %>% plyr::count(.) %>% data.frame %>% subset(freq <= CUTOFF) %>% dplyr::select(x) %>% unlist %>% unname %>% as.character 
  levels(var)[levels(var) %in% small.groups] <- "Other"
  return(droplevels(var))
}

drop.few.BID.obs.fn <- function(data = scf_cleaned.df,CUTOFF = 5){
  small.groups <- data$BID %>% plyr::count(.) %>% data.frame %>% subset(freq <= CUTOFF) %>% dplyr::select(x) %>% unlist %>% unname %>% as.character 
  data <- data %>% subset(!BID %in% small.groups)
  return(data)
}

drop.2009.fn <- function(data = scf_cleaned.df){
  return(data[data$Supplier.creationYear != 2009,])
}


get.fe.r.sq <- function(MODEL,NULL.MODEL){
  return(1-var(residuals(MODEL),na.rm=T)/var(residuals(NULL.MODEL),na.rm=T))
}