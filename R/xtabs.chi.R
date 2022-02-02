# From: https://cran.r-project.org/doc/contrib/Lemon-kickstart/kr_xtab.html
# format.xtab attempts to display a
# conventionally formatted 2D crosstabulation with
# optional chi-squared test.
format.xtab <- function(v1, v2, dataframe, dnn=NULL, fieldwidth=10, chisq=FALSE) {
 if(!missing(v1) && !missing(v2)) {
  # get the table of frequencies
  if(!missing(dataframe))
   basetab<-table(dataframe[[v1]],dataframe[[v2]])
  else basetab<-table(v1,v2)
  # get the dimension of the current table
  btdim<-dim(basetab)
  # create two empty vectors to hold each row of row and column percentages
  row.pc<-col.pc<-vector("numeric",btdim[2])
  # row and column sums
  row.sums<-apply(basetab,1,sum)
  col.sums<-apply(basetab,2,sum)
  # row and column names
  row.names<-formatC(rownames(basetab),width=fieldwidth)
  col.names<-formatC(colnames(basetab),width=fieldwidth)
  grand.total<-sum(row.sums)
  rowlabelspace<-paste(rep(" ",nchar(row.names[1])),sep="",collapse="")
  # make sure that there are some sort of dimension labels
  if(is.null(dnn))
   dnn<-formatC(c(v1,v2),width=fieldwidth)
  else dnn<-formatC(dnn,width=fieldwidth)
  # display the header
  cat("Crosstabulation of",dnn[1],"by",dnn[2],"\n")
  # display the column dimension label
  cat(rowlabelspace,dnn[2],"\n")
  # display the column labels
  cat(rowlabelspace,col.names,"\n")
  # display the row dimension label
  cat(dnn[1],"\n")
  # now display each row of frequencies with its label and row sum
  # followed by the percentages
  for(i in 1:btdim[1]) {
   row.pc<-ifelse(row.sums[i],100*basetab[i,]/row.sums[i],0)
   for(j in 1:btdim[2])
    col.pc[j]<-ifelse(col.sums[j],100*basetab[i,j]/col.sums[j],0)
   cat(row.names[i],formatC(basetab[i,],width=fieldwidth),
    formatC(row.sums[i],width=fieldwidth),"\n")
   cat(rowlabelspace,formatC(round(row.pc,2),width=fieldwidth),
    formatC(round(100*row.sums[i]/grand.total,2),width=fieldwidth),"\n")
   cat(rowlabelspace,formatC(round(col.pc,2),width=fieldwidth),"\n\n")
  }
  # display the column sums and grand total
  cat(rowlabelspace,formatC(col.sums,width=fieldwidth),
   formatC(grand.total,width=fieldwidth),"\n")
  # display the column percentages
  cat(rowlabelspace,
   formatC(round(100*col.sums/grand.total,2),width=fieldwidth),"\n\n")
  # do the chi squared if it was ordered
  if(chisq) {
   chisq.obs<-chisq.test(basetab)
   cat(names(chisq.obs$statistic),"=",
    round(chisq.obs$statistic,3),
    names(chisq.obs$parameter),"=",
    chisq.obs$parameter,"p =",
    round(chisq.obs$p.value,5),"\n\n")
  }
  invisible(basetab)
 }
 else cat("Usage: format.xtab(v1, v2, dataframe[, dnn=NULL, fieldwidth = 10, chisq = FALSE])\n")
}

# xtab will try to break down the formula passed to it into
# one or more 2D crosstabulations with hierarchical counts
# for higher level factors.

xtab <- function(formula, dataframe, dnn=NULL, fieldwidth=10, chisq=FALSE) {
 if(!missing(formula) && !missing(dataframe)) {
  xt<-as.character(attr(terms(formula),"variables")[-1])
  nxt<-length(xt)
  if(nxt > 2) {
   by.factor<-as.factor(dataframe[[xt[nxt]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   brkstats<-as.list(rep(0,nlevels))
   names(brkstats)<-factor.levels
   for(i in 1:nlevels) {
    currentdata<-subset(dataframe,by.factor == factor.levels[i])
    currentcount<-length(currentdata[[nxt]])
    totalcount<-length(dataframe[[nxt]])
    cat("\nCount for",xt[nxt],"=",factor.levels[i],"is",
     currentcount,"(",round(100*currentcount/totalcount,1),"%)\n\n")
    rightside <-ifelse(nxt > 3,paste(xt[2:(nxt-1)],sep="",collapse="+"),xt[2])
    next.formula<-
     as.formula(paste(xt[1],rightside,sep="~",collapse=""))
    xtab(next.formula,currentdata,dnn,fieldwidth,chisq)
   }
  }
  else format.xtab(xt[1],xt[2],dataframe,dnn,fieldwidth,chisq)
 }
 else cat("Usage: xtab(formula, dataframe[, dnn=NULL, fieldwidth = 10, chisq = FALSE])\n")
}
