
# Code by Kari Lock Morgan, 5/12
# Modifed by David Price, 3/15

#new functions
percentile = function(x,probs,df=NULL,...) {
  if (class(x)=="numeric") return(stats::quantile(x,probs=probs,na.rm=TRUE,...))
  if (class(x)=="data.frame") return(apply(x, 2, stats::quantile, probs=probs, na.rm=TRUE))
  else if (x == "normal") return(qnorm(p=probs,...))
  else if (x == "t") {
    if (is.null(df)) return("Error: Must specify df")
    else return(qt(p=probs,df=df, ...))
  }
}

tail.p = function(dist=NULL, stat=NULL, tail=NULL, mean=0, sd=1, df=NULL,df1=NULL, df2=NULL,...) {
  if (is.null(tail)) return("Please specify tail = 'lower', tail = 'upper', or tail = 'two'")
  if (class(dist)=="data.frame") dist = dist[,1]
  if(class(dist)=="numeric") {
    hist(dist, xlim=c(min(c(min(dist), stat)), max(c(max(dist), stat))),main="",xlab="",...)
    abline(v=stat, lwd=2, col="red")
    if (tail == "lower") return(mean(dist<= stat))
    else if (tail == "upper") return(mean(dist>= stat))
    else if (tail == "two") return(2*min(c(mean(dist<=stat), mean(dist>=stat))))
  }
  else if (dist == "normal") {
    xlim=c(min(mean-4*sd, stat), max(mean+4*sd, stat))
    xx = seq(from=xlim[1],to=xlim[2], len=1000)
    yy = dnorm(xx, mean=mean, sd=sd)
    plot(xx,yy, type="l",axes=FALSE,xlab="Statistic",ylab="", main="Normal Distribution")
    axis(1)
    abline(v=stat, lwd=2, col="red")    
    if (tail=="lower") return(pnorm(stat,lower.tail=TRUE,mean=mean, sd=sd))
    else if (tail=="upper") return(pnorm(stat,lower.tail=FALSE,mean=mean, sd=sd))
    else if (tail=="two") return(2*pnorm(abs(stat), lower.tail=FALSE, mean=mean, sd=sd))
  }
  else if (dist == "t") {
    if (is.null(df)) return("Please specify df")
    else {
      xlim=c(min(mean-4*sd, stat), max(mean+4*sd, stat))
      xx = seq(from=xlim[1],to=xlim[2], len=1000)
      yy = dt(xx, df=df)
      plot(xx,yy, type="l",axes=FALSE,xlab="Statistic",ylab="", main="t-distribution")
      axis(1)
      abline(v=stat, lwd=2, col="red")    
      if (tail=="lower") return(pt(stat,lower.tail=TRUE,df=df))
      else if (tail=="upper") return(pt(stat,lower.tail=FALSE,df=df))
      else if (tail=="two") return(2*pt(abs(stat), lower.tail=FALSE, df=df))
    }
  }
  else if (dist == "chisquare") {
    if (is.null(df)) return("Please specify df")
    else {
      x = rchisq(1000,df=df)
      xlim=c(0, max(c(x,stat)))
      xx = seq(from=xlim[1],to=xlim[2], len=1000)
      yy = dchisq(xx, df=df)
      plot(xx,yy, type="l",axes=FALSE,xlab="Statistic",ylab="", main="Chi-Square Distribution")
      axis(1)
      abline(v=stat, lwd=2, col="red")    
      if (tail=="lower") return(pchisq(stat,lower.tail=TRUE,df=df))
      else if (tail=="upper") return(pchisq(stat,lower.tail=FALSE,df=df))
      else if (tail=="two") return(2*pchisq(abs(stat), lower.tail=FALSE, df=df))
    }
  }
  else if (dist == "f") {
    if (is.null(df1) | is.null(df2)) return("Please specify df1 (numerator df) and df2 (denominator df)")
    else {
      x = rf(1000,df1=df1, df2=df2)
      xlim=c(0, max(c(x,stat)))
      xx = seq(from=xlim[1],to=xlim[2], len=1000)
      yy = df(xx, df1=df1, df2=df2)
      plot(xx,yy, type="l",axes=FALSE,xlab="Statistic",ylab="", main="F-Distribution")
      axis(1)
      abline(v=stat, lwd=2, col="red")    
      if (tail=="lower") return(pf(stat,lower.tail=TRUE,df1=df1, df2=df2))
      else if (tail=="upper") return(pf(stat,lower.tail=FALSE,df1=df1, df2=df2))
      else if (tail=="two") return(2*pf(abs(stat), lower.tail=FALSE, df1=df1, df2=df2))
    }
  }
}
  
#making R automatically ignore missing data - DOESN'T WORK FOR MEAN
#changed by dprice to avoid mosaic dependencies 20140903
mean = function(x,na.rm=TRUE,...) return(base::mean(x,na.rm=na.rm, ...))
range = function(x, na.rm=TRUE,...) return(base::range(x, na.rm=na.rm,...))
median = function(x, na.rm=TRUE, ...) return(stats::median(x,...,na.rm=na.rm))
sd = function(x,na.rm=TRUE, ...) return(stats::sd(x,..., na.rm=na.rm))
cor = function(x,y=NULL,use="pairwise.complete.obs", ...) return(stats::cor(x,y,...,use=use))  #DPRICE ADDED y=NULL and y arg
max = function(x,na.rm=TRUE,...) return(base::max(x, ..., na.rm=na.rm))
min = function(x,na.rm=TRUE,...) return(base::min(x, ..., na.rm=na.rm))

prop.test = function(...) return(stats::prop.test(...))
#differences in means, proportions
#diffMean = 


#for loading in google docs
#have to first "publish to the web" as csv
#key is everything between key= and # in url

google.doc = function(key=NULL) {
  #require(RCurl)
  myCsv = getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key, "&output=csv", sep=""))
  read.csv(textConnection(myCsv))
}

#bargraph 
barplot = function(x, ...) {
  if (class(x)=="table") return(graphics::barplot(x,...))
  else if(class(x)=="formula") {
    vars = model.frame(x)
    return(graphics::barplot(table(vars[,2],vars[,1]), legend=TRUE,...))
  }
  else  return(graphics::barplot(table(x), ...))
}

diffMean = function(formula, data=NULL, ...) compareMean(formula, data, na.rm=TRUE, ...)
diffProp = function(formula, data=NULL, ...) compareProportion(formula, data, na.rm=TRUE, ...)

coin.flips = function(n,p) rbinom(1, n, p)

#to go from a two-way table to a dataset

# make.data = function(counts, n, var1 = NULL, var2 = NULL, levels1 = NULL, levels2 = NULL) {
#   #var1: how rows of table are divided
#   #var2: how columns of table are divided
#   # counts = numbers in first row of table
#   # n = numbers in total row of table
#    if (is.null(levels1)) levels1 = 1:length(counts)
#    if (is.null(levels2)) levels2 = 1:
# }
 