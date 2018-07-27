### PERCEIVED SONG TIMES

times <- c(5,6,7,7,7,8,8,8,8,8,8,10,10,10,10,10,10,10,10,10,10,10,12,12,12,13,13,13,15,15,15,15,15,15,15,15,15,15,20,20,20,20,21,22,30,30,30,30)


# Code to accompany Statistics: Unlocking the Power of Data by Lock^5
# Loads in data from the textbook and designed to make R more user-friendly
# written by Kari Lock Morgan, Fall 2012

# from within RStudio, students run:
# source("/shared/kari.lock.morgan@gmail.com/Lock5.R")

######  BOOTSTRAPPING AND RANDOMIZATION TESTS
# Code by Kari Lock Morgan, 5/11
# From within RStudio: source("/shared/kari.lock.morgan@gmail.com/simulation.R")

#used for both bootstrap interval and randomization test
find.statistic = function(y1, y2=NULL, statistic = NULL, output = TRUE) {
  #browser() Q to quit
  #possible error
  if (!is.null(y2)) if (length(y1) != length(y2)) stop("The two variables must be of the same length.")
  
  #dealing with missing data
  if (is.null(y2)) {
    if (sum(is.na(y1))>0) y1 = y1[!is.na(y1)] 
  }
  else { #two variables
    if (sum(is.na(y1))>0 | sum(is.na(y2))>0) {
      y1.temp = y1[!is.na(y1) & !is.na(y2)]
      y2.temp = y2[!is.na(y1) & !is.na(y2)]
      y1 = y1.temp
      y2 = y2.temp
    }
  }
  
  #finding variable types
  type1 = "categorical"
  if (is.numeric(y1) & dim(table(y1))>2) type1 = "quantitative"
  if (is.null(y2)) {
    type2 = "only1var"
    if (output) cat(paste("One", type1, "variable", "\n"))
  }
  else { #y2 exists
    type2 = "categorical"
    if (is.numeric(y2) & dim(table(y2))>2) type2 = "quantitative"
    if (output) cat(paste("Variable 1:", type1, ", Variable 2:", type2, "\n"))
  }
  
  #the quantitative variable should be y1 if one categorical and one quantitative
  if (type1 == "categorical" & type2== "quantitative") {
    y1.temp = y2
    y2.temp = y1
    y1 = y1.temp
    y2 = y2.temp
    type1 = "quantitative"
    type2 = "categorical"
  }
  
  #finding sample statistic
  n = length(y1)
  label = "Statistic"
  
  if (is.null(y2)) { #one variable
    
    if (is.null(statistic)) {
      if (type1=="quantitative") {
        statistic = mean
        label = "Mean"
      }
      else { #categorical
        statistic = function(y1) sum(y1 == levels(as.factor(y1))[1])/n    #sample proportion
        label = "Proportion"
      }
    }
    actual = statistic(y1) 
  }
  
  else {   #two variables
    if (is.null(statistic)) {
      
      if (type1 == "categorical" & type2 == "categorical") {
        
        if (length(levels(as.factor(y1)))> 2 | length(levels(as.factor(y2)))> 2 ) { #chi-square
          statistic = function(y, x) chisq.test(table(y, x), correct=FALSE, simulate.p.value=TRUE, B=100)$statistic
          label = "Chi-Square Statistic"
          if (num==10000) num = 5000
        }
        
        else { #difference in proportions
          n1 = sum(y2==levels(as.factor(y2))[1])
          n2 = sum(y2==levels(as.factor(y2))[2])
          statistic = function(y,group) sum(y==levels(as.factor(y))[1] & group==levels(as.factor(group))[1])/n1 - sum(y==levels(as.factor(y))[1] & group==levels(as.factor(group))[2])/n2 
          label = "Difference in Proportions" 
        }
      }
      
      else if (type1 == "quantitative" & type2 == "categorical") {
        
        if (length(levels(as.factor(y2)))> 2 ) { #anova
          statistic = function(y, x) anova(lm(y~x))$"F value"[1]
          label = "F Statistic"
          if (num==10000) num = 2000
        }
        
        else { #difference in means
          statistic = function(y,group) mean(y[group==levels(as.factor(group))[1]]) - mean(y[group == levels(as.factor(group))[2]]) #difference in means
          label = "Difference in Means"
        }
      }
      
      else { #correlation
        statistic  = cor
        label = "Correlation"
      }
    }
    
    actual = statistic(y1, y2)
  }
  
  if(output) {
    if (is.null(statistic)) cat(label, "\n")
    cat(paste("Observed ", label, sep="", ": ", round(actual,3)), "\n")    
  }
  
  return(list("statistic" = statistic, "label"=label, "actual"=actual, "vars" = list(y1, y2)))
}


#Resampling Function
bootstrap.interval = function(y1, y2 = NULL, num = 10000, level=95, statistic=NULL, distribution = FALSE, visual=TRUE, output = TRUE) {
  #y1: variable
  #y2: second variable if applicable
  #num: number of bootstrap samples
  #level: confidence level, 95% by default
  #statistic: If unspecified, defaults to mean for quantitative data, proportion for binary data, difference in proportions for binary y and y2, difference in means for quantitative y, binary y2, and correlation for quantitative y and y2.  Otherwise, can be any function of y or y and y2 (e.g. median, sd, function(y, y2) = ...)
  #distribution:  make true if want whole bootstrap distribution returned, otherwise returns CI
  
  n = length(y1)
  
  stat = find.statistic(y1, y2)
  statistic = stat$statistic
  label = stat$label
  actual = stat$actual
  y1 = stat$vars[[1]]
  y2 = stat$vars[[2]]
  
  #creating bootstrap distribution
  if (output) cat("Resampling, please wait...", "\n")
  
  if (is.null(y2)) { #one variable
    y1.boot = matrix(NA, n, num) #each column is a bootstrap sample
    for (i in 1:num) y1.boot[,i] = sample(y1, n, replace=TRUE)
    bootstrap.dist = apply(y1.boot, 2, statistic)
  }
  
  else {   #two variables
    bootstrap.dist = rep(NA, num)
    for (i in 1:num) {
      indices = sample(1:n, n, replace=TRUE)
      bootstrap.dist[i] = statistic(y1[indices], y2[indices])#USE APPLY FOR THIS - REWRITE!!!
    }
  }
  
  if (output) cat(paste("SE = ", round(sd(bootstrap.dist),3)), "\n")
  
  #confidence interval
  if (level < 1) level = level*100
  ci = quantile(bootstrap.dist, c((1 - level/100)/2,1 - (1 - level/100)/2))
  if (output) cat(paste(level, sep="", "% Confidence Interval:"), "\n")
  if (distribution) cat(ci, "\n")
  
  if(visual) {
    counts = hist(bootstrap.dist, main="Bootstrap Distribution", xlab=label)$counts  
    for (i in 1:2) {
      segments(ci[i], 0, ci[i], max(counts)-max(counts)*.1, col="red", lwd=2)
      text(round(ci[i],3), max(counts), pos=1, col="red", round(ci[i],3))
    }
  }
  
  if (distribution) return(bootstrap.dist) 
  else return(ci)
  
}

#Reallocating function  (for a randomization test)
randomization.test = function(y1, y2=NULL, num=5000, tail=NULL, null=NULL, statistic = NULL, distribution = FALSE, tail.prob=FALSE, visual=TRUE, output=TRUE) {
  #y1 = response variable
  #y2 = group assignment (or second variable)
  #num = number of simulated randomizations
  #tail = "lower", "upper", or "two" (if left blank, return proportion in smaller tail)
  #null = specify null for single variable.  for two variables, null is no association.
  #statistic = if unspecified, defaults to difference in proportions for binary y, difference in means for quantitative y and binary group, or correlation for quantitative y and group.  Otherwise, can be specified to by any function in the form function(x, g), where x is the first variable and g is the second.
  #distribution: make TRUE if want randomization distribution returned
  #visual = TRUE (make false to not display plot)
  # browser()
  
  stat = find.statistic(y1, y2)
  statistic = stat$statistic
  label = stat$label
  actual = stat$actual
  y1 = stat$vars[[1]]
  y2 = stat$vars[[2]]
  n = length(y1)
  
  #randomization distribution
  if (output) cat("Randomizing, please wait...\n")
  
  if (!is.null(y2)) {
    group.sim = matrix(NA, n, num) #each column is a simulated randomization
    for (i in 1:num) group.sim[,i] = sample(y2, n, replace=FALSE)
    randomization.dist = apply(group.sim, 2, statistic, y = y1)  
  } 
  else { #just one variable
    if (is.null(null)) return("Please specify null")
    y1.boot = matrix(NA, n, num) #each column is a bootstrap sample
    for (i in 1:num) y1.boot[,i] = sample(y1, n, replace=TRUE)
    bootstrap.dist = apply(y1.boot, 2, statistic)
    randomization.dist = bootstrap.dist + (null - mean(bootstrap.dist)) #shifting to make null true
  }
  
  if(visual) {
    xmin = min(c(actual, randomization.dist))
    xmax = max(c(actual, randomization.dist))
    
    counts = hist(randomization.dist, main="Randomization Distribution", xlab=label, xlim=c(xmin, xmax))$counts  
    abline(v = actual, col="red", lwd=2)
  }
  
  if (output) cat("SE = ", round(sd(randomization.dist),3), "\n")
  
  if (!is.null(tail)) {
    if (tail=="lower") {
      p = mean(randomization.dist <= actual)
      if (visual) text(mean(c(min(randomization.dist), actual)), max(counts)*.2, round(p,3), col = "red")
      if (output) cat(paste("Lower-tail p-value: ", p), "\n")
    }
    if (tail=="upper") {
      p = mean(randomization.dist >= actual)
      if (visual) text(mean(c(max(randomization.dist), actual)), max(counts)*.2,  round(p,3), col = "red")
      if (output) cat(paste("Upper-tail p-value: ", p), "\n")
    }
    if (tail=="two") {
      smaller.tail = min(c(mean(randomization.dist <= actual), mean(randomization.dist >= actual))) 
      p = 2*smaller.tail
      if (visual) {
        if (actual <= median(randomization.dist)) text(mean(c(min(randomization.dist), actual)), max(counts)*.2, round(smaller.tail,3), col = "red")
        else text(mean(c(max(randomization.dist), actual)), max(counts)*.2,  round(smaller.tail,3), col = "red")
      }
      if (output) cat(paste("Two-tail p-value: ", p), "\n")
    }
  }
  
  else if (is.null(tail)) {
    
    if (label=="Chi-Square Statistic" | label == "F Statistic") {
      p = mean(randomization.dist >= actual)
      if (visual)  text(mean(c(max(randomization.dist), actual)), max(counts)*.2,  round(p,3), col = "red")
      if (output) cat(paste("Upper Tail Probability: ", p), "\n")
    }
    
    else {
      p = min(c(mean(randomization.dist <= actual), mean(randomization.dist >= actual))) 
      if (visual) {
        if (actual <= median(randomization.dist)) text(mean(c(min(randomization.dist), actual)), max(counts)*.2, round(p,3), col = "red")
        else text(mean(c(max(randomization.dist), actual)), max(counts)*.2,  round(p,3), col = "red")
      }
      if (output) cat(paste("Smaller Tail Probability: ", p), "\n")
    }
  }
  
  if (distribution) return(sort(randomization.dist)) 
  if(tail.prob) return(p)
  
}


# SIMULATION.R ABOVE, LOCK5.R BELOW

#installing and loading necessary packages, if not already installed and loaded

#if (!(library("Lock5Data", logical.return=TRUE))) {
#   install.packages("Lock5Data")
 #  library("Lock5Data")
#}

#if (!(library("mosaic", logical.return=TRUE))) {
 # install.packages("mosaic")
  #library("mosaic")
#}

#install.packages("devtools") DPRICE
#library("devtools") DPRICE
#install_github("mosaic","rpruim", "R2", local=TRUE) DPRICE
#library("mosaic") DPRICE

#### ADDED BY DPRICE
#requires mosaic to be installed?
#removed DPRICE for 2.15 if (!(library("devtools", logical.return=TRUE))) {
  #install.packages("devtools")
#  library("devtools")
#}
#if (!(library("mosaic", logical.return=TRUE))) {
#install.packages("mosaic")
#library("mosaic")
#}

#if (!(library("RCurl", logical.return=TRUE))) {
 # install.packages("RCurl")
 # library("RCurl")
#}

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
 