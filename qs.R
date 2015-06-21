clean.gd <- function() {
  
  ##set libraries
  require(googlesheets)
  require(dplyr)
  require(lubridate)
  require(ggplot2)
  require(reshape2)
  require(data.table)
  
  ##set working directory
  #dir <- "~/Dropbox/QS"
  #setwd(dir)
  
  ##authenticate 
  gs_auth()
  
  ##register spreadsheet
  qs.data <- gs_title("QS")
  
  ##grab data from google spreadsheets
  kw.qs <<- qs.data %>% get_via_csv(ws = "Keyword")
  
  ##clean data
  kw.qs$Date <- mdy(kw.qs$Date)
  names(kw.qs)[7] <<- "wQS"
  
  match.type.gd()
}

match.type.gd <- function() {
  
  ##break out match type
  
  ##idenitfy Match Type
  bmm.kw <<- kw.qs[grep("\\+",kw.qs$Keyword),]
  kw.qs <<- kw.qs[grep("\\+", kw.qs$Keyword, invert=TRUE),]
  bmm.kw$Type <<- "BMM"
  bracket.kw <<- kw.qs[grep("\\[",kw.qs$Keyword),]
  kw.qs <<- kw.qs[grep("\\[", kw.qs$Keyword, invert=TRUE),]
  bracket.kw$Type <<- "Exact"
  quote.kw <<- kw.qs[grep("\\\"",kw.qs$Keyword),]
  kw.qs <<- kw.qs[grep("\\\"", kw.qs$Keyword, invert=TRUE),]
  quote.kw$Type <<- "Phrase"
  broad.kw <<- kw.qs
  broad.kw$Type <<- "Broad"
  kw.qs <<- NULL
  kw.qs <<- data.frame(kw.qs)
  
  ##combine into original data frame
  kw.qs <<- rbind(bmm.kw, bracket.kw)
  kw.qs <<- rbind(broad.kw, kw.qs)
  kw.qs <<- rbind(quote.kw, kw.qs)

  adwords.data()
}

adwords.data <- function() {
  
  ##read data from csv
  adwords.db <<- read.csv("kw_report-1.csv", header=TRUE)
  
  
  mean.qs()
}

##Calculation function
mean.qs <- function() {
  
  ##For regualr Quality Score, calculate aggregate means for Accounts, Campaigns, AdGroups and by Date
  mean.ag.qs <<- aggregate(kw.qs$QS, data=kw.qs, by=list(AdGroup=factor(kw.qs$AdGroup)), mean)
  mean.cmpg.qs <<- aggregate(kw.qs$QS, data=kw.qs, by=list(Campaign=factor(kw.qs$Campaign)), mean)
  mean.type.qs <<- aggregate(kw.qs$QS, data=kw.qs, by=list(Type=factor(kw.qs$Type)), mean)
  mean.date.qs <<- aggregate(kw.qs$QS, data=kw.qs, by=list(Date=kw.qs$Date), mean)
  
  ##For weighted Quality Score, calculate aggregate means for Accounts, Campaigns, AdGroups and by Date
  mean.ag.wqs <<- aggregate(kw.qs$wQS, data=kw.qs, by=list(AdGroup=factor(kw.qs$AdGroup)), mean)
  mean.cmpg.wqs <<- aggregate(kw.qs$wQS, data=kw.qs, by=list(Campaign=factor(kw.qs$Campaign)), mean)
  mean.type.wqs <<- aggregate(kw.qs$wQS, data=kw.qs, by=list(Type=factor(kw.qs$Type)), mean)
  mean.date.wqs <<- aggregate(kw.qs$wQS, data=kw.qs, by=list(Date=kw.qs$Date), mean)
  
  
  colnames(mean.date.qs) <<- c("Date","QS")
  colnames(mean.date.wqs) <<- c("Date","wQS")
  date.qs <<- merge(mean.date.qs,mean.date.wqs)
  
  qs.graphs()
}


qs.graphs <- function() {
  
  
  ######First Graph######
  
  ##melt data for graph
  date.qs <<- melt(date.qs, id="Date")
  
  ##plots weighted QS and regular QS by Date
  qs.plot<<- ggplot(data=date.qs, aes(x=Date, colour=variable))+geom_line(aes(y=value,group=variable))+geom_line(aes(y=value,group=variable))+geom_point(aes(y=value, group=variable))
  
  ##Saves plot as PNG file
  ggsave(qs.plot, file="QSbyDatePlot.png", width = 7, height = 7, units="in", dpi=75)
  
  
  
  #####Next Graph#####
  
  ##Saves data frame as a data.table
  dt <- data.table(adwords.db)
  
  ##Aggregates data from adwords.db
  dt.out <<- dt[, list(wcQS=weighted.mean(Quality.score, Clicks), wiQS=weighted.mean(Quality.score, Impressions)), by=Day]
  
  ##melts data for graph
  dt.out <<- melt(dt.out, id="Day")
  
  ##creates graph
  w.qs.plot <<- ggplot(data=dt.out, aes(x=Day, colour=variable))+geom_line(aes(y=value, group=variable))+geom_line(aes(y=value, group=variable))

  ##Saves graph
  ggsave(w.qs.plot, file="wQSbyDatePlot.png", width = 7, height = 7, units="in", dpi=150)
}
 
third.graph <- function() { 
  ####Third Graph####
  
  dt <- data.table(adwords.db)
  ##aggregate data
  match.out <<- dt[, list(wcQS=weighted.mean(Quality.score, Clicks), wiQS=weighted.mean(Quality.score, Impressions)), by=Match.type]
  
  
  ##melt data for graph
  match.out <<- melt(match.out, id="Match.type") 
  
  
  ##create graph
  match.qs.plot <<- ggplot(data=match.out, aes(x=Match.type)) + geom_bar(aes(y=variable))
  
  
}
