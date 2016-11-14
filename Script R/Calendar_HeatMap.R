# Input load. Please do not change #
dataset = read.csv('emaildata.csv');
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

require(quantmod)
require(ggplot2)
require(reshape2)
require(plyr)
require(scales)

dataset$DateReceived <- as.Date(dataset$DateReceived)

# We will facet by year ~ month, and each subgraph will
# show week-of-month versus weekday
# the year is simple
dataset$year<-as.numeric(as.POSIXlt(dataset$DateReceived)$year+1900)
# the month too 
dataset$month<-as.numeric(as.POSIXlt(dataset$DateReceived)$mon+1)
# but turn months into ordered facors to control the appearance/ordering in the presentation
dataset$monthf<-factor(dataset$month,levels=as.character(1:12),labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),ordered=TRUE)
# the day of week is again easily found
dataset$weekday = as.POSIXlt(dataset$DateReceived)$wday
# again turn into factors to control appearance/abbreviation and ordering
# I use the reverse function rev here to order the week top down in the graph
# you can cut it out to reverse week order
dataset$weekdayf<-factor(dataset$weekday,levels=rev(0:6),labels=rev(c("Dom","Seg","Ter","Qua","Qui","Sex","Sab")),ordered=TRUE)
# the monthweek part is a bit trickier 
# first a factor which cuts the data into month chunks
dataset$yearmonth<-as.yearmon(dataset$DateReceived)
dataset$yearmonthf<-factor(dataset$yearmonth)
# then find the "week of year" for each day

dataset$week <- as.numeric(format(dataset$DateReceived,"%W"))
# and now for each monthblock we normalize the week to start at 1 
dataset<-ddply(dataset,.(yearmonthf),transform,monthweek=1+week-min(week))

# Now for the plot
ggplot(dataset, aes(monthweek, weekdayf, fill = EmailsDia)) + 
  geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="gray", high="blue") +
  ggtitle("E-mails recebidos por dia") +  xlab("Semana") + ylab("") + guides(fill=guide_legend(title="Quantidade de e-mails"))
