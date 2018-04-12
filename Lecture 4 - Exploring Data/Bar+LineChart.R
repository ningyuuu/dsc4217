#This script shows how to draw stacked bar chart together with line chart to show both comparison in numbers
#and ratios across categories

library(qqplot2)

#read in data
custdata<-read.table('custdata.tsv',header=T,sep = '\t')

#Build up statistics 
stats<-as.data.frame.matrix(table(custdata$marital.stat,custdata$health.ins))
stats$marital.stat <- row.names(stats)
stats$ratio <- stats[,2]/stats[,1] * 10    #multply 10 is for visual effect
#stats$univ <- rep("Y",nrow(stats))

#Plot the chart
ggplot(custdata) + geom_bar(aes(x=marital.stat,fill=health.ins)) + geom_line(data=stats,aes(x=marital.stat,y=ratio,group=1))