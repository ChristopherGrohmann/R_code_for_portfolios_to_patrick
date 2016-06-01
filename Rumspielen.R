#Create a Backup, so I don't always have to read the libary.
#hhy_backup <- hhy
hhy <- hhy_backup
####################################################################################################
#Rumspielen
#Mit hhy anfangen
####################################################################################################
#Median guy

#Hightrader

hist(hhy[year== 2010 ,anzahl_transactionen])
summary(hhy[year==2010 ,anzahl_transactionen])
hhy <- hhy[year==2010 & anzahl_transactionen >= 129.0 ,]

#Midtrader

#Lowtrader


###################
#plot the whole thing
###################
library(ggplot2)
pie(c(mean(hh$Stockper),mean(hh$Bondper),mean(hh$Fundper), mean(hh$Warrentper),mean(hh$Certificateper),mean(hh$Otherper)),
    labels = a,
    main="Percentage of transactions in category 2000 - 2011: semi-real portfolios")
pie(c(54.62,3.41,32.48,3.02,4.94,1.54),
    labels = a,
    main="Percentage of asset allocation in category 2000 - 2011: external data")
#Stacked Area chart of transactions in categories over time
#
#



allplot  <- hhy[ ,.(plotvalue = mean(ValueStock),(Category="Stock") )]
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(ValueBond),(Category="Bond") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(ValueFund),(Category="Fund") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(ValueWarrent),(Category="Warrent") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(ValueCertificate),(Category="Certificate") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(ValueOther),(Category="Other") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
ggplot(allplot, aes(x= year, y= plotvalue, group = V2, fill= plotvalue )) + geom_area(aes(fill= V2), position = 'stack') +
  ggtitle( "Volume of transactions in asset class over time") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))



