#Create a Backup, so I don't always have to read the libary.
hhy_backup <- hhy
#hhy <- hhy_backup

library(ggplot2)
####################################################################################################
#Rumspielen
#Mit hhy anfangen
####################################################################################################
#Median guy

#Hightrader
for (i in 2000 : 2014){
  x <- quantile (hhy[year == i  ,anzahl_transactionen], probs = seq(0,1,0.2))
  hhy_hightrade <- hhy[year == i| year == i+1 & anzahl_transactionen >= x[5]]
  hhy_lowtrade <- hhy[year == i| year == i+1 & anzahl_transactionen <= x[2]]
  hhy_medium <- hhy[year == i| year == i+1 & anzahl_transactionen > x[2]& anzahl_transactionen < x[5]]
  
  
  windows()
  par(mfrow = c(3,1))
  plot(hhy_hightrade$Valuebuy)
  plot(hhy_lowtrade$Valuebuy)
  plot(hhy_medium$Valuebuy)
}

#Over all time series
x <- quantile (hhy[,anzahl_transactionen], probs = seq(0,1,0.2))
x <- quantile (hhy[,yearly_transactions], probs = seq(0,1,0.2))

hhy_hightrade <- hhy[yearly_transactions >= x[5]]
hhy_lowtrade <- hhy[yearly_transactions <= x[2]]
hhy_medium <- hhy[yearly_transactions > x[2]& anzahl_transactionen < x[5]]

windows()
plottimeseries_assetclass(hhy_hightrade,"High trader")
windows()
plottimeseries_assetclass(hhy_lowtrade, "Low trader")
windows()
plottimeseries_assetclass(hhy_medium, "Medium trader")
windows()
plottimeseries_sellbuy_hh(hhy_hightrade,"High trader")
windows()
plottimeseries_sellbuy_hh(hhy_lowtrade,"Low trader")
windows()
plottimeseries_sellbuy_hh(hhy_medium,"Medium trader")

#mal nur die high trader:
summary(hhy_hightrade$active_time)
hist(hhy_hightrade$active_time)

hist(hhy_hightrade$anzahl_transactionen)
summary(hhy_hightrade$anzahl_transactionen)

hhy_100Top <- hhy[order(- rank(yearly_transactions))]
hhy_100Top <- hhy_100Top[1:100]

windows()
plottimeseries_sellbuy_hh(hhy_100Top,"100 most Trader")
windows()
plottimeseries_assetclass(hhy_100Top, "100 most Trader")
summary(hhy_100Top$active_time)
hist(hhy_100Top$active_time)


