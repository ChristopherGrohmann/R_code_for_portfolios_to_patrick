#Create a Backup, so I don't always have to read the libary.
#hhy_backup <- hhy
hhy <- hhy_backup
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
  window()
  plottimeseries_assetclass(hhy_hightrade, "High trader")
  plottimeseries_assetclass(hhy_lowtrade, "Low trader")
  plottimeseries_assetclass(hhy_medium, "Medium trader")
}

#Midtrader

#Lowtrader

#Over all time series
x <- quantile (hhy[,anzahl_transactionen], probs = seq(0,1,0.2))
hhy_hightrade <- hhy[anzahl_transactionen >= x[5]]
hhy_lowtrade <- hhy[anzahl_transactionen <= x[2]]
hhy_medium <- hhy[anzahl_transactionen > x[2]& anzahl_transactionen < x[5]]

window()
plottimeseries_assetclass(hhy_hightrade,"High trader")
plottimeseries_assetclass(hhy_lowtrade, "Low trader")
plottimeseries_assetclass(hhy_medium, "Medium trader")

