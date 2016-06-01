plottimeseries_assetclass = function (x,y = "Volume of transactions in asset class over time"){
  allplot  <- x[ ,.(plotvalue = mean(ValueStock),(Category="Stock") ), by = year]
  allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueBond),(Category="Bond") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueFund),(Category="Fund") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueWarrent),(Category="Warrent") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueCertificate),(Category="Certificate") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueOther),(Category="Other") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  ggplot(allplot, aes(x= year, y= plotvalue, group = V2, fill= plotvalue )) + geom_area(aes(fill= V2), position = 'stack') +
    ggtitle( y) +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}