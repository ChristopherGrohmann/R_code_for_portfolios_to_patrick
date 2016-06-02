plottimeseries_sellbuy_hh = function (x,y = "Sell and buy of hh over time"){
  allplot  <- x[ ,.(plotvalue = sum(Valuebuy),(Category="Buy") ), by = year]
  
  allplot <- merge(allplot, x[ ,.(plotvalue = sum(Valuesell),(Category="Sell") ), by = year]
                   ,by = c("year","V2", "plotvalue"), all = TRUE)
  pos <- allplot[plotvalue >= 0]
  neg <- allplot[plotvalue <0 ]
  ggplot()+
    geom_area( data = pos, aes(x= year,y= plotvalue, fill= V2), position = 'stack')+
    geom_area( data = neg, aes(x= year,y= plotvalue, fill= V2), position = 'stack')+
    aes(x= year, y= plotvalue, group = V2, fill= plotvalue )+ 
    ggtitle( y) +
    theme(plot.title = element_text(lineheight=.8, face="bold"))
}