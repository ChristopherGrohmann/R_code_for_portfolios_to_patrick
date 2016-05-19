#timeseries for most traded instruments

bestinstr <- transactions[,.(number_of_trans = as.integer(0) ), by=.(instrument_id,date)]

bestinstr[transactions[,.(number_of_trans = .N),by = .(instrument_id,date)]
          , number_of_trans:=i.number_of_trans, on = c(instrument_id = "instrument_id",date= "date")]

bestinstr[transactions[,.(sumvalue = sum(value)),by = .(instrument_id,date)]
          , sumvalue:=i.sumvalue, on = c(instrument_id = "instrument_id",date= "date")]

bestinstr[transactions[,instrument_description,by = .(instrument_id,date)]
          , instrument_description:=i.instrument_description, on = c(instrument_id = "instrument_id",date= "date")]

bestinstr[bestinstr[,.(sumall = sum(sumvalue)),by =instrument_id],
          sumall:=i.sumall, on = c(instrument_id = "instrument_id")]

bestinstr = bestinstr[order(-sumall,date)]

bestinstr[instrument_id == 1937897,.(plot(sumvalue))]

write.csv(bestinstr[1:15000], file = "C:/Users/Grohmann/Documents/Interactive data/Descriptive/time_series_of_2000_stock_dates.csv")
on = c(instrument_id = "instrument_id",date= "date")

ggplot(yt.views, aes(Date, Views)) + geom_line() +
  scale_x_date(format = "%b-%Y") + xlab("") + ylab("Daily Views")