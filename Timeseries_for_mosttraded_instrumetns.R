#timeseries for most traded instruments

bestinstr <- transactions[,.(number_of_trans = 0 ), by=.(instrument_id,date)]
bestinstr[transactions[,.(number_of_trans = length(transaction_id)),by = .(instrument_id,date)], number_of_trans:=i.number_of_trans, on = "instrument_id"]
bestinstr[transactions[,.(sumvalue = sum(value)),by = .(instrument_id,date)], sumvalue:=i.sumvalue, on = "instrument_id"]
bestinstr[transactions[,instrument_description,by = .(instrument_id,date)], instrument_description:=i.instrument_description, on = "instrument_id"]
bestinstr[bestinstr[,.(sumall = sum(sumvalue)),by =instrument_id], sumall:=i.sumall, on = "instrument_id"]
bestinstr = bestinstr[order(-sumall,date)]

write.csv(bestinstr[1:15000], file = "C:/Users/Grohmann/Documents/Interactive data/Descriptive/time_series_of_2000_stock_dates.csv")