#timeseries for most traded instruments
library(ggplot2)

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
bestinstr$from <- "Semi-real-portfolio"

vergleich <- read.csv("C:/Users/Grohmann/Documents/Interactive data/Vergleichsdaten_Bloomberg_co/Daten_Volume_Zeitreihe_Dax.csv",stringsAsFactors=FALSE)
vergleich = as.data.table(vergleich)
########################################################################################################
#Vergleich von unseren Daten mit real-world-data

bestinstr= merge(bestinstr,vergleich[,.(date = as.Date(X.NAME. , "%d.%m.%Y"),
                                        from="Real-world-data",
                                        sumvalue = DEUTSCHE.BANK.AG,
                                        instrument_description = "DEUTSCHE BANK AG"
                                        )],
                 by = c("date","sumvalue", "from", "instrument_description"), all = TRUE)
ggplot(bestinstr[instrument_description=="DEUTSCHE BANK AG"], aes(x = date,y= sumvalue)) +
  facet_grid(from~., scale="free") +    
  geom_line(stat = "identity") + xlab("year") + ylab("tradeVolume")

bestinstr= merge(bestinstr,vergleich[,.(date = as.Date(X.NAME. , "%d.%m.%Y"),
                                        from="Real-world-data",
                                        sumvalue = DEUTSCHE.TELEKOM.AG,
                                        instrument_description = "DEUTSCHE TELEKOM AG"
                                        )],
by = c("date","sumvalue", "from", "instrument_description"), all = TRUE)
ggplot(bestinstr[instrument_description=="DEUTSCHE TELEKOM AG"], aes(x = date,y= sumvalue)) +
  facet_grid(from~., scale="free") +    
  geom_line(stat = "identity") + xlab("year") + ylab("tradeVolume")


