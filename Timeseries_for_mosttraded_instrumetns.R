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
#vergleich <- read.csv("C:/Users/cgrohmann/Desktop/Daten_Volume_Zeitreihe_Dax.csv",stringsAsFactors=FALSE)
vergleich = as.data.table(vergleich)
########################################################################################################
#Vergleich von unseren Daten mit real-world-data

posdates = bestinstr[instrument_description=="DEUTSCHE BANK AG", date]
bestinstr= merge(bestinstr,vergleich[,.(date = as.Date(X.NAME. , "%d.%m.%Y"),
                                        from="External-data",
                                        sumvalue = DEUTSCHE.BANK.AG,
                                        instrument_description = "DEUTSCHE BANK AG"
)],
by = c("date","sumvalue", "from", "instrument_description"), all = TRUE)
ggplot(bestinstr[instrument_description=="DEUTSCHE BANK AG"], aes(x = date,y= sumvalue)) +
  facet_grid(from~., scale="free") +    
  geom_line(stat = "identity") + xlab("year") + ylab("Tradevolume")+
  ggtitle( "Volume of transactions daily")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))

bestinstr$sumvalue[is.na(bestinstr$sumvalue)] <- 0

posdates <- merge(bestinstr[instrument_description=="DEUTSCHE BANK AG" & from =="External-data",.(date,sumvalue)],
                  bestinstr[instrument_description=="DEUTSCHE BANK AG" & from =="Semi-real-portfolio",.(date,sumvalue)],
                  by = "date")

y <- posdates$sumvalue.x
x <- posdates$sumvalue.y
simplemodel <- lm( y~x)
summary(simplemodel)
cor(x,y)

bestinstr= merge(bestinstr,vergleich[,.(date = as.Date(X.NAME. , "%d.%m.%Y"),
                                        from="External-data",
                                        sumvalue = DEUTSCHE.TELEKOM.AG,
                                        instrument_description = "DEUTSCHE TELEKOM AG"
)],
by = c("date","sumvalue", "from", "instrument_description"), all = TRUE)
ggplot(bestinstr[instrument_description=="DEUTSCHE TELEKOM AG"], aes(x = date,y= sumvalue)) +
  facet_grid(from~., scale="free") +    
  geom_line(stat = "identity") + xlab("year") + ylab("Tradevolume")+
  ggtitle( "Volume of transactions daily")+
  theme(plot.title = element_text(lineheight=.8, face="bold"))
bestinstr$sumvalue[is.na(bestinstr$sumvalue)] <- 0

posdates <- merge(bestinstr[instrument_description=="DEUTSCHE TELEKOM AG" & from =="External-data",.(date,sumvalue)],
                  bestinstr[instrument_description=="DEUTSCHE TELEKOM AG" & from =="Semi-real-portfolio",.(date,sumvalue)],
                  by = "date")
bestinstr$sumvalue[is.na(bestinstr$sumvalue)] <- 0
y <- posdates$sumvalue.x
x <- posdates$sumvalue.y
simplemodel <- lm( y~x)
summary(simplemodel)
cor(x,y)

