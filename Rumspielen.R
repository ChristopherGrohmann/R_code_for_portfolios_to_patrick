
rm(list = ls())

######################################################################################################
#1. read in data from databases
library(RODBC)
library (data.table)

conn <- odbcConnect(dsn="Cronbach", uid="extern_root", pwd="hiwisskiera1")
transactions <- data.table(sqlQuery(conn, "SELECT * FROM remove_error2 LIMIT 100000")) #with limit atm
odbcClose(conn)

setkey(transactions,transaction_id)
transactions$date <- as.Date(as.character(transactions$date), "%Y-%m-%d %H:%M:%OS")

conn <- odbcConnect(dsn="Cronbach", uid="extern_root", pwd="hiwisskiera1")
instruments <- data.table(sqlQuery(conn, "SELECT * FROM instruments"))
odbcClose(conn)
setkey(instruments,instrument_id)

#secifying what time the transactions can be taken from
#transactions = transactions[date <= "2011-01-01",,]
transactions = transactions[date >= "2000-01-01",,]
transactions$value = transactions[,(amount*price*exchange_rate * (1 - flag))+ (flag*amount*exchange_rate)]

#####################################################################################################
#2. Join the data 


####################################################################################################
#3. Organizing it in an arrray

# transactions[,,keyby=user_id]
# hh = unique(transactions$user_id)
#Alternative
setkey(transactions, user_id)
hh = transactions[,.(anzahl_transactionen = 0),by=user_id]
# dates = unique(transactions$date)
# 
# portfolios = array(data = NA, dim = c(length(hh),1000,3,length(dates)))#, dimnames = list("hh","positions","attributes","date"))

# for (i in 1 : length(hh)){
#   portfolios[i,,,]=hh[i]
#   positions = transactions[hh[i],instrument_id_intern, by = instrument_id_intern]
#   for (j in 1 :lenght(positions)){
#     portfolios[i,j,,]=
#   }
# }
# 3. Create descriptive statistics:
#Using what we already used in MySQL
hh$anzahl_transactionen = transactions[,.(anzahl_transactionen =length(transaction_id)),by=user_id]$anzahl_transactionen
hh[transactions[,.(anzahl_transactionen = .N),by=user_id] anzahl_transactionen:=i.anzahl_transactionen, on ="user_id" ]
hh$anzahl_verkaufe = transactions[,.(anzahl_verkaufe = sum((tradetype=="V"), na.rm=TRUE)),by=user_id]$anzahl_verkaufe

hh$anteil_verkaufe = hh$anzahl_verkaufe/hh$anzahl_transactionen

hh$active_time = as.integer(transactions[,.(active_time = max(date)-min(date)),by=user_id]$active_time)

hh$yearly_transactions = hh$anzahl_transactionen/(hh$active_time/356)

hh$sum_transaction = transactions[,.(sum_transaction = sum(value) ),by=user_id]$sum_transaction

hh$avaerage_transaction = hh$sum_transaction/hh$anzahl_transactionen

hh$max_transaction = transactions[,.(max_transaction = max(value) ),by=user_id]$max_transaction

hh$sd_transactions_value = transactions[,.(sd_transactions_value = sd(value) ),by=user_id]$sd_transactions_value

hh$sd_by_average = hh$sd_transactions_value/hh$avaerage_transaction

hh$yearly_trunover = ifelse(hh$active_time/365>=1, hh$avaerage_transaction * hh$yearly_transactions, hh$sum_transaction)


####################################################################################################
#4. Have a criterium that checks for valid hh

hh <- hh[anzahl_transactionen > 3,,]
hh <- hh[anteil_verkaufe > 0,,]
hh <- hh[active_time >30,,]
hh <- hh[sd_by_average < 3]
hh <- hh[avaerage_transaction <=100000]
hh[,mean(avaerage_transaction)]
hist(hh$avaerage_transaction)
summary(hh$avaerage_transaction)
####################################################################################################
#Rumspielen
#Mit hhy anfangen
####################################################################################################
#Median guy

#Hightrader

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



allplot  <- hhy[ ,.(plotvalue = mean(ValueStock),(Category="Stock") ), by = year]
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



