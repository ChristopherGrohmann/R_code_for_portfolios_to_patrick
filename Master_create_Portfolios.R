#This is the master fiel for building the portfolios
#There are different steps to come to the Portfolio
#1. Read in Data from the Database
#2. Join the different files 'transactions' and 'instruments' # brauche ich eigentlich gar nicht
#3. Create descriptive statistics for hh
#4. Have criterium that checks for valid hh
#5. create a multidimensional array that connects each individual hh to a portfolio to time
#6. create descriptive statistics to compare hh portfolios
rm(list = ls())

######################################################################################################
#1. read in data from databases
library(RODBC)
library (data.table)

conn <- odbcConnect(dsn="Cronbach", uid="extern_root", pwd="hiwisskiera1")
transactions <- data.table(sqlQuery(conn, "SELECT * FROM remove_error2  LIMIT 100000")) #with limit atm
odbcClose(conn)

setkey(transactions,transaction_id)
transactions$date <- as.Date(as.character(transactions$date), "%Y-%m-%d %H:%M:%OS")

conn <- odbcConnect(dsn="Cronbach", uid="extern_root", pwd="hiwisskiera1")
instruments <- data.table(sqlQuery(conn, "SELECT * FROM instruments"))
odbcClose(conn)
setkey(instruments,instrument_id)

#secifying what time the transactions can be taken from
transactions = transactions[date <= "2011-01-01",,]
transactions = transactions[date >= "2000-01-01",,]
transactions$value = transactions[,amount*price*exchange_rate*(1-flag)]

#####################################################################################################
#2. Join the data 


####################################################################################################
#3. Organizing it in an arrray

# transactions[,,keyby=user_id]
# hh = unique(transactions$user_id)
#Alternative
setkey(transactions, user_id)
hh = transactions[,user_id,by=user_id]
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
#####################################################################################################
#5. Join hh to transactions to Instruments

#Firtst inner join of hh and transactions, to reduce the file.

setkey(hh, user_id)
setkey(transactions, user_id)
transactions <- transactions[hh, nomatch=0]

setkey(transactions, instrument_id_intern)
setkey(instruments, instrument_id)
transactions <- transactions[instruments, nomatch=0]

a= c("Stock", "Bond", "Fund", "Warrents", "Certificate", "Other")
setkey(transactions, user_id)
hh$Stock = transactions[,.(Stock=sum((instrument_type==1 ), na.rm=TRUE)),by=user_id]$Stock
hh$Bond = transactions[,.(Bond=sum((instrument_type==2 ), na.rm=TRUE)),by=user_id]$Bond
hh$Fund = transactions[,.(Fund=sum((instrument_type==7 ), na.rm=TRUE)),by=user_id]$Fund
hh$Warrents = transactions[,.(Warrents=sum((instrument_type==3 ), na.rm=TRUE)),by=user_id]$Warrents
hh$Certificate = transactions[,.(Certificate=sum((instrument_type== 13), na.rm=TRUE)),by=user_id]$Certificate
hh$Other = hh$anzahl_transactionen-hh$Stock -hh$Bond-hh$Fund-hh$Warrents- hh$Certificate
#percentages
hh$Stockper = hh$Stock/hh$anzahl_transactionen
hh$Bondper = hh$Bond/hh$anzahl_transactionen
hh$Fundper = hh$Fund/hh$anzahl_transactionen
hh$Warrentsper = hh$Warrents/hh$anzahl_transactionen
hh$Certificateper = hh$Certificate/hh$anzahl_transactionen
hh$Otherper = hh$Other/hh$anzahl_transactionen

#By year, kann ich noch machen
hhy = transactions[,user_id,by=.(user_id,year= year(date))]

setkey(hh, user_id)
setkey(hhy, user_id)
hhy <- hhy[hh, nomatch=0]

setkey(transactions, user_id)
hhy$anzahl_transactionen_y = transactions[,.(anzahl_transactionen =length(transaction_id)),by=.(user_id,year= year(date))]$anzahl_transactionen
hhy$Stock = transactions[,.(Stock=sum((instrument_type==1 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Stock
hhy$Bond = transactions[,.(Bond=sum((instrument_type==2 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Bond
hhy$Fund = transactions[,.(Fund=sum((instrument_type==7 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Fund
hhy$Warrents = transactions[,.(Warrents=sum((instrument_type==3 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Warrents
hhy$Certificate = transactions[,.(Certificate=sum((instrument_type== 13), na.rm=TRUE)),by=.(user_id,year= year(date))]$Certificate
hhy$Other = hhy$anzahl_transactionen_y-hhy$Stock -hhy$Bond-hhy$Fund-hhy$Warrents- hhy$Certificate

hhy$Stockper = hhy$Stock/hhy$anzahl_transactionen_y
hhy$Bondper = hhy$Bond/hhy$anzahl_transactionen_y
hhy$Fundper =hhy$Fund/hhy$anzahl_transactionen_y
hhy$Warrentsper = hhy$Warrents/hhy$anzahl_transactionen_y
hhy$Certificateper = hhy$Certificate/hhy$anzahl_transactionen_y
hhy$Otherper=hhy$Other/hhy$anzahl_transactionen_y
  
#Wih value
#hhy$ValueStock = transactions[instrument_type==7,.(ValueStock=sum(value) ),by=.(user_id,year= year(date))]$ValueStock



####################################################################################################
#6 Descriptive statistics and plots
library(ggplot2)

#Pie chart of transactions at a single point in time
pie(c(mean(hh$Stockper),mean(hh$Bondper),mean(hh$Fundper), mean(hh$Warrentsper),mean(hh$Certificateper),mean(hh$Otherper)),
    labels = a,
    main="Percentage of transactions in Category 2000 - 2011")


#Stacked Area chart of transactions in categories over time
#
d=data.frame(t= rep(as.numeric(unique(hhy$year)), each = 6))
d$category = rep(a,12)
d = d[order(d$t),] 

for ( i in 1:12){
  d$meanvalue[(i-1)*6+1] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Stockper))]$x
  d$meanvalue[(i-1)*6+2] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Bondper))]$x 
  d$meanvalue[(i-1)*6+3] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Fundper))]$x
  d$meanvalue[(i-1)*6+4] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Warrentsper))]$x
  d$meanvalue[(i-1)*6+5] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Certificateper))]$x
  d$meanvalue[(i-1)*6+6] = hhy[year == noquote(paste(d$t[i])),.(x = mean(Otherper))]$x
}

ggplot(d, aes(x= t, y = meanvalue, group= category, fill=meanvalue)) + geom_area(position="fill")

ggplot(hhy, aes(x= hhy$year, y = c(mean(hhy$Stock),mean(hhy$Bond)) ,group= hhy$year,fill=var)) + geom_area(position="fill")






