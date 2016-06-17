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

setkey(transactions, user_id)
hh = transactions[,.(anzahl_transactionen = 0),by=user_id]

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

transactions$amount[transactions$tradetype == "V"] <- -transactions$amount[transactions$tradetype == "V"]

#####################################################################################################
#5. Join hh to transactions to Instruments

#Firtst inner join of hh and transactions, to reduce the file.

setkey(hh, user_id)
setkey(transactions, user_id)
transactions <- transactions[hh, nomatch=0]

setkey(transactions, instrument_id_intern)
setkey(instruments, instrument_id)
transactions <- transactions[instruments, nomatch=0]

######################################################################################################
#6. Create portfolios with simultaneous actions in the portfolios

portfolios <- generate.portfolios(actions = transactions)

a= c("Stock", "Bond", "Fund", "Warrent", "Certificate", "Other")
setkey(transactions, user_id)
hh$Stock = transactions[,.(Stock=sum((instrument_type==1 ), na.rm=TRUE)),by=user_id]$Stock
hh$Bond = transactions[,.(Bond=sum((instrument_type==2 ), na.rm=TRUE)),by=user_id]$Bond
hh$Fund = transactions[,.(Fund=sum((instrument_type==7 ), na.rm=TRUE)),by=user_id]$Fund
hh$Warrent = transactions[,.(Warrent=sum((instrument_type==4 ), na.rm=TRUE)),by=user_id]$Warrent
hh$Certificate = transactions[,.(Certificate=sum((instrument_type== 13), na.rm=TRUE)),by=user_id]$Certificate
hh$Other = hh$anzahl_transactionen-hh$Stock -hh$Bond-hh$Fund-hh$Warrent- hh$Certificate
#percentages
hh$Stockper = hh$Stock/hh$anzahl_transactionen
hh$Bondper = hh$Bond/hh$anzahl_transactionen
hh$Fundper = hh$Fund/hh$anzahl_transactionen
hh$Warrentper = hh$Warrent/hh$anzahl_transactionen
hh$Certificateper = hh$Certificate/hh$anzahl_transactionen
hh$Otherper = hh$Other/hh$anzahl_transactionen

c(mean(hh$Stockper),mean(hh$Bondper),mean(hh$Fundper), mean(hh$Warrentper),mean(hh$Certificateper)
  ,mean(hh$Otherper),mean(hh$yearly_transactions))
c(sd(hh$Stockper),sd(hh$Bondper),sd(hh$Fundper), sd(hh$Warrentper),sd(hh$Certificateper)
  ,sd(hh$Otherper),sd(hh$yearly_transactions))
########################################################################
#By year, kann ich noch machen
hhy <- transactions[,.(anzahl_transactionen_y=0),by=.(user_id,year= year(date))]

setkey(hh, user_id)
setkey(hhy, user_id)
hhy <- hhy[hh, nomatch=0]

setkey(transactions, user_id)
hhy$anzahl_transactionen_y = transactions[,.(anzahl_transactionen =length(transaction_id)),by=.(user_id,year= year(date))]$anzahl_transactionen
hhy[transactions[,.(value_y =sum(value)),by=.(user_id,year= year(date))],value_y :=i.value_y, on = c(user_id = "user_id",year= "year")]
hhy$Stock = transactions[,.(Stock=sum((instrument_type==1 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Stock
hhy$Bond = transactions[,.(Bond=sum((instrument_type==2 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Bond
hhy$Fund = transactions[,.(Fund=sum((instrument_type==7 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Fund
hhy$Warrent = transactions[,.(Warrent=sum((instrument_type==4 ), na.rm=TRUE)),by=.(user_id,year= year(date))]$Warrent
hhy$Certificate = transactions[,.(Certificate=sum((instrument_type== 13), na.rm=TRUE)),by=.(user_id,year= year(date))]$Certificate
hhy$Other = hhy$anzahl_transactionen_y-hhy$Stock -hhy$Bond-hhy$Fund-hhy$Warrent- hhy$Certificate

hhy$Stockper = hhy$Stock/hhy$anzahl_transactionen_y
hhy$Bondper = hhy$Bond/hhy$anzahl_transactionen_y
hhy$Fundper =hhy$Fund/hhy$anzahl_transactionen_y
hhy$Warrentper = hhy$Warrent/hhy$anzahl_transactionen_y
hhy$Certificateper = hhy$Certificate/hhy$anzahl_transactionen_y
hhy$Otherper=hhy$Other/hhy$anzahl_transactionen_y

#######################################################################  
#Wih value
hhy$ValueStock <- hhy[,.(ValueStock =0)]
hhy[transactions[instrument_type==1,.(ValueStock = sum(value)), by=.(user_id,year= year(date))]
    ,ValueStock:= i.ValueStock, on = c(user_id = "user_id",year= "year")]
hhy$ValueBond <- hhy[,.(ValueBond =0)]
hhy[transactions[instrument_type==2,.(ValueBond = sum(value)), by=.(user_id,year= year(date))]
    ,ValueBond:= i.ValueBond, on = c(user_id = "user_id",year= "year")]
hhy$ValueFund <- hhy[,.(ValueFund =0)]
hhy[transactions[instrument_type==7,.(ValueFund = sum(value)), by=.(user_id,year= year(date))]
    ,ValueFund:= i.ValueFund, on = c(user_id = "user_id",year= "year")]
hhy$ValueWarrent <- hhy[,.(ValueWarrent =0)]
hhy[transactions[instrument_type==4,.(ValueWarrent = sum(value)), by=.(user_id,year= year(date))]
    ,ValueWarrent:= i.ValueWarrent, on = c(user_id = "user_id",year= "year")]
hhy$ValueCertificate <- hhy[,.(ValueCertificate =0)]
hhy[transactions[instrument_type==2,.(ValueCertificate = sum(value)), by=.(user_id,year= year(date))]
    ,ValueCertificate:= i.ValueCertificate, on = c(user_id = "user_id",year= "year")]
hhy$ValueOther <- hhy[,.(ValueOther =0)]
hhy$ValueOther <- hhy$value_y- hhy$ValueStock - hhy$ValueBond - hhy$ValueFund - hhy$ValueWarrent -hhy$ValueCertificate
#######################################################################
#Adding selles and buyes
hhy[transactions[tradetype== "K",.(Valuebuy = sum(value)), by=.(user_id,year= year(date))]
    ,Valuebuy:= i.Valuebuy, on = c(user_id = "user_id",year= "year")]

hhy[transactions[tradetype== "V",.(Valuesell = -sum(value)), by=.(user_id,year= year(date))]
    ,Valuesell:= i.Valuesell, on = c(user_id = "user_id",year= "year")]
#Knallhart!
hhy[is.na(hhy)] <- 0

#######################################################################
#Descriptive Statistics about values
hhy = hhy[value_y != 0]

hhy$ValueStockper = hhy$ValueStock/hhy$value_y
hhy$ValueBondper = hhy$ValueBond/hhy$value_y
hhy$ValueFundper =hhy$ValueFund/hhy$value_y
hhy$ValueWarrentper = hhy$ValueWarrent/hhy$value_y
hhy$ValueCertificateper = hhy$ValueCertificate/hhy$value_y
hhy$ValueOtherper=hhy$ValueOther/hhy$value_y

c( mean(hhy[,.(x = mean(ValueStockper)), by = user_id]$x),
   mean(hhy[,.(x = mean(ValueBondper)), by = user_id]$x),
   mean(hhy[,.(x = mean(ValueFundper)), by = user_id]$x),
   mean(hhy[,.(x = mean(ValueWarrentper)), by = user_id]$x),
   mean(hhy[,.(x = mean(ValueCertificateper)), by = user_id]$x),
   mean(hhy[,.(x = mean(ValueOtherper)), by = user_id]$x))

c( sd(hhy[,.(x = mean(ValueStockper)), by = user_id]$x),
   sd(hhy[,.(x = mean(ValueBondper)), by = user_id]$x),
   sd(hhy[,.(x = mean(ValueFundper)), by = user_id]$x),
   sd(hhy[,.(x = mean(ValueWarrentper)), by = user_id]$x),
   sd(hhy[,.(x = mean(ValueCertificateper)), by = user_id]$x),
   sd(hhy[,.(x = mean(ValueOtherper)), by = user_id]$x))

#######################################################################
#estimating average portfolio size:
hhy$add_per_year = hhy$avaerage_transaction*hhy$yearly_transactions*(1-hhy$anteil_verkaufe)
mean(hhy[,.(x = mean(add_per_year)), by = user_id]$x)
median(hhy[,.(x = mean(add_per_year)), by = user_id]$x)

hhy$sell_per_year = hhy$avaerage_transaction*hhy$yearly_transactions*(hhy$anteil_verkaufe)
mean(hhy[,.(x = mean(sell_per_year)), by = user_id]$x)
median(hhy[,.(x = mean(sell_per_year)), by = user_id]$x)

####################################################################################################
#6 Descriptive statistics and plots
library(ggplot2)

#Pie chart of transactions at a single point in time

pie(c(mean(hh$Stockper),mean(hh$Bondper),mean(hh$Fundper), mean(hh$Warrentper),mean(hh$Certificateper),mean(hh$Otherper)),
    labels = a,
    main="Percentage of transactions in category 2000 - 2011: semi-real portfolios")
pie(c(54.62,3.41,32.48,3.02,4.94,1.54),
    labels = a,
    main="Percentage of asset allocation in category 2000 - 2011: external data")
#Stacked Area chart of transactions in categories over time
#

allplot  <- hhy[ ,.(plotvalue = mean(Stock),(Category="Stock") ), by = year]
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(Bond),(Category="Bond") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(Fund),(Category="Fund") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(Warrent),(Category="Warrent") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(Certificate),(Category="Certificate") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- merge(allplot, hhy[ ,.(plotvalue = mean(Other),(Category="Other") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)
allplot <- allplot[order(Category = c("Stock", "Bond", "Fund", "Warrent", "Certificate", "Other"))]

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(allplot, aes(x= year, y= plotvalue, group = V2, fill= plotvalue )) + geom_area(aes(fill= V2), position = 'stack') +
  ggtitle( "Number of transactions in assetclass over time")


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

