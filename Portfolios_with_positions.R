#Simple example for getting portfolios
library (data.table)

actions <- data.table(user_id = c("Carl","Carl","Carl","Carl","Carl","Lisa","Lisa","Moe"),
                     date = c(1,2,2,3,4,2,3,2 ),
                     action_type = c("B","B","B","S","S","B","S","B"),
                     instrument_id_intern = c("1","2","1","1","2","2","2","1"),
                     amount= c(10,20,5,15,20,40,40,50)) 
                     
actions$date <- as.Date(actions$date, origin="1970-01-01")
###desired Result
portfolios <- data.table(user_id = c("Carl","Carl","Carl","Carl","Lisa","Moe","Moe","Moe"),
                         date = c(1,2,2,3,2,2,3,4 ),
                         position = c("X","X","Y","Y","Y","X","X","X"),
                         amount= c(10,15,20,20,40,50,50,50) )

#Changeing amounts to negative amounts for "S"ell
actions$amount[actions$action_type == "S"] <- -actions$amount[actions$action_type == "S"]

#pseudo code

#for users
  #for date
    #position(i,j) = position (i,j-1)
    #if action (i,j)
      #if action(i,j) is in  position(i,j)
        #position.amount = position.amount +action.amount
      #else
        #add a new row 
        #position(i,j) = action(i,j)
      #portfolios[user_id == i & date == j ] <- portfolios[user_id == i & date == j &  amount > 0]

#ist das so verst‰ndlich?
#Habe ich da was groﬂes vergessen?
actions <- transactions
#Don't throw away "thank you"s
actions$amount[actions$action_type == "S"] <- -actions$amount[actions$action_type == "S"]

hh <- actions[,.(mean(amount)) , by = c("user_id")]


generate.portfolios(actions = actions,hh =hh)

generate.portfolios= function(actions,churntime=720){
  #No does not stack the existing positions
  actions$user_id <- as.character(actions$user_id)
  setkey(actions,user_id,date,instrument_id_intern)
  #to create a empty portfolios, that has the right data configuartions in each variable
  portfolios <- actions[1,.(user_id,date, instrument_id_intern, amount , isnegative =1)]
  portfolios <- portfolios[isnegative != 1]
  
  hh <- actions[,.(v1=0) , by = c("user_id")]
  list <- hh$user_id
  for (k in 1: length(list)){
    i <- list[k]
    #don't start from the earliest date, but the earliest the user has a transaction
    # min <- as.double(actions[user_id==i, .(min(date ))])
    # max <- as.double(actions[user_id==i, .(max(date ))])
    #list, when the user has transactions
    tage <- actions[actions$user_id== i,(v1=0), by= date]
    actiondate <- as.numeric(tage$date)
    
    for (l in 1:length(actiondate)){    #min:(max+churntime)){   
      #churndate is the extra time after the last transaction, until the user is decided to have churned
      j <- actiondate[l]
      if (l != 1){ #This is the normal case, there exist already positioins for one user. 
        date_before <-  actiondate[l-1]
        portfolios <- rbindlist(list (portfolios,
                                      portfolios[user_id == i & date == date_before,
                                                 .( user_id, date = as.Date(j, origin="1970-01-01"), instrument_id_intern, amount, isnegative )]))
        #got through all positions seperatly, but sum them up, so there is in the end one position per several transaction per day per user per instrument.
        actions_at_time_dt <- actions[user_id == i & date == j,(v1=0),by= instrument_id_intern]
        actions_at_time <- as.numeric(actions_at_time_dt$instrument_id_intern)
        for (m in 1:length(actions_at_time)){
          x <- actions_at_time[m]
          if (x %in% portfolios[user_id ==i & date==j, instrument_id_intern]){
            portfolios[user_id ==i & date==j & instrument_id_intern == x, amount:= 
                         portfolios[user_id ==i & date==j & instrument_id_intern == x, amount]+
                         actions[user_id == i & date == j & instrument_id_intern == x,.(amount = sum(amount))]]
          }else{
            #Create a new row, that has the transaction
            portfolios <- rbindlist(list (portfolios,
                                          actions[user_id == i & date == j & instrument_id_intern == x ,
                                                  .(user_id,date, instrument_id_intern, amount = sum(amount) , isnegative =0)]))
          }
        }
        #those postitions that have zero amount should drop out
        portfolios[amount <=0 &  user_id ==i &  date==j, isnegative := 1]
        portfolios <- portfolios[isnegative != 1]
      }else{ #This is in the case it is the first time a user has traded. His first positions will have to be added to the data.tabel
        actions_at_time_dt <- actions[user_id == i & date == j,(v1=0),by= instrument_id_intern]
        actions_at_time <- as.numeric(actions_at_time_dt$instrument_id_intern)
        for (m in 1:length(actions_at_time)){
          x <- actions_at_time[m]
          portfolios <- rbindlist(list (portfolios,
                                        actions[user_id == i & date == j & instrument_id_intern == x,
                                                .(user_id,date, instrument_id_intern, amount = sum(amount) , isnegative =0)]))
        }
      }
      
    }
  }
  return(portfolios)
}

