#Simple example for getting portfolios
library (data.table)

actions <- data.table(user_id = c("Carl","Carl","Carl","Carl","Carl","Lisa","Lisa","Moe"),
                     date = c(1,2,2,3,4,2,3,2 ),
                     action_type = c("B","B","B","S","S","B","S","B"),
                     instrument_id_intern = c("X","Y","X","X","Y","Y","Y","X"),
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

#Don't throw away "thank you"s
actions$amount[actions$action_type == "S"] <- -actions$amount[actions$action_type == "S"]

hh <- actions[,.(mean(amount)) , by = c("user_id")]


generate.portfolios(actions = actions,hh =hh)

generate.portfolios= function(actions,churntime=720){
  #No does not stack the existing positions
  actions$user_id <- as.character(actions$user_id)
  portfolios <- actions[,.(date= min(date)-1, instrument_id_intern=mean(instrument_id_intern), amount = mean(amount),isnegative = 1), by = c("user_id")]
  hh <- actions[,.(mean(amount)) , by = c("user_id")]
  
  #Try the whole thing with loops
  list <- hh$user_id
  
  for (k in 1: length(list)){
    i <- list[k]
    #don't start from the earliest date, but the earliest the user has a transaction
    min <- as.double(actions[user_id==i, .(min(date ))])
    max <- as.double(actions[user_id==i, .(max(date ))])
    #list, when the user has transactions
    actiondate <- as.list(actions$date[actions$user_id== i])
    
    for (j in min:(max+churntime)){#churndate is the extra time after the last transaction, until the user is decided to have churned
      portfolios <- rbindlist(list (portfolios, portfolios[user_id == i & date == j - 1,.( user_id, date = as.Date(j, origin="1970-01-01"), instrument_id_intern, amount, isnegative )]))
      if ( j %in% actiondate){
        if (actions[user_id == i & date == j, instrument_id_intern] %in% portfolios[user_id ==i & date==j, instrument_id_intern]){
          x<- actions$instrument_id_intern[actions$user_id == i & actions$date == j]
          portfolios$amount[portfolios$user_id ==i & portfolios$date==j & portfolios$instrument_id_intern == x] <- portfolios$amount[portfolios$user_id ==i & portfolios$date==j & portfolios$instrument_id_intern == x] + actions$amount[actions$user_id == i & actions$date == j]
          
        }else{
          #Create a new row, that has the transaction
          portfolios <- rbindlist(list (portfolios,actions[user_id == i & date == j ,.(user_id,date, instrument_id_intern, amount, isnegative =0)]))
        }
        #those postitions that have zero amount should drop out
        portfolios$isnegative[portfolios$amount <=0 &  portfolios$user_id ==i &  portfolios$date==j] <- 1
        portfolios <- portfolios[isnegative != 1]
      }
    }
  }
  return(portfolios)
}

