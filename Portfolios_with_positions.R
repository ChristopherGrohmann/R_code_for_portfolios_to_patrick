#Simple example for getting portfolios
library (data.table)

actions <- data.table(User_id = c("Carl","Carl","Carl","Carl","Lisa","Lisa","Moe"),
                     time = c(1,2,3,4,2,3,2 ),
                     action_type = c("B","B","S","S","B","S","B"),
                     instrument = c("X","Y","X","Y","Y","Y","X"),
                     value= c(10,20,10,20,40,40,50) )

###desired Result
portfolios <- data.table(User_id = c("Carl","Carl","Carl","Carl","Lisa","Moe","Moe","Moe"),
                         time = c(1,2,2,3,2,2,3,4 ),
                         position = c("X","X","Y","Y","Y","X","X","X"),
                         value= c(10,10,20,20,40,50,50,50) )

#Changeing values to negative values for "S"ell
actions$value[actions$action_type == "S"] <- -actions$value[actions$action_type == "S"]

#pseudo code

#for users
  #for time
    #position(i,j) = position (i,j-1)
    #if action (i,j)
      #if action(i,j) is in  position(i,j)
        #position.value = position.value +action.value
      #else
        #add a new row 
        #position(i,j) = action(i,j)
      #portfolios[User_id == i & time == j ] <- portfolios[User_id == i & time == j &  value > 0]

#ist das so verst‰ndlich?
#Habe ich da was groﬂes vergessen?

#Don't throw away "thank you"s
actions$value[actions$action_type == "S"] <- -actions$value[actions$action_type == "S"]
#No does not stack the existing positions
portfolios <- actions[,.(time= 0, instrument=0, value = 0,isnegative = 1), by = c("User_id")]

#Try the whole thing with loops
hh <- actions[,.(mean(value)) , by = c("User_id")]
list <- as.list (hh$User_id)

for (k in 1: length(list)){
  i <- list[k]
  min <- as.double(actions[User_id==i, .(min(time ))])
  
  actiontime <- as.list(actions$time[actions$User_id== i])
  for (j in min:4){
    #portfolios[User_id == i & time == j] <- portfolios[User_id == i & time == j - 1,.( User_id, time=j, instrument, value, isnegative )]#case in the beginning, where no previous allocation exists
    portfolios <- rbindlist(list (portfolios,portfolios[User_id == i & time == j - 1,.( User_id, time = j, instrument, value, isnegative )]))
    
    if ( j %in% actiontime){
      if ( actions[User_id == i & time == j, instrument] %in% portfolios[User_id ==i & time==j, instrument]){
        x<- actions$instrument[actions$User_id == i & actions$time == j]
        portfolios$value[portfolios$User_id ==i & portfolios$time==j & portfolios$instrument == x] <- portfolios$value[portfolios$User_id ==i & portfolios$time==j & portfolios$instrument == x] + actions$value[actions$User_id == i & actions$time == j]
        
      }else{
        
        #Create a new row, that has the transaction
        portfolios <- rbindlist(list (portfolios,actions[User_id == i & time == j ,.(User_id,time, instrument, value, isnegative =0)]))
      }
      #those postitions that have zero value should drop out
      portfolios$isnegative[portfolios$value <=0 &  portfolios$User_id ==i &  portfolios$time==j] <- 1
      portfolios <- portfolios[isnegative != 1]
    }
  }
}

portfolios <- portfolios[order(time)]
####With loops:
for (i in 1 :4){
  merge(portfolios[time== i,], actions[time==i]) 
} 


allplot <- merge(allplot, x[ ,.(plotvalue = mean(ValueCertificate),(Category="Certificate") ), by = year]
                 ,by = c("year","V2", "plotvalue"), all = TRUE)

hhy[transactions[tradetype== "K",.(Valuebuy = sum(value)), by=.(user_id,year= year(date))]
    ,Valuebuy:= i.Valuebuy, on = c(user_id = "user_id",year= "year")]


portfolios