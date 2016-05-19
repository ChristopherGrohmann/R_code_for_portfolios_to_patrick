#Simple example
library (data.table)
actions = data.table(User_id = c("Carl","Carl","Carl","Lisa","Moe"),
                     category = c(1,1,2,2,1),
                     value= c(10,20,30,40,50))
actions[,sum(value[category == 1]),by= User_id]

users = actions[,User_id,by= User_id]
users$value_one = actions[category==1,.(value_one =sum(value)),by= User_id]$value_one

res <- actions[,.(value=0), by=User_id]
res[actions[category==1, sum(value), by=User_id], value:=V1, on="User_id"]





#Most Usefull
users = actions[, .(value_one = 0), by = User_id]; 
users[actions[category == 1, .(value_one = sum(value)),
              by = User_id], on = 'User_id', value_one := i.value_one]