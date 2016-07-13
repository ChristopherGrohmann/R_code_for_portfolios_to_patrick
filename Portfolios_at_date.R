portfolios_at_date <- function(date_at,portfolios,churntime = 720){
  date_at <- as.Date(date_at, "%Y-%m-%d")
  setkey(portfolios,user_id,date)
  portfolios <- portfolios[portfolios[date <= date_at &
                                        date >=  as.Date(as.numeric(date_at) - churntime, origin="1970-01-01") ,
                                      .(date =max(date)) ,by = user_id]]
  portfolios <- portfolios[date == i.date]
  return(portfolios)
}