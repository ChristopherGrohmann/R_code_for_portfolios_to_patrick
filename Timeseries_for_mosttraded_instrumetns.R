#timeseries for most traded instruments

bestinstr <- transactions[,.(number_of_trans = 0 ), by=.(instrument_id,date)]
bestinstr[transactions[]]