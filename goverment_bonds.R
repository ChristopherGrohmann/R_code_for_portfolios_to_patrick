#finding out the number of goverment bonds in the data.
#
Bonds <- transactions[instrument_type == 2, mean(instrument_id_intern) ,by = instrument_description]

landerliste <- read.csv("C:/Users/Grohmann/Downloads/countrylist.csv")

bond_list <- read.csv("C:/Users/Grohmann/Documents/Interactive data/Vergleichsdaten_Bloomberg_co/Bonds.csv")
bond_list[is.na(bond_list)]  <- 0
setDT(bond_list)
govbond<- bond_list[Deutsche_staatsanleihe ==1 | Europäisch == 1| Welt == 1]

#up to line 106 in Master_create_portfolios

setkey(govbond, V1)
transactions <- transactions[govbond, nomatch=0]

#go onup from line 107

# to 214
transactions <- transactions[instruments, nomatch=0]

plottimeseries_sellbuy_hh(hhy)