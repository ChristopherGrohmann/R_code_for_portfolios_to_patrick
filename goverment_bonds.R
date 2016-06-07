#finding out the number of goverment bonds in the data.
#
Bonds <- transactions[instrument_type == 2,instrument_id_intern ,by = instrument_description]
Bonds$ONs <- grep(paste(as.character(Bonds$instrument_description)), "EO")