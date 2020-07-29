#' @title qualityControlBM1
#' @description Quality control of data.table from a bm1 station
#' @param data_brute a data.table 
#' @return A data.table with correction
#' @export


qualityControlBM1 <- function(data_brute){
		## Contrôle vent
		data_brute[,WD_1_1_1 := ifelse((WS_1_1_1==0)|(WD_1_1_1<0)|is.na(WD_1_1_1),NA,WD_1_1_1)]
		data_brute[,WS_1_1_1 := ifelse(WS_1_1_1>60,NA,WS_1_1_1)]
		data_brute[,WD_1_1_1 := ifelse(WD_1_1_1==360,359.99,WD_1_1_1)]
		data_brute[,WD_1_1_1 := ifelse(WD_1_1_1>360,NA,WD_1_1_1)]
	
		## Correction Pression
		data_brute[,PA_1_1_1 := ifelse(PA_1_1_1<830,NA,PA_1_1_1)]
	
		## Correction humidité relative
		data_brute[,RH_1_1_1 := ifelse(RH_1_1_1>100,100,RH_1_1_1)]
		
		# Correction valeur PPFD_IN
		data_brute[,PPFD_IN_1_1_1 := PPFD_IN_1_1_1*1000]
}
	