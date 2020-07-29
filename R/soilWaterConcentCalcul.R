#' @title soilWaterConcentCalcul
#' @description Contrôle qualité d'une data.table
#' @param data_brute data.table 
#' @param SWC_variable string vector of SWC variable 
#' @param Pe_variable string vector of Pe variable 
#' @return Retourne une data.table meteosol semi-horaire
#' @export

soilWaterConcentCalcul <- function(data_brute,SWC_variable,Pe_variable){

	lapply(1:length(SWC_variable),function(x){
		swc <- SWC_variable[x]
		pe <- Pe_variable[x]
		data_brute[,eval(swc) := ((0.2535*log(get(pe))-0.3201)*100)]
		# Correction des valeurs négatives pouvant apparaître (valeur 0)
		data_brute[,eval(swc) := ifelse(get(swc)<0,0,get(swc))]
	})
}	