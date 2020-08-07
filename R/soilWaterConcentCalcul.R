#' @title soilWaterConcentCalcul
#' @description Soil Water Content calculation on a data.table with SWC and PE variable
#' @param data_brute data.table with SWC and Pe variables
#' @param SWC_variable string vector of SWC variables (ex. SWC_X_X_X)
#' @param Pe_variable string vector of Pe variables (ex. Pe_X_X_X)
#' @return data.table with SWC values
#' @export

soilWaterConcentCalcul <- function(data_brute,SWC_variable,Pe_variable){

	lapply(1:length(SWC_variable),function(x){
		swc <- SWC_variable[x]
		pe <- Pe_variable[x]
		
		data_brute[,eval(swc) := ((0.2535*log(get(pe))-0.3201)*100)]
		# Negatives values correction
		data_brute[,eval(swc) := ifelse(get(swc)<0,0,get(swc))]
	})
}	