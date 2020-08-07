#' @title soilHeatFluxCalcul
#' @description Soil Heat Flux calculation
#' @param data_brute data.table 
#' @param SWC_variable string vector of SWC variable (ex. SWC_1_1_1)
#' @param G_variable string vector of Gvariable (ex. G_1_1_1)
#' @param Gqf_variable string vector of Gqf variable ()
#' @param Esen_variable string vector of Esen variable 
#' @return data.table meteosol semi-hourly
#' @importFrom data.table shift
#' @importFrom data.table first
#' @export

soilHeatFluxCalcul <- function(data_brute,SWC_variable,G_variable,Gqf_variable,Esen_variable){

	# Soil Heat Flux calculation in a lapply loop
	lapply(1:length(G_variable),function(x){
		g <- G_variable[x]
		g_qf <- Gqf_variable[x]
		esen <- Esen_variable[x]
		swc <- SWC_variable[x]
		m <- gregexpr('[0-9]+',g)

		# Correction with Flag and Esen
		data_brute[,eval(g) := ifelse((get(g_qf)>0)|(get(esen)=="+INF"),NA,get(g))]
		
		# Temperature profil
		profil <- as.numeric(regmatches(g,m)[[1]])
		ts1 <- paste0("TS_",profil[1],"_",profil[2],"_",profil[3])
	    ts2 <- paste0("TS_",profil[1],"_",profil[2]+1,"_",profil[3])
	    
	    # Soil Heat Flux calculation
		data_brute[, meanTs := rowMeans(cbind(get(ts1),get(ts2)),na.rm=TRUE),with=TRUE]
		data_brute[, diff := (meanTs - shift(meanTs, fill = first(meanTs)))/60]
		data_brute[, eval(g) := get(g)+(0.05*((1.92*0.1*1000000)+(4.18*1*1000000*(get(swc)/100)))*diff)]					 
	})
}
