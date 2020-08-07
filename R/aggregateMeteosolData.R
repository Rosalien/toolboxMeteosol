#' @title aggregateMeteosolData
#' @description Elaborated variable calculation, quality control and semi-hourly average for raw data from biometeorological station of SNO-Tourbières
#' @param rawFilePath Complete file path of raw file from biometeorological station of SNO-Tourbières (ex. "/home/jbparoissien/SI_SNOT/processMeteoSol/inst/extdata/FR_LGT_001_20_036_0000.csv"). Need to respect format presented [here](https://sourcesup.renater.fr/www/si-snot/3.1_Sce_meteosol.html)
#' @return data.table meteosol semi-hourly
#' @importFrom data.table setDT
#' @importFrom lubridate floor_date
#' @importFrom dplyr count
#' @import testthat
#' @examples
#' rawFilePath <- system.file("extdata",
#' "FR_LGT_001_20_036_0000.csv",
#' package = "toolboxMeteosol")
#' aggregateMeteosolData(rawFilePath)
#' @export

aggregateMeteosolData <- function(rawFilePath){

	# Read, convert variable and configurate date
	data_brute <- readRawFilesBM(rawFilePath)
	
	# Semi-hourly date creation
	data_brute$semihour <- lubridate::floor_date(data_brute$DATE,'30 minutes')
	
	# Inverse WS and WD before 2016-02-25 12:01:00 for lgt/bm1
	if(substr(basename(rawFilePath),4,10)=="LGT_001" & data_brute[1,DATE] < "2016-02-25 12:01:00"){
		    colnames(data_brute)[colnames(data_brute)%in%c("WD_1_1_1","WS_1_1_1")] <- c("WS_1_1_1","WD_1_1_1")
	}

	# Remove RH value for specific period
	if(substr(basename(rawFilePath),4,10)=="LGT_001" & (data_brute[1,DATE] < "2017-04-05" & data_brute[1,DATE] > "2016-02-03")){
		data_brute[,RH_1_1_1:=NA]
	}

	# Elaborated variable calculation
	SWC_variable <- colnames(data_brute)[grepl("SWC", colnames(data_brute)) & grepl("TSWC", colnames(data_brute))==FALSE]
	Pe_variable <- colnames(data_brute)[grepl("PE", colnames(data_brute))]
	G_variable <- colnames(data_brute)[grepl("G_", colnames(data_brute)) & grepl("G_QF|V_G", colnames(data_brute))==FALSE]
	Gqf_variable <- colnames(data_brute)[grepl("G_QF", colnames(data_brute))]
	Esen_variable <- colnames(data_brute)[grepl("ESEN", colnames(data_brute))]

	## Soil water content calculation
	soilWaterConcentCalcul(data_brute,SWC_variable,Pe_variable)

	## Soil heat flux calculation
	soilHeatFluxCalcul(data_brute,SWC_variable,G_variable,Gqf_variable,Esen_variable)
	
	## Quality control
	if(substr(basename(rawFilePath),8,10)=="001"){
		qualityControlBM1(data_brute)
	}else{}

	# Semi-hourly aggregation
	
	TS_variable <- colnames(data_brute)[grepl("TS_", colnames(data_brute))]

	if(substr(basename(rawFilePath),8,10)=="001"){
		meteoVariable <- c("PA_1_1_1","P_1_1_1","PPFD_IN_1_1_1","WD_1_1_1","WS_1_1_1","TA_1_1_1","RH_1_1_1")	
		colSNOT <- c(meteoVariable,G_variable,SWC_variable,TS_variable)	
	}else{
		SWLW_variable <- colnames(data_brute)[grepl("SWOUT|LWOUT|SWIN|LWIN", colnames(data_brute))]
		
		# Convert variable code SWOUT >> SW_OUT
		SWLW_variable <- paste0(substr(SWLW_variable,1,2),"_",substr(SWLW_variable,3,11))
		colSNOT <- c(G_variable,SWC_variable,TS_variable,SWLW_variable)
		colnames(data_brute)[grepl("SWOUT|LWOUT|SWIN|LWIN", colnames(data_brute))] <- SWLW_variable
	}

	## Add WTD variable code in colSNOT list
	if(colnames(data_brute)[colnames(data_brute)=="WTD"]=="WTD"){
	  colSNOT <- c(colSNOT,"WTD")
	}else{}
	
	## semi-hourly average or accumulation
	data_semiHour <- data_brute[,lapply(.SD,mean,na.rm=TRUE),by=semihour,.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==FALSE]]
	if(substr(basename(rawFilePath),8,10)=="001"){
		data_semiHour[,P_1_1_1 := data_brute[,lapply(.SD,sum,na.rm=TRUE),by=semihour,.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==TRUE]][,P_1_1_1]]	
	}else{}

	# Remove average if less 10 values (only for G variables, why ?)
	lapply(1:length(G_variable),function(x){
		g <- G_variable[x]
		data_semiHour[!is.na(get(g)),countValue:=setDT(count(data_brute[!is.na(get(g))],semihour))[,n]]
		data_semiHour[!is.na(get(g)),eval(g):=ifelse((countValue<=10),NA,get(g))]
	})

	# Variable selection
	data_semiHour$timestamp <- as.POSIXct(format(data_semiHour$semihour),tz='Africa/Algiers')
	data_semiHour$date <- format(data_semiHour$timestamp,"%d/%m/%Y")
	data_semiHour$time <- format(data_semiHour$timestamp,"%H:%M")
	data_Ok <- data_semiHour[,c("timestamp","date","time",colSNOT),with=FALSE]
	
return(data_Ok)
}#end

