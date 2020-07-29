#' @title aggregateMeteosolData
#' @description Elaborated variable calculation, quality control and semi-hourly average for raw data from biometeorological station of SNO-Tourbières
#' @param dataMeteosol Complete file path of raw file from biometeorological station of SNO-Tourbières (ex. "/home/jbparoissien/SI_SNOT/processMeteoSol/inst/extdata/FR_LGT_001_20_036_0000.csv"). Need to respect format presented [here](https://sourcesup.renater.fr/www/si-snot/3.1_Sce_meteosol.html)
#' @return data.table meteosol semi-hourly
#' @importFrom data.table setDT
#' @importFrom lubridate floor_date
#' @importFrom dplyr count
#' @import testthat
#' @examples
#' dataMeteosol <- system.file("extdata","FR_LGT_001_20_036_0000.csv",package = "toolboxMeteosol")
#' aggregateMeteosolData(system.file("extdata","FR_LGT_001_20_036_0000.csv",package = "toolboxMeteosol"))
#' @export

aggregateMeteosolData <- function(dataMeteosol){

	# Test name file structure
	test_that(desc = "Test name file structure", code = {
		expect_true(substr(basename(dataMeteosol),1,2)=="FR")
		expect_true(substr(basename(dataMeteosol),4,6) %in% c("LGT","BDZ","FRN","LDM"))
		expect_true(substr(basename(dataMeteosol),8,10) %in% c("001","002","003","004","005"))
		expect_true(nchar(substr(basename(dataMeteosol),12,13))== 2)
		expect_true(as.numeric(substr(basename(dataMeteosol),12,13)) >= 0 & as.numeric(substr(basename(dataMeteosol),12,13)) < 40)
		expect_true(nchar(substr(basename(dataMeteosol),15,17))== 3)
		expect_true(as.numeric(substr(basename(dataMeteosol),15,17)) >= 1 & as.numeric(substr(basename(dataMeteosol),12,13)) < 366)
		expect_true(substr(basename(dataMeteosol),19,22)== "0000")
	})

	# Read file
	data_brute <- setDT(read.csv(dataMeteosol,header=FALSE,sep=",",stringsAsFactors = FALSE,row.names=NULL,check.names = FALSE,
		blank.lines.skip = TRUE))
	data_brute <- data_brute[,1:(ncol(data_brute)-1)]#Suppression de la dernière colonne

	names(data_brute) <- toupper(as.character(data_brute[1,]))
	names(data_brute) <- gsub(" ", "", names(data_brute), fixed = TRUE)

	if(substr(basename(dataMeteosol),8,10)=="001"){
		colnames(data_brute)[colnames(data_brute)=="P_RAIN_1_1_1"] <- "P_1_1_1"
		colnames(data_brute)[colnames(data_brute)=="PPFD_1_1_1"] <- "PPFD_IN_1_1_1"
	}else{}

	colnames(data_brute)[colnames(data_brute)=="GWL_1_1_1"] <- "WTD"
	colnames(data_brute)[colnames(data_brute)=="GWL_2_1_1"] <- "WTD"
	
	data_brute <- data_brute[-1, ]
	data_brute <- data_brute[, lapply(.SD, as.numeric)]
	is.na(data_brute) <- data_brute==-9999

	# Test structure file
	test_that(desc = "Test file structure", code = {
    	expect_true(colnames(data_brute[,1])=="DATE")
    	expect_true(nchar(format(data_brute[1,1],scientific=FALSE))==14)
	})

	# Date configuration
	data_brute$DATE <- as.POSIXct(format(data_brute$DATE,scientific=FALSE),"%Y%m%d%H%M%S",tz='Africa/Algiers')

	# Semi-hourly date creation
	data_brute$semihour <- lubridate::floor_date(data_brute$DATE,'30 minutes')
	
	# Inversion WS et WD avant 2016-02-25 12:01:00 pour la station bm1 de la Guette
	if(substr(basename(dataMeteosol),4,10)=="LGT_001" & data_brute[1,DATE] < "2016-02-25 12:01:00"){
			#print("Inversion WD_1_1_1 et WS_1_1_1")
		    colnames(data_brute)[colnames(data_brute)%in%c("WD_1_1_1","WS_1_1_1")] <- c("WS_1_1_1","WD_1_1_1")
	}

	# Remove RH value for specific period
	if(substr(basename(dataMeteosol),4,10)=="LGT_001" & (data_brute[1,DATE] < "2017-04-05" & data_brute[1,DATE] > "2016-02-03")){
		data_brute[,RH_1_1_1:=NA]
	}

	# Elaborated variable calculation
	SWC_variable <- colnames(data_brute)[grepl("SWC", colnames(data_brute)) & grepl("TSWC", colnames(data_brute))==FALSE]
	Pe_variable <- colnames(data_brute)[grepl("PE", colnames(data_brute))]
	G_variable <- colnames(data_brute)[grepl("G_", colnames(data_brute)) & grepl("G_QF|V_G", colnames(data_brute))==FALSE]
	Gqf_variable <- colnames(data_brute)[grepl("G_QF", colnames(data_brute))]
	Esen_variable <- colnames(data_brute)[grepl("ESEN", colnames(data_brute))]

	## Soil water content 
	soilWaterConcentCalcul(data_brute,SWC_variable,Pe_variable)

	## Soil heat flux 
	soilHeatFluxCalcul(data_brute,SWC_variable,G_variable,Gqf_variable,Esen_variable)
	
	## Quality control
	if(substr(basename(dataMeteosol),8,10)=="001"){
		qualityControlBM1(data_brute)
	}else{}

	# Semi-hourly aggregation
	
	## Variables cibles
	TS_variable <- colnames(data_brute)[grepl("TS_", colnames(data_brute))]

	if(substr(basename(dataMeteosol),8,10)=="001"){
		meteoVariable <- c("PA_1_1_1","P_1_1_1","PPFD_IN_1_1_1","WD_1_1_1","WS_1_1_1","TA_1_1_1","RH_1_1_1")	
		colSNOT <- c(meteoVariable,G_variable,SWC_variable,TS_variable)	
	}else{
		SWLW_variable <- colnames(data_brute)[grepl("SWOUT|LWOUT|SWIN|LWIN", colnames(data_brute))]
		# Correction du code des variables (SWOUT >> SW_OUT)
		SWLW_variable <- paste0(substr(SWLW_variable,1,2),"_",substr(SWLW_variable,3,11))
		colSNOT <- c(G_variable,SWC_variable,TS_variable,SWLW_variable)
		colnames(data_brute)[grepl("SWOUT|LWOUT|SWIN|LWIN", colnames(data_brute))] <- SWLW_variable
	}

	## Add WTD if necessary
	if(colnames(data_brute)[colnames(data_brute)=="WTD"]=="WTD"){
	  colSNOT <- c(colSNOT,"WTD")
	}else{}
	
	## semi-hourly average or accumulation
	data_semiHour <- data_brute[,lapply(.SD,mean,na.rm=TRUE),by=semihour,.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==FALSE]]
	if(substr(basename(dataMeteosol),8,10)=="001"){
		data_semiHour[,P_1_1_1 := data_brute[,lapply(.SD,sum,na.rm=TRUE),by=semihour,.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==TRUE]][,P_1_1_1]]	
	}else{}

	# Last quality control (remove average if less 10 values)
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

