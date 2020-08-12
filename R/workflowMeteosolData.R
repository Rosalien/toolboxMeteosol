#' @title Data processing of raw files from biometeorological station of SNO-Tourbières
#' @description Compilation + qualitycontrol of raw files from 1_BRUTE_meteosol/
#' @param repFile repository of raw files (ex. 1_BRUTE_meteosol/03-02-2016_actuel/)
#' @param repTraitement out repository for temp files 
#' @param repOut out repository for final files.  Ready to integrate to data-snot.cnrs.fr
#' @param yearsToIntegrate String of the years to integrate (yy) (ex. "19" for 2019...,"all" pour all years)
#' @param Site String of the code site code (ex. "lgt", "bdz", "ldm", or "frn")
#' @param Station String of the station code (ex. "bm1", "bm2", "bm3"...)
#' @importFrom data.table setDT
#' @importFrom data.table rbindlist
#' @import lubridate
#' @import testthat
#' @return A csv file ready to be integrated in data-snot.cnrs.fr
#' @examples 
#' workflowMeteosolData(repFile=system.file("extdata", package = "toolboxMeteosol"),
#'				repTraitement="~/",
#'				repOut="~/",
#'				yearsToIntegrate="20",
#'				Site="lgt",
#'				Station="bm1")
#' @export

workflowMeteosolData <- function(repFile,repTraitement,repOut,yearsToIntegrate,Site,Station){

	# UTC+1 configuration
	attr(ts, "tzone") <- "Africa/Algiers"
	Sys.setenv(TZ = "Africa/Algiers")

	# List of files
	listFile <- list.files(path = repFile,pattern="*.csv",full.names = TRUE)
	
	# Test repository
	if(length(listFile)==0){
		stop("no files in repository selected")
	}else{}

	# Test file name (to DO)

	listFileYears <- unique(substr(basename(listFile),12,13))

	if(yearsToIntegrate=="all"){
		yearsToIntegrate <- listFileYears
	}else{}
	
# For each years...
lapply(unique(yearsToIntegrate),function(x){
	
  print("---------Years---------")
  print(paste0("20",x))
  
  # Select group file for each years (x)
  ListFileByYears <- listFile[grepl(x,substr(basename(listFile),12,13))]
  
  # Progress bar
  nbrListFileByYears <- length(ListFileByYears)
  pb <- txtProgressBar(min = 0, max = nbrListFileByYears, style = 3)
  
  aggregateData <- rbindlist(lapply(1:nbrListFileByYears,function(y){
    setTxtProgressBar(pb, y)
    aggregateMeteosolData(rawFilePath=ListFileByYears[y])
  }))
  close(pb)
  
  # Variable list without timestamp,date,time
  colSNOT <- names(aggregateData)[grepl("timestamp|date|time",names(aggregateData))==FALSE]
  
  # Average 30min
  data_semiHour <- aggregateData[,lapply(.SD,mean,na.rm=TRUE),by=c("timestamp","date","time"),.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==FALSE]]
  if(length(unique(grepl("P_1_1_1",colSNOT)))>1){
    data_semiHour[,P_1_1_1 := aggregateData[,lapply(.SD,sum,na.rm=TRUE),by=c("timestamp","date","time"),.SDcols=colSNOT[grepl("P_1_1_1",colSNOT)==TRUE]][,P_1_1_1]]	
  }else{}
  
  # Formatage 
  ## Convert Na to -9999
  # data_semiHour[is.na(data_semiHour)] <- "-9999" 
  # Na in -9999 (much faster than solutions https://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table)
  is.na(data_semiHour) <- data_semiHour==-9999

  # WTD calculation (add a parameter for hRef?)
  if(colnames(data_semiHour)[colnames(data_semiHour)=="WTD"]=="WTD"){
      # Coef value to calculate WTD
      hRef <- 0.41
      data_semiHour[,WTD:=(WTD-hRef)/100]
  }else{}

  minDate <- min(data_semiHour$timestamp)
  maxDate <- max(data_semiHour$timestamp)

  # file name to save
  nomCompileFile <- paste0(repTraitement,"Compile_",Site,"_",Station,"_","20",x,".csv")
  print(paste0("nomCompileFile : ",nomCompileFile))

  # save file for each years
  if(file.exists(nomCompileFile)){
    write.table(data_semiHour,nomCompileFile,append=TRUE,row.names=FALSE,col.names=FALSE,qmethod="escape",sep=";",fileEncoding = "UTF8")
  }else{		
    write.table(data_semiHour,nomCompileFile,row.names=FALSE,col.names=TRUE,qmethod="escape",sep=";",fileEncoding = "UTF8")
  }
  
  # save file to integrate to data-snot.cnrs.fr
  buildSNOTFile(dataSNOT=data_semiHour[,-"timestamp"],theme="meteosol",frequence="infra-journalier",
           site=Site,station=Station,datatype="meteosol_infraj",
           minDate=minDate,maxDate=maxDate,repsortie=repOut)
})

}
  