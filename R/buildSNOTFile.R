#' @title buildSNOTFile
#' @description Build and write a file with SNO-Tourbières format. Ready to be integrated into data-snot.snot.cnrs.fr
#' @param dataSNOT dataframe of data to be integrated
#' @param theme string of theme code of the data (ex. "meteosol")
#' @param frequence string of frecency code (ex. "sh" or "infraj")
#' @param site string of the code site (ex. "lgt")
#' @param station string of the code station (ex. "bm1")
#' @param datatype string of datatype code (ex. "meteosol")
#' @param minDate string of begin date (ex. %d/%m/%Y)
#' @param maxDate string of end date (ex. %d/%m/%Y)
#' @param repsortie string of the path to export file (default "../3_SNOT_meteosol/")
#' @return Data file with name and structure for data-snot.cnrs.fr. Ready to be integrated in data-snot.cnrs.fr
#' @export 
#' 
buildSNOTFile <- function(dataSNOT,theme,frequence,site,station,datatype,minDate,maxDate,repsortie){

sepCSV <- paste(replicate(ncol(dataSNOT)-1, ";"), collapse = "")
minDateET <- format(minDate,"%d/%m/%Y")
maxDateET <- format(maxDate,"%d/%m/%Y")
minDateFName <- format(minDate,"%d-%m-%Y")
maxDateFName <- format(maxDate,"%d-%m-%Y")

variablesSNOT <- paste(colnames(dataSNOT),collapse=";")

# Header creation
header <- paste("site;",site,";",station,paste(replicate(ncol(dataSNOT)-3, ";"), collapse = ""),"
theme;",theme,paste(replicate(ncol(dataSNOT)-2, ";"), collapse = ""),"
frequence;",frequence,paste(replicate(ncol(dataSNOT)-2, ";"), collapse = ""),"
date de debut;",minDateET,paste(replicate(ncol(dataSNOT)-2, ";"), collapse = ""),"
date de fin;",maxDateET,paste(replicate(ncol(dataSNOT)-2, ";"), collapse = ""),"
commentaire;",paste(replicate(ncol(dataSNOT)-2, ";"), collapse = ""),"\n",sepCSV,"\n",sepCSV,"\n",
variablesSNOT,"\n",sepCSV,sep="")

	my.write <- function(x, file, header, f = write.csv, ...){
	# create and open the file connection
  	datafile <- file(file, open = 'wt')
	# close on exit
  	on.exit(close(datafile))
	# if a header is defined, write it to the file (@CarlWitthoft's suggestion)
	if(!missing(header)) writeLines(header,con=datafile)
	# write the file using the defined function and required addition arguments  
  	write.table(x, datafile,row.names=FALSE,col.names=FALSE,qmethod="escape",sep=";",fileEncoding = "UTF8")
	}	

fileName <- paste(repsortie,site,"-",station,"_",datatype,"_",minDateFName,"_",maxDateFName,".csv",sep="")

my.write(dataSNOT,fileName,header)

}#end
