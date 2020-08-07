#' @title readRawFilesBM
#' @description Read file from CR1000X datalogger, convert variable name and UTC+1 date configuration. Used for biometeorological station of SNO-Tourbières
#' @param rawFilePath Complete file path of raw file (ex. "~/SI_SNOT/processMeteoSol/inst/extdata/FR_LGT_001_20_036_0000.csv"). Need to respect format presented [here](https://sourcesup.renater.fr/www/si-snot/3.1_Sce_meteosol.html)
#' @return data.table meteosol semi-hourly
#' @importFrom data.table setDT setnames %chin%
#' @import testthat
#' @examples
#' rawFilePath <- system.file("extdata",
#' "FR_LGT_001_20_036_0000.csv",
#' package = "toolboxMeteosol")
#' readRawFilesBM(rawFilePath)
#' @export


readRawFilesBM <- function(rawFilePath){

	# --TEST--
	# Name file structure
	test_that(desc = paste0("Test name file structure of ",rawFilePath), code = {
		expect_true(substr(basename(rawFilePath),1,2)=="FR")
		expect_true(substr(basename(rawFilePath),4,6) %in% c("LGT","BDZ","FRN","LDM"))
		expect_true(substr(basename(rawFilePath),8,10) %in% c("001","002","003","004","005"))
		expect_true(nchar(substr(basename(rawFilePath),12,13))== 2)
		expect_true(as.numeric(substr(basename(rawFilePath),12,13)) >= 0 & as.numeric(substr(basename(rawFilePath),12,13)) < 40)
		expect_true(nchar(substr(basename(rawFilePath),15,17))== 3)
		expect_true(as.numeric(substr(basename(rawFilePath),15,17)) >= 1 & as.numeric(substr(basename(rawFilePath),12,13)) < 366)
	})

	# Read file
	data_brute <- setDT(read.csv(rawFilePath,header=FALSE,sep=",",stringsAsFactors = FALSE,row.names=NULL,check.names = FALSE,
		blank.lines.skip = TRUE))

	# Remove last column
	data_brute[,ncol(data_brute):=NULL]

	# Change variable name
	names(data_brute) <- toupper(as.character(data_brute[1,]))
	names(data_brute) <- gsub(" ", "", names(data_brute), fixed = TRUE)

	if(substr(basename(rawFilePath),8,10)=="001"){
		setnames(data_brute,c("P_RAIN_1_1_1","PPFD_1_1_1"),c("P_1_1_1","PPFD_IN_1_1_1"))
	}else{}
	
	# Convert GWL in WTD
	GWLCode <- c("GWL_1_1_1","GWL_2_1_1")
	setnames(data_brute,names(data_brute)[names(data_brute) %chin% GWLCode], "WTD")
	
	# Remove first line
	data_brute <- data_brute[-1, ]
	
	# Convert all column in numeric
	data_brute[, colnames(data_brute):=lapply(.SD, as.numeric)]

	# Remove null date
	data_brute <- data_brute[!is.na(DATE)]

	# --TEST--
	# Structure file
	test_that(desc = paste0("Test file structure of ",rawFilePath), code = {
    	expect_true(colnames(data_brute[,1])=="DATE")
    	expect_true(nchar(format(data_brute[1,1],scientific=FALSE))==14)
	})

	# Date configuration
	data_brute$DATE <- as.POSIXct(format(data_brute$DATE,scientific=FALSE),"%Y%m%d%H%M%S",tz='Africa/Algiers')

	# --TEST--
	# Date format and duplicated
	test_that(desc = paste0("Date format and duplicated test of ",rawFilePath), code = {
    	expect_true(unique(!is.na(data_brute$DATE)))
    	expect_true(unique(!duplicated(data_brute$DATE)))
	})

	return(data_brute)
}