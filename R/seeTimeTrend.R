#' @title csvToSimpleDygraph
#' @description Simple dygraph visualization from csv timeseries
#' @param compileFile csv file (with "timestamp" column date (%Y-%m-%d %H:%M:%S)
#' @param variableName String of variable code into compileFile (ex. "WTD")
#' @importFrom  data.table setDT
#' @importFrom  data.table setkey
#' @importFrom reshape melt
#' @importFrom dygraphs dygraph
#' @importFrom xts xts
#' @return html time trend with dygraph from each variable
#' @examples 
#' compileFilePath <- system.file("extdata",
#' "compilation/Compile_lgt_bm1_2020.csv",
#' package = "toolboxMeteosol")
#' dataCompile <- read.csv(compileFilePath,header=TRUE,sep=";")
#' csvToSimpleDygraph(dataCompile,"WTD")
#' @export

csvToSimpleDygraph <- function(compileFile,variableName){
  attr(ts, "tzone") <- "Africa/Algiers"
  Sys.setenv(TZ = "Africa/Algiers")

  setDT(compileFile)
  # Date configuration
  compileFile$timestamp <- as.POSIXct(compileFile$timestamp,"%Y-%m-%d %H:%M:%S",tz='Africa/Algiers')

  if(nrow(compileFile)==0){
    stop("no data into file")
  }else{}

  meltDataCompile <- setDT(reshape::melt(compileFile,id=1:3,na.rm=TRUE))
  is.na(meltDataCompile$value) <- meltDataCompile$value==-9999  
  meltDataCompile$variable <- as.character(meltDataCompile$variable)
  setkey(meltDataCompile, variable)

    res <- lapply(variableName,function(d){
      dataxts <- xts(meltDataCompile[variable %in% d,value],order.by=meltDataCompile[variable %in% d,timestamp])
      dygraphs::dygraph(dataxts,main=d,width="100%")
  })
    return(res)
}

#' @title csvToDygraph
#' @description Complex dygraph visualization from csv timeseries
#' @param compileFile csv file (specific format)
#' @param variableName String of variable code into compileFile (ex. "WTD")
#' @importFrom  data.table setDT
#' @importFrom  data.table setkey
#' @importFrom reshape melt
#' @import dygraphs
#' @importFrom xts xts
#' @importFrom wesanderson wes_palette
#' @return html time trend with dygraph with all variable selected ()
#' @examples 
#' compileFilePath <- system.file("extdata",
#' "compilation/Compile_lgt_bm1_2020.csv",
#' package = "toolboxMeteosol")
#' compileFile <- read.csv(compileFilePath,header=TRUE,sep=";")
#' variableName <- c("TS_1_1_1","TS_1_2_1")
#' csvToDygraph(compileFile,variableName)
#' @export

csvToDygraph <- function(compileFile,variableName){
  attr(ts, "tzone") <- "Africa/Algiers"
  Sys.setenv(TZ = "Africa/Algiers")

  setDT(compileFile)
  # Date configuration
  compileFile$timestamp <- as.POSIXct(compileFile$timestamp,"%Y-%m-%d %H:%M:%S",tz='Africa/Algiers')

  if(nrow(compileFile)==0){
    stop("no data into file")
  }else{}

  dateWindow <- c(min(compileFile$timestamp),max(compileFile$timestamp))
  fullSequence <- seq(min(compileFile$timestamp),max(compileFile$timestamp),by="30 min")

  meltDataCompile <- setDT(reshape::melt(compileFile,id=1:3,na.rm=TRUE))
  is.na(meltDataCompile$value) <- meltDataCompile$value==-9999  
  meltDataCompile$variable <- as.character(meltDataCompile$variable)
  setkey(meltDataCompile, variable)

  dataxts <- do.call("cbind",lapply(variableName,function(z){
  outp <- meltDataCompile[variable %in% z,]   
  
  # Time series for all variables
  tmp <-  outp[,list(timestamp,value)]
  tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$timestamp),])))
  db <- xts(tmp2[,value],order.by=tmp2[,timestamp])
  colnames(db) <- z
  db
  })
  )

  graph <- dygraphs::dygraph(dataxts,width="100%") %>%
                     dyOptions(stackedGraph = FALSE) %>%
                     dyRangeSelector(height = 20,strokeColor = "") %>%  
                     dyLegend(show = "onmouseover") %>% 
                     dyRangeSelector(dateWindow = dateWindow,retainDateWindow=TRUE) %>%
                     dyHighlight(highlightSeriesBackgroundAlpha = 0.8,highlightSeriesOpts = list(strokeWidth = 3)) %>%   
                     dyOptions(colors = wes_palette("Zissou1", length(variableName),type = "continuous"),retainDateWindow=TRUE,useDataTimezone=TRUE)

return(graph)

}

#' @title dataToXts
#' @description convert csv to xts format to use dygraph function
#' @param compileFile data.frame (specific format, add some test ?)
#' @param variableName String of variable code into compileFile (ex. "WTD", or c("WTD","TS_1_1_1"))
#' @param timestampColumn String of the timestampColumn with year, month, day, hour month and second(ex. "timestamp")
#' @param timestampFormat String of the timestampColumn format with year, month, day, hour month and second(ex. "%Y-%m-%d %H:%M:%S")
#' @importFrom  data.table setDT
#' @importFrom  data.table setkey
#' @importFrom reshape melt
#' @importFrom xts xts
#' @importFrom wesanderson wes_palette
#' @return xts object to used with dygraph with all variable selected ()
#' @examples 
#' compileFilePath <- system.file("extdata",
#' "compilation/Compile_lgt_bm1_2020.csv",
#' package = "toolboxMeteosol")
#' compileFile <- read.csv(compileFilePath,header=TRUE,sep=";")
#' timestampColumn <- "timestamp"
#' timestampFormat <- "%Y-%m-%d %H:%M:%S" 
#' variableName <- c("TS_1_1_1","TS_1_2_1")
#' dataToXts(compileFile,variableName,timestampColumn,timestampFormat)
#' @export

dataToXts <- function(compileFile,variableName,timestampColumn,timestampFormat){
  attr(ts, "tzone") <- "Africa/Algiers"
  Sys.setenv(TZ = "Africa/Algiers")

  # Test file
  test_that(desc = paste0("---Test file structure---"), code = {
    expect_true(nrow(compileFile)>0)
    expect_true(ncol(compileFile)>0)
  })

  setDT(compileFile)

  # Date configuration
  compileFile[,timestamp:=as.POSIXct(compileFile[,(timestampColumn),with=FALSE][[1]],timestampFormat,tz='Africa/Algiers')]

  test_that(desc = paste0("---Test date configuration---"), code = {
    expect_true(!is.na(compileFile[1,timestamp]))
  })

  dateWindow <- c(min(compileFile$timestamp),max(compileFile$timestamp))
  fullSequence <- seq(min(compileFile$timestamp),max(compileFile$timestamp),by="30 min")

  meltDataCompile <- setDT(reshape::melt(compileFile,id=1:3,na.rm=TRUE))
  is.na(meltDataCompile$value) <- meltDataCompile$value==-9999  
  meltDataCompile$variable <- as.character(meltDataCompile$variable)
  setkey(meltDataCompile, variable)

  dataxts <- do.call("cbind",lapply(variableName,function(z){
    outp <- meltDataCompile[variable %in% z,]   
    # Time series for all variables
    tmp <-  outp[,list(timestamp,value)]
    tmp2 <- setDT(data.frame(Date=fullSequence,with(tmp,tmp[match(fullSequence,tmp$timestamp),])))
    db <- xts(tmp2[,value],order.by=tmp2[,Date])
    colnames(db) <- z
    db
    })
  )

return(dataxts)

}

























