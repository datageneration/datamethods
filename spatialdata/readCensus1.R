#read in the census data set
readCensus <- function() {
  urlToRead <-"https://www2.census.gov/programs-surveys/popest/tables/2010-2015/state/totals/nst-est2015-01.csv"
  census2015DF=read.csv(url(urlToRead))
  #remove the first 8 rows (‘header information’)
  census2015DF<-census2015DF[-1:-8,]

  #only keep the first 9 columns
  census2015DF<-census2015DF[,1:9]

  #rename the first column
  census2015DF$stateName <- census2015DF[,1]
  census2015DF<-census2015DF[,-1]

  #remove the last rows (tail info)
  census2015DF<-census2015DF[-52:-58,]

  #remove the ‘dot’ from the state name
  census2015DF$stateName <- gsub("\\.","", census2015DF$stateName)

  #convert the columns to actual numbers and rename columns
  census2015DF$april10census <-Numberize(census2015DF$X)
  census2015DF$april10base <-Numberize(census2015DF$X.1)
  census2015DF$july10pop <-Numberize(census2015DF$X.2)
  census2015DF$july11pop <-Numberize(census2015DF$X.3)
  census2015DF$july12pop <-Numberize(census2015DF$X.4)
  census2015DF$july13pop <-Numberize(census2015DF$X.5)
  census2015DF$july14pop <-Numberize(census2015DF$X.6)
  census2015DF$july15pop <-Numberize(census2015DF$X.7)


  census2015DF <- census2015DF[,-1:-8]

  #remove the old rownames, which are now confusing
  rownames(census2015DF) <- NULL

  return(census2015DF)
  }

  Numberize <- function(inputVector)
  {
    # Get rid of commas
    inputVector<-gsub(",","", inputVector)
    # Get rid of spaces
    inputVector<-gsub(" ","", inputVector)

    return(as.numeric(inputVector))
  }
