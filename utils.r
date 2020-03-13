##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 1 - UTILS                                                                            #
##################################################################################################




####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library("stringr")



####################################################
# INTERNAL LIBRARIES                               #
####################################################




##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
sistema = c(Sys.info())
if (sistema[1] == "Linux"){
  Folder = paste("/home/", sistema[7], "/MultiLabelDataset", sep="")
  setwd(Folder)
} else {
  Folder = paste("C:/Users/", sistema[7], "/MultiLabelDataset", sep="")
  setwd(Folder)
}
setwd(Folder)
FolderRoot = Folder

setFolder <- function(){
  retorno = list()
  sistema = c(Sys.info())
  if (sistema[1] == "Linux"){
    Folder = paste("/home/", sistema[7], "/MultiLabelDataset", sep="")
    setwd(Folder)
  } else {
    Folder = paste("C:/Users/", sistema[7], "/MultiLabelDataset", sep="")
    setwd(Folder)
  }
  FolderRoot = Folder
  retorno$sistema = sistema
  retorno$Folder = Folder
  return(retorno)
}




##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
directories <- function(){
  
  retorno = list()
  
  folderResults = paste(FolderRoot, "/Results", sep="")
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  } else {
    dir.create(folderResults)
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  }
  
  folderDatasets = paste(FolderRoot, "/Datasets", sep="")
  if(dir.exists(folderDatasets) == TRUE){
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  } else {
    dir.create(folderDatasets)
    setwd(folderDatasets)
    dirDatasets = dir(folderDatasets)
    n_Datasets = length(dirDatasets)
  }
  
  folderLS = paste(FolderRoot, "/Datasets/LabelsSpace", sep="")
  if(dir.exists(folderLS) == TRUE){
    setwd(folderLS)
    dirLS = dir(folderLS)
    n_LS = length(dirLS)
  } else {
    dir.create(folderLS)
    setwd(folderLS)
    dirLS = dir(folderLS)
    n_LS = length(dirLS)
  }
  
  folderAS = paste(FolderRoot, "/Datasets/AttributesSpace", sep="")
  if(dir.exists(folderAS) == TRUE){
    setwd(folderAS)
    dirAS = dir(folderAS)
    n_AS = length(dirAS)
  } else {
    dir.create(folderAS)
    setwd(folderAS)
    dirAS = dir(folderAS)
    n_AS = length(dirAS)
  }
  
  folderL = paste(FolderRoot, "/Datasets/Labels", sep="")
  if(dir.exists(folderL) == TRUE){
    setwd(folderL)
    dirL = dir(folderL)
    n_L = length(dirL)
  } else {
    dir.create(folderL)
    setwd(folderL)
    dirL = dir(folderL)
    n_L = length(dirL)
  }
  
  folderStatistics = paste(FolderRoot, "/Datasets/Statistics", sep="")
  if(dir.exists(folderStatistics) == TRUE){
    setwd(folderStatistics)
    dirStatistics = dir(folderStatistics)
    n_Statistics = length(dirStatistics)
  } else {
    dir.create(folderStatistics)
    setwd(folderStatistics)
    dirStatistics = dir(folderStatistics)
    n_Statistics = length(dirStatistics)
  }
  
  folderSummary = paste(FolderRoot, "/Datasets/Summary", sep="")
  if(dir.exists(folderSummary) == TRUE){
    setwd(folderSummary)
    dirSummary = dir(getwd())
    n_Summary = length(dirSummary)
  } else {
    dir.create(folderSummary)
    setwd(folderSummary)
    dirSummary = dir(folderSummary)
    n_Summary = length(dirSummary)
  }
  
  folderIL = paste(FolderRoot, "/Datasets/InstancesPerLabels", sep="")
  if(dir.exists(folderIL) == TRUE){
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  } else {
    dir.create(folderIL)
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  }
  
  folderILS = paste(FolderRoot, "/Datasets/InstancesPerLabelsSpace", sep="")
  if(dir.exists(folderILS) == TRUE){
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  } else {
    dir.create(folderILS)
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  }
  
  folderCSV = paste(FolderRoot, "/Datasets/CSV", sep="")
  if(dir.exists(folderCSV) == TRUE){
    setwd(folderCSV)
    dirCSV = dir(folderCSV)
    n_CSV = length(dirCSV)
  } else {
    dir.create(folderCSV)
    setwd(folderCSV)
    dirCSV = dir(folderCSV)
    n_CSV = length(dirCSV)
  }
    
  folderDistinct = paste(FolderRoot, "/Datasets/Distinct", sep="")
  if(dir.exists(folderDistinct) == TRUE){
    setwd(folderDistinct)
    dirDistinct = dir(folderDistinct)
    n_Distinct = length(dirDistinct)
  } else {
    dir.create(folderDistinct)
    setwd(folderDistinct)
    dirDistinct = dir(folderDistinct)
    n_Distinct = length(dirDistinct)
  }
  
  # return folders
  retorno$folderLS = folderLS
  retorno$folderAS = folderAS
  retorno$folderL = folderL
  retorno$folderStatistics = folderStatistics
  retorno$folderSummary = folderSummary
  retorno$folderIL = folderIL
  retorno$folderILS = folderILS
  retorno$folderCSV = folderCSV
  retorno$folderDatasets = folderDatasets
  retorno$folderDistinct = folderDistinct
  
  # return files
  retorno$dirLS = dirLS
  retorno$dirAS = dirAS
  retorno$dirL = dirL
  retorno$dirStatistics = dirStatistics
  retorno$dirSummary = dirSummary
  retorno$dirIL = dirIL
  retorno$dirILS = dirILS
  retorno$dirCSV = dirCSV
  retorno$dirDatasets = dirDatasets
  retorno$dirDistinct = dirDistinct
  
  # return numbers
  retorno$n_LS = n_LS
  retorno$n_AS = n_AS
  retorno$n_L = n_L
  retorno$n_Statistics = n_Statistics
  retorno$n_Summary = n_Summary
  retorno$n_IL = n_IL
  retorno$n_ILS = n_ILS
  retorno$n_CSV = n_CSV
  retorno$n_Datasets = n_Datasets
  retorno$n_Distinct = n_Distinct
  
  return(retorno)
  
  gc()
}





##################################################################################################
# FUNCTION INFO DATA SET                                                                         #
# Objective:                                                                                     #
#     Gets the information that is in the "datsets.csv" file.                                    #  
# Parameters:                                                                                    #
#     dataset: the specific dataset                                                              #
# Return:                                                                                        #
#     Everything in the spreadsheet                                                              #
##################################################################################################
infoDataSet <- function(dataset){
  cat("\n FUNCAO INFO DATA SET \n")
  retorno = list()
  retorno$id = dataset$ID
  retorno$ds = dataset$DS
  retorno$name = dataset$Name
  retorno$domain = dataset$Domain
  retorno$instances = dataset$Instances
  retorno$labels = dataset$Labels
  retorno$predictiveAttributes = dataset$PredictiveAttributes
  retorno$attributesTotal = dataset$AttributesTotal
  retorno$attStart = dataset$AttStart
  retorno$attEnd = dataset$AttEnd
  retorno$labStart = dataset$LabStart
  retorno$labEnd = dataset$LabEnd
  retorno$nominal = dataset$Nominal
  retorno$numeric = dataset$Numeric
  retorno$cardinality = dataset$Dardinality
  retorno$density = dataset$Density
  retorno$distinct = dataset$Distinct
  retorno$dimensionality = dataset$Dimensionality
  retorno$xn = dataset$xn
  retorno$yn = dataset$yn
  retorno$gridn = dataset$gridn
  retorno$xt = dataset$xt
  retorno$yt = dataset$yt
  retorno$gridt = dataset$gridt
  return(retorno)
}





##################################################################################################
# FUNCTION FOLD NAMES                                                                            #
# Objective:                                                                                     #
#     Create folder names for each dataset                                                       #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     folderNames: a vector with folder names                                                    #
#     numberDataSets: number of files within folder                                              #
##################################################################################################
folderNames <- function(filenames, tipo){
  folderNames = filenames
  if(tipo == "train"){
    # -train.csv
    j = 0
    for(j in 1:length(folderNames)){
      a = str_length(folderNames[j])
      a = a - 10
      folderNames[j] = str_sub(folderNames[j], end = a)  
      j = j + 1
      gc()
    }  
  } else if(tipo=="test"){
    # -test.csv
    j = 0
    for(j in 1:length(folderNames)){
      a = str_length(folderNames[j])
      a = a - 9
      folderNames[j] = str_sub(folderNames[j], end = a)  
      j = j + 1
      gc()
    }
  } else {
    # -all.csv
    j = 0
    for(j in 1:length(folderNames)){
      a = str_length(folderNames[j])
      a = a - 8
      folderNames[j] = str_sub(folderNames[j], end = a)  
      j = j + 1
      gc()
    }
  }
  return(folderNames)
}




##################################################################################################
# FUNCTION FOLD NAMES                                                                            #
# Objective:                                                                                     #
#     Create folder names for each dataset                                                       #
# Parameters:                                                                                    #
#     fileNames: a vector with file names                                                        #
# Return:                                                                                        #
#     folderNames: a vector with folder names                                                    #
#     numberDataSets: number of files within folder                                              #
##################################################################################################
fileNamesFinal <- function(filenames){
  fnf = filenames
  j = 1
  for(j in 1:length(fnf)){
    str = paste(fnf[j], ".csv", sep="")
    fnf[j] = str
    j = j + 1
    gc()
  }
  return(fnf)
}

