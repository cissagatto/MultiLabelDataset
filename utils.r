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



####################################################
# INTERNAL LIBRARIES                               #
####################################################
library("stringr")



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
  
  cat("\nFunção Diretório\n")
  
  retorno = list()

  folderResults = paste(FolderRoot, "/Results", sep="")
  if(dir.exists(folderResults) == TRUE){
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  } else {
    dir.create(folderResults)
    setwd(folderLS)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
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
  
  folderS = paste(FolderRoot, "/Datasets/Statistics", sep="")
  if(dir.exists(folderS) == TRUE){
    setwd(folderS)
    dirS = dir(folderS)
    n_S = length(dirS)
  } else {
    dir.create(folderS)
    setwd(folderS)
    dirS = dir(folderS)
    n_S = length(dirS)
  }
  
  folderSummary = paste(FolderRoot, "/Datasets/Summary", sep="")
  if(dir.exists(folderSummary) == TRUE){
    setwd(folderS)
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
  
  # return folders
  retorno$folderLS = folderLS
  retorno$folderAS = folderAS
  retorno$folderL = folderL
  retorno$folderS = folderS
  retorno$folderSummary = folderSummary
  retorno$folderIL = folderIL
  retorno$folderILS = folderILS
  retorno$folderCSV = folderCSV
  
  # return files
  retorno$dirLS = dirLS
  retorno$dirAS = dirAS
  retorno$dirL = dirL
  retorno$dirS = dirS
  retorno$dirSummary = dirSummary
  retorno$dirIL = dirIL
  retorno$dirILS = dirILS
  retorno$dirCSV = dirCSV
  
  # return numbers
  retorno$n_LS = n_LS
  retorno$n_AS = n_AS
  retorno$n_L = n_L
  retorno$n_S = n_S
  retorno$n_Summary = n_Summary
  retorno$n_IL = n_IL
  retorno$n_ILS = n_ILS
  retorno$n_CSV = n_CSV
  
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
fileNames <- function(){
  retorno = list()
  d = directories()
  fileNames = c(d$dirCSV)
  retorno$fileNames = fileNames
  retorno$numberFiles = length(fileNames)
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
folderNames <- function(filenames){
  result = list()
  folderNames = filenames
  # retirando "-train.csv" do nome dos arquivos
  j = 0
  for(j in 1:length(folderNames)){
    a = str_length(folderNames[j])
    a = a - 10
    folderNames[j] = str_sub(folderNames[j], end = a)  
    j = j + 1
    gc()
  }
  result$folderNames = folderNames
  result$numberFolders = length(folderNames)
  return(result)
}



##################################################################################################
# FUNCTION LABELS NAME                                                                           #
# Objective:                                                                                     #
#     Get dataset label names                                                                    #
# Parameters:                                                                                    #
#     dataset: is the dataset file                                                               #
# Return:                                                                                        #
#     labels: dataset labels names                                                               #
##################################################################################################
labelsNames <- function(dataset){
  d = directories()
  setwd(d$folderL)
  labels = data.frame(read.csv(dataset))
  retorno$labelsNames = labelsNames
  return(labels)
}

