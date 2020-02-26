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






##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
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
sf = setFolder()
FolderRoot = sf$Folder






##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
directories <- function(){
  
  retorno = list()

  #
  folderResults = paste(FolderRoot, "/Results", sep="")
  if(dir.exists(folderResults) == TRUE){
    cat("\n|========== Results folder already exists! ==========|\n")
    setwd(folderResults)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  } else {
    cat("\n|========== Creating the Results folder ==========|\n")
    dir.create(folderResults)
    setwd(folderLS)
    dirResults = dir(folderResults)
    n_Results = length(dirResults)
  }
  
  # 
  folderLS = paste(FolderRoot, "/Datasets/LabelsSpace", sep="")
  if(dir.exists(folderLS) == TRUE){
    cat("\n|========== Labels Space folder already exists! ==========|\n")
    setwd(folderLS)
    dirLS = dir(folderLS)
    n_LS = length(dirLS)
  } else {
    cat("\n|========== Creating the Labels Space folder! ==========|\n")
    dir.create(folderLS)
    setwd(folderLS)
    dirLS = dir(folderLS)
    n_LS = length(dirLS)
  }
  
  # Creating the folder ATTRIBUTES SPACE
  folderAS = paste(FolderRoot, "/Datasets/AttributesSpace", sep="")
  if(dir.exists(folderAS) == TRUE){
    cat("\n|========== Attributes Space folder already exists! ==========|\n")
    setwd(folderAS)
    dirAS = dir(folderAS)
    n_AS = length(dirAS)
  } else {
    cat("\n|========== Creating the Attributes Space folder! ==========|\n")
    dir.create(folderAS)
    setwd(folderAS)
    dirAS = dir(folderAS)
    n_AS = length(dirAS)
  }
  
  #
  folderL = paste(FolderRoot, "/Datasets/Labels", sep="")
  if(dir.exists(folderL) == TRUE){
    cat("\n|========== Labels folder already exists! ==========|\n")
    setwd(folderL)
    dirL = dir(folderL)
    n_L = length(dirL)
  } else {
    cat("\n|========== Creating the Labels folder! ==========|\n")
    dir.create(folderL)
    setwd(folderL)
    dirL = dir(folderL)
    n_L = length(dirL)
  }
  
  #
  folderS = paste(FolderRoot, "/Datasets/Statistics", sep="")
  if(dir.exists(folderS) == TRUE){
    cat("\n|========== Statistics folder already exists! ==========|\n")
    setwd(folderS)
    dirS = dir(folderS)
    n_S = length(dirS)
  } else {
    cat("\n|========== Creating the Statistics folder! ==========|\n")
    dir.create(folderS)
    setwd(folderS)
    dirS = dir(folderS)
    n_S = length(dirS)
  }
  
  #
  folderSummary = paste(FolderRoot, "/Datasets/Summary", sep="")
  if(dir.exists(folderSummary) == TRUE){
    cat("\n|========== Summary folder already exists! ==========|\n")
    setwd(folderS)
    dirSummary = dir(getwd())
    n_Summary = length(dirSummary)
  } else {
    cat("\n|========== Creating the Summary folder! ==========|\n")
    dir.create(folderSummary)
    setwd(folderSummary)
    dirSummary = dir(folderSummary)
    n_Summary = length(dirSummary)
  }
  
  #
  folderIL = paste(FolderRoot, "/Datasets/InstancesPerLabels", sep="")
  if(dir.exists(folderIL) == TRUE){
    cat("\n|========== Instances per Labels folder already exists! ==========|\n")
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  } else {
    cat("\n|========== Creating the Instances per Labels  folder! ==========|\n")
    dir.create(folderIL)
    setwd(folderIL)
    dirIL = dir(folderIL)
    n_IL = length(dirIL)
  }
  
  #
  folderILS = paste(FolderRoot, "/Datasets/InstancesPerLabelsSpace", sep="")
  if(dir.exists(folderILS) == TRUE){
    cat("\n|========== Instances per Labels Space folder already exists! ==========|\n")
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  } else {
    cat("\n|========== Creating the Instances per Labels Space folder! ==========|\n")
    dir.create(folderILS)
    setwd(folderILS)
    dirILS = dir(folderILS)
    n_ILS = length(dirILS)
  }
  
  # 
  folderCSV = paste(FolderRoot, "/Datasets/CSV", sep="")
  if(dir.exists(folderCSV) == TRUE){
    cat("\n|========== CSV folder already exists! ==========|\n")
    setwd(folderCSV)
    dirCSV = dir(folderCSV)
    n_CSV = length(dirCSV)
  } else {
    cat("\n|========== Creating the CSV folder! ==========|\n")
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
infoDataSet <- function(a){
  retorno = list()
  retorno$id = a$ID
  retorno$ds = a$DS
  retorno$name = a$Name
  retorno$domain = a$Domain
  retorno$instances = a$Instances
  retorno$labels = a$Labels
  retorno$predictiveAttributes = a$PredictiveAttributes
  retorno$attributesTotal = a$AttributesTotal
  retorno$attStart = a$AttStart
  retorno$attEnd = a$AttEndt
  retorno$labStart = a$LabStart
  retorno$labEnd = a$LabEnd
  retorno$nominal = a$Nominal
  retorno$numeric = a$Numeric
  retorno$cardinality = a$Dardinality
  retorno$density = a$Density
  retorno$distinct = a$Distinct
  retorno$dimensionality = a$Dimensionality
  retorno$xn = a$xn
  retorno$yn = a$yn
  retorno$gridn = a$gridn
  retorno$xt = a$xt
  retorno$yt = a$yt
  retorno$gridt = a$gridt
  return(retorno)
}





##################################################################################################
# FUNCTION FOLD NAMES                                                                            #
# Objective:                                                                                     #
#     Get data set file names                                                                    #
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     fileNames: a vector with file names                                                        #
##################################################################################################
fileNames <- function(){
  d = directories()
  fileNames <- c(dir(d$folderCSV))
  return(fileNames)
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
folderNames <- function(fileNames){
  result = list()
  folderNames = fileNames
  # Deleting ".csv" of the names (ofthe files)
  j = 0
  for(j in 1:length(folderNames)){
    a = str_length(folderNames[j])
    a = a - 4
    folderNames[j] = str_sub(folderNames[j], end = a)  
    j = j + 1
    gc()
  }
  numberDataSets = length(folderNames)
  result$folderNames = folderNames
  result$numberDataSets = numberDataSets
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
  setwd(d$folderLS)
  arquivo = read.csv(dataset)
  labels = c(colnames(arquivo))
  return(labels)
}

