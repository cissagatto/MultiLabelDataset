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
sistema = c(Sys.info())
if (sistema[1] == "Linux"){
  Folder = getwd()
  setwd(Folder)
} else {
  Folder = getwd()
  setwd(Folder)
}

FolderRoot = Folder


setFolder <- function(){
  retorno = list()
  sistema = c(Sys.info())
  if (sistema[1] == "Linux"){
    Folder = paste("/home/", sistema[7], "/MultiLabelDataset", sep="")
    setwd(Folder)
  } else {
    Folder = paste("C:/users/", sistema[7], "/MultiLabelDataset", sep="")
    setwd(Folder)
  }
  retorno$sistema = sistema
  retorno$Folder = Folder
  return(retorno)
}



##################################################################################################
# Function to set the folder according to your operational system                                #
##################################################################################################
directories <- function(){
  FolderDataset = paste(FolderRoot, "/DataSets", sep="")
  setwd(FolderDataset)
  dirDataset = dir(FolderDataset)
  n = length(dirDataset)
}