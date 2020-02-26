##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 2 - Separating Label Space from Attributes                                           #
##################################################################################################




####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library("caret")
library("ISLR")
library("dplyr")



####################################################
# INTERNAL LIBRARIES                               #
####################################################
source("utils.r")
sf = setFolder()
FolderRoot = sf$Folder
setwd(FolderRoot)





##################################################################################################
# FUNCTION CARDINALITY                                                                           #
# Objective:                                                                                     #
#     Compute the cardinality                                                                    #
# Parameters:                                                                                    #
#     Classes = labels space                                                                     #
# Return:                                                                                        #
#     card = the cardinality                                                                     #
# Folders Created:                                                                               #
#     None                                                                                       #
# Files Created:                                                                                 #
#     None                                                                                       #
##################################################################################################
cardinality <- function(classes){
  add = 0  
  r = nrow(classes)
  c = ncol(classes)
  for(i in 1:r){
    for(j in 1:c){
      add = add + sum(classes[i,j])
    }
    gc()
  }
  card = (1/r) * (add)
  return(card)
  gc()
}




##################################################################################################
# FUNCTION DENSITY                                                                               #
# Objective:                                                                                     #
#     Compute the density                                                                        #
# Parameters:                                                                                    #
#     Classes = labels space                                                                     #
# Return:                                                                                        #
#     dens = the density                                                                         #
# Folders Created:                                                                               #
#     None                                                                                       #
# Files Created:                                                                                 #
#     None                                                                                       #
##################################################################################################
density <- function(classes){
  add = 0
  r = nrow(classes)
  c = ncol(classes)  
  for(i in 1:r){
    for(j in 1:c){
      add = add + (sum(classes[i,j])/c)
    }
    gc()
  }
  den = (1/r) * (add)
  return(den)
  gc()
}





##################################################################################################
# FUNCTION DENSITY                                                                               #
# Objective:                                                                                     #
#     Compute the density                                                                        #
# Parameters:                                                                                    #
#     Classes = labels space                                                                     #
# Return:                                                                                        #
#     dens = the density                                                                         #
# Folders Created:                                                                               #
#     None                                                                                       #
# Files Created:                                                                                 #
#     None                                                                                       #
##################################################################################################

cat("\n|========== START Separated Labels Space and Attributes Space ==========|\n")

d = directories()

dataset = c(0)
card = c(0)
dens = c(0)
dime = c(0)
dados = data.frame(dataset, card, dens, dime)

setwd(FolderRoot)
datasets = read.csv("datasets.csv")

fn = fileNames()
ffnn = folderNames(d$folderCSV)

i = 1
for(i in 1:d$n_CSV){
  cat("\n Dataset: ", fn$fileNames[i])
  ds = datasets[i,]
  info = infoDataSet(ds)
  setwd(d$folderCSV)
  tudo = read.csv(fn$fileNames[i])
  
  # atributos
  setwd(d$folderAS)
  atributos = data.frame(tudo[,info$attStart:info$attEnd])
  write.csv(atributos, paste(ffnn$folderNames[i], ".csv"), row.names = FALSE)
  
  # rotulos
  setwd(d$folderLS)
  classes = data.frame(tudo[,info$labStart:info$labEnd])
  write.csv(classes, paste(ffnn$folderNames[i], ".csv"), row.names = FALSE)
  
  # salvando os rótulos
  rotulos = c(colnames(classes))
  setwd(d$folderL)
  write.csv(rotulos, paste(ffnn$folderNames[i], ".csv"))
  
  # cardinalidade
  card = cardinality(classes)
  
  # densidade
  dens = density(classes)
  
  # dimensionalidade
  dimen = as.numeric(info$predictiveAttributes/info$instances)
  
  # salvando os números
  dados = rbind(dados, data.frame(ffnn$folderNames[i], card, dens, dime))
  setwd(sf$Folder)
  write.csv(dados, "sumario_datasets.csv", append = TRUE)
  
  # limpar
  rm(tudo)
  rm(atributos)
  rm(classes)
  rm(rotulos)
  
  i = i + 1
  
  gc()
}