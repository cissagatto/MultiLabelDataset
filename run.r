##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 3 - Run                                                                              #
##################################################################################################




####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library("stringr")



####################################################
# INTERNAL LIBRARIES                               #
####################################################
source("utils.r")
source("MultiLabelDataset.r")


####################################################
# PASTA DE TRABALHO ATUAL                           #
####################################################
sf = setFolder()
FolderRoot = sf$Folder
setwd(FolderRoot)




####################################################
# EXECUTE                                          #
####################################################
diretorios = directories()

# escolher TIPO igual a TRAIN ou TEST ou ALL
# TRAIN = conjunto de dados de treino
# TEST = conjunto de dados de teste
# ALL = todo o conjunto de dados
conjunto = "train"

fin = c(diretorios$dirCSV)
fon = folderNames(c(fn), tipo = conjunto)
fnf = fileNamesFinal(c(fon))

setwd(FolderRoot)
nome_ = paste("datasets-", conjunto, ".csv", sep="")
datasets = read.csv(nome)

dataset = c(0)
card = c(0)
dens = c(0)
dime = c(0)
instancias = c(0)
dados = data.frame(dataset, card, dens, dime, instancias)

i = 1
for(i in 1:diretorios$n_CSV){
  cat("\n Dataset: ", fn[i], "\n")
  ds = datasets[i,]
  info = infoDataSet(ds)
  setwd(diretorios$folderCSV)
  tudo = read.csv(fn[i])
  instancias = nrow(tudo)
  
  # atributos previsores
  cat("\nSeparando os atributos previsores!")
  setwd(diretorios$folderAS)
  atributos = data.frame(tudo[,info$attStart:info$attEnd])
  write.csv(atributos, paste(fon[i], "_attributes_", conjunto, ".csv", sep=""), row.names = FALSE)
  
  # atributos alvo
  cat("\nSeparando os atributos alvo!")
  setwd(diretorios$folderLS)
  classes = data.frame(tudo[,info$labStart:info$labEnd])
  write.csv(classes, paste(fon[i], "_labels_", conjunto, ".csv", sep=""), row.names = FALSE)
  
  # salvando os rotulos
  cat("\nSeparando os nomes dos atributos alvo!")
  rotulos = c(colnames(classes))
  setwd(diretorios$folderL)
  write.csv(rotulos, paste(fon[i], "_labels_only.csv", sep=""))
  
  # cardinalidade
  cat("\nCalculando a cardinalidade!")
  card = cardinality(classes)
  
  # densidade
  cat("\nCalculando a densidade!")
  dens = density(classes)
  
  # dimensionalidade
  cat("\nCalculando a dimensionalidade!")
  dimen = as.numeric(info$predictiveAttributes/info$instances)
  
  # salvando os numeros
  cat("\nSalvando as informacoes!")
  dataset = fn[i]
  dados = rbind(dados, data.frame(dataset, card, dens, dime, instancias))
  setwd(sf$Folder)
  write.csv(dados, "sumario_datasets_", conjunto, ".csv", append = TRUE)
  
  # limpar
  cat("\nLimpando os objetos criados!")
  rm(tudo)
  rm(atributos)
  rm(classes)
  rm(rotulos)
  
  cat("\nIncrementando!")
  i = i + 1
  
  cat("\nColetando lixo!\n")
  gc()
}

j = 1
for(j in 1:diretorios$n_CSV){
  cat("\n Dataset: ", fn[j], "\n")
  ds = datasets[j,]
  info = infoDataSet(ds)  
  instancesPerLabels(ds$ID[j], fon[j], fn[j], fnf[j], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd, ds$Instances, 
                     ds$Labels, conjunto)  
  instancesPerLabelsSpace(ds$ID[j], fon[j], fn[j], fnf[j], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd, ds$Instances, 
                          ds$Labels, conjunto)  
  j = j + 1
  gc()
}
