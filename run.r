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
library("plyr")
library("dplyr")
library("tidyverse")
library("utiml")
library("mldr")




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
fon = folderNames(c(fin), tipo = conjunto)
fnf = fileNamesFinal(c(fon))

setwd(FolderRoot)
datasets = data.frame(read.csv("datasets.csv"))

dataset = c(0)
card = c(0)
dens = c(0)
dime = c(0)
instancias = c(0)
dados = data.frame(dataset, card, dens, dime, instancias)


####################################################
# Separando os espaÃƒÂ§os                             #
####################################################
i = 1
for(i in 1:diretorios$n_CSV){
  nome = toString(fin[i])
  cat("\n Dataset: ", nome , "\n")
  
  ds = datasets[i,]
  print(ds)
  
  info = infoDataSet(ds)
  print(info)
  
  setwd(diretorios$folderCSV)
  tudo = read.csv(nome)
  instancias = nrow(tudo)

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
  dataset = nome
  dados = rbind(dados, data.frame(dataset, card, dens, dime, instancias))
  setwd(sf$Folder)
  write.csv(dados, "sumario_datasets_", conjunto, ".csv", append = TRUE)

  
  instancesPerLabels(ds$ID[i], fon[i], fin[i], finf[i], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd, ds$Instances, ds$Labels, conjunto)  
  
  instancesPerLabelsSpace(ds$ID[i], fon[i], fin[i], fnf[i], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd, ds$Instances, ds$Labels, conjunto)  
  
  
  FolderE = paste(diretorios$folderFolds, "/", fon[i], sep="")
  dir.create(FolderE)
  setwd(FolderE)
  
  mdata = mldr_from_dataframe(tudo, labelIndices = c(ds$LabStart, ds$LabEnd))
  kfcv <- create_kfold_partition(mdata, k=10, "iterative")
  write_rds(kfcv, "crossvalidation.rds")

  e = 1
  set.seed(1234)
  for(e in 1:10){
    pasta = partition_fold(kfcv, 1)
    setwd(FolderE)
    p1 = data.frame(pasta$train$dataset)
    w1 = paste(fon[i], "_", conjunto, "_fold_", e, "_train.csv", sep="")
    write.csv(p1,w1)
    p2 = data.frame(pasta$test$dataset)
    w2 = paste(fon[i],"_fold_", e, "_test.csv", sep="")
    write.csv(p2,w2)
    e = e + 1
    gc()
  }
  
  cat("\nIncrementando!")
  i = i + 1
  
  # limpar
  cat("\nLimpando os objetos criados!")
  rm(tudo)
  rm(classes)
  rm(rotulos)
  
  cat("\nColetando lixo!\n")
  gc()
}
