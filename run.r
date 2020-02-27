  ##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of Sao Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 2 - Run                                                                              #
##################################################################################################




####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library("stringr")



####################################################
# INTERNAL LIBRARIES                               #
####################################################
source("utils.r")
sf = setFolder()
FolderRoot = sf$Folder
setwd(FolderRoot)




####################################################
# EXECUTE                                          #
####################################################
cat("\n|========== START Separated Spaces ==========|\n")

d = directories()
setwd(FolderRoot)
datasets = read.csv("datasets.csv")
fn = fileNames()
ffnn = folderNames(c(fn$fileNames))
fnf = fileNamesFinal(c(ffnn$folderNames))

dataset = c(0)
card = c(0)
dens = c(0)
dime = c(0)
dados = data.frame(dataset, card, dens, dime)

i = 1
for(i in 1:d$n_CSV){
  cat("\n Dataset: ", fn$fileNames[i], "\n")
  ds = datasets[i,]
  info = infoDataSet(ds)
  setwd(d$folderCSV)
  tudo = read.csv(fn$fileNames[i])
  
  # atributos previsores
  cat("\nSeparando os atributos previsores!")
  setwd(d$folderAS)
  atributos = data.frame(tudo[,info$attStart:info$attEnd])
  write.csv(atributos, paste(ffnn$folderNames[i], ".csv", sep=""), row.names = FALSE)
  
  # atributos alvo
  cat("\nSeparando os atributos alvo!")
  setwd(d$folderLS)
  classes = data.frame(tudo[,info$labStart:info$labEnd])
  write.csv(classes, paste(ffnn$folderNames[i], ".csv", sep=""), row.names = FALSE)
  
  # salvando os rotulos
  cat("\nSeparando os nomes dos atributos alvo!")
  rotulos = c(colnames(classes))
  setwd(d$folderL)
  write.csv(rotulos, paste(ffnn$folderNames[i], ".csv", sep=""))
  
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
  dataset = fn$fileNames[i]
  dados = rbind(dados, data.frame(dataset, card, dens, dime))
  setwd(sf$Folder)
  write.csv(dados, "sumario_datasets.csv", append = TRUE)
  
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

cat("\n|========== START Separated Instances Per Labels ==========|\n")

j = 1
for(j in 1:d$n_LS){
  cat("\n Dataset: ", fn$fileNames[j], "\n")
  ds = datasets[j,]
  info = infoDataSet(ds)
  
  instancesPerLabels(ffnn$folderNames[j], fn$fileNames[j], fnf[j], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd,
                     ds$Instances, ds$Labels)
  
  # instancesPerLabelsSpace(ffnn$folderNames[j], fn$fileNames[j], fnf[j], ds$AttStart, ds$AttEnd, ds$LabStart, ds$LabEnd, ds$Instances, ds$Labels)
  j = j + 1
  gc()
}