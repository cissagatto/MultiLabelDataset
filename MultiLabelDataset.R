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
library("stringr")
library("plyr")
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
# FUNCTION INSTANCES PER LABELS                                                                  #
# Objective:                                                                                     #
#     Separate each instance per label of all dataset                                            #
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
# Folders Created:                                                                               #
#     /Documents/Kohonen/Datasets/InstancesPerLabels/[Dataset]                                   #
# Files Created:                                                                                 #
#     [label_i].csv                                                                              #
##################################################################################################
instancesPerLabels <- function(id, folderName, fileName, fileNameFinal, attStart, attEnd, labelStart, 
                               labelEnd, instances, labels, conjunto){
  
  cat("\n|========== START INSTANCES PER LABELS ==========|\n")   
  
  # gett inf info about directories
  diretorios = directories()
  
  # creating data frame to save info
  rotulo = c(0)
  valor = c(0)
  porcentagem = c(0)
  total = data.frame(rotulo, valor, porcentagem)  
  
  # Create specifc folder to save the instances
  folder0 = paste(diretorios$folderIL, "/", folderName, sep="")
  dir.create(folder0)  
  
  # obtendo os nomes dos rotulos
  setwd(diretorios$folderL)
  # arts1_labels_only.csv
  nomeArquivo = paste(folderName, "_labels_only.csv", sep="")
  rotulosArquivo = data.frame(read.csv(nomeArquivo))
  names(rotulosArquivo) = c("numero","label")
  rotulosArquivo = rotulosArquivo$label
  
  # Setting the folder
  setwd(diretorios$folderCSV)
  
  # Oppening the file
  arquivo = data.frame(read.csv(fileName), stringsAsFactors = F)
  
  # inicio dos r?tulos
  inicio = labelStart
  
  # fim dos r?tulos
  fim = labelEnd
  
  k = 1
  for(k in 1:labels){
    label_ = toString(rotulosArquivo[k])
    cat("\nRotulo:", label_)
    
    result = data.frame(arquivo[arquivo[inicio]==1,])
    
    setwd(folder0)
    write.csv(result, paste(label_, "_", conjunto, "_.csv", sep=""))
    
    rotulo = label_
    valor = nrow(result)
    porcentagem = valor/instances
    total = rbind(total, data.frame(rotulo, valor, porcentagem))
    
    setwd(diretorios$folderSummary)
    nome = paste(folderName, "_sumario_", conjunto, "_ipl.csv", sep="")
    write.csv(total, nome, sep="", append = TRUE)
    
    inicio = inicio + 1
    k = k + 1
    gc()
  }
  
  cat("\n|========== END INSTANCES PER LABELS ==========|\n")
  
  gc()
}




##################################################################################################
# FUNCTION INSTANCES PER LABELS                                                                  #
# Objective:                                                                                     #
#     Separate each instance per label of all dataset                                            #
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
# Folders Created:                                                                               #
#     /Documents/Kohonen/Datasets/InstancesPerLabels/[Dataset]                                   #
# Files Created:                                                                                 #
#     [label_i].csv                                                                              #
##################################################################################################
instancesPerLabelsSpace <- function(id, folderName, fileName, fileNameFinal, attStart, attEnd, labelStart, 
                                    labelEnd, instances, labels, conjunto){
  
  cat("\n|========== START INSTANCES PER LABELS ==========|\n")   
  
  # gettinf info about directories
  diretorios = directories()
  
  # criando pasta para salvar o sumário do dataset
  setwd(diretorios$folderSummary)
  subFolderSu = paste(diretorios$folderSummary, "/", folderName, sep="")
  dir.create(subFolderSu)
  
  # criando pasta para salvar a estatística do dataset
  setwd(diretorios$folderStatistics)
  subFolderSta = paste(diretorios$folderStatistics, "/", folderName, sep="")
  dir.create(subFolderSta)
  
  # criando pasta para salvar as instancias por rotulo
  setwd(diretorios$folderILS)
  subFolderILS = paste(diretorios$folderILS, "/", folderName, sep="")
  dir.create(subFolderILS)
  
  # criando data frame para salvar o total de instancias por rotulo
  rotulo = c(0)
  valor = c(0)
  porcentagem = c(0)
  total = data.frame(rotulo, valor, porcentagem)  
  
  # criando data frame para salvar o sumário do dataset
  soma_ = c(0)
  minimo_ = c(0)
  maximo_ = c(0)
  media_ = c(0)
  mediana_ = c(0)
  sd_ = c(0)
  final = data.frame(soma_, minimo_, maximo_, media_, mediana_, sd_)
  
  # obtendo os nomes dos rotulos
  setwd(diretorios$folderL)
  # arts1_labels_only.csv
  nomeArquivo = paste(folderName, "_labels_only.csv", sep="")
  rotulosArquivo = data.frame(read.csv(nomeArquivo))
  names(rotulosArquivo) = c("numero","label")
  rotulosArquivo = rotulosArquivo$label
  
  # Setting the folder
  setwd(diretorios$folderLS)
  namae = paste(folderName, "_labels_train.csv", sep="")
  arquivo = data.frame(read.csv(namae, stringsAsFactors = F))
 
  setwd(subFolderSu)
  
  # soma por linha (instancia)
  soma1 = data.frame(apply(arquivo, 1, sum))
  names(soma1) = c("instancias", "soma")
  write.csv(soma1, paste("arquivo_instancia_", conjunto, ".csv", sep=""))
  
  # soma por coluna (rotulos)
  soma2 = data.frame(apply(arquivo, 2, sum))
  names(soma2) = c("rotulos", "soma")
  write.csv(soma2, paste("arquivo_rotulo_", conjunto, ".csv", sep=""))
  
  # inicio dos rotulos
  inicio = labelStart
  
  # fim dos rotulos
  fim = labelEnd
  
  # passando por todos os rótulos
  k = 1
  for(k in 1:labels){
    
    label_ = toString(rotulosArquivo[k])
    
    cat("\nRotulo:", label_)
    
    result = data.frame(arquivo[arquivo[k]==1,])
    
    # salvando as intancias especificas do rotulo especifico
    setwd(subFolderILS)
    write.csv(result, paste(label_, "_", conjunto, ".csv", sep=""))
    
    # salvando informações de sumário
    setwd(subFolderSu)
    rotulo = label_
    valor = nrow(result)
    porcentagem = valor/instances
    total = rbind(total, data.frame(rotulo, valor, porcentagem))
    nome = paste(folderName, "_", label_, "_sumario_", conjunto, ".csv", sep="")
    write.csv(total, nome, append = TRUE)     
    
    # salvando informações de estatística
    setwd(subFolderSta)
    
    # soma por linha
    soma = data.frame(apply(result, 1, sum))
    write.csv(soma, paste(label_, "_instancia_soma_", conjunto, ".csv", sep=""))
    
    # mÃƒÂ©dia por linha
    media = data.frame(apply(result, 1, mean))
    write.csv(media, paste(label_, "_instancia_media_", conjunto, ".csv", sep=""))
    
    # mediana por linha
    mediana = data.frame(apply(result, 1, median))
    write.csv(mediana, paste(label_, "_instancia_mediana_", conjunto, ".csv", sep=""))
    
    # desvio padrÃƒÂ£o por linha
    desvioPadrao = data.frame(apply(result, 1, sd))
    write.csv(desvioPadrao, paste(label_, "_instancia_media_", conjunto, ".csv", sep=""))
    
    # soma por coluna
    soma_ = data.frame(apply(result, 2, sum))
    names(soma_) = "soma"
    
    # mÃƒÂ¡ximo por coluna
    maximo_ = data.frame(apply(result, 2, max))
    names(maximo_) = "maximo"
    
    # mÃƒnimo por coluna
    minimo_ = data.frame(apply(result, 2, min))
    names(minimo_) = "minimo"
    
    # media por coluna
    media_ = data.frame(apply(result, 2, mean))
    names(media_) = "media"
    
    # mediana por coluna
    mediana_ = data.frame(apply(result, 2, median))
    names(mediana_) = "mediana"
    
    # desvio padrÃƒÂ£o por coluna
    sd_ = data.frame(apply(result, 2, sd))
    names(sd_) = "desvioPadrao"
    
    final = data.frame(soma_, minimo_, maximo_, media_, mediana_, sd_)
    
    nome2 = paste("estatisticas_", label_, "_", conjunto, ".csv", sep="")
    write.table(final, nome2, row.names = TRUE, col.names = TRUE, sep=",")   
    
    k = k + 1
    gc()
  }
  
  setwd(folderSta)
  arquivos = c(dir(folderSta))
  n_arquivos = length(arquivos)
  
  arquivo = read.csv(paste("arquivo_rotulo_", conjunto, ".csv", sep=""))
  names(arquivo) = c("label", "soma")
  n = nrow(arquivo)
  teste = count(arquivo, vars=arquivo$soma)
  names(teste) = c("soma", "frequencia")
  write.table(teste, paste("frequencia_", conjunto, ".csv", sep=""), col.names = TRUE)
  
  cat("\n|========== END INSTANCES PER LABELS ==========|\n")
  
  gc()
}
