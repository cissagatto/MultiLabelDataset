##################################################################################################
# Elaine Cecilia Gatto | Prof. Dr. Ricardo Cerri | Prof. Dr. Mauri Ferrandin                     #
# www.professoracissagatto.com.br                                                                #
# Federal University of São Carlos (UFSCar: https://www2.ufscar.br/) Campus Sao Carlos           #
# Computer Department (DC: https://site.dc.ufscar.br/)                                           #
# Program of Post Graduation in Computer Science (PPG-CC: http://ppgcc.dc.ufscar.br/)            #
# Bioinformatics and Machine Learning Group (BIOMAL: http://www.biomal.ufscar.br/)               #
# Algorithm 2 - Pre Processing                                                                   #
##################################################################################################




####################################################
# EXTERNAL LIBRARIES                               #
####################################################
library(stringr)




####################################################
# INTERNAL LIBRARIES                               #
####################################################
source("utils.r")
sf = setFolder()
FolderRoot = sf$Folder
setwd(FolderRoot)




##################################################################################################
# FUNCTION INSTANCES PER LABELS CLASS SPACE                                                      #
# Objective:                                                                                     #
#     Separated the instances for each label only for the class space                            #
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
# Folder Created:                                                                                #
#     /Documents/Kohonen/Datasets/InstancesPerLabelsClassSpace/[Dataset]                         # 
# Files Created:                                                                                 #
#     [label_i].csv                                                                              #
##################################################################################################
instancesPerLabelsClassSpace <- function(folderName, fileName, PredictiveAttributes, 
                                         TotalAttributes, instances, labels){
  
  cat("\n|========== START: Separated Instances per Label Class Space ==========|\n")   
  
  d = directories()
  rotulo = c(0)
  valor = c(0)
  total = data.frame(rotulo, valor)  
  
  # Create specifc folder to save information about dataset
  folder0 = paste(d$folderILS, "/", folderName, sep="")
  dir.create(folder0)  
  
  # Setting the folder
  setwd(d$folderLS)
  fileName = "arts1.csv"
  # Oppening the file
  arquivo = data.frame(read.csv(fileName), stringsAsFactors = F)
  
  # obtendo os nomes dos rótulos
  nomesRotulos = c(labelsNames(fileName))
  
  # total de atributos previsores
  inicio = PredictiveAttributes
  
  # total de atributos do dataset (classes + previsores)
  fim = TotalAttributes   
  
  # Setting variables need in a loop
  n = 1
  u = 1
  linha = 1
  coluna = 1
  
  # While n <= labels then do
  while(n<=labels){
    
    cat("\n| RÓTULO: ", n)
    
    conta = 0
    
    dados = arquivo
    # Create a new data frame equal to original file
    dados = arquivo[-c(1:instances),]
    
    # From line 1 to line 7484 do
    for(linha in 1:instances){
      # cat("\nLinha:", linha)
      
      # Analize only the specific column
      # Start in column 1
      # After all of lines, go to the next column
      for(coluna in coluna){
        # cat("\nColuna:", coluna)
        
        # if the value in this cell is equal to 1 do
        if(arquivo[linha, coluna] == 1){
          
          conta = conta + 1
          
          # join the data frames
          dados = rbind(dados, arquivo[linha,])
          
          # Setting the folder to save files
          setwd(folder2)
          
          # write the file with the values of this label
          write.csv(dados, paste(nomesRotulos[coluna], ".csv", sep=""))
          
        } else {
          
        } # END IF/ELSE
      } # END FOR COLUMN
      
      # increment the line
      linha = linha + 1
      gc()
    } # END FOR LINE
    
    cat("\n ROTULO =", nomesRotulos[u], "CONTA = ", conta)
    rotulo = nomesRotulos[u]
    valor = conta
    total = rbind(total, data.frame(rotulo, valor))
    setwd(folder2)
    write.csv(total, "sumario.csv", sep="", append = TRUE)
    
    # increment the column
    coluna = coluna + 1
    
    # increment n
    n = n + 1
    
    # increment u
    u = u + 1
    
    gc()
  } # END WHILE
  
  cat("\n|========== END: Separated Instances per Label Class Space ==========|\n")
  
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
instancesPerLabels <- function(folderName, fileName, PredictiveAttributes, 
                               TotalAttributes, instances, labels){
  
  cat("\n|========== START: Separated Instances per Label ==========|\n")   
  
  # gettinf info about directories
  d = directories()
  
  # creating data frame to save info
  rotulo = c(0)
  valor = c(0)
  total = data.frame(rotulo, valor)  
  
  # Create specifc folder to save the instances
  folder0 = paste(d$folderILS, "/", folderName, sep="")
  dir.create(folder0)  
  
  # obtendo os nomes dos rótulos
  nomesRotulos = c(labelsNames(fileName))
  
  # Setting the folder
  setwd(d$folderCSV)
  
  # Oppening the file
  arquivo = data.frame(read.csv(fileName), stringsAsFactors = F)
  
  # total de atributos previsores
  inicio = PredictiveAttributes
  
  # total de atributos do dataset (classes + previsores)
  fim = TotalAttributes   
  
  # Setting variables need in a loop
  n = 1
  u = 1
  linha = 1
  coluna = 1
  
  # While n <= labels then do
  while(n<=labels){
    
    cat("\n| RÓTULO:", n, "|")
    
    conta = 0
    
    dados = arquivo
    # Create a new data frame equal to original file
    dados = arquivo[-c(1:instances),]
    
    # From line 1 to line 7484 do
    for(linha in 1:instances){
      # cat("\nLinha:", linha)
      
      # Analize only the specific column
      # Start in column 1
      # After all of lines, go to the next column
      for(coluna in coluna){
        # cat("\nColuna:", coluna)
        
        # if the value in this cell is equal to 1 do
        if(arquivo[linha, coluna] == 1){
          
          conta = conta + 1
          
          # join the data frames
          dados = rbind(dados, arquivo[linha,])
          
          # Setting the folder to save files
          setwd(folder2)
          
          # write the file with the values of this label
          write.csv(dados, paste(nomesRotulos[coluna], ".csv", sep=""))
          
        } else {
          
        } # END IF/ELSE
      } # END FOR COLUMN
      
      # increment the line
      linha = linha + 1
      gc()
    } # END FOR LINE
    
    rotulo = nomesRotulos[u]
    valor = conta
    total = rbind(total, data.frame(rotulo, valor))
    setwd("~/Documents/Kohonen/Datasets/Summary")
    nome = paste(folderName, "sumario.csv", sep="")
    write.csv(total, nome, sep="", append = TRUE)
    
    # increment the column
    coluna = coluna + 1
    
    # increment n
    n = n + 1
    
    # increment u
    u = u + 1
    
    gc()
  } # END WHILE
  
  cat("\n|========== END: Separated Instances per Label Class Space ==========|\n")
  
  gc()
}




##################################################################################################
# FUNCTION STATISTICS                                                                            #
# Objective:                                                                                     #
#     Compute some statistics about the datasets in general                                      #
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
# Folders Created:                                                                               #
#     /Documents/Kohonen/Datasets/Statistics[Dataset]                                            #
# Files Created:                                                                                 #
#    sumario.csv                                                                                 #
#    [Statistics_Label_i].csv                                                                    #
##################################################################################################
statistics <- function(){
  
  cat("\n|========== START FUNCTION: Statistics ==========|\n")
  
  setwd("~/Documents/Kohonen")
  
  datasets = data.frame(read.csv("datasets.csv"))
  
  folder2 = "~/Documents/Kohonen/Datasets/InstancesPerLabelsClassSpace"
  setwd(folder2)
  dir2 = c(dir(folder2))
  m = length(dir2)
  
  label = c()
  minimo = c()
  maximo = c()
  media = c()
  mediana = c()
  porcentagem = c()
  estatistica = data.frame(label, minimo, maximo, media, mediana, porcentagem)
  
  rotulo = c(0)
  instancias = c(0)
  porcentagem = c(0)
  sumario = data.frame(rotulo, instancias, porcentagem)
  
  j = 1
  for(j in j:m){
    nome = dir2[j]
    folder4 = paste("~/Documents/Kohonen/Datasets/InstancesPerLabelsClassSpace/", nome, sep="")
    dir4 = c(dir(folder4))
    setwd(folder4)
    u = length(dir4)
    a = datasets[j,]
    
    folder5 = "~/Documents/Kohonen/Datasets/Statistics/"
    setwd(folder5)
    folder6 = paste(folder5, nome, sep="")
    dir.create(folder6)
    
    # para o rótulo de 1 até o fim
    k = 1
    while(k <= u){
      setwd(folder4)
      arquivo = read.csv(dir4[k]) 
      arquivo$X = NULL
      
      totalLinhas = nrow(arquivo)
      totalColunas = ncol(arquivo)
      porcentagem = totalLinhas/a$Instances
      
      rotulo = dir4[k]
      instancias = totalLinhas
      sumario = rbind(sumario, data.frame(rotulo, instancias, porcentagem))
      
      setwd(folder6)
      
      # TOTAL LABELS
      total = data.frame(apply(arquivo, 1, sum))
      write.csv(total, "Instancia_Total.csv")
      
      # AVERAGE LABELS
      media = data.frame(apply(arquivo, 1, mean))
      write.csv(media, "Instancia_Media.csv")
      
      # AVERAGE LABELS
      mediana = data.frame(apply(arquivo, 1, median))
      write.csv(mediana, "Instancia_Mediana.csv")
      
      # AVERAGE LABELS
      desvioPadrao = data.frame(apply(arquivo, 1, sd))
      write.csv(desvioPadrao, "Instancia_Desvio.csv")
      
      # soma por coluna
      somaC = data.frame(apply(arquivo, 2, sum))
      names(somaC) = "soma"
      
      maxC = data.frame(apply(arquivo, 2, max))
      names(maxC) = "maximo"
      
      minC = data.frame(apply(arquivo, 2, min))
      names(minC) = "minimo"
      
      mediaC = data.frame(apply(arquivo, 2, mean))
      names(mediaC) = "media"
      
      medianC = data.frame(apply(arquivo, 2, median))
      names(medianC) = "mediana"
      
      sdC = data.frame(apply(arquivo, 2, sd))
      names(sdC) = "desvioPadrao"
      
      final = cbind(somaC, maxC, minC, medianC, mediaC, sdC)
      
      # salvando informações
      nome2 = paste("Rotulos_", dir4[k], sep="")
      write.csv(final, nome2)
      
      k = k + 1
      gc()
    }
    
    write.csv(sumario, "sumario.csv")
    
    j = j + 1
    gc()
  }
  
  cat("\n|========== END FUNCTION: Statistics ==========|\n")
  
  gc()
}



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
    gc(0)
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
# FUNCTION CARD DENS                                                                             #
# Objective:                                                                                     #
#     Compute the cardinality and the density
# Parameters:                                                                                    #
#     None                                                                                       #
# Return:                                                                                        #
#     None                                                                                       #
# Folders Created:                                                                               #
#     None                                                                                       #
# Files Created:                                                                                 #
#     cardinality.txt                                                                            #
#     density.txt                                                                                #
##################################################################################################
cardDen <- function(){
  
  cat("\n|========== START FUNCTION: Cardinality and Density ==========|\n")
  
  # Setting the directory datasets
  setwd("~/Documents/Kohonen/Datasets/Labels_Space")
  
  # Creating an array with the names of the datasets 
  # This compute density and cardinality for all datasets in this folder!
  archives <- c(dir("~/Documents/Kohonen/Datasets/Labels_Space"))
  print(archives)
  
  i = 1
  for(i in 1:length(archives)){
    
    setwd("~/Documents/Kohonen/Datasets/Labels_Space")
    
    cat("\n| DATASET: ", archives[i])
    
    # Reading the file
    classes = read.csv(archives[i])
    
    # Convert to matrix
    classes = as.matrix(classes)
    
    # Compute the cardinality
    card = cardinality(classes)
    
    # Creating the file with results
    str = paste(archives[i], ",", card, sep="")
    write(str, "cardinality.txt", append = TRUE)
    
    # Compute the density
    dens = density(classes)
    
    # Creating the file with results
    str = paste(archives[i], ",", dens, sep="")
    write(str, "density.txt", append = TRUE)
    
    i = i + 1
    
    gc()
  }
  
  cat("\n|========== END FUNCTION: Cardinality and Density ==========|\n")
  
  gc()
}