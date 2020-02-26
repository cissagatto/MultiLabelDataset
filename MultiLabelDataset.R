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

cat("\n|========== START FUNCTION: Separated Labels Space and Attributes Space ==========|\n")

d = directories()

dataset = c(0)
card = c(0)
dens = c(0)
dime = c(0)
dados = data.frame(dataset, card, dens, dime)

setwd(FolderRoot)
datasets = read.csv("datasets.csv")


###########################################################################################
# 20-NG-F
dataset= "20-NG-F"
cat("\n", dataset)
setwd(d$folderCSV)
ngf = read.csv("20-NG-F.csv")

# atributos
setwd(d$folderAS)
ngf_atributos = data.frame(ngf[21:1026])
write.csv(ngf_atributos, "20-NG-F.csv", row.names = FALSE)

# rotulos
setwd(d$folderLS)
ngf_classes = data.frame(ngf[,1:20])
write.csv(ngf_classes, "20-NG-Fs.csv", row.names = FALSE)

# salvando os rótulos
rotulos = c(colnames(ngf_classes))
setwd(d$folderL)
write.csv(rotulos, "20-NG-F.csv")

ngf_all = cbind(ngf_atributos, ngf_classes)
setwd(d$folderCSV)
write.csv(rotulos, "20-NG-F[correto].csv")

# cardinalidade
card = cardinality(ngf_classes)

# densidade
dens = density(ngf_classes)

# dimensionalidade
dimen = as.numeric((n_atributosAS/n_instancias))

# salvando os números
dados = rbind(dados, data.frame(dataset, n_rotulos, card, dens, dime, att_start, att_end, labels_start, 
                                labels_end, n_instancias, n_atributosT, n_atributosAS, n_atributosLS))

setwd(sf$Folder)
write.csv(dados, "sumario_datasets.csv", append = TRUE)

# limpar
rm(ngf)
rm(ngf_atributos)
rm(ngf_classes)
rm(ngf_all)
gc()



###########################################################################################
# 20-NG-F
dataset= "20-NG-F"
cat("\n", dataset)
setwd(d$folderCSV)
ngf = read.csv("20-NG-F.csv")

# atributos
setwd(d$folderAS)
ngf_atributos = data.frame(ngf[21:1026])
write.csv(ngf_atributos, "20-NG-F.csv", row.names = FALSE)

# rotulos
setwd(d$folderLS)
ngf_classes = data.frame(ngf[,1:20])
write.csv(ngf_classes, "20-NG-Fs.csv", row.names = FALSE)

# salvando os rótulos
rotulos = c(colnames(ngf_classes))
setwd(d$folderL)
write.csv(rotulos, "20-NG-F.csv")

ngf_all = cbind(ngf_atributos, ngf_classes)
setwd(d$folderCSV)
write.csv(rotulos, "20-NG-F[correto].csv")

# cardinalidade
card = cardinality(ngf_classes)

# densidade
dens = density(ngf_classes)

# dimensionalidade
dimen = as.numeric((n_atributosAS/n_instancias))

# salvando os números
dados = rbind(dados, data.frame(dataset, n_rotulos, card, dens, dime, att_start, att_end, labels_start, 
                                labels_end, n_instancias, n_atributosT, n_atributosAS, n_atributosLS))

setwd(sf$Folder)
write.csv(dados, "sumario_datasets.csv", append = TRUE)

# limpar
rm(arts)
rm(arts_atributos)
rm(arts_classes)
gc()
