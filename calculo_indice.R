#Script calculo IFSE
#autor: Beatriz Nery e Markus

#OBS: posteriormente será criada uma variável que repesente a criação de matriz, bem como a quantidade de
#suas linhas e colunas

##############1ªParte################tabelas##############################################
#carregar tabela (data set)                                                                         #
tabela_completa = read.csv2('tabela_ifse.csv', header = TRUE, sep = ';', dec = ',')      
#criar tabela de 0's para posteriormente colocar o index. 
tabela_ranqueada_abr = matrix(0, 898, 9)
tabela_ranqueada_fqr = matrix(0, 898, 9)
tabela_ranqueada_dor = matrix(0, 898, 9)
tabela_dummy = matrix(0,898,6) #tabela para colocar as colunas com as variáveis dummy
#ordena todas as colunas a partir da coluna abr
index_abr<- with(tabela_completa, order(abr,fqr, dor, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_abr =  tabela_completa[index_abr,]   
#ordena todas as colunas a partir de fqr
index_fqr <- with(tabela_completa, order(fqr, abr, dor, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_fqr = tabela_completa[index_fqr,]
#ordena todas as colunas a partir da coluna dor
index_dor <- with(tabela_completa, order(dor, abr, fqr, vcmr, biomr, usor, decreasing = TRUE))
tabela_ranqueada_dor = tabela_completa[index_dor,]
##########################################################################################

################################Caso For Exportar Tabela##################################
# library(xlsx) #importar xls
# #exportar tabelas completa e ranqueada
# write.xlsx(tabela_completa, "C:/Users/Beatriz Nery/Documents/calculo_indice/tabelas/tabela_completa.xlsx")
# write.xlsx(tabela_ranqueada_abr, "C:/Users/Beatriz Nery/Documents/calculo_indice/tabelas/tabela_ranqueada.xlsx")
########################################################################################@@

##############################Calculo ###############################################
t_individuos <- 36298 #nº total de indiviuos em mil
t_especies <- 898 #n? total de especies
t_parcelas_especies <- 315 #total de parcelas de ocorrencia pra todas as especies
t_area_basal <- 1321 #area basal total
#t_biomassa_especies <-  #biomassa de todas as especies
# valor_comercial_madeira_especie <- NULL #vcmr especie
# valor_total_comercial_madeira <- NULL #vcmr total
# uso_especie <- NULL #usor da especie
# t_uso_especie <- NULL #usor da especie
#qtd de linhas do arquivo .csv
qtd_linhas <- nrow(tabela_completa)

############dummy#############
#abr#
for (i in qtd_linhas) {
  
  n_individuos_especie_abr <- ((tabela_ranqueada_abr$abr/100) * t_individuos) #
}

dim(n_individuos_especie_abr) <- c(qtd_linhas,1) #transforma um vetor em matriz

soma <- 0 #inicializa a variável soma com 0
x <- matrix(0,898,1) #cria um matriz de 0's com uma coluna e 898 linhas #obs.
#for para criação das variáveis dummy
for (a in 1:898) {
  
  teste <- sum(n_individuos_especie_abr[a,]) #valor da linha da matriz
  soma <- soma+teste #soma as linhas da matriz
  
  if (soma <= (t_individuos/2)) {
    
    x[a,1] <- 1 #escrever variávei dummy na matriz
  }
}

#fqr#
#pegar o numero de parcelas por especie
for(f in qtd_linhas){
  n_parcelas_especie_fqr <- ((tabela_ranqueada_fqr$fqr/100) * t_parcelas_especies) #obter o valor de n_parcelas
}

dim(n_parcelas_especie_fqr) <- c(898,1) #transforma um vetor em matriz

soma_fqr <- 0 #inicializa a variável com 0
x_fqr <- matrix(0, 898, 1) #cria um matriz de 0's com uma coluna e 898 linhas #obs.
#for para somar fqr e escrever coluna 
 for(fq in 1:898){
   teste_fqr <- sum(n_parcelas_especie_fqr[fq,]) #pegar valor de cada linha da matriz
   soma_fqr <- soma_fqr + teste_fqr #soma o valor das linhas da coluna
   if (soma_fqr <= (t_parcelas_especies/2)){
     #criar matriz p/ colocar dummy
     x_fqr[fq,1] <- 1 #escrever variávei dummy na matriz
   }
 }

#Area Basal ABr =  dor na tabela
for(dr in qtd_linhas){
  
  area_basal <- (( tabela_ranqueada_dor$dor/100)*  t_area_basal)
  
}
dim(area_basal) <- c(qtd_linhas,1) # transforma um vetor em matriz

soma_area <- 0
x_area <- matrix(0,qtd_linhas,1) #cria uma matriz de 0's 
for (area in 1:898) {
  teste_area <- sum(area_basal[area,]) #pega o valor de cada linha da matriz 
  soma_area <- soma_area + teste_area #soma o valor das linhas da coluna
  
  if (soma_area <= (t_area_basal/2)) {
    x_area[area,1] <- 1 #escreve variável dummy na matriz
  }
  
}

###################################calculo ifse#########################################
# #calculo NIr
# NIr <- (n_individuos_especie/t_especies)*100
# #calculo fqr
# FQr <- (n_ocorrencia_parcela_especie/t_parcelas_especies)* 100
# #calculo ABr
# ABr <- (area_basal / t_area_basal)*100
# #calculo Yr
# Yr <- (biomassa_especie/t_biomassa_especies)*100
# #calculo VCMr
# VCMr <- (valor_comercial_madeira_especie/valor_total_comercial_madeira)*100
# #calculo PFNMr
# PFNMr <- (uso_especie/t_uso_especie)*100
# sink("C:/Users/Beatriz Nery/Documents/calculo_indice/log.txt")