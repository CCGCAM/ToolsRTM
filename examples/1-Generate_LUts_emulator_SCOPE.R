rm(list=ls())

##############################################################################################################################
#	0. load main Libraries   -----    
##############################################################################################################################

if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### hsdar for PROSAIL
if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("parallel")) { install.packages("parallel"); require("parallel") }  ### Paralell
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }  ### Paralell foreach and caret
# My packages in R
if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### Paralell foreach and caret
if (!require("caretEnsemble")) { install.packages("caretEnsemble"); require("caretEnsemble") }  ### Paralell foreach and caret

##############################################################################################################################
#	1. get LUT from SCOPE Outsputs  -----    
##############################################################################################################################

LUT<-getSCOPE_outputs(pathin = 'examples/SCOPE/OHP_2022-03-16-1634/',nsamples = 250,
                      resampling = 'Sentinel2a', reflectance = 'apparent',SIF=T, radiance = T)
#write.table(LUT, file = 'examples/LUTs/Emulator-SCOPE/LUT_OHP_22022-03-16-1634.csv',sep=',',row.names = F)


file.list <- list.files(path ='examples/LUTs/Emulator-SCOPE',pattern='OHP',full.names = T  )
LUT <- do.call("rbind", lapply(file.list, FUN = function(file) {
  read.table(file, header=TRUE, sep=",")
}))


dim(LUT)
LUT$ID<-c(1:dim(LUT)[1])

SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10','B11','B12')
SE_20m_NoB10<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')

variable_inputs <- c('ID','Cab','Cca','Cant','Cdm','Cw','N','LAI','LIDFa','Vcmax25','Rin','Rli','tts','tto')
rfl.bands<-names(LUT[,grep(colnames(LUT),pattern="R.",fixed = TRUE)])
LUT_rfl <- LUT[,c(variable_inputs,rfl.bands)]
colnames(LUT_rfl)<-c(variable_inputs,SE_20m)
head(LUT_rfl)
LUT_rfl <- LUT_rfl[, !grepl('B10', colnames(LUT_rfl))]
write.table(LUT_rfl,file=paste('examples/LUTs/Emulator-SCOPE//LUT_n1.2k_SCOPE_forEmulating.csv',sep=''),sep=',',row.names = F)



inputsSCOPE = read.table('examples/LUTs/inputs_SCOPE_emulator.csv', sep=',', header = T)
nSamples_topredict =200000
#inputs = ToolsRTM::inputsINF
LUT<-as.data.frame(getLUT(inputs = inputsSCOPE, nLUT=nSamples_topredict, setseed = 1234))
LUT$ID<-c(1:nSamples_topredict)
head(LUT)
dim(LUT)
variable_inputs <- c('ID','Cab','Cca','Cant','Cdm','Cw','N','LAI','LIDFa','Vcmax25','Rin','Rli','tts','tto')
LUT.sb <- LUT[,c(variable_inputs)]
write.table(LUT.sb,file=paste('examples/LUTs/Emulator-SCOPE/LUT_n',nSamples_topredict/1000,'k_SCOPEforPredicting_sb.csv',sep=''),sep=',',row.names = F)






