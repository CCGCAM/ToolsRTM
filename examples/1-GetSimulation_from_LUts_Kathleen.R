
rm(list= ls())

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


##############################################################################################################################
#	0. Get simulations for understanding spectral ----   
##############################################################################################################################
## input you can use are in the LUT table

LUT_cab<-ToolsRTM::getSim_fromLUT(trait = 'Cab',nmin = 10,nmax=90,Interval = 5,model = 'PROSAIL')
LUT_ewt<-ToolsRTM::getSim_fromLUT(trait = 'EWT',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'INFORM')
LUT_lma<-ToolsRTM::getSim_fromLUT(trait = 'LMA',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'PROSAIL')
LUT_prot<-ToolsRTM::getSim_fromLUT(trait = 'Prot',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'PROSPECT')
LUT_CBC<-ToolsRTM::getSim_fromLUT(trait = 'CBC',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'INFORM')
LUT_Car<-ToolsRTM::getSim_fromLUT(trait = 'Car',nmin = 0.0,nmax=20,Interval = 2,model = 'INFORM')
LUT_Anth<-ToolsRTM::getSim_fromLUT(trait = 'Anth',nmin = 0.0,nmax=20,Interval = 2,model = 'INFORM')
LUT_tto<-ToolsRTM::getSim_fromLUT(trait = 'tts',nmin = 0.0,nmax=90,Interval = 10,model = 'PROSAIL')
LUT_tts<-ToolsRTM::getSim_fromLUT(trait = 'tto',nmin = 0.0,nmax=90,Interval = 10,model = 'PROSAIL')

##############################################################################################################################
#	1. Get spectra from GetLUT ----   
##############################################################################################################################

inputsPRO = read.table('examples/LUTs/inputs_PROSAIL_Kathleen.csv', sep=',', header = T)
nSamples =500

#inputs = ToolsRTM::inputsINF
LUT<-as.data.frame(getLUT(inputs = inputsPRO, nLUT=nSamples, setseed = 1234))
LUT$LIDFa =24
head(LUT)
dim(LUT)
plot(LUT$Cab, LUT$Car)

data <- ToolsRTM::dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil 
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)

#rsoil0  <- psoil*Rsoil1+(1-psoil)*Rsoil2
#plot(rsoil0)
j=1
set.seed(j*1256)
psoil	 <-  0.1#runif(nSamples, 0, 1) 
rsoil0<- c(psoil*Rsoil1+(1-psoil)*Rsoil2)
#rsoil0<-list()
#for (k in c(1:nSamples)){
 # rsoil<- c(psoil[k]*Rsoil1+(1-psoil[k])*Rsoil2)
  #rsoil0[[k]]<-rsoil#
#}
#plot(rsoil0)
##############################################################################################################################
# 2.Get Simulations  ----
##############################################################################################################################

## choose number of processors/cores
no_cores <- detectCores() - 2
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sim.rfl<-list()
sims<-foreach(i=1:nSamples) %dopar% {
  #data.inform<-ToolsRTM::inform(inputLUT = LUT[i,], psoil =LUT[i,'psoil'],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO')
  data.foursail_pro<-ToolsRTM::m4SAIL(inputLUT=LUT[i,],rsoil=rsoil0,PROSPECTversion = 'PRO') ##rsoil=rsoil0 [[i]] Si hay diferente RFL suelo en la LUT. SI fijamos la misma rsoil para toda la LUT quitar [[i]]
  rdot<-data.foursail_pro[[1]]
  rsot<-data.foursail_pro[[2]]
  data.foursail_pro<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],data.light =ToolsRTM::dataSpec_PDB)
  
  #sim.rfl[[i]]<-data.inform[[1]]
  sim.rfl[[i]]<-data.foursail_pro
  #sim.rfl[[i]]<-data.foursail2_pro
  
  
} ##end paralle

##############################################################################################################################
# 3.   Generate Hdar object  ----   
##############################################################################################################################


sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:nSamples)
### Add IDs
idSpeclib(Spec.simula) <- as.character(IDs)
SI(Spec.simula) <- LUT
#plot at 1 nm
plot(Spec.simula)
##### From 1nm to Sentinel2a
Spec.simula.sentinel<-spectralResampling(Spec.simula, "Sentinel2a",response_function = TRUE)
#plot at SE2 resolotion
plot(Spec.simula.sentinel)



SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10','B11','B12')
SE_20m_NoB10<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')
rfl.hypertoExport<-as.data.frame(Spec.simula.sentinel)
colnames(rfl.hypertoExport)<-paste('R.',Spec.simula.sentinel@wavelength,sep='')

colnames(rfl.hypertoExport) <- SE_20m

LUT_rfl.hyper<-cbind(ID=IDs, LUT,rfl.hypertoExport)
LUT_rfl.hyper <- LUT_rfl.hyper[, !grepl('B10', colnames(LUT_rfl.hyper))]
head(LUT_rfl.hyper)

write.table(LUT_rfl.hyper,file=paste('examples/LUTs/Emulators/LUT_n',nSamples/1000,'k_PROSAIL_forEmulating.csv',sep=''),sep=',',row.names = F)

variable_inputs <- c('ID','Cab','Car','Anth','LMA','EWT','N','LIDFa','LAI','tto')
LUT_rfl.hyper.sb <- LUT_rfl.hyper[,c(variable_inputs,SE_20m_NoB10)]
write.table(LUT_rfl.hyper.sb,file=paste('examples/LUTs/Emulators/LUT_n',nSamples/1000,'k_PROSAIL_forEmulating_sb.csv',sep=''),sep=',',row.names = F)







