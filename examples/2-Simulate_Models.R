
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
if (!require("expint")) { install.packages("expint"); require("expint") }  ### Needed for  INFORM model

##############################################################################################################################
#	1. Get spectra from GetLUT ----   
##############################################################################################################################
version<-'Ve_'
n_sim<-'0.5k'
model_rtm<-c('INFORM','fourSAIL-PRO') # INFORM
model_i <- model_rtm[1]
nSamples<-200
ID=(1:nSamples)    

inputs = ToolsRTM::inputsINF
LUT<-as.data.frame(getLUT(inputs = inputs, nLUT=nSamples, setseed = 1234))
head(LUT)
dim(LUT)


##############################################################################################################################
# 1.1. Get Soil reflectance from PROSIAL model -----    
##############################################################################################################################

data <- ToolsRTM::dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil 
j=2
set.seed(j*1256)
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)
psoil	 <-  runif(nSamples, 0, 1) 
rsoil0<-list()
for (k in c(1:nSamples)){
  rsoil<- c(psoil[k]*Rsoil1+(1-psoil[k])*Rsoil2)
  rsoil0[[k]]<-rsoil#
}
set.seed(Sys.time())

##############################################################################################################################
# 2. Run RTM   ----   
##############################################################################################################################

## choose number of processors/cores
no_cores <- detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sim.rfl<-list()
sims<-foreach(i=1:nSamples) %dopar% {
  if (model_i == 'INFORM'){
    data.inform<-ToolsRTM::inform(inputLUT = LUT[i,],rsoil=rsoil0[[i]],LeafModel = 'PRO')
    sim.rfl[[i]]<-data.inform
  } else if (model_i == 'fourSAIL-PRO'){
    data.foursail_pro<-ToolsRTM::m4SAIL(inputLUT=LUT[i,],rsoil=rsoil0[[i]],LeafModel = 'PRO')
    rdot<-data.foursail_pro[[1]]
    rsot<-data.foursail_pro[[2]]
    rfl.prosail<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],data.light=ToolsRTM::dataSpec_PDB)
    sim.rfl[[i]]<-rfl.prosail
    #data.foursail2_pro<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],data.light=ToolsRTM::dataSpec_PDB)
    
  
  }

  
  
} ##end paralle
stopCluster(cl)
end_time <- Sys.time()
print(end_time - start_time)

##############################################################################################################################
# 3.1.   Generate Hdar object  ----   
##############################################################################################################################

sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:nSamples)
### Add IDs
idSpeclib(Spec.simula) <- as.character(IDs)
SI(Spec.simula) <- LUT
#mask(Spec.simula)<-c(801,990,1098,1190,1311,1505,1680,2600)
plot(Spec.simula)

##### From 1nm to Sentinel2a
Spec.simula.sentinel<-spectralResampling(Spec.simula, "Sentinel2a",response_function = TRUE)
plot(Spec.simula.sentinel)

##############################################################################################################################
# 4.   Export LUT  ----   
##############################################################################################################################

rfl.sentineltoExport<-as.data.frame(Spec.simula.sentinel)
colnames(rfl.sentineltoExport)<-paste('R.',Spec.simula.sentinel@wavelength,sep='')
head(rfl.sentineltoExport)
LUT_rfl.sentinel<-cbind(ID=IDs, LUT,rfl.sentineltoExport)

filename=paste('examples/LUTS/1-LUT_',model_i,'_',version,j,'_',n_sim,'.csv',sep='')
write.table(LUT_rfl.sentinel, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)



