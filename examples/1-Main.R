#
rm(list= ls())

##############################################################################################################################
#	0. Libraries   -----    
##############################################################################################################################

if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### hsdar for PROSAIL
if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("parallel")) { install.packages("parallel"); require("parallel") }  ### Paralell
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }  ### Paralell foreach and caret
# My packages in R
if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### Paralell foreach and caret

##############################################################################################################################
######################## 1. generate the  LUT matrix -----    
##############################################################################################################################

version<-'Opt_v1'
n_sim<-'500'
set.seed(1256)
nSamples<-500
ID=(1:nSamples)    

#####################################################################################
#	1.1. Soil form PRSOAIL model
############################################################################################# SCOPE Soil

#data    <- read.table('parameters/dataSpec_PDB.csv',header = T, sep=',')
data <- ToolsRTM::dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil 
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)

#rsoil0  <- psoil*Rsoil1+(1-psoil)*Rsoil2
#plot(rsoil0)
set.seed(1256)
psoil	 <-  runif(nSamples, 0, 0.5) 
rsoil0<-list()
for (i in c(1:nSamples)){
  rsoil<- c(psoil[i]*Rsoil1+(1-psoil[i])*Rsoil2)
  rsoil0[[i]]<-rsoil#
}

########################################
#	1.2 Get parameters with distribution
#########################################

# define min and max values for all parameters defined in TypeDistrib
minval <- data.frame('N' = 1.0,'Cab'=5,'Car'=0,'Ant' = 0,'Cbrown'= 0,
                     'EWT' = 0.001,'Prot' =  0.00001, 'CBC' = 0.00001,
                     'LIDFa' = 40, 'LAI' = 0.5)

# define min and max values for all parameters defined in TypeDistrib
maxval <- data.frame('N' = 3,'Cab'=70,'Car'=30,'Ant' = 7,'Cbrown'= 0.5,
                     'EWT' = 0.015,'Prot' =  0.0015, 'CBC' = 0.0015,
                     'LIDFa' = 70, 'LAI' = 3)

TypeDistrib<-data.frame('N' = 'Gaussian','Cab'='Gaussian','Car'='Gaussian',
                        'Ant' = 'Uniform','Cbrown'= 'Uniform',
                        'EWT' = 'Uniform',
                        'Prot' =  'Uniform', 'CBC' = 'Uniform',
                        'LIDFa' = 'Uniform', 'LAI' = 'Gaussian')
# define mean and STD for gaussian distributions
Mean_gauss <- data.frame('N'=2.5,'Cab'=60,'Car'=8,'LAI' = 2.25)
std_gauss <- Mean_gauss/2.0

data.LUT<-get_distributionLUT(minval=minval,maxval=maxval,
                              nSamples=nSamples,TypeDistrib=TypeDistrib,
                              Mean_gauss=Mean_gauss, Std_gauss=std_gauss,DepCab = T)
#plot(data.LUT$Cab,data.LUT$Car)
names(data.LUT)

###################################################
#	1.3  Create the LUT table 	
##################################################

LUT<-data.frame(data.LUT$N,data.LUT$Cab,data.LUT$Car,data.LUT$Ant,data.LUT$Cbrown,
                data.LUT$EWT,LMA=0.00,alpha=40,
                ## PROSPECT-PRO
                data.LUT$Prot,data.LUT$CBC,
                ## input for fourSAIL
                data.LUT$LIDFa,
                LIDFb=0,TypeLidf=2,
                data.LUT$LAI,hspot=0.01,tts=20, tto=0, psi=0,
                ### input for 4SAIL2
                fraction_brown = 0.5, diss = 0.0, Cv = 1,Zeta = 1)

colnames(LUT)<-c("N","Cab",'Car','Ant',"Cbrown","EWT","LMA","alpha","Prot","CBC",
                 "LIDFa","LIDFb","TypeLidf","LAI",
                 "hspot","tts","tto","psi",
                 "fraction_brown","diss" ,"Cv","Zeta")

head(LUT)
#filename <- paste('Tables/LUT/LUT_PROSAIL-PRO_',version,'_',n_sim,'.txt', sep = "")
#write.table(LUT, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)

##############################################################################################################################
######################## 2.   CALL  PROSPECT + Model4SAIL  ----     
##############################################################################################################################

## choose number of processors/cores
no_cores <- detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sim.rfl<-list()
sims<-foreach(i=1:nSamples) %dopar% {
  data.prosail<-ToolsRTM::m4SAIL(inputLUT=LUT[i,],rsoil=rsoil[[i]],PROSPECTversion = 'D')
 
   #data.prosail is a  list(rdot,rsot,rddt,rsdt)
  rdot<-data.prosail[[1]]
  rsot<-data.prosail[[2]]
  
  #Computes bidirectional reflectance factor based on outputs from PROSAIL and sun position
  BRF<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  #
  sim.rfl[[i]]<-BRF
  
} ##end paralle
stopCluster(cl)
end_time <- Sys.time()
print(end_time - start_time)
#######################################################################################################################################
######################## 2.2   Convert Simulations to Hsdar packages ----     
##############################################################################################################################

sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:nSamples)
### Add IDs
idSpeclib(Spec.simula) <- as.character(IDs)
SI(Spec.simula) <- LUT
mask(Spec.simula)<-c(801,990,1098,1190,1311,1505,1680,2600)
plot(Spec.simula)

##############################################################################################################################
######################## 2.2   CALL  PROSPECT + Model4SAIL2  ----     
##############################################################################################################################

# define a couple of leaf chemical constituents corresponding to green and brown leaves

LUT_Green_BrownVeg<-data.frame(N=c(1.5, 2), Cab=c(40,5),Car=c(8,5),Ant=c(0,1),Cbrown=c(0,1),
                               EWT=c(0.01, 0.005), LMA=c(0.009,0.008), alpha=c(40,40),
                               Prot=c(0 , 0),CBC=c(0 , 0))

## choose number of processors/cores
no_cores <- detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sim.rfl<-list()
sims<-foreach(i=1:nSamples) %dopar% {
  data.prosail<-ToolsRTM::m4SAIL2(LUT_GB=LUT_Green_BrownVeg,inputLUT=LUT[i,],rsoil=rsoil[[i]],PROSPECTversion = 'PRO')
  #data.prosail is a  list(rdot,rsot,rddt,rsdt)
  rdot<-data.prosail[[1]]
  rsot<-data.prosail[[2]]
  
  #Computes bidirectional reflectance factor based on outputs from PROSAIL and sun position
  BRF<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  #
  sim.rfl[[i]]<-BRF
  
} ##end paralle

stopCluster(cl)
end_time <- Sys.time()
print(end_time - start_time)


sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:nSamples)
### Add IDs
idSpeclib(Spec.simula) <- as.character(IDs)
SI(Spec.simula) <- LUT
mask(Spec.simula)<-c(801,990,1098,1190,1311,1505,1680,2600)
plot(Spec.simula)

#######################################################################################################################################
######################## 3.   Convert Simulations to Hsdar packages ----     
##############################################################################################################################

sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:nSamples)
### Add IDs
idSpeclib(Spec.simula) <- as.character(IDs)
SI(Spec.simula) <- LUT
mask(Spec.simula)<-c(801,990,1098,1190,1311,1505,1680,2600)
plot(Spec.simula)

#save.image(paste('Tables/Sims/Simulations_',version,'.RData',sep=''))

#####################################################################################################################################
######################## 4.  Check spectra means ----     
###################################################################################################################################

#######################################################################################################################################
######################## 4.1.   Get spectra from HyperVNIR ----     
##############################################################################################################################

dataset<-ToolsRTM::data_20190412 #read.table('Tables/DataBase/1-RFL_20190412.csv', head=T,sep="," ,na.strings = "NA")
dataset$ID_Parcela <- toupper(dataset$ID_Parcela)

list.m<-list()
list.treat<-list()
list.id<-list()
ID_parcel<-unique(dataset$ID_Parcela)

for (i in c(1:length(ID_parcel))) {
  
  ii<-ID_parcel[i]
  dataset.sp<-subset(dataset, ID_Parcela == ii)
  list.m[[i]]<-as.vector(t(dataset.sp$RFL))
  list.id[[i]]<-unique(dataset.sp$ID_Parcela)
  list.treat[[i]]<-dataset.sp$Nitrogen[1]
}
wave.hyper<-dataset.sp$wave
data_rfl<-data.frame(ID_parcel=as.vector(do.call(cbind, list.id)),Nitrogen = as.vector(do.call(cbind, list.treat)),
                     data.frame(do.call(rbind, list.m)))
rfl.bands<-paste('RFL', wave.hyper, sep='.')
colnames(data_rfl)<-c('ID_parcel','Nitrogen', rfl.bands)
dim(data_rfl)

Spec.data<- speclib(do.call(rbind, list.m), wave.hyper)
### Add IDs
idSpeclib(Spec.data) <- data_rfl$ID_parcel
SI(Spec.data) <- data_rfl$Nitrogen
mask(Spec.data)<-c(801,990,1098,1190,1311,1505,1680,2600)

plot(Spec.data)
#######################################################################################################################################
######################## 4.2.   Plot Simulations vs HyperVNIR sensor ----     
##############################################################################################################################


library(RColorBrewer)
color.d = brewer.pal(7, "Blues")
axis_x<-expression(bold('wave (nm)'))
axis_y<-expression(bold('Reflectance'))

par(mfrow=c(1,1),  mar = c(5,5,1.1,1),bg= "white", 
    font.main=1.5, cex.main=1.2,font.axis=2, cex.axis=1.2, las=1,
    font.lab=2, cex.lab=1.0)


plot(NA,NA, lwd=2,lty=2,type='l',col='forestgreen',ylim=c(0,0.6),xlim=c(400,1800),xlab=axis_x,ylab=axis_y)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
plot(Spec.simula, lwd=2,lty=2,type='l',col='navyblue',ylim=c(0,0.6),xlim=c(400,1800),xlab=axis_x,ylab=axis_y)
par(new=T)
plot(Spec.data, lwd=2,lty=2,type='l',col='red',ylim=c(0,0.6),xlim=c(400,1800),xlab=axis_x,ylab=axis_y)

legend("topright", legend = c(expression(bold('PROSAIL-PRO')),expression(bold('Dataset'))),
       fill=c('navyblue','red'),cex=0.8)





#####################################################################################################################################
######################## 5.  Inversion byOptmization by RMSE ----     
###################################################################################################################################

wave.vnir<-c(400:800)
wave.vnir.swir<-Spec.data@wavelength
rfl.sensor<- as.matrix(spectra(Spec.data))
rfl.prosail <- as.matrix(spectra(Spec.simula))

###################################################################################################################################
###################################################################################################################################

##method is the method (opt ='merit-RMSE','merit-DWT',merit-1stD')


###################################################################################################################################
###################################################################################################################################

inv.RMSE<-ToolsRTM::InversionOpt(rfl.sensor=rfl.sensor, #observado
                            rfl.prosail=rfl.prosail, #500
                            LUT=LUT,
                            wave=wave.vnir.swir, 
                            n=nSamples, #500
                            method='merit-RMSE', 
                            nOpt=100)

## for the best spectra from PROSAIL ( with min RMSE)
Table.bestOpt_RMSE<-inv.RMSE[[1]]
print(Table.bestOpt_RMSE)
#filename=paste('Tables/Results/1-Inversion_',version,'_',n_sim,'.csv')
#rite.table(Table.bestOpt_RMSE, file = filename, sep=",", row.names = F, col.names = T,append = F)

method=c('merit-RMSE','merit-DWT','merit-1stD')

for (j in c(1:3)) {
  print(paste('generating for ', method[j],sep='--'))
  inv.RMSE<-InversionOpt_nOpt(rfl.sensor=rfl.sensor,rfl.prosail=rfl.prosail,wave=wave.vnir.swir, n=n_cases,method=method[j], nOpt=100)
  
}

### Save Sensor characteristics with Field data
rfl.sensorToexport<-as.data.frame(rfl.sensor)
colnames(rfl.sensorToexport)<-paste('R',wave.vnir_f,sep='.')
rfl.sensorToexport<-cbind(ID = data_rfl$ID_parcel, NT =data_rfl$Nitrogen, rfl.sensorToexport)
filename='Tables/Results/1-Sensor_matrix_flight.csv'
write.table(rfl.sensorToexport, file = filename, sep=",", row.names = F, col.names = T,append = F)


###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

