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
minval <- data.frame('N' = 1.5,'Cab'=5,'Car'=0,'Anth' = 0,'Cbrown'= 0.0,
                     'EWT' = 0.001,'Prot' =  0.00001, 'CBC' = 0.00001,
                     'LIDFa' = 0, 'LAI' = 0.5)

# define min and max values for all parameters defined in TypeDistrib
maxval <- data.frame('N' = 3,'Cab'=70,'Car'=25,'Anth' = 7,'Cbrown'= 1,
                     'EWT' = 0.1,'Prot' =  0.0015, 'CBC' = 0.0015,
                     'LIDFa' = 30, 'LAI' = 4)

TypeDistrib<-data.frame('N' = 'Gaussian','Cab'='Gaussian','Car'='Gaussian',
                        'Anth' = 'Uniform','Cbrown'= 'Uniform',
                        'EWT' = 'Uniform',
                        'Prot' =  'Uniform', 'CBC' = 'Uniform',
                        'LIDFa' = 'Uniform', 'LAI' = 'Gaussian')
# define mean and STD for gaussian distributions
Mean_gauss <- data.frame('N'=2.2,'Cab'=45,'Car'=8,'LAI' = 2.25)
std_gauss <- Mean_gauss/2.0

data.LUT<-get_distributionLUT(minval=minval,maxval=maxval,
                              nSamples=nSamples,TypeDistrib=TypeDistrib,
                              Mean_gauss=Mean_gauss, Std_gauss=std_gauss,DepCab = T)
#plot(data.LUT$Cab,data.LUT$Car)
names(data.LUT)

###################################################
#	1.3  Create the LUT table 	
##################################################

LUT<-data.frame(data.LUT$N,data.LUT$Cab,data.LUT$Car,data.LUT$Anth,data.LUT$Cbrown,
                data.LUT$EWT,LMA=0.05,alpha=40,
                ## PROSPECT-PRO
                data.LUT$Prot,data.LUT$CBC,
                ## input for fourSAIL
                data.LUT$LIDFa,
                LIDFb=0,TypeLidf=2,
                data.LUT$LAI,hspot=0.2,tts=20, tto=0, psi=0,
                ### input for 4SAIL2
                fraction_brown = 0.5, diss = 0.0, Cv = 1,Zeta = 1,#
                #skyl
                skyl=0.1,
                ### input for INFORM
                phi = 0, LAIu = 0.5, sd = 650,cd = 4.5 , h=20, psoil=psoil)

colnames(LUT)<-c("N","Cab",'Car','Anth',"Cbrown","EWT","LMA","alpha","Prot","CBC",
                 "LIDFa","LIDFb","TypeLidf","LAI",
                 "hspot","tts","tto","psi",
                 "fraction_brown","diss" ,"Cv","Zeta",'skyl',
                 'phi','LAIu', 'sd','cd', 'h', 'psoil')

head(LUT)
LUT_Green_BrownVeg<-data.frame(N=c(1.5, 2), Cab=c(40,5),Car=c(8,5),Anth=c(0,1),Cbrown=c(0,1),
                               EWT=c(0.01, 0.005), LMA=c(0.009,0.008), alpha=c(40,40),
                               Prot=c(0 , 0),CBC=c(0 , 0))
####################################################################################
####################### check plot to verify with other version (J-B. Feerte)
#################################################################################
#################################################################################

### my Library

for (i in c(1:20)){
  
i
data.inform<-ToolsRTM::inform(inputLUT = LUT[i,], psoil =LUT[i,'psoil'],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO')
plot(data.inform[[1]],ylim=c(0,0.40),type='l',lty=2,lwd=2,col='red',ylab='Reflectance')
par(new=T)
data.prosail<-ToolsRTM::m4SAIL(inputLUT=LUT[i,],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO')
#Computes bidirectional reflectance factor based on outputs from PROSAIL and sun position
rdot<-data.prosail[[1]]
rsot<-data.prosail[[2]]
rfl.prosail<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
plot(rfl.prosail,ylim=c(0,0.40),type='l',lty=1,lwd=2,col='navyblue',ylab='Reflectance')
par(new=T)
data.prosail2<-ToolsRTM::m4SAIL2(LUT_GB=LUT_Green_BrownVeg,inputLUT=LUT[i,],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO', FieldObserv = NULL)
par(new=T)
plot(data.prosail2[[1]],ylim=c(0,0.40),type='l',lty=2,lwd=2,col='forestgreen',ylab='Reflectance')
title(main = i)
legend("topright", legend = c(expression(bold('INFORM+PRO')),expression(bold('fourSAIL+PRO')),expression(bold('fourSAIL2+PRO'))),
       fill=c('red','navyblue','forestgreen'),cex=0.8)
}

### other from J-B Feret

  inputLUT = LUT[i,]; psoil =LUT[i,'psoil'];rsoil=rsoil0[[i]];PROSPECTversion = 'PRO'

  ## fourSAIL
  LIDFa=inputLUT[,'LIDFa']; LIDFb=inputLUT[,'LIDFb']; TypeLidf=inputLUT[,'TypeLidf']; lai=inputLUT[,'LAI']
  hot=inputLUT[,'hspot']; tts=inputLUT[,'tts']; tto=inputLUT[,'tto']; psi=inputLUT[,'psi']
  ala=LIDFa
  
  
  ## INform model
  LIDFa=inputLUT[,'LIDFa']; LIDFb=inputLUT[,'LIDFb']; TypeLidf=inputLUT[,'TypeLidf']; lai=inputLUT[,'LAI']
  hot=inputLUT[,'hspot']; tts=inputLUT[,'tts']; tto=inputLUT[,'tto']; phi=inputLUT[,'phi']
  ala=LIDFa
  ## INform model
  scale=psoil[i] #inputLUT[,'psoil']
  lai=inputLUT[,'LAI']; laiu=inputLUT[,'LAIu']
  sd=inputLUT[,'sd']; cd=inputLUT[,'cd']; h=inputLUT[,'h']; psi=inputLUT[,'psi']
  ala=LIDFa
  skyl=inputLUT[,'skyl']
  
  
  ## Prospect-D
  N=inputLUT[,'N']; Cab=inputLUT[,'Cab']; Car=inputLUT[,'Car']; Anth=inputLUT[,'Anth']; Cbrown=inputLUT[,'Cbrown']
  EWT=inputLUT[,'EWT']; LMA=inputLUT[,'LMA'];alpha=inputLUT[,'alpha']
  Prot=inputLUT[,'Prot'];CBC=inputLUT[,'CBC']
  
  LRT<-ToolsRTM::prospect_PRO(N,Cab,Car,Anth,Cbrown,EWT,LMA,alpha,Prot,CBC)
  # Computing of leaf reflecance and transmittance

  
  r_leaf <- LRT[[2]] #rho Reflectance
  t_leaf <- LRT[[3]] #tau Transmittance
  LeafOptics<-list(r_leaf,t_leaf)
  names(LeafOptics)<-c('Reflectance','Transmittance')
  
  ## load source from Lib_PROSAIL.R
  model1<-fourSAIL(LeafOptics, TypeLidf = 2, LIDFa = LIDFa, LIDFb = LIDFb, lai = lai,
                        q = hot, tts = tts, tto =tto, psi = psi, rsoil = rsoil0[[i]])
  
  rdot<-model[[1]]
  rsot<-model[[2]]
  rfl.model<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  par(new=T)
  plot(rfl.model,ylim=c(0,0.50),type='l',col='black',ylab='Reflectance',lwd=2,lty=2)

  
  LUT_gb<-LUT_Green_BrownVeg
  LRT_g<-ToolsRTM::prospect_PRO(LUT_gb$N[1],LUT_gb$Cab[1],LUT_gb$Car[1],LUT_gb$Anth[1],LUT_gb$Cbrown[1],
                              LUT_gb$EWT[1],LUT_gb$LMA[1],LUT_gb$alpha[1],LUT_gb$Prot[1],LUT_gb$CBC[1])
  #LRT_g<-ToolsRTM::prospect_PRO(N,Cab,Car,Anth,Cbrown,EWT,LMA,alpha,Prot,CBC)
  
  r_leafg <- LRT_g[[2]] #rho Reflectance
  t_leafg <- LRT_g[[3]] #tau Transmittance
  leafgreen<-list(r_leafg,t_leafg)
  names(leafgreen)<-c('Reflectance','Transmittance')
  
  LUT_gb<-LUT_Green_BrownVeg
  LRT_b<-ToolsRTM::prospect_PRO(LUT_gb$N[2],LUT_gb$Cab[2],LUT_gb$Car[2],LUT_gb$Anth[2],LUT_gb$Cbrown[2],
                                 LUT_gb$EWT[2],LUT_gb$LMA[2],LUT_gb$alpha[2],LUT_gb$Prot[2],LUT_gb$CBC[2])
  r_leafb <- LRT_b[[2]] #rho Reflectance
  t_leafb <- LRT_b[[3]] #tau Transmittance
  leafbrown<-list(r_leafb,t_leafb)
  names(leafbrown)<-c('Reflectance','Transmittance')

  
  
  model2<-fourSAIL2(leafgreen,leafbrown, TypeLidf = 2, LIDFa = LIDFa, LIDFb = LIDFb, lai = lai,hot = hot, tts = tts, tto =tto, psi = psi, rsoil = rsoil0[[i]],
                    fraction_brown = 0.5, diss = 0.0, Cv = 1,Zeta = 1)
 
  par(new=T)
  plot(model2[[1]],ylim=c(0,0.50),type='l',col='forestgreen',ylab='Reflectance',lwd=2,lty=1)
  
  rdot2<-model2[[1]]
  rsot2<-model2[[2]]
  rfl.model2<-ToolsRTM::Compute_BRF(rdot=rdot2,rsot=rsot2,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  par(new=T)
  plot(rfl.model2,ylim=c(0,0.50),type='l',col='yellow',ylab='Reflectance',lwd=2,lty=1)
  
  
  
  
  
