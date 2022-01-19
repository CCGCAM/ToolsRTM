#
rm(list= ls())

##############################################################################################################################
#	0. Libraries   -----    
##############################################################################################################################

if (!require("raster")) { install.packages("raster"); require("raster") }  ### hsdar for PROSAIL
if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### hsdar for PROSAIL
if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("pls")) { install.packages("pls"); require("pls") }  ### PLSR
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("prospectr")) { install.packages("prospectr"); require("prospectr") }  ## for resample2
if (!require("MASS")) { install.packages("MASS"); require("MASS") }  ## for smoth t the rfl from leaves
if (!require("caret")) { install.packages("caret"); require("caret") }  ##  models (random forest)

if (!require("nnet")) { install.packages("nnet"); require("nnet") }  ## nNEt models
if (!require("NeuralNetTools")) { install.packages("NeuralNetTools"); require("NeuralNetTools") }  ## nNEt models
if (!require("neuralnet")) { install.packages("neuralnet"); require("neuralnet") }  ## nNEt models
if (!require("e1071")) { install.packages("e1071"); require("e1071") }  ### SVM model
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
n_cases<-500
n_casesNorm<-n_cases*4

ID=(1:n_cases)    

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
psoil	 <-  runif(n_cases, 0, 1) 
rsoil0<-list()
for (i in c(1:n_cases)){
  rsoil<- c(psoil[i]*Rsoil1+(1-psoil[i])*Rsoil2)
  rsoil0[[i]]<-rsoil#
}


########################################
#	1.2 PROSPECT-RPO Leaf properties
#########################################
### uniforme distribuccion
N<-runif(n_cases, 1.9, 3)      # structure coefficient (mesophyll)
hist(N)
##### normal respecto a una media y sd es Gaussinana
N<-rnorm(n_cases, mean=2.5, sd=0.55)
hist(N)

N<-gauss_byMin_Max(n=n_cases, m=1.5, s=0.25, lwr=1.0, upr=3, nnorm=n_casesNorm)
hist(N)

Cab<-runif(n_cases, 20,50)   # chlorophyll content (?g.cm-2)
#Cab<-rnorm(n_cases, mean=35, sd=15)
Cab<-gauss_byMin_Max(n=n_cases, m=35, s=5, lwr=20, upr=50, nnorm=n_casesNorm)
hist(Cab)
#Cab<-rnorm(n_cases, mean=35, sd=15)
#Car = correlatedValue(x=Cab/4, r=.8)

Car<-runif(n_cases, 0,25) 
#Car = correlatedValue(x=Cab/4, r=.8)
plot(Car,Cab)
#summary(Car)
#Ant = correlatedValue(x=Car/1.5, r=.8)
#Ant = abs(Ant-max(Ant))
Ant<-runif(n_cases, 0,7) 
hist(Ant)
#summary(Ant)
#plot(Car,Ant)
#plot(Cab,Ant)
#Car<-runif(n_cases, 0, 15)   # carotenoid content (?g.cm-2)
#Ant<-runif(n_cases, 0,7)   # Anthocyanins content (?g.cm-2)
Cbrown=0#runif(n_cases,0.0,1) # value fixed 0 brown pigment content (arbitrary units)
#Cw<-runif(n_cases, 0.005,0.02) ## default 0.009  # EWT  (g.cm-2)
Cw<-rnorm(n_cases, mean=0.0116, sd=0.002)
hist(Cw)
#summary(Cw)

Cm<-runif(n_cases, 0.002,0.01) ## default 0.012  LMA (g.cm-2)
#Cm<-rnorm(n_cases, mean=0.019, sd=0.002)
hist(Cm)
summary(Cm)
### leaf dry matter (LMA)  in  (g.cm-2) ### Si Fijamos Prot=0 and NonProt=0 es PROSPECT-D 
# LMA == PROt + NonProtet (CBC) 
# LMA == proteins and carbon-based constituents (CBC)
Prot   = 0 #runif(n_cases, 0.000001,0.0015)	# Protein content (g.cm-2) ## default 0.001
NonProt    =  0 #runif(n_cases, 0.000001,0.0015)	# CBC Carbon-based constituents (g.cm-2) ## default 0.009
alpha_pro=runif(n_cases, 30,40) 

########################################
#	1.3  LIDFs
#########################################

TypeLidf <- 2

# if 2-parameters LIDF: TypeLidf=1
if (TypeLidf==1){
  # LIDFa LIDF parameter a, which controls the average leaf slope
  # LIDFb LIDF parameter b, which controls the distribution's bimodality
  #	LIDF type 		a 		 b
  #	Planophile 		1		 0
  #	Erectophile    -1	 	 0
  #	Plagiophile 	0		-1
  #	Extremophile 	0		 1
  #	Spherical 	   -0.35 	-0.15
  #	Uniform 0 0
  # 	requirement: |LIDFa| + |LIDFb| < 1
  LIDFa	 <- 	-0.35
  LIDFb	 <- 	-0.15
  
  # if ellipsoidal LIDF: TypeLidf=2
} else if (TypeLidf==2){
  # 	LIDFa	= average leaf angle (degrees) 0 = planophile	/	90 = erectophile
  # 	LIDFb = 0
  LIDFa	 <- 	runif(n_cases, 40,70)#sample(seq(from=30, to=60, by=0.05), size=n_cases, replace=TRUE)
  LIDFb	 <- 	0
}

###################################################
#	1.4  4SAIL canopy structure parameteres 	
##################################################

LAI<-runif(n_cases, 1.5, 3)   # leaf area index (m^2/m^2)
hist(LAI)
#LAI<-rnorm(n_cases, mean=3, sd=0.25)
#hist(LAI)
summary(LAI)
hspot = 0.01                # hot spot
tts = 27#runif(n_cases,0,27)#25,45)   # solar zenith angle (?)
tto = 0     #runif(n_cases,0,45) #15 #65    #tto Observer zenith angle
psi = 0               #Relative azimuth angle

###################################################
#	1.3  Create the LUT table 	
##################################################

LUT<-data.frame(N,Cab,Car,Ant,Cbrown,Cw,Cm,Prot,NonProt,#alpha_pro,
                LIDFa,rep(LIDFb,length(n_cases)),rep(TypeLidf,length(n_cases)),LAI,
                hspot,tts, rep(tto,length(n_cases)), rep(psi,length(n_cases)))

colnames(LUT)<-c("N","Cab",'Car','Ant',"Cbrown","Cw","Cm","Prot","NonProt",#"alpha_pro",
                 "LIDFa","LIDFb","TypeLidf","LAI",
                 "hspot","tts","tto","psi")

#filename <- paste('Tables/LUT/LUT_PROSAIL-PRO_',version,'_',n_sim,'.txt', sep = "")
#write.table(LUT, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)
head(LUT)

##############################################################################################################################
######################## 2.   CALL  ModelPRO4SAIL  ----     
##############################################################################################################################

require(doParallel)
no_cores <- parallel::detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sims<-ToolsRTM::prospectsail(LUT = LUT,rsoil = rsoil0, PROSPECTversion = 'PRO')

stopCluster(cl)
end_time <- Sys.time()
print(end_time - start_time)

## choose number of processors/cores
no_cores <- parallel::detectCores() - 2 
cl <- parallel::makeCluster(no_cores)
doParallel::registerDoParallel(cl)
start_time <- Sys.time()


sim.rfl<-list()
sims<-foreach(i=1:n_cases) %dopar% {
  data.prosail<-ToolsRTM::PRO4SAIL(LUT[i,1],LUT[i,2],LUT[i,3],LUT[i,4],LUT[i,5],LUT[i,6],LUT[i,7],LUT[i,8],LUT[i,9],
                         LUT[i,10],LUT[i,11],LUT[i,12],LUT[i,13],LUT[i,14],LUT[i,15],LUT[i,16],LUT[i,17],
                         rsoil[[i]],PROSPECTversion = 'PRO')
  #data.prosail is a  list(rdot,rsot,rddt,rsdt)
  rdot<-data.prosail[[1]]
  rsot<-data.prosail[[2]]
  
  ##############################
  #	direct / diffuse light	##
  ##############################
  # the direct and diffuse light are taken into account as proposed by:
  # Francois et al. (2002) Conversion of 400?1100 nm vegetation albedo
  # measurements into total shortwave broadband albedo using a canopy
  # radiative transfer model, Agronomie
  # Es = direct
  # Ed = diffuse
  
  Es  <- data[,9]
  Ed  <- data[,10]
  rd  <- pi/180
  skyl	 <- 	0.847- 1.61*sin((90-LUT$tts[i])*rd)+ 1.04*sin((90-LUT$tts[i])*rd)*sin((90-LUT$tts[i])*rd)# # diffuse radiation
  
  PARdiro	 <- 	(1-skyl)*Es
  PARdifo	 <- 	(skyl*Ed)
  #print(paste('simulation ',i,sep=''))
  
  resv	 <-  (rdot*PARdifo+ rsot*PARdiro)/(PARdiro+PARdifo)  # resv : directional reflectance
  sim.rfl[[i]]<-resv
  
} ##end paralle

parallel::stopCluster(cl)
end_time <- Sys.time()
final_time= end_time - start_time
print(final_time)
#######################################################################################################################################
######################## 3.   Convert Simulations to Hsdar packages ----     
##############################################################################################################################

sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
#soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))
Spec.simula<- speclib(sim.canopy, wave)
IDs<-c(1:n_cases)
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


plot(NA,NA, lwd=2,lty=2,type='l',col='forestgreen',ylim=c(0,0.6),xlim=c(400,800),xlab=axis_x,ylab=axis_y)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
plot(Spec.simula, lwd=2,lty=2,type='l',col='navyblue',ylim=c(0,0.6),xlim=c(400,800),xlab=axis_x,ylab=axis_y)
par(new=T)
plot(Spec.data, lwd=2,lty=2,type='l',col='red',ylim=c(0,0.6),xlim=c(400,800),xlab=axis_x,ylab=axis_y)

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

inv.RMSE<-InversionOpt_nOpt(rfl.sensor=rfl.sensor, #observado
                            rfl.prosail=rfl.prosail, #500
                            LUT=LUT,
                            wave=wave.vnir.swir, 
                            n=n_cases, #500
                            method='merit-RMSE', 
                            nOpt=100)

## for the best spectra from PROSAIL ( with min RMSE)
Table.bestOpt_RMSE<-inv.RMSE[[1]]
print(Table.bestOpt_RMSE)
#filename=paste('Tables/Results/1-Inversion_',version,'_',n_sim,'.csv')
w#rite.table(Table.bestOpt_RMSE, file = filename, sep=",", row.names = F, col.names = T,append = F)

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

