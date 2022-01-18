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


##############################################################################################################################
#	0. Load modules   -----    
##############################################################################################################################
source('Codes/PROSPECT-PRO/prospect_PRO.R')  # PROSPECT-PRO
source('Codes/PROSPECT-PRO/calctav.m.R')

source('Codes/PROSAIL-PRO/PRO4SAIL.m.R')  # PRO4SAIL
source('Codes/PROSAIL-PRO/campbell.m.R')   #	Generate leaf angle distribution from average leaf angle (ellipsoidal) or (a,b) parameters
source('Codes/PROSAIL-PRO/volscatt.m.R')  #	Generate leaf angle distribution from average leaf angle (ellipsoidal) or (a,b) parameters
source('Codes/PROSAIL-PRO/Jfunc1.m.R')  #LAI geometry
source('Codes/PROSAIL-PRO/Jfunc2.m.R')  #LAI geometry  
source('Codes/PROSAIL-PRO/Jfunc3.m.R')  #LAI geometry
source('Codes/PROSAIL-PRO/dladgen.m.R')  #LIDF
source('Codes/PROSAIL-PRO/dcum.R')  #LIDF

source('codes/functions/Gaussian_MinMax.R') #for gauss_byMin_Max()
source('codes/functions/Correlated_value.R') #for correlated Cab-Car()

##############################################################################################################################
######################## 1. generate the  LUT matrix -----    
##############################################################################################################################

version<-'Opt_vSE_v0'
n_sim<-'500'
set.seed(1256)
n_cases<-500
n_casesNorm<-n_cases*2
ID=(1:n_cases)    
#####################################################################################
#	1.1. Soil form PRSOAIL model
############################################################################################# SCOPE Soil

data    <- read.table('codes/PROSAIL-PRO/dataSpec_PDB.csv',header = T, sep=',')
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil 
psoil	 <-  1     # soil factor (psoil=0: wet soil / psoil=1: dry soil)

rsoil0  <- psoil*Rsoil1+(1-psoil)*Rsoil2
set.seed(1256)
psoil	 <-  runif(n_cases, 0.5, 1) 
rsoil0<-list()
for (i in c(1:n_cases)){
  rsoil<- c(psoil[i]*Rsoil1+(1-psoil[i])*Rsoil2)
  rsoil0[[i]]<-rsoil#
}


########################################
#	1.2 PROSPECT-RPO Leaf properties
#########################################
### uniforme distribuccion
#N<-runif(n_cases, 1.9, 3)      # structure coefficient (mesophyll)
##### normal respecto a una media y sd es Gaussinana

#N<-rnorm(n_cases, mean=2.5, sd=0.55)
N<-gauss_byMin_Max(n=n_cases, m=2, s=0.25, lwr=1.3, upr=2.5, nnorm=n_casesNorm)
hist(N)

#Cab<-runif(n_cases, 5,70)   # chlorophyll content (?g.cm-2)
#Cab<-rnorm(n_cases, mean=35, sd=15)
Cab<-gauss_byMin_Max(n=n_cases, m=39.5, s=16.5, lwr=5, upr=75, nnorm=n_casesNorm)
hist(Cab)


# carotenoid content (?g.cm-2)
Car = correlatedValue(x=Cab/4, r=.8)
plot(Car,Cab)
# Anthocyanins content (?g.cm-2)
Ant = correlatedValue(x=Car/1.5, r=.8)
Ant = abs(Ant-max(Ant))
Ant<-runif(n_cases, 0,7) 

Cbrown=0 #runif(n_cases,0.0,1) # value fixed 0 brown pigment content (arbitrary units)
# EWT  (g.cm-2)
Cw<-runif(n_cases, 0.001,0.3)
#Cw<-runif(n_cases, 0.005,0.02) ## default 0.009  # EWT  (g.cm-2)
#Cw<-rnorm(n_cases, mean=0.0116, sd=0.002)

#LMA (g.cm-2)
Cm = 0.0
#Cm<-runif(n_cases, 0.002,0.01) ## default 0.012  LMA (g.cm-2)
#Cm<-rnorm(n_cases, mean=0.019, sd=0.002)

### leaf dry matter (LMA)  in  (g.cm-2) ### Si Fijamos Prot=0 and NonProt=0 es PROSPECT-D 
# LMA == PROt + NonProtet (CBC) 
# LMA == proteins and carbon-based constituents (CBC)
Prot   = runif(n_cases, 0.000001,0.001)	# Protein content (g.cm-2) ## default 0.001
NonProt    =  runif(n_cases, 0.000001,0.01)	# CBC Carbon-based constituents (g.cm-2) ## default 0.009
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
  LIDFa	 <- 	runif(n_cases, 40,90)#sample(seq(from=30, to=60, by=0.05), size=n_cases, replace=TRUE)
  LIDFb	 <- 	0
}


###################################################
#	1.4  4SAIL canopy structure parameteres 	
##################################################

LAI<-runif(n_cases, 1.5, 3)   # leaf area index (m^2/m^2)
LAI<-gauss_byMin_Max(n=n_cases, m=3, s=1, lwr=0.5, upr=5, nnorm=n_casesNorm)
hist(LAI)


hspot = 0.0            # hot spot
tts = 27#runif(n_cases,5,20)#25,45)   # solar zenith angle (?)
tto = 20     #runif(n_cases,0,45) #15 #65    #tto Observer zenith angle
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

filename <- paste('Tables/LUT/LUT_PROSAIL-PRO_',version,'_',n_sim,'.txt', sep = "")
write.table(LUT, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)
head(LUT)
dim(LUT)
##############################################################################################################################
######################## 2.   CALL  ModelPRO4SAIL  ----     
##############################################################################################################################

# rdot: hemispherical-directional reflectance factor in viewing direction
# rsot: bi-directional reflectance factor
# rsdt: directional-hemispherical reflectance factor for solar incident flux
# rddt: bi-hemispherical reflectance factor

library(doParallel)
## choose number of processors/cores
no_cores <- detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)


start_time <- Sys.time()

#### Inversion without LMA data info
sim.rfl<-list()
sims<-foreach(i=1:n_cases) %dopar% {
  #data.prosail<-PRO4SAIL(N,Cab,Car,Ant,Cbrown,Cw,Cm,LIDFa,LIDFb,TypeLidf,LAI,hspot,tts,tto,psi,rsoil0)
  data.prosail<-PRO4SAIL(LUT[i,1],LUT[i,2],LUT[i,3],LUT[i,4],LUT[i,5],LUT[i,6],LUT[i,7],LUT[i,8],LUT[i,9],
                         LUT[i,10],LUT[i,11],LUT[i,12],LUT[i,13],LUT[i,14],LUT[i,15],LUT[i,16],LUT[i,17],rsoil0[[i]]) #rsoil[[i]]
  
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
  
}
stopCluster(cl)
end_time <- Sys.time()
end_time - start_time


#######################################################################################################################################
######################## 3.   Convert Simulations to Hsdar packages ----     
##############################################################################################################################

sim.canopy<-do.call(rbind,sims)
wave<-data[,1]
IDs<-c(1:n_cases)
Spec.simula.1nm<- speclib(sim.canopy, wave)
idSpeclib(Spec.simula.1nm) <- as.character(IDs)
SI(Spec.simula.1nm) <- LUT


### Add random noisy to the LUT

noise=rnorm(2101, mean=0.0005, sd=0.002)
#plot(sim.canopy[1,]+noise, lwd=1,lty=1,type='l')

sim.canopy.noisy<-sim.canopy+noise
Spec.simula.noisy<- speclib(sim.canopy.noisy, wave)
idSpeclib(Spec.simula.noisy) <- as.character(IDs)
SI(Spec.simula.noisy) <- LUT
plot(Spec.simula.noisy[2])

### Save simulations for Python

### Simulations at 1 nm
colnames(sim.canopy)<-paste('R.',wave,sep='')
LUT.rfl<-cbind(ID=IDs,LUT,sim.canopy)
filename=paste('Tables/LUT/0-LUT_Simulations_1nm_',version,'_',n_sim,'.csv',sep='')
write.table(LUT.rfl, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)

### Simulations at 1 nm with Noisy
colnames(sim.canopy.noisy)<-paste('R.',wave,sep='')
LUT.rfl.noise<-cbind(ID=IDs,LUT,sim.canopy.noisy)

filename=paste('Tables/LUT/0-LUT_Simulations_1nm_withNoisy_',version,'_',n_sim,'.csv',sep='')
write.table(LUT.rfl.noise, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)


#######################################################################################################################################
######################## 4.   Interpolation PROSAIL inversion to SENTINEL  ----     
##############################################################################################################################

##### From 1nm
Spec.simula.sentinel<-spectralResampling(Spec.simula.1nm, "Sentinel2a",response_function = TRUE)
plot(Spec.simula.sentinel)


rfl.sentineltoExport<-as.data.frame(Spec.simula.sentinel)
colnames(rfl.sentineltoExport)<-paste('R.',Spec.simula.sentinel@wavelength,sep='')
head(rfl.sentineltoExport)
LUT_rfl.sentinel<-cbind(ID=IDs, LUT,rfl.sentineltoExport)

filename=paste('Tables/LUT/0-LUT_Simulations_SE2a_',version,'_',n_sim,'.csv',sep='')
write.table(LUT_rfl.sentinel, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)


#####################################################################################################################################
######################## 5.  Check spectra means ----     
###################################################################################################################################

#######################################################################################################################################
######################## 5.1.   Get spectra from Sentinel-2 ----     
##############################################################################################################################
SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')
wave_2A<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
factor = 1/10000
dataset   <- read.table('Tables/DataBase/1-Table_polygons_study_byMeans_fromStacks_area1.csv',header = T, sep=',')

head(dataset)
dataset.filter<-subset(dataset, B2 <= 0.2)
dataset.filter$Date<-as.Date(dataset.filter$Date, format = "%Y-%m-%d")
dataset.filter$Year<-as.numeric(substr(as.character(dataset.filter[,2]),1,4))
dataset.filter$Month<-as.numeric(substr(as.character(dataset.filter[,2]),6,7))
dataset.filter$Day<-as.numeric(substr(as.character(dataset.filter[,2]),9,10))

Spec.data<- speclib(as.matrix(dataset.filter[,SE_20m]), wave_2A)


#######################################################################################################################################
######################## 5.2.   Plot Simulations vs HyperVNIR sensor ----     
##############################################################################################################################
library(RColorBrewer)

color.d = brewer.pal(7, "Blues")
axis_x<-expression(bold('wave (nm)'))
axis_y<-expression(bold('Reflectance'))

par(mfrow=c(1,1),  mar = c(5,5,1.1,1),bg= "white", 
    font.main=1.5, cex.main=1.2,font.axis=2, cex.axis=1.2, las=1,
    font.lab=2, cex.lab=1.0)


plot(NA,NA, lwd=2,lty=2,type='l',col='forestgreen',ylim=c(0,0.6),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
plot(Spec.simula.sentinel, lwd=2,lty=2,type='b',col='navyblue',ylim=c(0,0.6),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)
par(new=T)
plot(Spec.data, lwd=2,lty=2,type='b',col='red',ylim=c(0,0.6),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)

legend("topright", legend = c(expression(bold('PROSAIL-PRO')),expression(bold('Dataset'))),
       fill=c('navyblue','red'),cex=0.8)

#####################################################################################################################################
######################## 6.  Extract indices for Sentinel----     
###################################################################################################################################

source('codes/functions/getIndices_SE2a.R')
Bands<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10','B11','B12')
regions<-c('Coast aerosol','Blue','Green','Red', 'REd-edge1', 'REd-edge2', 'REd-edge3', 'NIR', 'REd-edge4', 'Water-Vapour', 'SWIR-Cirrus', 'SWIR1','SWIR2')
wave_2A<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1373.5,1613.7,2202.4)
Sentinel_db<-cbind(Bands,regions,wave_2A)
head(Sentinel_db)


rfl.bands<-names(LUT_rfl.sentinel[,grep(colnames(LUT_rfl.sentinel),pattern="R.",fixed = TRUE)])
length(rfl.bands)
wavelengths.sentinel<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1373.5,1613.7,2202.4)

Tabla.indices.SE2a<-getIndicesSE2a(LUT_rfl.sentinel[,rfl.bands],wavelengths.sentinel, LUT_rfl.sentinel,header = F)           
filename=paste('Tables/LUT/2-LUT_Simulations_SE2a_withIndices,_',version,'_',n_sim,'.csv',sep='')
write.table(Tabla.indices.SE2a, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)

ggplot(Tabla.indices.SE2a, aes(x=NDVI, y=Cab)) + scale_fill_brewer(palette="Dark2") +
  geom_point(na.rm=TRUE) +  theme_bw() + # use the black and white theme
  theme(axis.text.x=element_text(angle =90)) 

#####################################################################################################################################
######################## 5.  Inversion byOptmization by RMSE ----     
###################################################################################################################################


wave.SE<-Spec.data@wavelength
rfl.sensor<- as.matrix(spectra(Spec.data))

rfl.sentineltoExport

filter_names<-names(rfl.sentineltoExport)[-11]
rfl.sentinel_filter<-rfl.sentineltoExport[,filter_names]
rfl.prosail <- as.matrix(rfl.sentinel_filter)

###################################################################################################################################
###################################################################################################################################

##method is the method (opt ='merit-RMSE','merit-DWT',merit-1stD')
source('codes/Functions/InversionOpt.R') 

###################################################################################################################################
###################################################################################################################################

inv.RMSE<-InversionOpt_nOpt(rfl.sensor=rfl.sensor,rfl.prosail=rfl.prosail,
                            LUT=LUT,wave=wave.vnir.swir, 
                            n=n_cases,method='merit-RMSE', 
                            nOpt=100)

## for the best spectra from PROSAIL ( with min RMSE)
Table.bestOpt_RMSE<-inv.RMSE[[1]]
filename=paste('Tables/Results/1-Inversion_',version,'_',n_sim,'.csv')
write.table(Table.bestOpt_RMSE, file = filename, sep=",", row.names = F, col.names = T,append = F)

method=c('merit-RMSE','merit-DWT','merit-1stD')

for (j in c(1:3)) {
  print(paste('generating for ', method[j],sep='--'))
  #inv.RMSE<-InversionOpt_nOpt(rfl.sensor=rfl.sensor,rfl.prosail=rfl.prosail,wave=wave.vnir.swir, n=n_cases,method=method[j], nOpt=100)
  
}



###################################################################################################################################
###################################################################################################################################
###################################################################################################################################

