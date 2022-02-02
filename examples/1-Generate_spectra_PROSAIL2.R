
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
#	1. Get spectra from Sentinel-2 ----   
##############################################################################################################################

SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')
wave_2A<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
factor = 1/10000
dataset   <- ToolsRTM::data_SE
head(dataset)
dataset.filter<-subset(dataset, B2 <= 0.2)
dataset.filter$Date<-as.Date(dataset.filter$Date, format = "%Y-%m-%d")
dataset.filter$Year<-as.numeric(substr(as.character(dataset.filter[,2]),1,4))
dataset.filter$Month<-as.numeric(substr(as.character(dataset.filter[,2]),6,7))
dataset.filter$Day<-as.numeric(substr(as.character(dataset.filter[,2]),9,10))

Spec.data<- speclib(as.matrix(dataset.filter[,SE_20m]), wave_2A)
plot(Spec.data)

##############################################################################################################################
# 2. generate the  LUT matrix -----    
##############################################################################################################################

version<-'Ve_'
n_sim<-'0.5k'
model_rtm<-'fourSAIL2-PRO' # INFORM
nSamples<-500
ID=(1:nSamples)    

#for (j in c(1:10)){
 #print(paste('number of LUTS: ',j,sep=''))
  j=1

##############################################################################################################################
# 2.1. Soil form PRSOAIL model -----    
##############################################################################################################################

data <- ToolsRTM::dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil 
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)

#rsoil0  <- psoil*Rsoil1+(1-psoil)*Rsoil2
#plot(rsoil0)
set.seed(j*1256)
psoil	 <-  runif(nSamples, 0, 1) 
rsoil0<-list()
for (k in c(1:nSamples)){
  rsoil<- c(psoil[k]*Rsoil1+(1-psoil[k])*Rsoil2)
  rsoil0[[k]]<-rsoil#
}


set.seed(Sys.time())

##############################################################################################################################
# 2.2. Get parameters with distribution from leaf constituents and canopy traits -----    
##############################################################################################################################


# define min and max values for all parameters defined in TypeDistrib
minval <- data.frame('N' = 1.5,'Cab'=5,'Car'=0,'Anth' = 0,'Cbrown'= 0.0,
                     'EWT' = 0.001,'Prot' =  0.00001, 'CBC' = 0.00001,
                     'LIDFa' = 0, 'LAI' = 0.5,
                     ### input for INFORM
                     LAIu = 0, sd = 200,cd = 0.2 , h=5,
                     ### input for fourSAIL-2
                     fraction_brown = 0, diss = 0.1,Cv = 0.3, Zeta=0)

# define min and max values for all parameters defined in TypeDistrib
maxval <- data.frame('N' = 3,'Cab'=70,'Car'=25,'Anth' = 7,'Cbrown'= 0.2,
                     'EWT' = 0.035,'Prot' =  0.03, 'CBC' = 0.03,
                     'LIDFa' = 70, 'LAI' = 7,
                      ### input for INFORM
                      LAIu = 0.8, sd = 1000,cd = 7, h=20,
                      ### input for fourSAIL-2
                     fraction_brown = 1, diss = 1,Cv = 1, Zeta=0.2)
# define the type of distribution

TypeDistrib<-data.frame('N' = 'Gaussian','Cab'='Gaussian','Car'='Gaussian',
                        'Anth' = 'Uniform','Cbrown'= 'Uniform',
                        'EWT' = 'Uniform',
                        'Prot' =  'Uniform', 'CBC' = 'Uniform',
                        'LIDFa' = 'Uniform', 'LAI' = 'Gaussian',
                        ### input for INFORM
                        LAIu = 'Uniform', sd = 'Uniform',cd = 'Uniform' , h='Uniform',
                        ### input for fourSAIL-2
                        fraction_brown = 'Uniform', diss = 'Uniform',Cv = 'Uniform', Zeta='Uniform')
# define mean and STD for gaussian distributions

Mean_gauss <- data.frame('N'=2.2,'Cab'=45,'Car'=8,'LAI' = 2.25)
std_gauss <- Mean_gauss/2.0

data.LUT<-get_distributionLUT(minval=minval,maxval=maxval,
                              nSamples=nSamples,TypeDistrib=TypeDistrib,
                              Mean_gauss=Mean_gauss, Std_gauss=std_gauss,DepCab = T,setseed = j*123)

set.seed(Sys.time())
#print(data.LUT[1,])

#names(data.LUT)


##############################################################################################################################
# 2.3. Create the LUT table -----    
##############################################################################################################################


LUT<-data.frame(data.LUT$N,data.LUT$Cab,data.LUT$Car,data.LUT$Anth,data.LUT$Cbrown,
                data.LUT$EWT,LMA=0,alpha=40, #LMA=0.05
                ## PROSPECT-PRO
                data.LUT$Prot,data.LUT$CBC,
                #Prot=0,CBC=0,
                ## input for fourSAIL
                data.LUT$LIDFa,
                LIDFb=0,TypeLidf=2,
                data.LUT$LAI,hspot=0.1,tts=20, tto=0, psi=0,
                ### input for 4SAIL2
               # fraction_brown = 0.5, diss = 0, Cv = 0.5,Zeta = 1,#
               fraction_brown = data.LUT$fraction_brown, diss = data.LUT$diss, Cv = data.LUT$Cv,Zeta = data.LUT$Zeta,#
                #skyl
                skyl=0.1,
                ### input for INFORM
                phi = 0, LAIu = data.LUT$LAIu, sd = data.LUT$sd,cd = data.LUT$cd , h=data.LUT$h, psoil=psoil)

colnames(LUT)<-c("N","Cab",'Car','Anth',"Cbrown","EWT","LMA","alpha","Prot","CBC",
                 "LIDFa","LIDFb","TypeLidf","LAI",
                 "hspot","tts","tto","psi",
                 "fraction_brown","diss" ,"Cv","Zeta",'skyl',
                 'phi','LAIu', 'sd','cd', 'h', 'psoil')


head(LUT)
dim(LUT)
LUT_Green_BrownVeg<-data.frame(N=c(1.5, 2), Cab=c(40,5),Car=c(8,5),Anth=c(0,1),Cbrown=c(0,1),
                               EWT=c(0.01, 0.005), LMA=c(0.009,0.008), alpha=c(40,40),
                               Prot=c(0 , 0),CBC=c(0 , 0))

##############################################################################################################################
# 3.   CALL  PROSPECT + INFORM  ----   
##############################################################################################################################

## choose number of processors/cores
no_cores <- detectCores() - 2 
cl <- makeCluster(no_cores)
registerDoParallel(cl)

start_time <- Sys.time()
sim.rfl<-list()
sims<-foreach(i=1:nSamples) %dopar% {
  #data.inform<-ToolsRTM::inform(inputLUT = LUT[i,], psoil =LUT[i,'psoil'],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO')
  data.foursail_pro<-ToolsRTM::m4SAIL(inputLUT=LUT[i,],rsoil=rsoil0[[i]],PROSPECTversion = 'PRO')
  rdot<-data.foursail_pro[[1]]
  rsot<-data.foursail_pro[[2]]
  rfl.prosail<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  #data.foursail2_pro<-ToolsRTM::Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],SpecATM_Sensor=ToolsRTM::dataSpec_PDB)
  
  #sim.rfl[[i]]<-data.inform[[1]]
  #sim.rfl[[i]]<-data.foursail-pro
  sim.rfl[[i]]<-rfl.prosail
  
  
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
#plot(Spec.simula)

##### From 1nm to Sentinel2a
Spec.simula.sentinel<-spectralResampling(Spec.simula, "Sentinel2a",response_function = TRUE)
#plot(Spec.simula.sentinel)
##############################################################################################################################
# 3.2.   Comparison Plots  ----   
##############################################################################################################################

color.d = brewer.pal(7, "Blues")
axis_x<-expression(bold('wave (nm)'))
axis_y<-expression(bold('Reflectance'))

par(mfrow=c(1,1),  mar = c(5,5,1.1,1),bg= "white", 
    font.main=1.5, cex.main=1.2,font.axis=2, cex.axis=1.2, las=1,
    font.lab=2, cex.lab=1.0)


plot(NA,NA, lwd=2,lty=2,type='l',col='forestgreen',ylim=c(0,0.4),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)
#rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "gray")
par(new=T)
#plot(Spec.simula, lwd=1,lty=2,type='l',col='black',ylim=c(0,0.4),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)
#par(new=T)
plot(Spec.simula.sentinel, lwd=1,lty=2,type='b',col='black',pch=19,ylim=c(0,0.4),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)
par(new=T)
plot(Spec.data, lwd=1,lty=2,type='b',col='forestgreen',pch=19,ylim=c(0,0.4),xlim=c(400,2300),xlab=axis_x,ylab=axis_y)

legend("topright", legend = c(expression(bold('INFORM')),expression(bold('Sentinel-2'))),
       fill=c('black','forestgreen'),cex=0.8)


##############################################################################################################################
# 3.3.   Export LUT with simulated re-sampled at Sentinel-2A  ----   
##############################################################################################################################

Spec.simula.sentinel<-spectralResampling(Spec.simula, "Sentinel2a",response_function = TRUE)
#plot(Spec.simula.sentinel)


rfl.sentineltoExport<-as.data.frame(Spec.simula.sentinel)
colnames(rfl.sentineltoExport)<-paste('R.',Spec.simula.sentinel@wavelength,sep='')
head(rfl.sentineltoExport)
LUT_rfl.sentinel<-cbind(ID=IDs, LUT,rfl.sentineltoExport)

filename=paste('examples/data//1-LUT_',model_rtm,'_',version,j,'_',n_sim,'.csv',sep='')
write.table(LUT_rfl.sentinel, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)

#} # end j

library(plyr)
dataset <- ldply(list.files('examples/data/', pattern = '10k',full.names = T), read.csv, header=TRUE)
dim(dataset)
filename=paste('examples/data/1-LUT_',model_rtm,'_with_100k.csv',sep='')
write.table(dataset, file = filename, sep=",", row.names = FALSE, col.names = T,append = F)


##############################################################################################################################
# 3.3.   Resample simulatons to specfic sensors  ----   
##############################################################################################################################

## Create spectral response with gaussian density function
center <- seq(450,2450, 50)
fwhm   <- 6.4
wl     <- Spec.simula@wavelength
response.hyper <- speclib(t(sapply(center, get_response.R, wl, fwhm)), wl)
plot(response.hyper)
## Perform resampling
Spec.simula_.hyper <- spectralResampling(Spec.simula, response_function = response.hyper)
plot(Spec.simula_.hyper)
## Sentinel 2A
get.sensor.characteristics() ## to check the avalaible sensors
data_s2a <- hsdar::get.sensor.characteristics("Sentinel2a", TRUE)

plot(c(0,1)~c(attr(data_s2a$response, "minwl"),
              attr(data_s2a$response, "maxwl")),
     type = "n", xlab = "Wavelength [nm]", 
     ylab = "Spectral response")
xwl_response <- seq.int(attr(data_s2a$response, "minwl"),
                        attr(data_s2a$response, "maxwl"),
                        attr(data_s2a$response, "stepsize"))
for (i in 1:nrow(data_s2a$characteristics)){
  lines(xwl_response, data_s2a$response[,i], col = i)
}


