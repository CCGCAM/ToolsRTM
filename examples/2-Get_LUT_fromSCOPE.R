
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
##########################################################################################
### 1. Read tables ----- 
##########################################################################################
### Read 25 simulations  Tables/SCOPE_outputs//verification_run_2020-11-06-0755
List<-list.files("examples/SCOPE/OHP_2022-02-25-1826/",full.names =T)
print(List)
nrows_=1000
######################################################################################
## units in nm for RFL
path_outs = 'examples/SCOPE/OHP_2022-02-25-1826/'
wl_files<-list.files(path_outs,pattern='wlS.txt',full.names =F)
data.wlS<-read.table(paste(path_outs,'/',wl_files,sep=""),header=F)
colnames(data.wlS)<-c('wavelS')
wave<-c(data.wlS$wavelS)

inputs<-data.table::fread('examples/SCOPE/OHP_2022-02-25-1826/pars_and_input_short.csv',header=F,skip=2,nrows=nrows_,sep=',')
colnames(inputs)<-c('n_pars','Cab','Cca','Cdm','Cw','N','Cant','Vcmax25','BallBerrySlope','LAI','LIDFa','Rin','Ta','Rli','u','Ca','tts','tto')



######################################################################################
###Solar(Esun) and sky (Esky) irradiance above the canopy
#irradiance in (W m-2 um-1)
#Rin*(fEsun+fEsky)
######################################################################################
Esky_ <- as.matrix(data.table::fread("examples/SCOPE/OHP_2022-02-25-1826/Esky.csv", skip=2,nrows = nrows_ ))
Esun_ <- as.matrix(data.table::fread("examples/SCOPE/OHP_2022-02-25-1826/Esun.csv", skip=2,nrows = nrows_))
Etotal<-(Esky_+Esun_)/cos(0)
SpecIrrad<- speclib(Etotal[,c(1:2001)], wave[c(1:2001)])
plot(SpecIrrad,ylab='Irradiance')

# ######################################################################################
# #reflectance  #fraction of radiation in observation direction *pi / irradiance 
# ######################################################################################
rfl.app <- as.matrix(data.table::fread("examples/SCOPE/OHP_2022-02-25-1826/apparent_reflectance.csv", skip=2,nrows = nrows_))
SpecRefl <- speclib(rfl.app[,c(1:2001)], wave[c(1:2001)])
plot(SpecRefl,ylab='radiance with F')
plot(SpecRefl,FUN=min,ylab='radiance with F',xlim=c(740,780))

#upwelling radiance including fluorescence
#W m-2 um-1 sr-1
rad.LoF <- as.matrix(data.table::fread("examples/SCOPE/OHP_2022-02-25-1826/Lo_spectrum_inclF.csv", skip=2,nrows = nrows_))
SpecLoF <- speclib(rad.LoF[,c(1:2001)], wave[c(1:2001)])
plot(SpecLoF,ylab='radiance with F')
plot(SpecLoF,FUN=min,ylab='radiance with F',xlim=c(740,780))
## Radiation
radiation<-data.table::fread('examples/SCOPE/OHP_2022-02-25-1826/radiation.csv',header=F,skip=2,nrows=nrows_,sep=',')
colnames(radiation)<-c('simulation_number','year','DoY','ShortIn','LongIn','HemisOutShort','HemisOutLong','lo','Lot','Lote')

### Fluxes
fluxes<-data.table::fread('examples/SCOPE/OHP_2022-02-25-1826/fluxes.csv',header=F,skip=2,nrows=nrows_,sep=',')
colnames(fluxes)<-c('simulation_number','nu_iterations','year','DoY','Rnctot','lEctot','Hctot','Actot','Tcave','Rnstot','lEstot','Hstot','Gtot','Tsave','Rntot','lEtot','Htot')

# Fluoresecence Parameters
Fluo_scalar<-data.table::fread('examples/SCOPE/OHP_2022-02-25-1826/fluorescence_scalars.csv',header=F,skip=2,nrows=nrows_,sep=',')
colnames(Fluo_scalar)<-c('F_1stpeak','wl_1stpeak','F_2ndpeak','wl_2ndpeak','F687','F760','LFtot','EFtot','EFtot_RC')

# Vegeetation Parameters
vegetation<-data.table::fread('examples/SCOPE/OHP_2022-02-25-1826/vegetation.csv',header=F,skip=2,nrows=nrows_,sep=',')
colnames(vegetation)<-c('simulation_number','year','DoY','aPAR','aPARbyCab','aPARbyCab(energyunits)','Photosynthesis','Electron_transport','NPQ_energy','LST')
#

SpecLoF.SE2<-spectralResampling(SpecLoF, "Sentinel2a",response_function = TRUE)
SpecRefl.SE2<-spectralResampling(SpecRefl, "Sentinel2a",response_function = TRUE)
plot(SpecRefl.SE2)


Vcmax<-c()
SIF_1nm<-c()
SIF_SE<-c()
n_cases<-nrows_
progress_bar = txtProgressBar(min=0, max=n_cases, style = 3, char="=")
for (m in c(1:n_cases)) {
  #print(m)
  Vcmax[m]<-inputs[m,'Vcmax25']
  ### rad at 1nm
  rad.total.i<-SpecLoF@spectra[m][341:451]
  wave_rad<-SpecLoF@wavelength[341:451]
  rad.SE.i<-SpecLoF.SE2@spectra[m]
  wave.SE.rad<-SpecLoF.SE2@wavelength

  wave.i<-SpecIrrad@wavelength[341:451]##wave from 740:850
  irrad.i<-data.frame( wave=wave.i,Eo=SpecIrrad@spectra[m][341:451])
  #print(rad.total.i[c(10,23)])
  SIF_1nm[m]<-getFLD2(rad.total.i,wave_rad,irrad.i)
  SIF_SE[m]<-getFLD2_SE(rad.SE.i,wave.SE.rad,irrad.i)
  setTxtProgressBar(progress_bar, value = m)
}

close(progress_bar)

plot(inputs$Vcmax25,SIF_1nm)
plot(inputs$Vcmax25,SIF_SE)

Data_scope<-cbind(vegetation,Fluo_scalar,fluxes,radiation)
Data_scope$SIF_1nm<-SIF_1nm
Data_scope$SIF_SE<-SIF_SE
LUT<-cbind(inputs,Data_scope)

rfl.sim<-as.data.frame(SpecRefl.SE2)
colnames(rfl.sim)<-paste('R.',wave.SE.rad,sep='')

LUT_rfl<-cbind(LUT,rfl.sim)





