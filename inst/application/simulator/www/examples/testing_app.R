

rm(list= ls())

##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################

if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("parallel")) { install.packages("parallel"); require("parallel") }  ### Paralell
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }  ### Paralell foreach and caret
if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }  ### dataframes


##############################################################################################################################
#	1. Get spectra from GetLUT ----
##############################################################################################################################
inputsPRO <- inputsPROSAIL
nSamples =1
LUT<-as.data.frame(getLUT(inputs = inputsPRO, nLUT=nSamples, setseed = 1234))

#inputs = Soil spectrum
data <- dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)

set.seed(1*1256)
psoil	 <-  0.5#runif(nSamples, 0, 1)
rsoil0<- c(psoil*Rsoil1+(1-psoil)*Rsoil2)


data.foursail_pro<-foursail(inputLUT=LUT[1,],rsoil=rsoil0,LeafModel = 'PROSPECT-PRO')
rdot<-data.foursail_pro[[1]]
rsot<-data.foursail_pro[[2]]
data.foursail_pro<-Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[1,'tts'],data.light =dataSpec_PDB)


sim.canopy<-do.call(rbind,list(data.foursail_pro))
wave<-c(400:2500)

# Convert matrix to data frame
df <- data.frame(sim.canopy)
df$row <- 1:nrow(df)  # Add a row identifier
dim(df)
###
get.plots(df=df,wave=wave)

sensor.i = Sentinel2A.MSI
df_ <- data.frame(wave=wave, rfl=t(sim.canopy))
head(df_)
df_resampled <-get.spectral.convolution.rfl(df = df_,sensor.i, get.plots=F)


