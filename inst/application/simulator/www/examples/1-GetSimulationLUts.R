
rm(list= ls())

##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################


if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("parallel")) { install.packages("parallel"); require("parallel") }  ### Paralell
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }  ### Paralell foreach and caret
# My packages in R
#if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### Paralell foreach and caret
if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }  ### dataframes


##############################################################################################################################
#	0. Get simulations for understanding spectral ----
##############################################################################################################################
## input you can use are in the LUT table

LUT_cab<-getSim_fromLUT(trait = 'Cab',nmin = 10,nmax=90,Interval = 10,model = 'PROSAIL', method='ggplot')

LUT_ewt<-getSim_fromLUT(trait = 'EWT',nmin = 0.001,nmax=0.5,Interval = 0.05,model = 'PROSAIL')
LUT_lma<-getSim_fromLUT(trait = 'LMA',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'PROSAIL')
LUT_prot<-getSim_fromLUT(trait = 'Prot',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'PROSAIL')
LUT_CBC<-getSim_fromLUT(trait = 'CBC',nmin = 0.001,nmax=0.35,Interval = 0.05,model = 'PROSAIL')
LUT_LIDFa<-getSim_fromLUT(trait = 'LIDFa',nmin = 0.0,nmax=90,Interval = 10,model = 'PROSAIL')
LUT_LAI<-getSim_fromLUT(trait = 'LAI',nmin = 0,nmax=7,Interval = 1,model = 'PROSAIL')
LUT_Car<-getSim_fromLUT(trait = 'Car',nmin = 0.0,nmax=20,Interval = 2,model = 'INFORM')
LUT_Anth<-getSim_fromLUT(trait = 'Anth',nmin = 0.0,nmax=20,Interval = 2,model = 'INFORM')
LUT_tts<-getSim_fromLUT(trait = 'tto',nmin = 0.0,nmax=90,Interval = 10,model = 'PROSAIL')
LUT_tto<-getSim_fromLUT(trait = 'tto',nmin = 0.0,nmax=90,Interval = 5,model = 'PROSAIL')


##############################################################################################################################
#	1. Get spectra from GetLUT ----
##############################################################################################################################
inputsPRO <- inputsPROSAIL
inputsPRO = read.table('Tables/LUTs/inputs_PROSAIL.csv', sep=',', header = T)
nSamples =200

#inputs = inputsINF
LUT<-as.data.frame(getLUT(inputs = inputsPRO, nLUT=nSamples, setseed = 1234))
#LMA = Proteins (Prot)+ Carbon-Based constituents (CBC)
## variations in Prot and CB when using PROSPECT-PRO
#LUT$Prot <- 0
#LUT$CBC <- 0
head(LUT)
dim(LUT)

# Plot using ggplot2
ggplot(LUT, aes(x = Cab, y = Car)) +
  geom_point() + theme_bw()

##### Get soil reflectance

data <- dataSpec_PDB
Rsoil1  <- data[,11]  # rsoil1 = dry soil
Rsoil2 <- data[,12]  # rsoil2 = wet soil
#psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)

j=1
set.seed(j*1256)
psoil	 <-  0.5#runif(nSamples, 0, 1)
rsoil0<- c(psoil*Rsoil1+(1-psoil)*Rsoil2)

plot(c(400:2500),rsoil0)


###### ### ### ### ### ### ### ### ###
### This spte is for varying psoil
#psoil	 <-  runif(nSamples, 0, 1)
#rsoil0<-list()
#for (k in c(1:10)){
 # rsoil<- c(psoil[k]*Rsoil1+(1-psoil[k])*Rsoil2)
  #rsoil0[[k]]<-rsoil#
  #print(plot(rsoil0[[k]]))
#}
###


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

  data.foursail_pro<-foursail(inputLUT=LUT[i,],rsoil=rsoil0,LeafModel = 'PROSPECT-PRO')
  rdot<-data.foursail_pro[[1]]
  rsot<-data.foursail_pro[[2]]
  data.foursail_pro<-Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT[i,'tts'],data.light =dataSpec_PDB)

  sim.rfl[[i]]<-data.foursail_pro


} ##end paralle



stopCluster(cl)
end_time <- Sys.time()
print(end_time - start_time)

##############################################################################################################################
# 3.   Generate Plot object  ----
##############################################################################################################################


sim.canopy<-do.call(rbind,sims)
wave<-c(400:2500)

# Convert matrix to data frame
df <- data.frame(sim.canopy)
df$row <- 1:nrow(df)  # Add a row identifier
head(df)

# Reshape the data to long format
df_long <- tidyr::gather(df, key = "band", value = "value", -row)
# Make 'band' an ordered factor with desired order
df_long$band <- factor(df_long$band, levels = paste0("X", 1:ncol(df)))

head(df_long)

# Calculate average, 25th percentile, and 50th percentile for each band
summary_stats <- df_long %>%
  group_by(band) %>%
  summarise(
    average = mean(value),
    median = median(value),
    percentile_25 = quantile(value, 0.25),
    percentile_50 = quantile(value, 0.50),
    percentile_75 = quantile(value, 0.75)
  )

summary_stats$band <-wave

# Plot using ggplot2
ggplot(summary_stats, aes(x = band)) +
  geom_line(aes(y = average), color = "black", size = 0.8) +
  geom_line(aes(y = median),  linetype = "dashed", color = "black", size = 0.8) +
 geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "black", alpha = 0.3) +
  labs(
    title = "PROSAIL Simulations",
    x = "wavelength (nm)",
    y = "Reflectance"
  ) +
  theme_bw()

###
get.plots(df=df,wave=wave)





