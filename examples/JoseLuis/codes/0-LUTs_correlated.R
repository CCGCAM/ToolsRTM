rm(list=ls())

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
if (!require("caretEnsemble")) { install.packages("caretEnsemble"); require("caretEnsemble") }  ### Paralell foreach and caret

##############################################################################################################################
#	1. get LUT from SCOPE Outsputs  -----    
##############################################################################################################################

inputsPRO = read.table('examples/JoseLuis/Tables/inputs_PROSAIL.csv', sep=',', header = T)
nSamples_topredict =2500
#inputs = ToolsRTM::inputsINF
LUT<-as.data.frame(getLUT(inputs = inputsPRO, nLUT=nSamples_topredict, setseed = 1234))
ID<-c(1:nSamples_topredict)
LUT<-cbind(ID, LUT)
head(LUT)
dim(LUT)

variable_inputs <- c('ID','Cab','Car','Anth','LMA','EWT','N','LAI','LIDFa','tts','tto')
LUT.sb <- LUT[,c(variable_inputs)]


LUT.pigments<-getCor(n_inputs = 4,setseed = 1234,distribution = 'Uniform',nLUT = nSamples_topredict,rho=0.99,
                     Varnames = c('Cab','Car','Anth','LAI'),MinRage = c(0.5,0.1,0,0.5), MaxRange = c(95,40,7,7))

summary(LUT.pigments$LUT)
LUT.pigments$LUT[,3]<- scales::rescale(LUT.pigments$LUT[,3], to = c(7, 0))  

for (i_input in colnames(LUT.pigments$LUT)){
  
  
  plot_<-ggplot(LUT.pigments$LUT, aes_string(x='Cab', y=i_input)) +
    geom_point(alpha=0.6,aes(), size=2) +  
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title=element_blank())
  print(plot_)
}

# Calculate kernel density estimate
df.matrix.kde <- MASS::kde2d(LUT.pigments$LUT[,1], LUT.pigments$LUT[,3], n = 300/0.4)   # from MASS package
# Contour plot overlayed on heat map image of results
graphics::image(df.matrix.kde)       # from base graphics package
contour(df.matrix.kde, add = TRUE)  


###### histogram 
ggplot(LUT.pigments$LUT, aes(x=Car)) +   geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +  theme_bw() 

