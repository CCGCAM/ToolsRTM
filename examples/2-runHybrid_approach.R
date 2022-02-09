
rm(list= ls())

##############################################################################################################################
#	0. load main Libraries   -----    
##############################################################################################################################

if (!require("hsdar")) { install.packages("hsdar"); require("hsdar") }  ### hsdar for PROSAIL
if (!require("RColorBrewer")) { install.packages("RColorBrewer"); require("RColorBrewer") }  ### colors
if (!require("signal")) { install.packages("signal"); require("signal") }  ### interpolations
if (!require("parallel")) { install.packages("parallel"); require("parallel") }  ### Paralell
if (!require("doParallel")) { install.packages("doParallel"); require("doParallel") }  ### Paralell foreach and caret
if (!require("nnet")) { install.packages("nnet"); require("nnet") }
if (!require("caretEnsemble")) { install.packages("caretEnsemble"); require("caretEnsemble") }
if (!require("dplyr")) { install.packages("dplyr"); require("dplyr") }
if (!require("ModelMetrics")) { install.packages("ModelMetrics"); require("ModelMetrics") }
if (!require("gmb")) { install.packages("gmb"); require("gmb") }
if (!require("kernlab")) { install.packages("kernlab"); require("kernal") }
if (!require("pROC")) { install.packages("pROC"); require("pROC") } ## very importat fro CaretList


# My packages in R
if (!require("ToolsRTM")) { install.packages("ToolsRTM"); require("ToolsRTM") }  ### Paralell foreach and caret
#source('codes/functions/hybrid_inversion.R')
##############################################################################################################################
#	0. load  LUT table  -----    
##############################################################################################################################

LUT    <- read.table('examples/outputs/LUTS/fourSAIL2/1-LUT_fourSAIL2-PRO_with_100k.csv',header = T, sep=',') #fourSAIL2
#LUT    <- read.table('examples/data/INFORM_vR/1-LUT_Ve_1_10k.csv',header = T, sep=',') #INFOMR
#LUT    <- read.table('examples/data/INFORM_type1/1-LUT_Ve_1_10k.csv',header = T, sep=',') #INFOMR with type

SE_20m<-c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12')
rfl.bands<-names(LUT[,grep(colnames(LUT),pattern="R.",fixed = TRUE)])
rfl.bands<-rfl.bands[-11] ##remove B10 is 1373.5 nm
wave_2A<-c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
Spec.rtm<- speclib(as.matrix(LUT[,rfl.bands]), wave_2A)
input_to<-c('ID','Cab','LAI','Car','Anth','EWT','LIDFa','Prot','CBC')

data.rtm<-data.frame(LUT[,input_to],spectra(Spec.rtm))
colnames(data.rtm)<-c(input_to,paste('R.',round(Spec.rtm@wavelength,4), sep=''))
head(data.rtm)

data.field    <- read.table('examples/field-dataset/Lanzhot_Leaf_mergeSE.csv',header = T, sep=',')
main_inputs<-c('ID_leaf','TreeID','Species','Date_field','Date_SE','DOY_field','DOY_SE','Total_chloro_ug_cm2','LAI_measured',SE_20m)
data.field.sb<-data.field[,main_inputs]
colnames(data.field.sb)<-c('IDleaf','TreeID','Species','Date_field','Date_SE','DOY_field','DOY_SE','Cab_obsv','LAI_obsv',rfl.bands)
head(data.field.sb)
##############################################################################################################################
#	1. Hybrid approach  -----    
##############################################################################################################################


#### 1.1   Model for Cab ----     

start_time <- Sys.time()

## Calculate 1st derivation
#d1.first <- derivative.speclib(Spec.simu.interp)
rtm_model='fourSAIL2'
#hybrid_method='Ensemble'
n_samples<-5000
n_samp<-'5k'
data.rtm.sb<-data.rtm[sample(nrow(data.rtm), n_samples), ]
dim(data.rtm.sb)
inputsNames<-c( 'Cab')#,'LAI')#,'LAI','Car','EWT' )
names_methods<-'nnet'#c('nnet','SVM','RF','GB') #'Ensemble',
r.hybrid<-list()
for (i in inputsNames){
  print(i)
  for (j in names_methods){
    print(j)
    hybrid_method=j
  r.hybrid[[i]]<-hybrid_inversion(LUT = data.rtm.sb,split = 0.8,setseed = 123,input = i,
                                            method = j, #'SVM','RF','GB','nnet','Ensemble'
                                            Field.data = data.field.sb, acron = '_obsv')
  print(r.hybrid[[i]]$Plot)
  print(r.hybrid[[i]]$Plot_field)
  saveRDS(r.hybrid, file=paste('examples/outputs/models/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'.RData',sep=''))
  print(r.hybrid[[i]]$Plot)
  ggsave(paste('examples/outputs/plots/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'_testing.png',sep=''))
  
  print(r.hybrid[[i]]$Plot_field)
  ggsave(paste('examples/outputs/plots/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'_Field_data.png',sep=''))
  
  data.pred<-r.hybrid[[i]]$Field.pred
  data.pred$DOY_dif<-abs(data.pred$DOY_field -data.pred$DOY_SE)
  data.pred$DOY_group<-NA
  data.pred$DOY_group[(data.pred$DOY_dif <= 5)] <- 5
  data.pred$DOY_group[(data.pred$DOY_dif > 5 & data.pred$DOY_dif < 10 )] <- 10
  data.pred$DOY_group[(data.pred$DOY_dif > 10 & data.pred$DOY_dif < 15 )] <- 15
  data.pred$DOY_group[(data.pred$DOY_dif > 15 & data.pred$DOY_dif < 20 )] <- 20
  data.pred$DOY_group[(data.pred$DOY_dif >= 20 )] <- 30
  
  data.pred.sb<-subset(data.pred, DOY_group == 5)
  data.pred.sb$Date_field <-as.Date( data.pred.sb$Date_field,"%d/%m/%Y")
  data.pred.sb$Year<-as.numeric(substr(as.character(data.pred.sb[,'Date_field']),1,4))
  
  ## summary by species
  if ( i == 'Cab'){
    data.summary<-data.pred.sb
    data.summary %>%
      group_by(Species) %>%
      summarise(
        RMSE = rmse(Cab_obsv, Cab_pred)
        ,R2 = cor(Cab_obsv, Cab_pred)^2) %>%
      write.csv(paste('examples/outputs/stats/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'.csv',sep=''))
    
  } else{
    print('no stats')
    
  } 

  ## plots
  input=i
  axis_x<-bquote(bold(.(input)['measured'])) # axis x
  axis_y<-bquote(bold(.(input)['predicted']))# axis y
  color.d = brewer.pal(7, "Blues")
  statsTitle= paste0('Bubble chart based on DOY distance')

  ggplot(data.pred, aes(y=Cab_pred, x=Cab_obsv,size = DOY_group,color = DOY_group)) +
    geom_point(alpha=0.6,show.legend = F) + theme_bw()+
    geom_smooth(aes(color = DOY_group, group = DOY_group), method = lm,size=0.5,se=F) +
    theme(legend.position = "right") +
    geom_abline(intercept = 0, slope = 1,linetype="dashed", size=0.5,color='gray') +
    scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07")) +
    # coord_fixed(ratio = 1,xlim = c(0, max(data.pred$Cab_obsv)), ylim = c(0, max(data.pred$Cab_pred))) +
    xlab(axis_x) + ylab(axis_y) + ggtitle(statsTitle) + scale_size(range = c(2, 8)) +
    labs(color='DOY diff') +  xlim(0, 80) +ylim(0,80)
  ggsave(paste('examples/outputs/plots/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'byDOY.png',sep=''))
  
  ggplot(data.pred.sb, aes(y=Cab_pred, x=Cab_obsv,color = as.factor(Species))) +
    geom_point(alpha=0.6,show.legend = F) + theme_bw()+
    geom_smooth(aes(color = as.factor(Species), group = as.factor(Species),), method = lm,size=0.5,se=F) +
    #theme(legend.position = "right") +
    geom_abline(intercept = 0, slope = 1,linetype="dashed", size=0.5,color='gray') +
    # coord_fixed(ratio = 1,xlim = c(0, max(data.pred$Cab_obsv)), ylim = c(0, max(data.pred$Cab_pred))) +
    xlab(axis_x) + ylab(axis_y) + ggtitle(statsTitle) + scale_size(range = c(1, 2)) +
    labs(color='DOY diff') +  xlim(0, 80) +ylim(0,80)
  ggsave(paste('examples/outputs/plots/',i,'_',hybrid_method,'_',rtm_model,'_',n_samp,'_byDOY_l5.png',sep=''))

  
  end_time <- Sys.time()
  print(end_time - start_time)

}
}

#saveRDS(r.hybrid, file="examples/data/Cab_svm_n1.RData")
#### load 
#r.hybrid<-readRDS("examples/data/Cab_svm_fourSAIL2_n10k.RData")


