
#' extract spectral indices at Sentinel-2 resolution
#'
#' @param df a dataframe with reflectance where each rows correspond with an spectrum 
#' @param wavelengths  wavelent of each reflectance
#' @param df.data  dataset with IDs that corresponde with each spectrum, is null is also enable
#' @param header TRUE organize the indices by caterioas / False only return then name
#'
#' @return a dataframe with indices and your dataset
#' @export
#'
#' @examples
#' 
getIndicesSE2a <- function(df, wavelengths,df.data=NULL, header = F) {
  
  if (is.null(wavelengths) | length(wavelengths) == 12) {
    range2interpo <- c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1613.7,2202.4)
  } else if(length(wavelengths) == 13) {
  range2interpo <- c(442.7,492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,945.1,1373.5,1613.7,2202.4)
  } else if(length(wavelengths) == 10) {
    range2interpo <- c(492.4,559.8,664.6,704.1,740.5,782.8,832.8,864.7,1613.7,2202.4)
  } else{
    message('please check the band form Sentinel-2A')
    stop('number of bands are incorrect')
  }
  ##original conf form J.B Feret for CR_SWIR
  S2a_Bands <- c('B02'=496.6, 'B03'=560.0, 'B04'=664.5, 'B05'=703.9, 'B06'=740.2,
               'B07' = 782.5, 'B08' = 835.1, 'B8A' = 864.8, 'B11' = 1613.7, 'B12' = 2202.4)
  indices.list = list()
  for (i in c(1:dim(df)[1])){
  
  values<-df[i,]
  r = values<-df[i,]
  names(r) <- range2interpo

 

  indices = c()

    if(header) indices['Structural'] <- ''
    
    # NDVI (R800-R670)/(R800+R670)
    # Rouse et al. (1974)
    indices['NDVI'] <- (r['832.8'] - r['664.6']) / (r['832.8'] + r['664.6'])
    
    # RDVI (R800-R670)/(R800+R670)^0.5
    # Rougean and Breon (1995)
    indices['RDVI'] <- (r['832.8'] - r['664.6']) / (r['832.8'] + r['664.6']) ** 0.5
    
    # SR R800/R670
    # Jordan (1969)
    indices['SR'] <- r['832.8'] / r['664.6']
    
    # MSR (R800/R670-1)/((R800/R670)^0.5+1)
    # Chen (1996)
    indices['MSR'] <- (r['832.8'] / r['664.6'] - 1) / ((r['832.8'] / r['664.6']) ** 0.5 + 1)
    
    # OSAVI [(1+0.16)*(R800-R670)/(R800+R670+0.16)]
    # Rondeaux et al. (1996)
    indices['OSAVI'] <- ((1 + 0.16) * (r['832.8'] - r['664.6']) / (r['832.8'] + r['664.6'] + 0.16))
    
    # MSAVI 1/2*[(2*R800+1-?((?(2*R800+1)?^2)-8*(R800-R670))]
    # Qi et al. (1994)    
    indices['MSAVI'] <- 1 / 2 * (2 * r['832.8'] + 1 - sqrt(((2 * r['832.8'] + 1) ^ 2) - 8 * (r['832.8'] - r['664.6'])))
    
    # MTVI1 1.2*[1.2*(R800-R550)-2.5*(R670-R550)]
    # Broge & Leblanc (2000); Haboudane et al. (2004)
    indices['MTVI1'] <- 1.2 * (1.2 * (r['832.8'] - r['559.8']) - 2.5 * (r['664.6'] - r['559.8']))
    
    # MTVI2 (1.5*[1.2*(R800-R550)-2.5*(R670-R550)])/SQR((2*R800+1)^2-(6*R800-5*SQR(R670))-0.5)
    # Haboudane et al. (2004)    
    indices['MTVI2'] <- (1.5 * (1.2 * (r['832.8'] - r['559.8']) - 2.5 * (r['664.6'] - r['559.8']))) / sqrt((2 * r['832.8'] + 1) ^ 2 - (6 * r['832.8'] - 5 * sqrt(r['664.6'])) - 0.5)
    
    # MCARI ((R700-R670) - 0.2*(R700-R550))*(R700/R670)
    # Hermann et al. (2010)
    indices['MCARI'] <- ((r['704.1'] - r['664.6']) - 0.2 * (r['704.1'] - r['559.8'])) * (r['704.1'] / r['664.6'])
    
    # MCARI1 1.2* [2.5* (R800-  R670)-  1.3* (R800 -R550) ]
    # Haboudane et al. (2004)
    indices['MCARI1'] <- 1.5 * (2.5 * (r['832.8'] - r['664.6']) - 1.3 * (r['832.8'] - r['559.8']))
    
    # MCARI2 (1.5*[2.5*(R800-R670)-1.3*(R800-R550) ])/SQRT((2*R800+1)^2-(6*R800-5*SQRT(R670))-0.5)
    # Haboudane et al. (2004)  
    indices['MCARI2'] <- (1.5 * (2.5 * (r['832.8'] - r['664.6']) - 1.3 * (r['832.8'] - r['559.8']))) / sqrt((2 * r['832.8'] + 1) ^ 2 - (6 * r['832.8'] - 5 * sqrt(r['664.6'])) - 0.5)
    
    # EVI 2.5*(R800-R670)/(R800+6*R670-7.5*R400+1)
    # Huete et al. (2002)    
    indices['EVI'] <- 2.5 * (r['832.8'] - r['664.6']) / (r['832.8'] + 6 * r['664.6'] - 7.5 * r['492.4'] + 1)

    ## Pigmentos
    if(header) indices['Pigments'] <- ''
    

    # GM1 R750/R550
    # Gitelson and Merzlyak (1997)
    indices['GM1'] <- r['740.5'] / r['559.8']
    
    # GM2 R750/R700
    # Gitelson and Merzlyak (1997)
    indices['GM2'] <- r['740.5'] / r['704.1']
    
    # TCARI 3*[(R700-R670)-0.2*(R700-R550)*(R700/R670)]
    # Haboudane et al. (2002)
    indices['TCARI'] <- 3 * ((r['704.1'] - r['664.6']) - 0.2 * (r['704.1'] - r['559.8']) * (r['704.1'] / r['664.6']))
    
    # TCARI/OSAVI TCARI/OSAVI
    # Haboudane et al. (2002)
    indices['TCARI_OSAVI'] <- as.numeric(indices['TCARI']) / as.numeric(indices['OSAVI'])
    
    # TVI 0.5*[120*(R750-R550)-200*(R670-R550) ]
    # Broge and Leblanc (2000)
    indices['TVI'] <- 0.5 * (120 * (r['740.5'] - r['559.8']) - 200 * (r['664.6'] - r['559.8']))
    # SRPI R430/R680
    
    if (is.null(wavelengths) | length(wavelengths) == 12  | length(wavelengths) == 13 ) {
      # SIPI (R800-R445)/(R800+R680)
      # Pe??uelas et al. (1995)
      indices['SIPI'] <- (r['832.8'] - r['442.7']) / (r['832.8'] + r['664.6'])
    } else{
      
    }



    ## SEntinel 2a
    if(header) indices['SE2a'] <- ''
    
    #Anthocyanin reflectance index
    indices['ARI'] <- (1/r['559.8'])- (1/r['704.1']) 
    indices['GNDVI'] <- (r['832.8'] - r['559.8']) / (r['832.8'] + r['559.8'])
    indices['CIg'] <- r['832.8'] / r['559.8'] -1 
    y = 0.069;
    indices['ARVI'] <- (r['864.7'] - r['832.8'] - y * (r['664.6'] -r['492.4']) ) / (r['864.7'] + r['664.6'] - y * (r['664.6'] -r['492.4']) )
    
    if (is.null(wavelengths) | length(wavelengths) == 12  | length(wavelengths) == 13 ) {
      indices['AVI'] <- 2.0 * r['945.1'] - r['664.6']
    } else{
  
    }
    #Atmospherically Resistant Vegetation Index 2  (abbrv. ARVI2)
    indices['ARV2'] <- -0.18 + 1.17 *(r['832.8'] - r['664.6']) / (r['832.8'] + r['664.6'])
    #Normalized Difference NIR/SWIR Normalized Burn Ratio (abbrv. NBR)
    indices['NBR'] <- (r['832.8'] - r['2202.4']) / (r['832.8'] + r['2202.4'])
    indices['NBR-2'] <- (r['1613.7'] - r['2202.4']) / (r['1613.7'] + r['2202.4'])
    
    #Normalized Difference NIR/Rededge Normalized Difference Red-Edge (abbrv. NDRE)
    indices['NDRE'] <- (r['832.8'] - r['704.1']) / (r['832.8'] + r['704.1'])
    #Normalized Difference NIR/MIR Modified Normalized Difference Vegetation Index (abbrv. MNDVI)
    indices['MNDVI'] <- (r['832.8'] - r['1613.7']) / (r['832.8'] + r['1613.7'])
    
    #Red edge 1  (abbrv. Rededge1)
    indices['RedEg1'] <- r['704.1'] / r['664.6']
    indices['RedEg2'] <- (r['704.1'] - r['664.6']) / (r['704.1'] + r['664.6'])
    #Wide Dynamic Range Vegetation Index  (abbrv. WDRVI)
    indices['WDRVI'] <- (0.1 * r['832.8'] - r['664.6']) / (0.1 * r['832.8'] + r['664.6'])
    #Normalized Difference Water Index
    indices['NDWI'] <- (r['864.7'] - r['1613.7']) / (r['864.7'] + r['1613.7'])
    indices['NDWI2'] <- (r['864.7'] - r['2202.4']) / (r['864.7'] + r['2202.4'])
    
    #Leaf Water Content Index  (abbrv. LWCI)
    #MIDIR = r['1613.7']
    #indices['LWCI'] <- log(1.0 -( r['832.8'] - r['1613.7'])) / -log(1-0 * (r['832.8'] - r['1613.7']))
    #CR_SWIR from J.B.Feret
    indices['CR_SWIR'] <- r['1613.7']/(r['864.7']+(S2a_Bands['B11']-S2a_Bands['B8A'])*(r['2202.4']-r['864.7'])/(S2a_Bands['B12']-S2a_Bands['B8A']))

    #CIre
    indices['CIre'] <- (r['782.8'] / r['704.1'])-1
    indices['CIgreen'] <- (r['832.8'] / r['559.8'])-1
    indices['IRECI'] <- (r['782.8'] - r['664.6']) / (r['704.1'] / r['740.5'])
    indices['S2REP'] <- 700 + 35*( ((r['782.8'] - r['664.6']/2) -  r['704.1'])/ (r['740.5'] - r['704.1']))
    indices['RVI'] <-  (r['832.8'] / r['664.6'])
    #Perpendicular Vegetation Index 
    #Initialize parameters
    a = 0.149
    ar = 0.374
    b = 0.735
    indices['PVI'] <-  (1.0 /sqrt(a** 2.0+ 1.0)) * (r['832.8'] - ar - b)
    #Red-Edge Inflection Point 1  (abbrv. REIP1)
    indices['REIP1'] <- 700 + 405 * ( ((r['664.6'] - r['782.8']/2) -  r['704.1'])/ (r['740.5'] - r['704.1']))
    indices['REIP2'] <- 700 + 405 * ( ((r['664.6'] - r['782.8']/2) -  r['704.1'])/ (r['740.5'] - r['704.1']))
    
    ## BGR
    if(header) indices['BGR'] <- ''
    
    # G R550/R670
    # -
    indices['Greeness'] <- r['559.8'] / r['664.6']
    
    # R R700/R670
    # Gitelson et al. (2000)
    indices['Redness'] <- r['704.1'] / r['664.6']
    
    # RARS R746/R513
    
    ## NIR-VIS
    if(header) indices['NIR-VIS'] <- ''
    
    # PSSRa R800/R680
    # Blackburn (1998)    
    indices['PSSRa'] <- r['832.8'] / r['664.6']
   

    indices.list[[i]] = indices
  }
  df.indices <- data.frame(matrix(unlist(indices.list), nrow=length(indices.list), byrow=T))
  colnames(df.indices)<-names(indices)
  df.rfl<-as.data.frame(df)
  colnames(df.rfl)<-paste0('RFL.',wavelengths,sep='')
  
  if (is.null(df.data)){
    
    df.indices_<-df.indices
  } else{
    df.indices_<-cbind(df.data,df.indices)
  }

  
  return(df.indices_)
}
