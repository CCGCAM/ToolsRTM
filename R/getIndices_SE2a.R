
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
  
  range2interpo = as.numeric(gsub(".*?([0-9]+).*", "\\1", wavelengths))
 

  ##original conf form J.B Feret for CR_SWIR
  S2a_Bands <- c('B2'=496.6, 'B3'=560.0, 'B4'=664.5, 'B5'=703.9, 'B6'=740.2,
               'B7' = 782.5, 'B8' = 835.1, 'B8A' = 864.8, 'B11' = 1613, 'B12' = 2202)
  indices.list = list()
  # create progress bar
  total=dim(df)[1]
  barProgress <- txtProgressBar(min = 1, max = total, style = 3)
  
  for (i in c(1:dim(df)[1])){
  
  values<-df[i,]
  r = values<-df[i,]
  names(r) <- range2interpo

 

  indices = c()

    if(header) indices['Structural'] <- 'Structural'
    
    # NDVI (R800-R670)/(R800+R670)
    # Rouse et al. (1974)
    indices['NDVI'] <- (r['832'] - r['664']) / (r['832'] + r['664'])
    
    # RDVI (R800-R670)/(R800+R670)^0.5
    # Rougean and Breon (1995)
    indices['RDVI'] <- (r['832'] - r['664']) / (r['832'] + r['664']) ** 0.5
    
    # SR R800/R670
    # Jordan (1969)
    indices['SR'] <- r['832'] / r['664']
    
    # MSR (R800/R670-1)/((R800/R670)^0.5+1)
    # Chen (1996)
    indices['MSR'] <- (r['832'] / r['664'] - 1) / ((r['832'] / r['664']) ** 0.5 + 1)
    
    # OSAVI [(1+0.16)*(R800-R670)/(R800+R670+0.16)]
    # Rondeaux et al. (1996)
    indices['OSAVI'] <- ((1 + 0.16) * (r['832'] - r['664']) / (r['832'] + r['664'] + 0.16))
    
    # MSAVI 1/2*[(2*R800+1-?((?(2*R800+1)?^2)-8*(R800-R670))]
    # Qi et al. (1994)    
    indices['MSAVI'] <- 1 / 2 * (2 * r['832'] + 1 - sqrt(((2 * r['832'] + 1) ^ 2) - 8 * (r['832'] - r['664'])))
    
    # MTVI1 1.2*[1.2*(R800-R550)-2.5*(R670-R550)]
    # Broge & Leblanc (2000); Haboudane et al. (2004)
    indices['MTVI1'] <- 1.2 * (1.2 * (r['832'] - r['559']) - 2.5 * (r['664'] - r['559']))
    
    # MTVI2 (1.5*[1.2*(R800-R550)-2.5*(R670-R550)])/SQR((2*R800+1)^2-(6*R800-5*SQR(R670))-0.5)
    # Haboudane et al. (2004)    
    indices['MTVI2'] <- (1.5 * (1.2 * (r['832'] - r['559']) - 2.5 * (r['664'] - r['559']))) / sqrt((2 * r['832'] + 1) ^ 2 - (6 * r['832'] - 5 * sqrt(r['664'])) - 0.5)
    
    # MCARI ((R700-R670) - 0.2*(R700-R550))*(R700/R670)
    # Hermann et al. (2010)
    indices['MCARI'] <- ((r['704'] - r['664']) - 0.2 * (r['704'] - r['559'])) * (r['704'] / r['664'])
    
    # MCARI1 1.2* [2.5* (R800-  R670)-  1.3* (R800 -R550) ]
    # Haboudane et al. (2004)
    indices['MCARI1'] <- 1.5 * (2.5 * (r['832'] - r['664']) - 1.3 * (r['832'] - r['559']))
    
    # MCARI2 (1.5*[2.5*(R800-R670)-1.3*(R800-R550) ])/SQRT((2*R800+1)^2-(6*R800-5*SQRT(R670))-0.5)
    # Haboudane et al. (2004)  
    indices['MCARI2'] <- (1.5 * (2.5 * (r['832'] - r['664']) - 1.3 * (r['832'] - r['559']))) / sqrt((2 * r['832'] + 1) ^ 2 - (6 * r['832'] - 5 * sqrt(r['664'])) - 0.5)
    
    # EVI 2.5*(R800-R670)/(R800+6*R670-7.5*R400+1)
    # Huete et al. (2002)    
    indices['EVI'] <- 2.5 * (r['832'] - r['664']) / (r['832'] + 6 * r['664'] - 7.5 * r['492'] + 1)

    ## Pigmentos
    if(header) indices['Pigments'] <- 'Pigments'
    

    # GM1 R750/R550
    # Gitelson and Merzlyak (1997)
    indices['GM1'] <- r['740'] / r['559']
    
    # GM2 R750/R700
    # Gitelson and Merzlyak (1997)
    indices['GM2'] <- r['740'] / r['704']
    
    # TCARI 3*[(R700-R670)-0.2*(R700-R550)*(R700/R670)]
    # Haboudane et al. (2002)
    indices['TCARI'] <- 3 * ((r['704'] - r['664']) - 0.2 * (r['704'] - r['559']) * (r['704'] / r['664']))
    
    # TCARI/OSAVI TCARI/OSAVI
    # Haboudane et al. (2002)
    indices['TCARI_OSAVI'] <- as.numeric(indices['TCARI']) / as.numeric(indices['OSAVI'])
    
    # TVI 0.5*[120*(R750-R550)-200*(R670-R550) ]
    # Broge and Leblanc (2000)
    indices['TVI'] <- 0.5 * (120 * (r['740'] - r['559']) - 200 * (r['664'] - r['559']))
    # SRPI R430/R680
    
    if (is.null(wavelengths) | length(wavelengths) == 12  | length(wavelengths) == 13 ) {
      # SIPI (R800-R445)/(R800+R680)
      # Pe??uelas et al. (1995)
      indices['SIPI'] <- (r['832'] - r['442']) / (r['832'] + r['664'])
    } else{
      
    }



    ## SEntinel 2a
    if(header) indices['SE2a'] <- 'SE2a Indices'
    
    #Anthocyanin reflectance index
    indices['ARI'] <- (1/r['559'])- (1/r['704']) 
    indices['GNDVI'] <- (r['832'] - r['559']) / (r['832'] + r['559'])
    indices['CIg'] <- r['832'] / r['559'] -1 
    y = 0.069;
    indices['ARVI'] <- (r['864'] - r['832'] - y * (r['664'] -r['492']) ) / (r['864'] + r['664'] - y * (r['664'] -r['492']) )
    
    if (is.null(wavelengths) | length(wavelengths) == 12  | length(wavelengths) == 13 ) {
      indices['AVI'] <- 2.0 * r['945'] - r['664']
    } else{
  
    }
    #Atmospherically Resistant Vegetation Index 2  (abbrv. ARVI2)
    indices['ARV2'] <- -0.18 + 1.17 *(r['832'] - r['664']) / (r['832'] + r['664'])
    #Normalized Difference NIR/SWIR Normalized Burn Ratio (abbrv. NBR)
    indices['NBR'] <- (r['832'] - r['2202']) / (r['832'] + r['2202'])
    indices['NBR.2'] <- (r['1613'] - r['2202']) / (r['1613'] + r['2202'])
    
    #Normalized Difference NIR/Rededge Normalized Difference Red-Edge (abbrv. NDRE)
    indices['NDRE'] <- (r['832'] - r['704']) / (r['832'] + r['704'])
    #Normalized Difference NIR/MIR Modified Normalized Difference Vegetation Index (abbrv. MNDVI)
    indices['MNDVI'] <- (r['832'] - r['1613']) / (r['832'] + r['1613'])
    
    #Red edge 1  (abbrv. Rededge1)
    indices['RedEg1'] <- r['704'] / r['664']
    indices['RedEg2'] <- (r['704'] - r['664']) / (r['704'] + r['664'])
    #Wide Dynamic Range Vegetation Index  (abbrv. WDRVI)
    indices['WDRVI'] <- (0.1 * r['832'] - r['664']) / (0.1 * r['832'] + r['664'])
    #Normalized Difference Water Index
    indices['NDWI'] <- (r['864'] - r['1613']) / (r['864'] + r['1613'])
    indices['NDWI2'] <- (r['864'] - r['2202']) / (r['864'] + r['2202'])
    
    #Leaf Water Content Index  (abbrv. LWCI)
    #MIDIR = r['1613']
    #indices['LWCI'] <- log(1.0 -( r['832'] - r['1613'])) / -log(1-0 * (r['832'] - r['1613']))
    #CR_SWIR from J.B.Feret
    indices['CR.SWIR'] <- r['1613']/(r['864']+(S2a_Bands['B11']-S2a_Bands['B8A'])*(r['2202']-r['864'])/(S2a_Bands['B12']-S2a_Bands['B8A']))

    #CIre
    indices['CIre'] <- (r['782'] / r['704'])-1
    indices['CIgreen'] <- (r['832'] / r['559'])-1
    indices['IRECI'] <- (r['782'] - r['664']) / (r['704'] / r['740'])
    indices['S2REP'] <- 700 + 35*( ((r['782'] - r['664']/2) -  r['704'])/ (r['740'] - r['704']))
    indices['RVI'] <-  (r['832'] / r['664'])
    #Perpendicular Vegetation Index 
    #Initialize parameters
    a = 0.149
    ar = 0.374
    b = 0.735
    indices['PVI'] <-  (1.0 /sqrt(a** 2.0+ 1.0)) * (r['832'] - ar - b)
    #Red-Edge Inflection Point 1  (abbrv. REIP1)
    indices['REIP1'] <- 700 + 405 * ( ((r['664'] - r['782']/2) -  r['704'])/ (r['740'] - r['704']))
    indices['REIP2'] <- 700 + 405 * ( ((r['664'] - r['782']/2) -  r['704'])/ (r['740'] - r['704']))
    
    ## BGR
    if(header) indices['BGR'] <- 'BGR'
    
    # G R550/R670
    # -
    indices['Greeness'] <- r['559'] / r['664']
    
    # R R700/R670
    # Gitelson et al. (2000)
    indices['Redness'] <- r['704'] / r['664']
    
    # RARS R746/R513
    
    ## NIR-VIS
    if(header) indices['NIR-VIS'] <- 'NIR-VIS'
    
    # PSSRa R800/R680
    # Blackburn (1998)    
    indices['PSSRa'] <- r['832'] / r['664']
   
    if(header) indices['Red-edge'] <- 'Red-edge'
      if(any(range2interpo == 864)){
        
        indices['SIFe1'] <- r['740']  / (r['864'] + (bandsSE['B6'] - bandsSE['B8A']) * (r['782'] - r['864']) / (bandsSE['B7'] - bandsSE['B8A']))
        indices['SIFe2'] <- r['704']  / (r['782'] + (bandsSE['B5'] - bandsSE['B7']) * (r['740'] - r['782']) / (bandsSE['B6'] - bandsSE['B7']))
        indices['SIFe3'] <- r['704']  / (r['864'] + (bandsSE['B5'] - bandsSE['B8A']) * (r['740'] - r['864']) / (bandsSE['B6'] - bandsSE['B8A']))
        indices['SIFe4'] <- r['665']  / (r['864'] + (bandsSE['B4'] - bandsSE['B8A']) * (r['704'] - r['864']) / (bandsSE['B6'] - bandsSE['B8A']))
        indices['SIFe5'] <- r['740']  / (r['864'] + (bandsSE['B6'] - bandsSE['B8A']) * (r['704'] - r['864']) / (bandsSE['B5'] - bandsSE['B8A']))
        indices['SIFe6'] <- r['782']  / (r['864'] + (bandsSE['B7'] - bandsSE['B8A']) * (r['704'] - r['864']) / (bandsSE['B5'] - bandsSE['B8A']))
        indices['SIFe7'] <- r['762']  / (r['864'] + (bandsSE['B.762'] - bandsSE['B8A']) * (r['704'] - r['864']) / (bandsSE['B5'] - bandsSE['B8A']))
      
      } else {
        indices['SIFe1'] <- r['740']  / (r['832'] + (bandsSE['B6'] - bandsSE['B8']) * (r['782'] - r['832']) / (bandsSE['B7'] - bandsSE['B8']))
        indices['SIFe2'] <- r['704']  / (r['782'] + (bandsSE['B5'] - bandsSE['B7']) * (r['740'] - r['782']) / (bandsSE['B6'] - bandsSE['B7']))
        indices['SIFe3'] <- r['704']  / (r['832'] + (bandsSE['B5'] - bandsSE['B8']) * (r['740'] - r['832']) / (bandsSE['B6'] - bandsSE['B8']))
        indices['SIFe4'] <- r['665']  / (r['832'] + (bandsSE['B4'] - bandsSE['B8']) * (r['704'] - r['832']) / (bandsSE['B6'] - bandsSE['B8']))
        indices['SIFe5'] <- r['740']  / (r['832'] + (bandsSE['B6'] - bandsSE['B8']) * (r['704'] - r['832']) / (bandsSE['B5'] - bandsSE['B8']))
        indices['SIFe6'] <- r['782']  / (r['832'] + (bandsSE['B7'] - bandsSE['B8']) * (r['704'] - r['832']) / (bandsSE['B5'] - bandsSE['B8']))
        indices['SIFe7'] <- r['762']  / (r['832'] + (bandsSE['B.762'] - bandsSE['B8']) * (r['704'] - r['832']) / (bandsSE['B5'] - bandsSE['B8']))
      }  

    indices.list[[i]] = indices
    setTxtProgressBar(barProgress, i)
    
  }
 
  df.indices <- data.frame(matrix(unlist(indices.list), nrow=length(indices.list), byrow=T))
  colnames(df.indices)<-names(indices)
  df.rfl<-as.data.frame(df)
  colnames(df.rfl)<-paste0('RFL.',wavelengths,sep='')
  
  if (is.null(df.data)){
    ## remove indices wih no data
    df.indices = df.indices[, colSums(is.na(df.indices)) != nrow(df.indices)]
    df.indices_<-df.indices

 
  } else{
    ## remove indices wih no data
    df.indices = df.indices[, colSums(is.na(df.indices)) != nrow(df.indices)]
    df.indices_<-cbind(df.data,df.indices)
    
  }

  close(barProgress)
  return(df.indices_)
}
