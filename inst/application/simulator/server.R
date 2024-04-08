
#  Define SERVER for the app -----------------------------------------

# Define a reactive values object to store the selected model
selected_model <- reactiveValues(model = NULL)

server <-shinyServer(function(input, output, session) {

  inputs.liberty = inputsLiberty
  LUT.liberty<-as.data.frame(getLUT_liberty(inputs = inputs.liberty, nLUT=1, setseed = 1234))
  sim.liberty<-liberty(inputLUT = LUT.liberty[1,])


  ########### 3) SCOPE -------------------------------

  ## Reactive expression (SCOPE) for the leaf and canopy parameters -------------------------------
  params_SCOPE <- reactive({
    leaf_params_SCOPE <- switch(input$Leaf_scope,


                          "FLUSPECT-B" = c(N=input$N_fd_scope,Cab = input$Cab_fd_scope, Car = input$Car_fd_scope,
                                           EWT=input$EWT_fd_scope,LMA=input$LMA_fd_scope,
                                           Cx=input$Cx_fd_scope, Cs = input$Cs_fd_scope,
                                           fqe = input$fqe_fd_scope),

                          "FLUSPECT-B-Cx" = c(N=input$N_fp_scope,Cab = input$Cab_fp_scope, Car = input$Car_fp_scope,
                                              EWT=input$EWT_fp_scope,LMA=input$LMA_fp_scope,
                                              Cx=input$Cx_fp_scope, Cs = input$Cs_fp_scope,
                                              fqe = input$fqe_fp_scope))


    ## // parameters for fourSAIL


    canopy_params_SCOPE <- switch(input$Canopy_scope,
                            "fourSAILH" = c(LAI=input$LAI_scope, TypeLidf=1, LIDFa=input$LIDFa_scope,
                                            LIDFb=input$LIDFb_scope, hspot=input$hotspot_scope,
                                            tts=input$tts_scope, tto=input$tto_scope,
                                            psi=0.5))


    ## // dditional parameters

    Vcmax_scope <- input$Vcmax_scope
    BaLBerrySlope <- input$BaLBerrySlope
    BaLBerry0 <- input$BaLBerry0

    Rn_scope <- input$Rn_scope
    Rli_scope <- input$Rli_scope
    Ta_scope <- input$Ta_scope

    additional_params_SCOPE <- c(Vcmax25 = Vcmax_scope, BallBerrySlope = BaLBerrySlope,
                                          BallBerry0 = BaLBerry0,
                                 Rn = Rn_scope,
                                 Rli = Rli_scope,
                                 Ta =Ta_scope)

    return(list(leaf_params_SCOPE, canopy_params_SCOPE,additional_params_SCOPE))
  })


  # Reactive expression for the LUT leaf data (SCOPE)---------------------------------------------------
  lut_scope.leaf <- reactive({

    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(params_SCOPE()[[1]])
    leaf_params_names<-names(params_SCOPE()[[1]])

    bioleaf_params <- unlist(params_SCOPE()[[3]])
    bioleaf_params_names<-names(params_SCOPE()[[3]])
    # Call the LUT function using the selected parameters

    # Call the LUT function using the selected parameters
    lut_leaf <- c(leaf_params, bioleaf_params)
    #colnames(lut_leaf) <- c(leaf_params_names, names(params_SCOPE()[[3]]))

   # lut_leaf <- data.frame(rbind(leaf_params, bioleaf_params_names)) #, canopy_params))
    print(lut_leaf)
    return(lut_leaf)
  })



  # Reactive expression for LUT canopy data ---------------------------------------------------
  lut_scope.canopy <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    canopy_params <- unlist(params_SCOPE()[[2]])
    # Call the LUT function using the selected parameters
    lut_canopy <- canopy_params
    return(lut_canopy)
  })



  # Render the printed output in verbatimTextOutput
  output$lut_scope_output_param <- renderPrint({
    params_SCOPE()

  })

  # Render the printed output in verbatimTextOutput
  output$lut_scope_output_canopy <- renderPrint({
    lut_scope.canopy()

  })


  # Render the printed output in verbatimTextOutput
  output$lut_scope_output_leaf <- renderPrint({
    lut_scope.leaf()

  })


  # Render the LUT SCOPE leaf table ----------------------------------
  output$lut_table_scope.leaf <- renderTable({
    lut_scope.leaf()
  })


  # Render the LUT canopy table ----------------------------------
  output$lut_table_scope.canopy <- renderTable({
    lut_scope.canopy()
  }, tableHeader = "Canopy Parameters")


  # Reactive expression for the LUT data ---------------------------------------------------
  lut_scope.sim <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(params_SCOPE()[[1]])
    bioleaf_params <- unlist(params_SCOPE()[[3]])

    canopy_params <- unlist(params_SCOPE()[[2]])

    # Call the LUT function using the selected parameters
    lut_leaf <- data.frame(t(c(leaf_params,bioleaf_params)) )#, canopy_params))

    lut_canopy <- data.frame(t(canopy_params)) #, canopy_params))

    lut.to_sim <-data.frame(cbind(lut_leaf,lut_canopy))


    # Call the Simulations
    LUT.SCOPE <- SCOPE.LUT.default

    # Assign values from lut.to_sim to LUT.SCOPE
    for (input_name in names(lut.to_sim)) {
      LUT.SCOPE[[input_name]] <- lut.to_sim[[input_name]]
    }



    # Print the LUT.SCOPE dataframe
    print(LUT.SCOPE)
    return(list(LUT.SCOPE))


  })


  # Render the printed output in verbatimTextOutput
  output$lut_scope_output <- renderPrint({
    lut_scope.sim()


  })





  # Reactive expression for the reflectance data ---------------------------------------------------
  get_scope <- reactive({

    LUT_ <- lut_scope.sim()[[1]]

    db.sim <- get.SCOPE(LUT=LUT_,options.SCOPE = data.opts,path.out = 'www/outs/',
                                  optipar=optipar2021.Pro.CX,
                                  leaf.model='fluspect-CX',canopy.model='fourSAIL',
                                  get.outputs = 'Main', get.plots = F)

    reflectance_app <- db.sim[[1]]$data.rad$reflapp
    reflectance_values <- db.sim[[1]]$data.rad$refl
    reflectance_rsd <- db.sim[[1]]$data.rad$rsd

    reflectance_rdo <- db.sim[[1]]$data.rad$rdo

    wave.rfl <- db.sim[[1]]$data.spectral$wlS

    LoF_sunlit <- db.sim[[1]]$data.rad$LoF_sunlit
    LoF_shaded <- db.sim[[1]]$data.rad$LoF_shaded
    LoF_soil<- db.sim[[1]]$data.rad$LoF_soil
    LoF_all<- db.sim[[1]]$data.rad$Femliave_

    LoF_ <- db.sim[[1]]$data.rad$LoF_
    wave.LoF <- db.sim[[1]]$data.spectral$wlF

    Lotot_ <- db.sim[[1]]$data.rad$Lotot_
    Lototf_ <- db.sim[[1]]$data.rad$Lototf_
    wave.Lot <- db.sim[[1]]$data.spectral$wlS
    # Combine the reflectance values with wavelength values
    reflectance_df <- data.frame(wavelength = wave.rfl,
                                 reflapp = reflectance_app,
                                 reflectance = reflectance_values,rdo =reflectance_rdo,
                                 rsd = reflectance_rsd)
    LoF_df <- data.frame(wavelength = wave.LoF,
                         LoF_sunlit = LoF_sunlit, LoF_soil = LoF_soil,
                         LoF_shaded = LoF_shaded, LoF_all = LoF_all,
                         LoF_ = LoF_)
    Lotot_df <- data.frame(wavelength = wave.Lot, Lotot_ = Lotot_,Lototf_ = Lototf_)


    #  return(list(reflectance_df = reflectance_df, LoF_df= LoF_df, Lotot_df= Lotot_df))
    return(list(reflectance_df = reflectance_df, LoF_df= LoF_df, Lotot_df= Lotot_df))


  })


  # Render the printed output in verbatimTextOutput
  output$model_scope_outputs <- renderPrint({
    get_scope()


  })



  # Render the reflectance plot ---------------------------------------------------
  output$reflectance_plot_scope <- renderPlot({

    # Show the modal window
    show_modal_spinner()

    df.to_plot<-get_scope()$reflectance_df

    # Plot the variables using ggplot
    p1 <- ggplot(df.to_plot, aes(x = wavelength)) +
      geom_rect(aes(xmin = 725, xmax = 800, ymin = 0.3, ymax = 0.7), color = 'grey', linetype = 1, alpha = 0.1) +
      geom_line(aes(y = reflectance, color = 'reflectance'), linewidth = 1) +
      geom_line(aes(y = reflapp, color = 'reflect. Apparent'), linewidth = 1) +
      geom_line(aes(y = rdo, color = 'rdo'), linewidth = 1) +
      geom_line(aes(y = rsd, color = 'rsd'), linewidth = 1) +
      labs(x = "Wavelength (nm)", y = "Reflectance") +
      #    scale_color_manual(values = c('black','orange', 'red', 'navyblue'),
      #                      labels = c('reflectance','reflect.App','rdo', 'rsd')) +
      theme_bw() +
      xlim(400, 2400) + ylim(0,0.7) +
      theme(legend.position = 'top',
            #       legend.box.background = element_rect(color = "black",linewidth=1),
            plot.title = element_text(hjust = 0.5, size=14,face="bold"),
            axis.title = element_text(face="bold", size=14),
            legend.text = element_text(face="bold", size=10),
            axis.text.y=element_text(hjust = 0.5, size=12,face="bold"),
            axis.text.x=element_text(hjust = 0.5, size=12,face="bold"),
            legend.title=element_blank())

    # Zoomed-in plot
    p2 <- ggplot(df.to_plot, aes(x = wavelength)) +
      geom_line(aes(y = reflectance, color = 'reflectance'), linewidth = 1) +
      geom_line(aes(y = reflapp, color = 'reflect. Apparent'), linewidth = 1) +
      geom_line(aes(y = rdo, color = 'rdo'), linewidth = 1) +
      geom_line(aes(y = rsd, color = 'rsd'), linewidth = 1) +
      labs(x = "", y = "reflectance") +
      theme_bw() +
      xlim(725, 800) +
      ylim(0.2, 0.7) +
      theme(
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.text.x = element_text(hjust = 0.5, size = 8, face = "bold"),
        legend.title = element_blank()
      )

    # Remove modal window when done

    remove_modal_spinner()
    # Combine the plots
    p1 +
      annotation_custom(ggplotGrob(p2), xmin = 410, xmax = 700, ymin = 0.3, ymax = 0.7) +
      geom_rect(aes(xmin = 400, xmax = 710, ymin = 0.3, ymax = 0.7), color = 'black', linetype = 'dashed', alpha = 0)

    # geom_path(
    #  aes(x, y, group = grp),
    # data = data.frame(x = c(800, 700, 900, 800), y = c(0, 0.4, 0, 0.7), grp = c(1, 1, 2, 2)),
    #  linetype = 'dashed')


  })



  # Create a reactive expression for the accumulated data
  accumulated_data <- reactive({
    if (input$checkbox_sif) {
      # Accumulate the data
      accumulate_spectra(get_scope()$LoF_df)
    } else {
      get_scope()$LoF_df
    }
  })


  # Accumulating spectra -------------------
  accumulate_spectra <- function(df) {
    # //Melt the data frame to long format
    df.melted <- reshape2::melt(df, id.vars = 'wavelength', measure.vars = "LoF_", variable.name = 'Variable', value.name = "Spectrum")

    # Repeat the spectra columns based on the number of repeats you want
    num_repeats <- 5  # Specify the number of repeats you want
    df.melted <- df.melted %>%
      group_by(Variable) %>%
      mutate(Spectrum = rep(Spectrum, num_repeats))

    # Return the accumulated data frame
    df.melted
  }




  # Render the SIF plot ---------------------------------------------------
  output$sif_plot_scope<- renderPlot({



    if (input$checkbox_sif == F) {


      df.to_plot <- accumulated_data()
      ggplot(df.to_plot, aes(x = wavelength)) +
        geom_line(aes(y = LoF_sunlit, color = 'LoF sunlit'), linewidth = 1) +
        # geom_line(aes(y = LoF_shaded, color = 'LoF shaded'), linewidth = 1) +
        #  geom_line(aes(y = LoF_soil, color = 'LoF soil'), linewidth = 1) +
        #  geom_line(aes(y = LoF_all, color = 'LoF all leaves'), linewidth = 1) +
        geom_line(aes(y = LoF_, color = 'LoF_'), linewidth = 1) +

        labs(x = "Wavelength (nm)", y = "fluorescence emission") +
        #   scale_color_manual(values = c('orange', 'red','black'),
        #                     labels = c('LoF_sunlit','LoF_shaded','LoF_total')) +
        theme_bw() + xlim(640,800) +  theme(legend.position = 'top',
                                            #  legend.box.background = element_rect(color = "black",linewidth=1),
                                            plot.title = element_text(hjust = 0.5, size=14,face="bold"),
                                            axis.title = element_text(face="bold", size=14),
                                            legend.text = element_text(face="bold", size=10),
                                            axis.text.y=element_text(hjust = 0.5, size=12,face="bold"),
                                            axis.text.x=element_text(hjust = 0.5, size=12,face="bold"),
                                            legend.title=element_blank()) +
        # legend.title = element_text(face = "bold", size = 14)) +
        guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
    } else {

      df.to_plot <- accumulated_data()
      # Plot the spectra for each variable
      ggplot(df.to_plot, aes(x = wavelength, y = Spectrum, color = )) +
        geom_line(linewidth = 1) +
        labs(x = "Wavelength (nm)", y = "fluorescence emission") +
    #    scale_color_manual(values = color_palette1(num_inputs1)) +
     #   scale_color_manual(values = color_palette2(num_inputs2)) +
        theme_bw()
    }

  })


  ## Render the Lototf_ plot ---------------------------------------------------
  output$Lo_plot_scope<- renderPlot({

    df.to_plot<-get_scope()$Lotot_df

    p1 <-ggplot(df.to_plot, aes(x = wavelength)) +
      geom_rect(aes(xmin = 725, xmax = 800, ymin = 20, ymax = 150), color = 'grey', linetype = 1, alpha = 0.1) +

      geom_line(aes(y = Lototf_, color = 'Lototf_'), linewidth = 1) +
      geom_line(aes(y = Lotot_, color = 'Lotot_'), linewidth = 1) +

      labs(x = "Wavelength (nm)", y = "Radiance excluding/adding fluorescence") +
      theme_bw()  + xlim(400,900) +
  #    scale_color_manual(values = c('orange', 'black'),
   #                      labels = c('Lototf_','Lotot_')) +
    theme(legend.position = 'top',
        #  legend.box.background = element_rect(color = "black",linewidth=1),
          plot.title = element_text(hjust = 0.5, size=14,face="bold"),
          axis.title = element_text(face="bold", size=14),
          legend.text = element_text(face="bold", size=10),
          axis.text.y=element_text(hjust = 0.5, size=12,face="bold"),
          axis.text.x=element_text(hjust = 0.5, size=12,face="bold"),
          legend.title=element_blank()) +
      # legend.title = element_text(face = "bold", size = 14)) +
      guides(color = guide_legend(title.position = "top", title.hjust = 0.5))


    # Zoomed-in plot
    p2 <- ggplot(df.to_plot, aes(x = wavelength)) +
      geom_line(aes(y = Lototf_, color = 'Lototf_'), linewidth = 1) +
      geom_line(aes(y = Lotot_, color = 'Lotot_'), linewidth = 1) +

      labs(x = "", y = "Radiance") +
      theme_bw() +
      xlim(725, 800) +
      ylim(0, 150) +
      theme(
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(face = "bold", size = 10),
        axis.text.y = element_text(hjust = 0.5, size = 8, face = "bold"),
        axis.text.x = element_text(hjust = 0.5, size = 8, face = "bold"),
        legend.title = element_blank())

    p1 +
      annotation_custom(ggplotGrob(p2), xmin = 410, xmax = 600, ymin = 50, ymax = 150) +
      geom_rect(aes(xmin = 400, xmax = 610, ymin = 50, ymax = 150), color = 'black', linetype = 'dashed', alpha = 0)



  })

  ########## end SCOPE module ######################################################

  ########### 2) SPART -------------------------------

  ### Define table for SPART model ---------------------------------------------------


  output$spart_table <- renderTable({
    data <- data.frame(
      Parameter = c('Pa','aot550', 'uo3','uh2o','alt_m','Pa0'),
      Description = c('Air pressure', 'AOT at 550 nm', 'Ozone content ','Water vapour','altitude','sea level air pressure'),
      Units = c('hPa','-','cm','g cm-2','m','hPa'),
      Min = c(400,0,0,0,-1000,900),
      Max = c(1300,2.5,5,5,600,1100),
      Default = c(-999,0.3246,0.3480,1.4116,0.0,1013.25)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })


  # Reactive expression for the leaf and canopy parameters -------------------------------
  params_spart <- reactive({
    leaf_params <- switch(input$leaf_spart,

                          "PROSPECT-PRO" = c(N=input$N_spart,Cab=input$Cab_spart, Car=input$Car_spart,
                                             Anth=input$Anth_spart, Cbrown=input$Cbrown_spart,
                                             EWT=input$EWT_spart, LMA=0, alpha=40, #input$alpha,
                                             Prot=input$Prot_spart, CBC=input$CBC_spart))
    ## parameters for fourSAIL

    canopy_params <- switch(input$canopy_spart,
                            "fourSAILH" = c(LAI=input$LAI_spart, TypeLidf=2, LIDFa=input$LIDFa_spart, LIDFb=0, hspot=input$hotspot_spart,
                                            tts=input$tts_spart, tto=input$tto_spart, psi=input$psi_spart,
                                            LAIu=0.100,sd=640, h=20, cd=4.50,
                                            psoil=0.5))


    atmos_params <- switch(input$atmos,
                            "SPART" = c(Pa=input$Pa_spart, aot550=input$aot550_spart, uo3=input$uo3_spart,
                                        uh2o=input$uh2o_spart, alt_m=0, Pa0=1013.25,
                                        skyl=0.1))


    return(list(leaf_params, canopy_params,atmos_params))
  })


  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.leaf.spart <- reactive({

    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(params_spart()[[1]])
    leaf_params_names<-names(params_spart()[[1]])

    # Call the LUT function using the selected parameters
    lut_leaf <- data.frame(rbind(leaf_params)) #, canopy_params))
    print(lut_leaf)
    return(lut_leaf)
  })

  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.canopy.spart <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    canopy_params <- unlist(params_spart()[[2]])
    # Call the LUT function using the selected parameters
    lut_canopy <- data.frame(rbind(canopy_params)) #, canopy_params))
    print(lut_canopy)
    return(lut_canopy)
  })

  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.atmos.spart <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    atmos_params <- unlist(params_spart()[[3]])
    # Call the LUT function using the selected parameters
    lut_atmos <- data.frame(rbind(atmos_params)) #, canopy_params))
    print(lut_atmos)
    return(lut_atmos)
  })

  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.sim_spart <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(params_spart()[[1]])

    canopy_params <- unlist(params_spart()[[2]])

    atmos_params <- unlist(params_spart()[[3]])

    # Call the LUT function using the selected parameters
    lut_leaf <- data.frame(rbind(leaf_params)) #, canopy_params))
    lut_canopy <- data.frame(rbind(canopy_params)) #, canopy_params))
    lut_atmos <- data.frame(rbind(atmos_params)) #, canopy_params))
    lut.to_sim <-cbind(lut_leaf,lut_canopy,lut_atmos)

    # Call the Simulations
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]  # rsoil2 = wet soil
    psoil = lut.to_sim[1,'psoil']
    #psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)
    rsoil<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
    #print(lut.to_sim)
    #reflectance_values <- -foursail(inputLUT=lut.to_sim[1,],rsoil=rsoil,LeafModel = 'PRO')
    return(list(lut.to_sim,rsoil))


  })

  # Reactive expression for the reflectance data ---------------------------------------------------
  reflectance_data_spart <- reactive({

    LUT_ <- lut_data.sim_spart()[[1]]
    rsoil_ <- lut_data.sim_spart()[[2]]

    if (input$sensor_spart == 'Sentinel2a') {

      sensor.i = Sentinel2A.MSI


    } else if (input$sensor_spart == 'Sentinel2b') {

      sensor.i = Sentinel2B.MSI


    } else if (input$sensor_spart == 'Landsat-4') {

      sensor.i = LANDSAT4.TM

    } else if (input$sensor_spart == 'Landsat-5') {

      sensor.i = LANDSAT5.TM


    } else if (input$sensor_spart == 'Landsat-7') {

      sensor.i = LANDSAT7.ETM


    } else if (input$sensor_spart == 'Landsat-8') {

      sensor.i = LANDSAT8.OLI


    } else if (input$sensor_spart == 'MODIS') {
      sensor.i = TerraAqua.MODIS

    }

    data.spart<- SPART(inputLUT = LUT_[1,],optipar=optipar2021.Pro.CX,
                                      CanopyModel = 'fourSAIL',
                                      LeafModel='PROSPECT-PRO',
                                      df.irradiance = NULL,
                                      sensor.i = sensor.i)
    rfl.toa <- data.spart$output$rfl.toa
    rfl.toc <- data.spart$output$rfl.toc
    rfl.toc.brdf <- data.spart$output$rfl.toc.BRDF
    reflectance_df <- data.frame(wavelength = data.spart$output$wave, rfl.toa = rfl.toa,
                                 rfl.toc=rfl.toc,rfl.toc.brdf=rfl.toc.brdf)
    return(reflectance_df)

  })


  # Render the reflectance plot ---------------------------------------------------
  output$reflectance_plot_spart <- renderPlot({


    to_plot<-reflectance_data_spart()
    #print(to_plot)
    ggplot(data = to_plot, aes(x = wavelength)) +
      labs(y = "Reflectance", x = "") +
      geom_point(aes(y = rfl.toa, color = "TOA rfl."), size = 2) +
      geom_line(aes(y = rfl.toa, color = "TOA rfl."),size = 1.5) +

      geom_point(aes(y = rfl.toc, color = "TOC rfl. (SMAC)"), size = 2) +
      geom_line(aes(y = rfl.toc, color = "TOC rfl. (SMAC)"), size = 1.5) +

      theme_bw() +
      guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""), shape = guide_legend(title = "")) +

      theme(legend.position = "top",
            legend.key.size = unit(4, "lines"),
            text = element_text(size = 16, face='bold'),  # Increase the text size
            legend.text = element_text(face = "bold", size = 16), # Increase legend text size and make it bold
            axis.title.x = element_text(face = "bold", size = 18), # Increase x axis label size and make it bold
            axis.title.y = element_text(face = "bold", size = 18)) # Increase y axis label size and make it bold


  })

  # Create a reactive data frame for saving data
  save_data_spart <- reactive({
    to_save<-reflectance_data_spart()
  })
  # Add the download handler for saving data
  output$downloadData_spart <- downloadHandler(
    filename = function() {
      paste("SPART_sim_with_", input$leaf_spart,'_', input$canopy_spart,'_', input$sensor_spart, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(save_data_spart(), file, row.names = FALSE)
    }
  )

# 1) ToolsRTM   ---------------------------------------------------


  # Reactive expression for the leaf and canopy parameters -------------------------------

  params <- reactive({
    leaf_params <- switch(input$leaf_model,

                          "PROSPECT-PRO" = c(N=input$N,Cab=input$Cab, Car=input$Car, Anth=input$Anth, Cbrown=input$Cbrown,
                                             EWT=input$EWT, LMA=0, alpha=40, #input$alpha,
                                             Prot=input$Prot, CBC=input$CBC),
                          "PROSPECT-D" = c(N=input$N_d,Cab=input$Cab_d, Car=input$Car_d,  Anth=input$Anth_d,Cbrown=input$Cbrown_d,
                                           EWT=input$EWT_d, LMA=input$LMA_d, alpha=40),#input$alpha_d),

                          "Liberty" = c(cell.d=input$cell_d, inter.c=input$inter_c,
                                        baseline.abs=input$baseline_abs, leaf.thick=input$leaf_thick,
                                        albino.abs=input$albino_abs,
                                        Cab=input$Cab_l, EWT=input$EWT_l,
                                        lign.cell=input$lign_cell, Nitrogen=input$Nitrogen),


                          "FLUSPECT-B" = c(N=input$N_fd,Cab = input$Cab_fd, Car = input$Car_fd, Cs=input$Cs_fd,
                                           EWT=input$EWT_fd,LMA=input$LMA_fd,
                                           Cx = input$Cx_fd,
                                           fqe = input$fqe_fd),

                          "FLUSPECT-B-Cx" = c(N=input$N_fp,Cab = input$Cab_fp, Car = input$Car_fp, Cs=input$Cs_fp,
                                             EWT=input$EWT_fp,LMA=input$LMA_fp,
                                             Cx = input$Cx_fp,
                                             fqe = input$fqe_fp))


    ## parameters for fourSAIL


    canopy_params <- switch(input$canopy_model,
                            "fourSAILH" = c(LAI=input$LAI, TypeLidf=2, LIDFa=input$LIDFa, LIDFb=0, hspot=input$hotspot,
                                            tts=input$tts, tto=input$tto, psi=input$psi, psoil=input$psoil),


                            "INFORM" = c(LAI=input$LAI_, TypeLidf=2, LIDFa=input$LIDFa_, LIDFb=0, hspot=input$hotspot_,
                                         tts=input$tts_, tto=input$tto_, psi=input$psi_, psoil=input$psoil_,
                                         LAIu=input$LAIu_, cd=input$cd_, sd=input$sd_, h=input$h_, skyl=0.1),


                            "fourSAILH2" = c(p1=input$p1_s2, p2=input$p2_s2, p3=input$p3_s2))
    return(list(leaf_params, canopy_params))
  })


  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.leaf <- reactive({

    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(params()[[1]])
    leaf_params_names<-names(params()[[1]])

    # Call the LUT function using the selected parameters
    lut_leaf <- data.frame(rbind(leaf_params)) #, canopy_params))

    return(lut_leaf)
  })

  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.canopy <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    canopy_params <- unlist(params()[[2]])
    # Call the LUT function using the selected parameters
    lut_canopy <- data.frame(rbind(canopy_params)) #, canopy_params))
    return(lut_canopy)
  })

  # Define the reactive expression for the leaf parameters ---------------------------------------------------
  leaf_params_prospect <- reactive({

    if(input$leaf_model == "PROSPECT-PRO" && (input$CBC != 0 & input$Prot != 0)) {

      leaf_params <- c(N = input$N, Cab = input$Cab, Car = input$Car, Anth = input$Anth,
                       Cbrown = input$Cbrown,EWT = input$EWT, LMA = 0, alpha = 40,#input$alpha,
                       Prot = input$Prot, CBC = input$CBC)

    } else if (input$leaf_model == "PROSPECT-D") {

      leaf_params <- c(N = input$N_d, Cab = input$Cab_d, Car = input$Car_d,  Anth=input$Anth_d, Cbrown = input$Cbrown_d,
                       alpha = 40, EWT = input$EWT_d, LMA = input$LMA_d) #input$alpha_d

    } else if (input$leaf_model == "Liberty") {

      leaf_params <- c(cell.d=input$cell_d, inter.c=input$inter_c,
                       baseline.abs=input$baseline_abs, leaf.thick=input$leaf_thick,
                       albino.abs=input$albino_abs,
                       Cab=input$Cab_l, EWT=input$EWT_l,
                       ## Adding PROSAIL-inputs for  Infinitive crown reflectance /unerstory
                       N = input$N, Car = input$Car, Anth = input$Anth,
                       Cbrown = input$Cbrown, LMA = input$LMA, alpha = 40,#input$alpha,
                       ##
                       lign.cell=input$lign_cell, Nitrogen=input$Nitrogen)



    } else if (input$leaf_model == "FLUSPECT-B") {

      leaf_params <- c(N = input$N_fd, Cab = input$Cab_fd, Car = input$Car_fd, Anth = 0, Cbrown= input$Cs_fd,
                       Cs = input$Cs_fd, Cx = input$Cx_fd,
                       EWT=input$EWT_fd,LMA=input$LMA_fd, alpha = 40, Prot = 0, CBC = 0,
                       fqe = input$fqe_fd)



    } else if (input$leaf_model == "FLUSPECT-B-Cx") {

      leaf_params <- c(N = input$N_fp, Cab = input$Cab_fp, Car = input$Car_fp, Anth=input$Anth_fp, Cbrown= input$Cs_fp,
                       Cs = input$Cs_fp, Cx = input$Cx_fp,
                       EWT=input$EWT_fp,LMA=input$LMA_fp, alpha = 40, Prot = input$Prot_fp,
                       CBC = input$CBC_fp,
                       fqe = input$fqe_fo)



    } else if (input$leaf_model == "PROSPECT-PRO" && (input$CBC == 0 & input$Prot == 0)) {

      leaf_params <- c(N = input$N, Cab = input$Cab, Car = input$Car, Anth = input$Anth, Cbrown = input$Cbrown,
                       EWT = input$EWT, LMA = input$LMA, alpha = 40, Prot = 0, CBC = 0) #input$alpha,
    }
    return(list(leaf_params))

  })


  # Reactive expression for the LUT data ---------------------------------------------------
  lut_data.sim <- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- unlist(leaf_params_prospect()[[1]])

    canopy_params <- unlist(params()[[2]])

    # Call the LUT function using the selected parameters
    lut_leaf <- data.frame(rbind(leaf_params)) #, canopy_params))
    lut_canopy <- data.frame(rbind(canopy_params)) #, canopy_params))
    lut.to_sim <-cbind(lut_leaf,lut_canopy)


    # Call the Simulations
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]  # rsoil2 = wet soil
    psoil = lut.to_sim[1,'psoil']
    #psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)
    rsoil<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
    #reflectance_values <- -foursail(inputLUT=lut.to_sim[1,],rsoil=rsoil,LeafModel = 'PRO')
    return(list(lut.to_sim,rsoil))

  })



  # Reactive expression for the reflectance data ---------------------------------------------------
  reflectance_data <- reactive({

    LUT_ <- lut_data.sim()[[1]]
    rsoil_ <- lut_data.sim()[[2]]

    # Call the RTM function using the selected leaf and canopy parameters
    # return a list of two vectors
    if (input$canopy_model == 'fourSAILH' & input$leaf_model == 'PROSPECT-PRO'){

      reflectance_values <- foursail(inputLUT=LUT_[1,],rsoil=rsoil_, LeafModel = 'PROSPECT-PRO')
      rdot<-reflectance_values[[1]]
      rsot<-reflectance_values[[2]]
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'],data.light=dataSpec_PDB)

    } else if  (input$canopy_model == 'fourSAILH' & input$leaf_model == 'PROSPECT-D'){

      reflectance_values <- foursail(inputLUT=LUT_[1,],rsoil=rsoil_, LeafModel = 'PROSPECT-D')
      rdot<-reflectance_values[[1]]
      rsot<-reflectance_values[[2]]
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'],data.light=dataSpec_PDB)

    } else if  (input$canopy_model == 'fourSAILH' & input$leaf_model == 'Liberty'){

      reflectance_values <- foursail(inputLUT=LUT_[1,],rsoil=rsoil_,LeafModel = 'Liberty')
      rdot<-reflectance_values[[1]]
      rsot<-reflectance_values[[2]]
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'],data.light=dataSpec_PDB)

    } else if  (input$canopy_model == 'fourSAILH' & input$leaf_model == 'FLUSPECT-B'){

      reflectance_values <- foursail(inputLUT=LUT_[1,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')
      rdot<-reflectance_values[[1]]
      rsot<-reflectance_values[[2]]
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'],data.light=dataSpec_PDB, short.waves = T)

    } else if  (input$canopy_model == 'fourSAILH' & input$leaf_model == 'FLUSPECT-B-Cx'){

      reflectance_values <- foursail(inputLUT=LUT_[1,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')
      rdot<-reflectance_values[[1]]
      rsot<-reflectance_values[[2]]
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'],data.light=dataSpec_PDB, short.waves = T)

    } else if  (input$canopy_model == 'INFORM' & input$leaf_model == 'PROSPECT-PRO'){

      reflectance_values <- inform(inputLUT = LUT_[1,],rsoil=rsoil_,LeafModel = 'PROSPECT-PRO')


    } else if  (input$canopy_model == 'INFORM' & input$leaf_model == 'PROSPECT-D'){

      reflectance_values <- inform(inputLUT = LUT_[1,],rsoil=rsoil_,LeafModel = 'PROSPECT-D')

    } else if  (input$canopy_model == 'INFORM' & input$leaf_model == 'FLUSPECT-B'){

      reflectance_values <- inform(inputLUT = LUT_[1,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')

    } else if  (input$canopy_model == 'INFORM' & input$leaf_model == 'FLUSPECT-B-Cx'){

      reflectance_values <- inform(inputLUT = LUT_[1,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')

    } else if  (input$canopy_model == 'INFORM' & input$leaf_model == 'Liberty'){

      reflectance_values <- inform(inputLUT = LUT_[1,],rsoil=rsoil_,LeafModel = 'Liberty')

    } else {

      reflectance_values<-sim.liberty$refl

    }
    wavelength=seq(400,2500,1)

    # Combine the reflectance values with wavelength values
    if (input$leaf_model == 'FLUSPECT-B-Cx' | input$leaf_model == 'FLUSPECT-B'){

      reflectance_df <- data.frame(wavelength = wavelength[1:2001], reflectance = reflectance_values)
      return(reflectance_df)
    } else {

      reflectance_df <- data.frame(wavelength = wavelength, reflectance = reflectance_values)
      return(reflectance_df)
    }



  })





  # Render the reflectance plot ---------------------------------------------------
  output$reflectance_plot <- renderPlot({



    if (input$sensor == 'RTM'){

      to_plot<-reflectance_data()


    #  if (input$checkbox_spectra) {

     #   to_plot_nd<-c(reflectance_data()$reflectance)


      #}

    } else if (input$sensor == 'Sentinel2a') {

      to_plot <- reflectance_data()


      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = Sentinel2A.MSI
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)





    }  else if (input$sensor == 'Sentinel2b') {

      to_plot <- reflectance_data()

      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = Sentinel2B.MSI
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)

    }   else if (input$sensor == 'Landsat4') {

      to_plot <- reflectance_data()


      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = LANDSAT4.TM
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)

    } else if (input$sensor == 'Landsat5') {

      to_plot <- reflectance_data()

      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = LANDSAT5.TM
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)


    } else if (input$sensor == 'Landsat7') {

      to_plot <- reflectance_data()

      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = LANDSAT7.ETM
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)

    } else if (input$sensor == 'Landsat8') {

      to_plot <- reflectance_data()

      Spec.simula<- data.frame(wave = to_plot$wavelength, rfl=to_plot$reflectance)
      sensor.i = LANDSAT8.OLI
      df_resampled <-get.spectral.convolution.rfl(df = Spec.simula,sensor.i, get.plots=F)
      to_plot <- data.frame(wavelength = df_resampled$wave, reflectance = df_resampled$RFL)


    } else if (input$sensor == 'MODIS') {

      to_plot <- reflectance_data()



    } else if (input$sensor == 'RapidEye') {

      to_plot <- reflectance_data()


    } else if (input$sensor == 'Quickbird') {

      to_plot <- reflectance_data()


    } else if (input$sensor == 'EnMAP') {

      to_plot <- reflectance_data()

      ###

    } else if (input$sensor == 'PRISMA') {

      to_plot <- reflectance_data()


    } else if (input$sensor == 'Hyperion') {

      to_plot <- reflectance_data()



    } else if (input$sensor == 'WorldView2-4') {

      to_plot <- reflectance_data()


    } else if (input$sensor == 'WorldView2-8') {

      to_plot <- reflectance_data()

    } else if (input$sensor == 'ALI') {

      to_plot <- reflectance_data()


    }

    # Plot Polygons by Sensor  ---------------------------------------------------

    # Create a list of polygons for each bandset ---------------------------------------------------
    sentinel2a_polygons <- list(
      geom_rect(xmin = 432.2, xmax = 453.2, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 459.4, xmax = 525.4, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 541.8, xmax = 577.8, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 649.1, xmax = 680.1, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 696.6, xmax = 711.6, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 733.0, xmax = 748.0, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 772.8, xmax = 792.8, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 779.8, xmax = 885.8, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 854.2, xmax = 875.2, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 935.1, xmax =  955.1, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1358.0, xmax = 1389.0, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1568.2, xmax =   1659.2, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2114.9, xmax =  2289.9, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    sentinel2b_polygons <- list(
      geom_rect(xmin = 431.8, xmax = 452.8, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 459.1, xmax = 525.1, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 541.0, xmax = 577.0, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 649.5, xmax = 680.5, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 695.8, xmax = 711.8, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 731.6, xmax = 746.6, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 769.7, xmax = 789.7, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 780.0, xmax = 886.0, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 853.0, xmax = 875.0, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 932.7, xmax =  953.7, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1361.9, xmax = 1391.9, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1563.4, xmax = 1657.4, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2093.2, xmax =  2278.2, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    modis_polygons <- list(
      geom_rect(xmin = 405, xmax = 420, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 438, xmax = 448, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 459, xmax = 479, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 483, xmax = 493, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 526, xmax = 536, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 546, xmax = 556, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 545, xmax = 565, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5),
      geom_rect(xmin = 662, xmax = 672, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 673, xmax = 683, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 743, xmax = 753, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 862, xmax = 877, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 890, xmax =  920, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 915, xmax = 965, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 931, xmax = 941, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1230, xmax = 1250, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1628, xmax = 1652, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2105, xmax = 2155, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5)
    )

    landsat45_polygons <- list(
      geom_rect(xmin = 450, xmax = 520, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 520, xmax = 600, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5),
      geom_rect(xmin = 630, xmax = 690, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 760, xmax = 900, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1550, xmax = 1750, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2080, xmax = 2350, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    landsat7_polygons <- list(
      geom_rect(xmin = 450, xmax = 520, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 530, xmax = 610, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 630, xmax = 690, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5),
      geom_rect(xmin = 780, xmax = 900, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1550, xmax = 1750, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2090, xmax = 2350, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    landsat8_polygons <- list(
      geom_rect(xmin = 427, xmax = 462, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5),
      geom_rect(xmin = 435, xmax = 530, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 512, xmax = 602, ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5),
      geom_rect(xmin = 625, xmax = 685, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 830, xmax = 897, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1340, xmax = 1405, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1515, xmax = 1697, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2037, xmax = 2352, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    ali_polygons <- list(
      geom_rect(xmin = 433, xmax = 453, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 450, xmax = 525, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 525, xmax = 605, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 630, xmax = 690, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 775, xmax = 805, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 845, xmax = 890, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1200, xmax = 1300, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 1550, xmax = 1750, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5),
      geom_rect(xmin = 2080, xmax = 2350, ymin = -Inf, ymax = Inf, fill = "grey80", alpha = 0.5))

    if (input$sensor != 'RTM') {



      # Is Add region is active, ggplot plot the spectral bandwidth  of the sensors

      if (input$AddRegion == T) {

        # Determine which polygons to add based on the selected sensor
        if (input$sensor == "Sentinel2a") {
          polygons_to_add <- sentinel2a_polygons
        } else if (input$sensor == "Sentinel2b") {
          polygons_to_add <- sentinel2b_polygons
        } else if (input$sensor == "Landsat4" | input$sensor == "Landsat5") {
          polygons_to_add <- landsat45_polygons
        } else if (input$sensor == "Landsat7") {
          polygons_to_add <- landsat7_polygons
        } else if (input$sensor == "Landsat8") {
          polygons_to_add <- landsat8_polygons
        } else if (input$sensor == "ALI") {
          polygons_to_add <- ali_polygons
        } else if (input$sensor == "MODIS") {
          polygons_to_add <- modis_polygons
        } else {
          polygons_to_add <- NULL
        }

      } else {
        polygons_to_add <- NULL
      }


      ggplot(reflectance_data(), aes(x = wavelength, y = reflectance)) +
        polygons_to_add +
        geom_line(aes(color='RTM'), size = 0.8) +

        geom_line(data = to_plot, aes(x = wavelength, y = reflectance, color = "Sensor"),
                  lwd=1, linetype = "dashed") +
        geom_point(data = to_plot, aes(x = wavelength, y = reflectance), color = "red",cex=3) +
        scale_color_manual(name = "Reflectance", values = c("RTM" = "black", "Sensor" = "dodgerblue3"),
                           labels = c("RTM", input$sensor)) +
        labs(x = "Wavelength (nm)", y = "Reflectance") +
        # Set axis labels and title
        theme_bw() + ylim(0,0.7) +
        theme(legend.position = "insider",
              legend.box.background = element_rect(color = "black",linewidth=1),
              plot.title = element_text(hjust = 0.5, size=14,face="bold"),
              axis.title = element_text(face="bold", size=14),
              legend.text = element_text(face="bold", size=10),
              axis.text.y=element_text(hjust = 0.5, size=12,face="bold"),
              axis.text.x=element_text(hjust = 0.5, size=12,face="bold"),
              legend.title=element_blank()) +
        # legend.title = element_text(face = "bold", size = 14)) +
        guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
    } else {

      ggplot(to_plot, aes(x = wavelength, y = reflectance)) +
        geom_line(aes(color='RTM'), linewidth = 1,color='black') +
        labs(x = "Wavelength (nm)", y = "Reflectance") +
        theme_bw() + ylim(0,0.7) +
        theme(legend.position = 'none',
              legend.box.background = element_rect(color = "black",linewidth=1),
              plot.title = element_text(hjust = 0.5, size=14,face="bold"),
              axis.title = element_text(face="bold", size=14),
              legend.text = element_text(face="bold", size=10),
              axis.text.y=element_text(hjust = 0.5, size=12,face="bold"),
              axis.text.x=element_text(hjust = 0.5, size=12,face="bold"),
              legend.title=element_blank()) +
        # legend.title = element_text(face = "bold", size = 14)) +
        guides(color = guide_legend(title.position = "top", title.hjust = 0.5))
    }

  })
    #### Plot Polygons by Sensor  -------------------------


  #### Create a reactive data frame for saving data  ---------------------------------------------------

  export_table <- reactive({

    if (input$sensor != 'RTM') {
      rfl_ <- t(reflectance_data()$reflectance)
      wave_names <- paste('R',reflectance_data()$wavelength,sep='.')
    } else {
      rfl_ <- t(reflectance_data()$reflectance)
      wave_names <- paste('R',reflectance_data()$wavelength,sep='.')
    }

    LUT_ <- lut_data.sim()[[1]]
    to_save<-cbind(LUT_,rfl_)
    #colnames(to_save) <- c(names(LUT_),wave_names)

  })
      #### Add the download handler for saving data ---------------
  output$downloadData_sim <- downloadHandler(
    filename = function() {
      paste("LUT_", input$leaf_model,'_', input$canopy_model,'_', input$sensor, ".csv", sep = "")
    },
    content = function(file) {

      write.csv(export_table(), file, row.names = FALSE)
    }
  )


  ### Define table for Liberty model ---------------------------------------------------

  output$Liberty_table <- renderTable({
    data <- data.frame(
      Parameter = c('cell.d','inter.c','baseline.abs', 'leaf.thick',
                    'albino_abs','Cab','EWT','lign.cell','Nitrogen'),
      Description = c('Cell diameter','Intercellular air space', 'baseline absorption','leaf thickness',
                      'Albino absorption', 'Chlorophyll content', 'Leaf water content','Lignin and cellulose content',
                      'leaf nitrogen'),
      Units = c('m-6','-','-','-','-','μg cm-2','μg cm-2','-','g cm-2'),
      Min = c(20,0.01,0.0004,1,0,0,0,10,0.3),
      Max = c(200,0.1,0.0006,10,4,60,0.05,80,2),
      Default = c(45,0.0045,0.0004,1.6,2,40,0.009,40,1)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })


  ### Define table for Prospect model ---------------------------------------------------


  output$prospect_table <- renderTable({
    data <- data.frame(
      Parameter = c('N','Cab','Car', 'Anth','Cbrown','EWT','LMA','alpha','Prot','CBC'),
      Description = c('Leaf mesophyll','Chlorophyll content', 'Carotenoid content', 'Anthocyanins content',
                      'Brown pigments','Leaf water content','Leaf matter content','alpha','leaf proteins','carbon-based Constutient'),
      Units = c('-','μg cm-2','μg cm-2','μg cm-2','-','g cm-2','g cm-2','-','g cm-2','g cm-2'),
      Min = c(1,0,0,0,0,0,0,0,0,0),
      Max = c(4,100,40,7,1,0.05,0.05,60,0.05,0.05),
      Default = c(2.5,40,10,0,0,0.009,0.0012,40,0.01,0.01)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })


  ### Define table for Prospect model ---------------------------------------------------


  output$fluspect_table <- renderTable({
    data <- data.frame(
      Parameter = c('N','Cab','Car', 'Anth','Cbrown','EWT','LMA','Cs','Prot','CBC', 'Cx'),
      Description = c('Leaf mesophyll','Chlorophyll content', 'Carotenoid content', 'Anthocyanins content',
                      'Brown pigments','Leaf water content','Leaf matter content','Leaf senescence',
                      'leaf proteins','carbon-based Constutient', 'Violaxanthin - Zeaxanthin transition status'),
      Units = c('-','μg cm-2','μg cm-2','μg cm-2','-','g cm-2','g cm-2','-','g cm-2','g cm-2','-'),
      Min = c(1,0,0,0,0,0,0,0,0,0,0),
      Max = c(4,100,40,7,1,0.05,0.05,1,0.05,0.05,1),
      Default = c(2.5,40,10,0,0,0.009,0.0012,0.1,0.01,0.01,0.1)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })



  ### Define table for INFORM  model ---------------------------------------

  output$inform_table <- renderTable({
    data <- data.frame(
      Parameter = c('LAIu','sd','h', 'cd'),
      Description = c('Leaf Area Index understorey','Stem density ','Tree height',
                      'Crown diameter'),
      Units = c('m2 m-2','ha-1','m','m'),
      Min = c(0.01,0,0,0.1),
      Max = c(3,3000,50,10),
      Default = c(0.1,650,20,4.5)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })



  ### Define table for foursailh model ---------------------------------------------------

  output$foursailh_table <- renderTable({
    data <- data.frame(
      Parameter = c('LAI','LIDFa','LIDFb', 'hotspot',
                    'tts','tto','psi','psoil','Soil reflectance'),
      Description = c('Leaf Area Index','leaf inclination distribution function a','Type of leaf inclination distribution function b',
                      'Hot Spot parameter','Sun zeith angle','Observer zeith angle','zimuth Sun / Observer','soil factor','Soil reflectance'),
      Units = c('m2 m-2','deg','deg','-','deg','deg','deg','-','%'),
      Min = c(0.001,0,0,0,0,0,0,0,NA),
      Max = c(10,90,0,1,90,90,180,1,NA),
      Default = c(4,30,0,0.5,0,30,0,0.2,NA)
    )
    colnames(data) <- c("Input", "Description", "Units", "Min", "Max","default")
    return(data)
  })

  # 4) LUT generator ---------------------------------------------------

  v <- reactiveValues(data = NULL)



#   observeEvent(input$buttonLUT, {
#
#     ### Define a reactive expression for the progress bar value
#     progress_val <- reactive({
#       # Calculate the progress bar value based on input values
#       input$n_samples * 1 # Change this value to reflect your own calculation
#     })
#   ### Plot for LUTs using RT models ---------------------------------------------------
#   # withProgress(
#   #         message = "Loading...",
#   #         detail = "Please wait...",
#   #         value = 0,
#   #      #   {
#   #
#   #      #     for (i in seq(1, progress_val(), by = 1)) {
#   #       #      Sys.sleep(0.001)
#   #        #     incProgress(1 / progress_val(), detail = sprintf("%d%%", round(i / progress_val() * 100)))
#   #         #  }
#   #
#   #       #  }
#   # )
#
#
# })  ## end observeEvent(input$buttonLUT)




  # Reactive expression for the reflectance data ---------------------------------------------------
  LUT_to <- reactive({

    # Return the data frame
    data_lut_leaf <- get_data(RTmodel = input$model_selected_leaf, input$n_samples, input$seed)
    data_lut_canopy <- get_data(RTmodel = input$model_selected_canopy, input$n_samples, input$seed)
    data_lut <- cbind(data_lut_leaf,data_lut_canopy)

    SE2_bands <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10', 'B11','B12')
    # Call the Simulations
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]  # rsoil2 = wet soil


    LUT_ <- lut_data.sim()[[1]]
    rsoil_ <- lut_data.sim()[[2]]
    output$plot <- renderPlot({

      # Show the modal window
      #show_modal_spinner()
      #remove_modal_spinner()


      if ((input$model_selected_leaf == 'PROSPECT-PRO') & (input$model_selected_canopy == 'fourSAILH')) {

        # choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()

        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadFunctions()
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")

          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-PRO')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
          sim.rfl[[i]]<-rfl.prosail

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)


      } else if ((input$model_selected_leaf == 'PROSPECT-D') & (input$model_selected_canopy == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples, .export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-D')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
          sim.rfl[[i]]<-rfl.prosail

        }

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
          sim.rfl[[i]]<-rfl.prosail

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)


      } else if ((input$model_selected_leaf == 'PROSPECT-PRO') & (input$model_selected_canopy == 'INFORM')) {

        data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

        ## choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()

          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-PRO')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)


      } else if ((input$model_selected_leaf == 'PROSPECT-D') & (input$model_selected_canopy == 'INFORM')) {

        data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

        ## choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-D')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle
        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'INFORM')) {


        data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

        ## choose number of processors/cores
        no_cores <- parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      }

      sim.canopy<-do.call(rbind,sims)
      wave<-seq(400,2500,1)

      #soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))

      IDs<-c(1:input$n_samples)
      # Convert matrix to data frame
      df <- data.frame(sim.canopy)
      df$row <- 1:nrow(df)  # Add a row identifier
      ###
      if (input$sensor_selected == 'RTM') {

        df.to_export <- cbind(IDs,data_lut,df)

      } else if (input$sensor_selected == 'Sentinel2a') {

        fwhm <-c(Sentinel2A.MSI$band_width)
        center_wvl <-c(Sentinel2A.MSI$center_wvl)
        sim.canopy<-as.matrix(do.call(rbind,sims))

        # Initialize an empty list to store interpolated reflectance data
        SE2_ <-list()
        SE2_df <- list()

        # Interpolate each row of the matrix
        reflectance <- apply(sim.canopy, 1, function(row) {
          interp1(x = seq_along(row), y = row, xi = seq_along(center_wvl), method = "spline")
        })


        # Convert the list to a matrix
        df.SE2 <- do.call(rbind, SE2_df)
        df.SE2_rfl <- do.call(rbind, SE2_)
        colnames(df.SE2_rfl) <-SE2_bands


        df.to_export <- cbind(IDs,data_lut,df.SE2_rfl)


      } else if (input$sensor_selected == 'Sentinel2b') {

        fwhm <-c(Sentinel2B.MSI$band_width)
        center_wvl <-c(Sentinel2B.MSI$center_wvl)
        sim.canopy<-as.matrix(do.call(rbind,sims))



        df.to_export <- cbind(IDs,data_lut,df.SE2_rfl)


      } else if (input$sensor_selected == 'Landsat4') {

        fwhm <-c(LANDSAT4.TM$band_width)
        center_wvl <-sort(c(LANDSAT4.TM$center_wvl))[1:6]
        sim.canopy<-as.matrix(do.call(rbind,sims))


        # Convert the list to a matrix
        df.SE2 <- do.call(rbind, SE2_df)
        df.SE2_rfl <- do.call(rbind, SE2_)
        colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')


        df.to_export <- cbind(IDs,data_lut,df.SE2)

      } else if (input$sensor_selected == 'Landsat5') {

        fwhm <-c(LANDSAT5.TM$band_width)
        center_wvl <-sort(c(LANDSAT5.TM$center_wvl))[1:6]
        sim.canopy<-as.matrix(do.call(rbind,sims))

        # Initialize an empty list to store interpolated reflectance data
        SE2_ <-list()
        SE2_df <- list()


        # Convert the list to a matrix
        df.SE2 <- do.call(rbind, SE2_df)
        df.SE2_rfl <- do.call(rbind, SE2_)
        colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')


      } else if (input$sensor_selected == 'Landsat7') {

        fwhm <-c(LANDSAT7.ETM$band_width)
        center_wvl <-sort(c(LANDSAT7.ETM$center_wvl))[1:7]
        sim.canopy<-as.matrix(do.call(rbind,sims))


        # Convert the list to a matrix
        df.SE2 <- do.call(rbind, SE2_df)
        df.SE2_rfl <- do.call(rbind, SE2_)
        colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')



      }   else if (input$sensor_selected == 'Landsat8') {

        fwhm <-c(LANDSAT8.OLI$band_width)
        center_wvl <-sort(c(LANDSAT8.OLI$center_wvl))
        sim.canopy<-as.matrix(do.call(rbind,sims))

        # Initialize an empty list to store interpolated reflectance data
        SE2_ <-list()
        SE2_df <- list()
        # Perform linear interpolation for each row in sim.canopy
        for (i in 1:nrow(sim.canopy)) {
          # Perform linear interpolation to resample to Sentinel-2 bands
          reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
          # Store the interpolated reflectance in the list
          SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
          SE2_[[i]] <-reflectance.i
        }

        # Convert the list to a matrix
        df.SE2 <- do.call(rbind, SE2_df)
        df.SE2_rfl <- do.call(rbind, SE2_)
        colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')

      }



    }) ###end plots





  })



  output$plot <- renderPlot({

    # Show the modal window

    # Return the data frame
    data_lut_leaf <- get_data(RTmodel = input$model_selected_leaf, input$n_samples, input$seed)
    data_lut_canopy <- get_data(RTmodel = input$model_selected_canopy, input$n_samples, input$seed)
    data_lut <- cbind(data_lut_leaf,data_lut_canopy)

    SE2_bands <- c('B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B10', 'B11','B12')
    # Call the Simulations
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]  # rsoil2 = wet soil

    # Show the modal window
    #show_modal_spinner()
    #remove_modal_spinner()


    if ((input$model_selected_leaf == 'PROSPECT-PRO') & (input$model_selected_canopy == 'fourSAILH')) {

      # choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()

      sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
        loadFunctions()
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")

        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-PRO')
        rdot<-data.foursail[[1]]
        rsot<-data.foursail[[2]]
        rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
        sim.rfl[[i]]<-rfl.prosail

      } ##end paralle

      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)


    } else if ((input$model_selected_leaf == 'PROSPECT-D') & (input$model_selected_canopy == 'fourSAILH')) {

      ## choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()
      sims<-foreach::foreach(i=1:input$n_samples, .export = c("loadFunctions",'loadRDa')) %dopar% {
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")
        loadFunctions()
        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-D')
        rdot<-data.foursail[[1]]
        rsot<-data.foursail[[2]]
        rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
        sim.rfl[[i]]<-rfl.prosail

      }

      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)

    } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'fourSAILH')) {

      ## choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()
      sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")
        loadFunctions()
        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
        rdot<-data.foursail[[1]]
        rsot<-data.foursail[[2]]
        rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB)
        sim.rfl[[i]]<-rfl.prosail

      } ##end paralle

      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)


    } else if ((input$model_selected_leaf == 'PROSPECT-PRO') & (input$model_selected_canopy == 'INFORM')) {

      data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

      ## choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()
      sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")
        loadFunctions()

        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-PRO')
        sim.rfl[[i]]<-rfl.inform

      } ##end paralle

      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)


    } else if ((input$model_selected_leaf == 'PROSPECT-D') & (input$model_selected_canopy == 'INFORM')) {

      data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

      ## choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()
      sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")
        loadFunctions()
        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'PROSPECT-D')
        sim.rfl[[i]]<-rfl.inform

      } ##end paralle
      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)

    } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'INFORM')) {


      data_lut<- getLUT_liberty(inputs=inputsINFORM, nLUT = input$n_samples, setseed = input$seed)

      ## choose number of processors/cores
      no_cores <- parallel::detectCores() - 2
      cl <- parallel::makeCluster(no_cores)
      doParallel::registerDoParallel(cl)
      sim.rfl<-list()
      sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
        loadRDa("www/data/ToolsRTM")
        loadRDa("www/data")
        loadFunctions()
        psoil = data_lut[i,'psoil']
        rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

        rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
        sim.rfl[[i]]<-rfl.inform

      } ##end paralle

      # Close the cluster after parallel processing is complete
      parallel::stopCluster(cl)

    }

    sim.canopy<-do.call(rbind,sims)
    wave<-seq(400,2500,1)

    #soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))

    IDs<-c(1:input$n_samples)
    # Convert matrix to data frame
    df <- data.frame(sim.canopy)
    df$row <- 1:nrow(df)  # Add a row identifier
    ###
    if (input$sensor_selected == 'RTM') {

      print(get.plots(df=df,wave=wave))
      df.to_export <- cbind(IDs,data_lut,df)

    } else if (input$sensor_selected == 'Sentinel2a') {

      fwhm <-c(Sentinel2A.MSI$band_width)
      center_wvl <-c(Sentinel2A.MSI$center_wvl)
      sim.canopy<-as.matrix(do.call(rbind,sims))

      # Initialize an empty list to store interpolated reflectance data
      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)
      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-SE2_bands

      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "forestgreen", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "forestgreen", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "forestgreen", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)


      df.to_export <- cbind(IDs,data_lut,df.SE2_rfl)


    } else if (input$sensor_selected == 'Sentinel2b') {

      fwhm <-c(Sentinel2B.MSI$band_width)
      center_wvl <-c(Sentinel2B.MSI$center_wvl)
      sim.canopy<-as.matrix(do.call(rbind,sims))

      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)

      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-SE2_bands
      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "red", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "red", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "red", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)

      df.to_export <- cbind(IDs,data_lut,df.SE2_rfl)


    } else if (input$sensor_selected == 'Landsat4') {

      fwhm <-c(LANDSAT4.TM$band_width)
      center_wvl <-sort(c(LANDSAT4.TM$center_wvl))[1:6]
      sim.canopy<-as.matrix(do.call(rbind,sims))

      # Initialize an empty list to store interpolated reflectance data
      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)
      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')

      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "darkgoldenrod2", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "darkgoldenrod2", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "darkgoldenrod2", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)
      df.to_export <- cbind(IDs,data_lut,df.SE2)

    } else if (input$sensor_selected == 'Landsat5') {

      fwhm <-c(LANDSAT5.TM$band_width)
      center_wvl <-sort(c(LANDSAT5.TM$center_wvl))[1:6]
      sim.canopy<-as.matrix(do.call(rbind,sims))

      # Initialize an empty list to store interpolated reflectance data
      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)
      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')

      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "dodgerblue3", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "dodgerblue3", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "dodgerblue3", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)
      df.to_export <- cbind(IDs,data_lut,df.SE2)

    } else if (input$sensor_selected == 'Landsat7') {

      fwhm <-c(LANDSAT7.ETM$band_width)
      center_wvl <-sort(c(LANDSAT7.ETM$center_wvl))[1:7]
      sim.canopy<-as.matrix(do.call(rbind,sims))

      # Initialize an empty list to store interpolated reflectance data
      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)
      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')

      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "darkseagreen3", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "darkseagreen3", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "darkseagreen3", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)
      df.to_export <- cbind(IDs,data_lut)

    }   else if (input$sensor_selected == 'Landsat8') {

      fwhm <-c(LANDSAT8.OLI$band_width)
      center_wvl <-sort(c(LANDSAT8.OLI$center_wvl))
      sim.canopy<-as.matrix(do.call(rbind,sims))

      # Initialize an empty list to store interpolated reflectance data
      SE2_ <-list()
      SE2_df <- list()
      # Perform linear interpolation for each row in sim.canopy
      for (i in 1:nrow(sim.canopy)) {
        # Perform linear interpolation to resample to Sentinel-2 bands
        reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
        # Store the interpolated reflectance in the list
        SE2_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
        SE2_[[i]] <-reflectance.i
      }

      # Convert the list to a matrix
      df.SE2 <- do.call(rbind, SE2_df)
      df.SE2_rfl <- do.call(rbind, SE2_)
      colnames(df.SE2_rfl) <-paste('B.',center_wvl,sep='')

      # Calculate average, 25th percentile, and 50th percentile for each band
      stats.SE2 <- df.SE2 %>%
        group_by(center_wvl) %>%
        summarise(
          average = mean(reflectance,na.rm=T),
          median = median(reflectance,na.rm=T),
          percentile_25 = quantile(reflectance, 0.25,na.rm=T),
          percentile_50 = quantile(reflectance, 0.50,na.rm=T),
          percentile_75 = quantile(reflectance, 0.75,na.rm=T))
      # Plot using ggplot2
      plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + ylim(0,0.8) +
        xlim(400,2500) +
        geom_line(aes(y = average), color = "indianred2", size = 0.8) +
        geom_line(aes(y = median),  linetype = "dashed", color = "indianred2", size = 0.8) +
        geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = "indianred2", alpha = 0.3) +
        labs(
          title = "",
          x = "wavelength (nm)",
          y = "Reflectance"
        ) +
        theme_bw() +  theme(
          text = element_text(size = 14, face='bold'),  # Increase the text size
          axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
          axis.text = element_text(face = "bold"),  # Make axis numbers bold
          plot.title = element_text(face = "bold"),  # Make plot title bold
          plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
        )
      print(plot_)
      df.to_export <- cbind(IDs,data_lut,df.SE2)

    }




    # Create a reactive data frame for saving data
    save_data <- reactive({
      df.to_export
    })
    # Add the download handler for saving data
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("LUT_", input$model_selected_leaf,'_', input$model_selected_canopy,'_', input$sensor_selected, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(save_data(), file, row.names = FALSE)
      }
    )

  }) ###end plots




  #### Render the LUT leaf table ----------------------------------
  output$lut_table.leaf <- renderTable({
    lut_data.leaf()
  })


  #### Render the LUT canopy table ----------------------------------
  output$lut_table.canopy <- renderTable({
    lut_data.canopy()
  }, tableHeader = "Canopy Parameters")

  #### Render the LUT sims able ----------------------------------
  output$lut_table.sim <- renderTable({
    #lut_data.sim()[[1]]
  }, tableHeader = "Canopy Parameters")


})


# Run the application
source("ui.R")
shinyApp(ui = ui, server = server)
