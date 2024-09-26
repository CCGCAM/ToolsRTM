
#  Define SERVER for the app -----------------------------------------

# Define a reactive values object to store the selected model
selected_model <- reactiveValues(model = NULL)

server <-shinyServer(function(input, output, session) {

  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = '2')  # Set log level to reduce TensorFlow output
  Sys.setenv("CUDA_VISIBLE_DEVICES" = "-1")  # Disable GPU

  # Load required libraries
  library(tensorflow)
  library(keras)

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
    showNotification("SCOPE executed successfully.", type = "message")
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
                                       uh2o=input$uh2o_spart, alt_m=0, Pa0=0.00100133,
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
    lut_to_sim <-cbind(lut_leaf,lut_canopy,lut_atmos)

    # Call the Simulations
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]  # rsoil2 = wet soil
    psoil = lut_to_sim[1,'psoil']
    #psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)
    rsoil<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
    #print(lut_to_sim)
    #reflectance_values <- -foursail(inputLUT=lut.to_sim[1,],rsoil=rsoil,LeafModel = 'PRO')
    return(list(lut_to_sim,rsoil,lut_leaf,lut_canopy,lut_atmos))


  })

  # Reactive expression for the reflectance data ---------------------------------------------------
  reflectance_data_spart <- reactive({

    LUT_ <-as.data.frame(lut_data.sim_spart()[[1]])

    rsoil_ <- lut_data.sim_spart()[[2]]
    LUT_$TypeLidf = 1
    print(LUT_)
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
    print(to_plot)
    ggplot(data = to_plot, aes(x = wavelength)) +
      labs(y = "Reflectance", x = "") +
      geom_point(aes(y = rfl.toa, color = "TOA rfl."), size = 1) +
      geom_line(aes(y = rfl.toa, color = "TOA rfl."),size = 1.0) +

      geom_point(aes(y = rfl.toc, color = "TOC rfl. (SMAC)"), size = 1.0) +
      geom_line(aes(y = rfl.toc, color = "TOC rfl. (SMAC)"), size = 1.0) +

      theme_bw() +
      guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""), shape = guide_legend(title = "")) +

      theme(legend.position = "top",
            legend.key.size = unit(4, "lines"),
            text = element_text(size = 16, face='bold'),  # Increase the text size
            legend.text = element_text(face = "bold", size = 14), # Increase legend text size and make it bold
            axis.title.x = element_text(face = "bold", size = 14), # Increase x axis label size and make it bold
            axis.title.y = element_text(face = "bold", size = 14)) # Increase y axis label size and make it bold


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

  # Create the function to generate the output
  output$lut_spart_output_leaf <- renderTable({
    # Your actual data or function to generate the table
    data.to_ <- lut_data.sim_spart()[[3]]
    # Return the data to display in the table
    data.to_
  })


  # Create the function to generate the output
  output$lut_spart_output_canopy <- renderTable({
    # Your actual data or function to generate the table
    data.to_ <- lut_data.sim_spart()[[4]]
    # Return the data to display in the table
    data.to_
  })
  # Create the function to generate the output
  output$lut_spart_output_atmo <- renderTable({
    # Your actual data or function to generate the table
    data.to_ <- lut_data.sim_spart()[[5]]
    # Return the data to display in the table
    data.to_
  })


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
                       fqe = input$fqe_fp)



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
    print(LUT_)
    rsoil_ <- lut_data.sim()[[2]]
    if (input$canopy_model == 'fourSAILH'){
      LUT_$psi <- input$psi
      LUT_$psoil <- input$psoil
    } else {
      LUT_$psi <- input$psi_
      LUT_$psoil <- input$psoil
    }
    data <- dataSpec_PDB
    Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
    Rsoil.wet <- data[,12]
    psoil = LUT_[1,'psoil']
    #psoil	 <-  1    # soil factor (psoil=0: wet soil / psoil=1: dry soil)
    rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

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
      reflectance_values<- Compute_BRF(rdot=rdot,rsot=rsot,tts=LUT_[1,'tts'], short.waves = T)

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

  ## 4.1) Personal LUT Section  ---------------------------------------


  params_lut <- reactive({
    leaf_ranges <- switch(input$leaf_lut,
                          "PROSPECT-PRO" = data.frame(
                            Inputs = c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "Prot", "CBC", "alpha"),
                            Min = c(min(input$N2), min(input$Cab2), min(input$Car2), min(input$Anth2), min(input$Cbrown2),
                                    min(input$EWT2), 0, min(input$Prot2), min(input$CBC2),40),
                            Max = c(max(input$N2), max(input$Cab2), max(input$Car2), max(input$Anth2), max(input$Cbrown2),
                                    max(input$EWT2), 0, max(input$Prot2), max(input$CBC2),40)
                          ),

                          "PROSPECT-D" = data.frame(
                            Inputs = c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "alpha"),
                            Min = c(min(input$N_d2), min(input$Cab_d2), min(input$Car_d2), min(input$Anth_d2), min(input$Cbrown_d2),
                                    min(input$EWT_d2), min(input$LMA_d2), 40),  # Assuming alpha is constant at 40
                            Max = c(max(input$N_d2), max(input$Cab_d2), max(input$Car_d2), max(input$Anth_d2), max(input$Cbrown_d2),
                                    max(input$EWT_d2), max(input$LMA_d2), 40)  # Assuming alpha is constant at 40
                          ),

                          "Liberty" = data.frame(
                            Inputs = c("cell.d", "inter.c", "baseline.abs", "leaf.thick", "albino.abs", "Cab", "EWT", "lign.cell", "Nitrogen"),
                            Min = c(min(input$cell_d2), min(input$inter_c2), 0.0004, min(input$leaf_thick2),
                                    min(input$albino_abs2), min(input$Cab_l2), min(input$EWT_l2), min(input$lign_cell2), min(input$Nitrogen2)),
                            Max = c(max(input$cell_d2), max(input$inter_c2), 0.0004, max(input$leaf_thick2),
                                    max(input$albino_abs2), max(input$Cab_l2), max(input$EWT_l2), max(input$lign_cell2), max(input$Nitrogen2))
                          ),
                          "FLUSPECT-B-Cx" = data.frame(

                            Inputs = c('N',"fqe", "Cab",  "Car", 'Anth',"Cs",  "Cx", "EWT", "LMA","Prot","CBC","alpha"),
                            Min = c(min(input$N_fp_2),min(input$fqe_fp_2), min(input$Cab_fp_2), min(input$Car_fp_2),min(input$Anth_fp_2),
                                    min(input$Cs_fp_2), min(input$Cx_fp_2), min(input$EWT_fp_2),min(input$LMA_fp_2),
                                    min(input$Prot_fp_2),min(input$CBC_fp_2),40),
                            Max = c(max(input$N_fp_2),max(input$fqe_fp_2), max(input$Cab_fp_2), max(input$Car_fp_2),max(input$Anth_fp_2),
                                    max(input$Cs_fp_2), max(input$Cx_fp_2), max(input$EWT_fp_2), max(input$LMA_fp_2),
                                    max(input$Prot_fp_2), max(input$CBC_fp_2),40)
                          ),
                          "FLUSPECT-B" = data.frame(
                            Inputs = c('N',"fqe", "Cab",  "Car", "Cs", "EWT", "LMA", "Cx","alpha"),
                            Min = c(min(input$N_fd_2),min(input$fqe_fd_2), min(input$Cab_fd_2), min(input$Car_fd_2),
                                    min(input$Cs_fd_2), min(input$EWT_fd_2), min(input$LMA_fd_2),min(input$Cx_fd_2), 40),
                            Max = c(max(input$N_fd_2),max(input$fqe_fd_2), max(input$Cab_fd_2), max(input$Car_fd_2),
                                    max(input$Cs_fd_2), max(input$EWT_fd_2), max(input$LMA_fd_2), max(input$Cx_fd_2), 40)
                          )
    )


    canopy_ranges <- switch(input$canopy_lut,
                            "fourSAILH" = data.frame(
                              Inputs = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil"),
                              Min = c(min(input$LAI2), 2, min(input$LIDFa2), 0, min(input$hotspot2),
                                      min(input$tts2), min(input$tto2), min(input$psi2), min(input$psoil2)),
                              Max = c(max(input$LAI2), 2, max(input$LIDFa2), 0, max(input$hotspot2),
                                      max(input$tts2), max(input$tto2), max(input$psi2), max(input$psoil2))
                            ),

                            "INFORM" = data.frame(
                              Inputs = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                                         "LAIu", "cd", "sd", "h", "skyl"),
                              Min = c(min(input$LAI_2), 2, min(input$LIDFa_2), 0, min(input$hotspot_2),
                                      min(input$tts_2), min(input$tto_2), min(input$psi_2), min(input$psoil_2),
                                      min(input$LAIu_2), min(input$cd_2), min(input$sd_2), min(input$h_2), 0.1),
                              Max = c(max(input$LAI_2), 2, max(input$LIDFa_2), 0, max(input$hotspot_2),
                                      max(input$tts_2), max(input$tto_2), max(input$psi_2), max(input$psoil_2),
                                      max(input$LAIu_2), max(input$cd_2), max(input$sd_2), max(input$h_2), 0.1)
                            )
    )

    return(list(leaf_ranges, canopy_ranges))
  })



  # Reactive expression for the LUT data ---------------------------------------------------
  lut_datasets<- reactive({
    # Get the selected leaf and canopy parameters from the reactive `params` expression
    leaf_params <- params_lut()[[1]]
    canopy_params <- params_lut()[[2]]
    lut_ranges<-rbind(leaf_params,canopy_params)
    return(lut_ranges)
  })


  # Render the canopy parameters
  output$params_output <- renderPrint({
    # Call the reactive expression
    lut_to_show <- lut_datasets()
    # Print the canopy parameters
    print(lut_to_show)
  })


  observe({
    # Print the reactive parameters to the console when they change
    params <- params_lut()

    # Print leaf parameters
   # print("Leaf Parameters:")
   # print(params[[1]])  # First element in the list: leaf_params

    # Print canopy parameters
    #print("Canopy Parameters:")
    #print(params[[2]])  # Second element in the list: canopy_params
    # Print canopy parameters
    #print("All Parameters:")
    #print(lut_datasets())  # Second element in the list: canopy_params

  })


  w <- reactiveValues(data = NULL)

  observeEvent(input$buttonLUT2, {


    output$plot_lut <- renderPlot({

      # Call the Simulations
      data <- dataSpec_PDB
      Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
      Rsoil.wet <- data[,12]  # rsoil2 = wet soil

      # Show the modal window
      show_modal_spinner()

      db.ranges <-lut_datasets()
      print(db.ranges)
      # Determine the distribution based on the checkbox status
      data_lut<- get.LUTfromRanges(LUT=db.ranges,nLUT = input$n_samples_lut, setseed = input$seed_lut,
                                   leaf.model = input$leaf_lut ,
                                   canopy.model = input$canopy_lut,
                                   distribution = 'uniform')


      # Create a list to store distribution types for each parameter
      distributions <- list(
        ##PROSPECT-PRO
        Cab = ifelse(input$Dist_cab2, 'gauss', 'uniform'),
        Car = ifelse(input$Dist_car2, 'gauss', 'uniform'),
        Anth = ifelse(input$Dist_anth2, 'gauss', 'uniform'),
        Cbrown = ifelse(input$Dist_cbrown2, 'gauss', 'uniform'),
        N = ifelse(input$Dist_n2, 'gauss', 'uniform'),
        EWT = ifelse(input$Dist_ewt2, 'gauss', 'uniform'),
        Prot = ifelse(input$Dist_prot2, 'gauss', 'uniform'),
        CBC = ifelse(input$Dist_cbc2, 'gauss', 'uniform'),
        ##PROSPECT-D
        Cab = ifelse(input$Dist_cab_d2, 'gauss', 'uniform'),
        Car = ifelse(input$Dist_car_d2, 'gauss', 'uniform'),
        Anth = ifelse(input$Dist_anth_d2, 'gauss', 'uniform'),
        Cbrown = ifelse(input$Dist_cbrown_d2, 'gauss', 'uniform'),
        N = ifelse(input$Dist_n_d2, 'gauss', 'uniform'),
        EWT = ifelse(input$Dist_ewt_d2, 'gauss', 'uniform'),
        LMA = ifelse(input$Dist_lma_d2, 'gauss', 'uniform'),


        ##FLUSPECT-B
        Cab = ifelse(input$Dist_cab_fd_2, 'gauss', 'uniform'),
        Car = ifelse(input$Dist_car_fd_2, 'gauss', 'uniform'),
        fqe = ifelse(input$Dist_fqe_fd_2, 'gauss', 'uniform'),
        Cs = ifelse(input$Dist_cs_fd_2, 'gauss', 'uniform'),
        Cx = ifelse(input$Dist_cx_fd_2, 'gauss', 'uniform'),
        N = ifelse(input$Dist_n_fd_2, 'gauss', 'uniform'),
        EWT = ifelse(input$Dist_ewt_fd_2, 'gauss', 'uniform'),
        LMA = ifelse(input$Dist_lma_fd_2, 'gauss', 'uniform'),

        ##FLUSPECT-B-Cx
        Cab = ifelse(input$Dist_cab_fp_2, 'gauss', 'uniform'),
        Car = ifelse(input$Dist_car_fp_2, 'gauss', 'uniform'),
        Anth = ifelse(input$Dist_ant_fp_2, 'gauss', 'uniform'),
        fqe = ifelse(input$Dist_fqe_fp_2, 'gauss', 'uniform'),
        Cs = ifelse(input$Dist_cs_fp_2, 'gauss', 'uniform'),
        Cx = ifelse(input$Dist_cx_fp_2, 'gauss', 'uniform'),
        N = ifelse(input$Dist_n_fp_2, 'gauss', 'uniform'),
        EWT = ifelse(input$Dist_ewt_fp_2, 'gauss', 'uniform'),
        LMA = ifelse(input$Dist_lma_fp_2, 'gauss', 'uniform'),
        Prot = ifelse(input$Dist_prot_fp_2, 'gauss', 'uniform'),
        CBC = ifelse(input$Dist_cbc_fp_2, 'gauss', 'uniform'),

        ##Liberty
        cell.d = ifelse(input$Dist_cell_d_l2, 'gauss', 'uniform'),
        inter.c = ifelse(input$Dist_inter_c_l2, 'gauss', 'uniform'),
        baseline.abs = ifelse(input$Dist_baseline_abs_l2, 'gauss', 'uniform'),
        leaf.thick = ifelse(input$Dist_leaf_thick_l2, 'gauss', 'uniform'),
        albino.abs = ifelse(input$Dist_albino_abs_l2, 'gauss', 'uniform'),
        Cab = ifelse(input$Dist_cab_l2, 'gauss', 'uniform'),
        EWT = ifelse(input$Dist_ewt_l2, 'gauss', 'uniform'),
        lign.cell = ifelse(input$Dist_lign_cell_l2, 'gauss', 'uniform'),
        Nitrogen = ifelse(input$Dist_Nitrogen_l2, 'gauss', 'uniform'),

        ##fourSAILH
        LAI = ifelse(input$Dist_lai2, 'gauss', 'uniform'),
        LIDFa = ifelse(input$Dist_lidfa2, 'gauss', 'uniform'),
        hspot = ifelse(input$Dist_hspot2, 'gauss', 'uniform'),
        tts = ifelse(input$Dist_tts2, 'gauss', 'uniform'),
        tto = ifelse(input$Dist_tto2, 'gauss', 'uniform'),
        psi = ifelse(input$Dist_psi2, 'gauss', 'uniform'),
        psoil = ifelse(input$Dist_psoil2, 'gauss', 'uniform'),

        ##INFOMR
        LAI = ifelse(input$Dist_lai_2, 'gauss', 'uniform'),
        LIDFa = ifelse(input$Dist_lidfa_2, 'gauss', 'uniform'),
        hspot = ifelse(input$Dist_hotspot_2, 'gauss', 'uniform'),
        tts = ifelse(input$Dist_tts_2, 'gauss', 'uniform'),
        tto = ifelse(input$Dist_tto_2, 'gauss', 'uniform'),
        psi = ifelse(input$Dist_psi_2, 'gauss', 'uniform'),
        psoil = ifelse(input$Dist_psoil_2, 'gauss', 'uniform'),
        LAIu = ifelse(input$Dist_laiu_2, 'gauss', 'uniform'),
        cd = ifelse(input$Dist_cd_2, 'gauss', 'uniform'),
        sd = ifelse(input$Dist_sd_2, 'gauss', 'uniform'),
        h = ifelse(input$Dist_h_2, 'gauss', 'uniform') )

      # Define expected inputs for each model
      expected_inputs <- list(
        "PROSPECT-D" = c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "alpha"),
        "PROSPECT-PRO" =  c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "Prot", "CBC", "alpha"),
        'FLUSPECT-B' = c('N',"fqe", "Cab",  "Car", "Cs", "EWT", "LMA", "Cx","alpha"),
        'FLUSPECT-B-Cx' = c('N',"fqe", "Cab",  "Car", 'Anth',"Cs",  "Cx", "EWT", "LMA","Prot","CBC","alpha"),
        "Liberty" =   c("cell.d", "inter.c", "baseline.abs", "leaf.thick", "albino.abs", "Cab", "EWT", "lign.cell", "Nitrogen"),
        "fourSAILH" = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil"),
        "INFORM" = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                     "LAIu", "cd", "sd", "h", "skyl") )

      # Check if the selected leaf or canopy model requires special handling
      if (input$leaf_lut %in% names(expected_inputs) || input$canopy_lut %in% names(expected_inputs)) {
        # Get expected parameters for the selected model
        model_inputs <- expected_inputs[[input$leaf_lut]] %||% expected_inputs[[input$canopy_lut]]

        # Apply distributions only for the specified parameters present in the data
        for (param in model_inputs) {
          if (param %in% names(distributions) && distributions[[param]] == 'gauss') {
            # Calculate standard deviation for Gaussian distribution
            sd_ <- (max(data_lut[[param]]) - min(data_lut[[param]])) / 6  # Assuming 99.7% coverage for min/max in normal distribution
            data_lut[[param]] <- stats::rnorm(input$n_samples_lut, mean = mean(data_lut[[param]], na.rm = TRUE), sd = sd_)
          }
        }
      }
      # We consider only LIDFa
      data_lut$TypeLidf <- 2
      #Adding alpha to 40 (default in PROSPECT)
      data_lut$alpha <- 40
      # Alternatively, if you want to return it as a data frame in a reactive value
      w$data <- data_lut  # Store LUT in reactive values for later use


      if ((input$leaf_lut == 'PROSPECT-PRO') & (input$canopy_lut == 'fourSAILH')) {

        # choose number of processors/cores
        no_cores <- 2 #parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()

        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
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


      } else if ((input$leaf_lut == 'PROSPECT-D') & (input$canopy_lut == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut, .export = c("loadFunctions",'loadRDa')) %dopar% {
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

      } else if ((input$leaf_lut == 'FLUSPECT-B') & (input$canopy_lut == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut, .export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB,short.waves = T)
          sim.rfl[[i]]<-rfl.prosail

        }

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$leaf_lut == 'FLUSPECT-B-Cx') & (input$canopy_lut == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut, .export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB, short.waves = T)
          sim.rfl[[i]]<-rfl.prosail

        }

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$leaf_lut == 'Liberty') & (input$canopy_lut == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2 #parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
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


      } else if ((input$leaf_lut == 'PROSPECT-PRO') & (input$canopy_lut == 'INFORM')) {

        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
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


      } else if ((input$leaf_lut == 'PROSPECT-D') & (input$canopy_lut == 'INFORM')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
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

      } else if ((input$leaf_lut == 'FLUSPECT-B') & (input$canopy_lut == 'INFORM')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle
        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$leaf_lut == 'FLUSPECT-B-Cx') & (input$canopy_lut == 'INFORM')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          rfl.inform<-inform(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle
        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$leaf_lut == 'Liberty') & (input$canopy_lut == 'INFORM')) {


        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples_lut,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
          rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      }


      sim.canopy<-do.call(rbind,sims)
      if ((input$leaf_lut == 'FLUSPECT-B') | (input$leaf_lut == 'FLUSPECT-B-Cx') ){
        wave<-seq(400,2500,1)[1:2001]
      } else{
        wave<-seq(400,2500,1)
      }


      IDs<-c(1:input$n_samples_lut)
      # Convert matrix to data frame
      df <- data.frame(sim.canopy)
      print(dim(df))
      df$row <- 1:nrow(df)  # Add a row identifier
      #df <-df[!complete.cases(df), ]
      df <-na.omit(df)
      # Extract the row indices that were kept in df_clean
      rows_kept <- df$row


      print('removing cases')
      print(dim(df))
      print(any(is.na(df)))
      #print(colSums(is.na(df)))

      if (any(is.na(df)) == T){

        print(dim( df[!complete.cases(df), ]))
      }

      ###
      if (input$sensor_lut == 'RTM') {

        df <-na.omit(df)
        # Now, select the same rows from data_lut based on the remaining rows in df
        data_lut_filtered <- data_lut[rownames(df), ]
        IDs<-c(1:input$n_samples_lut)[rownames(df)]
        print(get.plots(df=df,wave=wave))
        df.to_export <- cbind(IDs,data_lut_filtered,df)

      } else {

        df.sensor <- subset(sensor.characteristics, Sensor == input$sensor_lut)
        color.selected <- c('forestgreen','red','darkgoldenrod2','dodgerblue3','darkseagreen3','indianred2')
        print(df.sensor)
        if (input$sensor_lut == 'Sentinel2a'){
          color.i <-color.selected[1]
        } else if (input$sensor_lut == 'Sentinel2b'){
          color.i <-color.selected[2]
        } else if (input$sensor_lut == 'Landsat4'){
          color.i <-color.selected[3]
        } else if (input$sensor_lut == 'Landsat5'){
          color.i <-color.selected[4]
        } else if (input$sensor_lut == 'Landsat7'){
          color.i <-color.selected[5]
        } else if (input$sensor_lut == 'Landsat8'){
          color.i <-color.selected[6]
        }


        fwhm <-(df.sensor$lb -df.sensor$ub)
        center_wvl <-c(df.sensor$average)
        Band_ <- paste('B',df.sensor$channel,sep='')

        # Use these indices to remove the same rows from sim.canopy
        sim.canopy <- sim.canopy[rows_kept, ]

        # Initialize an empty list to store interpolated reflectance data
        Sz_ <-list()
        Sz_df <- list()
        # Perform linear interpolation for each row in sim.canopy
        for (i in 1:nrow(sim.canopy)) {
          # Perform linear interpolation to resample to Sentinel-2 bands
          reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
          # Store the interpolated reflectance in the list
          Sz_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
          Sz_[[i]] <-reflectance.i
        }

        # Convert the list to a matrix
        df.Sz <- do.call(rbind, Sz_df)
        df.Sa_rfl <- as.data.frame(do.call(rbind, Sz_))
        colnames(df.Sa_rfl) <-Band_
        IDs <-rownames(df.Sa_rfl)  # Change this to ensure the IDs match df.Sa_rfl's dimensions

        df.to_export <- cbind(IDs,data_lut[rows_kept,],df.Sa_rfl)

        # Calculate average, 25th percentile, and 50th percentile for each band
        stats.SE2 <- df.Sz %>%
          group_by(center_wvl) %>%
          summarise(
            average = mean(reflectance,na.rm=T),
            median = median(reflectance,na.rm=T),
            percentile_25 = quantile(reflectance, 0.25,na.rm=T),
            percentile_50 = quantile(reflectance, 0.50,na.rm=T),
            percentile_75 = quantile(reflectance, 0.75,na.rm=T))
        # Plot using ggplot2
        plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + #ylim(0,0.8) +
          xlim(400,2500) +
          geom_line(aes(y = average), color = color.i, size = 0.8) +
          geom_line(aes(y = median),  linetype = "dashed", color = color.i, size = 0.8) +
          geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = color.i, alpha = 0.3) +
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

      }
      remove_modal_spinner()

      #Create a reactive data frame for saving data
      save_data <- reactive({
        df.to_export
      })
      # Add the download handler for saving data
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste("LUT_", input$leaf_lut,'_', input$canopy_lut,'_', input$sensor_lut, ".csv", sep = "")
        },
        content = function(file) {
          write.csv(save_data(), file, row.names = FALSE)
        }
      )


    }) #end outplots

  })    #end observer pannel


  # Print LUT data outside the observeEvent
  observe({
    req(w$data)  # Ensure w$data is not NULL
    print(head(w$data))  # Print the LUT data frame to the console
  })

  output$trait_distribution_plot <- renderPlot({
    req(w$data)
    # Filter out traits where all values are the same
    filtered_data <- w$data %>%
      select(where(~ sd(.) != 0))  # Keep only columns with non-zero standard deviation

    # Convert data to long format
    data_long <- filtered_data %>%
      pivot_longer(cols = everything(), names_to = "Trait", values_to = "Value")

    # Create the ggplot
    ggplot(data_long, aes(x = Value)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      facet_wrap(~Trait, scales = "free") +
      theme_bw() +
      labs(title = "",x = "", y = "Frequency") + theme(
        text = element_text(size = 14, face='bold'),  # Increase the text size
        axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
        axis.text = element_text(face = "bold"),  # Make axis numbers bold
        plot.title = element_text(face = "bold"),  # Make plot title bold
        plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
      )
  })

  # Alternatively,  show the first few rows of the data
  output$lut_head <- renderTable({
    req(w$data)  # Ensure w$data is not NULL
    head(w$data)  # Show the first few rows of the data frame
  })


  ## 4.2) Automatic LUT Section  ---------------------------------------
  v <- reactiveValues(data = NULL)

  observeEvent(input$buttonLUT, {


    output$plot <- renderPlot({

      # Call the Simulations
      data <- dataSpec_PDB
      Rsoil.dry  <- data[,11]  # rsoil1 = dry soil
      Rsoil.wet <- data[,12]  # rsoil2 = wet soil

      # Show the modal window
      show_modal_spinner()
      if (input$model_selected_leaf == 'PROSPECT-PRO' & input$model_selected_canopy== 'fourSAILH' ){

        inputs.leaf =  c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "Prot", "CBC", "alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$LMA = 0.0
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2


      } else if (input$model_selected_leaf == 'PROSPECT-D' & input$model_selected_canopy== 'fourSAILH' ){

        inputs.leaf =  c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))

        data_lut$Prot = 0.0
        data_lut$CBC = 0.0
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'FLUSPECT-B' & input$model_selected_canopy== 'fourSAILH' ){

        inputs.leaf = c('N',"fqe", "Cab",  "Car", "Cs", "EWT", "LMA", "Cx","alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'FLUSPECT-B-Cx' & input$model_selected_canopy== 'fourSAILH' ){

        inputs.leaf = c('N',"fqe", "Cab",  "Car", 'Anth',"Cs",  "Cx", "EWT", "LMA","Prot","CBC","alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'Liberty' & input$model_selected_canopy== 'fourSAILH' ){

        inputs.leaf = c("cell.d", "inter.c", "baseline.abs", "leaf.thick", "albino.abs", "Cab", "EWT", "lign.cell", "Nitrogen")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))


        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'Liberty' & input$model_selected_canopy== 'INFORM' ){

        inputs.leaf = c("cell.d", "inter.c", "baseline.abs", "leaf.thick", "albino.abs", "Cab", "EWT", "lign.cell", "Nitrogen",'alpha')
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                          "LAIu", "cd", "sd", "h", "skyl")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))

        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'PROSPECT-PRO' & input$model_selected_canopy== 'INFORM' ){

        inputs.leaf =  c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "Prot", "CBC", "alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                          "LAIu", "cd", "sd", "h", "skyl")

        inputs_to_sim <- inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$LMA = 0.0
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'PROSPECT-D' & input$model_selected_canopy== 'INFORM' ){

        inputs.leaf =  c("N", "Cab", "Car", "Anth", "Cbrown", "EWT", "LMA", "alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                          "LAIu", "cd", "sd", "h", "skyl")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$Prot = 0.0
        data_lut$CBC = 0.0
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'FLUSPECT-B' & input$model_selected_canopy== 'INFORM' ){

        inputs.leaf = c('N',"fqe", "Cab",  "Car", "Cs", "EWT", "LMA", "Cx","alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                          "LAIu", "cd", "sd", "h", "skyl")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2

      } else if (input$model_selected_leaf == 'FLUSPECT-B-Cx' & input$model_selected_canopy== 'INFORM' ){

        inputs.leaf = c('N',"fqe", "Cab",  "Car", 'Anth',"Cs",  "Cx", "EWT", "LMA","Prot","CBC","alpha")
        inputs.canopy = c("LAI", "TypeLidf", "LIDFa", "LIDFb", "hspot", "tts", "tto", "psi", "psoil",
                          "LAIu", "cd", "sd", "h", "skyl")

        inputs_to_sim <-inputsRTMs %>%
          filter(variable %in% c(inputs.leaf, inputs.canopy))

        data_lut<- as.data.frame(get.LUTfromRanges(LUT=inputs_to_sim[,1:3], nLUT = input$n_samples, setseed = input$seed,
                                                   leaf.model = input$model_selected_leaf ,
                                                   canopy.model = input$model_selected_canopy,
                                                   distribution = 'uniform'))
        data_lut$alpha <- 40
        data_lut$TypeLidf = 2
      }




      # Alternatively, if you want to return it as a data frame in a reactive value
      v$data_default <- data_lut  # Store LUT in reactive values for later use

      #Calculate the min, mean, and standard deviation for each column in data_lut
      summary_stats <- data.frame(
        Trait = rep(colnames(data_lut), each = 1),  # Repeat trait names
        Statistic = c(rep("Min", ncol(data_lut)), rep("Mean", ncol(data_lut)),  rep("Max", ncol(data_lut)), rep("Stdv", ncol(data_lut))),
        Value = c(
          apply(data_lut, 2, min, na.rm = TRUE),
          apply(data_lut, 2, mean, na.rm = TRUE),
          apply(data_lut, 2, max, na.rm = TRUE),
          apply(data_lut, 2, sd, na.rm = TRUE)

        )
      )

      # Reshape the table
      summary_table <- reshape(summary_stats, idvar = "Trait", timevar = "Statistic", direction = "wide")
      # Convert all values to 3 decimal places
      summary_table[ , -1] <- round(summary_table[ , -1], 3)

      # Clean column names
      colnames(summary_table) <- gsub("Value.", "", colnames(summary_table))
      v$data_summary <- summary_table  # Store LUT in reactive values for later use
      print(summary_table)

      if ((input$model_selected_leaf == 'PROSPECT-PRO') & (input$model_selected_canopy == 'fourSAILH')) {

        # choose number of processors/cores
        no_cores <- 2 #parallel::detectCores() - 2
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
        no_cores <- 2# parallel::detectCores() - 2
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

      } else if ((input$model_selected_leaf == 'FLUSPECT-B') & (input$model_selected_canopy == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples, .export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB,short.waves = T)
          sim.rfl[[i]]<-rfl.prosail

        }

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'FLUSPECT-B-Cx') & (input$model_selected_canopy == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples, .export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)

          data.foursail<- foursail(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')
          rdot<-data.foursail[[1]]
          rsot<-data.foursail[[2]]
          rfl.prosail<-Compute_BRF(rdot=rdot,rsot=rsot,tts=data_lut[i,'tts'],data.light=dataSpec_PDB,short.waves = T)
          sim.rfl[[i]]<-rfl.prosail

        }

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'fourSAILH')) {

        ## choose number of processors/cores
        no_cores <- 2 #parallel::detectCores() - 2
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

        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
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

        ## choose number of processors/cores
        no_cores <- 2# parallel::detectCores() - 2
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

      } else if ((input$model_selected_leaf == 'FLUSPECT-B') & (input$model_selected_canopy == 'INFORM')) {


        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
          rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'FLUSPECT-B-Cx') & (input$model_selected_canopy == 'INFORM')) {


        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
          rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_[1:2001],LeafModel = 'Fluspect-B-Cx')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      } else if ((input$model_selected_leaf == 'Liberty') & (input$model_selected_canopy == 'INFORM')) {


        ## choose number of processors/cores
        no_cores <- 2#parallel::detectCores() - 2
        cl <- parallel::makeCluster(no_cores)
        doParallel::registerDoParallel(cl)
        sim.rfl<-list()
        sims<-foreach::foreach(i=1:input$n_samples,.export = c("loadFunctions",'loadRDa')) %dopar% {
          loadRDa("www/data/ToolsRTM")
          loadRDa("www/data")
          loadFunctions()
          psoil = data_lut[i,'psoil']
          rsoil_<- c(psoil*Rsoil.dry+(1-psoil)*Rsoil.wet)
          rfl.inform<- inform(inputLUT=data_lut[i,],rsoil=rsoil_,LeafModel = 'Liberty')
          sim.rfl[[i]]<-rfl.inform

        } ##end paralle

        # Close the cluster after parallel processing is complete
        parallel::stopCluster(cl)

      }
      sim.canopy<-do.call(rbind,sims)
      if ((input$model_selected_leaf == 'FLUSPECT-B') | (input$model_selected_leaf == 'FLUSPECT-B-Cx') ){
        wave<-seq(400,2500,1)[1:2001]
      } else{
        wave<-seq(400,2500,1)
      }


      #soil.matrix<-rbind(soil.matrix,t(soil.scope_2nm), t(soil.scope_3nm))

      IDs<-c(1:input$n_samples)
      # Convert matrix to data frame
      df <- data.frame(sim.canopy)
      df$row <- 1:nrow(df)  # Add a row identifier
      df <-na.omit(df)
      dim(df)
      ###
      if (input$sensor_selected == 'RTM') {
        df <-na.omit(df)
        # Now, select the same rows from data_lut based on the remaining rows in df
        data_lut_filtered <- data_lut[rownames(df), ]
        IDs<-c(1:input$n_samples)[rownames(df)]
        print(get.plots(df=df,wave=wave))
        df.to_export <- cbind(IDs,data_lut_filtered,df)

      } else {

        df.sensor <- subset(sensor.characteristics, Sensor == input$sensor_selected)
        color.selected <- c('forestgreen','red','darkgoldenrod2','dodgerblue3','darkseagreen3','indianred2')
        print(df.sensor)
        if (input$sensor_selected == 'Sentinel2a'){
          color.i <-color.selected[1]
        } else if (input$sensor_selected == 'Sentinel2b'){
          color.i <-color.selected[2]
        } else if (input$sensor_selected == 'Landsat4'){
          color.i <-color.selected[3]
        } else if (input$sensor_selected == 'Landsat5'){
          color.i <-color.selected[4]
        } else if (input$sensor_selected == 'Landsat7'){
          color.i <-color.selected[5]
        } else if (input$sensor_selected == 'Landsat8'){
          color.i <-color.selected[6]
        }



        fwhm <-(df.sensor$lb -df.sensor$ub)
        center_wvl <-c(df.sensor$average)
        Band_ <- paste('B',df.sensor$channel,sep='')
        sim.canopy<-as.matrix(do.call(rbind,sims))


        # Initialize an empty list to store interpolated reflectance data
        Sz_ <-list()
        Sz_df <- list()
        # Perform linear interpolation for each row in sim.canopy
        for (i in 1:nrow(sim.canopy)) {
          # Perform linear interpolation to resample to Sentinel-2 bands
          reflectance.i <- signal::interp1(wave, sim.canopy[i, ], center_wvl, method = "spline")
          # Store the interpolated reflectance in the list
          Sz_df[[i]] <- data.frame(center_wvl = center_wvl, reflectance = reflectance.i)
          Sz_[[i]] <-reflectance.i
        }
        # Convert the list to a matrix
        df.Sz <- do.call(rbind, Sz_df)
        df.Sa_rfl <- do.call(rbind, Sz_)

        colnames(df.Sa_rfl) <-Band_

        # Calculate average, 25th percentile, and 50th percentile for each band
        stats.SE2 <- df.Sz %>%
          group_by(center_wvl) %>%
          summarise(
            average = mean(reflectance,na.rm=T),
            median = median(reflectance,na.rm=T),
            percentile_25 = quantile(reflectance, 0.25,na.rm=T),
            percentile_50 = quantile(reflectance, 0.50,na.rm=T),
            percentile_75 = quantile(reflectance, 0.75,na.rm=T))
        # Plot using ggplot2
        plot_ <-ggplot(stats.SE2, aes(x = center_wvl)) + #ylim(0,0.8) +
          xlim(400,2500) +
          geom_line(aes(y = average), color = color.i, size = 0.8) +
          geom_line(aes(y = median),  linetype = "dashed", color = color.i, size = 0.8) +
          geom_ribbon(aes(ymin = percentile_25, ymax = percentile_75), linetype = "dashed",fill = color.i, alpha = 0.3) +
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
        df.to_export <- cbind(IDs,data_lut,df.Sa_rfl)
      }
      remove_modal_spinner()
      #Create a reactive data frame for saving data
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
    })   #edn output


  })


  # Print LUT data outside the observeEvent
  observe({
    req(v$data_default)  # Ensure w$data is not NULL
    #print(head(v$data_default))  # Print the LUT data frame to the console
  })

  output$trait_distribution_plot_default <- renderPlot({
    req(v$data_default)
    # Filter out traits where all values are the same
    filtered_data <- v$data_default %>%
      select(where(~ sd(.) != 0))  # Keep only columns with non-zero standard deviation

    # Convert data to long format
    data_long <- filtered_data %>%
      pivot_longer(cols = everything(), names_to = "Trait", values_to = "Value")

    # Create the ggplot
    ggplot(data_long, aes(x = Value)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "black") +
      facet_wrap(~Trait, scales = "free") +
      theme_bw() +
      labs(title = "",x = "", y = "Frequency") + theme(
        text = element_text(size = 14, face='bold'),  # Increase the text size
        axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
        axis.text = element_text(face = "bold"),  # Make axis numbers bold
        plot.title = element_text(face = "bold"),  # Make plot title bold
        plot.subtitle = element_text(size = 14, face='bold')  # Adjust subtitle size
      )
  })

  # Alternatively,  show the first few rows of the data
  output$lut_head_default <- renderTable({
    req(v$data_summary)  # Ensure w$data is not NULL
   # print(v$data_summary)  # Show the first few rows of the data frame
  })


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



  # 5) MACHINE LEARNIG module   ---------------------------------------


  # Reactive value to keep track of whether the dataset has been uploaded
  dataset_uploaded <- reactiveVal(FALSE)

  # Create a reactive value to store the uploaded dataset
  user_lut_data <- reactiveVal()
  # Create a reactive value to store the uploaded dataset
  user_lut_data_indices <- reactiveVal()

  ## Get datasets   ---------------------------------------


  loadDataset <- function(dataset_type) {
    if (dataset_type == "upload" && !is.null(input$uploaded_file)) {
      # Load uploaded dataset
      user_lut <- tryCatch({
        read.csv(input$uploaded_file$datapath)
      }, error = function(e) {
        showNotification("Error loading uploaded file: Please check the file format.", type = "error")
        return(NULL)
      })
      if (!is.null(user_lut)) {
        showNotification("File uploaded successfully.", type = "message")
      }
      return(user_lut)

    } else if (dataset_type == "PROSAIL") {
      # Load predefined PROSAIL dataset
      user_lut <- readRDS("www/datasets/LUT_prosailSE2.rds")
      showNotification("PROSAIL dataset loaded successfully.", type = "message")
      return(user_lut)

    } else if (dataset_type == "INFORM") {
      # Load predefined INFORM dataset
      user_lut <- readRDS("www/datasets/LUT_informSE2.rds")
      showNotification("INFORM dataset loaded successfully.", type = "message")
      return(user_lut)

    } else {
      # No dataset selected or uploaded
      showNotification("No valid dataset selected or uploaded.", type = "error")
      return(NULL)
    }
  }


  ## Get Processed datasets   ---------------------------------------

  # Reactive function to load the selected or uploaded dataset
  processed_data <- reactive({
    req(input$lut_db)  # Ensure `lut_db` input is available
    dataset <- loadDataset(input$lut_db)
    user_lut_data(dataset)
    return(dataset)
  })
  # UI Output for custom error message
  output$error_message <- renderUI({
    if (is.null(processed_data())) {
      div(style = "color: red; font-weight: bold;",
          "Error: No data available. Please upload a valid dataset or select a predefined option.")
    } else {
      return(NULL)
    }
  })

  # Render data table for the selected dataset
  output$lut_header <- DT::renderDataTable({
    dataset <- processed_data()
    if (!is.null(dataset) && ncol(dataset) > 0) {
      DT::datatable(
        dataset,
        options = list(
          scrollX = TRUE,
          columnDefs = list(list(visible = TRUE, targets = 0:4)),
          pageLength = 5
        ),
        selection = 'none'
      )
    } else {
      data.frame(Headers = c("No headers found in the dataset."))
    }
  })

  ## Get statiistical   ---------------------------------------

  # Reactive to calculate summary statistics for the selected bands
  band_stats <- reactive({

    validate(need(processed_data(), "No dataset available."))
    dataset <- processed_data()

    # Select only columns that start with 'B' (e.g., B1, B2, etc.)

    band_cols <- dataset %>%
      dplyr::select(starts_with("B"))

    if (input$plotting_indices == T ){
      # Show the modal window
      show_modal_spinner()
      indices <-getIndicesSE2.ML(df=as.data.frame(band_cols), sensor = "Sentinel-2a", df.data = NULL, fast.process =T)
      #print(indices)
      remove_modal_spinner()
      # After processing, update to the Indices tab
      updateTabsetPanel(session, "tabs", selected = "Indices")

      # Scroll to the top of the page using JavaScript
      session$sendCustomMessage(type = 'scrollToTop', message = list())

      if (is.null(indices) || nrow(indices) == 0) {
        showNotification("Error calculating indices. Please check your input data.", type = "error")
        return(NULL)
      }

      dataset_indices <- cbind(dataset, indices)
      showNotification("Indices calculated successfully.", type = "message")
    } else {
      showNotification("Please check the 'Calculate indices' box to proceed.", type = "warning")

    }


    # Calculate average, median, and percentiles for each band

    stats.SE2 <- band_cols %>%
      reframe(
        average = apply(band_cols, 2, mean, na.rm = TRUE),
        median = apply(band_cols, 2, median, na.rm = TRUE),
        percentile.25 = apply(band_cols, 2, quantile, probs = 0.25, na.rm = TRUE),
        percentile.50 = apply(band_cols, 2, quantile, probs = 0.50, na.rm = TRUE),
        percentile.75 = apply(band_cols, 2, quantile, probs = 0.75, na.rm = TRUE)
      ) %>%
      mutate(Band = names(band_cols)) %>%
      dplyr::select(Band, everything())   %>%
      mutate(Band = names(band_cols)) %>%
      select(Band, everything()) %>%
      filter(Band != "B8A")
    # Sort the Band column as a factor
    stats.SE2 <- stats.SE2 %>%
      mutate(Band = factor(Band, levels = paste('B', sort(as.numeric(gsub('B', '', Band))), sep = "")))

    # Reshape data to long format for easier plotting
    stats_long <- stats.SE2 %>%
      pivot_longer(cols = -Band, names_to = "Statistic", values_to = "Value")

    if (input$plotting_indices == T ){

    return(list(stats_long = stats_long, dataset_indices = dataset_indices))
    } else {
      return(list(stats_long = stats_long))
    }

  })
  ## Get plot_checking   ---------------------------------------

  # Render the plot in UI
  output$plot_checking <- renderPlot({
    stats_long <- band_stats()$stats_long
    # Color if I need
    color.selected <- c('forestgreen','red','darkgoldenrod2','dodgerblue3','darkseagreen3','indianred2')
    color.i = color.selected[4]

    # Generate the plot using ggplot2
    plot_ <- ggplot(stats_long, aes(x = Band, y = Value, color=Statistic, group = Statistic)) +
      geom_line() +  # Lines connecting points
      geom_point() +  # Points at each value

      labs(title = "Average, Median, and Percentiles by Band",
           x = "Band",
           y = "Value") +
      theme_bw() +
      labs(title = "", x = "",y = "Reflectance") +

      theme(
        text = element_text(size = 14, face='bold'),  # Increase the text size
        axis.title = element_text(size = 16, face = "bold"),  # Make axis titles bold
        axis.text = element_text(face = "bold"),  # Make axis numbers bold
        axis.text.x = element_text(angle = 45, hjust = 1,face='bold'),
        plot.title = element_text(face = "bold"),  # Make plot title bold
        plot.subtitle = element_text(size = 14,face='bold')  # Adjust subtitle size
      )

    plot_


  })
  # In your server function
  output$instructions_ui <- renderUI({
    if (input$plotting_indices && !is.null(band_stats()))  {  # Check if indices have been estimated
      tagList(
        br(),
        h4("Instructions:"),
        p("Select the variables for the X and Y axes below to explore their relationship."),
        p("Note₁: Use the dropdowns to select the variables you wish to plot."),
        p(em("Note₂: Ensure that the indices have been calculated.")),
        p(HTML("If the option is not available, please check the corresponding <strong>Calculate Indices</strong> box."))
      )
    }
  })

  ## Get scatter_plot   ---------------------------------------
  #Render scatter plot for exploring relationships between indices
  output$scatter_plot <- renderPlot({
    dataset_indices <- band_stats()$dataset_indices
    req(dataset_indices)  # Ensure dataset_indices is available

    x_var <- input$x_axis  # User-selected variable for the x-axis
    y_var <- input$y_axis  # User-selected variable for the y-axis

    # Create scatter plot if both x and y variables are selected
    if (!is.null(x_var) && !is.null(y_var)) {
      ggplot(dataset_indices, aes_string(x = x_var, y = y_var)) +
        geom_point(color = 'blue') +
        labs(title = paste("Scatter-plot of", y_var, "vs", x_var),
             x = x_var,
             y = y_var) +
        theme_bw() +
        theme(
          text = element_text(size = 14, face = 'bold'),
          axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(face = "bold")
        )
    } else {
      ggplot() + labs(title = "Select variables to plot")  # Show empty plot with a message
    }
  })

  # UI for selecting variables for the scatter plot
  output$select_axes_ui <- renderUI({
    dataset_indices <- band_stats()$dataset_indices
    req(dataset_indices)  # Ensure dataset_indices is available

    # Create selectInput for X-axis and Y-axis
    tagList(
      selectInput("x_axis", "Select X-axis variable:", choices = names(dataset_indices), selected = NULL),
      selectInput("y_axis", "Select Y-axis variable:", choices = names(dataset_indices), selected = 'NDVI')
    )
  })
  # Set the file size limit for uploads
  options(shiny.maxRequestSize = 10 * 1024^2)  # 10 MB

  # Reactive output to indicate if a dataset has been uploaded
  output$hasUploaded <- reactive({
    return(!is.null(user_lut_data()))
  })

  outputOptions(output, "hasUploaded", suspendWhenHidden = FALSE)


  # Render links based on R's Notebook selection (notebook_r)
  output$lut_selection <- renderUI({
    if (input$lut_db == "upload") {
      tagList(
        # Output for the plot

        br(),
        # Description for the DataTable
        p('The selected dataset contains relevant information and visualizations.'),
        p('Please find the details below.'),
      )
    } else if (input$lut_db == "PROSAIL") {
      tagList(

        br(),
        # Description for the DataTable
        p('The selected PROSAIl dataset contains relevant information and visualizations.'),
        p('Please find the details below.'),


      )
    } else if (input$lut_db == "INFORM") {
      tagList(
        br(),

        # Description for the DataTable
        p('The selected INFORM dataset contains relevant information and visualizations.'),
        p('Please find the details below.'),

      )
    } else {
      NULL
    }
  })


  ## Get depVar   ---------------------------------------
  # Function to get the input bands based on the dependent variable
  get_input_bands <- function(depVar) {
    if (depVar %in% c('Cab')) {
      return(c('B2','B3','B4','B5','B6','B7','B8','CR.red.nir.1',
               'CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown'))
    } else if (depVar == 'LAI') {
      return(c('B2','B3','B4','B5','B6','B7','B8','B11','B12',
               'CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1',
               'NDRE','IRECI','CIgreen','CR.SWIR'))
    } else if (depVar == 'leaf water content') {
      return(c('B8A','B11','B12','WDRVI','MNDVI','NDWI','NDWI2','SBI','WET','GVI','CR.SWIR'))
    } else if (depVar == 'Cbrown') {
      return(c('B2','B3','B4','B5','B6','B7','B8','EVI','TCARI',
               'CR.red.nir.6','NDVIv','kNDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown'))
    }  else if (depVar == 'EWT') {
      return(c('B8','B11','B12','WDRVI','MNDVI','NDWI','NDWI2','SBI','WET','GVI','CR.SWIR'))
    }

  }


  # Function to get the input bands based on the dependent variable
  get_selected_input <- function(inputs_toML) {
    if (inputs_toML %in% c('indices')) {
      return(c('CR.red.nir.1',
               'CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown'))

    }  else if (inputs_toML == 'visible_indices') {
      return(c('CR.red.nir.1','CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown'))

    } else if (inputs_toML == 'rededge_indices') {
      return(c('CR.red.nir.1', 'CR.red.nir.6','RedEg1','CIre','CIgreen', 'CR.Brown'))

    } else if (inputs_toML == 'swir_indices') {
      return(c('WDRVI','MNDVI','NDWI','NDWI2','SBI','WET','GVI','CR.SWIR'))

    } else if (inputs_toML == 'reflectance') {
      return(c('B2','B3','B4','B5','B6','B7','B8','B11','B12'))

    } else if (inputs_toML == 'vnir_reflectance') {
      return(c('B2','B3','B4','B5','B6','B7','B8'))

    } else if (inputs_toML == 'swir_reflectance') {
      return(c('B8','B11','B12'))

    } else if (inputs_toML == 'indices_reflectance') {
      return(c('B2','B3','B4','B5','B6','B7','B8','B11','B12','CR.red.nir.1',
               'CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown','WDRVI','MNDVI','NDWI','NDWI2','SBI','WET','GVI','CR.SWIR'))

    } else if (inputs_toML == 'indices_reflectance_vnir') {
      return(c('B2','B3','B4','B5','B6','B7','B8','CR.red.nir.1',
               'CR.red.nir.6','NDVI','TCARI_OSAVI','RedEg1','CIre','PSSRa','MCARI','OSAVI','GM1','Datt1',
               'NDRE','IRECI','CIgreen', 'CR.Brown'))

    } else if (inputs_toML == 'indices_reflectance_swir') {
      return(c('B8','B11','B12','WDRVI','MNDVI','NDWI','NDWI2','SBI','WET','GVI','CR.SWIR'))
    }


  }



  observeEvent(input$train_model, {
    # Extract input values

    # Check if the checkbox is not selected
    if (!input$plotting_indices) {
      showModal(modalDialog(
        title = "Indices Calculation Required",
        "Please check 'Calculate and plot vegetation indices' to proceed with the calculation of indices.",
        easyClose = TRUE,
        footer = NULL
      ))

      # Exit the event if the checkbox is not checked
      return()
    }


    # After training is done, update to the Predictions tab
    updateTabsetPanel(session, "tabs", selected = "Predictions")

    # Scroll to the top of the page using JavaScript
    session$sendCustomMessage(type = 'scrollToTop', message = list())




    model <- input$models_
    depVar <- input$depVar
    inputML <- input$inputs_toML

    # Define the data frame and dependent variable
    dataset <- band_stats()$dataset_indices

    # Check the LUT database selection
    if (is.null(dataset)) {
      showModal(modalDialog(
        title = "Dataset Selection Required",
        "Please select a valid LUT database or upload a new dataset before proceeding.",
        easyClose = TRUE,
        footer = NULL
      ))

      # Exit the event if no valid LUT database is selected
      return()
    }

    if (input$depVar == 'EWT' | input$depVar == 'Prot'  | input$depVar == 'CBC' ){
      dataset[[depVar]] <- dataset[[depVar]] * 1000
    }
    #inputs_bands <- get_input_bands(depVar)
    inputs_bands <- get_selected_input(inputML)
    print(inputs_bands)
    # Generate a random seed based on the current time
    set.seed(as.numeric(Sys.time()) %% 10000)  # Limits seed to 4 digits

    # Define the directory for model outputs
    output_dir <- file.path('www', 'models.ML')

    # Remove the existing directory if it exists
    if (dir.exists(output_dir)) {
      unlink(output_dir, recursive = TRUE)
    }
    # Create a temporary directory
    # Define the directory for model outputs
    temp_dir <- file.path(output_dir, format(Sys.time(), "%Y%m%d_%H%M%S"),'/')
    dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

    # Generate filename with date and time
    paste0("model_and_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tar")



    show_modal_spinner()
    # Use withProgress to show a progress bar during the model training process
    withProgress(message = 'Training model', value = 0, {

    if (input$models_ =='CNN' | input$models_ == 'Hidden_layers'){
      if(input$models_ == 'Hidden_layers'){
        model_ <- 'Hidden-layers'
      } else {
        model_ <-input$models_
      }

      method.preProcess <- input$method.preProcess
      optimizer <- input$optimizer
      n_layers <- input$n_layers
      n_neurons <- input$n_neurons
      batch_size <- input$batch_size
      n_epochs <- input$n_epochs
      # Call the getMLmodel.withRetrain function
      models.time <- getMLmodel.withRetrain(
        dataset = dataset[, c(depVar, inputs_bands)],
        depVar = depVar,
        model = model_,
        optimizer = optimizer,
        n.times = 1,
        n.neurons = n_neurons,
        n.layers = n_layers,
        batch.size = batch_size,
        n.epochs = n_epochs,
        save.model = T,
        path.model = temp_dir,
        prop.split = c(0.8, 0.2),
        data.trans = 'preProcess',
        method.preProcess = method.preProcess,
        depVar.trans = FALSE,
        session=session )

      # Render the training history table
      #output$training_history <- DT::renderDT({
       # DT::datatable(history_df)
      #})
      output$status <- renderText("Model training complete.")

    } else {

      ## Best RF; SVM (no importance); NN; GB, xGB;
      algorithms<-c('RF','SVM','NN','GB', 'xGB','BRNN','Ensemble')
      algorithm.i  <- input$models_

      # Convert percentage to proportion
      n.prop <- input$p_samplesML / 100

      n.samples.reduced <-ceiling(n.prop * nrow(dataset))

      print(length(n.samples.reduced))
      require(e1071)
      require(caret)
      #inputs <- inputs <- c('LAI.preds.median','TVI','EVI','NDVIv','kNDVI','CR.red.nir.6')


      models.time<- get.inversion(data=dataset[, c(depVar, inputs_bands)], depVar=depVar, inputs=inputs_bands,n.cores=2,
                            n.samples=n.samples.reduced,algorithm=algorithm.i,method.resampling='repeatedcv',
                            seed=set.seed(as.numeric(Sys.time()) %% 10000) , save.model = T, save.path = temp_dir)
     # print(models.time)

    }
    }) ## end training the model

    # Print the model summary to the viewer
   # output$model_summary <- renderPrint({
    #  summary(models.time[['models.keras']])
    #})

    # Define the download handler
    output$downloadData_ML <- downloadHandler(
      filename = function() {
        paste0("model_and_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tar")
      },
      content = function(file) {

        current_date_dir <- temp_dir # Updated to current date
        # List all files in the current date directory
        files_to_compress <- list.files(path = current_date_dir, full.names = TRUE)

        # Print files to console (for debugging)
        print(files_to_compress)

        # Check if there are files to compress
        if (length(files_to_compress) > 0) {

          #Define the name of the output tar file
          tar_file_name <- paste0("www/downloads/model_and_stats_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tar")

          # Create the tar file, compressing all listed files
          tar(tar_file_name, files = files_to_compress)

          # Output a message indicating successful compression
          message(paste("Compressed files into:", tar_file_name))

          # Remove all files and folders in the www/models.ML/ directory
          ML_outputs <- 'www/models.ML/'
          all_folders_to_rm <- list.dirs(ML_outputs, full.names = TRUE, recursive = FALSE)  # List only the immediate subdirectories
          unlink(all_folders_to_rm, recursive = TRUE, force = TRUE)  # Remove only the folders
          message("Removed all folders in the www/models.ML/ directory.")

          # Additional check for the downloads directory
          downloads_dir <- 'www/downloads/'
         # if (length(list.files(downloads_dir)) > 2) {
          #  unlink(file.path(downloads_dir, "*"), recursive = TRUE, force = TRUE)  # Remove all files in downloads
           # message("Removed all files in the downloads directory.")
         # }

        } else {
          message("No files to compress in the specified directory.")
        }

        # Move the tar file to the specified output file location
        file.copy(tar_file_name, file)
      }
    )

    # Create reactive values to manage readiness state
    output_ready <- reactiveValues(plot = FALSE, table = FALSE)

    # Generate a ggplot (for demonstration)
    output$predictionPlot <- renderPlot({

      if (input$models_ =='CNN' | input$models_ == 'Hidden_layers'){
        plot_val <- models.time[['plot.val']]
      } else {
        plot_val <- models.time[['plot']]
      }
        print(plot_val)

        output_ready$plot <- TRUE
    })

    # Show notification after model training is complete
    observe({
      if (input$models_ =='CNN' | input$models_ == 'Hidden_layers'){
        if (!is.null(models.time[['plot.val']]) && !is.null(models.time[['stats']])) {
          showNotification("Model training complete.", type = "message")
        }
      } else {
        if (!is.null(models.time[['plot']]) && !is.null(models.time[['statistics']])) {
          showNotification("Model training complete.", type = "message")
        }
      }
    })


    # Render table for the dataset in models.time[['stats']]
    output$statsTable_ML <- DT::renderDataTable({
      # Access the dataset
      if (input$models_ =='CNN' | input$models_ == 'Hidden_layers'){
        stats_df <- models.time[['stats']]
      } else {
        stats_df <- models.time[['statistics']]
      }


      # Render the dataset as a table
      DT::datatable(stats_df, options = list(pageLength = 10, autoWidth = TRUE))

     # output_ready$table <- TRUE
    })


    remove_modal_spinner()


    # Reactive block to generate code based on user selections
    model_code <- reactive({

      # If the selected model is CNN or Hidden Layers Neural Network
      if (input$models_ == 'CNN' | input$models_ == 'Hidden_layers') {

        # Define the model name
        model_ <- if (input$models_ == 'Hidden_layers') 'Hidden-layers' else input$models_

        # Generate code for CNN/Hidden Layers Neural Network model
        code <- paste0(
          "# Define the selected model and key hyperparameters\n",
          "model_ <- '", model_, "'\n",  # Model type (CNN or Hidden Layers)
          "method.preProcess <- '", input$method.preProcess, "'\n",  # Data pre-processing method
          "optimizer <- '", input$optimizer, "'\n",  # Optimizer type for model training
          "n_layers <- ", input$n_layers, "\n",  # Number of layers in the neural network
          "n_neurons <- ", input$n_neurons, "\n",  # Number of neurons per layer
          "batch_size <- ", input$batch_size, "\n",  # Batch size for training
          "n_epochs <- ", input$n_epochs, "\n",  # Number of epochs for training
          # Properly format inputs_bands as a vector
          "inputs_bands <- c(",paste("'",input$inputs_bands, "'",sep='', collapse = ", "), ")\n",  # Input bands selected by the user

          "# Call to function to train and evaluate the model\n",
          "models.time <- getMLmodel.withRetrain(\n",
          "  dataset = dataset[, c(",input$depVar,", inputs_bands)],\n",  # Use dependent variable and selected input bands
          "  depVar = depVar,\n",  # Dependent variable (trait to estimate)
          "  model = model_,\n",  # Model type (CNN/Hidden Layers)
          "  optimizer = optimizer,\n",  # Optimizer for training
          "  n.times = 1,\n",  # Number of times to repeat training
          "  n.neurons = n_neurons,\n",  # Number of neurons per layer
          "  n.layers = n_layers,\n",  # Number of layers
          "  batch.size = batch_size,\n",  # Batch size
          "  n.epochs = n_epochs,\n",  # Number of epochs
          "  save.model = TRUE,\n",  # Option to save the trained model
          "  path.model = temp_dir,\n",  # Path to save the model
          "  prop.split = c(0.8, 0.2),\n",  # Split ratio for training and testing data (80-20%)
          "  data.trans = 'preProcess',\n",  # Data transformation method
          "  method.preProcess = method.preProcess,\n",  # Pre-processing method
          "  depVar.trans = FALSE\n)"  # No transformation on dependent variable

        )

      } else {  # For other models (e.g., Random Forest, SVM, XGBoost, etc.)

        # Generate code for other machine learning models
        code <- paste0(
          "# Define the selected algorithm and key settings\n",
          "algorithm.i <- '", input$models_, "'\n",  # Selected algorithm (RF, SVM, XGBoost, etc.)
          "n.prop <- ", input$p_samplesML, " / 100\n",  # Proportion of data to use for training
          "n.samples.reduced <- ceiling(n.prop * nrow(dataset))\n",  # Calculate the number of samples for training
          # Properly format inputs_bands as a vector
          "inputs_bands <- c(", paste("'", get_selected_input(input$inputs_toML), "'", sep='',collapse = ", "), ")\n",  # Input bands selected by the user

          "# Call to function to train and evaluate the model\n",
          "models.time <- get.inversion(\n",
          "  data = dataset[, c(",input$depVar,", inputs_bands)],\n",  # Use dependent variable and selected input bands
          "  depVar = depVar,\n",  # Dependent variable (trait to estimate)
          "  inputs = inputs_bands,\n",  # Input bands or features selected by the user
          "  n.cores = 2,\n",  # Number of CPU cores to use for parallel processing
          "  n.samples = n.samples.reduced,\n",  # Number of samples for training
          "  algorithm = algorithm.i,\n",  # Algorithm type
          "  method.resampling = 'repeatedcv',\n",  # Resampling method (repeated cross-validation)
          "  seed = set.seed(as.numeric(Sys.time()) %% 10000),\n",  # Set a random seed for reproducibility
          "  save.model = TRUE,\n",  # Option to save the trained model
          "  save.path = temp_dir\n)"  # Path to save the model

        )
      }

      # Return the generated code as output
      return(code)
    })

    # Render the R code in the UI
    output$model_code <- renderText({
      model_code()
    })

    output$download_code <- downloadHandler(
      filename = function() { paste0(input$models_, "_model_code.R") },
      content = function(file) {
        writeLines(model_code(), file)
      }
    )

    # Check the ML is done, in this context downloadbutton apparece
    output$plotReady_tableReady <- renderUI({
      # Ensure both plot and table are ready
      req(output_ready$plot)  # Ensure plot data is available

      tagList(
        div(
          style = "display: inline-block; margin-right: 10px;",  # Style for first button
          downloadButton("downloadData_ML", "Download Model & Stats")
        ),

        div(
          style = "display: inline-block;",  # Style for second button
          downloadButton("download_code", "Download R Code")
        ),
        br(), br(),
        h4("Dowloading also the R Code used in your ML parametrization:"),
        verbatimTextOutput("model_code"),  # Display the generated R code
        # Properly format inputs_bands as a vector
        #inputs_bands <- paste("'", get_selected_input(input$inputs_toML), "'", sep = '', collapse = ", "),



        p(em('Note₁: The download includes the trained model and associated statistical data.')),
        p(em('Note₂: The R download button provides the main script for model training.')),
        br(),
        p(("The inputs used in the selected model are: ")),  # Subscript note for input bands
        DT::dataTableOutput("inputsBandsTable"),  # Placeholder for the inputs_bands DT table  # Placeholder for the inputs_bands table


      )
    })

    # Render the inputs_bands table using DT
    output$inputsBandsTable <- DT::renderDataTable({
      # Create a data frame with inputs_bands for display
      inputs_bands_vector <- get_selected_input(input$inputs_toML)  # Get selected input bands
      inputs_df <- data.frame(Inputs = inputs_bands_vector)  # Create a data frame
      # Render the dataset as a table
      DT::datatable(inputs_df, options = list(pageLength = 10, autoWidth = TRUE))

    })

  }) ## ed observer



  # 7) Tutorials module   ---------------------------------------

  # Render links based on Python's Notebook selection (notebook_p)
  output$python_notebooks <- renderUI({
    if (input$notebook_p == "STAC Application") {
      tagList(
        p("Click below to explore the STAC Application notebook for Python:"),
        a("Explore STAC Application (Python)", href = "Notebooks/R/S2Cube/s2cube_rstac.html", target = "_blank")
      )
    } else if (input$notebook_p == "Time-Series with Sentinel-2") {
      tagList(
        p("Click below to explore Time-Series with Sentinel-2 (Python):"),
        a("Explore Sentinel-2 (Python)", href = "https://example.com/sentinel-2", target = "_blank")
      )
    } else if (input$notebook_p == "Extraction and buffer") {
      tagList(
        p("Click below to explore Extraction and buffer (Python):"),
        a("Explore Extraction and buffer (Python)", href = "https://example.com/extraction", target = "_blank")
      )
    } else if (input$notebook_p == "Google Earth Engine") {
      tagList(
        br(),  # Add some space between the link and the logo
        p("This link takes you directly to a Google Colab notebook where you can explore Google Earth Engine using Python.
          The notebook is designed for retrieving metadata from the Sentinel-2 collection, saving time series data as TIFF format,
          and managing the data using Xarray for efficient analysis."),
        p("Click below to explore Google Earth Engine (Google Colab):"),
        a("Explore Google Earth Engine using Google Colab (Python)", href = "https://colab.research.google.com/drive/1gAFfOyQQ9kM9GQk4zRVlyQIQVn0DYTo9?usp=sharing", target = "_blank"),
        br(),
        br(),
        #   img(src = "GooogleColab.png", alt = "Google Colab Logo", style = "width:100px; height:auto; margin-top: 10px;")
      )
    } else {
      NULL
    }
  })


  # Render links based on R's Notebook selection (notebook_r)
  output$r_notebooks <- renderUI({
    if (input$notebook_r == "STAC Application") {
      tagList(
        p("Click below to explore the STAC Application notebook for R:"),
        a("Explore STAC Application (R)", href = "Notebooks/R/S2Cube/s2cube_rstac.html", target = "_blank")
      )
    } else if (input$notebook_r == "Time-Series with Sentinel-2") {
      tagList(
        p("Click below to explore Time-Series with Sentinel-2 (R):"),
        a("Explore Sentinel-2 (R)", href = "https://example.com/sentinel-2", target = "_blank")
      )
    } else if (input$notebook_r == "ToolsRTM package") {
      tagList(
        p("Click below to explore how to simulate canopy-level reflectance using  radiative transfer models (RTMs) and resample the simulations at 1nm (400-2500 nm) to Sentinel-2 spectral resolution:"),
        p("This notebook utilizes key functions from the ToolsRTM package for advanced spectral analysis. "),
        p(HTML("In particular, here we show the <strong>getIndicesSE</strong> function to calculate essential spectral indices  for Sentinel-2 sensors, which are crucial for monitoring vegetation health and evaluating biophysical traits.")),

        a("Explore ToolsRTM package (R)", href = "Notebooks/R/ToolsRTM/ToolsRTM.html", target = "_blank")
      )
    } else if (input$notebook_r == "SCOPEinR package") {
      tagList(
        p("Click below to explore how to simulate the reflectance at canopy level using SCOPE model(R):"),
        a("Explore SCOPEinR package (R)", href = "Notebooks/R/SCOPEinR/SCOPEinR.html", target = "_blank")
      )
    } else if (input$notebook_r == "Extraction and buffer") {
      tagList(
        p("Click below to explore Extraction and buffer (R):"),
        a("Explore Extraction and buffer (R)", href = "https://example.com/extraction", target = "_blank")
      )
    } else if (input$notebook_r == "Google Earth Engine") {
      tagList(
        p("Click below to explore Google Earth Engine (R):"),
        a("Explore Google Earth Engine (R)", href = "https://earthengine.google.com", target = "_blank")
      )
    } else {
      NULL
    }
  })


  downloads_dir <- "www/downloads"  # Specify your downloads directory

  # Cleanup when the session ends

  session$onSessionEnded(function() {
    # Check if there are files to remove
    if (length(list.files(downloads_dir)) > 0) {
      # Remove all files in the downloads directory
      unlink(file.path(downloads_dir, "*"), recursive = TRUE, force = TRUE)
      message("Removed all files in the downloads directory.")
    } else {
      message("No files to remove in the downloads directory.")
    }
  })

  ## end server ----------------------------------
})


# Run the application
source("ui.R")
shinyApp(ui = ui, server = server)

