
##############################################################################################################################
#	0. load main Libraries   -----
##############################################################################################################################


#  Define UI for the app -----------------------------------------

ui <- navbarPage("Online reflectance simulator",theme = shinytheme("cosmo"),
                 
   
      ######################################################################################
      #####################################################################################
                 tabPanel(title = "Interactive ToolsRTM",
                          # Add a sidebar layout
                          sidebarLayout(
                            # Add a sidebar panel
                            sidebarPanel(
                              # Add a little information about ebird data
                              class = "sidebar",
                              style = "height: 90vh; overflow-y: auto;", 
                              # Leaf Model selection
                              h3('Select RT models:'),
                              #tags$img(src= "JRC.png", height = 80, width = 200),
                              div(id = "upper-panel",
                                  selectInput("leaf_model", label = "Leaf Model:",
                                              choices = c("PROSPECT-PRO","PROSPECT-D", "Liberty", "FLUSPECT-B", "FLUSPECT-B-Cx"))
                                  
                              ),
                              div(id = "lower-panel",
                                  # Canopy Model selection
                                  selectInput("canopy_model", label = "Canopy Model:",
                                              choices = c("fourSAILH", "INFORM", "fourSAILH2"))
                              ),
                              h4('leaf parameters :'),
                              # Leaf Model parameters (1)
                              conditionalPanel(
                                condition = "input.leaf_model == 'PROSPECT-PRO'",
                                sliderInput("Cab", "Chlorophyll content (μg cm-2)", min = 0, max = 100, value = 45, step = 0.1),
                                sliderInput("Car", "Carotenoid content (μg cm-2)", min = 0, max = 40, value = 12, step = 0.1),
                                sliderInput("Anth", "Anthocyanin content (μg cm-2)", min = 0, max = 7, value = 2, step = 0.1),
                                sliderInput("Cbrown", "Cbrown", min = 0, max = 1, value = 0.2, step = 0.05),
                                sliderInput("N", "mesophyll structure parameter", min = 1, max = 4, value = 2.45, step = 0.1),
                                sliderInput("EWT", "Water content (g cm-2)", min = 0.0001, max = 0.05, value = 0.01, step = 0.005),
                            #    sliderInput("LMA", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.005),
                                sliderInput("alpha", "alpha", min = 0, max = 60, value = 40, step = 0.1),
                                sliderInput("Prot", "Proteins (g cm-2)", min = 0.0001, max = 0.03, value = 0.012, step = 0.005),
                                sliderInput("CBC", "Carbon-based Constutient (g cm-2)", min = 0, max = 0.03, value = 0.010, step = 0.005)
                                
                              ),
                              
                              # Leaf Model parameters (2)
                              conditionalPanel(
                                condition = "input.leaf_model == 'PROSPECT-D'",
                                
                                sliderInput("Cab_d", "Chlorophyll content (μg cm-2)", min = 0, max = 100, value = 20, step = 0.1),
                                sliderInput("Car_d", "Carotenoid content (μg cm-2)", min = 0, max = 10, value = 2.5, step = 0.1),
                                sliderInput("Anth_d", "Anthocyanin content (μg cm-2)", min = 0, max = 7, value = 2, step = 0.1),
                                sliderInput("Cbrown_d", "Cbrown", min = 0, max = 1, value = 0.2, step = 0.1),
                                sliderInput("N_d", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_d", "Water content (g cm-2)", min = 0, max = 0.05, value = 0.009, step = 0.002),
                                sliderInput("LMA_d", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.009, step = 0.005),
                                sliderInput("alpha_d", "alpha", min = 0, max = 60, value = 20, step = 0.1)
                                
                              ),
                              # Leaf Model parameters (3)
                              conditionalPanel(
                                condition = "input.leaf_model == 'Liberty'",
                                sliderInput("Cab_l", "Chlorophyll content  (μg cm-2) ", min = 0, max = 60, value = 40, step = 1),
                                sliderInput("EWT_l", "Water content  (μg cm-2)", min = 0, max = 0.05, value = 0.009, step = 0.001),
                                sliderInput("lign_cell", "Lignin and cellulose content", min = 10, max = 80, value = 40, step = 0.1),
                                sliderInput("Nitrogen", "Nitrogen content (g m-2)", min = 0.3, max = 2, value = 1, step = 0.1),
                                sliderInput("cell_d", "Cell diameter (m-6)", min = 20, max = 200, value = 58, step = 0.1),
                                sliderInput("inter_c", "Intercellular air space", min = 0.01, max = 0.1, value = 0.045, step = 0.005),
                                sliderInput("baseline_abs", "baseline", min = 0.0004, max = 0.0006, value = 0.0004),
                                sliderInput("leaf_thick", "leaf thickness", min = 1, max = 10, value = 1.6, step = 0.1),
                                sliderInput("albino_abs", "Albino absorption", min = 0, max = 4, value = 2, step = 0.1)

                                
                              ),
                              # Leaf Model parameters (4)
                              conditionalPanel(
                                condition = "input.leaf_model == 'FLUSPECT-B'",
                                
                                sliderInput("fqe_fd", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = 0.02, step = 0.005),
                                sliderInput("Cab_fd", "Chlorophyll content (μg cm-2)", min = 0, max = 100, value = 50, step = 0.1),
                                sliderInput("Car_fd", "Carotenoid content (μg cm-2)", min = 0, max = 40, value = 20, step = 0.1),
                                sliderInput("Cs_fd", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                                sliderInput("Cx_fd", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                                sliderInput("N_fd", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_fd", "Water content (g cm-2)", min = 0, max = 0.05, value = 0.01, step = 0.005),
                                sliderInput("LMA_fd", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.01)
                                
                              ),
                              
                              # Leaf Model parameters (4)
                              conditionalPanel(
                                condition = "input.leaf_model == 'FLUSPECT-B-Cx'",
                                sliderInput("fqe_fp", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = 0.02, step = 0.005),
                                sliderInput("Cab_fp", "Chlorophyll content (μg cm-2)", min = 0, max = 100, value = 50, step = 0.1),
                                sliderInput("Car_fp", "Carotenoid content (μg cm-2)", min = 0, max = 40, value = 20, step = 0.1),
                                sliderInput("Anth_fp", "Anthocyanin content (μg cm-2)", min = 0, max = 7, value = 2, step = 0.1),
                                sliderInput("Cs_fp", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                                sliderInput("Cx_fp", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                                sliderInput("N_fp", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_fp", "Water content (g cm-2)", min = 0, max = 0.05, value = 0.01, step = 0.005),
                                sliderInput("LMA_fp", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.005),
                                sliderInput("Prot_fp", "Proteins (g cm-2)", min = 0, max = 0.03, value = 0.012, step = 0.005),
                                sliderInput("CBC_fp", "Carbon-based Constutient (g cm-2)", min = 0, max = 0.03, value = 0.010, step = 0.005)
                              ),
                              
                              h4('canopy parameters :'),
                              # Leaf Model parameters 
                              conditionalPanel(
                                condition = "input.canopy_model == 'fourSAILH'",
                                sliderInput("LAI", "LAI (m2/m2)", min = 0, max = 8, value = 4, step = 0.1),
                                sliderInput("LIDFa", "LIDFa (°)", min = 0, max = 90, value = 30, step = 0.1),
                                sliderInput("hotspot", "hotspot", min = 0, max = 1, value = 0.5, step = 0.01),
                                sliderInput("tts", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                                sliderInput("tto", "tto (deg)", min = 0, max = 90, value = 30, step = 0.2),
                                sliderInput("psi", "psi (deg)", min = 0, max = 180, value = 0, step = 0.5),
                                sliderInput("psoil", "soil factor", min = 0, max = 1, value = 0.35,step = 0.1)
                                
                              ),
                              conditionalPanel(
                                condition = "input.canopy_model == 'INFORM'",
                                sliderInput("LAI_", "LAI (m2/m2)", min = 0, max = 8, value = 4, step = 0.1),
                                sliderInput("LIDFa_", "LIDFa (deg)", min = 0, max = 90, value = 30, step = 0.1),
                                sliderInput("hotspot_", "hotspot", min = 0, max = 1, value = 0.1, step = 0.1),
                                sliderInput("tts_", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                                sliderInput("tto_", "tto (deg)", min = 0, max = 90, value = 45, step = 0.2),
                                sliderInput("psi_", "psi (deg)", min = 0, max = 180, value = 0, step = 0.5),
                                sliderInput("LAIu_", "understory LAI (m2/m2)", min = 0.05, max = 2, value = 0.5,step = 0.015),
                                sliderInput("cd_", "Crown diameter (m)", min = 0, max = 10, value = 4.5, step = 0.5),
                                sliderInput("sd_", "Stem density (ha-1)", min = 0, max = 3000, value = 2500, step = 1.5),
                                sliderInput("h_", "tree height (m)", min = 1, max = 40, value = 20, step = 0.1),
                                sliderInput("skyl_", "skyl (fraction-Fixed to 0.1)", min = 0.001, max = 0.4, value = 0.1, step = 0.005),
                                sliderInput("psoil_", "soil factor", min = 0, max = 1, value = 0.1, step = 0.1)
                              ),
                              
                              conditionalPanel(
                                condition = "input.canopy_model == 'fourSAILH2'",
                                sliderInput("p1_s2", "to_define ...", min = 0, max = 8, value = 0.5),
                                sliderInput("p2_s2", "to define ...", min = 0, max = 2, value = 0.1),
                                sliderInput("p3_s2", "to define ...", min = 0, max = 1, value = 0.1)
                              ),
                              tags$img(src = "JRC.png", height = "100px", width = "200px"),
                              
                            ),# Close sidebarPanel  
                            
                            mainPanel(
                              tabsetPanel(
                                ### Firs Pannel
                                tabPanel("Canopy reflectance",
                                         h2("Interactive reflectance"),
                                         #checkboxInput("checkbox", "Accumulate reflectance spectra", FALSE),
                                         #prettyCheckbox(inputId = "checkbox", label = "Check me!", icon = icon("check")),
                                         plotOutput("reflectance_plot"),
                                         
                                         materialSwitch(inputId = "checkbox", label = "Accumulate reflectance spectra", status = "danger"),
                                         
                                         selectInput("sensor", "Select satellite sensor:",
                                                     choices = c('RTM','Sentinel2a','Sentinel2b',
                                                                 'EnMAP','PRISMA','Hyperion','MODIS',
                                                                 'Landsat4','Landsat5','Landsat7','Landsat8', #'ALI',
                                                                 'Quickbird','RapidEye','WorldView2-4','WorldView2-8')),
                                         checkboxInput("AddRegion", "Adding bandwidths", value = FALSE),
                                         p('Download the reflectance spectrum at selected sensor resolution with main plant traits:'),
                                         downloadButton("downloadData_sim", "Download spectrum"),
                                         br(),
                                         br(),
                                         p('The selected reflectance spectrum was generated based on plant traits and canopy parameters showed in the following tables: '),
                                         
                                         strong("Plant traits values used for the selected leaf model:"),
                                         br(),
                                         br(),
                                         tableOutput("lut_table.leaf"),
                                         strong("Plant traits and geometric parameters values used for the selected canopy model:"),
                                         br(),
                                         br(),
                                         tableOutput("lut_table.canopy"),
                                         # strong("LUT for leaf + canopy model"),
                                         br(),
                                         br(),
                                         #tableOutput("lut_table.sim")
                                ),         # end Tab Interactive panel 
                                
                                ### Second Pannel
                                tabPanel("Inputs",
                                         h3("PROSPECT model"),
                                         p('Plant leaf reflectance and transmittance are calculated from 400 nm to 2500 nm (1 nm step) with the following parameters:'),
                                         tableOutput("prospect_table"),
                                         br(),
                                         p(HTML('<b>PROSPECT-D </b>model uses the following parameters: Cab, Car, Anth, CBrown, alpha, EWT and LMA')),
                                         p(HTML('<b>PROSPECT-PRO </b>model uses the following parameters: Cab, Car, Anth, CBrown, alpha, EWT, leaf proteins and CBC')),
                                         p('In PROSPECT-PRO model, LMA = 0, when leaf proteins and CBC is provided'),
                                         h3("Liberty model"),
                                         p('Leaf radiative transfer model designed for conifer needles'),
                                         p(HTML('<b>Liberty </b>model simulates plant leaf reflectance and transmittance  with the following parameters:')),
                                         
                                         tableOutput("Liberty_table"),
                                         br(),
                                         p('Note that absorption coefficients used in Liberty model are based on the original values from Dawson et al. (1998). Improved specific absorption coefficients are available from later work by Di Vittorio (2009).'),
                                         
                                         h3("FLUSPECT model"),
                                         p(' Leaf radiative transfer model designed for adding fluorescence emision.'),
                                         tableOutput("fluspect_table"), 
                                         h3("fourSAILH model"),
                                         p(HTML('<b>fourSAILH </b>model is based on a version provided by	Wout Verhoef et al. (2007)')),
                                         p("original version downloadable at ", 
                                           tags$a(href="http://teledetection.ipgp.jussieu.fr/prosail/", "http://teledetection.ipgp.jussieu.fr/prosail/")),
                                         tableOutput("foursailh_table"),
                                         h3("INFORM model"),
                                         p(HTML('<b>INFORM </b>model simulates the bi-directional reflectance of forest stands between 400 and 2500 nm. ')),
                                         p(HTML('This model integrates the  Ground coverage <b>(FLIM)</b> model and computes the average horizontal area of a single tree crown in hectare (k) corrected by the factor (adapt=0.6)')),
                                         tableOutput("inform_table"),
                                         
                                         h3("fourSAILH2 model"),
                                         p(' must be complete ....'),
                                         
                                ),# end Tab Inputs panel 
                                # end Tab Inputs panel 
                                tabPanel("References", 
                                         br(),
                                         "If you use ToolSRTM package, please cite the following references:",
                                         br(),
                                         h3("PROSPECT model"),
                                         
                                         p("Jacquemoud, S., Baret, F., 1990. PROSPECT: a model of leaf optical properties spectra. Remote Sens. Environ. 34, 75–91. ", 
                                           tags$a(href="https://doi.org/10.1016/0034-4257 (90)90100-Z", "https://doi.org/10.1016/0034-4257 (90)90100-Z")),
                                         p("Jacquemoud S, Baret F, Hanocq J-F, 1992. Modeling spectral and bidirectional soil reflectance. Remote Sensing of Environment, 41, 123–132.", 
                                           tags$a(href="https://doi.org/10.1016/0034-4257(92)90072-R", "https://doi.org/10.1016/0034-4257(92)90072-R")),
                                         
                                         p("Féret J-B, Gitelson AA, Noble SD & Jacquemoud S, 2017. PROSPECT-D: Towards modeling leaf optical properties through a complete lifecycle. Remote Sensing of Environment, 193, 204–215.", 
                                           tags$a(href="https://doi.org/10.1016/j.rse.2017.03.004", "https://doi.org/10.1016/j.rse.2017.03.004")),
                                         p("Féret, J.B., Berger, K., de Boissieu, F., Malenovský, Z., 2021. PROSPECT-PRO for estimating content of nitrogen-containing leaf proteins and other carbon-based constituents. Remote Sens. Environ. 252.", 
                                           tags$a(href="https://doi.org/10.1016/j.rse.2020.112173", "https://doi.org/10.1016/j.rse.2020.112173")),
                                         
                                         h3("Liberty model"),
                                         
                                         p("Dawson, T. P., Curran, P. J., & Plummer, S. E. (1998). LIBERTY—Modeling the Effects of Leaf Biochemical Concentration on Reflectance Spectra. Remote Sensing of Environment, 65(1), 50–60.", 
                                           tags$a(href="https://doi.org/10.1016/S0034-4257(98)00007-8", "https://doi.org/10.1016/S0034-4257(98)00007-8")),
                                         p("Di Vittorio, A. V. (2009). Enhancing a leaf radiative transfer model to estimate concentrations and in vivo specific absorption coefficients of total carotenoids and chlorophylls a and b from single-needle reflectance and transmittance. Remote Sensing of Environment, 113(9), 1948–1966.", 
                                           tags$a(href="https://doi.org/10.1016/j.rse.2009.05.002", "https://doi.org/10.1016/j.rse.2009.05.002")),
                                         
                                         h3("FLUSPECT model"),
                                         
                                         p("Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016. Fluspect-B: A model for leaf fluorescence, reflectance and transmittancespectra. 
                                           Remote Sens. Environ. 186, 596?615.", 
                                           tags$a(href="https://doi:10.1016/j.rse.2016.09.017", "https://doi:10.1016/j.rse.2016.09.017")),
                                         
                                         h3("fourSAIL & fourSAIL-2 models"),
                                         p("Verhoef W & Bach H, 2007. Coupled soil–leaf-canopy and atmosphere radiative transfer modeling to simulate hyperspectral multi-angular surface reflectance and TOA radiance data. Remote Sensing of Environment, 109:166-182.", 
                                           tags$a(href="https://doi:10.1016/j.rse.2006.12.013", "https://doi:10.1016/j.rse.2006.12.013")),
                                         
                                         p("Verhoef W, Jia L, Xiao Q & Su Z, 2007. Unified optical-thermal four-stream radiative transfer theory for homogeneous vegetation canopies. IEEE Transactions in Geosciences and Remote Sensing, 45:1808–1822.", 
                                           tags$a(href=" https://doi.org/10.1109/TGRS.2007.895844", " https://doi.org/10.1109/TGRS.2007.895844")),
                                         
                                         p("Jacquemoud S, Verhoef W, Baret F, Bacour C, Zarco-Tejada PJ, Asner GP, François C & Ustin SL, 2009. PROSPECT+ SAIL models: A review of use for vegetation characterization. Remote Sensing of Environment, 113:S56–S66. ", 
                                           tags$a(href="https://doi.org/doi:10.1016/j.rse.2008.01.026", "https://doi.org/doi:10.1016/j.rse.2008.01.026")),
                                         
                                         h3("Invertible Forest Reflectance  Model"),
                                         
                                         p('Atzberger, C., 2000. Development of an Invertible Forest Reflectance Model: The INFOR- model.'),
                                         
                                         tags$p("Atzberger, C. (2000). Development of an invertible forest reflectance model: The INFOR-Model. In Buchroithner (Ed.), A decade of trans-european remote sensing cooperation. Proceedings of the 20th EARSeL Symposium Dresden, Germany, 14.-16. June 2000 (pp. 39-44)."),
                                         #br(),
                                         p("Schlerf, M., Atzberger, C., 2006. Inversion of a forest reflectance model to estimate structural canopy variables from hyperspectral remote sensing data. Remote Sens. Environ. 100, 281–294. ", 
                                           tags$a(href="https://doi.org/10.1016/j.rse.2005.10.006", "https://doi.org/10.1016/j.rse.2005.10.006")),
                                         #br(),
                                         #p("Schlerf, M., Atzberger, C., 2006. Inversion of a forest reflectance model to estimate structural canopy variables from hyperspectral remote sensing data. Remote Sens. Environ. 100, 281–294.",
                                         # tags$a(href='https://doi.org/10.1016/j.rse.2005.10.006.','https://doi.org/10.1016/j.rse.2005.10.006.')),
                                         #p(" ", 
                                         #tags$a(href="", "")),
                                         br(),
                                         
                                ),# end Tab Reference panel 
                                
                                tabPanel("About ToolsRTM", "",
                                         br(),
                                         br(),
                                         'ToolsRTM package integrates the main radiative transfer (RT) models for simulating canopy reflectance at  hyperspectral and Sentinel-2 scales.',
                                         br(),
                                         br(),
                                         'This package uses several functions for estimating plant traits, spectral indices and useful functions for generating time series, validation of the predictions with field observations ....',
                                         br(),
                                         br(),
                                         "Manual is available at ",
                                         tags$a(href="https://toolsrtm-tutorial.readthedocs.io/en/latest/", 
                                                "ReadTheDocs"),
                                         ".",
                                         br(),
                                         br(),
                                         tags$figure(
                                           tags$img(src = "rtm_sims.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")
                                           
                                         ), 
                                         strong("Fig 1."),'Reflectance simulation using several RT models.' ,
                                        
                                         #tags$img(src = "rtm_sims.png", height = "450px", width = "500px"),
                                         #htmltools::img(src = "rtm_sims.png"),
                                         
                                         br(),
                                         h3("Install ToolsRTM"),
                                         p('ToolsRTM is avalaible on gitlab, so you can install using the R console:'),
                                         code('git_url <- "https://gitlab.com/caminoccg/toolsrtm.git"'),
                                         br(),
                                         code('devtools::install_gitl(git_url, auth_token = "Student.Acces.Tokens")'),
                                         br(),
                                         p(''),
                                         p('Student.Acces.Tokens is provided by the C.Camino'),
                                         
                                         h3("Citation"),
                                         p('If you use ToolSRTM, please cite the following references:'),
                                         p('Camino et al., (2023). Quantifying Vcmax and physiological plant traits by coupling Sentinel-2 imagery with biophysical models over EU flux towers for tracking forest disturbances.'),
                                         
                                         h3("R package dependencies"),
                                         br(),
                                         strong('hsdar: '),
                                         textOutput("citation_hsdar"),
                                         
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         
                                         #textInput("Install_package", "", value = "...."),
                                         
                                ), # end Tab Reference panel 
                                
                              ) # close tabsetPanel
                              
                            ) # Close mainPanel
                          ) # Close sideBarLayout
                 ), # Close the Interactive ToolsRTM tab panel
                 
               #  ), ###end Menu    
      
      
      ##############################################################################################
      ###############################################################################################
      
      tabPanel("SCOPE model",
               # Add a sidebar layout
               sidebarLayout(
                 # Add a sidebar panel
                 sidebarPanel(
                   # Add a little information about ebird data
                   class = "sidebar",
                   style = "height: 90vh; overflow-y: auto;", 
                   # Leaf Model selection
                   h3('Select RT models:'),
                   div(id = "upper-panel",
                       selectInput("Leaf_scope", label = "Leaf Model:",
                                   choices = c("FLUSPECT-B", "FLUSPECT-B-Cx"))
                       
                   ),
                   div(id = "lower-panel",
                       # Canopy Model selection
                       selectInput("Canopy_scope", label = "Canopy Model:",
                                   choices = c("fourSAILH"))
                   ),
                   
                   h4('leaf parameters :'),
                   # Leaf Model parameters (1)
  
                   conditionalPanel(
                     condition = "input.Leaf_scope == 'FLUSPECT-B'",
                     sliderInput("fqe_fd_scope", "Fluorescence quantum efficiency (fqe)", min = 0.01, max = 0.05, value = 0.02, step = 0.005),
                     sliderInput("Cab_fd_scope", "Chlorophyll content (μg cm-2)", min = 0.01, max = 100, value = 50, step = 0.1),
                     sliderInput("Car_fd_scope", "Carotenoid content (μg cm-2)", min = 0.01, max = 40, value = 20, step = 0.1),
                     sliderInput("Cs_fd_scope", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                     sliderInput("Cx_fd_scope", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                     
                     sliderInput("EWT_fd_scope", "Water content (g cm-2)", min = 0.001, max = 0.05, value = 0.01, step = 0.005),
                     sliderInput("LMA_fd_scope", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.005),
                     sliderInput("N_fd_scope", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1)
                     
                    ),
                   
                   # Leaf Model parameters (4)
                   conditionalPanel(
                     condition = "input.Leaf_scope == 'FLUSPECT-B-Cx'",
                     sliderInput("fqe_fp_scope", "Fluorescence quantum efficiency (fqe)", min = 0.01, max = 0.05, value = 0.02, step = 0.005),
                     sliderInput("Cab_fp_scope", "Chlorophyll content (μg cm-2)", min = 0.01, max = 100, value = 50, step = 0.1),
                     sliderInput("Car_fp_scope", "Carotenoid content (μg cm-2)", min = 0.01, max = 40, value = 20, step = 0.1),
                     sliderInput("Anth_fp_scope", "Anthocyanin content (μg cm-2)", min = 0.01, max = 7, value = 2, step = 0.1),
                     sliderInput("Cs_fp_scope", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                     sliderInput("Cx_fp_scope", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                     sliderInput("EWT_fp_scope", "Water content (g cm-2)", min = 0, max = 0.05, value = 0.01, step = 0.005),
                     sliderInput("LMA_fp_scope", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.005),
                     sliderInput("N_fp_scope", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1)
                   ),
                   
                   
                   h4('leaf biochemical parameters :'),
                   # Leaf Model parameters (1)
                   
                   sliderInput("Vcmax_scope", "Vcmax (µmol m-2s-1)", min = 0, max = 175, value = 80, step = 0.5),
                   sliderInput("BaLBerrySlope", "Ball-Berry Slope", min = 0, max = 8, value = 8, step = 0.1),
                   sliderInput("BaLBerry0", "BaLBerry 0", min = 0, max = 0.2, value = 0.1, step = 0.001),
                   
                   h4('Meteorological parameters :'),
                   # Leaf Model parameters (1)
                   
                   sliderInput("Rn_scope", "broadband incoming shortwave radiation (W m-2)", min = 400, max = 1000, value = 600, step = 10),
                   sliderInput("Rli_scope", "broadband incoming longwave radiation (W m-2)", min = 150, max = 450, value = 300, step = 10),
                   sliderInput("Ta_scope", "air temperature", min = 20, max = 40, value = 25, step = 0.1),
                   
                   
                   h4('canopy parameters :'),
                   # Leaf Model parameters 
                   conditionalPanel(
                     condition = "input.Canopy_scope == 'fourSAILH'",
                     sliderInput("LAI_scope", "LAI (m2/m2)", min = 0, max = 8, value = 4, step = 0.1),
                     sliderInput("LIDFa_scope", "LIDFa", min = -1, max = 1, value = 0.5, step = 0.1),
                     sliderInput("LIDFb_scope", "LIDFb", min = -1, max = 1, value = 0.5, step = 0.1),
                     sliderInput("hotspot_scope", "hotspot", min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput("tts_scope", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                     sliderInput("tto_scope", "tto (deg)", min = 0, max = 90, value = 30, step = 0.2)),
           
                   tags$img(src = "JRC.png", height = "100px", width = "200px"),
                   
                 ),# Close sidebarPanel  
                 
                 mainPanel(
                   tabsetPanel(
                     ### Firs Pannel
                     tabPanel("Reflectance",
                              
                              h2("Interactive reflectance"),
                             
                              plotOutput("reflectance_plot_scope"),
                              # strong("LUT for leaf + canopy model"),
                     
                              materialSwitch(inputId = "checkbox_scope", label = "Accumulate reflectance spectra", status = "danger"),
                              
                              selectInput("sensor_scope", "Select satellite sensor:",
                                          choices = c('SCOPE','Sentinel2a','Sentinel2b',
                                                      'EnMAP','Hyperion','MODIS',
                                                      'Landsat4','Landsat5','Landsat7','Landsat8', #'ALI',
                                                      'Quickbird','RapidEye','WorldView2-4','WorldView2-8')),
                             
                              p('Download the reflectance spectrum at selected sensor resolution with main plant traits:'),
                              downloadButton("downloadData_scope", "Download reflectance"),
                              br(),
                              br(),
                              p('The selected reflectance spectra were generated based on plant traits and canopy parameters showed in the following tables: '),
                              br(),
                              br(),
                              strong("Plant traits values used for the selected leaf model:"),
                              br(),
                              br(),
                              verbatimTextOutput("lut_scope_output_leaf"),
                              strong("Plant traits and geometric parameters values used for the selected canopy model:"),
                              br(),
                              br(),
                              verbatimTextOutput("lut_scope_output_canopy"),
                              
                              #tableOutput("lut_table.sim")
                     ),         # end Tab Interactive panel 
                     
                     ### Second Pannel
                     tabPanel("Fluorescence",
                              h2("Chlorohyl fluorescence emission"),
                             
                              plotOutput("sif_plot_scope"),
                              
                              materialSwitch(inputId = "checkbox_sif", label = "Accumulate SIF spectra", status = "danger"),
                              
                              selectInput("sensor_sif", "Select satellite sensor:",
                                          choices = c('SCOPE','Sentinel2a','Sentinel2b',
                                                      'EnMAP','Hyperion','MODIS',
                                                      'Landsat4','Landsat5','Landsat7','Landsat8', #'ALI',
                                                      'Quickbird','RapidEye','WorldView2-4','WorldView2-8')),
                              p('Download the reflectance spectrum at selected sensor resolution with main plant traits:'),
                              downloadButton("downloadData_sif", "Download fluorescence"),
                              br(),
                              br(),
                              p('The selected fluorescence spectra were generated based on plant traits and canopy parameters showed in the Reflectance tab: '),
                              br(),
                              br(),
                              
                     ),# end Tab Inputs panel 
                     # end Tab Inputs panel 
                     tabPanel("Radiance", 
                              h2("Radiance profile"),
                              plotOutput("Lo_plot_scope"),
                              
                              materialSwitch(inputId = "checkbox_LO", label = "Accumulate radiance spectra", status = "danger"),
                              
                              selectInput("sensor_LO", "Select satellite sensor:",
                                          choices = c('SCOPE','Sentinel2a','Sentinel2b',
                                                      'EnMAP','Hyperion','MODIS',
                                                      'Landsat4','Landsat5','Landsat7','Landsat8', #'ALI',
                                                      'Quickbird','RapidEye','WorldView2-4','WorldView2-8')),
                              p('Download the radiance spectrum at selected sensor resolution with main plant traits:'),
                              downloadButton("downloadData_LO", "Download radiance"),
                              br(),
                              br(),
                              p('The selected radiance spectra were generated based on plant traits and canopy parameters showed in the Reflectance tab: '),
                              br(),
                              br(),
                              # Add a verbatimTextOutput for printing LUT.SCOPE
                              #verbatimTextOutput("lut_scope_output"),
                              br(),
                             #verbatimTextOutput("model_scope_outputs"),
                              br(),
                        #      verbatimTextOutput("lut_scope_output_leaf"),

                              br(),
                         #     verbatimTextOutput("lut_scope_output_param"),
                              
                              br(),
                              
                             
                     ),# end Tab Reference panel 
                     
                     tabPanel("About SCOPEinR", "",
                              wellPanel( style = "background: white",
                                         h3('SCOPEinR package'),
                                         p(HTML('<b>SCOPEinR</b> package is designed for running the Soil Canopy Observation, Photochemistry and Energy fluxes (SCOPE, Van der Tol at al., 2009, Yang et al., 2020) radiative transfer model.')),
                                         
                                         p('This package simulates reflectance and chorophyll fluorescence emission using the SCOPE model. To make inter-comparison with other main radiative transfer (RT) models is recommeded to install the ToolsRTM package. This ToolsRTM package uses several functions for simulating canopy reflectance at several spectral resolution (1nm, hyper spectral and Sentinel-2).'),
                                         
                                         "Manual is available at ",
                                         tags$a(href="https://scopeinr-tutorial.readthedocs.io/en/latest/", 
                                                "ReadTheDocs"),
                                         ".",
                                         br(),
                                         
                                         tags$figure(
                                           tags$img(src = "rtm_scope.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")
                                           
                                         ), 
                                         br(),
                                         strong("Fig 1."),'Simulated reflectance spectrum using the SCOPE model.' ,
                                         #tags$img(src = "rtm_sims.png", height = "450px", width = "500px"),
                                         #htmltools::img(src = "rtm_sims.png"),
                                         
                                         br(),
                                         br(),
                                         tags$figure(
                                           tags$img(src = "rtm_scope_sif.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")
                                           
                                         ), 
                                         br(),
                                         strong("Fig 1."),'Simulated chorohyll fluorescence emission using theSCOPE model.' ,
                                         
                                         br(),
                                         h3("Install SCOPEinR"),
                                         p('SCOPEinR is avalaible on gitlab, so you can install using the R console:'),
                                         code('git_url <- "https://gitlab.com/caminoccg/scopeinr.git"'),
                                         br(),
                                         code('devtools::install_gitl(git_url, auth_token = "Student.Acces.Tokens")'),
                                         br(),
                                         p(''),
                                         p('Student.Acces.Tokens is provided by the C.Camino'),
                                         
                                         # Add information on cranes and prompt user to use slider
                                         
                                         h3("GitLab repositories"),
                               
                                         a("SCOPEinR package", href = "https://gitlab.com/caminoccg/scopeinr"),
                                         br(),
                                         h3("Citation"),
                                         p(HTML('If you use <b>SCOPEinR</b> package, please cite the following references:')),
                                         
                                         p('Camino et al., (2023). Quantifying Vcmax and physiological plant traits by coupling Sentinel-2 imagery with biophysical models over EU flux towers for tracking forest disturbances'),
                                         
                                         div(tags$img(src = "JRC.png", height = "100px", width = "300px",align="left")
                                             , style="text-align: left;"),
                                         
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         
                              ) # end well panel
                              
                              #textInput("Install_package", "", value = "...."),
                              
                     ), # end Tab Reference panel 
                     
                   ) # close tabsetPanel
                   
                 ) # Close mainPanel
               ) # Close sideBarLayout
      ), # end tab panel
##############################################################################################
###############################################################################################

                 navbarMenu("LUT generator",
                            tabPanel("Uniform LUT generator", # Open a second tabPanel for generating LUTs
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("model_selected_leaf", "Select a leaf RT Model:",
                                                     choices = c("PROSPECT-PRO", "PROSPECT-D", "Liberty","FLUSPECT-B","FLUSPECT-B-Cx")),
                                         selectInput("model_selected_canopy", "Select a canopy RT Model:",
                                                     choices = c('fourSAILH','INFORM','fourSAILH2')),
                                         selectInput("sensor_selected", "Select sensor for resampling resolution:",
                                                     choices = c('RTM','Sentinel2a','Sentinel2b','Landsat4','Landsat5','Landsat7','Landsat8')),
                                         numericInput("n_samples", "Number of samples:", value = 100,min = 10,max = 20000),
                                         p('maximum LUT to 20,000 simulations'),
                                         numericInput("seed", "ramdom seed:", value = 1234),
                                         p('Random seed parameter for repeatability'),
                                         checkboxInput("savetable", "accumulative LUTs", value = FALSE),
                                         # Add a progress bar
                                         #progressBar(id = "my_progress", value = 0, total = 100, display_pct = TRUE)
                                         
                                       ),
                                       mainPanel(
                                         h3('Simulated average reflectance'),
                                         plotOutput("plot"),
                                         #uiOutput("histogram_ui"),
                                         actionButton("buttonLUT", "Generate the LUT"),
                                         downloadButton("downloadData", "Download")
                                       )## close the mainPanel
                                     ) ## close the sidebarLayout
                                     
                            ) # Close the tab panel
                            
                 ), # Close the navbarMenu
                 
##############################################################################################
###############################################################################################

                 tabPanel(title = "Inversion Methods",
                          # Add a sidebar layout 
                          sidebarLayout(
                            # Add a sidebar panel
                            sidebarPanel(
                              # Add information on cranes and prompt user to use slider
                              h3("Fungus detection at SR2"),
                              dateRangeInput("date_range", "Select a time period: ",
                                             start = "2020-01-01", end = "2020-12-31",
                                             min = "2015-06-23", max = Sys.Date(),
                                             format = "yyyy-mm-dd"),
                              
                              sliderInput("cloud", "Maximum Cloud Coverage (%):", min = 0, max = 100, value = 2),
                              h3("Base Map"),
                              selectInput("baseMap", "Select a base map:",
                                          c("Sentinel-2" = "Sentinel2",
                                            "Google Maps" = "Roadmap",
                                            "Landsat" = "Landsat7",
                                            "MODIS" = "MODIS")),
                              hr(),
                              hr(),
                              tags$div(
                                id = "coords",
                                tags$strong("Coordinates:"),
                                verbatimTextOutput("click"),
                                #tags$p("Latitude:")
                              ),
                              hr(),
                              tags$div(
                                id = "chart",
                                style = "width: 100%; height: 400px;"
                              ),
                              hr(),
                              tags$div(
                                id = "legend",
                                style = "width: 100%; height: 30px;"
                              )
                              
                            ), # Close sidebarPanel
                            mainPanel( # open main panel
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Hybrid ML", 
                                           h3("Interactive Model"),
                                           br(),
                                           
                                           materialSwitch(inputId = "checkbox_spectra", label = "Get spectral information", status = "danger"),
                                           p('(If checkbox is activate, click on the map for generating time series ....)'),
                                           br(),
                                           leafletOutput("map", width = "700px", height = "400px")),
                                  tabPanel("Spectral plot",
                                           h3("Interactive Sentinel-2 Spectrum"),
                                           plotOutput("spectral_chart"),
                                           br(),
                                           strong("Fig 1."),'Median Spectral reflectance for the selected period.' ,),
                                  tabPanel("Spectral Time Series",
                                           h3("Interactive spectral time series"),
                                           plotOutput("serie_plot_ndvi"),
                                           br(),
                                           br(),
                                           strong("Fig 1."),' Sentinel-2 Time serie for NDVI and sensitive plant traits.',
                                           br(),
                                           br(),
                                           div(id = "upper-panel",
                                               selectInput("serie_indices", label = "Select the plant trait:",
                                                           choices = c("NDVI","Vcmax","Chlorophyll content", "Carotenoid content",'LAI' )) )
                                           
                                  ), ### close tab panel
                                  tabPanel("ML info",
                                           h3("Fungus infected area in vicinity of the ICOS San Rossore-2 flux"),
                                           p('The IT-SR2 site is located inside the Parco Regionale Migliarino, San Rossore, 
                            Massaciuccoli, west of Pisa, Italy. The forest ecosystem is a very homogeneous and dense stand of 
                            stone pine (Pinus Pinea), in addition only scarce occurrences of holm oak (Quercus ilex), 
                            European ash (Fraxinus excelsior), and alder (Alnus glutinosa)'),
                                           br(),
                                           p('In the surrounding of this ICOS flux tower, an outbreak of the parasite fungus fomes fomentarius
                            was confirmed in the summer of 2020. We use the San Rossore area to verify the effectiveness of the 
                            proposed FHM system based on RT models, as an exploratory phase, for that, 
                            we explored the early detection capability of key plant physiological traits and red-edge spectral indicators'),
                                           br(),
                                           br()
                                           
                                           
                                  ), ### close tab panel
                                  tabPanel("ML references",
                                           h3("About the early detection"),
                                           tags$li(tags$span("Delimitation of the study area")),
                                           tags$li(tags$span("Spatial anomalies")),
                                           tags$li(tags$span("Plant trait predictions")),
                                           
                                           br(),
                                           br(),
                                           br()
                                           
                                           
                                  ) ### close tab panel
                                )  ## close tabsetPanel
                                
                                
                              ) # Close mainPanel
                            ) # Close mainPanel
                          ) # Close sideBarLayout
                 ), # Close the breeding distribution tab panel
                 # Add a References tab panel, linking each Reference to its source.


##############################################################################################
###############################################################################################


##############################################################################################
###############################################################################################

navbarMenu("References",
           tabPanel("About ToolsRTM",
                    wellPanel( style = "background: white",
                               h3('ToolsRTM package'),
                               p(HTML('<b>ToolsRTM</b> package integrates the main radiative transfer (RT) models for simulating canopy reflectance at  hyperspectral and Sentinel-2 scales.')),
                               
                               'This package uses several functions for estimating plant traits, spectral indices and useful functions for generating time series, validation of the predictions with field observations ....',
                               br(),
                               br(),
                               "Manual is available at ",
                               tags$a(href="https://toolsrtm-tutorial.readthedocs.io/en/latest/", 
                                      "ReadTheDocs"),
                               ".",
                               br(),
                               
                               tags$figure(
                                 tags$img(src = "rtm_sims.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")
                                 
                               ), 
                               br(),
                               strong("Fig 1."),'Reflectance simulation using several RT models.' ,
                               #tags$img(src = "rtm_sims.png", height = "450px", width = "500px"),
                               #htmltools::img(src = "rtm_sims.png"),
                               
                               br(),
                               h3("Install ToolsRTM"),
                               p('ToolsRTM is avalaible on gitlab, so you can install using the R console:'),
                               code('git_url <- "https://gitlab.com/caminoccg/toolsrtm.git"'),
                               br(),
                               code('devtools::install_gitl(git_url, auth_token = "Student.Acces.Tokens")'),
                               br(),
                               p(''),
                               p('Student.Acces.Tokens is provided by the C.Camino'),
                               
                               h3("R package dependencies"),
                               br(),
                               br(),
                               br(),
                               br(),
                               #textInput("citation", strong("check the R package dependecies:")),
                               #verbatimTextOutput("value"),
                               
                               
                               # Add information on cranes and prompt user to use slider
                               
                               h3("GitLab repositories"),
                               
                               # Link to the Gitlab website to direct folks to actual analyses
                               a("gitLab ToolsRTM app ", href = "https://gitlab.com/caminoccg/toolsrtm_app"),
                               br(), # linebreak
                               br(), # linebreak
                               a("ToolsRTM package", href = "https://gitlab.com/caminoccg/toolsrtm"),
                               br(),
                               h3("Citation"),
                               p(HTML('If you use <b>ToolsRTM</b> package, please cite the following references:')),
                               
                               p('Camino et al., (2023). Quantifying Vcmax and physiological plant traits by coupling Sentinel-2 imagery with biophysical models over EU flux towers for tracking forest disturbances'),
                               
                               div(tags$img(src = "JRC.png", height = "100px", width = "300px",align="left")
                                   , style="text-align: left;"),
                               
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               
                    ) # end well panel
           ), # end tab panel
           
           tabPanel("About SCOPEinR",
                    wellPanel( style = "background: white",
                            
                               h3('SCOPEinR package'),
                               p(HTML('<b>SCOPEinR</b> package is designed for running the Soil Canopy Observation, Photochemistry and Energy fluxes (SCOPE, Van der Tol at al., 2009, Yang et al., 2020) radiative transfer model.')),
                               
                               p('This package simulates reflectance and chorophyll fluorescence emission using the SCOPE model. To make inter-comparison with other main radiative transfer (RT) models is recommeded to install the ToolsRTM package. This ToolsRTM package uses several functions for simulating canopy reflectance at several spectral resolution (1nm, hyper spectral and Sentinel-2).'),
                             
                            
                               "Manual is available at ",
                               tags$a(href="https://scopeinr-tutorial.readthedocs.io/en/latest/", 
                                      "ReadTheDocs"),
                               ".",
                               br(),
                               
                               tags$figure(
                                 tags$img(src = "rtm_scope.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")
                                 
                               ), 
                               br(),
                               strong("Fig 1."),'Simulated reflectance spectrum using the SCOPE model.' ,
                               #tags$img(src = "rtm_sims.png", height = "450px", width = "500px"),
                               #htmltools::img(src = "rtm_sims.png"),
                               
                               br(),
                               br(),
                               tags$figure(
                                 tags$img(src = "rtm_scope_sif.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")
                                 
                               ), 
                               br(),
                               strong("Fig 1."),'Simulated chorohyll fluorescence emission using theSCOPE model.' ,
                               
                               br(),
                               h3("Install SCOPEinR"),
                               p('SCOPEinR is avalaible on gitlab, so you can install using the R console:'),
                               code('git_url <- "https://gitlab.com/caminoccg/scopeinr.git"'),
                               br(),
                               code('devtools::install_gitl(git_url, auth_token = "Student.Acces.Tokens")'),
                               br(),
                               p(''),
                               p('Student.Acces.Tokens is provided by the C.Camino'),
                               
                               # Add information on cranes and prompt user to use slider
                               
                               h3("GitLab repositories"),
                               
                               a("SCOPEinR package", href = "https://gitlab.com/caminoccg/scopeinr"),
                               br(),
                               h3("Citation"),
                               p(HTML('If you use <b>SCOPEinR</b> package, please cite the following references:')),
                               
                               p('Camino et al., (2023). Quantifying Vcmax and physiological plant traits by coupling Sentinel-2 imagery with biophysical models over EU flux towers for tracking forest disturbances'),
                               
                               div(tags$img(src = "JRC.png", height = "100px", width = "300px",align="left")
                                   , style="text-align: left;"),
                             
                               br(),
                               br(),
                               br(),
                               br(),
                               br(),
                               
                    ) # end well panel
           ), # end tab panel
           tabPanel("About Online reflectance simulator",
                    wellPanel( style = "background: white",
                               h3('Online reflectance simulator'),
                               p(HTML('<b>Online reflectance simulator</b> enables run in forward mode main radiative transfer (RT) models for simulating canopy reflectance at  hyperspectral and seveal setellites (e.g., Sentinel-2, Landsat7,8, MODIS, ...')),
                               
                               p(HTML('The online RT simulator integrates two R packages with R dependencies associated to other R packages:')),
                               
                               p(HTML('- The <b>ToolsRTM</b> package with several functions for LUT generation, estimating plant traits, spectral indices and useful functions for generating time series, validation of the predictions with field observations ...')),
                               p(HTML('- The <b>SCOPEinR</b> package with all the RT models included in the SCOPE model neeeded for estimating reflectance, fluorescence and more function for integrating a balance energy model.')),
               
                               h3("Citation"),
                               p(HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> package, please cite the following references:')),
                               p('Camino et al., (2023). Quantifying Vcmax and physiological plant traits by coupling Sentinel-2 imagery with biophysical models over EU flux towers for tracking forest disturbances'),
                               br(),
                             
                               br(),
                               div(tags$img(src = "JRC.png", height = "100px", width = "300px",align="left")
                                   , style="text-align: left;"),
                               br(),
                               br(),
                               br(),
                               br(),
                               
                    )
           ) # end tab panel
  )# end navbarMenu

) ##  # Close the references menu

