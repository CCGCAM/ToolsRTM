


#  Define UI for the app -----------------------------------------

ui <- navbarPage("Online reflectance simulator",theme = shinytheme("flatly"),


# 1) Tab Interactive ToolsRTM -----------------------------------------

                 tabPanel(title = "Interactive ToolsRTM",
                          # Beta Version Notice
                          tags$div(
                            style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                            "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                          ),

                          # Add a sidebar layout
                          sidebarLayout(
                            # Add a sidebar panel
                            sidebarPanel(
                              # Add a little information about ebird data
                              class = "sidebar",
                              style = "height: 90vh; overflow-y: auto;",
                              # Leaf Model selection
                              h3('Select RT models:'),
                              #tags$img(src= "images/JRC.png", height = 80, width = 200),
                              div(id = "upper-panel",
                                  selectInput("leaf_model", label = "Leaf Model:",
                                              choices = c("PROSPECT-PRO","PROSPECT-D", "Liberty", "FLUSPECT-B", "FLUSPECT-B-Cx"))

                              ),
                              div(id = "lower-panel",
                                  # Canopy Model selection
                                  selectInput("canopy_model", label = "Canopy Model:",
                                              choices = c("fourSAILH", "INFORM")) # fourSAILH2
                              ),
                              h4('leaf parameters :'),
                              # Leaf Model parameters (1)
                              # Checkbox to show/hide leaf parameters
                              #checkboxInput("show_leaf_params", "Show leaf parameters", value = FALSE),

                              conditionalPanel(
                                condition = "input.leaf_model == 'PROSPECT-PRO'",
                                sliderInput("Cab", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = 45, step = 0.5),
                                sliderInput("Car", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 20, value = 12, step = 0.5),
                                sliderInput("Anth", HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 7, value = 0, step = 0.5),
                                sliderInput("Cbrown", "Cbrown", min = 0, max = 1, value = 0.2, step = 0.05),
                                sliderInput("N", "mesophyll structure parameter", min = 1, max = 4, value = 2.45, step = 0.1),
                                sliderInput("EWT", HTML("Water content (g cm<sup>-2</sup>)"), min = 0.0001, max = 0.05, value = 0.01, step = 0.005),
                            #    sliderInput("LMA", "LMA (g cm-2)", min = 0, max = 0.05, value = 0.005),
                              #  sliderInput("alpha", "alpha", min = 0, max = 60, value = 40, step = 0.1),
                                sliderInput("Prot", HTML("Proteins (g cm<sup>-2</sup>)"), min = 0.0001, max = 0.03, value = 0.012, step = 0.005),
                                sliderInput("CBC",  HTML("Carbon-based constituent (g cm<sup>-2</sup>)"), min = 0, max = 0.03, value = 0.010, step = 0.005)

                              ),

                              # Leaf Model parameters (2)
                              conditionalPanel(
                                condition = "input.leaf_model == 'PROSPECT-D'",

                                sliderInput("Cab_d",  HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = 20, step = 0.1),
                                sliderInput("Car_d", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 20, value = 2.5, step = 0.1),
                                sliderInput("Anth_d",  HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 7, value = 2, step = 0.1),
                                sliderInput("Cbrown_d", "Cbrown", min = 0, max = 1, value = 0.2, step = 0.1),
                                sliderInput("N_d", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_d", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.009, step = 0.002),
                                sliderInput("LMA_d", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.009, step = 0.005)
                              #  sliderInput("alpha_d", "alpha", min = 0, max = 60, value = 20, step = 0.1)

                              ),
                              # Leaf Model parameters (3)
                              conditionalPanel(
                                condition = "input.leaf_model == 'Liberty'",
                                sliderInput("Cab_l", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 60, value = 40, step = 1),
                                sliderInput("EWT_l", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.009, step = 0.001),
                                sliderInput("lign_cell", "Lignin and cellulose content", min = 10, max = 80, value = 40, step = 0.1),
                                sliderInput("Nitrogen", HTML("Nitrogen content (g m<sup>-2</sup>)"), min = 0.3, max = 2, value = 1, step = 0.1),
                                sliderInput("cell_d", HTML("Cell diameter  (m<sup>-6</sup>)"), min = 20, max = 200, value = 58, step = 0.1),
                                sliderInput("inter_c", "Intercellular air space", min = 0.01, max = 0.1, value = 0.045, step = 0.005),
                                sliderInput("baseline_abs", "baseline", min = 0.0004, max = 0.0006, value = 0.0004),
                                sliderInput("leaf_thick", "leaf thickness", min = 1, max = 10, value = 1.6, step = 0.1),
                                sliderInput("albino_abs", "Albino absorption", min = 0, max = 4, value = 2, step = 0.1)


                              ),
                              # Leaf Model parameters (4)
                              conditionalPanel(
                                condition = "input.leaf_model == 'FLUSPECT-B'",

                                sliderInput("fqe_fd", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = 0.02, step = 0.005),
                                sliderInput("Cab_fd", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = 50, step = 0.1),
                                sliderInput("Car_fd", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 20, value = 20, step = 0.1),
                                sliderInput("Cs_fd", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                                sliderInput("Cx_fd", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                                sliderInput("N_fd", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_fd", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.01, step = 0.005),
                                sliderInput("LMA_fd", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.01)

                              ),

                              # Leaf Model parameters (4)
                              conditionalPanel(
                                condition = "input.leaf_model == 'FLUSPECT-B-Cx'",
                                sliderInput("fqe_fp", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = 0.02, step = 0.005),
                                sliderInput("Cab_fp", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = 50, step = 0.1),
                                sliderInput("Car_fp", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 40, value = 20, step = 0.1),
                                sliderInput("Anth_fp",  HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 7, value = 2, step = 0.1),
                                sliderInput("Cs_fp", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                                sliderInput("Cx_fp", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                                sliderInput("N_fp", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                                sliderInput("EWT_fp", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.01, step = 0.005),
                                sliderInput("LMA_fp", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.005),
                                sliderInput("Prot_fp", HTML("Proteins (g cm<sup>-2</sup>)"), min = 0, max = 0.03, value = 0.012, step = 0.005),
                                sliderInput("CBC_fp", HTML("Carbon-based Constituent  (g cm<sup>-2</sup>)"), min = 0, max = 0.03, value = 0.010, step = 0.005)
                              ),

                              h4('canopy parameters :'),
                              # Leaf Model parameters
                              conditionalPanel(
                                condition = "input.canopy_model == 'fourSAILH'",
                                sliderInput("LAI", HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 8, value = 4, step = 0.1),
                                sliderInput("LIDFa", "LIDFa (°)", min = 0, max = 90, value = 30, step = 0.1),
                                sliderInput("hotspot", "hotspot", min = 0, max = 1, value = 0.5, step = 0.01),
                                sliderInput("tts", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                                sliderInput("tto", "tto (deg)", min = 0, max = 90, value = 30, step = 0.2),
                                sliderInput("psi", "psi (deg)", min = 0, max = 180, value = 0, step = 0.5),
                                sliderInput("psoil", "soil factor", min = 0, max = 1, value = 0.35,step = 0.1)

                              ),
                              conditionalPanel(
                                condition = "input.canopy_model == 'INFORM'",
                                sliderInput("LAI_", HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 8, value = 4, step = 0.1),
                                sliderInput("LIDFa_", "LIDFa (deg)", min = 0, max = 90, value = 30, step = 0.1),
                                sliderInput("hotspot_", "hotspot", min = 0, max = 1, value = 0.1, step = 0.1),
                                sliderInput("tts_", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                                sliderInput("tto_", "tto (deg)", min = 0, max = 90, value = 45, step = 0.2),
                                sliderInput("psi_", "psi (deg)", min = 0, max = 180, value = 0, step = 0.5),
                                sliderInput("LAIu_", HTML("understory LAI (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0.05, max = 2, value = 0.5,step = 0.015),
                                sliderInput("cd_", "Crown diameter (m)", min = 0, max = 10, value = 4.5, step = 0.5),
                                sliderInput("sd_",  HTML("Stem density (ha<sup>-1</sup>)"), min = 0, max = 3000, value = 2500, step = 1.5),
                                sliderInput("h_", "tree height (m)", min = 1, max = 40, value = 20, step = 0.1),
                                sliderInput("skyl_", "skyl (fraction-Fixed to 0.1)", min = 0.001, max = 0.4, value = 0.1, step = 0.005),
                                sliderInput("psoil_", "soil factor", min = 0, max = 1, value = 0.1, step = 0.1)
                              ),

                              conditionalPanel(
                                condition = "input.canopy_model == 'fourSAILH2'",
                                sliderInput("p1_s1", HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 8, value = 4, step = 0.1),
                                sliderInput("p2_s2", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                                sliderInput("p3_s2", "tto (deg)", min = 0, max = 90, value = 45, step = 0.2),
                                sliderInput("p4_s2", "psi (deg)", min = 0, max = 180, value = 0, step = 0.5)
                              ),
                             # tags$img(src = "images/JRC.png", height = "100px", width = "200px"),

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
                                         DT::dataTableOutput("lut_table.leaf"),
                                        # tableOutput("lut_table.leaf"),
                                         strong("Plant traits and geometric parameters values used for the selected canopy model:"),
                                         br(),
                                         br(),
                                        DT::dataTableOutput("lut_table.canopy"),
                                         #tableOutput("lut_table.canopy"),
                                         # strong("LUT for leaf + canopy model"),
                                         br(),
                                         br(),
                                         #tableOutput("lut_table.sim")
                                        #DT::dataTableOutput("lut_table.sim"),
                                ),         # end Tab Interactive panel

                                ### Second Pannel
                                tabPanel("Inputs",
                                         h4("PROSPECT model"),
                                         p(style = "text-align: justify;",'Plant leaf reflectance and transmittance are calculated from 400 nm to 2500 nm (1 nm step) with the following parameters:'),
                                         tableOutput("prospect_table"),
                                         br(),
                                         p(style = "text-align: justify;",HTML('<b>PROSPECT-D </b>model uses the following parameters: Cab, Car, Anth, CBrown, alpha, EWT and LMA')),
                                         p(style = "text-align: justify;",HTML('<b>PROSPECT-PRO </b>model uses the following parameters: Cab, Car, Anth, CBrown, alpha, EWT, leaf proteins and CBC')),
                                         p('In PROSPECT-PRO model, LMA = 0, when leaf proteins and CBC is provided'),

                                         h4("Liberty model"),

                                         p(style = "text-align: justify;",'Leaf radiative transfer model designed for conifer needles'),
                                         p(style = "text-align: justify;",HTML('<b>Liberty </b>model simulates plant leaf reflectance and transmittance  with the following parameters:')),

                                         tableOutput("Liberty_table"),
                                         br(),
                                         p(style = "text-align: justify;",'Note that absorption coefficients used in Liberty model are based on the original values from Dawson et al. (1998). Improved specific absorption coefficients are available from later work by Di Vittorio (2009).'),

                                         h4("FLUSPECT model"),

                                         p(style = "text-align: justify;",'Leaf radiative transfer model designed for adding fluorescence emision.'),
                                         tableOutput("fluspect_table"),
                                         h4("fourSAILH model"),

                                         p(HTML('<b>fourSAILH </b>model is based on a version provided by	Wout Verhoef et al. (2007)')),
                                         p("original version downloadable at ",
                                           tags$a(href="http://teledetection.ipgp.jussieu.fr/prosail/", "http://teledetection.ipgp.jussieu.fr/prosail/")),
                                         tableOutput("foursailh_table"),
                                         h4("INFORM model"),

                                         p(style = "text-align: justify;",HTML('<b>INFORM </b>model simulates the bi-directional reflectance of forest stands between 400 and 2500 nm. ')),
                                         p(style = "text-align: justify;",HTML('This model integrates the  Ground coverage <b>(FLIM)</b> model and computes the average horizontal area of a single tree crown in hectare (k) corrected by the factor (adapt=0.6)')),
                                         tableOutput("inform_table"),



                                ),# end Tab Inputs panel
                                tabPanel("Functions",
                                         # Introduction to the module
                                         h4("Main funtions integrated in the ToolsRTM Package"),

                                         p(style = "text-align: justify;", "This module utilizes key functions integrated into the ", strong("ToolsRTM"), " package to process and analyze vegetation data derived from Sentinel-2 satellite imagery. These functions are critical for estimating spectral indices, training machine learning models, and predicting plant traits such as gross primary production (GPP) and other physiological traits."),

                                         # Section for fourSAIL
                                         h4("1. fourSAIL"),
                                         p(style = "text-align: justify;", "The ", code("fourSAIL"), " function models canopy reflectance using various leaf models such as ", strong("PROSPECT-D"), ", ", strong("PROSPECT-PRO"), ", ", strong("Liberty"), ", ", strong("FLUSPECT-B-Cx"), ", and ", strong("FLUSPECT-B"), ". It calculates the Bidirectional Reflectance Function (BRF) based on vegetation parameters, soil reflectance, and canopy structure, providing accurate reflectance simulations at Sentinel-2 resolution."),
                                         # Key parameters explanation
                                         p(strong("Key Parameters:")),
                                         # List of function arguments
                                         tags$ul(
                                           tags$li(strong("inputLUT:"), " A Look-Up Table (LUT) containing the distribution of biophysical parameters used as input parameters in the model."),
                                           tags$li(strong("rsoil:"), " A numeric value representing the soil reflectance, which affects the overall canopy reflectance calculation."),
                                           tags$li(strong("LeafModel:"), " The version of the leaf model used; options include 'PROSPECT-PRO', 'PROSPECT-D', 'Liberty', 'FLUSPECT-B' and 'FLUSPECT-B-Cx'. The default is 'PROSPECT-PRO'."),
                                           tags$li(strong("spectrum.all:"), " A boolean value indicating whether to calculate the full spectrum. Set to TRUE for PROSPECT and Liberty models (400-2500 nm) and FALSE for SPART and Fluspect models (400-2400 nm).")
                                         ),
                                         p(strong("Usage:")),
                                         # Example usage of the function
                                         p(style = "text-align: justify;", "Example of using the ", code("fourSAIL"), " function:"),
                                         code("reflectance <- fourSAIL(inputLUT = myLUTTable, rsoil = 0.2, LeafModel = 'PROSPECT-PRO', spectrum.all = TRUE);"),
                                         br(),


                                         # Section for INFORM
                                         h4("2. INFORM"),
                                         p(style = "text-align: justify;", "The ", code("INFORM"), " function models forest canopy reflectance, with a focus on analyzing disturbances such as bark beetle outbreaks. It integrates spectral data from leaf and soil models to assess forest health. This function simulates reflectance across the visible, near-infrared, and shortwave infrared wavelengths for various platforms such as Sentinel-2 and Landsat."),
                                         p(style = "text-align: justify;", em("Reference: Camino et al. (2025). Understanding bark beetle outbreaks using INFORM canopy models in forest ecosystems.")),

                                         # Key parameters explanation
                                         p(strong("Key Parameters:")),
                                         # List of function arguments
                                         tags$ul(
                                           tags$li(strong("inputLUT:"), " A Look-Up Table (LUT) that specifies the distribution of biophysical parameters used as input parameters in the model."),
                                           tags$li(strong("rsoil:"), " A numeric value representing the soil reflectance, which influences the overall reflectance calculation."),
                                           tags$li(strong("LeafModel:"), " The version of the leaf model to be used; options include 'PROSPECT-PRO','PROSPECT-D', 'Liberty', 'FLUSPECT-B' and 'FLUSPECT-B-Cx'."),
                                          ),
                                         p(strong("Usage:")),
                                         # Example usage of the function
                                         p(style = "text-align: justify;", "Example of using the ", code("INFORM"), " function:"),
                                         code("reflectance <- INFORM(inputLUT = LUT_[1,], rsoil = rsoil_, LeafModel = 'PROSPECT-PRO');"),
                                         br(),

                                         # Section for Compute_BRF
                                         h4("3. Compute_BRF"),
                                         p(style = "text-align: justify;", "The ", code("Compute_BRF"), " function computes the Bidirectional Reflectance Function (BRF) using shortwave infrared (SWIR) spectral data. It models the interaction of light with the canopy, incorporating both soil and leaf scattering properties. This function provides detailed simulations across the spectrum from visible to shortwave infrared."),
                                         p(style = "text-align: justify;", "This function is particularly beneficial for models that require SWIR wavelengths (up to 2400 nm), as utilized in ", code("FLUSPECT-B"), " and ", code("FLUSPECT-B-Cx"), "."),

                                         # Key parameters explanation for Compute_BRF
                                         p(strong("Key Parameters:")),
                                         tags$ul(
                                           tags$li(strong("rdot:"), " Hemispherical-directional reflectance factor in the viewing direction, representing how much light is reflected by the canopy in a specified direction."),
                                           tags$li(strong("rsot:"), " Bi-directional reflectance factor, which represents the reflectance of the soil surface from multiple angles."),
                                           tags$li(strong("tts:"), " A numeric value indicating the viewing angle (top of the canopy)."),
                                           tags$li(strong("data.light:"), " A data frame containing the  direct solar radiation and diffuse solar radiation required for the BRF calculation; If this is NULL, we use a default values")
                                         ),

                                         p(strong("Usage:")),
                                         # Example usage of the Compute_BRF function
                                         p(style = "text-align: justify;", "Example of using the ", code("Compute_BRF"), " function:"),
                                         code("reflectance_values <- foursail(inputLUT = LUT_[1,], rsoil = rsoil_, LeafModel = 'PROSPECT-PRO');"),
                                         br(),
                                         code("rdot <- reflectance_values[[1]];"),
                                         br(),
                                         code("rsot <- reflectance_values[[2]];"),
                                         br(),
                                         code("brf_values <- Compute_BRF(rdot = rdot, rsot = rsot, tts = LUT_[1, 'tts'], data.light = dataSpec_PDB);"),
                                         br(),

                                         # Section for get.spectral.convolution.rfl
                                         h4("4. get.spectral.convolution.rfl"),
                                         p(style = "text-align: justify;", "The ", code("get.spectral.convolution.rfl"), " function resamples reflectance data to align with specific satellite bands (e.g., Landsat-8, Landsat-5, Sentinel-2). It takes high-resolution reflectance data and convolves it to match the spectral response of various sensors, making the data compatible for different satellite imagery applications."),
                                         p(style = "text-align: justify;", "This function ensures that reflectance values from simulations are accurately aligned with the spectral bands of the satellite platform."),

                                         # Key parameters explanation for get.spectral.convolution.rfl
                                         p(strong("Key Parameters:")),
                                         tags$ul(
                                           tags$li(strong("df:"), " A data frame containing the high-resolution reflectance data to be resampled."),
                                           tags$li(strong("sensor.i:"), " The sensor specification to which the data will be aligned."),
                                           tags$li(strong("get.plots:"), " A boolean value indicating whether to generate plots of the convolution results (default is FALSE).")
                                         ),


                                         # Section for Sensor Specification
                                         h4("Sensor Specification (sensor.i)"),
                                         p(style = "text-align: justify;", "In ToolsRTM, the available sensor specifications for aligning reflectance data include:"),

                                         # List of supported sensors
                                         tags$ul(
                                           tags$li(strong("Sentinel2A.MSI:"), " Sentinel-2A MultiSpectral Instrument."),
                                           tags$li(strong("Sentinel2B.MSI:"), " Sentinel-2B MultiSpectral Instrument."),
                                           tags$li(strong("Sentinel3A.OLCI:"), " Sentinel-3A Ocean and Land Colour Instrument."),
                                           tags$li(strong("Sentinel3B.OLCI:"), " Sentinel-3B Ocean and Land Colour Instrument."),
                                           tags$li(strong("LANDSAT4.TM:"), " Landsat 4 Thematic Mapper."),
                                           tags$li(strong("LANDSAT5.TM:"), " Landsat 5 Thematic Mapper."),
                                           tags$li(strong("LANDSAT7.ETM:"), " Landsat 7 Enhanced Thematic Mapper."),
                                           tags$li(strong("LANDSAT8.OLI:"), " Landsat 8 Operational Land Imager."),
                                           tags$li(strong("TerraAqua.MODIS:"), " MODIS onboard Terra and Aqua satellites.")
                                         ),

                                         p(strong("Usage:")),
                                         # Example usage of the get.spectral.convolution.rfl function
                                         p(style = "text-align: justify;", "Example of using the ", code("get.spectral.convolution.rfl"), " function:"),
                                         code("df_resampled <- get.spectral.convolution.rfl(df = Spec.simula, sensor.i = ToolsRTM::Sentinel2A.MSI, get.plots = FALSE);"),
                                         br(),# Section for get.spectral.convolution.rfl
                                         h4("Citation"),
                                         p(style = "text-align: justify;",HTML('If you use these models with <b>ToolsRTM</b> package, please cite the following references:')),

                                         p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                         p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                         p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),


                                ), #end TabPanel

                                tabPanel("References for RTMs",

                                         br(),
                                         "If you utilize any of the RTM models within the ToolsRTM package, please be sure to cite the following references:",

                                         h4("PROSPECT model"),

                                         p(style = "text-align: justify;","Jacquemoud, S., Baret, F., 1990. PROSPECT: a model of leaf optical properties spectra. Remote Sens. Environ. 34, 75–91. ",
                                           br(),
                                           tags$a(href="https://doi.org/10.1016/0034-4257 (90)90100-Z", "https://doi.org/10.1016/0034-4257 (90)90100-Z")),
                                         p(style = "text-align: justify;","Jacquemoud S, Baret F, Hanocq J-F, 1992. Modeling spectral and bidirectional soil reflectance. Remote Sensing of Environment, 41, 123–132.",
                                         br(),
                                         tags$a(href="https://doi.org/10.1016/0034-4257(92)90072-R", "https://doi.org/10.1016/0034-4257(92)90072-R")),

                                         p(style = "text-align: justify;","Féret J-B, Gitelson AA, Noble SD & Jacquemoud S, 2017. PROSPECT-D: Towards modeling leaf optical properties through a complete lifecycle. Remote Sensing of Environment, 193, 204–215.",
                                           tags$a(href="https://doi.org/10.1016/j.rse.2017.03.004", "https://doi.org/10.1016/j.rse.2017.03.004")),
                                         p(style = "text-align: justify;","Féret, J.B., Berger, K., de Boissieu, F., Malenovský, Z., 2021. PROSPECT-PRO for estimating content of nitrogen-containing leaf proteins and other carbon-based constituents. Remote Sens. Environ. 252.",
                                           tags$a(href="https://doi.org/10.1016/j.rse.2020.112173", "https://doi.org/10.1016/j.rse.2020.112173")),

                                         h4("Liberty model"),

                                         p(style = "text-align: justify;","Dawson, T. P., Curran, P. J., & Plummer, S. E. (1998). LIBERTY—Modeling the Effects of Leaf Biochemical Concentration on Reflectance Spectra. Remote Sensing of Environment, 65(1), 50–60.",
                                           tags$a(href="https://doi.org/10.1016/S0034-4257(98)00007-8", "https://doi.org/10.1016/S0034-4257(98)00007-8")),
                                         p(style = "text-align: justify;","Di Vittorio, A. V. (2009). Enhancing a leaf radiative transfer model to estimate concentrations and in vivo specific absorption coefficients of total carotenoids and chlorophylls a and b from single-needle reflectance and transmittance. Remote Sensing of Environment, 113(9), 1948–1966.",
                                           tags$a(href="https://doi.org/10.1016/j.rse.2009.05.002", "https://doi.org/10.1016/j.rse.2009.05.002")),

                                         h4("FLUSPECT model"),

                                         p(style = "text-align: justify;","Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016. Fluspect-B: A model for leaf fluorescence, reflectance and transmittancespectra.
                                           Remote Sens. Environ. 186, 596?615.",
                                           tags$a(href="https://doi:10.1016/j.rse.2016.09.017", "https://doi:10.1016/j.rse.2016.09.017")),

                                         h4("fourSAIL & fourSAIL-2 models"),

                                         p(style = "text-align: justify;","Verhoef W & Bach H, 2007. Coupled soil–leaf-canopy and atmosphere radiative transfer modeling to simulate hyperspectral multi-angular surface reflectance and TOA radiance data. Remote Sensing of Environment, 109:166-182.",
                                           tags$a(href="https://doi:10.1016/j.rse.2006.12.013", "https://doi:10.1016/j.rse.2006.12.013")),

                                         p(style = "text-align: justify;","Verhoef W, Jia L, Xiao Q & Su Z, 2007. Unified optical-thermal four-stream radiative transfer theory for homogeneous vegetation canopies. IEEE Transactions in Geosciences and Remote Sensing, 45:1808–1822.",
                                           tags$a(href=" https://doi.org/10.1109/TGRS.2007.895844", " https://doi.org/10.1109/TGRS.2007.895844")),

                                         p(style = "text-align: justify;","Jacquemoud S, Verhoef W, Baret F, Bacour C, Zarco-Tejada PJ, Asner GP, François C & Ustin SL, 2009. PROSPECT+ SAIL models: A review of use for vegetation characterization. Remote Sensing of Environment, 113:S56–S66. ",
                                           tags$a(href="https://doi.org/doi:10.1016/j.rse.2008.01.026", "https://doi.org/doi:10.1016/j.rse.2008.01.026")),

                                         h4("Invertible Forest Reflectance  Model"),

                                         p(style = "text-align: justify;",'Atzberger, C., 2000. Development of an Invertible Forest Reflectance Model: The INFOR- model.'),

                                         tags$p("Atzberger, C. (2000). Development of an invertible forest reflectance model: The INFOR-Model. In Buchroithner (Ed.), A decade of trans-european remote sensing cooperation. Proceedings of the 20th EARSeL Symposium Dresden, Germany, 14.-16. June 2000 (pp. 39-44)."),
                                         #br(),
                                         p(style = "text-align: justify;","Schlerf, M., Atzberger, C., 2006. Inversion of a forest reflectance model to estimate structural canopy variables from hyperspectral remote sensing data. Remote Sens. Environ. 100, 281–294. ",
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
                                         p(style = "text-align: justify;",HTML('In this online RT-Simulator, we integrate the <strong>ToolsRTM</strong> package to simulate canopy reflectance using the primary radiative transfer (RT) models.
                                         This package allows for the rescaling of spectral resolution to accommodate various scales, including hyperspectral, Landsat and Sentinel-2.')),
                                         br(),

                                         p(style = "text-align: justify;",HTML('The <strong>ToolsRTM</strong> package is a vital component of the ToolsRTM Simulator app, which relies on two key R packages to ensure optimal functionality.
                                         These packages are essential for the efficient simulation and execution of functions within the server.')),
                                         br(),
                                         p(style = "text-align: justify;",HTML('<strong>ToolsRTM</strong> Package: This comprehensive package consolidates a variety of RT models for simulating reflectance at the Top of Canopy (TOC).
                                         It features several canopy models, including fourSAIL, fourSAIL2, and INFORM, as well as various leaf models. Additionally, the <strong>ToolsRTM</strong> package incorporates the Soil-Plant-Atmosphere Radiative Transfer (SPART) model, which simulates Top-of-Atmosphere (TOA) reflectance by integrating atmospheric parameters.
                                         SPART utilizes three computationally efficient sub-models—BSM for soil, PROSAIL for vegetation canopies, and SMAC for atmospheric effects—ensuring
                                         accurate simulation of directional TOA observations.')),
                                         br(),
                                         "Manual is available at ",
                                         tags$a(href="https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_e851c6b2/Notebooks/R/ToolsRTM/ToolsRTM.html",
                                                "ReadTheDocs"),
                                         ".",
                                         br(),
                                         br(),
                                         tags$figure(
                                           tags$img(src = "images/rtm_sims.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")

                                         ),
                                         strong("Fig 1."),'Reflectance simulation using several RT models.' ,

                                         #tags$img(src = "images/rtm_sims.png", height = "450px", width = "500px"),
                                         #htmltools::img(src = "images/rtm_sims.png"),

                                         br(),
                                         h4("Install ToolsRTM"),
                                         p(style = "text-align: justify;",HTML('<strong>ToolsRTM</strong> is avalaible on gitlab, so you can install using the R console:')),

                                         code('install.packages("toolsrtm-main.tar.gz",repos = NULL,type = "source")'),

                                         h4("Citation"),

                                         p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> packages, please cite the following references:')),

                                         p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                           <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                         p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                         p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),
                                         br(),


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

# 2) Tab SPART model -----------------------------------------


      tabPanel("SPART model",
               # Beta Version Notice
               tags$div(
                 style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                 "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
               ),
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
                       # Canopy Model selection
                       selectInput("atmos", label = "Atmospheric Model:",
                                   choices = c("SPART"))
                   ),
                   div(id = "middle-panel",
                       selectInput("leaf_spart", label = "Leaf Model:",
                                   choices = c("PROSPECT-PRO"))

                   ),

                   div(id = "lower-panel",
                       # Canopy Model selection
                       selectInput("canopy_spart", label = "Canopy Model:",
                                   choices = c("fourSAILH"))
                   ),

                   h4('Atmospheric parameters :'),
                   Parameter = c('Pa','aot550', 'uo3','uh2o','alt_m','Pa0'),
                   Description = c('Air pressure', 'AOT at 550 nm', 'Ozone content ','Water vapour','altitude','sea level air pressure'),
                   # Leaf Model parameters (1)
                   condition = "input.atmos == 'SPART'",
                   sliderInput("Pa_spart", "Air pressure", min = 400, max = 1300, value = 1000, step = 10),
                   sliderInput("aot550_spart", "AOT at 550 nm", min = 0, max = 2.5, value = 0.3246, step = 0.1),
                   sliderInput("uo3_spart", "Ozone content", min = 0, max = 5, value = 0.3480, step = 0.1),
                   sliderInput("uh2o_spart", "Water vapour", min = 0, max = 5, value = 1.4116, step = 0.1),

                   #  sliderInput("alt_m_spart", "altitude", min = 0, max = 600, value = 100, step = 10),
                   #sliderInput("Pa0_spart", "sea level air pressure", min = 999, max = 1020, value = 1013.25, step = 0.1)

                   h4('canopy parameters :'),
                   # Leaf Model parameters
                   conditionalPanel(
                     condition = "input.canopy_spart == 'fourSAILH'",
                     sliderInput("LAI_spart",  HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 8, value = 4, step = 0.1),
                     sliderInput("LIDFa_spart", "LIDFa (deg)", min = 0.10, max = 90, value = 0.5, step = 0.1),
                     sliderInput("hotspot_spart", "hotspot", min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput("tts_spart", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                     sliderInput("tto_spart", "tto (deg)", min = 0, max = 90, value = 30, step = 0.2),
                     sliderInput("psi_spart", "psi (deg)", min = 0, max = 180, value = 45, step = 0.5)

                   ),
                   h4('leaf parameters :'),
                   # Leaf Model parameters (1)

                   conditionalPanel(
                     condition = "input.leaf_spart == 'PROSPECT-PRO'",
                     sliderInput("Cab_spart",HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0.01, max = 100, value = 50, step = 0.1),
                     sliderInput("Car_spart", HTML("Carotenoids content (μg cm<sup>-2</sup>)"), min = 0, max = 10, value = 2.5, step = 0.1),
                     sliderInput("Anth_spart", HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 7, value = 2, step = 0.1),
                     sliderInput("Cbrown_spart", "Cbrown", min = 0, max = 1, value = 0.2, step = 0.1),
                     sliderInput("N_spart", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1),
                     sliderInput("EWT_spart", HTML("Water content (g cm<sup>-2</sup>)"), min = 0.001, max = 0.05, value = 0.01, step = 0.005),
                     sliderInput("Prot_spart",HTML("Proteins (g cm<sup>-2</sup>)"), min = 0.0001, max = 0.03, value = 0.012, step = 0.005),
                     sliderInput("CBC_spart", HTML("Carbon-based constituent (g cm<sup>-2</sup>)"), min = 0, max = 0.03, value = 0.010, step = 0.005)

                   ),

                   #    tags$img(src = "images/JRC.png", height = "100px", width = "200px"),

                 ),# Close sidebarPanel

                 mainPanel(
                   tabsetPanel(
                     ### Firs Pannel
                     tabPanel("Reflectance",

                              h2("Interactive reflectance"),
                              plotOutput("reflectance_plot_spart"),


                              # strong("LUT for leaf + canopy model"),

                            # materialSwitch(inputId = "checkbox_scope", label = "Accumulate reflectance spectra", status = "danger"),

                              selectInput("sensor_spart", "Select satellite sensor:",
                                          choices = c('Sentinel2a','Sentinel2b',
                                                      'MODIS','Landsat-4','Landsat-5','Landsat-7','Landsat-8')),

                              p('Download the reflectance spectrum at selected sensor resolution with main plant traits:'),
                              downloadButton("downloadData_spart", "Download reflectance"),
                              br(),
                              br(),
                            # Conditional panel to show tables after the plot is rendered
                            conditionalPanel(
                              condition = "output.reflectance_plot_spart != null", # Show after plot is rendered
                              p('The selected reflectance spectra were generated based on plant traits and canopy parameters showed in the following tables: '),

                              p("Plant traits values used for the selected leaf model:"),
                             # tableOutput("lut_spart_output_leaf"),
                              DT::dataTableOutput("lut_spart_output_leaf"),

                              p("Structural, viewing angles and geometric values used for the selected canopy model:"),
                              #tableOutput("lut_spart_output_canopy"),
                              DT::dataTableOutput("lut_spart_output_canopy"),
                              strong("Atmospheric parameters:"),
                              DT::dataTableOutput("lut_spart_output_atmo")
                              #tableOutput("lut_spart_output_atmo")
                            )


                              #tableOutput("lut_table.sim")
                     ),         # end Tab Interactive panel

                     tabPanel("Functions",
                              # Introduction to the module
                              h4("Main funtions integrated in the ToolsRTM Package"),

                              p(style = "text-align: justify;", "This module leverages essential functions from the ", strong("ToolsRTM"), " package to simulate and analyze vegetation data. It particularly focuses on the ", strong("SPART"), " model, which integrates soil-plant-atmosphere radiative transfer for vegetation monitoring using satellite data."),

                              # Section for SPART
                              h4("1. SPART Model"),
                              p(style = "text-align: justify;", "The ", code("SPART"), "function allow us to simulate the SPART model stands for Soil-Plant-Atmosphere Radiative Transfer, a computationally efficient model for simulating satellite measurements across the solar spectrum. It uses three sub-models: ", strong("BSM (soil)"), ", ", strong("PROSAIL (vegetation canopy)"), ", and ", strong("SMAC (atmosphere)"), " and couples them using the four-stream theory and adding method."),
                              p(style = "text-align: justify;", "SPART accurately simulates Top of Atmosphere (TOA) spectral observations, considering all major effects like sun-observer geometries and non-Lambertian surface reflectance. This model is particularly useful for analyzing Sentinel-2 imagery and other satellite data, providing accurate simulations of canopy reflectance and atmosphere interaction."),
                              #br(),

                              # Key parameters explanation
                              p(strong("Key Parameters:")),
                              # List of function arguments
                              tags$ul(
                                tags$li(strong("inputLUT:"), " A Look-Up Table (LUT) containing the distribution of biophysical parameters for modeling."),
                                tags$li(strong("optipar:"), " Optical parameters and reflection indices from radiative transfer models (RTMs)."),
                                tags$li(strong("CanopyModel:"), " The canopy model used; default is 'fourSAIL'."),
                                tags$li(strong("LeafModel:"), " The leaf model to be applied; 'PROSPECT-PRO' is commonly used."),
                                tags$li(strong("sensor.i:"), " The sensor specification for data alignment; available options include 'Sentinel2A.MSI', 'Sentinel2B.MSI', 'Sentinel3A.OLCI', 'Sentinel3B.OLCI', 'LANDSAT4.TM', 'LANDSAT5.TM', 'LANDSAT7.ETM', 'LANDSAT8.OLI', 'TerraAqua.MODIS'."),
                                tags$li(strong("df.irradiance:"), " A dataframe of direct and diffuse irradiance values for clear conditions; if NULL, default values will be used."),
                                tags$li(strong("get.plots:"), " A boolean indicating whether to generate plots; set to TRUE to visualize the results.")
                              ),
                              p(strong("Supported sensors:")),
                              # List of supported sensors
                              tags$ul(
                                tags$li(strong("Sentinel2A.MSI:"), " Sentinel-2A MultiSpectral Instrument."),
                                tags$li(strong("Sentinel2B.MSI:"), " Sentinel-2B MultiSpectral Instrument."),
                                tags$li(strong("Sentinel3A.OLCI:"), " Sentinel-3A Ocean and Land Colour Instrument."),
                                tags$li(strong("Sentinel3B.OLCI:"), " Sentinel-3B Ocean and Land Colour Instrument."),
                                tags$li(strong("LANDSAT4.TM:"), " Landsat 4 Thematic Mapper."),
                                tags$li(strong("LANDSAT5.TM:"), " Landsat 5 Thematic Mapper."),
                                tags$li(strong("LANDSAT7.ETM:"), " Landsat 7 Enhanced Thematic Mapper."),
                                tags$li(strong("LANDSAT8.OLI:"), " Landsat 8 Operational Land Imager."),
                                tags$li(strong("TerraAqua.MODIS:"), " MODIS onboard Terra and Aqua satellites.")
                              ),
                              p(strong("Supported Optical parameters :")),
                              tags$ul(
                                tags$li(strong("optipar2017.ProspectD:"), " For the PROSPECT-PRO model."),
                                tags$li(strong("optipar:"), " For the PROSPECT-D model."),
                                tags$li(strong("optipar2021.Pro.CX:"), " For the FLUSPECT-B-Cx model."),
                                tags$li(strong("optipar2020.prospectD.BSM2019:"), " For the FLUSPECT-B model with BSM 2019 parameters.")
                              ),

                              p(strong("Usage:")),
                              # Example usage of the SPART function
                              p(style = "text-align: justify;", "Example of using the ", code("SPART"), " function:"),
                              code("data.spart <- SPART(inputLUT = LUT[1,], optipar = optipar2017.ProspectD"),
                              br(),
                              code("CanopyModel = 'fourSAIL',LeafModel = 'PROSPECT-PRO',"),
                              br(),
                              code("df.irradiance = NULL,sensor.i = 'Sentinel2A.MSI'  "),
                              br(),

                              # Reference to SPART model documentation
                              p("More information is available at "),
                              a("SPART GitHub", href = "https://github.com/peiqiyang/SPART"),
                              br(),
                              # Section for SPART
                              h4("Citation"),
                              p(style = "text-align: justify;", em("Yang et al. (2020). SPART: Soil-Plant-Atmosphere Radiative Transfer Model for remote sensing applications.")),


                              p(style = "text-align: justify;",HTML('If you use this model with <b>ToolsRTM</b> package, please cite the following references:')),

                              p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                              p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                              p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),


                     ), #end TabPanel

                     tabPanel("About SPART", "",
                              wellPanel( style = "background: white",
                                         h4('ToolsRTM package'),
                                         p(style = "text-align: justify;", HTML('<b>ToolsRTM</b> package is also designed for running the soil-plant-atmosphere radiative transfer (SPART) model (Yang et. al, 2020) for satellite measurements in the solar spectrum.')),

                                         p(style = "text-align: justify;", 'The SPART model uses three computationally efficient RTMs for soil (BSM), vegetation canopies (PROSAIL) and atmosphere (SMAC), respectively. The sub-models are coupled by using the four-stream theory and the adding method.'),
                                         p(style = "text-align: justify;", 'The resulting `Soil-Plant-Atmosphere Radiative Transfer model simulates directional TOA spectral observations, with all major effects included, such as sun-observer geometries and non-Lambertia reflectance of the land surface.'),


                                         p("More information is available at "),
                                         a("SPART Github", href = "https://github.com/peiqiyang/SPART"),
                                         br(),
                                         h4('SPART model: '),

                                         tableOutput("spart_table"),
                                        br(),

                                         tags$figure(
                                           tags$img(src = "images/rtm_spart.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")

                                         ),
                                         br(),
                                         strong("Fig 1."),'Simulated reflectance spectrum using the SPART model.' ,

                                         br(),
                                         h4("Install ToolsRTM"),

                                         p('ToolsRTM is avalaible on gitlab, so you can install using the R console:'),

                                         code('install.packages("toolsrtm-main.tar.gz",repos = NULL,type = "source")'),


                                         # Add information on cranes and prompt user to use slider

                                         h4("GitLab repositories"),

                                         a("ToolsRTM package", href = "https://gitlab.com/caminoccg/toolsrtm"),
                                         br(),

                                         h4("Citation"),

                                         p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> packages, please cite the following references:')),

                                         p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                         p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                        p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),


                                         h4("Citation for SPART model"),

                                         p('Yang, P., van der Tol, C., Yin, T., & Verhoef, W. (2020). The SPART model: A soil-plant-atmosphere radiative transfer model for satellite
                                          measurements in the solar spectrum. Remote Sensing of Environment, 247, 111870.'),

                                         p('For the details of the radiative transfer modelling'),

                                         p('Yang, P., Verhoef, W., & van der Tol, C. (2017). The mSCOPE model: A simple adaptation to the SCOPE model to describe reflectance,
                                           fluorescence and photosynthesis of vertically heterogeneous canopies.Remote sensing of environment, 201, 1-11.'),
                                        #    div(tags$img(src = "images/JRC.png", height = "100px", width = "300px",align="left")
                                        #       , style="text-align: left;"),



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


# 3) Tab SCOPE model -----------------------------------------

      tabPanel("SCOPE model",
               # Beta Version Notice
               tags$div(
                 style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                 "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
               ),
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
                     sliderInput("Cab_fd_scope",  HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0.01, max = 100, value = 50, step = 0.1),
                     sliderInput("Car_fd_scope",  HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0.01, max = 40, value = 20, step = 0.1),
                     sliderInput("Cs_fd_scope", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                     sliderInput("Cx_fd_scope", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),

                     sliderInput("EWT_fd_scope",  HTML("Water content (g cm<sup>-2</sup>)"), min = 0.001, max = 0.05, value = 0.01, step = 0.005),
                     sliderInput("LMA_fd_scope", HTML("Dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.005),
                     sliderInput("N_fd_scope", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1)

                    ),

                   # Leaf Model parameters (4)
                   conditionalPanel(
                     condition = "input.Leaf_scope == 'FLUSPECT-B-Cx'",
                     sliderInput("fqe_fp_scope", "Fluorescence quantum efficiency (fqe)", min = 0.01, max = 0.05, value = 0.02, step = 0.005),
                     sliderInput("Cab_fp_scope",  HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0.01, max = 100, value = 50, step = 0.1),
                     sliderInput("Car_fp_scope",  HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0.01, max = 40, value = 20, step = 0.1),
                     sliderInput("Anth_fp_scope", HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0.01, max = 7, value = 2, step = 0.1),
                     sliderInput("Cs_fp_scope", "Leaf Senescence", min = 0, max = 1, value = 0.01),
                     sliderInput("Cx_fp_scope", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1, value = 0.1),
                     sliderInput("EWT_fp_scope", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.01, step = 0.005),
                     sliderInput("LMA_fp_scope",  HTML("Dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = 0.005),
                     sliderInput("N_fp_scope", "mesophyll structure parameter", min = 1, max = 4, value = 2.5, step = 0.1)
                   ),


                   h4('leaf biochemical parameters :'),
                   # Leaf Model parameters (1)

                   sliderInput("Vcmax_scope", HTML("Vcmax (μmol m<sup>-2</sup>s<sup>-1</sup>)"), min = 0, max = 175, value = 80, step = 0.5),
                   sliderInput("BaLBerrySlope", "Ball-Berry Slope", min = 0, max = 8, value = 8, step = 0.1),
                   sliderInput("BaLBerry0", "BaLBerry 0", min = 0, max = 0.2, value = 0.1, step = 0.001),

                   h4('Meteorological parameters :'),
                   # Leaf Model parameters (1)

                   sliderInput("Rn_scope",  HTML("broadband incoming shortwave radiation (Watt m<sup>-2</sup>)"), min = 400, max = 1000, value = 600, step = 10),
                   sliderInput("Rli_scope", HTML("broadband incoming longwave radiation (Watt m<sup>-2</sup>)"), min = 150, max = 450, value = 300, step = 10),
                   sliderInput("Ta_scope", HTML("Air temperature (deg)"), min = 20, max = 40, value = 25, step = 0.1),


                   h4('canopy parameters :'),
                   # Leaf Model parameters
                   conditionalPanel(
                     condition = "input.Canopy_scope == 'fourSAILH'",
                     sliderInput("LAI_scope",  HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 8, value = 4, step = 0.1),
                     sliderInput("LIDFa_scope", "LIDFa", min = -1, max = 1, value = 0.5, step = 0.1),
                     sliderInput("LIDFb_scope", "LIDFb", min = -1, max = 1, value = 0.5, step = 0.1),
                     sliderInput("hotspot_scope", "hotspot", min = 0, max = 1, value = 0.5, step = 0.01),
                     sliderInput("tts_scope", "tts (deg)", min = 0, max = 90, value = 0, step = 0.1),
                     sliderInput("tto_scope", "tto (deg)", min = 0, max = 90, value = 30, step = 0.2)),

               #    tags$img(src = "images/JRC.png", height = "100px", width = "200px"),

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
                              #uiOutput("plotReady_tableReady_scope"),
                              strong("Plant traits values used for the selected leaf model:"),
                              br(),
                              br(),
                              #verbatimTextOutput("lut_scope_output_leaf"),
                              # Output for the DataTable
                              DT::dataTableOutput("lut_table_scope_leaf"),
                              strong("Plant traits and geometric parameters values used for the selected canopy model:"),
                              br(),
                              br(),
                              DT::dataTableOutput("lut_table_scope_canopy"),
                              #verbatimTextOutput("lut_scope_output_canopy"),
                              br(),
                              br(),
                              DT::dataTableOutput("lut_table_scope_bioleaf"),

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
                     tabPanel("Functions",
                              # Introduction to the module
                              h4("Main funtions integrated in the SCOPEinR Package"),

                              p(style = "text-align: justify;", "This module utilizes key functions integrated into the ", strong("SCOPEinR"), " package to process and analyze vegetation data derived from Sentinel-2 satellite imagery. These functions are critical for estimating spectral indices, training machine learning models, and predicting plant traits such as gross primary production (GPP) and other physiological traits."),

                              # getLUTfromRanges
                              h4("1. getLUT.SCOPE"),
                              # Section for getMLmodel.withRetrain
                              p(style = "text-align: justify;", "The ", code("getLUT.SCOPE"), " function generates a Look-Up Tables (LUTs) adaptaed to SCOPE model. It requires a table of inputs specifying the ranges of variables and allows users
                      to define the number of LUT rows and set a seed for randomization. This function is essential for creating LUTs used in various analyses related to vegetation and canopy properties.
                      Below are the key arguments for this function:"),
                              ## Key parameters explanation
                              p(strong("Key Parameters:")),
                              # List of function arguments
                              tags$ul(
                                tags$li(strong("inputs:"), " A table with specific ranges and types of distributions for the variables."),
                                tags$li(strong("nLUT:"), " The number of rows to be generated for the LUT."),
                                tags$li(strong("setseed:"), " A seed number to control the random process. By default, it is set to 123, but you can specify a different number to ensure reproducibility.")
                              ),
                              p(strong("Usage:")),
                              # Example usage of the function
                              p(style = "text-align: justify;", "Example of using this function:"),
                              code("LUTs <- getLUT.SCOPE(inputs = SCOPEinR::inputsSCOPE, nLUT = 100, setseed = 123);"),
                              br(),
                              # Section for getSCOPE.parallel
                              h4("2. get.SCOPE.parallel"),
                              p(style = "text-align: justify;", "The ", code("get.SCOPE.parallel"), " function runs SCOPE simulations in parallel, allowing for efficient processing of large datasets. This function requires a Look-Up Table (LUT) containing various properties needed for the SCOPE model, and it offers options to customize the output, parallel processing, and the models used. Below are the key arguments for this function:"),

                              # Key parameters explanation
                              p(strong("Key Parameters:")),
                              # List of function arguments
                              tags$ul(
                                tags$li(strong("LUT:"), " Leaf, biochemistry, viewing angles, meteo, and canopy properties needed for running the SCOPE model."),
                                tags$li(strong("options.SCOPE:"), " Optical leaf properties and total irradiance. If a specific value is provided, it will replace the usual Modtran output."),
                                tags$li(strong("path.out:"), " Folder for saving the SCOPE outputs."),
                                tags$li(strong("parallel:"), " Logical, indicating whether to use parallel processing. Default is TRUE."),
                                tags$li(strong("canopy.model:"), " Selection of canopy model options available: 'fourSAIL', 'INFORM'. Default is 'fourSAIL'."),
                                tags$li(strong("leaf.model:"), " Selection of leaf model options available: 'fluspect-CX', 'fluspect-B', 'PROSPECT', 'Liberty'. Default is 'fluspect-CX'."),
                                tags$li(strong("get.outputs:"), " Specifies which variables to retrieve; 'ALL' retrieves all variables, while 'Main' retrieves only the main variables."),
                                tags$li(strong("get.plots:"), " Logical, indicating whether to plot intermediate results. Default is TRUE."),
                                tags$li(strong("get.csv:"), " Logical, indicating whether to save outputs. Default is TRUE."),
                                tags$li(strong("n.cores:"), " Integer, indicating the number of cores to use. Default is 2 if this parameter is null or missing.")
                              ),

                              p(strong("Usage:")),
                              p(strong("Usage:")),
                              p(style = "text-align: justify;", "Example of using this function:"),
                              code("db.sims <- SCOPEinR::get.SCOPE.parallel(LUT = my_LUT, options.SCOPE = my_options,"),
                              br(),
                              code(" path.out = 'outputs/', parallel = TRUE, canopy.model = 'fourSAIL', "),
                                  br(),
                              code("leaf.model = 'fluspect-CX',"),
                              br(),
                              code("get.outputs = 'Main', get.plots = TRUE, get.csv = TRUE, n.cores = 8)"),

                              h4("Citation "),
                              p(style = "text-align: justify;",HTML('If you use these functions with <b>SCOPEinR</b> package, please cite the following references:')),

                              p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                              p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                              p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),

                     ),
                     tabPanel("About SCOPEinR", "",
                              wellPanel( style = "background: white",
                                         h4('SCOPEinR package'),
                                         p(style = "text-align: justify;",HTML('<b>SCOPEinR</b> package is designed for running the Soil Canopy Observation, Photochemistry and Energy fluxes (SCOPE, Van der Tol at al., 2009, Yang et al., 2020) radiative transfer model.')),

                                         p(style = "text-align: justify;",'This package simulates reflectance and chorophyll fluorescence emission using the SCOPE model. To make inter-comparison with other main radiative transfer (RT) models is recommeded to install the ToolsRTM package. This ToolsRTM package uses several functions for simulating canopy reflectance at several spectral resolution (1nm, hyper spectral and Sentinel-2).'),

                                         "Manual is available at ",
                                         tags$a(href="https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_e851c6b2/Notebooks/R/SCOPEinR/SCOPEinR.html",
                                                "ReadTheDocs"),
                                         ".",
                                         br(),

                                         tags$figure(
                                           tags$img(src = "images/rtm_scope.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")

                                         ),
                                         br(),
                                         strong("Fig 1."),'Simulated reflectance spectrum using the SCOPE model.' ,
                                         #tags$img(src = "rtm_sims.png", height = "450px", width = "500px"),
                                         #htmltools::img(src = "rtm_sims.png"),

                                         br(),
                                         br(),
                                         tags$figure(
                                           tags$img(src = "images/rtm_scope_sif.png",height = "450px", width = "500px"),
                                           tags$figcaption("", style = "font-weight: bold;")

                                         ),
                                         br(),
                                         strong("Fig 1."),'Simulated chorohyll fluorescence emission using theSCOPE model.' ,

                                         br(),
                                         h4("Install SCOPEinR"),
                                         p('SCOPEinR is avalaible on gitlab, so you can install using the R console:'),

                                         code('install.packages("scopeinr-main.tar.gz",repos = NULL,type = "source")'),


                                         # Add information on cranes and prompt user to use slider

                                         h4("GitLab repositories"),

                                         a("SCOPEinR package", href = "https://gitlab.com/caminoccg/scopeinr"),
                                         br(),
                                         h3("Citation"),

                                         p(HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> packages, please cite the following references:')),

                                         p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                           <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                         p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                         p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),

                                         h4("Citation for SCOPE model"),

                                         p(style = "text-align: justify;",'Yang, P., Prikaziuk, E., Verhoef, W., and Van der Tol, C. 2021 "SCOPE 2.0: a model to simulate vegetated land surface
                                         fluxes and satellite signals" Geoscientific Model Development, 14, 4697–4712, https://doi.org/10.5194/gmd-14-4697-2021'),

                                         p('Van der Tol, C., W. Verhoef, J Timmermans, A Verhoef, and Z Su. 2009. “An Integrated Model of Soil-Canopy Spectral Radiances,
                                         Photosynthesis, Fluorescence, Temperature and Energy Balance.” Biogeosciences 6 (12): 3109–29, https://doi.org/10.5194/bg-6-3109-2009'),

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

# 4) Tab LUT generator  -----------------------------------------

navbarMenu("Look-up table generator",

           tabPanel("Create your Look-up table", # Open a second tabPanel for generating LUTs
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("leaf_lut", "Select a leaf RT Model:",
                                    choices = c("PROSPECT-PRO", "PROSPECT-D","Liberty","FLUSPECT-B","FLUSPECT-B-Cx")), # ,""
                        selectInput("canopy_lut", "Select a canopy RT Model:",
                                    choices = c('fourSAILH','INFORM')), ##fourSAILH2
                        selectInput("sensor_lut", "Select sensor for resampling resolution:",
                                    choices = c('RTM','Sentinel2a','Sentinel2b','Landsat4','Landsat5','Landsat7','Landsat8')),
                        numericInput("n_samples_lut", "Number of samples:", value = 25,min = 10,max = 20000),
                        p('maximum LUT to 20,000 simulations'),
                        numericInput("seed_lut", "ramdom seed:", value = 1234),
                        p('Random seed parameter for repeatability'),
                        h4(strrep("-", 60)),
                        h4('Leaf parameters:'),

                        conditionalPanel(
                          condition = "input.leaf_lut == 'PROSPECT-PRO'",
                          sliderInput("Cab2", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = c(2,60)),
                          checkboxInput("Dist_cab2", "Use a gauss distribution", value = F),

                          sliderInput("Car2", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 40, value = c(5,20)),
                          checkboxInput("Dist_car2", "Use a gauss distribution", value = TRUE),

                          sliderInput("Anth2", HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 10, value = c(0,7)),
                          checkboxInput("Dist_anth2", "Use a gauss distribution", value = TRUE),

                          sliderInput("Cbrown2", "Cbrown", min = 0, max = 1, value = c(0,1)),
                          checkboxInput("Dist_cbrown2", "Use a gauss distribution", value = FALSE),

                          sliderInput("N2", "mesophyll structure parameter", min = 1, max = 4, value = c(1.5,3)),
                          checkboxInput("Dist_n2", "Use a gauss distribution", value = FALSE),

                          sliderInput("EWT2", HTML("Water content (g cm<sup>-2</sup>)"), min = 0.0001, max = 0.05, value = c(0.01,0.03)),
                          checkboxInput("Dist_ewt2", "Use a gauss distribution", value = FALSE),

                          sliderInput("Prot2", HTML("Proteins (g cm<sup>-2</sup>)"), min = 0.0001, max = 0.03, value = c(0.0001,0.005)),
                          checkboxInput("Dist_prot2", "Use a gauss distribution", value = FALSE),

                          sliderInput("CBC2",  HTML("Carbon-based constituent (g cm<sup>-2</sup>)"), min = 0, max = 0.03, value = c(0,0.003)),
                          checkboxInput("Dist_cbc2", "Use a gauss distribution", value = FALSE),

                        ),

                        # Leaf Model parameters (4)
                        conditionalPanel(
                          condition = "input.leaf_lut == 'FLUSPECT-B'",

                          sliderInput("fqe_fd_2", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = c(0.02,0.03)),
                          checkboxInput("Dist_fqe_fd_2", "Use a gauss distribution", value = T),

                          sliderInput("Cab_fd_2", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100,  value = c(10,50)),
                          checkboxInput("Dist_cab_fd_2", "Use a gauss distribution", value = F),

                          sliderInput("Car_fd_2", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 20,  value = c(4,10)),
                          checkboxInput("Dist_car_fd_2", "Use a gauss distribution", value = T),

                          sliderInput("Cs_fd_2", "Leaf Senescence", min = 0, max = 1,  value = c(0.2,0.3)),
                          checkboxInput("Dist_cs_fd_2", "Use a gauss distribution", value = F),

                          sliderInput("Cx_fd_2", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1,  value = c(0.5,0.8)),
                          checkboxInput("Dist_cx_fd_2", "Use a gauss distribution", value = F),

                          sliderInput("N_fd_2", "mesophyll structure parameter", min = 1, max = 4,  value = c(1.5,2.5)),
                          checkboxInput("Dist_n_fd_2", "Use a gauss distribution", value = F),

                          sliderInput("EWT_fd_2", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = c(0.01,0.02)),
                          checkboxInput("Dist_ewt_fd_2", "Use a gauss distribution", value = T),

                          sliderInput("LMA_fd_2", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05,  value = c(0.02,0.04)),
                          checkboxInput("Dist_lma_fd_2", "Use a gauss distribution", value = T),


                        ),

                        # Leaf Model parameters (4)
                        conditionalPanel(
                          condition = "input.leaf_lut == 'FLUSPECT-B-Cx'",

                          sliderInput("fqe_fp_2", "Fluorescence quantum efficiency (fqe)", min = 0, max = 0.05, value = c(0.01,0.02)),
                          checkboxInput("Dist_fqe_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("Cab_fp_2", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100,  value = c(30,60)),
                          checkboxInput("Dist_cab_fp_2", "Use a gauss distribution", value = T),


                          sliderInput("Car_fp_2", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 40,  value = c(10,20)),
                          checkboxInput("Dist_car_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("Anth_fp_2",  HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 7,  value = c(0.5,6)),
                          checkboxInput("Dist_ant_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("Cs_fp_2", "Leaf Senescence", min = 0, max = 1,  value = c(0.1,0.5)),
                          checkboxInput("Dist_cs_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("Cx_fp_2", "Violaxanthin - Zeaxanthin transition status", min = 0, max = 1,  value = c(0.1,0.5)),
                          checkboxInput("Dist_cx_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("N_fp_2", "mesophyll structure parameter", min = 1, max = 4,  value = c(1.3,2.5)),
                          checkboxInput("Dist_n_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("EWT_fp_2", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05,  value = c(0.01,0.02)),
                          checkboxInput("Dist_ewt_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("LMA_fp_2", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = c(0.01,0.02)),
                          checkboxInput("Dist_lma_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("Prot_fp_2", HTML("Proteins (g cm<sup>-2</sup>)"), min = 0, max = 0.03,  value = c(0.001,0.005)),
                          checkboxInput("Dist_prot_fp_2", "Use a gauss distribution", value = T),

                          sliderInput("CBC_fp_2", HTML("Carbon-based Constituent  (g cm<sup>-2</sup>)"), min = 0, max = 0.03,  value = c(0.001,0.002)),
                          checkboxInput("Dist_cbc_fp_2", "Use a gauss distribution", value = T),

                        ),
                        # Leaf Model parameters (2)
                        conditionalPanel(
                          condition = "input.leaf_lut == 'PROSPECT-D'",

                          sliderInput("Cab_d2",  HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 100, value = c(5,60)),
                          checkboxInput("Dist_cab_d2", "Use a gauss distribution", value = F),

                          sliderInput("Car_d2", HTML("Carotenoid content (μg cm<sup>-2</sup>)"), min = 0, max = 40,  value = c(4,20)),
                          checkboxInput("Dist_car_d2", "Use a gauss distribution", value = F),

                          sliderInput("Anth_d2",  HTML("Anthocyanin content (μg cm<sup>-2</sup>)"), min = 0, max = 10,  value = c(0,5)),
                          checkboxInput("Dist_anth_d2", "Use a gauss distribution", value = F),

                          sliderInput("Cbrown_d2", "Cbrown", min = 0, max = 1,  value = c(0,1)),
                          checkboxInput("Dist_cbrown_d2", "Use a gauss distribution", value = F),

                          sliderInput("N_d2", "mesophyll structure parameter", min = 1, max = 5,  value = c(1,3)),
                          checkboxInput("Dist_n_d2", "Use a gauss distribution", value = F),

                          sliderInput("EWT_d2", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05,  value = c(0.01,0.03)),
                          checkboxInput("Dist_ewt_d2", "Use a gauss distribution", value = F),

                          sliderInput("LMA_d2", HTML("dry matter content (g cm<sup>-2</sup>)"), min = 0, max = 0.05,  value = c(0.01,0.03)),
                          checkboxInput("Dist_lma_d2", "Use a gauss distribution", value = F),



                        ),
                        # Leaf Model parameters (3)
                        conditionalPanel(
                          condition = "input.leaf_lut == 'Liberty'",

                          sliderInput("Cab_l2", HTML("Chlorophyll content (μg cm<sup>-2</sup>)"), min = 0, max = 80, value = c(40,70)),
                          checkboxInput("Dist_cab_l2", "Use a gauss distribution", value = F),

                          sliderInput("EWT_l2", HTML("Water content (g cm<sup>-2</sup>)"), min = 0, max = 0.05, value = c(0.01,0.02)),
                          checkboxInput("Dist_ewt_l2", "Use a gauss distribution", value = F),

                          sliderInput("lign_cell2", "Lignin and cellulose content", min = 10, max = 80, value = c(20,60)),
                          checkboxInput("Dist_lign_cell_l2", "Use a gauss distribution", value = F),

                          sliderInput("Nitrogen2", HTML("Nitrogen content (g m<sup>-2</sup>)"), min = 0.3, max = 2,value = c(0.5,1)),
                          checkboxInput("Dist_Nitrogen_l2", "Use a gauss distribution", value = F),

                          sliderInput("cell_d2", HTML("Cell diameter  (m<sup>-6</sup>)"), min = 20, max = 200, value = c(50,100)),
                          checkboxInput("Dist_cell_d_l2", "Use a gauss distribution", value = F),

                          sliderInput("inter_c2", "Intercellular air space", min = 0.01, max = 0.1, value = c(0.05,0.1)),
                          checkboxInput("Dist_inter_c_l2", "Use a gauss distribution", value = F),

                          sliderInput("baseline_abs2", "baseline", min = 0.0004, max = 0.0006, value = c(0.0004,0.0006)),
                          checkboxInput("Dist_baseline_abs_l2", "Use a gauss distribution", value = F),

                          sliderInput("leaf_thick2", "leaf thickness", min = 1, max = 10, value = c(2,5)),
                          checkboxInput("Dist_leaf_thick_l2", "Use a gauss distribution", value = F),

                          sliderInput("albino_abs2", "Albino absorption", min = 0, max = 4, value = c(0,4)),
                          checkboxInput("Dist_albino_abs_l2", "Use a gauss distribution", value = F),

                        ),
                        h4(strrep("-", 60)),
                        h3('canopy parameters :'),
                        # Leaf Model parameters
                        conditionalPanel(
                          condition = "input.canopy_lut == 'fourSAILH'",
                          sliderInput("LAI2", HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 10,  value = c(0.5,5)),
                          checkboxInput("Dist_lai2", "Use a gauss distribution", value = F),

                          sliderInput("LIDFa2", "LIDFa (°)", min = 0, max = 90, value = c(30,90)),
                          checkboxInput("Dist_lidfa2", "Use a gauss distribution", value = T),

                          sliderInput("hotspot2", "hotspot", min = 0, max = 1, value = c(0.5,0.9)),
                          checkboxInput("Dist_hotspot2", "Use a gauss distribution", value = F),

                          sliderInput("tts2", "tts (deg)", min = 0, max = 90, value = c(0,15)),
                          checkboxInput("Dist_tts2", "Use a gauss distribution", value = T),

                          sliderInput("tto2", "tto (deg)", min = 0, max = 90, value = c(0,15)),
                          checkboxInput("Dist_tto2", "Use a gauss distribution", value = T),

                          sliderInput("psi2", "psi (deg)", min = 0, max = 180, value = c(25,45)),
                          checkboxInput("Dist_psi2", "Use a gauss distribution", value = T),

                          sliderInput("psoil2", "soil factor", min = 0, max = 1, value = c(0.5,0.8)),
                          checkboxInput("Dist_psoil2", "Use a gauss distribution", value = T),


                        ),
                        conditionalPanel(
                          condition = "input.canopy_lut == 'INFORM'",
                          sliderInput("LAI_2", HTML("leaf area index (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0, max = 10,  value = c(0.5,5)),
                          checkboxInput("Dist_lai_2", "Use a gauss distribution", value = T),


                          sliderInput("LIDFa_2", "LIDFa (deg)", min = 0, max = 90, value = c(30,90)),
                          checkboxInput("Dist_lidfa_2", "Use a gauss distribution", value = T),

                          sliderInput("hotspot_2", "hotspot", min = 0, max = 1,value = c(0.5,1)),
                          checkboxInput("Dist_hotspot_2", "Use a gauss distribution", value = F),

                          sliderInput("tts_2", "tts (deg)", min = 0, max = 90, value = c(0,15)),
                          checkboxInput("Dist_tts_2", "Use a gauss distribution", value = T),

                          sliderInput("tto_2", "tto (deg)", min = 0, max = 90, value = c(0,15)),
                          checkboxInput("Dist_tto_2", "Use a gauss distribution", value = T),

                          sliderInput("psi_2", "psi (deg)", min = 0, max = 180, value = c(15,65)),
                          checkboxInput("Dist_psi_2", "Use a gauss distribution", value = F),

                          sliderInput("LAIu_2", HTML("understory LAI (m<sup>2</sup>/m<sup>-2</sup>)"), min = 0.05, max = 2, value = c(0.5,1.5)),
                          checkboxInput("Dist_laiu_2", "Use a gauss distribution", value = F),

                          sliderInput("cd_2", "Crown diameter (m)", min = 0, max = 10, value = c(2,5)),
                          checkboxInput("Dist_cd_2", "Use a gauss distribution", value = F),

                          sliderInput("sd_2",  HTML("Stem density (ha<sup>-1</sup>)"), min = 0, max = 3000,  value = c(500,600)),
                          checkboxInput("Dist_sd_2", "Use a gauss distribution", value = F),

                          sliderInput("h_2", "tree height (m)", min = 10, max = 40,  value = c(25,30)),
                          checkboxInput("Dist_h_2", "Use a gauss distribution", value = F),

                          #  sliderInput("skyl_2", "skyl (fraction-Fixed to 0.1)", min = 0.001, max = 0.4, value = c(0.001,0.4)),
                          sliderInput("psoil_2", "soil factor", min = 0, max = 1,  value = c(0.3,0.6)),
                          checkboxInput("Dist_psoil_2", "Use a gauss distribution", value = T),
                        ),

                      ),
                      mainPanel(
                        h3('Simulated average reflectance'),
                        plotOutput("plot_lut"),

                        #uiOutput("histogram_ui"),
                        actionButton("buttonLUT2", "Generate the LUT"),
                        downloadButton("downloadData2", "Download"),
                        br(),br(),
                        p(em('Note₁: The "Generate the LUT" button is designed to function correctly
                                             only on the first use. Please ensure to click it only once for accurate results.')),
                        p(em('Note₂: Uniform distribution is used as default. Those unrealist simulations were discarded')),
                        p(em('Note₃: Please use the scroll panel to adjust the input ranges, and then generate the Look-Up Table.')),
                        p(em('Note₄: The plant traits shown below represent their respective distributions based on the input parameters.')),
                        # Provide a clear instruction to the user
                        # Add a grey horizontal line
                        tags$hr(style = "border-color: grey;"),
                        p('Please review your selected inputs by choosing between the Min-Max LUT summary or the distribution of plant traits.'),
                        p(em('Note₅:: This option is only active when the simulations are run for the first time; thereafter, it becomes dynamic.')),
                        # Add selectInput to toggle between views
                        selectInput("view_selector", "",
                                    choices = c("Min-Max Inputs (LUT Summary)" = "LUT",
                                                "Plant Trait Distribution" = "Trait"),
                                    selected = "LUT"), # Set default to LUT summary

                        # Conditional panel for the LUT parameters summary
                        conditionalPanel(
                          condition = "input.view_selector == 'LUT'", # Show LUT summary by default
                          h3('Min-Max Inputs (LUT Summary)'),
                          p("Biochemical, biophysical, structural, and geometric parameters values used for the selected canopy model:"),
                          verbatimTextOutput("params_output") # LUT parameters summary output
                        ),

                        # Conditional panel for the trait distribution plot
                        conditionalPanel(
                          condition = "input.view_selector == 'Trait'", # Show trait distribution plot when selected
                          h3("Plant Trait Distribution"),
                          plotOutput("trait_distribution_plot") # For trait distribution plot
                        ),



                      )## close the mainPanel
                    ) ## close the sidebarLayout

           ), # Close the tab panel

           tabPanel("Automatic LUT", # Open a second tabPanel for generating LUTs
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("model_selected_leaf", "Select a leaf RT Model:",
                                    choices = c("PROSPECT-PRO", "PROSPECT-D", "Liberty",'FLUSPECT-B','FLUSPECT-B-Cx')),
                        selectInput("model_selected_canopy", "Select a canopy RT Model:",
                                    choices = c('fourSAILH','INFORM')),
                        selectInput("sensor_selected", "Select sensor for resampling resolution:",
                                    choices = c('RTM','Sentinel2a','Sentinel2b','Landsat4','Landsat5','Landsat7','Landsat8')),
                        numericInput("n_samples", "Number of samples:", value = 50,min = 10,max = 20000),
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
                        downloadButton("downloadData", "Download"),
                        br(),br(),
                        p(em('Note₁: The "Generate the LUT" button is designed to function correctly
                                             only on the first use. Please ensure to click it only once for accurate results.')),

                        p(em('Note₂: Uniform distribution is used as default.Those unrealist simulations were discarded')),
                        p(em('Note₃: The plant traits shown below represent their respective distributions based on the input parameters.')),

                        # Provide a clear instruction to the user
                        # Add a grey horizontal line
                        tags$hr(style = "border-color: grey;"),
                        p(em('Note₅:: This option is only active when the simulations are run for the first time; thereafter, it becomes dynamic.')),
                        # Add selectInput to toggle between views
                        selectInput("view_selector_default", "",
                                    choices = c("Min-Max Inputs (LUT Summary)" = "LUT_default",
                                                "Plant Trait Distribution" = "Trait_default"),
                                    selected = "LUT"), # Set default to LUT summary

                        # Conditional panel for the LUT parameters summary
                        conditionalPanel(
                          condition = "input.view_selector_default == 'LUT_default'", # Show LUT summary by default
                          h3('Min-Max Inputs (LUT Summary)'),
                          p("Biochemical, biophysical, structural, and geometric parameters values used for the selected canopy model:"),
                          tableOutput("lut_head_default") # LUT parameters summary output
                        ),

                        # Conditional panel for the trait distribution plot
                        conditionalPanel(
                          condition = "input.view_selector_default == 'Trait_default'", # Show trait distribution plot when selected
                          h3("Plant Trait Distribution"),
                          plotOutput("trait_distribution_plot_default") # For trait distribution plot
                        ),

                      )## close the mainPanel
                    ) ## close the sidebarLayout

           ) # Close the tab panel
           ,

           tabPanel("Main functions used in ToolsRTM",
                    # Introduction to the module
                    h4("Main funtions in ToolsRTM package used in this module"),

                    p(style = "text-align: justify;", "This module utilizes key functions integrated into the ", strong("ToolsRTM"), " package to process and analyze vegetation data derived from Sentinel-2 satellite imagery. These functions are critical for estimating spectral indices, training machine learning models, and predicting plant traits such as gross primary production (GPP) and other physiological traits."),

                    # getLUTfromRanges
                    h4("1. getLUTs"),
                    # Section for getMLmodel.withRetrain
                    p(style = "text-align: justify;", "The ", code("getLUTs"), " function generates Look-Up Tables (LUTs) for radiative transfer models. It requires a table of inputs specifying the ranges of variables and allows users to define the number of LUT rows and set a seed for randomization. This function is essential for creating LUTs used in various analyses related to vegetation and canopy properties. Below are the key arguments for this function:"),
                    # Key parameters explanation
                    p(strong("Key Parameters:")),
                    # List of function arguments
                    tags$ul(
                      tags$li(strong("inputs:"), " A LUT table with variables and their specific ranges. Main inputs can be extracted from ",
                              code("ToolsRTM::inputsFlUSPECT"), ", ", code("inputsINFORM"), ", ", code("inputsSCOPE"), ", ",
                              code("inputsPROSAIL"), ", ", code("inputsSPART"), " and ", code("inputsRTMs"), ", which are dataframes containing the main inputs for RTMs."),
                      tags$li(strong("nLUT:"), " The number of rows to be generated for the LUT."),
                      tags$li(strong("dependencies:"), " Dependencies required for the function (e.g., set to 'Car')."),
                      tags$li(strong("setseed:"), " A seed number to control the random process. By default, it is set to 123, but you can specify a different number to ensure reproducibility.")
                    ),
                    p(style = "text-align: justify;",'In this function, we specifically include correlations between chlorophyll content (Cab)
                      and carotenoid content (Car). It is essential that both inputs share the same names in the LUT to ensure accurate modeling.'),
                    p(strong("Usage:")),
                    # Example usage of the function
                    p(style = "text-align: justify;", "Example of using this function:"),
                    code("LUTs <- getLUTs(inputs = ToolsRTM::inputsPROSAIL, nLUT = 100, dependencies = 'Car', setseed = 123);"),
                    br(),
                    # Section for getLUT
                    h4("2. getCor"),
                    p(style = "text-align: justify;", "The ", code("getCor"), "integrate in ", strong("ToolsRTM"), " package"),
                    # Key parameters explanation
                    p(strong("Key Parameters:")),
                    # List of function arguments

                    tags$ul(
                      tags$li(code("n_inputs"), " - A number indicating the length for the LUT."),
                      tags$li(code("nLUT"), " - The number of rows for the LUT."),
                      tags$li(code("distribution"), " - Specifies the distribution type for the variable ranges."),
                      tags$li(code("setseed"), " - A seed value to control the randomization process."),
                      tags$li(code("rho"), " - The correlation value for uniform distribution."),
                      tags$li(code("Varnames"), " - Names for the variables in the LUT."),
                      tags$li(code("MinRange"), " - Minimum range values for the variables."),
                      tags$li(code("MaxRange"), " - Maximum range values for the variables.")
                    ),
                    p(strong("Usage:")),
                    # Example usage of the function
                    p(style = "text-align: justify;", "Example of using this function:"),
                    code("LUT$Vcmax25 = stats::runif(n.samples, min = 5, max = 90)"),
                    br(),
                    code("pigments <- getCor(n_inputs = 2, setseed = n_seed, distribution = 'Uniform', nLUT = n.samples, rho = 0.99,"),
                    br(),
                    code("  Varnames = c('Cab', 'Vcmax25'), MinRange = c(0.5, 5), MaxRange = c(95, 90))"),
                    br(),
                    # Section for getLUT
                    h4("3. getLUT"),
                    p(style = "text-align: justify;", "The ", code("getLUT"), " function serves as a streamlined version of ", code("getLUTs"), " for generating
                      uniform range Look-Up Tables (LUTs) specifically for radiative transfer models.
                      This function requires a table of inputs that delineates the ranges of variables,
                      enabling users to specify the desired number of LUT rows and set a seed for randomization.
                      It is vital for creating LUTs that facilitate various analyses related to vegetation and canopy
                      properties. A straightforward approach involves utilizing a dataset with three columns: inputs, min, and max.
                      The inputs column identifies the variables of interest, while the min and max columns indicate the
                      respective minimum and maximum values for each variable. Below are the key arguments for this function:"),
                    # Key parameters explanation
                    p(strong("Key Parameters:")),
                    # List of function arguments
                    tags$ul(
                      tags$li(strong("inputs:"), " A table with specific ranges and types of distributions for the variables."),
                      tags$li(strong("nLUT:"), " The number of rows to be generated for the LUT."),
                      tags$li(strong("setseed:"), " A seed number to control the random process. By default, it is set to 123, but you can specify a different number to ensure reproducibility.")
                    ),
                    p(strong("Usage:")),
                    # Example usage of the function
                    p(style = "text-align: justify;", "Example of using this function:"),
                    code("LUTs <- getLUTs(inputs = ToolsRTM::inputsPROSAIL[,1:3], nLUT = 100, setseed = 123);"),
                    br(),

                    # Section for getLUT_liberty
                    h4("4. getLUT_liberty"),
                    p(style = "text-align: justify;", "The ", code("getLUT_liberty"), " function generates Look-Up Tables (LUTs) specifically for the Liberty canopy radiative transfer model. This function is designed to create LUTs that minimize correlations between critical inputs, such as chlorophyll content (cab) and carotenoid content (car), ensuring that the LUTs are representative of realistic canopy conditions. It requires a set of inputs and allows users to define the number of LUT rows and set a seed for randomization. Below are the key arguments for this function:"),
                    # Key parameters explanation
                    p(strong("Key Parameters:")),
                    # List of function arguments
                    tags$ul(
                      tags$li(strong("inputs:"), " A table that specifies the ranges of variables relevant to the Liberty model, including cab, car, and other parameters."),
                      tags$li(strong("nLUT:"), " The number of rows to be generated for the LUT; this example uses nLUT = 1."),
                      tags$li(strong("setseed:"), " A seed number to control the random process, which ensures reproducibility. The default is set to 1234.")
                    ),
                    p(strong("Usage:")),
                    # Example usage of the function
                    p(style = "text-align: justify;", "Example of using this function for generating a LUT for the Liberty model:"),
                    code("LUT.liberty <- as.data.frame(getLUT_liberty(inputs = inputs.liberty, nLUT = 1, setseed = 1234));"),
                    br(),

                    # Section for get.LUTfromRanges
                    h4("5. get.LUTfromRanges"),
                    p(style = "text-align: justify;", "The ", code("get.LUTfromRanges"), " function extracts LUTs from specified ranges. It allows the user to define parameters such as leaf and canopy models, as well as the type of distribution used for sampling. Below are the key arguments for this function:"),
                    # Key parameters explanation
                    p(strong("Key Parameters:")),
                    # List of function arguments
                    tags$ul(
                      tags$li(strong("LUT:"), " The LUT table to be processed."),
                      tags$li(strong("nLUT:"), " The number of rows for the LUT. For example: ", code("nLUT = 100"), "."),
                      tags$li(strong("setseed:"), " A seed number to control the random process."),
                      tags$li(strong("leaf.model:"), " The model to be used for leaf reflectance (e.g., 'Liberty')."),
                      tags$li(strong("canopy.model:"), " The model to be used for canopy reflectance (e.g., 'fourSAILH')."),
                      tags$li(strong("distribution:"), " The type of distribution for sampling, which can be 'uniform' or 'gaussian'.")
                    ),
                    p(strong("Usage:")),
                    # Example usage of the function
                    p(style = "text-align: justify;", "Example of using this function:"),
                    code("LUT_ranges <- get.LUTfromRanges(LUT = ToolsRTM::inputsINFORM[,1:3], nLUT = 100, setseed = 1234, leaf.model = 'Liberty', canopy.model = 'fourSAILH', distribution = 'uniform');"),
                    br(),
                    h4("Citation"),
                    p(style = "text-align: justify;",HTML('If you use these models with <b>ToolsRTM</b> package, please cite the following references:')),

                    p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                    p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                    p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),


                    br()
           ),
                 ), # Close the navbarMenu

# 5) Tab Inversion module  -----------------------------------------

                 tabPanel(title = "Inversion of Plant Traits via Hybrid ML models",
                          # Beta Version Notice
                          tags$div(
                            style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                            "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                          ),
                          # Add a sidebar layout
                          sidebarLayout(
                            # Add a sidebar panel
                            sidebarPanel(
                              # Add information on cranes and prompt user to use slider
                              h4("Select the parameters:"),

                              # LUT database selection
                              selectInput("lut_db", "Select LUT Database:",
                                          choices = list(
                                            "Upload New Dataset" = "upload",
                                            "PROSAILH (pre-defined LUT)" = "PROSAIL",
                                            "INFORM (pre-defined LUT)" = "INFORM"
                                           )),
                              # Conditional panel to show file input when "Upload New Dataset" is selected
                              conditionalPanel(
                                condition = "input.lut_db == 'upload'",

                                fileInput("uploaded_file", "Upload Your Dataset (CSV):",
                                          accept = c(".csv"), multiple = FALSE)

                              ),
                              # Checkbox for plotting indices
                              checkboxInput("plotting_indices", "Calculate spectral indices", value = FALSE),


                              tags$hr(style = "border-color: grey;"),

                              #h4("Inversion Method:"),
                              # Dependent variable selection
                              selectInput("depVar", "Select a plant trait to estimate:",
                                          choices = c('Chorophyll content (Cab)' ='Cab',
                                                      'Carotenoid content (Car)' ='Car', 'Anthocyanin  content (Anth)' ='Anth',
                                                      'Leaf area index (LAI)'= 'LAI', 'leaf water content (EWT)' = 'EWT',
                                                      'leaf protein content (Prot)' = 'Prot',
                                                      'Carbon-based constituent (CBC)' = 'CBC',
                                                      'Leaf brown pigments (Cbrown)'= 'Cbrown')),
                              tags$hr(style = "border-color: grey;"),

                              selectInput("inputs_toML", "Select input features for training the ML model:",
                                          choices = c( 'Red-edge Vegetation Indices' = 'rededge_indices',
                                                       'Main Vegetation Indices' = 'indices',
                                                      'Visble-NIR Vegetation Indices' = 'visible_indices',

                                                      'SWIR Vegetation Indices' = 'swir_indices',
                                                      'Reflectance Bands' = 'reflectance',
                                                      'Reflectance VNIR Bands' = 'vnir_reflectance',
                                                      'Reflectance SWIR Bands' = 'swir_reflectance',
                                                      'Combined Vegetation Indices and Reflectance' = 'indices_reflectance',
                                                      'Combined Vegetation Indices and Reflectance in VNIR' = 'indices_reflectance_vnir',
                                                      'Combined Vegetation Indices and Reflectance in SWIR' = 'indices_reflectance_swir')),


                              tags$hr(style = "border-color: grey;"),



                              # Inversion model selection
                              selectInput("models_", "Select Model:",
                                          choices = list(
                                           "Support Vector Machine" = "SVM",
                                           "deep Neural Network" = "Hidden_layers",
                                           "Random Forest" = "RF",
                                    #        "Convolution Neural Network" = "CNN",
                                       #    "Neural Network (caret)" = "NN",
                                          "Partial least square regression" = "PLSR",
                                          "eXtreme Gradient Boosting (XGBoost)" = "xGB",
                                           "Gradient Boosting" = "GB"
                                            )),




                              tags$hr(style = "border-color: grey;"),

                              # Conditional panel for neural networks (CNN or NNe)
                              conditionalPanel(
                                condition = "input.models_ == 'CNN' || input.models_ == 'Hidden_layers'",

                                h4("Neural Network Configuration"),

                                # Preprocessing method selection
                                selectInput("method.preProcess", "Select Preprocessing Method:",
                                            choices = c('Normalize', 'Standarize', 'Center',
                                                        'YeoJohnson', 'BoxCox')),

                                # Optimizer selection
                                selectInput("optimizer", "Select Optimizer:",
                                            choices = c('adam', 'adadelta', 'adagrad', 'adamax',
                                                        'nadam', 'msprop', 'sgd')),

                                # Neural network parameters
                                numericInput("n_layers", "Number of Layers:", 3, min = 1,max=5),
                                numericInput("n_neurons", "Number of Neurons:", 128, min = 1,max=1024),
                                numericInput("batch_size", "Batch Size:", 32, min = 1,max=2048),
                                numericInput("n_epochs", "Number of Epochs:", 10, min = 1, max=1000),
                                numericInput("p_samplesML_keras", "Percentage of samples:", value=1, min = 1,max=90),
                              ), # Conditional panel for neural networks (CNN or NNe)
                              conditionalPanel(
                                condition = "input.models_ == 'SVM' || input.models_ == 'RF'  || input.models_ == 'NN' || input.models_ == 'PLSR'
                                || input.models_ == 'xGB' || input.models_ == 'GB'",

                                h4("Parameters: "),

                                # Preprocessing method selection
                                numericInput("p_samplesML", "Percentage of samples:", value=1, min = 1,max=90),
                                p(em('Adjust this value to control the proportion of the samples used in the training processing.
                                     Increasing the percentage may enhance the accuracy of the results but will require more processing time.'))

                              ),

                              actionButton("train_model", "Train Model")



                            ), # Close sidebarPanel
                            mainPanel( # open main panel
                              mainPanel(
                                tabsetPanel(
                                  id = "tabs",

                                  tabPanel("LUTs",
                                           h4(""),
                                           # Conditional panel for showing the outputs based on the selected LUT database
                                           conditionalPanel(
                                             condition = "input.lut_db != ''",  # Trigger panel when any LUT database is selected
                                             h4('Statistical Reflectance Profiles for the ML Inversion'),
                                             # Display a custom error message if there's no uploaded file or processing issues
                                             uiOutput("error_message"), # Placeholder for error message

                                             # Output for the plot
                                             plotOutput("plot_checking"),
                                             br(),
                                            # uiOutput("lut_selection"),  # Links based on `notebook_p`

                                             # Output for the DataTable
                                             DT::dataTableOutput("lut_header"),
                                           ),

                                           ), ## end panel

                                  tabPanel("Indices",
                                           h4("Scatterplot Analysis"),
                                           # Informative note for users
                                           p("Note: Estimation of indices is required. Please ensure to check the 'Calculate Indices' box to enable this feature."),
                                           br(),
                                           # UI for selecting variables for the scatter plot
                                           uiOutput("select_axes_ui"),  # Dynamically generated UI for axis selection

                                           # Output for the scatter plot
                                           plotOutput("scatter_plot"),  # Render the scatter plot
                                           br(),
                                           # Informative note for users
                                           uiOutput("instructions_ui"),  # Dynamically generated UI for instructions




                                  ), ### close tab panel
                                  tabPanel("Predictions",
                                           h3("Hybrid Inversion Model Results"),
                                           br(),
                                           textOutput("status"),
                                           verbatimTextOutput("training_progress"),  # Real-time training progress
                                           DT::DTOutput("training_history") , # Final training history
                                          # DT::dataTableOutput("training_history"),  # New output for training history
                                           plotOutput("predictionPlot"),
                                           br(),
                                           DT::dataTableOutput("statsTable_ML"),
                                           br(),
                                           uiOutput("plotReady_tableReady"),

                                           br(),
                                           tags$hr(style = "border-color: grey;"),


                                  ), ## end panel

                                  tabPanel("Info",

                                           h4("How use the inversion module"),
                                           # Custom CSS for instruction box
                                           tags$style(HTML("
                                           .instruction-box {
                                           border: 1px solid #007BFF;  /* Blue border */
                                           background-color: #F0F8FF;  /* Light blue background */
                                           padding: 15px;
                                           margin-bottom: 20px;
                                           border-radius: 5px;
                                           font-size: 16px;
                                           color: #333;
                                           }
                                                     ")),

                                           # Instruction Box
                                           p(strong("Step 1:")),

                                           p(style = "text-align: justify;",
                                             "Select a pre-trained Look-up table (LUT) for uploading your dataset in the required format accroding the LUT module."),

                                           p(em("Note₁: Review the spectral profiles of your reflectance bands in the 'LUTs' tab.")),

                                           # Instruction Box
                                           p(strong("Step 2:")),
                                           # Instruction Box

                                           p(style = "text-align: justify;",
                                             "Estimate a set of key spectral indices related to biochemical, biophysical, and structural
                                                      traits for Sentinel-2 resolution data"),

                                           p(em("Note₂: Check the relationship between indices and model inputs in the 'Indices' tab.")),
                                           p(em('Note₃: These indices will be used to train a model for retrieving key plant traits.')),

                                           # Instruction Box
                                           p(strong("Step-3:")),

                                           p(style = "text-align: justify;",
                                             "Select a deep machine learning (ML) or classical ML model and adjust the parameters."),

                                           p(em("Note₄: Once the deep learning model parameters are
                                           defined, proceed to train the model by clicking the 'Train the model' button.")),
                                           p(em("Note₅: The deep ML algorithms are the 1D-Convolutional Neural Networks (CNN) and a clasical ML model with hidden layers.")),

                                           p(strong("Step-4:")),

                                           p(style = "text-align: justify;",
                                             "In the 'Predictions' tab, review the results and download the scaler, statistical scores, and the trained model,
                                                     "),

                                           p(em("This module utilizes the LUT generated by the PROSAIL-PRO model (PROSPECT-PRO & fourSAILH) as described in ", strong("Camino et al. (2024, in preparation)."))),
                                           br(),
                                           # Instruction Box
                                           tags$div(class = "instruction-box",
                                                    p(style = "text-align: justify; font-size: 12px;",
                                                      "This module is designed to work with Sentinel-2A and Sentinel-2B imagery using a convolved LUT. Herem thw plant traits were retrieved from reflectance data
                                                     ad main spectral indices followed the method described in Camino et al., (2024; in prep.)
                                                     entitled 'Integrating physiological plant traits with Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances.'"),
                                                    p(style = "text-align: justify; font-size: 12px;",
                                                      "Future updates will integrate the ", strong("ToolsRTMs")," package, enabling compatibility with additional platforms and expanding remote sensing applications. (e.g., MODIS, Landsat)"),
                                                    p(style = "text-align: justify; font-size: 12px;",
                                                      "For support with other satellite (e.g., Landsat) or aerial platforms (e.g., hyperspectral imagery), please consult the ", strong("ToolsRTMs")," package documentation on CRAN.")
                                           ),
                                           br(),
                                           p(em("The module uses the ", code("getMLmodel.withRetrain")," function, integrated into ", strong("ToolsRTMs")," package.")),
                                           p(em("The module uses the ", code("get.inversion")," function, integrated into ", strong("ToolsRTMs")," package.")),


                                  ), ## end panel

                                  tabPanel("Functions",
                                           # Introduction to the module
                                           h4("Main funtions integrated in the ToolsRTM Package"),

                                           p(style = "text-align: justify;", "This module utilizes key functions integrated into the ", strong("ToolsRTM"), " package to process and analyze vegetation data derived from Sentinel-2 satellite imagery. These functions are critical for estimating spectral indices, training machine learning models, and predicting plant traits such as gross primary production (GPP) and other physiological traits."),
                                           br(),
                                           # Section for getIndicesSE2
                                           h4("1. getIndicesSE2"),
                                           p(style = "text-align: justify;", "The ", code("getIndicesSE2"), " function is designed to compute various spectral indices using Sentinel-2 data. It processes the hyperspectral bands to derive essential vegetation indices like ", strong("NDVI"), ", ", strong("EVI"), ", and others. These indices are crucial for estimating parameters such as GPP and monitoring the health and traits of vegetation."),

                                           # Arguments
                                           p(strong("KeyParameters:")),
                                           tags$ul(
                                             tags$li(code("df"), " - A dataframe with reflectance where each row corresponds to a spectrum."),
                                             tags$li(code("sensor"), " - Sensor options: 'Sentinel-2a' or 'Sentinel-2b'."),
                                             tags$li(code("df.data"), " - Dataset with IDs corresponding to each spectrum; can be NULL."),
                                             tags$li(code("fast.process"), " - Set to TRUE if bands are ordered for Sentinel-2; otherwise, FALSE or NULL.")
                                           ),
                                           # Usage
                                           p(strong("Usage:")),
                                           code("getIndicesSE2(df, sensor = 'Sentinel-2a', df.data = NULL, "),
                                           br(),
                                           code("fast.process = NULL)"),
                                           br(),
                                           br(),
                                           # Section for getMLmodel.withRetrain
                                           h4("2. getMLmodel.withRetrain"),
                                          p(style = "text-align: justify;", "The ", code("getMLmodel.withRetrain"), " function is a deep learning-based model retraining process. It helps fine-tune machine learning models by retraining them on the input features and vegetation indices derived from Sentinel-2 data. The model is trained to predict variables like GPP or specific plant traits. This function depends on the following packages: ", code("MLMetric"), ", ", code("keras"), ", and ", code("tensorflow"), ". Note that the non-retraining version of this function is ", code("getMLmodel"), " from the ToolsRTM package."),

                                           # Key parameters explanation
                                           p(strong("Key Parameters:")),
                                           p(style = "text-align: justify;", code("dataset"), " - The dataset that contains the input features (bands) and the dependent variable."),
                                           p(style = "text-align: justify;", code("depVar"), " - The dependent variable (e.g., GPP, LAI, chlorophyll content) being predicted."),
                                           p(style = "text-align: justify;", code("model"), " - The type of machine learning model to be used. Options include 'CNN' for Convolutional Neural Networks or 'Hidden-layers' for models with specified hidden layers."),
                                           p(style = "text-align: justify;", code("optimizer"), " - The optimization algorithm used for training the model. Options include 'adam', 'adadelta', 'adagrad', 'adamax', 'nadam', 'msprop', and 'sgd'."),
                                           p(style = "text-align: justify;", code("n.times"), " - The number of times the model retrains. By default, this is set to 1, but it can be adjusted for multiple training iterations."),
                                           p(style = "text-align: justify;", code("n.neurons"), " - The number of neurons to use in the hidden layers of the model. Default is 128 neurons, applicable primarily for models that utilize hidden layers."),
                                           p(style = "text-align: justify;", code("n.layers"), " - The number of hidden layers in the model. Default is 4, only relevant for the 'Hidden-layers' model."),
                                           p(style = "text-align: justify;", code("batch.size"), " - The number of samples processed before the model is updated. Default is 125, but it can be adjusted based on the dataset size."),
                                           p(style = "text-align: justify;", code("n.epochs"), " - The number of complete passes through the training dataset. Default is 100 epochs, which can be modified as needed."),
                                           p(style = "text-align: justify;", code("save.model"), " - A boolean value that specifies whether to save the trained model. Options are TRUE or FALSE; if TRUE, specify a path in ", code("path.model"), " for saving."),
                                           p(style = "text-align: justify;", code("path.model"), " - The path for saving the trained model. If not specified, defaults to 'Models'."),
                                           p(style = "text-align: justify;", code("prop.split"), " - A vector indicating the proportion of the dataset to be used for training and validation. The default is ", code("c(0.8, 0.2)"), " which represents 80% for training and 20% for validation."),
                                           p(style = "text-align: justify;", code("data.trans"), " - Specifies the method of data transformation to be applied to the dataset before training. Options include 'PCA' and 'preProcess'."),
                                           p(style = "text-align: justify;", code("method.preProcess"), " - The specific data transformation method applied during preprocessing. Options include 'Normalize', 'YeoJohnson', 'BoxCox', 'Standardize', 'Center', 'Scale', and 'PCA'."),
                                           p(style = "text-align: justify;", code("depVar.trans"), " - A boolean value indicating whether to apply data transformation to the dependent variable. Options are TRUE or FALSE."),
                                           br(),
                                          p(strong("Return Value:")),
                                          p(style = "text-align: justify;", "The ", code("getMLmodel.withRetrain"), " function returns a list with the following elements:"),
                                          tags$ul(
                                            tags$li(code("model"), " - The trained Keras model object."),
                                            tags$li(code("history"), " - The training history of the model (loss and accuracy metrics over epochs)."),
                                            tags$li(code("stats"), " - Performance statistics to export (e.g., RMSE, R²)."),
                                            tags$li(code("Scalar.train"), " - The scaling parameters applied to the training data."),
                                            tags$li(code("Scalar.Ytrain"), " - The scaling parameters applied to the dependent variable for the training data."),
                                            tags$li(code("plot.val"), " - A plot visualizing validation results."),
                                            tags$li(code("plot.cor"), " - A plot displaying correlation between predicted and actual values.")
                                          ),
                                          p(style = "text-align: justify;", "An example of the model's performance visualization is shown below:"),
                                          img(src = "images/model_cab.png", alt = "Model RandomForest Performance", style = "width: 100%; height: auto;"),
                                          br(),
                                           br(),
                                           p(strong("Usage:")),
                                           # Example usage of the getMLmodel.withRetrain function
                                           p(style = "text-align: justify;", "Example of using the ", code("getMLmodel.withRetrain"), " function:"),
                                           code(style = "text-align: justify;","trained_model <- getMLmodel.withRetrain("),
                                           code(style = "text-align: justify;","dataset = my_data_frame, depVar = 'Cab', model = 'CNN', optimizer = 'adam',"),
                                           code(style = "text-align: justify;","n.times = 1, n.neurons = 128, n.layers = 3, batch.size = 32,"),
                                           code(style = "text-align: justify;","n.epochs = 100, save.model = TRUE, path.model = 'Models/',"),
                                           code(style = "text-align: justify;","prop.split = c(0.8, 0.2), data.trans = 'preProcess',"),
                                           code("method.preProcess = 'Normalize', depVar.trans = FALSE)"),
                                           br(),
                                           # Section for get.inversion
                                           br(),
                                           h4("3. get.inversion"),
                                           p(style = "text-align: justify;", "The ", code("get.inversion"), " function performs inversion of plant traits using various machine learning models. The function relies on several packages, including ", code("caret"), ", ", code("randomForest"), ", ", code("e1071"), ", ", code("doParallel"), ", ", code("caretEnsemble"), ", and ", code("stats"), ". It allows you to run different machine learning algorithms for predicting dependent variables like plant traits from independent variables (inputs). Below are the key configurations for this function:"),

                                           # Key parameters explanation
                                           p(strong("Key Parameters:")),
                                           p(style = "text-align: justify;", code("data"), " - The dataset containing the independent and dependent variables."),
                                           p(style = "text-align: justify;", code("depVar"), " - The dependent variable that you want to predict (e.g., GPP, LAI, chlorophyll content)."),
                                           p(style = "text-align: justify;", code("inputs"), " - A vector of independent variables (e.g., vegetation indices, reflectance bands) used for training the model."),
                                           p(style = "text-align: justify;", code("algorithm"), " - The type of machine learning model. Available options include:"),
                                           tags$ul(
                                             tags$li(code("PLSR"), " - Partial Least Squares Regression"),
                                             tags$li(code("SVM"), " - Support Vector Machine"),
                                             tags$li(code("RF"), " - Random Forest"),
                                             tags$li(code("NN"), " - Neural Network"),
                                             tags$li(code("GB"), " - Gradient Boosting"),
                                             tags$li(code("xGB"), " - Extreme Gradient Boosting (XGBoost) with linear base learners"),
                                             tags$li(code("Bayesian"), " - Bayesian Additive Regression Trees"),
                                             tags$li(code("AdaBag"), " - Bagged AdaBoost"),
                                             tags$li(code("qLASSO"), " - Quantile Regression with LASSO penalty"),
                                             tags$li(code("RVM"), " - Relevance Vector Machines (RVM) with linear kernel"),
                                             tags$li(code("BRNN"), " - Bayesian Regularized Neural Networks"),
                                             tags$li(code("Ensemble"), " - Stacking Ensemble models")
                                           ),
                                           p(style = "text-align: justify;", code("method.resampling"), " - The resampling method for controlling the training of the ML model. Options include 'boot' (Bootstrapping), 'cv' (Cross-Validation), 'LOOCV' (Leave-One-Out Cross-Validation), among others."),
                                           p(style = "text-align: justify;", code("n.cores"), " - The number of CPU cores to use for parallel processing. If not specified, defaults to single-core."),
                                           p(style = "text-align: justify;", code("seed"), " - The seed for random number generation, used to ensure reproducibility of results. Default is 123."),
                                           p(style = "text-align: justify;", code("n.samples"), " - The number of samples used for tuning and model training. Default is 500."),
                                           p(style = "text-align: justify;", code("save.model"), " - A logical value indicating whether to save the trained model. If TRUE, specify the path using ", code("save.path"), ". Default is FALSE."),
                                           p(style = "text-align: justify;", code("save.path"), " - The file path where the trained model will be saved, if ", code("save.model"), " is set to TRUE."),

                                           br(),
                                           p(strong("Return Value:")),
                                           p(style = "text-align: justify;", "The ", code("get.inversion"), " function returns a list with the following elements:"),
                                           tags$ul(
                                             tags$li(code("model.label"), " - The name of the algorithm used (e.g., 'SVM', 'RF')."),
                                             tags$li(code("model"), " - The trained model object."),
                                             tags$li(code("predictions"), " - A list containing the model's predictions for both the training and testing datasets:"),
                                             tags$ul(
                                               tags$li(code("train"), " - Predictions for the training dataset."),
                                               tags$li(code("test"), " - Predictions for the testing dataset.")
                                             ),
                                             tags$li(code("statistics"), " - Model performance statistics (e.g., RMSE, R², etc.)."),
                                             tags$li(code("plot"), " - A plot showing model performance or predictions."),
                                             tags$li(code("importance"), " - Variable importance scores (if applicable, depending on the model type).")
                                           ),
                                          p(style = "text-align: justify;", "An example of the model's performance visualization is shown below:"),
                                          img(src = "images/model_xgb.png", alt = "Model XGBoost Performance", style = "width: 100%; height: auto;"),

                                           br(),
                                           p(strong("Usage:")),
                                           # Example usage of the get.inversion function
                                           p(style = "text-align: justify;", "Example of using the ", code("get.inversion"), " function:"),
                                           code(style = "text-align: justify;", "inversion_results <- get.inversion("),
                                           code(style = "text-align: justify;", "data = my_data, depVar = 'Cab', inputs = c('NDVI', 'TCARI'), algorithm = 'SVM',"),
                                           code(style = "text-align: justify;", "method.resampling = 'cv', seed = 123, n.samples = 500, save.model = TRUE, save.path = 'Models/')"),
                                           br(),
                                           br(),
                                           # Datasets used for Sentinel-2 applications
                                           h4("Datasets used for Sentinel-2 Applications"),
                                           p("The datasets utilized in these functions include both pre-established and specifically developed datasets for Sentinel-2 applications:"),

                                           # PROSAIL Dataset
                                           p(style = "text-align: justify;", strong("PROSAIL Dataset:"), " The PROSAIL dataset serves as the Look-Up Table (LUT) in the study ", em("Camino et al. 2024 (in prep.)"), " for integrating physiological plant traits with Sentinel-2 imagery. It is widely used for estimating variables like LAI, chlorophyll content, and other key plant traits."),

                                           # INFORM Dataset
                                           p(style = "text-align: justify;", strong("INFORM Dataset:"), " The INFORM dataset is developed for a study on bark beetle outbreaks in forest canopies. This dataset is tailored for analyzing the impact of bark beetles on forest health using Sentinel-2 imagery, and it forms the basis of a study currently in preparation (", em("Camino et al. 2025 (in prep.)"), ")."),
                                           # Section for SPART
                                           h4("Citation"),

                                           p(style = "text-align: justify;",HTML('If you use these functions with <b>ToolsRTM</b> package, please cite the following references:')),

                                           p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                           p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                           p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),

                                           br()
                                  ),
                                  tabPanel("References",
                                           h3(""),

                                           p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> packages, please cite the following references:')),

                                           p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                           p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                           p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with
                                             Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),
                                           br(),
                                  ) ### close tab panel
                                )  ## close tabsetPanel


                              ) # Close mainPanel
                            ) # Close mainPanel
                          ) # Close sideBarLayout
                 ), # Close the breeding distribution tab panel
                 # Add a References tab panel, linking each Reference to its source.



# 6) Tab Monitoring plant pests  -----------------------------------------

navbarMenu("Monitoring Plant pests",
           tabPanel("San Rossore scenario", # Open a second tabPanel for generating LUTs
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        dateRangeInput("date_range", "Select a time period: ",
                                       start = "2020-01-01", end = "2020-12-31",
                                       min = "2015-06-23", max = Sys.Date(),
                                       format = "yyyy-mm-dd"),
                        sliderInput("cloud", "Maximum Cloud Coverage (%):", min = 0, max = 100, value = 2),
                        selectInput("Spatial_map", "Base Maps:",
                                    choices = c("Sentinel-2a Collection","Sentinel-2b Collection","Sentinel-2c Collection",
                                                "Landsat 8 OLI-TIRS Collection","Landsat 7 ETM+ Collection","Landsat 4 TM Collection",
                                                'MODIS Daily Collection',"PRISMA Collection" )),


                      ),
                      mainPanel(
                        ###
                        tabsetPanel(
                          tabPanel("Map Viewer",

                                   h3(""),
                                   br(),
                                   p(style = "text-align: justify;",HTML('This section provides a spatial map for visualizing reflectance profiles and time-series data of key spectral indices
                                   (e.g., NDVI, CR-SWIR) along with essential plant traits derived from the PROSAIL model. These tools enable detailed analysis of
                                          vegetation health and dynamics over time.')),
                                   tags$figure(
                                     tags$img(src = "images/MapViewer.png",height = "400px", width = "600px"),
                                     tags$figcaption("", style = "font-weight: bold;")),

                                   br(),

                                   materialSwitch(inputId = "checkbox_spectra", label = "Get spectral information", status = "danger"),
                                   p('(If checkbox is activate, click on the map for generating time series ....)'),
                                   br(),

                          ), ## end panel
                          tabPanel("Spectral Plot",
                                   h3(""),

                                   br(),
                          ), ## end panel
                          tabPanel("Time series",
                                   h3(""),

                                   br(),
                          ), ## end panel
                          tabPanel("References",
                                   h3(""),

                                   p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> package, please cite the following references:')),

                                   p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                   p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                   p('Camino et al., (in prep). Integrating physiological plant traits with Sentinel-2
                                     imagery for monitoring gross primary production and detecting forest disturbances. .'),
                                   br(),
                          ) ### close tab panel
                        )  ## close tabsetPanel
                        ####

                      )## close the mainPanel
                    ) ## close the sidebarLayout

           ), # Close the tab panel

           tabPanel("Bark beetle outbreak",
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        dateRangeInput("date_range2", "Select a time period: ",
                                       start = "2022-01-01", end = "2022-12-31",
                                       min = "2021-06-23", max = Sys.Date(),
                                       format = "yyyy-mm-dd"),
                        sliderInput("cloud2", "Maximum Cloud Coverage (%):", min = 0, max = 100, value = 2),
                        selectInput("Spatial_map2", "Base Maps:",
                                    choices = c("Sentinel-2a Collection","Sentinel-2b Collection","Sentinel-2c Collection",
                                                "Landsat 8 OLI-TIRS Collection","Landsat 7 ETM+ Collection","Landsat 4 TM Collection",
                                                'MODIS Daily Collection',"PRISMA Collection" )),
                      ),
                      mainPanel(
                        ###
                        tabsetPanel(
                          tabPanel("Map Viewer",
                                   br(),
                                    p(style = "text-align: justify;",HTML('This section provides a spatial map for visualizing reflectance profiles and time-series data of key spectral indices
                                   (e.g., NDVI, CR-SWIR) along with essential plant traits derived from the PROSAIL model. These tools enable detailed analysis of
                                          vegetation health and dynamics over time.')),br(),

                                   tags$figure(
                                     tags$img(src = "images/MapViewer2.png",height = "400px", width = "600px"),
                                     tags$figcaption("", style = "font-weight: bold;")),

                                   br(),
                                   materialSwitch(inputId = "checkbox_spectra2", label = "Get spectral information", status = "danger"),
                                   p('(If checkbox is activate, click on the map for generating time series ....)'),

                          ), ## end panel
                          tabPanel("Spectral Plot"), ## end panel
                          tabPanel("Time series"), ## end panel
                          tabPanel("References",

                                   h4(""),
                                   p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> package, please cite the following references:')),

                                   p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                   p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                   p('Camino et al., (in prep). Integrating physiological plant traits with Sentinel-2
                                     imagery for monitoring gross primary production and detecting forest disturbances. '),
                                   br(),
                            ) ### close tab panel
                        )  ## close tabsetPanel
                        ####

                      )## close the mainPanel

                      ) ## close the sidebarLayout),

           ) ## close Tab Panel

), # Close the navbarMenu

# 7) Tab Tutorials  -----------------------------------------

navbarMenu("Tutorials",

         tabPanel("Python's and R's Notebook", # Open a second tabPanel for generating LUTs
                  # Beta Version Notice
                  tags$div(
                    style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                    "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                  ),
                    sidebarLayout(
                      sidebarPanel(
                        selectInput("notebook_r", "R's Notebook:",
                                    choices = c("ToolsRTM package",
                                                "SCOPEinR package"
                                                #"Time-Series with Sentinel-2",
                                                #"Extraction and buffer", "STAC Application",
                                                #"Google Earth Engine"
                                                )),

                        selectInput("notebook_p", "Python's Notebook:",
                                    choices = c("STAC Application",  "Google Earth Engine"
                                             #   "Time-Series with Sentinel-2",
                                              #  "Extraction and buffer"
                                              )),

                      ),
                      mainPanel(
                        ###
                        tabsetPanel(
                          tabPanel("A brief description",
                                  # Add new paragraph to describe ToolsRTM and SCOPE in R

                                   h4("Tutorials for ToolsRTM and SCOPE packages"),
                                   p(style = "text-align: justify;",HTML('In this section, we include tutorials for generating the main functions to explore the <strong>ToolsRTM</strong> and <strong>SCOPE</strong> packages in R.
                                   These packages are essential for simulating canopy reflectance and radiative transfer processes, providing a detailed understanding
                                   properties and energy exchange mechanisms in the context of remote sensing.')),

                                  # Add new paragraph to describe Spatial's Notebooks

                                   h4("Spatial's Notebooks"),
                                   p(style = "text-align: justify;",'This section  is designed for student of PhD and Masters, offering a selection of various Python and R notebooks developed in R
                                   and Python and hosted on our Server or Google Colab. These notebooks are tailored for integrating
                                   spatial analysis workflows with a diverse range of satellite data collections available through STAC (SpatioTemporal Asset Catalog)
                                   servers or Google Earth Engine, all within a Python environment.'),
                                   p(style = "text-align: justify;",'The resources provided here enable students to seamlessly explore, process, and analyze geospatial data,
                                     fostering the application of advanced remote sensing techniques to environmental and spatial research projects.'),

                                   h4("Useful Links for R's Notebook"),
                                   uiOutput("r_notebooks"), # Links based on `notebook_r`
                                   br(),
                                   h4("Useful Links for Python's Notebook"),
                                   uiOutput("python_notebooks"),  # Links based on `notebook_p`

                                   br(),
                                  h4("Supported by:"),  # Updated header for logos
                                  # Add logos directly in the UI in a horizontal layout
                                  tags$div(style = "display: flex; justify-content: left; align-items: center;",
                                           tags$img(src = "images/logoR.png", alt = "R Logo", width = "80"),
                                           tags$img(src = "images/logoPython.png", alt = "Python Logo", width = "80"),
                                           tags$img(src = "images/GooogleColab.png", alt = "Google Colab Logo", width = "80")
                                  ),
                          ), ## end panel
                          tabPanel("References",
                                   h4("Citation"),

                                   p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> packages, please cite the following references:')),

                                   p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                                   p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                                   p(style = "text-align: justify;",'Camino et al., (in prep).Integrating physiological plant traits with Sentinel-2 imagery
                                     for monitoring gross primary production and detecting forest disturbances. '),
                                   br(),
                          ) ### close tab panel
                        )  ## close tabsetPanel
                        ####

                      )## close the mainPanel
                    ) ## close the sidebarLayout

          ), # Close the tab panel


), # Close the navbarMenu


# 8) Tab References  -----------------------------------------


navbarMenu("References",
           tabPanel("About ToolsRTM",
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    wellPanel( style = "background: white",
                               h4('ToolsRTM package'),
                               p(style = "text-align: justify;",HTML('<b>ToolsRTM</b> package integrates the main radiative transfer (RT) models for simulating canopy reflectance at  hyperspectral and Sentinel-2 scales.')),

                               'This package uses several functions for estimating plant traits, spectral indices and useful functions for generating time series, validation of the predictions with field observations ....',
                               br(),
                               br(),
                               "Manual is available at ",
                               tags$a(href="https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_e851c6b2/Notebooks/R/ToolsRTM/ToolsRTM.html",
                                      "ReadTheDocs"),
                               ".",
                               br(),

                               tags$figure(
                                 tags$img(src = "images/rtm_sims.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")

                               ),
                               br(),
                               strong("Fig 1."),'Reflectance simulation using several RT models.' ,
                               #tags$img(src = "images/rtm_sims.png", height = "450px", width = "500px"),
                               #htmltools::img(src = "images/rtm_sims.png"),

                               br(),
                               h4("Install ToolsRTM"),
                               p('ToolsRTM is avalaible on gitlab, so you can install using the R console:'),
                               code('install.packages("toolsrtm-main.tar.gz",repos = NULL,type = "source")'),
                               br(),

                               # Add information on cranes and prompt user to use slider

                               h4("GitLab repositories"),

                               # Link to the Gitlab website to direct folks to actual analyses
                               a("gitLab ToolsRTM app ", href = "https://gitlab.com/caminoccg/toolsrtm-simulator"),
                               br(), # linebreak
                               br(), # linebreak
                               a("ToolsRTM package", href = "https://gitlab.com/caminoccg/toolsrtm"),
                               br(),
                               h4("Citation"),
                               p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> packages, please cite the following references:')),

                               p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                               p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                               p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits with Sentinel-2 imagery for monitoring gross
                               primary production and detecting forest disturbances.'),

                           #    div(tags$img(src = "images/JRC.png", height = "100px", width = "300px",align="left")
                            #       , style="text-align: left;"),

                               br(),
                               br(),
                               br(),
                               br(),
                               br(),

                    ) # end well panel
           ), # end tab panel

           tabPanel("About SCOPEinR",
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    wellPanel( style = "background: white",

                               h4('SCOPEinR package'),
                               p(style = "text-align: justify;",HTML('<b>SCOPEinR</b> package is designed for running the Soil Canopy Observation, Photochemistry and Energy fluxes (SCOPE, Van der Tol at al., 2009, Yang et al., 2020) radiative transfer model.')),

                               p(style = "text-align: justify;",'This package simulates reflectance and chorophyll fluorescence emission using the SCOPE model. To make inter-comparison with other main radiative transfer (RT) models is recommeded to install the ToolsRTM package. This ToolsRTM package uses several functions for simulating canopy reflectance at several spectral resolution (1nm, hyper spectral and Sentinel-2).'),

                               "Manual is available at ",
                               tags$a(href="https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_e851c6b2/Notebooks/R/SCOPEinR/SCOPEinR.html",
                                      "ReadTheDocs"),
                               ".",
                               br(),

                               tags$figure(
                                 tags$img(src = "images/rtm_scope.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")

                               ),
                               br(),
                               strong("Fig 1."),'Simulated reflectance spectrum using the SCOPE model.' ,
                               #tags$img(src = "images/rtm_sims.png", height = "450px", width = "500px"),
                               #htmltools::img(src = "images/rtm_sims.png"),

                               br(),
                               br(),
                               tags$figure(
                                 tags$img(src = "images/rtm_scope_SIF.png",height = "450px", width = "500px"),
                                 tags$figcaption("", style = "font-weight: bold;")

                               ),
                               br(),
                               strong("Fig 1."),'Simulated chorohyll fluorescence emission using theSCOPE model.' ,

                               br(),
                               h4("Install SCOPEinR"),
                               p('SCOPEinR is avalaible on gitlab, so you can install using the R console:'),
                               code('git_url <- "https://gitlab.com/caminoccg/scopeinr.git"'),
                               br(),
                               code('install.packages("scopeinr-main.tar.gz",repos = NULL,type = "source")'),

                               # Add information on cranes and prompt user to use slider

                               h4("GitLab repositories"),

                               a("SCOPEinR package", href = "https://gitlab.com/caminoccg/scopeinr"),
                               br(),
                               h4("Citation"),
                               p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> or <b>SCOPEinR</b> package, please cite the following references:')),
                               p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                               p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                   <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                   <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                               p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits
                                 with Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),

                               h4("Citation for SCOPE model"),
                               p(style = "text-align: justify;",'Yang, P., Prikaziuk, E., Verhoef, W., and Van der Tol, C. 2021 "SCOPE 2.0: a model to simulate vegetated land surface
                                         fluxes and satellite signals" Geoscientific Model Development, 14, 4697–4712, https://doi.org/10.5194/gmd-14-4697-2021'),

                               p(style = "text-align: justify;",'Van der Tol, C., W. Verhoef, J Timmermans, A Verhoef, and Z Su. 2009. “An Integrated Model of Soil-Canopy Spectral Radiances,
                                         Photosynthesis, Fluorescence, Temperature and Energy Balance.” Biogeosciences 6 (12): 3109–29, https://doi.org/10.5194/bg-6-3109-2009'),

                               br(),
                               br(),
                               br(),
                               br(),
                               br(),

                    ) # end well panel
           ), # end tab panel
           tabPanel("About Online reflectance simulator",
                    # Beta Version Notice
                    tags$div(
                      style = "color: red; font-weight: bold; font-size: 18px; margin-bottom: 20px;",
                      "⚠️ Beta Version Notice: This app is currently in beta. We welcome feedback and suggestions as we work to improve it!"
                    ),
                    wellPanel( style = "background: white",
                               h4('Online reflectance simulator'),

                               p(style = "text-align: justify;",

                               HTML('<b>Online Reflectance Simulator</b> facilitates the forward-mode execution of primary radiative transfer (RT)
                                    models for simulating canopy reflectance across a range of spectral scales, including hyperspectral and various
                                    satellite resolutions (e.g., Sentinel-2, Landsat 7, 8, MODIS, etc.). In this simulator,
                                    we integrate the <b>ToolsRTM</b> and <b>SCOPEinR</b> packages to simulate reflectance at canopy scales
                                    using widely used 1-D RTMs designed for crops (e.g., PROSAILH, PROSPECT-PRO, FLUSPECT, SCOPE) and forest
                                    canopies (e.g., INFORM, Liberty). These packages provide essential functions for rescaling the native spectral
                                    resolution of RTMs to accommodate different spectral ranges, thereby enhancing the versatility of
                                    reflectance modeling.')),


                               p(style = "text-align: justify;", HTML('This online RT-Simulator can also be run offline using the <code>get.simulator()</code> function,
                                                                      which requires all dependencies for running the Shiny app to be installed on your computer.')),

                               p(style = "text-align: justify;", HTML('The <b>ToolsRTM</b> package includes several functions for LUT generation, estimating plant traits,
                                                                      spectral indices, and generating time series, along with validation of
                                                                      predictions against field observations.')),

                               p(style = "text-align: justify;", HTML('The <b>SCOPEinR</b> package encompasses all the RT models included in the SCOPE model, which are
                                                                      essential for estimating reflectance, fluorescence, and integrating a balanced energy model.')),

                               h4("Citation"),

                               p(style = "text-align: justify;",HTML('If you use <b>ToolsRTM</b> package or <b>SCOPEinR</b> package, please cite the following references:')),

                               p(style = "text-align: justify;",HTML('Camino et al., (2024). RT-Simulator: An Online Platform to Simulate Canopy Reflectance from Biochemical and Structural Plant Properties Using Radiative Transfer Models,
                                           <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 2811-2814,
                                          <a href="https://ieeexplore.ieee.org/document/10642442" target="_blank">doi: 10.1109/IGARSS53475.2024.10642442</a>.')),

                               p(style = "text-align: justify;",HTML('Arano et al., (2024). Enhancing Chlorophyll Content Estimation With Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical Models,
                                          <i>IGARSS 2024 - 2024 IEEE International Geoscience and Remote Sensing Symposium</i>, Athens, Greece, 2024, pp. 4486-4489,
                                          <a href="https://ieeexplore.ieee.org/document/10641613" target="_blank">doi: 10.1109/IGARSS53475.2024.10641613</a>.')),

                               p(style = "text-align: justify;",'Camino et al., (in prep). Integrating physiological plant traits
                                 with Sentinel-2 imagery for monitoring gross primary production and detecting forest disturbances. '),
                               br(),

                               br(),
                             #  div(tags$img(src = "images/JRC.png", height = "100px", width = "300px",align="left")
                              #     , style="text-align: left;"),
                               br(),
                               br(),
                               br(),
                               br(),

                    )
           ) # end tab panel
  )# end navbarMenu

) ##  # Close the references menu

