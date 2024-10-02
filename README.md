
### ToolsRTM package

The **ToolsRTM** package provides a comprehensive suite of tools for
simulating canopy reflectance using various radiative transfer (RT)
models at multiple satellite resolutions. Currently in the testing
phase, this package is designed to facilitate detailed simulations,
enabling versatile and accurate analyses of canopy reflectance
characteristics.

For canopy-level simulations, the package features models such as
**INFORM**, **fourSAIL**, and **fourSAIL2**. When it comes to leaf-level
simulations, it includes the **PROSPECT** model (with D and PRO
variants), **Liberty**, and **FLUSPECT** (B-Cx). These models empower
users to conduct sophisticated simulations that capture the intricate
dynamics of reflectance behavior in both leaf and canopy contexts.

Additionally, the **SPART** model (Soil-Plant-Atmosphere Radiative
Transfer model) is tailored for satellite measurements in the solar
spectrum. It integrates three computationally efficient RT models: the
**BSM** model for soil, **PROSAIL** for vegetation canopies, and
**SMAC** for the atmosphere. These components are interconnected using
the four-stream theory and the adding method, allowing SPART to simulate
directional top-of-atmosphere (TOA) spectral observations. This approach
accounts for significant effects, such as sun-observer geometries and
the non-Lambertian reflectance of the land surface.

### Getting started

To install the ToolsRTM package, please follow these steps in R session:

1)  Download the ToolsRTM package as .tar.gz file

```         
# install ToolsRTM
install.packages('pathWithFile/toolsrtm-main.tar.gz',repos = NULL,type = "source"2
```

2.  Check the installed version:

```         
# Check the version of ToolsRTM
packageVersion("ToolsRTM")
```
Note: The last version is 0.60

### Installing the SCOPEinR Package

The **SCOPEinR** package is required for running the Soil Canopy
Observation, Photochemistry and Energy fluxes (SCOPE) model, which
simulates soil, canopy observation, photochemistry, and energy fluxes
(SCOPE, Van der Tol et al., 2009; Yang et al., 2020).

This R package enables to run the SCOPE model developed in MATLAB by Van
der Tol at al. (2009), Yang et al. (2020)

```         
## Install additional SCOPEinR package. 
install.packages('pathWithFile/scopeinr-main.tar.gz',repos = NULL,type = "source")
```

### Manuals

The manuals are accessible through the [Shiny
app](https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/) or
directly within the
[ToolsRTM](https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_ef4421a7/Notebooks/R/ToolsRTM/ToolsRTM.html)
and
[SCOPEinR](https://carlos-camino.shinyapps.io/0-toolsrtm-simulator/_w_ef4421a7/Notebooks/R/SCOPEinR/SCOPEinR.html)
packages. Vignettes are currently under development.

### Citation

If you use the **ToolsRTM** or **SCOPEinR** packages, please consider
citing the following references:

1.  Camino et al. (2024). **RT-Simulator: An Online Platform to Simulate
    Canopy Reflectance from Biochemical and Structural Plant Properties
    Using Radiative Transfer Models**. *IGARSS 2024 - 2024 IEEE
    International Geoscience and Remote Sensing Symposium*, Athens,
    Greece, pp. 2811-2814. [doi:
    10.1109/IGARSS53475.2024.10642442.](10.1109/IGARSS53475.2024.10642442)

2.  Arano et al. (2024). **Enhancing Chlorophyll Content Estimation with
    Sentinel-2 Imagery: A Fusion of Deep Learning and Biophysical
    Models**. *IGARSS 2024 - 2024 IEEE International Geoscience and
    Remote Sensing Symposium*, Athens, Greece, pp. 4486-4489.\
    doi:
    [10.1109/IGARSS53475.2024.10641613](10.1109/IGARSS53475.2024.10641613).

3.  Camino et al. (in preparation). **Integrating Physiological Plant
    Traits with Sentinel-2 Imagery for Monitoring Gross Primary
    Production and Detecting Forest Disturbances**.

### Citation of the main radiative transfer models 

For further details on the the radiative transfer modelsl, please refer
to the original publications by the authors.

#### PROSPECT model

Féret J-B, Gitelson AA, Noble SD & Jacquemoud S, 2017. PROSPECT-D:
Towards modeling leaf optical properties through a complete lifecycle.
Remote Sensing of Environment, 193, 204--215.
<https://doi.org/10.1016/j.rse.2017.03.004>

Féret, J.B., Berger, K., de Boissieu, F., Malenovský, Z., 2021.
PROSPECT-PRO for estimating content of nitrogen-containing leaf proteins
and other carbon-based constituents. Remote Sens. Environ. 252.
<https://doi.org/10.1016/j.rse.2020.112173>

Jacquemoud S, Baret F, Hanocq J-F, 1992. Modeling spectral and
bidirectional soil reflectance. Remote Sensing of Environment, 41,
123--132.
[https://doi.org/10.1016/0034-4257(92)90072-R](https://doi.org/10.1016/0034-4257(92)90072-R){.uri}

Jacquemoud, S., Baret, F., 1990. PROSPECT: a model of leaf optical
properties spectra. Remote Sens. Environ. 34, 75--91.
<https://doi.org/10.1016/0034-4257> (90)90100-Z.

More info: <http://teledetection.ipgp.fr/prosail/>

#### FLUSPECT model

Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016.
Fluspect-B: A model for leaf fluorescence, reflectance and transmittance
spectra. Remote Sens. Environ. 186, 596?615.
<doi:10.1016/j.rse.2016.09.017>

#### Liberty model

Dawson, T. P., Curran, P. J., & Plummer, S. E. (1998). LIBERTY—Modeling the Effects of Leaf Biochemical Concentration on Reflectance Spectra. Remote Sensing of Environment, 65(1), 50–60. <https://doi.org/10.1016/S0034-4257(98)00007-8>

Di Vittorio, A. V. (2009). Enhancing a leaf radiative transfer model to estimate concentrations and in vivo specific absorption coefficients of total carotenoids and chlorophylls a and b from single-needle reflectance and transmittance. Remote Sensing of Environment, 113(9), 1948–1966. <https://doi.org/10.1016/j.rse.2009.05.002> 

#### fourSAIL & fourSAIL-2 models

Verhoef W & Bach H, 2007. Coupled soil--leaf-canopy and atmosphere
radiative transfer modeling to simulate hyperspectral multi-angular
surface reflectance and TOA radiance data. Remote Sensing of
Environment, 109:166-182. <doi:10.1016/j.rse.2006.12.013>

Verhoef W, Jia L, Xiao Q & Su Z, 2007. Unified optical-thermal
four-stream radiative transfer theory for homogeneous vegetation
canopies. IEEE Transactions in Geosciences and Remote Sensing,
45:1808--1822. <https://doi.org/10.1109/TGRS.2007.895844> PROSAIL

Jacquemoud S, Verhoef W, Baret F, Bacour C, Zarco-Tejada PJ, Asner GP,
François C & Ustin SL, 2009. PROSPECT+ SAIL models: A review of use for
vegetation characterization. Remote Sensing of Environment,
113:S56--S66. <https://doi.org/doi:10.1016/j.rse.2008.01.026>

Berger K, Atzberger C, Danner M, D'Urso G, Mauser W, Vuolo F & Hank T
2018. Evaluation of the PROSAIL Model Capabilities for Future
Hyperspectral Model Environments: A Review Study. Remote Sensing, 10:85.
<https://doi.org/10.3390/rs10010085>

#### Invertible Forest Reflectance Model

Atzberger, C., 2000. Development of an Invertible Forest Reflectance
Model: The INFOR- model.

Schlerf, M., Atzberger, C., 2006. Inversion of a forest reflectance
model to estimate structural canopy variables from hyperspectral remote
sensing data. Remote Sens. Environ. 100, 281--294.
<https://doi.org/10.1016/j.rse.2005.10.006>.

Atzberger, C. 2000: Development of an invertible forest reflectance
model: The INFOR-Model.In: Buchroithner (Ed.): A decade of
trans-european remote sensing cooperation. Proceedings of the 20th
EARSeL Symposium Dresden, Germany, 14.-16. June 2000: 39-44.

Rosema, A., Verhoef, W., Noorbergen, H. 1992: A new forest light
interaction model in support of forest monitoring. Remote Sensing of
Environment, 42: 23-41.

Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G.,
Hosgood B. (1996): Estimating leaf biochemistry using the PROSPECT leaf
optical properties model, Remote Sens. Environ., 56:194-202.

Verhoef, W. 1984: Light scattering by leaf layers with application to
canopy reflectance modeling: The SAIL model. Remote Sensing of
Environment, 16: 125-141.

Basic version of INFORM: Clement Atzberger, 1999 INFORM modifications
and validation: Martin Schlerf, 2004-2007

#### The SPART model: a soil-plant-atmosphere radiative transfer model for satellite measurements in the solar spectrum

Yang, P., van der Tol, C., Yin, T., & Verhoef, W. (2020). The SPART
model: A soil-plant-atmosphere radiative transfer model for satellite
measurements in the solar spectrum. Remote Sensing of Environment, 247,
111870.

#### SCOPE model: The Soil Canopy Observation, Photochemistry and Energy fluxes model

Yang, P., Verhoef, W., & van der Tol, C. (2017). The mSCOPE model: A
simple adaptation to the SCOPE model to describe reflectance,
fluorescence and photosynthesis of vertically heterogeneous canopies.
Remote sensing of environment, 201, 1-11.

Yang, P., Prikaziuk, E., Verhoef, W., & Van der Tol, C. (2021). **SCOPE
2.0: A model to simulate vegetated land surface fluxes and satellite
signals**. *Geoscientific Model Development*, 14, 4697–4712.
<https://doi.org/10.5194/gmd-14-4697-2021>.

Van der Tol, C., Verhoef, W., Timmermans, J., Verhoef, A., & Su, Z.
(2009). **An integrated model of soil-canopy spectral radiances,
photosynthesis, fluorescence, temperature, and energy balance**.
*Biogeosciences*, 6(12), 3109–3129.
<https://doi.org/10.5194/bg-6-3109-2009>.

### License

![](https://img.shields.io/badge/License-MIT-yellow.svg)

The **ToolsRTM** package is licensed under the MIT License, allowing for free use, modification, and distribution. This package is available on GitLab, and we encourage contributions and collaborations from the community. For more details, please refer to the LICENSE file in the repository.


