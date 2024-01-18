---
editor_options: 
  markdown: 
    wrap: 72
---

# ToolsRTM

An R packages with tools for simulating canopy reflectance using a set of radiative transfer (RT) models at several satellite resolutions


### Getting started

To install the ToolsRTM package, please follow these instructions in R session:

1)  Download the ToolsRTM package as .tar.gz file

```         
## install SCOPEinR

install.packages('pathWithFile/ToolsRTM-main.tar.gz',repos = NULL,type = "source")
```

2.  Additional package SCOPEinR package is needed for the SCOPE model.

A R package for running the Soil Canopy Observation, Photochemistry and Energy fluxes (SCOPE, Van der Tol at al., 2009, Yang et al., 2020) radiative transfer model developed in MATLAB.

```         
## Install additional SCOPEinR package. 
# SCOPEinR uses inversion method and main RT models at leaf and canopy scales (INFORM, PROSAIL-2, PROSPECT-D and PRO, Liberty)

install.packages('pathWithFile/SCOPEinR-main.tar.gz',repos = NULL,type = "source")
```

## Manual

Manual is available at ReadTheDocs
<https://toolsrtm-tutorial.readthedocs.io/en/latest/> (in progress)

## Description

This package integrates the main radiative transfer (RT) models for
simulating canopy reflectance at hyperspectral and Sentinel-2 scales.
This package uses several functions for estimating plant traits,
spectral indices and useful functions for generating time series,
validation of the predictions with field observations ....

## Citation

If you use ToolSRTM, please cite the following references:

## PROSPECT model

Authors:Jean-Baptiste FERET
([jb.feret\@teledetection.fr](mailto:jb.feret@teledetection.fr){.email});
Frédéric BARET
([baret\@avignon.inra.fr](mailto:baret@avignon.inra.fr){.email});
Stephane JACQUEMOUD
([jacquemoud\@ipgp.fr](mailto:jacquemoud@ipgp.fr){.email})

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

Basic version of PROSPECT-D and PROSPECT-PRO: Féret J.-B., 2021

## fourSAIL & fourSAIL-2 models

Authors: Verhoef W & Bach H

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

More info: <http://teledetection.ipgp.fr/prosail/>

Basic version of fourSAIL and fourSAIL2: Verhoef W., Bach. H. fourSAILs
modifications: Féret J.-B., 2021

## FLUSPECT model

Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016.
Fluspect-B: A model for leaf fluorescence, reflectance and transmittance
spectra. Remote Sens. Environ. 186, 596?615.
<doi:10.1016/j.rse.2016.09.017>

## Invertible Forest Reflectance Model

Authors:Atzberger, C.; Schlerf, M.

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

# The SPART model: a soil-plant-atmosphere radiative transfer model for satellite measurements in the solar spectrum

The model uses three computationally efficient RTMs for soil (BSM),
vegetation canopies (PROSAIL) and atmosphere (SMAC), respectively. The
sub-models are coupled by using the four-stream theory and the adding
method. The resulting \`Soil-Plant-Atmosphere Radiative Transfer model'
(SPART) simulates directional TOA spectral observations, with all major
effects included, such as sun-observer geometries and non-Lambertian
reflectance of the land surface.

Authors:Yang Peiqgiyang

Yang, P., van der Tol, C., Yin, T., & Verhoef, W. (2020). The SPART
model: A soil-plant-atmosphere radiative transfer model for satellite
measurements in the solar spectrum. Remote Sensing of Environment, 247,
111870.

For the details of the radiative transfer modelling

Yang, P., Verhoef, W., & van der Tol, C. (2017). The mSCOPE model: A
simple adaptation to the SCOPE model to describe reflectance,
fluorescence and photosynthesis of vertically heterogeneous canopies.
Remote sensing of environment, 201, 1-11.

## License

For open source projects, say how it is licensed.
