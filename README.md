# ToolsRTM

An R packages with tools for simulating canopy reflectance using a set of radiative transfer (RT) models at Sentinel-2 scale


## Getting started

Install the package toolsrtm with the following command line in R session:
>
> devtools::install_gitlab('caminoccg/toolsrtm')
>

( This package is in a private repository for this reason this step doesn't work)

## Manual 

Manual is available at ReadTheDocs https://toolsrtm-tutorial.readthedocs.io/en/latest/ (in progress)

## Description

This package integrates the main radiative transfer (RT) models for simulating canopy reflectance at  hyperspectral and Sentinel-2 scales. This package uses several functions for estimating plant traits, spectral indices and useful functions for generating time series, validation of the predictions with field observations ....

## Citation

If you use ToolSRTM, please cite the following references:

## PROSPECT model

Authors:Jean-Baptiste FERET (jb.feret@teledetection.fr); Frédéric BARET (baret@avignon.inra.fr); Stephane JACQUEMOUD  (jacquemoud@ipgp.fr)

Féret J-B, Gitelson AA, Noble SD & Jacquemoud S, 2017. PROSPECT-D: Towards modeling leaf optical properties through a complete lifecycle. Remote Sensing of Environment, 193, 204–215. https://doi.org/10.1016/j.rse.2017.03.004

Féret, J.B., Berger, K., de Boissieu, F., Malenovský, Z., 2021. PROSPECT-PRO for estimating content of nitrogen-containing leaf proteins and other carbon-based constituents. Remote Sens. Environ. 252. https://doi.org/10.1016/j.rse.2020.112173

Jacquemoud S, Baret F, Hanocq J-F, 1992. Modeling spectral and bidirectional soil reflectance. Remote Sensing of Environment, 41, 123–132. https://doi.org/10.1016/0034-4257(92)90072-R

Jacquemoud, S., Baret, F., 1990. PROSPECT: a model of leaf optical properties spectra. Remote Sens. Environ. 34, 75–91. https://doi.org/10.1016/0034-4257 (90)90100-Z.

More info:
http://teledetection.ipgp.fr/prosail/

Basic version of PROSPECT-D and PROSPECT-PRO: Féret J.-B., 2021

## fourSAIL & fourSAIL-2 models

Authors: Verhoef W & Bach H 

Verhoef W & Bach H, 2007. Coupled soil–leaf-canopy and atmosphere radiative transfer modeling to simulate hyperspectral multi-angular surface reflectance and TOA radiance data. Remote Sensing of Environment, 109:166-182. doi:10.1016/j.rse.2006.12.013

Verhoef W, Jia L, Xiao Q & Su Z, 2007. Unified optical-thermal four-stream radiative transfer theory for homogeneous vegetation canopies. IEEE Transactions in Geosciences and Remote Sensing, 45:1808–1822. https://doi.org/10.1109/TGRS.2007.895844
PROSAIL

Jacquemoud S, Verhoef W, Baret F, Bacour C, Zarco-Tejada PJ, Asner GP, François C & Ustin SL, 2009. PROSPECT+ SAIL models: A review of use for vegetation characterization. Remote Sensing of Environment, 113:S56–S66. https://doi.org/doi:10.1016/j.rse.2008.01.026

Berger K, Atzberger C, Danner M, D’Urso G, Mauser W, Vuolo F & Hank T 2018. Evaluation of the PROSAIL Model Capabilities for Future Hyperspectral Model Environments: A Review Study. Remote Sensing, 10:85. https://doi.org/10.3390/rs10010085

More info:
http://teledetection.ipgp.fr/prosail/

Basic version of fourSAIL and fourSAIL2: Verhoef W., Bach. H. 
fourSAILs modifications: Féret J.-B., 2021


## FLUSPECT model

Vilfan, N., van der Tol, C., Muller, O., Rascher, U., Verhoef, W., 2016.
Fluspect-B: A model for leaf fluorescence, reflectance and transmittance
spectra. Remote Sens. Environ. 186, 596?615. doi:10.1016/j.rse.2016.09.017

## Invertible Forest Reflectance  Model

Authors:Atzberger, C.; Schlerf, M.

Atzberger, C., 2000. Development of an Invertible Forest Reflectance Model: The INFOR- model.

Schlerf, M., Atzberger, C., 2006. Inversion of a forest reflectance model to estimate structural canopy variables from hyperspectral remote sensing data. Remote Sens. Environ. 100, 281–294. https://doi.org/10.1016/j.rse.2005.10.006.

Atzberger, C. 2000: Development of an invertible forest reflectance model: The INFOR-Model.In: Buchroithner (Ed.): A decade of trans-european remote sensing cooperation. Proceedings of the 20th EARSeL Symposium Dresden, Germany, 14.-16. June 2000: 39-44.

Rosema, A., Verhoef, W., Noorbergen, H. 1992: A new forest light interaction model in support of forest
monitoring. Remote Sensing of Environment, 42: 23-41.

Jacquemoud S., Ustin S.L., Verdebout J., Schmuck G., Andreoli G., Hosgood B. (1996): Estimating leaf
biochemistry using the PROSPECT leaf optical properties model, Remote Sens. Environ., 56:194-202.

Verhoef, W. 1984: Light scattering by leaf layers with application to canopy reflectance modeling: The
SAIL model. Remote Sensing of Environment, 16: 125-141.

Basic version of INFORM: Clement Atzberger, 1999
INFORM modifications and validation: Martin Schlerf, 2004-2007

# Soil Canopy Observation, Photochemistry and Energy fluxes model (SCOPE)

Authors:Christiaan van der Tol

G.James Collatz, J.Timothy Ball, Cyril Grivet, and Joseph A Berry. Physiological and environmental regulation of stomatal conductance, photosynthesis and transpiration: a model that includes a laminar boundary layer. Agric. For. Meteorol., 54(2-4):107–136, apr 1991. URL: https://www.sciencedirect.com/science/article/pii/0168192391900028, doi:10.1016/0168-1923(91)90002-8.

GJ Collatz, M Ribas-Carbo, and JA Berry. Coupled Photosynthesis-Stomatal Conductance Model for Leaves of C \textless sub\textgreater 4\textless /sub\textgreater Plants. Aust. J. Plant Physiol., 19(5):519, 1992. URL: http://www.publish.csiro.au/?paper=PP9920519, doi:10.1071/PP9920519.

Albert Porcar-Castell. A high-resolution portrait of the annual dynamics of photochemical and non-photochemical quenching in needles of Pinus sylvestris. Physiol. Plant., 143(2):139–153, oct 2011. URL: http://www.ncbi.nlm.nih.gov/pubmed/21615415 http://doi.wiley.com/10.1111/j.1399-3054.2011.01488.x, doi:10.1111/j.1399-3054.2011.01488.x.

G. Schaepman-Strub, M. E. Schaepman, T. H. Painter, S. Dangel, and J. V. Martonchik. Reflectance quantities in optical remote sensing-definitions and case studies. Remote Sens. Environ., 103(1):27–42, 2006. doi:10.1016/j.rse.2006.03.002.

Christiaan van der Tol, Micol Rossini, Sergio Cogliati, Wouter Verhoef, Roberto Colombo, Uwe Rascher, and Gina Mohammed. A model and measurement comparison of diurnal cycles of sun-induced chlorophyll fluorescence of crops. Remote Sens. Environ., 186:663–677, dec 2016. URL: https://www.sciencedirect.com/science/article/pii/S0034425716303649, doi:10.1016/j.rse.2016.09.021.

Wout. Verhoef and Nationaal Lucht- en Ruimtevaartlaboratorium (Netherlands). Theory of radiative transfer models applied in optical remote sensing of vegetation canopies. [publisher not identified], 1998. ISBN 9054858044. URL: https://library.wur.nl/WebQuery/wda/945481.

Wouter Verhoef, Christiaan van der Tol, and Elizabeth M. Middleton. Hyperspectral radiative transfer modeling to explore the combined retrieval of biophysical parameters and canopy fluorescence from FLEX – Sentinel-3 tandem mission multi-sensor data. Remote Sens. Environ., 204(August 2016):942–963, 2018. URL: https://doi.org/10.1016/j.rse.2017.08.006, doi:10.1016/j.rse.2017.08.006.

Nastassia Vilfan, Christiaan van der Tol, Onno Muller, Uwe Rascher, and Wouter Verhoef. Fluspect-B: A model for leaf fluorescence, reflectance and transmittance spectra. Remote Sens. Environ., 186:596–615, 2016. URL: http://dx.doi.org/10.1016/j.rse.2016.09.017, doi:10.1016/j.rse.2016.09.017.

Peiqi Yang, Wout Verhoef, and Christiaan van der Tol. The mSCOPE model: A simple adaptation to the SCOPE model to describe reflectance, fluorescence and photosynthesis of vertically heterogeneous canopies. Remote Sens. Environ., 201:1–11, nov 2017. URL: https://www.sciencedirect.com/science/article/pii/S0034425717303954, doi:10.1016/j.rse.2017.08.029.

Xinyou Yin, Jeremy harbinson and Paul C. Struix. Mathematical review of literature to assess alternative electron transports and interphotosystem excitation partitioning of steady-state C3 photosynthesis under limiting light. Plant, Cell Environ., 29(9):1771–1782, sep 2006. URL: http://doi.wiley.com/10.1111/j.1365-3040.2006.01554.x, doi:10.1111/j.1365-3040.2006.01554.x.

Xinyou Yin and Paul C. Struik. Crop systems biology as an avenue to bridge applied crop science and fundamental plant biology. In Proc. - 2012 IEEE 4th Int. Symp. Plant Growth Model. Simulation, Vis. Appl. PMA 2012, 15–17. IEEE, oct 2012. URL: http://ieeexplore.ieee.org/document/6524806/, doi:10.1109/PMA.2012.6524806.

C Van der Tol, J A Berry, P K E Campbell, and U Rascher. Models of fluorescence and photosynthesis for interpreting measurements of solar-induced chlorophyll fluorescence. J. Geophys. Res. Biogeosciences, 119(12):2312–2327, 2014.

C. Van der Tol, W. Verhoef, J Timmermans, A Verhoef, and Z Su. An integrated model of soil-canopy spectral radiances, photosynthesis, fluorescence, temperature and energy balance. Biogeosciences, 6(12):3109–3129, dec 2009. URL: www.biogeosciences.net/6/3109/2009/, doi:10.5194/bg-6-3109-2009.

## License
For open source projects, say how it is licensed.


