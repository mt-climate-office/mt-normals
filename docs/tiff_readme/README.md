## Description of Normals Data
This directory contains GeoTIFFs of climate normals for the state of Montna for the 1991 - 2020 reference period. All normals are derived from [gridMET](https://www.climatologylab.org/gridmet.html) climate data. The base gridMET units and coordinate system (WGS 84) are maintained to minimize any errors introduced by unit conversion or reprojection. The units and variable names associated with each of the sub-directories are listed in the table below:

| Abbreviation | Long Name                 | Units                   |
|--------------|---------------------------|-------------------------|
| erc          | Energy Release Component  | NFDRS fire danger index |
| pr           | Precipitation             | mm                      |
| rmax         | Maximum Relative Humidity | %                       |
| rmin         | Minimum Relative Humidity | %                       |
| sph          | Specific Humidity         | kg/kg                   |
| srad         | Solar Radiation           | W/m^2                   |
| tmmn         | Minimum Temperature       | K                       |
| tmmx         | Maximum Temperature       | K                       |
| vpd          | Vapor Pressure Deficit    | kPa                     |
| vs           | Wind Speed                | m/s                     |

Within each subdirectory, files adhere to a naming convention of {time}\_{statistic}.tif, where time is the time period of the normal (e.g., annual, jan, feb, etc.) and statistic is the climate normal summary statistic (e.g., mean, median, etc). Within each directory is another subdirectory titled quantiles/{time}\_{n}th.tif, where n is a quantile value for a given variable. All *.tif* files are stored as [Cloud Otimized GeoTIFFs](https://www.cogeo.org/) (cog) to enhance rendering and display on the web. 

For more details about how the climate normals were created, please visit the information tab on the [Montana Climate Atlas](https://mt-climate-office.github.io/mt-normals/). If you have any other questions about the data, please reach out to Colin Brust at the Montana Climate Office ([colin.brust@mso.umt.edu](colin.brust@mso.umt.edu)).