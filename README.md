<div>
   <p align="center">
   Data* and Code for Our <a href="https://doi.org/10.5194/soil-2021-41"> Research Paper</a>
  </p>
 
 <h3 align="center">Long-Term Impact of Cover Crop and Reduced Disturbance Tillage on Soil Pore Size Distribution and Soil Water Storage</h3>

</div>

<!-- ABOUT THE PROJECT -->
### About The Project
We studied the long-term impact of contrasting tillage and cover cropping systems on soil structure and soil hydraulic properties. We measured water retention and conductivity properties and analyzed the implication of these changes on water storage using numerical simulations in HYDRUS-2D software. Our study concludes that the long-term practices of no-till and cover crop systems were beneficial in terms of changes to the pore size distribution. No-till and cover cropping systems made marginal improvements in soil water conductivity and water storage.


### Contents and Folder Structure
The analysis was done primarily in R. For each `*.R` code file, I have compiled a report with the corresponding file name in markdown (`.md`) and HTML file formats. The folder structure and description are outlined here:  

* **Data_Processed/**: Data tables in `.csv` and/or `.rds` format that have been processed from raw data
* **Data_Raw/**: Raw data exported from lab instruments (*KSAT* and *HYPROP*) and *HYDRUS-2D* simulation output files
* **Plots/**: Contains plots and figures from analysis of data in `.pdf` and/or `png` file formats.
* **Reports/**: Compiled reports of the processing `.R` codes with corresponding file name.
* **".R" files**: R codes: 
   * Files starting with **"01"** to **"04"** are for analyzing laboratory data,
   * Files starting with **"H2D"** are for analyzing Hydrus-2D data


<!-- LICENSE -->
### License

Distributed under the Creative Commons Attribution 4.0 (CC BY 4.0) License. See `Licnese.txt` for more information.

<!-- CONTACT -->
### Contact

Samuel Araya - [@SamuelA209](https://twitter.com/samuela209) - [https://samuelna.netlify.app/](https://samuelna.netlify.app/)

<sub>* HYDRUS-2D Mesh-level data are not available in this repository due to GitHub file size limts. Please contact author for access.</sub>
