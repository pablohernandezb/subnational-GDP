# Sub-national GDP Analysis for Venezuela 

## Overview
This project analyzes and visualizes sub-national GDP per capita data for Venezuela at a high-resolution 0.25° x 0.25° grid level. The analysis includes both mainland Venezuela and the Esequibo region, using data from Rossi-Hansberg & Zhang (2025).

## Key Features
- High-resolution GDP per capita mapping (0.25° x 0.25° grid cells)
- Integration of multiple geographical datasets (states, municipalities)
- Custom visualization of territorial boundaries
- Handling of complex geospatial data transformations
- Bilingual output (Spanish/English)

## Technical Skills Demonstrated
- **R Programming**
  - Data manipulation with dplyr
  - Geospatial analysis using sf package
  - Advanced data visualization with ggplot2
  - Complex data joins and transformations

- **Geospatial Analysis**
  - Working with different coordinate reference systems (CRS)
  - Handling shapefiles and raster data
  - Spatial joins and intersections
  - Custom boundary definitions

- **Data Visualization**
  - Custom color palettes
  - Multiple layer mapping
  - Professional-grade cartographic output
  - Distinct styling for different geographical features

## Project Structure
```
├── 0_25deg/                       # High-resolution grid data
├── gadm/                          # Administrative boundary data
├── docs/                          # Documentation
├── *PRINT.png                     # Output visualizations
├── subnational_gdppc_VE.R         # Main analysis script
└── subnational_gdppc_VE_square.R  # Alternative visualization
```

## Data Sources
- GDP Data: Rossi-Hansberg & Zhang (2025)
- Administrative Boundaries: GADM database
- Geographic Features: Various official sources

## Key Visualizations
The project produces several types of maps:
1. GDP per capita distribution across 0.25° grid cells
2. State-level administrative boundaries
3. Special handling of the Esequibo region
4. Combined visualization with multiple geographic layers

## Requirements
```r
library(ggplot2)
library(dplyr)
library(sf)
library(geodata)
library(rnaturalearth)
```

## Usage
1. Clone the repository
2. Ensure all required R packages are installed
3. Run the main script:
```r
source("subnational_gdppc_VE.R")
```

## Outputs
- High-resolution maps in both Spanish and English
- GDP per capita visualizations at various scales
- Custom-styled geographical boundaries
- Publication-ready figures

## License
© 2025 Pablo Hernández Borges

## Acknowledgments
- Data provided by Rossi-Hansberg & Zhang (2025)
- GADM for administrative boundary data
- R Spatial community for tools and packages