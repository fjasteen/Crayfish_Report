# Craywatch project

This repository contains the source files for the [Craywatch website](https://craywatch.inbo.be) and the analyses included in the Craywatch report.

## Website Usage

This website makes use of the static website generator [Jekyll](https://jekyllrb.com/) and the [Petridish](https://github.com/peterdesmet/petridish) theme. **Each commit to `main` will automatically trigger a new build on GitHub Pages.** There is no need to build the site locally, but you can by installing Jekyll and running `bundle exec jekyll serve`.

## Repo structure

The repository structure follows that of Jekyll websites.

```
├── README.md              : Description of this repository
├── LICENSE                : Repository license
├── .gitignore             : Files and directories to be ignored by git
├── _config.yml            : General site settings
├── Gemfile                : ?
├── Gemfile.lock           : ?
├── favicon.ico            : Website icon
├── pages                  : Website pages and their content
├── assets                 : Images and static files
├── _layouts               : Website layout
├── _data                  
│   ├── footer.yml             : Footer content
│   ├── navigation.yml         : Top navigation
│   └── team.yml               : Team members
└── R
    ├── images 
           ├── banner_bottom.png    : Bottom banner
           ├── map.png              : Map image for newsletter
           └── treemap.png          : Treemap of provinces for newsletter
    ├── data               : Data used and generated in src (input, output, observations)
    └── src
         ├── operational            : To select, reserve and update the reservation status of sample locations
              ├── 01_Select_Localities.R          : Processes the local KML-file from Google MyMpas and generates an updated CSV-file
              ├── 02_Update_observations.R        : Reads and processes new exports from Observations.be 
              ├── 03_Process_observation_data.R   : Combines validated observations-data and generates a craywatch dataset
              ├── 04_analyze_validated_data.R     : Generates a dataset for analysis that combines filtered craywatch data and GBIF data since 2000
              ├── Readme_Update_observations.docx : Description of the operational source files
              ├── utils                           : Supportive operational tools
              └── communication                   : Generates e-mails and newsletters
                 
         └── analysis               : Analyses for the Craywatch report
              ├── config.R                        : Central settings for craywatch data pipe
              ├── 01_prepare_craywatch_data.R     : Reads and checks validated craywatch data and aggregates per session
              ├── 02_prepare_gbif_data.R          : Downloads and filters GBIF data
              ├── 03_merge_craywatch-gbif.R       : Combines craywatch and GBIF data into analysis dataset
              ├── 04_link_vhag-wclc.R             : Link crayfish data to waterbody codes
              ├── 05_deelbekken_maps.R            : Generates maps per species of their status in the FLemish water basins
              ├── 06_deelbekken_leaflet.R         : Generates a leaflet map including all species and their status in the water basins
              ├── 07_deelbekken_tables.R          : Generates a matrix table of species presence in the water basins
              ├── 08_load_aq_sbz.R                : Load and filter shapefiles for protected areas and prepare crayfish data
              ├── 09_aq_sbz_analyse.R             : Generates static maps of crayfish presence relative to protected areas
              ├── 10_sbp_analyse_afstanden.R      : Generates a table with crayfish presences in relation to areas with species protection programs
              ├── 11_distance_hbtrl_tables.R      : Generates a table with crayfish presences in relation to protected habitats
              ├── 12_sbz_leaflet.R                : Generates an interactive leaflet map of crayfish presences and protected areas
              ├── 13_urbanisation_plot.R          : Creates plot of distribution of crayfish caught in different levels of urbanisation
              ├── 14_watertype_plot_glmm.R        : Creates plot of distribution of crayfish caught in different water types
              ├── 15_watertype_glmm.R             : Analyzes the presence of crayfish species in different water types through GLMM
              ├── 16_citizenscience_tables.R      : Metadata analysis of citizen science data
              ├── 17_craywatch_maps.R             : Generates static Craywatch maps
              ├── 18_cpue_maps_trends.R           : Generates distribution maps with crayfish presences, absences and CPUE
              ├── 19_gridcell_trend_plot.R        : Analysis of expansion of crayfish species & generate plot
              ├── 20_Prepare_fc_data.R            : Validation of physicochemical data
              ├── 21_link_fc_craywatch.R          : Links crayfish sample points to physicochemical sample points
              ├── 22_craywatch_FC_trend.Rmd       : Physicochemical data trend evaluation
              ├── 23_fc_plots.R                   : Calculate and visualise correlations between physicochemical parameters and crayfish presence/CPUE
              └── 24_fc_PCA.R                     : Run PCA 
              
```

## Data sources
The raw data file (first_data.csv) generated in src/operational/03_Process_observation_data.R was manually validated to correct entry errors and discrepancies in the data registered by the volunteers. The resulting validated file (craywatch_data.csv) is the source file for src/analysis/01_prepare_craywatch_data.R (as defined in src/analysis/config.R).

## License

This work is licensed under a [Creative Commons Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/).
