# StreamRiverHeatwaves

Analysis of stream and river heatwaves throghout the USA between 1996-2020 using United States Geological Survey (USGS) data. Examining how heatwaves vary based on stream order, discharge, region, season, time, and position in landscape relative to a reservoir.

The analysis flows as follows:
1. DataPull.R
   - This file pulls the daily mean water temperature and discharge (Q) data from USGS using the 'dataRetrieval' R package, then cleans the datasets according to the:
     - [USGS Daily Data-Value Qualification Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd)
     - [USGS Daily Data-Value Status Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-and-daily-value-status-codes)

   - This part of the analysis identifies 131 USGS stations that have the water temperature data availability. For those sites, station level data is added to the `lat_long` file after getting exported as a .csv file. This additional information identifies the Stahler Stream Order, position in landscape relative to a reservoir, and drainage area (mi2). See the `LatLong_StreamOrder_Reservoir_DrainageArea` file.
     - Stream Order
     
       - values come from the [USGS NHDPlus High Resolution service, a part of The National Map](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/nhdplus-high-resolution) (data refreshed as of August 2020) which was accessed using ESRI ArcMap ArcGIS server https://hydro.nationalmap.gov/arcgis/services and the layer used was 'FlowDirection'. 
     - Reservoir Position
     
       - categorical assignment comes from visual inspection of each site and was cross-referenced against the [National Inventory of Dams](http://nid.usace.army.mil/), an US Army Corps of Engineers maintained ArcMap feature layer that consists of dams throughout the USA that meet one of four criteria
         1. High hazard potential classification - loss of human life is likely if the dam fails.
         2. Significant hazard potential classification - no probable loss of human life but can cause economic loss, environmental damage, disruption of lifeline facilities, or impact other concerns.
         3. Equal or exceed 25 feet in height and exceed 15 acre-feet in storage.
         4. Equal or exceed 50 acre-feet storage and exceed 6 feet in height.
         
     - Drainage Area: if available, these values were pulled from the site descrition of a station on the USGS website - [see exaple here](https://waterdata.usgs.gov/nwis/inventory/?site_no=02011400&agency_cd=USGS) where the drainage area for this sites = 157 square miles.
2. ResidualQ.R
   - This file calculates a residual Q in order to determine if the observed mean Q on any day was higher or lower than normal. There are three methods we tested to determine how to calculate residual Q:
     - Median Q
     - Rolling 14-day Mean of Median Q
     - Generalized Additive Model (GAM) using Day of Year (DoY) with a cubic-cyclic spline as a preditor of Q

3. SR_HWs.R
   - This file runs the stream and river (SR) heatwave (HW) analyses.
     - [Vignettes and event metric/unit descriptions found here](https://cran.rstudio.com/web/packages/heatwaveR/readme/README.html)
