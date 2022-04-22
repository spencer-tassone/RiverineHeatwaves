# Riverine Heatwaves

Increasing heatwave frequency in streams and rivers of the United States. This work was submitted for publication to Limnology & Oceanorgraphy Letters in April 2022.

Authors: Spencer J. Tassone*, Alice F. Besterman, Cal D. Buelo, Dat T. Ha, Jonathan A. Walter, Michael L. Pace

*Contact author: sjt7jc@virginia.edu


## Background
Analysis of riverine heatwaves throughout the USA between 1996-2021 using United States Geological Survey (USGS) data. Examining how heatwaves vary based on stream order, discharge, region, season, time, and position in landscape relative to a reservoir.

## The analysis flows as follows:
1. DataPull.R
   - This file pulls the daily mean water temperature and discharge (Q) data from USGS using the 'dataRetrieval' R package, then cleans the datasets according to the:
     - [USGS Daily Data-Value Qualification Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd)
     - [USGS Daily Data-Value Status Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-and-daily-value-status-codes)

   - This part of the analysis identifies 70 USGS stations that have the water temperature data available with 51 sites having concurrent daily mean discarge records that were ≥ 90% complete. Site specific variables included region, altitude, Stahler stream order, and position in landscape relative to a reservoir. Regional assignment was classified according to historically climatic consistent regions of the U.S. ([Karl and Koss 1984](https://www.ncei.noaa.gov/monitoring-references/maps/us-climate-regions)). Altitude was extracted using the ‘dataRetrieval’ R package with [NGVD29 converted to NAVD88 by adding 3.6 feet](https://pubs.usgs.gov/sir/2010/5040/section.html). Stahler stream order was determined using the USGS NHDPlus High Resolution geospatial database using ESRI ArcMap version 10.8 (see Stream Order below). Categorical assignment of site position relative to a reservoir (i.e., above, below, none) was determined from aerial photographic visual inspection of each sites location relative to the U.S. Army Corps of Engineers National Inventory of Dams (NID) in ArcMap (see Reservoir Position below).  See the `Station_Details` and `Station_Details_Metadata` files in this repository for site specific information.
     - Stream Order
     
       - values come from the [USGS NHDPlus High Resolution service, a part of The National Map](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/nhdplus-high-resolution) (data refreshed as of August 2020) which was accessed using ESRI ArcMap ArcGIS server https://hydro.nationalmap.gov/arcgis/services and the layer used was 'FlowDirection'. 
     - Reservoir Position
     
       - categorical assignment comes from visual inspection of each site and was cross-referenced against the [National Inventory of Dams](http://nid.usace.army.mil/), an US Army Corps of Engineers maintained ArcMap feature layer that consists of dams throughout the USA that meet one of four criteria
         1. High hazard potential classification - loss of human life is likely if the dam fails.
         2. Significant hazard potential classification - no probable loss of human life but can cause economic loss, environmental damage, disruption of lifeline facilities, or impact other concerns.
         3. Equal or exceed 25 feet in height and exceed 15 acre-feet in storage.
         4. Equal or exceed 50 acre-feet storage and exceed 6 feet in height.
         
     - Drainage Area: if available, these values were pulled from the site descrition of a station on the USGS website - [see example here](https://waterdata.usgs.gov/nwis/inventory/?site_no=02011400&agency_cd=USGS) where the drainage area for this sites = 157 square miles.
2. ResidualQ.R
   - This file calculates a residual Q in order to determine if the observed mean Q on any day was higher or lower than normal. The expected daily mean discharge was determined using the ‘heatwaveR’ R package which calculates a local seasonally varying discharge climatology based on the supplied 26-year observed daily mean discharge. Residual discharge values > 0 and < 0 indicated above and below normal discharge respectively.

3. SR_HWs.R
   - This file runs the stream and river (SR) heatwave (HW) analyses.
     - [Vignettes and event metric/unit descriptions found here](https://cran.rstudio.com/web/packages/heatwaveR/readme/README.html)
