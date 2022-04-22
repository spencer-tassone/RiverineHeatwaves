# Riverine Heatwaves

Increasing heatwave frequency in streams and rivers of the United States. This work was submitted for publication to Limnology & Oceanorgraphy Letters in April 2022.

Authors: Spencer J. Tassone*, Alice F. Besterman, Cal D. Buelo, Dat T. Ha, Jonathan A. Walter, Michael L. Pace

*Contact author: sjt7jc@virginia.edu


## Background
River water temperature trends have been increasing since the 20th century. The drivers of these trends are related, in part, to increased diffusive vertical heat flux across the air-water surface from increasing atmospheric temperatures and alterations in water supply that increase advective heat fluxes. With projected climate warming, these heat fluxes are expected to intensify throughout the 21st century, increasing riverine water temperatures in most locations. 

Rising water temperatures have spurred interest in anomalous extreme temperature events called heatwaves due to their potentially disproportionate impact. Positive trends in heatwave frequency, duration, and intensity have been documented in ocean, coastal, and lake ecosystems. Aquatic heatwaves have been linked to shifts in ecosystem metabolism, organism mortality, and poor water quality. However, little is known regarding heatwaves in riverine ecosystems despite their low thermal inertia and evidence of warming.

In this study we document the frequency, duration, and intensity of riverine heatwaves using in-situ water temperature measurements from 1996-2021 for sites located throughout the U.S. We also compare regional and seasonal patterns in heatwaves and test relationships between heatwave characteristics, atmospheric temperature, precipitation, discharge, stream order, and position relative to reservoirs.


## The analysis flows as follows:
1. DataPull.R
   - This file pulls the daily mean water temperature and discharge (Q) data from USGS using the ```dataRetrieval``` R package, then cleans the datasets according to the:
     - [USGS Daily Data-Value Qualification Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd)
     - [USGS Daily Data-Value Status Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-and-daily-value-status-codes)

   - This part of the analysis identifies 70 USGS stations that have the water temperature data available with 51 sites having concurrent daily mean discarge records that were ≥ 90% complete. Site specific variables included region, Stahler stream order, and position in landscape relative to a reservoir. Regional assignment was classified according to historically climatic consistent regions of the U.S. ([Karl and Koss 1984](https://www.ncei.noaa.gov/monitoring-references/maps/us-climate-regions)). Stahler stream order was determined using the USGS NHDPlus High Resolution geospatial database using ESRI ArcMap version 10.8 (see Stream Order below). Categorical assignment of site position relative to a reservoir (i.e., above, below, none) was determined from aerial photographic visual inspection of each sites location relative to the U.S. Army Corps of Engineers National Inventory of Dams (NID) in ArcMap (see Reservoir Position below).  See the `Station_Details.csv` and `Station_Details_METADATA` files in this repository for the site specific data and associated metadata respectively.
     - Stream Order
     
       - values come from the [USGS NHDPlus High Resolution service, a part of The National Map](https://www.usgs.gov/core-science-systems/ngp/national-hydrography/nhdplus-high-resolution) (data refreshed as of August 2020) which was accessed using ESRI ArcMap ArcGIS server https://hydro.nationalmap.gov/arcgis/services and the layer used was 'FlowDirection'. 
     - Reservoir Position
     
       - categorical assignment comes from visual inspection of each site and was cross-referenced against the [National Inventory of Dams](http://nid.usace.army.mil/), an US Army Corps of Engineers maintained ArcMap feature layer that consists of dams throughout the USA that meet one of four criteria
         1. High hazard potential classification - loss of human life is likely if the dam fails.
         2. Significant hazard potential classification - no probable loss of human life but can cause economic loss, environmental damage, disruption of lifeline facilities, or impact other concerns.
         3. Equal or exceed 25 feet in height and exceed 15 acre-feet in storage.
         4. Equal or exceed 50 acre-feet storage and exceed 6 feet in height.
         
2. ResidualQ.R
   - This file calculates a residual Q in order to determine if the observed mean Q on any day was higher or lower than normal. The expected daily mean discharge was determined using the ‘heatwaveR’ R package which calculates a local seasonally varying discharge climatology based on the supplied 26-year observed daily mean discharge. Residual discharge values > 0 and < 0 indicated above and below normal discharge respectively.

3. SR_HWs.R
   - This file runs the stream and river (SR) heatwave (HW) analyses.
     - [Vignettes and event metric/unit descriptions found here](https://cran.rstudio.com/web/packages/heatwaveR/readme/README.html)
