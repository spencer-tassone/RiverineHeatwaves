# StreamRiverHeatwaves

Analysis of stream and river heatwaves throghout the USA between 1996-2020 using United States Geological Survey (USGS) data. Examining how heatwaves vary based on stream order, discharge, region, season, time, and position in landscape relative to a reservoir.

The analysis flows as follows:
1. DataPull.R
   - this file pulls the daily mean water temperature and discharge (Q) data from USGS using the 'dataRetrieval' R package, then cleans the datasets according to the:
     - [USGS Daily Data-Value Qualification Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd)
     - [USGS Daily Data-Value Status Codes](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-and-daily-value-status-codes)
2. ResidualQ.R
   - this file calculates a residual Q in order to determine if the observed mean Q on any day was higher or lower than normal. There are three methods we tested to determine how to calculate residual Q:
     - Median Q
     - Rolling 14-day Mean of Median Q
     - Generalized Additive Model (GAM) using Day of Year (DOY) with a cubic-cyclic spline as a preditor of Q
