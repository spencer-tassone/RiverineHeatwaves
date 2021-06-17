# StreamRiverHeatwaves

Analysis of stream and river heatwaves throghout the USA between 1996-2020 using United States Geological Survey (USGS) data. Examining how heatwaves vary based on stream order, discharge, region, season, time, and position in landscape relative to a reservoir.

The analysis flows as follows:
1. DataPull.R
+ this file pulls the data from USGS using the 'dataRetrieval' R package then cleans the datasets according to the [USGS quality control codes.](https://help.waterdata.usgs.gov/codes-and-parameters/instantaneous-value-qualification-code-uv_rmk_cd)
