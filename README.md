Cumulative Rainfall Review
==========================
Background
----------
Daily rainfall totals can be compared to long term averages. Daily totals will vary depending on prevailing weather conditions and will show wetter or dryer periods than average.

This routine sets out to present trends around the long term average over a Jul-Jun 12 month period (southern hemisphere Winter-Spring-Summer-Autumn-Winter period). This is useful in assessing likelihood of droughty conditions if prevailing rainfall is trending low from the beginning of a period.

Description
-----------
Two scripts are provided here. The first, **DailyRainfall_CreateReferenceData_10yrs.R**, creates reference data for a user-specified time period (10 years is used in the script). This reference data is stored locally on a file share and then re-used in the second script, **DailyRainfall_Accumulation_Outputs.R** for comparison against daily rainfalls and the output of charts for each rainfall station.

NOTE:
-----
1. Scripts are written for data in a Hilltop Server environment. For other time series servers, you will need to ammend
  * the arguments used in the URL's to query the data
  * the XPATH used to extract data from the XML responses
2. Scripts use BASE graphics. 

Acknowledgements
----------------
Thanks to the Catchment Data team of Horizons Regional Council for review and helpful comments in the development of this script. Many thanks also to the creators and maintainers of the XML, reshape2, zoo and hydroTSM library's.
