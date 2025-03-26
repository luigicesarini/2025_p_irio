# What is needed to run the IRIO model

1. _Sample_FullParamSpace_REV1_DiffCons.csv_: read the file containing all the confugurations tested in the estimation.  
A 100000 x 48 matrix containing 100000 possible configurations of the 48 models parameters. Change over time and/or region?
2. _IO_ITAregions.rds_: IO Table  
A 906 x 1010 matrix, where 906 is the number of sector (43) times the regions (21) plus 'Value added at basic prices''Taxes less subsidies on products''Imports', and 1010 is the number of sector (43) times the regions (21) plus 5 categories of consumptions per region plus 'Change in inventories and acquisition less disposal valuables''Exports' = 43 *21 + 5 *21 + 2 = 1010. Chamge over time? Don't think so.
3. _measures_spese_flood.rds_:
4. _H.rds_; Matrix H, 903x903 matrix that endogenizes part of the consumption, translating changes in past periods into changes in final consumption demand. Change in time? I'd say yes
5. _lab_shocks_EROMgeoloc.csv_:Labor shocks using Hazus restoration times. A n_sector x n_weeks matrix having the impacts expressed as percentage of workforce. Changes per event
6. _output_shocks_EROMgeoloc_M.csv_:Labor shocks using insurance claims' business interruption data related to machinery damages. Same dimension as above, but using claims as restoration times. 
7. _LR_shocks_EROMgeoloc_I_met2.csv_: Shocks to inventories using insurance claims' loss ratio. I'm not sure waht it is.


