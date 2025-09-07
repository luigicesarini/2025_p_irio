# Structure of the shocks

To create the shocks we neeed a previous step that is fixed in time, and for each region.

1. Compute the empluyes for each sector in each region

## 30-05-2025
Three main problems to address:
1. Refactoring the previously named `flood_shock_EROM_insurance.R`, making the script more concise and generalize it to any region/regions. Similar to the work done for HAZUS
that can be found in `src/shock/create_input.R/.py`
2. Generalize the run of the model  


Organize the claims shock generation

**Output_per_BI.R**: generates the database with Marcello's script and save it into /out/shocks/claims/marcello/{year}/EVENT_40840_2015_Campania_River_ul.rds
Input needed:path to the file of the event


then go into 
**create_shocks_claims.r**: generates the shocks for each event and each region and save all the files into out/shocks/claims/{year}/{S,M,Y}/
input needed: path of the outcome of the previous scripts

