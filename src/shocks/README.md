# Structure of the shocks

To create the shocks we neeed a previous step that is fixed in time, and for each region.

1. Compute the empluyes for each sector in each region

## 30-05-2025
Three main problems to address:
1. Refactoring the previously named `flood_shock_EROM_insurance.R`, making the script more concise and generalize it to any region/regions. Similar to the work done for HAZUS
that can be found in `src/shock/create_input.R/.py`
2. Generalize the run of the model  
