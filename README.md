# UCLP DSS

Repo to organize all data/code related to the Upper CLP Decision Support System. 

## Primarily project goals:

-   Create dashboard for all WQ and quanity data relevant to CLP Municipal water supplies

-   Collate all relevant in-stream water quality data to create models

-   Create models of in-stream water quality based on in-situ sonde data and physical conditions

## Data:

Sensor data is housed in Poudre Sonde Network OneDrive folder managed by Katie Willi. Please reach out to Katie Willi or Sam Struthers for access

For code to function properly, KW's `poudre_sonde_network/data` folder must be symlinked to your local `data` folder.

ROSS Grab sample data is updated bi-annually to Zenodo folder (<https://zenodo.org/records/12752311>)

Fort Collins Grab sample data is managed/shared by Jared Heath and Diana Schmidt

## Code:

Currently the repo is broken into three folder:

### Source:

This is where scripts to pull in data and other functions will live.

### Dashboard:

This will store code to create a prototype Shiny dashboard for treatment operators The current version of the dashboard is available at : <https://geocentroid.shinyapps.io/UCLP_WQ_Dashboard/>

### Modeling:

This is where we will organize our data and test models to predict grab sample analytes from sensor data. Data will be

## License:

The code in this repository is covered by the MIT use license. We request that all downstream uses of this work be available to the public when possible.
