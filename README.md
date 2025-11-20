---
editor_options: 
  markdown: 
    wrap: 72
---

# UCLP DSS

Repo to organize all data/code related to the Upper CLP Decision Support
System.

## Primarily project goals:

-   Create dashboard for all WQ and quanity data relevant to CLP
    Municipal water supplies

-   Collate all relevant in-stream water quality data to create models

-   Create models of in-stream water quality based on in-situ sonde data
    and physical conditions

## Symlink Folders:

The following folders are symlink-ed from the ROSS SharePoint "Poudre
ROSS" document library:

-   `data`: all data related to the Poudre work

-   `docs`: rendered markdowns and figures

-   `creds`: credentials to access data

## Code:

Currently the repo is broken into three folders:

### Source: (`src`)

This is where custom functions are stored.

### Dashboard:

This will store code to create a prototype Shiny dashboard for treatment
operators The current version of the dashboard is available at:
<https://geocentroid.shinyapps.io/UCLP_Alpha_WQ_Dashboard/>
Contact Sam Struthers for username & password for access

### Modeling:

This is where we store R Markdowns for realtime sensor estimates and
forecasting efforts. Knit markdowns are stored in the `docs` folder.

### Scratch:

This folder contains work-in-progress or incomplete scripts.

## License:

The code in this repository is covered by the MIT use license. We
request that all downstream uses of this work be available to the public
when possible.
