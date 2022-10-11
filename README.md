# Histrorical fishing reconstructions for Atlantis GOA

This code process a number of data sources to create catch forcing files for Atlantis GOA hindcast (1990-2020) runs. It also performs a number of other tasks, including exploring catch, observer, and fish ticket data, and creating selectivity curves needed by Atlantis to distribute the catch among age classes of each species.  

The main data sources are:  

1. AKRO-BLEND Catch Accounting Groundfish Total Catch by Fishery for fishing in Alaska.
2. DFO data for British Columbia.
3. IPHC data for Pacific halibut.
4. ADF&G data for salmon landings.
5. NMFS data for non-target species.
6. ADF&G data for crabs.

Selectivity curves for Atlantis are either taken from SAFE reports for Tier 3 stocks or they approximate age at maturity for Tier 4+. 

The code writes out the catch.ts files. 
