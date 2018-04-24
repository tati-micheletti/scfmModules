# Model

probability_of_detection ~ B0 + 
                           B1 * L_CUT +  # Station in recently harvested cutblock (≤30 years) Y/N ==> HarvestStateMap > 0
                           B2 * L_YDEC + # Station in young deciduous forest (31–90 years) Y/N ==> Age map
                           B3 * L_ODEC + # Station in old deciduous forest (>90 years) Y/N ==> Age map
                           B4 * L_MIX + # Station in mixed deciduous/conifer forest (>30 years) Y/N ==> Age map
                           B5 * L_CC + # Area weighted mean forest crown closure ==> ???
                           B6 * L_WDIS + # Distance from station centre to nearest river or lake ==> ???
                             B7 * N_CUT + # Station in recently harvested cutblock (≤30 years) Y/N ==> HarvestStateMap > 0
                             B8 * N_LATE + # Proportion in old forest (90+ year) Y/N ==> Age map
                             B9 * N_DEC + # Proportion in deciduous forest AGE? ==> Age map?
                             B10 * N_MIX + # Station in mixed deciduous/conifer forest (>30 years) Y/N ==> Age map
                             B11 * N_SB + # Presence of black spruce forest Y/N ==> ???
                             B12 * N_RICH # Number of habitat classes in neighbourhood ==> ???

# B... : Get from table Appendix 1
# Values for the variables get from rasters Age Map, Harvest State Map...?

# Questions to Steve:
# 1. How is the fire going to be accounted for in the model? = L_CUT?

# Need to extract from the maps, per year:
