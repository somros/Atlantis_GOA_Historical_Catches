# Alberto Rovellini
# August 26 2022
# Code to explore the catch length composition from observer data, convert it to age composition, and work out age composition of the catch
# The goal is to build the CatchTS_agedistribXXX parameters for groups without selectivity patterns from stock assessments (e.g. Tier >3)
# We only need this for vertebrates

# NORPAC Lenght Report data retrieved from AKFIN Answers on August 26 2022
# https://akfinbi.psmfc.org/analytics/saw.dll?Dashboard&PortalPath=%2fshared%2fStock%20Assessment%2f_portal%2fStock%20Assessment&Page=NORPAC%20Length%20Report&Done=Dashboard%26PortalPath%3d%252fshared%252fStock%2520Assessment%252f_portal%252fStock%2520Assessment%26Page%3dObserver%2520and%2520EM%2520Data%26ViewState%3duggn42gl9p6okl96jimfkrtb0a

# Fields:
# Year: 1990:2020
# FMP Area: GOA
# FMP Subarea: CG, SE, WG, WOC, WY
# NMFS Area: --Select Value--
# Gear Code: --Select Value--
# Gear Description: --Select Value--
# Performance: --Select Value--
# Performance Description: --Select Value--
# Species Code: --Select Value--
# Species Name:ALASKA PLAICE,ALASKA SKATE,ALEUTIAN SKATE,ARROWTOOTH FLOUNDER,ATKA MACKEREL,BERING SKATE,BIG SKATE,BIGMOUTH SCULPIN,BLACK ROCKFISH,BLACKTAIL SNAILFISH GROUP,BLUE SHARK,BOCACCIO ROCKFISH,BUTTER SOLE,BUTTERFLY SKATE,CALIFORNIA TONGUEFISH,CANARY ROCKFISH,CHINOOK SALMON,CHUM SALMON,COHO SALMON,COMMANDER SKATE,DARK BLOTCHED ROCKFISH,DARK ROCKFISH,DEEPSEA SKATE,DEEPSEA SOLE,DOVER SOLE,DUSKY ROCKFISH,DUSKY ROCKFISH UNIDENTIFIED,ENGLISH SOLE,FLATHEAD SOLE,GIANT GRENADIER,GREAT SCULPIN,GRENADIER UNIDENTIFIED,HARLEQUIN ROCKFISH,JACK MACKEREL,KAMCHATKA FLOUNDER,KELP GREENLING,LINGCOD,LONGHEAD DAB,LONGNOSE LANCETFISH,LONGNOSE SKATE,LONGSPINE THORNYHEAD ROCKFISH,MUD SKATE,NORTHERN ROCK SOLE,NORTHERN ROCKFISH,OLIVE ROCKFISH,PACIFIC COD,PACIFIC FLATNOSE,PACIFIC GRENADIER,PACIFIC HAKE,PACIFIC HALIBUT,PACIFIC HERRING,PACIFIC MACKEREL,PACIFIC OCEAN PERCH,PACIFIC SLEEPER SHARK,PACIFIC TOMCOD,PARALOMIS MULTISPINA,PARALOMIS VERILLI,PETRALE SOLE,PINK SALMON,PLAIN SCULPIN,POLLOCK,QUILLBACK ROCKFISH,RED BANDED ROCKFISH,REDSTRIPE ROCKFISH,REX SOLE,ROCK SOLE UNIDENTIFIED,ROUGHEYE ROCKFISH,ROUGHTAIL SKATE,SABLEFISH (BLACKCOD),SAFFRON COD,SALMON SHARK,SAND SOLE,SHARPCHIN ROCKFISH,SHORTBELLY ROCKFISH,SHORTRAKER ROCKFISH,SHORTRAKER/ROUGHEYE ROCKFISH,SHORTSPINE THORNYHEAD,SILVERGRAY ROCKFISH,SKATE UNIDENTIFIED,SNAILFISH UNIDENTIFIED,SOCKEYE SALMON,SOUPFIN SHARK,SOUTHERN ROCK SOLE,SPINY DOGFISH SHARK,SPLITNOSE ROCKFISH,STARRY FLOUNDER,THORNYHEAD ROCKFISH UNIDENT,THRESHER SHARK,TIGER ROCKFISH,WARTY SCULPIN,WHITEBLOTCHED SKATE,WHITEBROW SKATE,WIDOW ROCKFISH,YELLOW IRISH LORD,YELLOWEYE ROCKFISH,YELLOWFIN SOLE,YELLOWMOUTH ROCKFISH,YELLOWTAIL ROCKFISH
# Species Sex: --Select Value--
# Length Greater Than: --
