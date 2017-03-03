# After selecting the environment options, the script can be run as a
#  stand-alone initialization, but to preserve the environment one should
#  put them directly in the session start-up scripts.
#
# Initialize the SAC Enviornment 
export SACHOME=/usr/local/cwp/sac
export PATH=${PATH}:${SACHOME}/bin
export SACAUX=${SACHOME}/aux

# Uncomment the desired optons below
#
# SAC_DISPLAY_COPYRIGHT 
# Undefined or 1 -- Show Copyright Notice on Initialization ( Default )
#              0 -- Do not show Copyright Notice
# The copyright is only displayed during the initialization of SAC
# It contains the build date, version number and copyright information
# Default: SAC_DISPLAY_COPYRIGHT 1
# export SAC_DISPLAY_COPYRIGHT=1

# SAC_USE_DATABASE
# Undefined or 0 -- Do Not Use SeisMgr Database ( Default )
#              1 --        Use SeisMgr Database
# The SeisMgr database attempts to keep the CSS data fields in line
#  with those in the SAC header.  If you are handling CSS data it 
#  would be wise to keep the database on.  Using the SeisMgr database
#  currently can be very slow due when handling hundreds of files
#  and turning it off should show a dramatic speed increase.
# Default: SAC_USE_DATABASE 0
# export SAC_USE_DATABASE=0

# SAC_PPK_LARGE_CROSSHAIRS
# Undefined or 0 - Tiny Cross Hairs    ( Default )
#              1 - Cross Hairs of the Full Plot Window
# In older version of SAC, ppk displayed crosshairs on the window
# which extened the entire window length.  To some, this was desirable.
# To utilize this behavior again, set SAC_PPK_LARGE_CROSSHAIRS to 1
# Default: SAC_PPK_LARGE_CROSSHAIRS 0
# export SAC_PPK_LARGE_CROSSHAIRS=0

