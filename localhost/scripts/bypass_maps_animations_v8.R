# Author: A Ghilardi
# Version: 1.0
# Date: 2015
# EGOML dependency bundle: V8

# BEGIN USER INPUTS ----------------------------------------------------------
# The calling Dinamica model selects this script when maps are disabled.
# Change the model control to enable maps and animations.
# END USER INPUTS ------------------------------------------------------------

rm(list=ls(all=TRUE))

textmsg<-"Maps and animations turned off by user"
write.csv(textmsg,paste("Out//",textmsg,".csv",sep=""),row.names = FALSE)


###############################
#########END OF SCRIPT#########
###############################
