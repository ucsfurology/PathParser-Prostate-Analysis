*11/25/16*
import excel "/Users/coops/Dropbox/Documents/ Docs/ UCSF/UODB/SDE_parsing project/results-20170710.xlsx", firstrow allstring sheet("All SDE") clear
local spu = "S_ P_ U_"
	foreach q of local spu {
	g `q'nodespos=real(`q'PATHNODES_POSITIVE)
	g `q'nodestaken=real(`q'PATHNODES_DISSECTED)
}


**pGS analysis
local varlist = "GPRIMP GSECONDP GTERTP" 
   foreach m of local varlist {
   *tab S_`m' P_`m' if (S_`m'~="" | P_`m'~=""), miss    /* evaluate only those with either parsed or SDE */
   *tab S_`m' P_`m', ro co								/* evaluate only those with both parsed and SDE */
   table S_`m' P_`m', by(U_`m')
   } 

*pStage analysis
local varlist = "TSTAGEP NSTAGEP PATHMGNPOS PATHSVIPOS PATHECEPOS" 
   foreach m of local varlist {
   table S_`m' P_`m', by(U_`m')
   }

*Nodes taken/positive
graph matrix P_nodestaken U_nodestaken S_nodestaken, jitter (1) msize(small) half
