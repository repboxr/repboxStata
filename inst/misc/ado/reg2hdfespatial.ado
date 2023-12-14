capture program drop reg2hdfespatial 
*! Thiemo Fetzer 4/2015: WRAPPER PROGRAM TO ESTIMATE SPATIAL HAC FOR OLS REGRESSION MODELS WITH HIGH DIMENSIONAL FIXED EFFECTS
*! The function uses the reg2hdfe procedure to demean the data by the time- and panel-variable you specify
*! This ensures that you do not compute large variance covariance matrices to compute
*! Spatial HAC errors for coefficients you do not actually care about.
*! Updates available on http://www.trfetzer.com
*! Please email me in case you find any bugs or have suggestions for improvement.
*! Please cite: Fetzer, T. (2014) "Can Workfare Programs Moderate Violence? Evidence from India", STICERD Working Paper.
*! Also credit Sol Hsiang.
*! Hsiang, S. M. (2010). Temperatures and cyclones strongly associated with economic production in the Caribbean and Central America. PNAS, 107(35), 15367–72.
*! The Use of the function is simple
*!  reg2hdfespatial Yvar Xvarlist, lat(latvar) lon(lonvar) Timevar(tvar) Panelvar(pvar) [DISTcutoff(#) LAGcutoff(#) bartlett DISPlay star dropvar demean altfetime(varname) altfepanel(varname)]
*!
*!
*! You can also specify other fixed effects:
*! reg2hdfespatial Yvar     Xvarlist   ,timevar(year) panelvar(district) altfetime(regionyear)  lat(y) lon(x) distcutoff(500) lagcutoff(20) 
*!
*! here I specify the time variable as the year, but I demean the data first
*! by region x year fixed effects.
*! This turns out to matter as the OLS_Spatial_HAC for the autocorrelation correction which you may want
*! to be done at a level different from the level at which you have the time fixed effects specified.
/*-----------------------------------------------------------------------------

 Syntax:
 
 reg2hdfespatial Yvar Xvarlist, lat(latvar) lon(lonvar) Timevar(tvar) Panelvar(pvar) [DISTcutoff(#) LAGcutoff(#) bartlett DISPlay star dropvar demean altfetime(varname) altfepanel(varname)]

 -----------------------------------------------------------------------------*/

program reg2hdfespatial, eclass byable(recall)
//version 9.2
version 11
syntax varlist(ts fv min=2) [if] [in], ///
				lat(varname numeric) lon(varname numeric) ///
				Timevar(varname numeric) Panelvar(varname numeric) [LAGcutoff(integer 0) DISTcutoff(real 1) ///
				DISPlay star bartlett dropvar altfetime(varname) altfepanel(varname) ]
				
/*--------PARSING COMMANDS AND SETUP-------*/

preserve
if "`if'"~="" {
	qui keep `if' 
}


capture drop touse
marksample touse				// indicator for inclusion in the sample
gen touse = `touse'

*keep if touse
//parsing variables
loc Y = word("`varlist'",1)		

loc listing "`varlist'"


loc X ""
scalar k_variables = 0

//make sure that Y is not included in the other_var list
foreach i of loc listing {
	if "`i'" ~= "`Y'"{
		loc X "`X' `i'"
		scalar k_variables = k_variables + 1 // # indep variables
		
	}
}
local wdir `c(pwd)'

tmpdir returns r(tmpdir):
local tdir  `r(tmpdir)'


**clear temp folder of existing files
qui cd "`tdir'"
local tempfiles : dir . files "*.dta"
foreach f in `tempfiles' {
	erase `f'
}

quietly {
if("`altfepanel'" !="" & "`altfetime'" !="") {
di "CASE 1"
reg2hdfe `Y' `X' `lat' `lon' `timevar' `panelvar' ,  id1(`altfepanel') id2(`altfetime') out("`tdir'") noregress 
loc iteratevarlist "`Y' `X' `lat' `lon' `timevar' `panelvar'" 
reg2hdfe `Y' `X' , id1(`altfepanel') id2(`altfetime')
}
if("`altfepanel'" =="" & "`altfetime'" !="") {
di "CASE 2"

reg2hdfe `Y' `X' `lat' `lon' `timevar' ,  id1(`panelvar') id2(`altfetime') out("`tdir'") noregress 
loc iteratevarlist "`Y' `X' `lat' `lon' `timevar' " 

reg2hdfe `Y' `X' , id1(`panelvar') id2(`altfetime')
}
if("`altfepanel'" !="" & "`altfetime'" =="") {
di "CASE 3"

reg2hdfe `Y' `X' `lat' `lon' `panelvar' ,  id1(`altfepanel') id2(`timevar') out("`tdir'") noregress 
reg2hdfe `Y' `X' , id1(`altfepanel') id2(`timevar') 
loc iteratevarlist "`Y' `X' `lat' `lon' `panelvar'"
}
if("`altfepanel'" =="" & "`altfetime'" =="") {
di "CASE 4"
reg2hdfe `Y' `X' `lat' `lon' ,  id1(`panelvar') id2(`timevar') out("`tdir'") noregress 
loc iteratevarlist "`Y' `X' `lat' `lon'" 
reg2hdfe `Y' `X' ,  id1(`panelvar') id2(`timevar')
}

	foreach var of varlist `X' {
		lincom `var'		
		if `r(se)' != 0 {
			loc newVarList "`newVarList' `var'"
			scalar k_variables = k_variables + 1
		}
	}
	
	loc XX "`newVarList'"


/* From reg2hdfe.ado */
tempfile tmp1 tmp2 tmp3 readdata

	use _ids, clear
	sort __uid
	qui save "`tmp1'", replace
	if "`cluster'"!="" {
		merge __uid using _clustervar
		if r(min)<r(max) { 
			di "Fatal Error"
			error 198
		}
		drop _merge
		sort __uid
		rename __clustervar `clustervar'
		qui save "`tmp1'", replace
		}
	

* Now read the original variables
	foreach var in `iteratevarlist'  {
		merge __uid using _`var'
		sum _merge, meanonly
		if r(min)<r(max) { 
			di "Fatal Error"
			error 198
		}
		tab _merge
		drop _merge
		drop __fe2*
		drop __t_*
		sort __uid
		qui save "`tmp2'", replace
	}
	foreach var in  `iteratevarlist'  {
		rename __o_`var' `var'
	}
	
	
 	tempvar yy sy
	gen double `yy'=(`depvar'-r(mean))^2
	gen double `sy'=sum(`yy')
	local tss=`sy'[_N]
	drop `yy' `sy'
	qui save "`readdata'", replace
	use `tmp1', clear
	foreach var in  `iteratevarlist'  {
		merge 1:1 __uid using _`var'
		sum _merge, meanonly
		if r(min)<r(max) { 
			di "Fatal Error."
			error 198
		}
		
	drop _merge
	}

		drop __fe2*
		rename __o_`lon' `lon'
		rename __o_`lat' `lat'
		if("`altfepanel'" !="" ) {
 		rename __o_`panelvar' `panelvar'
 		}
 		if("`altfetime'" !="" ) {
 		rename __o_`timevar' `timevar'
 		}
 			
		drop __o_*
		sort __uid
		qui save "`tmp3'", replace
        
	foreach var in `Y' `X'  {
		rename __t_`var' `var'
	}

}
ols_spatial_HAC `Y' `XX', lat(`lat') lon(`lon') timevar(`timevar') panelvar(`panelvar') lagcutoff(`lagcutoff') distcutoff(`distcutoff') bartlett

cd "`wdir'"
end


