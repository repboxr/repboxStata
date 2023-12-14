// Copyright 2008 Johannes Schmieder
// Program: guse
// Version: 0.9   (September 2008)
// NOTE:  This program is distributed under the GNU GPL.
// See end of this file and http://www.gnu.org/licenses/ for details.
// Please report errors to johannesschmieder /at/ gmail.com

/*==================================*/
// Notes:
// This tool allows to directly read compressed 
// Stata .dta files in the gzip format (and ending in .dta.gz)
// if the tool gzip/gunzip is installed on the system.
// The Syntax is the same as for the standard Stata "use" command.
// e.g. guse MyData.dta if year==1999, clear
// or   guse Var1 Var2 in 1/100 using MyData.dta, clear

// This program is intended to be used in conjunction with "gsave",
// which automatically compresses saved .dta files.
// The syntax of guse and gsave should be exactly the same as use
// and save and thus existing programs can be adjusted with minimal
// effort. 
/*==================================*/

cap program drop guse
program define guse
	
	gettoken part 0:0, p(" ,") bind q	
	while `"`part'"' != "," & `"`part'"' != "" {
		local left `"`left' `part'"'
		gettoken part 0:0, p(" ,") bind q	
	}
	local 0 , `0'
	syntax , [clear NOLabel]
	
	if strmatch(`"`left'"',"* using *")	{
		gettoken 1 left:left, bind quotes
		while "`1'"!="" {
			if "`1'"!="using" {
				local namelist `namelist' `1'
			}
			if "`1'"=="using" {
				gettoken using postusing:left, bind 
				continue, break
			}				
			gettoken 1 left:left, bind quotes
		}
	}
	else {
		gettoken using namelist:left, bind 
	}	
	
	if strmatch(`"`using'"',"*.dta*")	local using `using'.gz
	else local using `using'.dta.gz
  tempfile tmpdata
	shell nice gunzip "`using'" -c > `tmpdata'
	use `namelist' `postusing' using `tmpdata' , `clear' `nolabel'

end // guse

/*
This program and all programs referenced in it are free software. You
can redistribute the program or modify it under the terms of the GNU
General Public License as published by the Free Software Foundation;
either version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
USA.
*/
