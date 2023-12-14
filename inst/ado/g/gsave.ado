// Copyright 2008 Johannes Schmieder
// Program: gsave
// Version: 0.9   (September 2008)
// NOTE:  This program is distributed under the GNU GPL.
// See end of this file and http://www.gnu.org/licenses/ for details.
// Please report errors to johannesschmieder /at/ gmail.com

/*==================================*/
// Notes:
// This tool allows to directly save Stata .dta files and
// compress them to the gzip format if the tool gzip/gunzip  
// is installed on the system (and the excecutable is in the
// search path). The resulting files will have the extension .dta.gz
// The Syntax is the same as for the standard Stata "save" command.
// e.g. gsave MyData.dta
// or   gsave "My Data.dta" , replace
// or   gsave MyData, nolabel

// This program is intended to be used in conjunction with "guse".
// The syntax of guse and gsave should be exactly the same as use
// and save and thus existing programs can be adjusted with minimal
// effort. 
/*==================================*/

cap program drop gsave
program define gsave
	local args 0

	gettoken file 0:0, p(",") bind 
	syntax [, clear NOLabel replace all Orphans EMPTYok]
	if !strmatch(`"`file'"',"*.dta")	local file `file'.dta
	
	cap confirm new file `"`file'.gz"'
	if _rc {
		if  "`replace'"!="" erase `"`file'.gz"'
		else {
			di in red "`file'.gz : " 
			error 602
		}
	}
	save "`file'" `0'
	shell gzip "`file'"	
	confirm file "`file'.gz"
	di in gr "file `file' compressed to `file'.gz"

end // gsave

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
