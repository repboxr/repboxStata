program repbox_ivreg2_write_first_stage
  args file_prefix
  _estimates dir
  local vars = r(names)
  foreach var of local vars {
    display "Store `var'"
    _estimates unhold `var'
    parmest, label saving("`file_prefix'_`var'.dta", replace)
    repbox_write_reg_scalars "`file_prefix'_`var'_scalars.txt"
    repbox_write_reg_macros "`file_prefix'_`var'_macros.txt"
  }

end
