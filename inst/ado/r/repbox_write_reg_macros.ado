program repbox_write_reg_macros
  args file

  file open wrIte_reG_stats_fiLe using "`file'", write replace
  local vars : e(macros)
  foreach var of local vars {
    capture local val = e(`var')
    capture file write wrIte_reG_stats_fiLe "`var'=`val'"
    capture file write wrIte_reG_stats_fiLe _n
  }
  file close wrIte_reG_stats_fiLe
end
