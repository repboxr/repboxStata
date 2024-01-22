program repbox_write_test_scalars
  args file

  file open wrIte_reG_stats_fiLe using "`file'", write replace
  local vars : r(scalars)
  foreach var of local vars {
    local val = r(`var')
    file write wrIte_reG_stats_fiLe "`var'=`val'"
    file write wrIte_reG_stats_fiLe _n
  }
  file close wrIte_reG_stats_fiLe
end
