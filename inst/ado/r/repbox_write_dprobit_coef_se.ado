program repbox_write_dprobit_coef_se
  args csv_file
  repbox_write_reg_coef_se_csv "e(dfdx)" "e(se_dfdx)" "`csv_file'"
end

