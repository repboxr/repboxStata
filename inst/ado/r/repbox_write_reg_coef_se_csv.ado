program repbox_write_reg_coef_se_csv
  args coef_var se_var csv_file
  capture erase "`csv_file'"
  mata: mata_write_reg_coef_se_csv("`coef_var'","`se_var'","`csv_file'")
end



mata:
// Define a function that takes a matrix name and a file name as arguments
string scalar mata_write_reg_coef_se_csv(string scalar coef_var, string scalar se_var, string scalar filename)
{
    // Get the matrix from Stata
    coef_mat = st_matrix(coef_var)
    se_mat = st_matrix(se_var)

    // Get the row names of the matrix
    string matrix var_names
    var_names = st_matrixcolstripe(coef_var)

    // Create a file handle for writing
    fh = fopen(filename, "w")
    fput(fh, "var,coef,se")

    // Loop over the rows of the matrix
    for (i = 1; i <= cols(coef_mat); i++) {
        // Write the name and the coefficient to the file, separated by a comma
      str = var_names[i,2] + "," + strofreal(coef_mat[i]) + "," + strofreal(se_mat[i])
      fput(fh, str)
    }

    // Close the file handle
    fclose(fh)
    return(filename)
}
end
