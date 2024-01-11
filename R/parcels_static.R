# Performs static Stata code analysis and stores results as parcel
# stata_cmd

repbox_stata_static_parcel = function(project_dir, parcels=list()) {
  restore.point("repbox_stata_static_parcel")

  parcels = repdb_load_parcels(project_dir, "stata_source", parcels)
  source_df = parcels$stata_source$script_source

  # No Stata do file exists
  if (NROW(source_df)==0) return(parcels)


  tab_df = lapply(seq_len(NROW(source_df)), function(i) {
    res = parse.sup.do(file = source_df$file_path[i],project_dir=project_dir, code = source_df$text[[i]])
    tab = res$tab[[1]]
    tab$file_path = source_df$file_path[[i]]
    tab
  }) %>% bind_rows()

  artid = basename(project_dir)

  cmd_df = tab_df %>%
    mutate(
      artid = artid,
      cmdline = txt,
      in_loop = in_loop==1,
      in_program = is.true(in.program>=1),
      num_runs = NA_integer_,
      num_err_runs = NA_integer_
    ) %>%
    rename(
      prefix_cmd1 = colon_cmd1,
      prefix_cmd2 = colon_cmd2,
      is_reg = is.regcmd
    )

  repdb_check_data(cmd_df,"stata_cmd")
  parcels$stata_cmd = list(stata_cmd = cmd_df)
  repdb_save_parcels(parcels["stata_cmd"], file.path(project_dir,"repdb"))
  parcels
}

# Already done in repboxRun::repbox_make_script_parcel
#
# repdb_make_stata_script_parcels = function(project_dir, parcels=list()) {
#   restore.point("repdb_make_script_parcels")
#   parcels = repdb_load_parcels(project_dir, "file_info",parcels)
#   file_df = parcels$file_info$file_info
#
#   script_df = file_df %>%
#     ungroup() %>%
#     filter(file_type == "do") %>%
#     mutate(
#       sup_dir = paste0(project_dir, "/org"),
#       long_path = paste0(sup_dir,"/", file_path ),
#       script_num = seq_len(n()),
#       file_exists = file.exists(long_path),
#       source_added = file_exists
#     )
#   text = rep(NA_character_, NROW(script_df))
#   num_lines = rep(NA_integer_, NROW(script_df))
#
#   for (i in seq_len(NROW(script_df))) {
#     if (script_df$file_exists[i]) {
#       txt = readLines(script_df$long_path[i],encoding = "UTF-8")
#       text[i] = merge.lines(txt)
#       num_lines[i] = length(txt)
#     }
#   }
#   script_df$num_lines = num_lines
#   script_df$text = text
#
#   parcels = list(
#     stata_file = list(script_file=script_df),
#     stata_source = list(script_source = script_df)
#   )
#   repdb_save_parcels(parcels[c("stata_file","stata_source")], dir = file.path(project_dir, "repdb") )
#   return(parcels)
# }
