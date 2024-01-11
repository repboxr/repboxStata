# Store results of original Stata repbox run as parcels
# stata_run_cmd and stata_run_log
# Results after regression run and metastudy run will be stored
# separately


repbox_save_stata_run_parcels = function(project_dir, parcels=list()) {
  restore.point("repbox_save_stata_run_parcels")

  parcels = repdb_load_parcels(project_dir, "stata_file", parcels=parcels)
  artid = basename(project_dir)
  results.file = file.path(project_dir, "repbox", "stata","repbox_results.Rds")
  res = readRDS.or.null(results.file)

  # No Stata run results stored
  if (is.null(res)) {
    return(parcels)
  }

  dotab = res$dotab
  dotab$file_path = str.right.of(dotab$file, paste0("/",artid,"/mod/"))

  if (!has.col(res$tab,"in_loop")) {
    cat("\nNote there was no 'in_loop' col in repbox_results$tab\n")
    res$tab$in_loop = NA_integer_
  }

  run_df = res$run.df %>%
    mutate(
      artid = artid,
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/", artid, "/mod/"))
    ) %>%
    rename(

      start_time = stime,
      end_time = etime,
      errcode = runerrcode,
      errmsg = runerrmsg,
      out_img_file = out.img.file
    ) %>%
    left_join(select(dotab, donum, file_path), by="donum") %>%
    left_join(select(dotab, rootdonum = donum, root_file_path = file_path), by="rootdonum")

  run_df$runid = seq_len(NROW(run_df))
  script_df = parcels$stata_file$script_file
  run_df = left_join(run_df, select(script_df, file_path, script_num), by="file_path")

  repdb_check_data(run_df,"stata_run_cmd")
  repdb_check_data(run_df,"stata_run_log")

  parcels$stata_run_cmd = list(stata_run_cmd = run_df)
  parcels$stata_run_log = list(stata_run_log = run_df)


  # Store some aggregate information on the run
  run_info = run_df %>%
    summarize(
      artid = first(artid),
      runtype = "org",
      runs = n(),
      err_runs = sum(runerr),
      reg_runs = sum(is.regcmd),
      err_reg_runs = sum(is.regcmd & runerr),
      ok_reg_runs = reg_runs-err_reg_runs,
      start_time = first(start_time[!is.na(start_time)]),
      end_time = last(end_time[!is.na(end_time)]),
      run_sec = time.diff(start_time, end_time)
    )

  if (run_info$err_runs >0) {
    repbox_problem(msg = paste0(run_info$err_runs, " of the ", run_info$runs, " run commands threw an error."), type = "stata_err",fail_action = "msg")
  }

  parcels$stata_run_info = list(stata_run_info=run_info)

  repdb_save_parcels(parcels[c("stata_run_cmd","stata_run_log","stata_run_info")], file.path(project_dir, "repdb") )
  invisible(parcels)
}


# Only saves run_info after regression run
# Regression information will be stored after metareg run
repbox_save_stata_reg_run_parcels = function(project_dir, parcels=list()) {
  restore.point("repbox_save_stata_reg_run_parcels")

  parcels = repdb_load_parcels(project_dir, c("stata_run_info","stata_file"), parcels=parcels)
  artid = basename(project_dir)
  results.file = file.path(project_dir, "repbox", "stata","repbox_results.Rds")
  res = readRDS.or.null(results.file)

  # No Stata run results stored
  if (is.null(res)) {
    return(parcels)
  }

  dotab = res$dotab
  dotab$file_path = str.right.of(dotab$file, paste0("/",artid,"/mod/"))

  if (!has.col(res$tab,"in_loop")) {
    cat("\nNote there was no 'in_loop' col in repbox_results$tab\n")
    res$tab$in_loop = NA_integer_
  }

  run_df = res$run.df %>%
    mutate(
      artid = artid,
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/", artid, "/mod/"))
    ) %>%
    rename(

      start_time = stime,
      end_time = etime,
      errcode = runerrcode,
      errmsg = runerrmsg,
      out_img_file = out.img.file
    ) %>%
    left_join(select(dotab, donum, file_path), by="donum") %>%
    left_join(select(dotab, rootdonum = donum, root_file_path = file_path), by="rootdonum")

  run_df$runid = seq_len(NROW(run_df))
  script_df = parcels$stata_file$script_file
  run_df = left_join(run_df, select(script_df, file_path, script_num), by="file_path")

  # Don't save detailed run results after reg run

  #repdb_check_data(run_df,"stata_run_cmd")
  #repdb_check_data(run_df,"stata_run_log")

  #parcels$stata_run_cmd = list(stata_run_cmd = run_df)
  #parcels$stata_run_log = list(stata_run_log = run_df)

  # Store some aggregate information on the run
  run_info = run_df %>%
    summarize(
      artid = first(artid),
      runtype = "reg",
      runs = n(),
      err_runs = sum(runerr),
      reg_runs = sum(is.regcmd),
      err_reg_runs = sum(is.regcmd & runerr),
      ok_reg_runs = reg_runs-err_reg_runs,
      start_time = first(start_time[!is.na(start_time)]),
      end_time = last(end_time[!is.na(end_time)]),
      run_sec = time.diff(start_time, end_time)
    )


  org_run_info = parcels$stata_run_info$stata_run_info

  if (NROW(org_run_info)==0) {
    if (run_info$err_runs >0) {
      repbox_problem(msg = paste0("The Stata run had ", run_info$err_runs, " commands that threw an error"), type = "stata_err",fail_action = "msg")
    }
  } else {
    if (run_info$err_runs > org_run_info$err_runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information had more errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    } else if (run_info$runs != org_run_info$runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information executed a different number of commands than the original run."), type = "stata_reg_runs_diff",fail_action = "msg")
    } else if (run_info$err_runs < org_run_info$runs) {
      repbox_problem(msg = paste0("The Stata run  which stored regression information had fewer errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    }
  }
  parcels$stata_reg_run_info = list(stata_run_info=run_info)

  repdb_save_parcels(parcels[c("stata_reg_run_info")], file.path(project_dir, "repdb") )
  invisible(parcels)
}

