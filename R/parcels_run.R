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
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/", artid, "/mod/")),
      missing_data = !has.data
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
      ok_runs = sum(is.true(has.data & !runerr)),
      no_dat_runs = sum(!is.true(has.data)),
      err_runs = sum(runerr & is.true(has.data)),
      reg_runs = sum(is.regcmd),
      no_dat_reg_runs = sum(is.regcmd & !is.true(has.data)),
      err_reg_runs = sum(is.regcmd & runerr & is.true(has.data)),
      ok_reg_runs = reg_runs-err_reg_runs-no_dat_reg_runs,
      start_time = first(start_time[!is.na(start_time)]),
      end_time = last(end_time[!is.na(end_time)]),
      run_sec = time.diff(start_time, end_time)
    )


  if (run_info$no_dat_runs >0) {
    repbox_problem(msg = paste0(run_info$no_dat_runs, " of the ", run_info$runs, " run commands had a missing data set."), type = "stata_no_dat",fail_action = "msg")
  }

  if (run_info$err_runs >0) {
    repbox_problem(msg = paste0(run_info$err_runs, " of the ", run_info$runs, " run commands without missing data threw an error."), type = "stata_err",fail_action = "msg")
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

  run_info = run_df %>%
    summarize(
      artid = first(artid),
      runtype = "reg",
      runs = n(),
      ok_runs = sum(is.true(has.data & !runerr)),
      no_dat_runs = sum(!is.true(has.data)),
      err_runs = sum(runerr & is.true(has.data)),
      reg_runs = sum(is.regcmd),
      no_dat_reg_runs = sum(is.regcmd & !is.true(has.data)),
      err_reg_runs = sum(is.regcmd & runerr & is.true(has.data)),
      ok_reg_runs = reg_runs-err_reg_runs-no_dat_reg_runs,
      start_time = first(start_time[!is.na(start_time)]),
      end_time = last(end_time[!is.na(end_time)]),
      run_sec = time.diff(start_time, end_time)
    )






  org_run_info = parcels$stata_run_info$stata_run_info

  if (NROW(org_run_info)==0) {
    if (run_info$err_runs >0) {
      repbox_problem(msg = paste0("The Stata run had ", run_info$err_runs, " commands that threw an error"), type = "stata_err",fail_action = "msg")
    }
    if (run_info$no_dat_runs >0) {
      repbox_problem(msg = paste0(run_info$no_dat_runs, " of the ", run_info$runs, " run commands had a missing data set."), type = "stata_no_dat",fail_action = "msg")
    }

  } else {
    if (run_info$err_runs > org_run_info$err_runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information had more errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    } else if (run_info$runs != org_run_info$runs) {
      repbox_problem(msg = paste0("The Stata run which stored regression information executed a different number of commands than the original run."), type = "stata_reg_runs_diff",fail_action = "msg")
    } else if (run_info$err_runs < org_run_info$err_runs) {
      repbox_problem(msg = paste0("The Stata run  which stored regression information had fewer errors than the original run."), type = "stata_reg_err",fail_action = "msg")
    }
  }
  parcels$stata_reg_run_info = list(stata_run_info=run_info)

  repdb_save_parcels(parcels[c("stata_reg_run_info")], file.path(project_dir, "repdb") )
  invisible(parcels)
}


make_parcel_stata_do_run_info = function(project_dir, parcels = list()) {
  restore.point("make_parcel_stata_do_run_info")
  library(repboxDB)
  parcels = repdb_load_parcels(project_dir, c("stata_file","stata_run_cmd"), parcels=parcels)

  do_df = parcels$stata_file$script_file

  if (NROW(do_df)==0) {
    parcels$stata_do_run_info = list(stata_do_run_info = repdb_null_to_empty(NULL, "stata_do_run_info"))
    repboxDB::repdb_save_parcels(parcels["stata_do_run_info"],file.path(project_dir, "repdb"))
    return(parcels)
  }

  # dotab contains some information that we have not yet nicely stored
  # in a repdb table
  dotab_file = file.path(project_dir, "/repbox/stata/dotab.Rds")
  dotab = readRDS.or.null(dotab_file)
  if (is.null(dotab)) {
    dotab = data.frame(file_path=character(0), timeout=logical(0), runtime=numeric(0),is.included=logical(0), parse.err = logical(0))
  }
  #cat(paste0('"', names(dotab),'"', collapse=", "))
  old_cols = c("num.reg.lines", "parse.err",  "is.included", "does.include", "timeout", "runtime")
  new_cols = c("num_reg_lines", "has_parse_err",  "is_included", "does_include", "timeout", "runtime")
  dotab = rename.cols(dotab, old_cols, new_cols)

  artid = basename(project_dir)
  dotab$file_path = str.right.of(normalizePath(dotab$file, winslash = "/"),paste0("/",artid,"/mod/"))
  #dotab$file_path = str.right.of(normalizePath(dotab$file),paste0(normalizePath(dotab$project_dir),"/mod/"))
  dotab$analyzed = rep(TRUE, NROW(dotab))
  do_df = left_join(do_df, dotab, by="file_path") %>%
    mutate(analyzed = na.val(analyzed, FALSE))

  #cmd_df = parcels$stata_cmd$stata_cmd
  run_df = parcels$stata_run_cmd$stata_run_cmd

  run_info_df = run_df %>%
    mutate(
      loads_data = cmd %in% c("use","u","us","import","guse","gzuse","insheet"),
      has_error = is.true(errcode != 0)
    ) %>%
    group_by(file_path) %>%
    summarize(
      was_run = TRUE,
      num_runs = n(),
      num_runs_ok = sum(!has_error & !missing_data),
      num_runs_no_dat = sum(missing_data),
      num_runs_err = sum(has_error & !missing_data),
      num_load_data = sum(loads_data),
      num_load_data_err = sum(has_error & loads_data)
    )

  do_df = do_df %>%
    left_join(run_info_df, by="file_path") %>%
    mutate(
      was_run = na.val(was_run,FALSE),
      has_parse_err = na.val(has_parse_err, FALSE)
    )

  do_df = repdb_select_fields(do_df, "stata_do_run_info")
  parcels$stata_do_run_info = list(stata_do_run_info = do_df)
  repboxDB::repdb_save_parcels(parcels["stata_do_run_info"],file.path(project_dir, "repdb"))

  parcels
}


