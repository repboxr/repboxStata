# Store results of original Stata repbox run as parcels
# stata_run_cmd and stata_run_log
# Results after regression run and metastudy run will be stored
# separately

repbox_load_internal_repbox_results = function(project_dir) {
  results.file = file.path(project_dir, "repbox", "stata","repbox_results.Rds")
  res = readRDS.or.null(results.file)

}


repbox_save_stata_run_parcels = function(project_dir, parcels=list(), repbox_results=repbox_load_internal_repbox_results(project_dir)) {
  restore.point("repbox_save_stata_run_parcels")

  parcels = repdb_load_parcels(project_dir, "stata_file", parcels=parcels)
  artid = basename(project_dir)

  res = repbox_results
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
      #found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/", artid, "/mod/")),
      found_path = file_path_relative_to_supp(foundfile, paste0("/", artid, "/mod/"),wdir = wdir, supp.dir = paste0(project_dir, "/mod/")),
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


  script_df = parcels[["stata_file"]]
  run_df = left_join(run_df, select(script_df, file_path, script_num), by="file_path")



  repdb_check_data(run_df,"stata_run_cmd")
  repdb_check_data(run_df,"stata_run_log")

  parcels$stata_run_cmd = run_df
  parcels$stata_run_log = run_df

  # Infos on panel dimension variables extracted from Stata run
  xtvar = run_df %>%
    select(runid, timevar, panelvar, tdelta) %>%
    filter(!is_empty_str(timevar) | !is_empty_str(panelvar) | !is_empty_str(tdelta))

  # Augment xtvar with parsed timevar and failvar (panelvar) from stset for stcox regressions
  stcox_runs = run_df %>% filter(cmd == "stcox")
  if (nrow(stcox_runs) > 0) {
    stset_runs = run_df %>% filter(cmd == "stset")
    if (nrow(stset_runs) > 0) {
      stcox_xtvar_list = lapply(stcox_runs$runid, function(r_id) {
        # Find the most recent stset before this regression
        valid_stset = stset_runs %>% filter(runid < r_id) %>% arrange(runid)
        if (nrow(valid_stset) == 0) return(NULL)

        last_stset = tail(valid_stset$cmdline, 1)

        # Extract timevar (first argument after stset)
        timevar = stringi::stri_match_first_regex(last_stset, "^\\s*stset\\s+([A-Za-z0-9_\\.]+)")[,2]

        # Extract failvar from failure() or fail() and map it to panelvar
        panelvar = stringi::stri_match_first_regex(last_stset, "(?:failure|fail)\\(\\s*([A-Za-z0-9_\\.]+)")[,2]

        if (is.na(timevar)) return(NULL)

        tibble(
          runid = as.integer(r_id),
          timevar = as.character(timevar),
          panelvar = as.character(ifelse(is.na(panelvar), NA_character_, panelvar)),
          tdelta = NA_character_
        )
      })
      stcox_xtvar = bind_rows(stcox_xtvar_list)
      if (nrow(stcox_xtvar) > 0) {
        xtvar = bind_rows(xtvar, stcox_xtvar) %>% distinct(runid, .keep_all = TRUE)
      }
    }
  }

  parcels$xtvar = xtvar


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

  parcels$stata_run_info = run_info



  repdb_save_parcels(parcels[c("stata_run_cmd","stata_run_log","stata_run_info", "xtvar")], file.path(project_dir, "repdb") )
  parcels = make_stata_reg_run_cmd_parcel(project_dir, parcels=parcels)

  # Generate the parcel assigning progrunid
  parcels = make_stata_prog_run_parcel(project_dir, parcels=parcels)

  invisible(parcels)
}

make_stata_reg_run_cmd_parcel = function(project_dir, parcels=list()) {
  restore.point("make_stata_reg_run_cmd_parcel")

  parcels = repdb_load_parcels(project_dir, "stata_run_cmd", parcels=parcels)
  run_df = parcels$stata_run_cmd
  run_df = set_run_df_cmd_type(run_df)
  parcels$stata_reg_run_cmd = run_df %>% filter(cmd_type=="reg")
  repdb_save_parcels(parcels[c("stata_reg_run_cmd")], file.path(project_dir, "repdb") )
  invisible(parcels)

}



make_parcel_stata_do_run_info = function(project_dir, parcels = list()) {
  restore.point("make_parcel_stata_do_run_info")
  library(repboxDB)
  parcels = repdb_load_parcels(project_dir, c("stata_file","stata_run_cmd"), parcels=parcels)

  do_df = parcels$stata_file

  if (NROW(do_df)==0) {
    parcels$stata_do_run_info = repdb_null_to_empty(NULL, "stata_do_run_info")
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
  dotab$file_path = str.right.of(normalizePath(dotab$file, winslash = "/",mustWork = FALSE),paste0("/",artid,"/mod/"))
  #dotab$file_path = str.right.of(normalizePath(dotab$file),paste0(normalizePath(dotab$project_dir),"/mod/"))
  dotab$analyzed = rep(TRUE, NROW(dotab))
  do_df = left_join(do_df, dotab, by="file_path") %>%
    mutate(analyzed = na.val(analyzed, FALSE))

  #cmd_df = parcels$stata_cmd
  run_df = parcels$stata_run_cmd

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
  parcels$stata_do_run_info = do_df
  repboxDB::repdb_save_parcels(parcels["stata_do_run_info"],file.path(project_dir, "repdb"))

  parcels
}

set_run_df_cmd_type = function(run_df) {
  cmd_types = repboxDRF::drf_stata_cmd_types_vec()
  run_df$cmd_type = cmd_types[run_df$cmd]
  run_df
}


#' Create a parcel that maps runid to progrunid for commands inside custom Stata programs
make_stata_prog_run_parcel = function(project_dir, parcels = list(), overwrite = FALSE) {
  restore.point("make_stata_prog_run_parcel")

  if (!overwrite && repboxDB::repdb_has_parcel(project_dir, "stata_prog_run")) {
    return(parcels)
  }

  parcels = repboxDB::repdb_load_parcels(project_dir, c("stata_run_cmd", "stata_cmd","stata_file"), parcels = parcels)
  run_df = parcels$stata_run_cmd
  stata_cmd = parcels$stata_cmd

  if (is.null(run_df) || nrow(run_df) == 0 || is.null(stata_cmd) || nrow(stata_cmd) == 0) {
    parcels$stata_prog_run = tibble::tibble(runid = integer(0), progid = character(0), progrunid = integer(0))
    repboxDB::repdb_save_parcels(parcels["stata_prog_run"], file.path(project_dir, "repdb"))
    return(parcels)
  }

  # 1. Vectorized progid assignment based on static stata_cmd
  is_prog_start = (stata_cmd$cmd %in% c("program", "prog", "pr", "progr")) &
                  (!repboxUtils::is.true(stata_cmd$cmd2 %in% c("drop", "dir", "list")))

  if (sum(is_prog_start)==0) {
    parcels$stata_prog_run = tibble::tibble(runid = integer(0), progid = character(0), progrunid = integer(0))
    repboxDB::repdb_save_parcels(parcels["stata_prog_run"], file.path(project_dir, "repdb"))
    return(parcels)
  }

  stata_cmd = left_join(stata_cmd, parcels$stata_file %>% select(file_path, script_num=script_num), by="file_path")


  stata_cmd = stata_cmd %>%
    dplyr::group_by(script_num) %>%
    dplyr::mutate(
      prog_num = cumsum(is_prog_start),
      progid = ifelse(in_program, paste0(script_num, "_", prog_num), NA_character_)
    ) %>%
    dplyr::ungroup()

  # 2. Join progid into the chronological run_df
  run_df = run_df %>%
    dplyr::left_join(stata_cmd %>% dplyr::select(file_path, line, progid), by = c("file_path", "line")) %>%
    dplyr::arrange(runid)

  prog_ids = run_df$progid
  prog_ids[is.na(prog_ids)] = ""

  # 3. Fast RLE compression for the Call Stack
  # This compresses potentially 100,000 commands into a few hundred blocks
  rle_prog = rle(prog_ids)
  blocks = rle_prog$values
  lens = rle_prog$lengths

  progrunid_block = rep(NA_integer_, length(blocks))
  stack = character(0)
  stack_id = integer(0)
  counter = 0L

  # O(N_blocks) loop. Extremely fast because N_blocks is tiny.
  for (i in seq_along(blocks)) {
    p = blocks[i]
    if (p == "") {
      # Outside any program -> clear stack
      stack = character(0)
      stack_id = integer(0)
    } else {
      idx = match(p, stack)
      if (!is.na(idx)) {
        # Resuming a previous call in the stack (popping subroutines)
        stack = stack[1:idx]
        stack_id = stack_id[1:idx]
        progrunid_block[i] = stack_id[idx]
      } else {
        # Entering a new call -> push to stack
        counter = counter + 1L
        stack = c(stack, p)
        stack_id = c(stack_id, counter)
        progrunid_block[i] = counter
      }
    }
  }

  # 4. Expand block progrunids back to individual runids
  progrunid_vec = rep(progrunid_block, lens)

  stata_prog_run = tibble::tibble(
    runid = run_df$runid,
    progid = run_df$progid,
    progrunid = progrunid_vec
  ) %>% dplyr::filter(!is.na(progid))

  parcels$stata_prog_run = stata_prog_run
  repboxDB::repdb_save_parcels(parcels["stata_prog_run"], file.path(project_dir, "repdb"))

  invisible(parcels)
}
