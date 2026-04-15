
intermediate.data.paths = function(projetc_dir) {
  project_dir = normalizePath(project_dir, mustWork = FALSE, winslash = "/")

  list(
    mod_dir = normalizePath(
      file.path(project_dir, "mod"),
      mustWork = FALSE,
      winslash = "/"
    ),
    state_dir = normalizePath(
      file.path(project_dir, "repbox", "stata", "intermediate_state"),
      mustWork = FALSE,
      winslash = "/"
    ),
    intermediate_dir = normalizePath(
      file.path(project_dir,"repbox", "stata",  "intermediate_data"),
      mustWork = FALSE,
      winslash = "/"
    )
  )
}



repbox_intermediate_empty_df = function() {
  restore.point("repbox_intermediate_empty_df")
  data.frame(
    runid = integer(0),
    instance_num = integer(0),
    file_path = character(0),
    copy_path = character(0),
    has_copy = logical(0),
    stringsAsFactors = FALSE
  )
}


repbox_intermediate_init = function(project_dir, opts = rbs.opts()) {
  restore.point("repbox_intermediate_init")
  empty = repbox_intermediate_empty_df()
  im_rds_file = file.path(project_dir, "repbox/stata/intermediate_data.Rds")
  if (!isTRUE(opts$capture_intermediate_data)) {
    saveRDS(im_rds_file)
    return()
  }
  im_state_dir = file.path(project_dir, "repbox/stata/intermediate_state")
  clear.and.create.dir(im_state_dir)
  im_data_dir = file.path(project_dir, "repbox/stata/intermediate_data")
  clear.and.create.dir(im_data_dir)
  saveRDS(empty, im_rds_file)
}


repbox_intermediate_read_state_df = function(project_dir, run_df, opts = rbs.opts()) {
  restore.point("repbox_intermediate_read_state_df")
  im_state_dir = file.path(project_dir, "repbox/stata/intermediate_state")
  im_data_dir = file.path(project_dir, "repbox/stata/intermediate_data")


  files = list.files(
    path = im_state_dir,
    pattern = "[.]csv$",
    recursive = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )

  empty = repbox_intermediate_empty_df()
  if (length(files) == 0) {
    return(empty)
  }

  df = bind_rows(lapply(files, function(file) {
    res = read.csv(file,header=FALSE, sep=";")
  }))
  names(df) = c("file_path", "donum", "line", "counter")
  df = df %>%
    group_by(file_path) %>%
    mutate(
      instance_num = 1:n(),
      copy_path = ifelse(dirname(file_path) == ".",
        paste0("c", 1:n(), "_", basename(file_path)),
        file.path(dirname(file_path), paste0("c", 1:n(), "_", basename(file_path)))
      )
    )
  df$has_copy = file.exists(file.path(im_data_dir, df$copy_path))

  df = df %>% left_join(run_df %>% select(donum,line, counter, runid), by = c("donum","line","counter"))
  cols = c("runid","instance_num", "file_path", "copy_path", "has_copy")
  df = df[,cols]

  df
}


repbox_make_intermediate_data_df = function(project_dir, run_df, opts = rbs.opts()) {
  restore.point("repbox_intermediate_finalize")
  im_rds_file = file.path(project_dir, "repbox/stata/intermediate_data.Rds")
  df = repbox_intermediate_read_state_df(project_dir = project_dir, run_df, opts = opts)

  saveRDS( df, im_rds_file)
  invisible(df)
}


