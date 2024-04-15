example = function() {
  library(repboxRun)
  #set_stata_paths(stata_dir="C:/programs/Stata17",ado_dirs = c(plus = "C:/libraries/repbox/ado/plus"))
  check_stata_paths_and_ado()
  project = "testsupp"
  project_dir = "~/repbox/projects_reg/testsupp"
  #project_dir = file.path("C:/libraries/repbox/projects_reg",project)
  parcels = repbox_stata_static_parcel(project_dir)

  update.repbox.project(project_dir,run.lang = "stata")
  rstudioapi::filesPaneNavigate(project_dir)



  library(repboxMain)
  project_dir = "~/repbox/projects2/testsupp"
  project_dir = "~/repbox/projects_reg/testsupp"
  init.repbox.project(project_dir)
  opts = repbox_stata_opts(just.extract=FALSE, force=FALSE, extract.reg.info = TRUE)
  update.repbox.project(project_dir,stata_opts=opts,run.lang = "stata")
  repbox_project_run_stata(project_dir, opts=opts)

  library(repboxMain)
  opts = repbox_stata_opts(just.extract=TRUE, force=TRUE, overwrite=TRUE)
  update.repbox.project(project_dir,stata_opts=opts)
  rstudioapi::filesPaneNavigate(project_dir)
}



repbox_project_run_stata = function(project_dir, opts=repbox_stata_opts(), parcels=list(), ...) {
  restore.point("repbox_project_run_stata")
  options(dplyr.summarise.inform = FALSE)
  options(repbox.stata.options=opts)
  verbose = opts$verbose

  project = basename(project_dir)
  sup.dir = file.path(project_dir, "mod")
  setwd(sup.dir)
  repbox.dir = file.path(project_dir,"repbox/stata")

  res.file = file.path(repbox.dir,"repbox_results.Rds")
  if (!opts$force & file.exists(res.file)) {
    cat(paste0("\nStata replication results already exist for ", project_dir, "\n"))
    return(invisible(parcels))
  }

  if (opts$just.extract) {
    cat("\nJust extract results of previous run of Stata do files...\n")
    repbox_stata_extract(project_dir)
    return(invisible(parcels))
  }

  if (opts$check.stata.paths.and.ado) {
    check_stata_paths_and_ado(on_fail="error")
  }


  if (!dir.exists(repbox.dir)) dir.create(repbox.dir,recursive = TRUE)


  # Reset log of find_files.R
  writeLines(
    "mode,found_file,org_file,sup_dir,cmd,default_ext,wdir",
    file.path(repbox.dir,"find_path_log.csv")
  )

  cmd.file = file.path(repbox.dir, "stata_cmd.csv")
  if (file.exists(cmd.file)) file.remove(cmd.file)

  # Remove __MACOSX dirs from supplement
  # TODO: Run on a more general level
  try(remove.macosx.dirs(project_dir),silent = TRUE)

  do.files = list.files(sup.dir,glob2rx("*.do"),full.names = TRUE,recursive = TRUE)
  do.files = do.files[!startsWith(basename(do.files),"repbox_")]

  if (!is.null(opts[["just.files"]])) {
    do.files = do.files[basename(do.files) %in% opts$just.files]
  }

  if (verbose)
    cat("\nReplicate ", project, " with ", NROW(do.files), " do files.\n")

  if (NROW(do.files)==0) {
    if (verbose) cat("\nNo do files to analyse")
    return(invisible(parcels))
  }

  do.df = lapply(do.files, parse.sup.do, project_dir=project_dir) %>% bind_rows()

  do.df = add.includes.to.do.df(do.df)
  do.df$use.includes = opts$use.includes

  do.df = set.do.df.run.prio(do.df)

  do.df$project_dir = project_dir

  do.df$donum = seq_len(NROW(do.df))

  which.do = seq_len(NROW(do.df))

  if (opts$install.missing.modules) {
    # Check missing modules
    for (i in which.do) {
      cat(paste0("\nCheck stata modules: ",i, " of ", length(which.do)))
      do.df$tab[[i]] =  tab.install.missing.modules(do.df$tab[[i]])
    }
  }

  incl.which.do = which.do[do.df$is.included[which.do]]

  if (!opts$use.includes) incl.which.do = NULL

  clear.and.create.dir(file.path(repbox.dir,"logs"))
  clear.and.create.dir(file.path(repbox.dir,"dta"))
  clear.and.create.dir(file.path(repbox.dir,"cmd"))
  clear.and.create.dir(file.path(repbox.dir,"output"))
  clear.and.create.dir(file.path(repbox.dir,"timer"))

  if (opts$extract.reg.info | !opts$keep.old.reg.info) {
    clear.and.create.dir(file.path(repbox.dir,"tsv"))
  } else {
    if (!dir.exists(file.path(repbox.dir,"tsv")))
      dir.create(file.path(repbox.dir,"tsv"))
  }

  # First inject code and generate repbox_.do files
  # for do files that are included in other do files
  incl.inject.res = lapply(incl.which.do, function(i) {
    do = do.df[i,]
    cat(paste0("\n inject code for included ", do$dofile))
    res = inject.do(do)
    res
  })

  if (opts$set.stata.defaults.perma) {
    # Run global Stata setup do file
    setup.do.file = system.file("misc/stata_setup.do",package = "repboxStata")
    file.copy(setup.do.file, file.path(project_dir,"repbox/stata"))
    run_stata_do(file.path(project_dir,"repbox/stata/stata_setup.do"), verbose=FALSE)
  }


  run.start.time = Sys.time()
  # Run do files that are not included in other do files
  which.do = setdiff(which.do, incl.which.do)
  do.li = lapply(seq_along(which.do), function(i) {
    do.i = which.do[i]
    do = do.df[do.i,]
    cat(paste0("\n",i, " of ", length(which.do), " inject and run"))
    do = stata.inject.and.run(do, opts=opts,start.time = run.start.time)
  })

  i = 2
  incl.extract.do.li = lapply(incl.which.do, function(i) {
    do = do.df[i,]
    log.file = file.path(repbox.dir,"logs",paste0("include_", do$donum,".log"))
    # The include did not run. Run manually instead
    if (!file.exists(log.file)) {
      repbox_problem(type="included_do_no_log", msg=paste0("\n run ", do$dofile, " (no existing log even though it should have been included)\n"), fail_action = "msg")
      do$is.included = FALSE
      do = stata.inject.and.run(do, opts=opts,start.time = run.start.time)
      return(do)
    }
    do$timeout = NA
    do$runtime = NA
    do
  })

  dotab = bind_rows(c(do.li, incl.extract.do.li)) %>%
    arrange(donum)
  saveRDS(dotab, file.path(repbox.dir,"dotab.Rds"))

  res = repbox_stata_extract(project_dir, dotab, opts=opts)

  parcels$stata_scalar = list(stata_scalar = res$scalar_df)

  invisible(parcels)
}


repbox_stata_extract = function(project_dir, dotab = readRDS.or.null(file.path(project_dir,"repbox/stata/dotab.Rds")), opts=rbs.opts()) {
  restore.point("repbox_stata_extract")

  if (is.null(dotab)) {
    cat("\nNo dotab.Rds file exists, cannot extract stata results.")
    return(NULL)
  }
  project = basename(project_dir)
  sup.dir = file.path(project_dir, "mod")

  res = extract.stata.results(project_dir, dotab)

  # After inject and run also some new dta
  # file may have been generated
  data.files = list.project.data.files(sup.dir)
  datamb = file.size(data.files) / 1e6
  datatab = tibble(datafile=data.files, database=basename(data.files),dataext = tools::file_ext(data.files), datamb=datamb)

  rep.res = c(res,list(datatab=datatab, timestamp = Sys.time()))
  saveRDS(rep.res, file.path(project_dir,"repbox/stata/repbox_results.Rds"))

  # Save tab separately to later include in db
  # We may delete repbox_results.Rds because it is too big
  saveRDS(res$tab, file.path(project_dir,"repbox/stata/tab.Rds"))

  if (opts$extract.reg.info) {
    if (!require(repboxReg)) {
      cat("\nExtraction of specific regression information is planned for a new package repboxReg. That package does not yet exist.\n")
      opts$extract.reg.info = FALSE
    }
  }

  if (opts$extract.reg.info) {
    regtab = extract.stata.regs(project_dir = project_dir, run.df = res$run.df, dotab=dotab)
  }

  if (opts$extract.scalar.vals) {
    # Storing of scalar values is newer.
    # So we directly store it as a parcel
    restore.point("ishfhsufhd")
    scalar_df = extract.stata.scalars(project_dir)
    scalar_parcel = list(stata_scalar=list(stata_scalar=scalar_df))
    repboxDB::repdb_save_parcels(scalar_parcel, file.path(project_dir, "repdb"), check=TRUE)
    rep.res$scalar_df = scalar_df
  }

  return(invisible(rep.res))
}


parse.sup.do = function(file, reg.cmds = get.regcmds(), project_dir="", catch.err=TRUE, code=NULL) {
  restore.point("parse.sup.do")
  #if (endsWith(file,"Table6.do")) {
  #  restore.point("jskfjlsfjlsflkjlf")
  #  stop()
  #}
  project=basename(project_dir)
  if (is.null(code)) {
    txt = readLines(file,warn = FALSE)
    # To avoid invalid multibyte string errors
    txt = enc2utf8(txt)
  } else {
    txt = sep.lines(code)
  }
  err = NULL
  next.cmd = "normalize.do"
  #err = try({
    s = repbox.normalize.do(txt,file)
    next.cmd = "make.tab"
    res = repbox.do.table(s)
    tab=res$tab; ph.df = res$ph.df
  #})
  if (is(err,"try-error")) {
    return(tibble(project=project, project_dir=project_dir, file = file,ok=FALSE, save.dta = list(NULL), use.dta = list(NULL), ph = list(NULL), tab=list(NULL),num.reg.lines = 0, reg.lines=list(NULL), parse.err = TRUE, parse.err.type=next.cmd))
  }

  #tab = tab.add.dta.file.ext(tab)
  tab$is.regcmd = tab$cmd %in% reg.cmds
  tab = tab.add.cmd.installed(tab)
  # Find saved data sets

  reg.lines = which(tab$cmd %in% reg.cmds)
  loads.data = sum(tab$cmd %in% c("use","u","us", "import","guse","insheet","gzuse")) > 0
  do = list(project=project, project_dir=project_dir,file=file, dofile = basename(file), doid =tools::file_path_sans_ext(basename(file)),ok=TRUE, ph = list(ph.df), tab=list(tab),num.reg.lines = length(reg.lines), reg.lines = list(reg.lines), parse.err=FALSE, parse.err.type="")

  dta.info = lapply(static.do.use.dta.info(do),function(x) list(x))

  if (!is.null(dta.info$found.dta.tab[[1]]))
    tab = left_join(tab, dta.info$found.dta.tab[[1]], by="line")
  do$tab[[1]] = tab
  do = as_tibble(c(do,dta.info))
  do$loads.data = loads.data
  do

}

# Choose which do files are run in which order
set.do.df.run.prio = function(do.df) {

  restore.point("set.do.df.run.prio")
  do.df$run.prio = 0
  do.df$parse.err = NA

  make.need.df = bind_rows(do.df$make.need.df)
  if (NROW(make.need.df)==0) {
    return(do.df)
  }
  need.df = filter(make.need.df, need, !found.dta)
  make.df = filter(make.need.df, make, !found.dta)

  doid.li = vector("list", NROW(do.df))

  use.doid = new.doid = do.df$doid
  do.df$run.prio = 1
  for (prio in setdiff(seq_len(NROW(do.df)),1)) {
    need.files = filter(need.df,doid %in% use.doid)$dta.base
    new.doid = filter(make.df, dta.base %in% need.files)$doid
    if (length(new.doid)==0) break
    rows = which(do.df$doid %in% new.doid)
    do.df$run.prio[rows] = prio
  }
  do.df = arrange(do.df, desc(run.prio))
  do.df
}

stata.inject.and.run = function(do, reg.cmds = get.regcmds(), save.changed.data=1, opts=rbs.opts(), start.time = NULL) {
  restore.point("stata.inject.and.run")

  # Check if global project timeout is reached
  if (!is.null(start.time)) {

    runsec = as.numeric(Sys.time())-as.numeric(start.time)
    if (isTRUE(runsec > opts$all.do.timeout)) {
      do$timeout = TRUE
      do$runtime = NA
      return(do)
    }
  }

  res = inject.do(do, reg.cmds=reg.cmds, save.changed.data=save.changed.data)
  do = res$do
  do = run.do(do)
  do
}

get.do.logfile = function(do) {
  project_dir = do$project_dir
  log.file = file.path(project_dir,"repbox/stata/logs", paste0("log_", do$donum,".log"))
  log.file
}

run.do = function(do, timeout=opts$timeout, verbose=TRUE, opts=rbs.opts()) {
  restore.point("run.do")

  project_dir = do$project_dir
  org.file = do$file
  do.dir = dirname(org.file)
  org.base = basename(org.file)
  new.base = paste0("repbox_", org.base)
  new.file = file.path(do.dir, new.base)

  # Create repbox dirs and remove files that will be overwritten
  repbox.dir = file.path(project_dir,"repbox/stata")
  if (!dir.exists(repbox.dir)) dir.create(repbox.dir)
  tsv.dir = file.path(repbox.dir,"tsv")
  create.dir(tsv.dir)
  create.dir(file.path(repbox.dir,"dta"))

  cmd.dir = file.path(repbox.dir,"cmd")
  if (!dir.exists(cmd.dir)) dir.create(cmd.dir)
  cmd.file = file.path(cmd.dir, paste0("precmd_",do$doid,".csv"))
  if (file.exists(cmd.file)) file.remove(cmd.file)
  cmd.file = file.path(cmd.dir, paste0("postcmd_",do$doid,".csv"))
  if (file.exists(cmd.file)) file.remove(cmd.file)

  res = run_stata_do(new.file,timeout=timeout, verbose=verbose)

  do$timeout = res$timeout
  do$runtime = res$runtime

  return(invisible(do))
}

get.graphcmds = function() {
  restore.point("get.graphcmds")
  cmds = getOption("repbox.graph.cmds")
  if (!is.null(cmds)) return(cmds)
  file = system.file("misc/graph_cmds.txt", package="repboxStata")
  cmds = readLines(file,warn = FALSE) %>% trimws()
  options(repbox.graph.cmds = cmds)
  return(cmds)
}

get.nographcmds = function() {
  restore.point("get.nographcmds")
  cmds = getOption("repbox.nograph.cmds")
  if (!is.null(cmds)) return(cmds)
  file = system.file("misc/nograph_cmds.txt", package="repboxStata")
  str = readLines(file,warn = FALSE) %>% trimws()
  cmds = tibble(
    cmd = str.left.of(str, " ",not.found = str),
    arg = str.right.of(str, " ", not.found=NA)
  )
  options(repbox.nograph.cmds = cmds)
  return(cmds)
}


get.regcmds = function() {
  restore.point("get.regcmds")
  reg.cmds = getOption("repbox.reg.cmds")
  if (!is.null(reg.cmds)) return(reg.cmds)
  file = system.file("misc/regression_cmds.txt", package="repboxStata")
  reg.cmds = readLines(file,warn = FALSE) %>% trimws()
  options(repbox.reg.cmds = reg.cmds)
  return(reg.cmds)
}

