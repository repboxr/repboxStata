
example.run_stata_do = function() {
  run_stata_do("/home/rstudio/statabox/supp/aejmac_vol_3_issue_4_article_3/AEJMacro-2010-0079_data/repbox_summarystats_aej_final.do", timeout=10)
}

run_stata_do = function(do.file, stata_bin=get_stata_bin(), set.dir=TRUE, nostop=TRUE, timeout=60*5, verbose=TRUE, use.timeout = !is_windows, is_windows =  .Platform$OS.type == "windows") {
  restore.point("run_stata_do")
  if (set.dir) {
    do.dir = dirname(do.file)
    old.dir = getwd()
    setwd(do.dir)
    file = basename(do.file)
  } else {
    file = do.file
  }


  if (verbose) {
    cat("\n\nRun ", do.file)
  }

  if (is_windows & nostop) {
    cmd = paste0(stata_bin,' /e do "',file,'"', if(nostop) ', nostop')
    #
    # Note: On Windows in BATCH mode no shell commands can be executed.
    # This means rcall cannot be used.
  } else if (is_windows & !nostop) {
    #stop("Need to check windows without nostop")
    cmd = paste0(stata_bin,' /e "',file,'"')
  } else if (!nostop) {
    # Without nostop we must write the code differently (at least on Linux)
    # without explicitly calling the do command
    cmd = paste0(stata_bin,' -b "',file,'"')
    if (!is.empty(timeout) & use.timeout) {
      cmd = paste0("timeout ", timeout," ", cmd)
    }
  } else if (!is.empty(timeout) & use.timeout) {
    cmd = paste0(stata_bin,' -b \'do "',file,'"', if(nostop) ', nostop','\'')
    cmd = paste0("timeout ", timeout," ", cmd)
  } else {
    cmd = paste0(stata_bin,' -b do "',file,'"', if(nostop) ', nostop')
  }

  start.time = Sys.time()

  res = system(cmd)
  end.time = Sys.time()
  runtime = as.numeric(Sys.time()-start.time)

  if (!is.empty(timeout) & !is_windows) {
    if (res != 0) {
      if (verbose) {
        cat(paste0("\n    stopped due to timeout (", timeout, " seconds.)\n"))
      }
      return(list(timeout=TRUE, runtime=runtime))
    }
  } else if (res!=0) {
    stop(paste0("There was an error (code ", res,") when running the Stata command:\n\n", cmd, "\n\nPossible you have to specify the path to the Stata binary by calling set_stata_paths(...)."),call. = FALSE)
  }
 if (verbose) {
    cat(paste0("\n    finished after ", round(runtime,2), " seconds.\n"))
  }
  return(list(timeout=FALSE, runtime=runtime))
}

set_stata_paths = function(stata_bin = NULL,ado_dirs=NULL, base_ado_dir = NULL, stata_dir=NULL, is_windows =  .Platform$OS.type == "windows") {
  if (is_windows) {
    if (!is.null(stata_dir)) {
      if (is.null(stata_bin)) {
        stata_bin = paste0(stata_dir,"/StataSE-64.exe")
      } else if (basename(stata_bin)==stata_bin) {
        stata_bin = paste0(stata_dir, "/", stata_bin)
      }
      if (is.null(base_ado_dir)) {
        base_ado_dir = file.path(stata_dir, "ado","base")
      }
    }
  }
  options(repbox.stata.paths = list(stata_bin=stata_bin, ado_dirs = ado_dirs, base_ado_dir = base_ado_dir))
}

check_stata_paths_and_ado = function(is_windows = .Platform$OS.type == "windows", check_base_ado_dir = FALSE, on_fail = c("error","warn","msg","silent")[1]) {
  p = getOption("check_stata_paths_and_ado")
  if (on_fail=="error") {
    fail_fun = function(...) stop(paste0(...))
  } else if (on_fail=="warn") {
    fail_fun = function(...) warning(paste0(...))
  } else if (on_fail=="msg") {
    fail_fun = function(...) cat(paste0(...))
  } else {
    fail_fun = function(...) {}
  }

  if (is.null(p) & is_windows) {
    fail_fun("On windows you must set custom stata paths. Please call set_stata_paths")
    return(FALSE)
  }
  ok = TRUE
  stata_bin = get_stata_bin()
  if (basename(stata_bin)!=stata_bin) {
    if (!file.exists(stata_bin)) {
      ok = FALSE
      fail_fun(paste0("The stata binary ", stata_bin, " could not be found."))
    }
  }

  dir = ado_dirs = get_ado_dirs()
  if (!any(dir.exists(dir))) {
    ok = FALSE
    fail_fun(paste0("The modules ado directories ",paste0(dir, collapse=", "), " do not all exist."))
  }

  if (check_base_ado_dir) {
    dir = get_base_ado_dir()
    if (!dir.exists(dir)) {
      ok = FALSE
      fail_fun(paste0("The base ado directory ",dir, " does not exist."))
    }
  }


  if (ok) {
    required_ado = c("parmest.ado",basename(list_repbox_ado_files(only_required=TRUE)))
    initials = substring(required_ado,1,1)
    exists = rep(FALSE, length(required_ado))
    for (dir in ado_dirs) {
      exists = exists |
        file.exists(paste0(dir,"/",required_ado)) |
        file.exists(paste0(dir,"/plus/",required_ado)) |
        file.exists(paste0(dir,"/plus/",initials,"/",required_ado))
    }
    if (!all(exists)) {
      ok = FALSE
      msg = paste0("\nNot all repbox specific Stata ado files can be found in your specified ado directories:\n\n  ", paste0(ado_dirs, collapse="\n  "), "\n\nPlease call copy_repbox_ado_files(ado_dir) to copy the required ado files.")
      fail_fun(msg)
    }
  }

  return(ok)
}

get_stata_bin = function(default = "stata-se") {
  res = getOption("repbox.stata.paths")$stata_bin
  if (is.null(res)) return(default)
  return(res)
}

