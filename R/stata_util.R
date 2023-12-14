
example.run.stata.do = function() {
  run.stata.do("/home/rstudio/statabox/supp/aejmac_vol_3_issue_4_article_3/AEJMacro-2010-0079_data/repbox_summarystats_aej_final.do", timeout=10)
}

run.stata.do = function(do.file, stata.bin=get.stata.bin(), set.dir=TRUE, nostop=TRUE, timeout=60*5, verbose=TRUE, use.timeout = !is.windows, is.windows =  .Platform$OS.type == "windows") {
  restore.point("run.stata.do")
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

  if (is.windows & nostop) {
    cmd = paste0(stata.bin,' /e do "',file,'"', if(nostop) ', nostop')
    #
    # Note: On Windows in BATCH mode no shell commands can be executed.
    # This means rcall cannot be used.
  } else if (is.windows & !nostop) {
    stop("Need to check windows without nostop")
    cmd = paste0(stata.bin,' /e "',file,'"')
  } else if (!nostop) {
    # Without nostop we must write the code differently (at least on Linux)
    # without explicitly calling the do command
    cmd = paste0(stata.bin,' -b "',file,'"')
    if (!is.empty(timeout) & use.timeout) {
      cmd = paste0("timeout ", timeout," ", cmd)
    }
  } else if (!is.empty(timeout) & use.timeout) {
    cmd = paste0(stata.bin,' -b \'do "',file,'"', if(nostop) ', nostop','\'')
    cmd = paste0("timeout ", timeout," ", cmd)
  } else {
    cmd = paste0(stata.bin,' -b do "',file,'"', if(nostop) ', nostop')
  }

  start.time = Sys.time()

  res = system(cmd)
  end.time = Sys.time()
  runtime = as.numeric(Sys.time()-start.time)

  if (!is.empty(timeout) & !is.windows) {
    if (res != 0) {
      if (verbose) {
        cat(paste0("\n    stopped due to timeout (", timeout, " seconds.)\n"))
      }
      return(list(timeout=TRUE, runtime=runtime))
    }
  } else if (res!=0) {
    stop(paste0("There was an error (code ", res,") when running the Stata command:\n\n", cmd, "\n\nPossible you have to specify the path to the Stata binary by calling set.stata.paths(...)."),call. = FALSE)
  }
 if (verbose) {
    cat(paste0("\n    finished after ", round(runtime,2), " seconds.\n"))
  }
  return(list(timeout=FALSE, runtime=runtime))
}

set.stata.paths = function(stata.bin = NULL,ado.dirs=NULL, base.ado.dir = NULL, stata.dir=NULL, is.windows =  .Platform$OS.type == "windows") {
  if (is.windows) {
    if (!is.null(stata.dir)) {
      if (is.null(stata.bin)) {
        stata.bin = paste0(stata.dir,"/StataSE-64.exe")
      } else if (basename(stata.bin)==stata.bin) {
        stata.bin = paste0(stata.dir, "/", stata.bin)
      }
      if (is.null(base.ado.dir)) {
        base.ado.dir = file.path(stata.dir, "ado","base")
      }
    }
  }
  options(repbox.stata.paths = list(stata.bin=stata.bin, ado.dirs = ado.dirs, base.ado.dir = base.ado.dir))
}

check.stata.paths = function(is.windows = .Platform$OS.type == "windows") {
  p = getOption("repbox.stata.paths")
  if (is.null(p) & is.windows) {
    warning("On windows you must set custom stata paths. Please call set.stata.paths")
    return(FALSE)
  }
  ok = TRUE
  stata.bin = get.stata.bin()
  if (basename(stata.bin)!=stata.bin) {
    if (!file.exists(stata.bin)) {
      ok = FALSE
      warning(paste0("The stata binary ", stata.bin, " could not be found."))
    }
  }

  dir = module.dirs = get.ado.dirs()
  if (!any(dir.exists(dir))) {
    ok = FALSE
    warning(paste0("The modules ado directories ",paste0(dir, collapse=", "), " do not all exist."))
  }
  dir = get.base.ado.dir()
  if (!dir.exists(dir)) {
    ok = FALSE
    warning(paste0("The base ado directory ",dir, " does not exist."))
  }

  if (ok) {
    if (!any(file.exists(paste0(c(module.dirs, dir, paste0(module.dirs, "/plus")),"/r/rcall.ado")))) {
      ok = FALSE
      warning("In your module ado dir you should have installed  r/rcall.ado")
    }
    if (!any(file.exists(paste0(c(module.dirs, dir, paste0(module.dirs, "/plus")),"/p/parmest.ado")))) {
      ok = FALSE
      warning("In your module ado dir you should have installed  p/parmest.ado")
    }
  }

  return(ok)
}

get.stata.bin = function(default = "stata-se") {
  res = getOption("repbox.stata.paths")$stata.bin
  if (is.null(res)) return(default)
  return(res)
}

get.ado.dirs = function(default = "~/ado/plus") {
  res = getOption("repbox.stata.paths")$ado.dirs
  if (is.null(res)) return(default)
  return(res)
}

get.base.ado.dir = function(default = "/usr/local/stata/ado/base") {
  res = getOption("repbox.stata.paths")$base.ado.dir
  if (is.null(res)) return(default)
  return(res)
}
