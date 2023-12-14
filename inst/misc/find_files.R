

example = function() {
  file = '.\\your\\path/outputdata\\new_donors_MAIN'
  sup.dir = "~/repbox/projects_ejd/aejpol_13_4_7/mod"
  wdir = sup.dir
  default.ext = "dta"

  find.path("../../../sdsd", "/home/rstudio/repbox/projects/testsupp/mod")
}

is.abs.path = function(x) {
  startsWith(x, "/") | startsWith(x,"~") | grepl(":",x, fixed=TRUE)
}

# Find closest directory to dir starting from end
find.closest.path = function(dir, dirs) {
  sp = strsplit(dir, "/")[[1]]
  n = length(sp)
  for (i in 1:n) {
    di = do.call(file.path,as.list(sp[i:n]))
    ok= which(endsWith(dirs, di))
    if (any(ok))
      return(dirs[ok[1]])
  }
  return(NULL)
}


write.log.find.path = function(found.file, org.file, sup.dir,cmd, default.ext, wdir,mode="") {
  log.file = file.path(substring(sup.dir,1,nchar(sup.dir)-4),"repbox","stata","find_path_log.csv")

  if(!file.exists(log.file)) return()
  log.txt = paste0(c(mode, found.file, org.file, sup.dir, cmd, default.ext, wdir), collapse=",")
  write(log.txt, log.file, append=TRUE)

}

# if we search for "/code/table1.do we want "abs_path/code/repbox_table1.do"
find.repbox.do.file = function(...) {
  file = find.path(...)
  if (!isTRUE(endsWith(tolower(file), ".do"))) return(file)
  base = basename(file)
  dir = dirname(file)
  if (dir == ".") return(paste0("repbox_"), base)
  return(paste0(dir,"/", "repbox_", base))
}

find.path = function(file, sup.dir,cmd="", default.ext="", wdir = getwd()) {
  #restore.point("find.path")
  setwd(wdir)

  org.file = file


  # normalize to linux convention
  file = gsub("//","/", file, fixed=TRUE)

  # starts with main directory
  # possibly because some Stata path
  # prefix variable was empty
  # Then add sup.dir to file
  if (isTRUE(substring(file,1,1)=="/")) {
    file = paste0(sup.dir, file)
  }

  if (default.ext != "") {
    ext = tools::file_ext(file)
    if (ext == "") {
      file = paste0(file,".",default.ext)
    }
  }

  if (cmd %in% c("cd","adopath")) {
    dir = file
    abs.dir = normalizePath(dir,mustWork = FALSE)
    sup.dir = normalizePath(sup.dir, mustWork = FALSE)

    if (dir.exists(dir)) {
      if (startsWith(abs.dir, sup.dir)) {
        write.log.find.path(dir, org.file,  sup.dir,cmd, default.ext, wdir,"direct_exists")
        return(dir)
      }
    }
    dirs = list.dirs(sup.dir,recursive = TRUE, full.names = TRUE)
    mdir = find.closest.path(dir, dirs)
    if (!is.null(mdir)) {
      write.log.find.path(mdir, org.file,  sup.dir,cmd, default.ext, wdir,"dir_found")
      return(mdir)
    }
    if (is.abs.path(dir)) {
      write.log.find.path(mdir, org.file,  sup.dir,cmd, default.ext, wdir,"nodir_abs")
      return(sup.dir)
    } else {
      write.log.find.path(mdir, org.file,  sup.dir,cmd, default.ext, wdir,"nodir_rel")
      return(sup.dir)
    }
  }


  if (isTRUE(try(file.exists(file)))) {
    write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir,"direct_exists")
    return(file)
  }


  # In a save command we just want to match the directory
  # the file itself may not exist
  if (cmd %in% c("save","sa","sav","saveold", "saving", "export","estout","esttab","mkdir","graph", "gr","gra")) {
    if (cmd=="mkdir") {
      if (endsWith(file,"/") | endsWith(file,"\\")) {
        file = substring(file,1,nchar(file)-1)
      }
    }
    dirs = list.dirs(sup.dir,recursive = TRUE, full.names = TRUE)

    dir = dirname(file)

    # Normalize dir: This also transforms a relative path
    # to an absolute path
    abs.dir = normalizePath(dir,mustWork = FALSE)
    sup.dir = normalizePath(sup.dir, mustWork = FALSE)


    if (isTRUE(try(dir.exists(dir)))) {
      # Save directory should also be inside sup.dir
      if (startsWith(abs.dir, sup.dir)) {
        write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir, "save_nodir_exists")
        return(file)
      }

    }
    mdir = find.closest.path(dir, dirs)
    if (is.null(mdir)) {
      # If no match is found for absolute path
      # return sup.dir
      if (is.abs.path(dir) | !startsWith(abs.dir, sup.dir)) {
        file = file.path(sup.dir,basename(file))
        write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir, "save_nodir_abs")
        return(file)
      }
      # For a relative path just return the original file
      try(dir.create(dirname(file)))
      write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir,"save_nodir_rel")
      return(file)
    }
    file = file.path(mdir, basename(file))
    write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir,"save_dir_found")
    return(file)
  }

  files = list.files(sup.dir,full.names = TRUE,recursive = TRUE, include.dirs = TRUE)
  mfile = find.closest.path(file,files)
  if (is.null(mfile)) {
    write.log.find.path(file, org.file,  sup.dir,cmd, default.ext, wdir, "not_found")
    return(file)
  }
  # Return absolute path
  write.log.find.path(mfile, org.file,  sup.dir,cmd, default.ext, wdir, "found")
  return(mfile)
}

