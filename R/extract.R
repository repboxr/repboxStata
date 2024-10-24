#
# This file contains functions to extract the results after the modififed Stata do files have been run.
#
# Note that we cannot perform extraction separately for each do file. If there are Stata programs the log of another do file may contain commands associated with the do file where the program is defined.
#

extract.stata.results = function(project_dir, dotab, opts = rbs.opts()) {
  restore.point("extract.stata.results")

  project = basename(project_dir)
  tab = lapply(seq_len(NROW(dotab)), function(i) {
    tab = dotab$tab[[i]]
    tab$project = project
    tab$donum = dotab$donum[[i]]
    select(tab, project, donum, line,orgline, cmd,  everything())
  }) %>% bind_rows()


  run.df = extract.stata.run.cmds(project_dir)
  log.df = extract.stata.logs(project_dir)
  run.df = left_join(run.df, log.df, by=c("donum","line","counter"))
  run.df = left_join(run.df, select(tab, donum, line, orgline, cmd, is.regcmd, in.program, opens_block), by=c("donum", "line"))
  run.df = arrange(run.df, rootdonum, counter, donum,line)
  run.df = adapt.run.df.error.and.log(run.df, project_dir)
  run.df = adapt.run.df.for.timeout(run.df, dotab, project_dir, opts=opts)
  run.df = add.has.data.to.run.df(run.df)

  run.df = extract.stata.do.output(project_dir, run.df)

  # Extract written Latex code by commands like esttab
  # Need to extend to created images
  #run.err.df = extract.do.output(do, run.err.df)

  # Add latex output to logtxt. This allows to match numbers
  # run.err.df$logtxt = ifelse(run.err.df$out.txt=="", run.err.df$logtxt,
  #                            paste0(run.err.df$logtxt,"\n\n---Content of created file---\n\n", run.err.df$out.txt)
  # )


  tab = add.tab.error.info(tab, run.df)

  # Add error info for dotab
  agg = run.df %>%
    group_by(donum) %>%
    summarize(
      run.err = any(is.true(runerr))
    )
  dotab = left_join(dotab, agg, by=c("donum"))

  dotab = extract.do.runtimes(project_dir, dotab)

  data.use = stata.repbox.data.use.info(run.df=run.df, dotab=dotab)
  saveRDS(data.use, file.path(project_dir,"repbox/stata/do_data_use.Rds"))

  list(run.df=run.df,tab=tab, dotab=dotab)
}

extract.stata.run.cmds = function(project_dir) {
  restore.point("extract.stata.run.cmds")
  dir = file.path(project_dir, "repbox/stata/cmd")

  files = list.files(dir,"^(postcmd|precmd).*csv$",full.names = FALSE)

  pre.files = paste0(dir,"/",files[startsWith(files,"precmd_")])
  str = lapply(pre.files, readLines, warn=FALSE) %>% unlist()

  donum = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  line = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  counter = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  rootdonum = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  stime = str.left.of(str,";")
  str = str.right.of(str,";")
  wdir = str.left.of(str,";")
  str = str.right.of(str,";")
  foundfile = str.left.of(str,";")
  cmdline = trimws(str.right.of(str,";"))

  prun.df = as_tibble(list(donum=donum, line=line, counter=counter, rootdonum=rootdonum, stime=stime, cmdline=cmdline,wdir=wdir, foundfile=foundfile))

  # Postcmd files
  post.files = paste0(dir,"/",files[startsWith(files,"postcmd_")])
  str = lapply(post.files, readLines, warn=FALSE) %>% unlist()

  donum = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  line = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  counter = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  etime = str.left.of(str,";")
  str =  trimws(str.right.of(str,";",not.found = ""))
  errcode = as.integer(str.left.of(str,";"))
  str =  trimws(str.right.of(str,";",not.found = ""))
  datasig = str.left.of(str,";")
  str = str.right.of(str,";", not.found="")
  timevar = str.left.of(str,";")
  str =  trimws(str.right.of(str,";",not.found = ""))
  panelvar = str.left.of(str,";")
  str =  trimws(str.right.of(str,";",not.found = ""))
  tdelta = str.left.of(str,";")

  runerrcode = errcode
  runerr = is.true(runerrcode >= 100)

  post.df = as_tibble(list(donum=donum, line=line, counter=counter,etime=etime, datasig=datasig, timevar=timevar, panelvar=panelvar, tdelta=tdelta, runerr=runerr, runerrcode=runerrcode, runerrmsg=""))


  df = left_join(prun.df, post.df, by = c("donum", "line", "counter"))


  df$runsec = time.diff(df$stime, df$etime)

  df
}


extract.stata.scalars = function(project_dir) {
  restore.point("extract.stata.scalars")
  dir = file.path(project_dir, "repbox/stata/cmd")
  files = list.files(dir,"^(scalar).*csv$",full.names = TRUE)

  str = lapply(files, readLines, warn=FALSE) %>% unlist()

  donum = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  line = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  runid = as.integer(str.left.of(str,";"))
  str = str.right.of(str,";")
  var = str.left.of(str,";")
  str = str.right.of(str,";")
  val = str
  num_val = suppressWarnings(as.numeric(val))

  scalar.df = as_tibble(list(script_num=donum, runid=runid,line=line, scalar_var=var, scalar_val=val, scalar_num_val=num_val))

  scalar.df
}


# This functions tries to assess for every row of run.df
# whether the supposed data set is available.
#
# If there is an error in the previous command that loads data
# we assume that the data set is not available.
#
# Dealing with includes and preserve / restore makes the code
# more complex.
add.has.data.to.run.df = function(run.df) {
  restore.point("add.has.data.to.run.df")
  new.dat.cmd = c("use","u","us", "clear","import","insheet","guse","gzuse")
  add.dat.cmd = c("merge","joinby")
  run.df$data.cmd.type = ""
  rows = run.df$cmd %in% new.dat.cmd
  run.df$data.cmd.type[rows] = "new"
  rows = run.df$cmd %in% add.dat.cmd
  run.df$data.cmd.type[rows] = "add"
  run.df$has.data = NA

  # Update: 2024-04-13 We ignore errors in clear commands...
  # They can just arise because we changed clear all to clear
  # Then wrongly we might get missing data labels
  clear_err_rows = which(run.df$cmd == "clear" & run.df$runerr)
  run.df$runerr[clear_err_rows] = FALSE
  run.df$runerrcode[clear_err_rows] = 0
  run.df$runerrmsg[clear_err_rows] = ""




  oldrootdonum = -1

  # Temporary fix: Don't know yet why
  # rootdonum sometimes has NA
  na.rows = which(is.na(run.df$rootdonum))
  if (length(na.rows)) {
    max.root = max(run.df$rootdonum,na.rm = TRUE)
    if (is.na(max.root)) max.root = 0

    # We have duplicated roots for na.rows
    if (any(duplicated(run.df$counter[na.rows]))) {
      run.df$rootdonum[na.rows] = max.root + run.df$donum[na.rows]
      run.df = arrange(run.df, rootdonum, counter, donum,line)
    # All rootdonum with NA are the same root
    } else {
      run.df$rootdonum[na.rows] = max.root + 1
    }
  }


  for (i in seq_len(NROW(run.df))) {
    rootdonum = run.df$rootdonum[i]

    #if (i==9) stop()

    # Complete restart
    if (rootdonum != oldrootdonum) {
      preserve.has.data = NA
      has.data = TRUE
      do.stack = list()
      donum = -1
      pdonum = -1
      oldrootdonum = rootdonum
    }

    type = run.df$data.cmd.type[i]
    cmd = run.df$cmd[i]
    err = run.df$runerr[i]
    prev.donum = donum
    donum=run.df$donum[i]

    if (cmd == "do" | cmd == "run") {
      do.stack = c(list(list(pdonum = donum, has.data = has.data, preserve.has.data=preserve.has.data)), do.stack)
      pdonum = donum
    } else {

      # Check if a do command is finished and we are back to the parent
      # do file. We then move back to the parent state
      if (donum != prev.donum & !run.df$in.program[i] & length(do.stack)>0 & donum==pdonum) {
        pstate = do.stack[[1]]
        has.data = pstate$has.data
        preserve.has.data = pstate$preserve.has.data
        do.stack = do.stack[-1]
      }
    }

    if (type == "new") {
      has.data = isTRUE(!err)
    } else if (type =="add") {
      has.data = !err & has.data
    } else if (isTRUE(cmd=="restore" & !err)) {
      has.data = preserve.has.data
    } else if (isTRUE(cmd=="preserve" & !err)) {
      preserve.has.data = has.data
    }
    run.df$has.data[i] = has.data
  }
  run.df
}

adapt.run.df.error.and.log = function(run.df, project_dir) {
  restore.point("adapt.run.df.error.and.log")

  # Remove log from do, run and include commands
  rows = run.df$cmd %in% c("do","include","run")
  run.df$logtxt[rows] =  "\nOutput of do and include commands is not shown.\nLook in the corresponding do file that is run."

  # Change `r(repbox_corrected_path)' in log where
  # filename was replaced
  rows = is.true(run.df$foundfile != "")
  foundfiles = run.df$foundfile[rows]
  sup.dir = normalizePath(file.path(project_dir, "mod"), mustWork = FALSE,winslash = "/")
  foundfiles = gsub(sup.dir, "${repbox_path}", foundfiles, fixed=TRUE)

  run.df$logtxt[rows] =  stringi::stri_replace_all(run.df$logtxt[rows],fixed = "`r(repbox_corrected_path)'",replacement = foundfiles)

  rows = run.df$opens_block
  run.df$logtxt[rows] = ""

  # Perform some adaptions
  na.rows = is.na(run.df$runerr)
  if (sum(na.rows)>0) {
    err.na.rows =  is.true(na.rows & (!run.df$opens_block & (!run.df$cmd %in% c(c("do","include","run","if","else","end","program","prog","pr","progr","exit")))))

    # runerr must be set to FALSE or TRUE (not NA)
    run.df$runerr[na.rows] = err.na.rows[na.rows]
    run.df$runerrmsg[err.na.rows] = paste0(run.df$cmdline[err.na.rows], ": Error message could not be retrieved.")
  }

  rows = which(is.true(run.df$runerr & !is.na(run.df$runerrcode)))
  for (i in rows) {
    str = sep.lines(run.df$logtxt[i])
    err.msg = paste0(unique(str), collapse=" ")
    run.df$runerrmsg[i] = err.msg
  }
  run.df
}

extract.do.runtimes = function(project_dir, dotab) {
  restore.point("extract.do.runtimes")
  dotab$start_time = dotab$end_time = NA

  file = paste0(project_dir,"/repbox/stata/timer/start.txt")
  if (file.exists(file)) {
    str = readLines(file)
    donum = str.left.of(str,";") %>% as.integer()
    time = str.right.of(str,";") %>%
      lubridate::parse_date_time("%h:%M:%s;%d %b %y")

    dotab$start_time[donum] = time
  }

  file = paste0(project_dir,"/repbox/stata/timer/end.txt")
  if (file.exists(file)) {
    str = readLines(file)
    donum = str.left.of(str,";") %>% as.integer()
    time = str.right.of(str,";") %>%
      lubridate::parse_date_time("%h:%M:%s;%d %b %y")

    dotab$end_time[donum] = time
  }
  dotab = mutate(dotab,
    runtime = ifelse(is.na(start_time) | is.na(end_time), runtime, as.numeric(end_time)-as.numeric(start_time))
  )
  dotab
}


extract.stata.logs = function(project_dir) {
  restore.point("extract.stata.logs")
  dir = file.path(project_dir, "repbox/stata/logs")
  files = list.files(dir,glob2rx("log_*.log"),full.names = TRUE)

  #   # I hoped this code would be faster, but that seems not the case
  #
  #   txt = unlist(lapply(files, readLines, warn=FALSE)) %>% enc2utf8()
  #   bdf = extract.inject.blocks(txt, type="RUNCMD")
  #
  #   log.df = lapply(seq_len(NROW(bdf)), function(i) {
  #     str = bdf$str[[i]]
  #     donum = bdf$donum[i]
  #     line=bdf$line[i]
  #     counter=bdf$counter[i]
  #     ignore = has.substr(str,"#~# INJECT") | has.substr(str,"#~# END INJECT")
  #     str = str[!ignore]
  #     if (isTRUE(str[length(str)]==".")) str[-length(str)]
  #     #str = str[nchar(str)>0]
  #     logtxt = merge.lines(str)
  #     # To avoid later invalid multibyte string errors
  #     logtxt = iconv(logtxt, to="UTF-8", sub="?")
  #     logtxt[is.na(logtxt)] = ""
  #     logtxt = gsub("capture:  noisily: ","",logtxt,fixed = TRUE)
  #     tibble(donum=donum, line=line, counter=counter,logtxt=logtxt)
  #   }) %>% bind_rows()
  #
  #   return(log.df)


  res.li = lapply(files, function(file) {
    txt = readLines(file,warn=FALSE) %>% enc2utf8()
    check.stata.log.for.critical.problems(txt)
    bdf = extract.inject.blocks(txt, type="RUNCMD")

    log.df = lapply(seq_len(NROW(bdf)), function(i) {
      str = bdf$str[[i]]
      donum = bdf$donum[i]
      line=bdf$line[i]
      counter=bdf$counter[i]
      ignore = has.substr(str,"#~# INJECT") | has.substr(str,"#~# END INJECT")
      str = str[!ignore]
      if (isTRUE(str[length(str)]==".")) str[-length(str)]
      #str = str[nchar(str)>0]
      logtxt = merge.lines(str)

      # We don't store log of a custom function
      # inside which we store logs again
      if (grepl("!.REPBOX.CUSTOM.PROGRAM>*",logtxt, fixed=TRUE)) {
        logtxt = ""
      }

      # To avoid later invalid multibyte string errors
      logtxt = iconv(logtxt, to="UTF-8", sub="?")
      logtxt[is.na(logtxt)] = ""
      logtxt = gsub("capture:  noisily: ","",logtxt,fixed = TRUE)
      tibble(logfile = rep(basename(file), length(logtxt)), donum=donum, line=line, counter=counter,logtxt=logtxt)
    }) %>% bind_rows()
  })

  log.df = bind_rows(res.li)
  return(log.df)
}

check.stata.log.for.critical.problems = function(txt) {
  if (any(has.substr(txt,"command repbox_correct_path is unrecognized"))) {
    stop("The Stata run did not work properly since the required ado command 'repbox_correct_path' could not be found. Make sure that you install it into your ado path.")
  }

}


extract.inject.blocks = function(txt, type, broken.rows = 5) {
  restore.point("extract.inject.blocks")
  start = paste0("#~# INJECT ", type," ")
  end = paste0("#~# END INJECT ", type," ")
  sl = which(startsWith(txt,start))
  el = which(startsWith(txt,end))
  skey = str.right.of(txt[sl],start)
  ekey = str.right.of(txt[el],end)
  if (length(sl)==0) return(
    return(as_tibble(list(start=integer(0), end=integer(0), head=character(0), str = vector("list",0), donum=integer(0), line=integer(0), counter=integer(0), broken=logical(0))))
  )

  # NOTE: Not each start block may have an end block
  # the block can be broken if there is an error
  # inside a for loop.
  sel = el[match(skey, ekey)]
  broken = is.na(sel)
  str = lapply(seq_along(sl), function(i) {
    if (broken[i]) {
      return(txt[sl[i]:min(NROW(txt),sl[i]+broken.rows)])
    }
    txt[sl[i]:sel[i]]
  })
  head = sapply(seq_along(sl), function(i) {
    txt[sl[i]]
  })
  s = str.right.of(head,start)
  donum = str.left.of(s, " ") %>% as.integer()
  s = str.right.of(s, " ")
  line = str.left.of(s, " ") %>% as.integer()
  counter = str.right.of(s, " ") %>% as.integer()
  return(as_tibble(list(donum=donum, line=line, counter=counter, start=sl, end=sel, head=head, str=str, broken=broken)))
}



extract.start.end.line.blocks = function(txt, start, end, startsWith=TRUE, has.line.counter=TRUE) {
  restore.point("extract.start.end.line.blocks")
  sl = which(startsWith(txt,start))
  el = which(startsWith(txt,end))
  txt[sl]
  txt[el]
  cbind(c(txt[sl],NA), txt[el])
  if (length(sl) != length(el)) stop("Number of start and end lines is not equal.")
  if (length(sl)==0) return(
    return(as_tibble(list(start=integer(0), end=integer(0), head=character(0), str = vector("list",0), line=integer(0), counter=integer(0))))
  )
  str = lapply(seq_along(sl), function(i) {
    txt[sl[i]:el[i]]
  })
  head = sapply(seq_along(sl), function(i) {
    txt[sl[i]]
  })
  if (!has.line.counter)
    return(as_tibble(list(start=sl, end=el, head=head, str=str)))
  if (has.line.counter) {
    s = str.right.of(head,start)
    line = str.left.of(s, " ") %>% as.integer
    counter = str.right.of(s, " ") %>% as.integer
    return(as_tibble(list(start=sl, end=el, head=head, str=str, line=line, counter=counter)))
  }
}

add.tab.error.info = function(tab, run.df) {
  restore.point("add.tab.error.info")
  runerr.agg = run.df %>%
    group_by(donum,line) %>%
    summarize(
      runs = sum(!runerr, na.rm=TRUE),
      errruns = sum(runerr, na.rm=TRUE),
      runerrmsg = if (isTRUE(any(runerr))) first(runerrmsg[runerr]) else "",
      runerr = any(runerr, na.rm=TRUE),
      cmdline = first(cmdline)
    )

  if (!has.col(tab,"found.all.dta"))
    tab$found.all.dta = NA
  tab = tab %>% left_join(runerr.agg, by=c("donum", "line"))
  rows = which(is.na(tab$runs))
  tab$runs[rows] = 0
  tab$errruns[rows] = 1
  tab$runerr[rows] = 1
  tab$runerrmsg[rows] = "not run due to earlier error"
  tab
}


# code to extract special output
extract.stata.do.output = function(project_dir, run.df) {
  restore.point("extract.stata.do.output")

  run.df = add.col(run.df, "out.ext","")
  run.df = add.col(run.df, "out.txt","")
  run.df = add.col(run.df, "out.img.file","")


  output.dir = paste0(project_dir,"/repbox/stata/output")
  out.files = list.files(output.dir, full.names=TRUE)

  if (length(out.files)==0) return(run.df)


  donum_line_counter = basename(out.files) %>% str.left.of(".")

  run.rows = match(donum_line_counter, paste0(run.df$donum,"_",run.df$line,"_", run.df$counter))

  img.dir = paste0(project_dir,"/repbox/www/images")
  if (!dir.exists(img.dir)) dir.create(img.dir,recursive = TRUE)


  run.df$out.ext[run.rows] = tools::file_ext(out.files)

  # Latex files
  rows = endsWith(out.files, ".tex")
  files = out.files[rows]
  f = files[1]
  txt = lapply(files, function(f) {
    readLines(f, warn=FALSE) %>% merge.lines()
  }) %>% unlist()

  run.df$out.txt[run.rows[rows]] = txt
  for (f in files) {
    pdf.file = paste0(img.dir,"/", tools::file_path_sans_ext(basename(f)),".pdf")
    try(compile.tex.fragment(f,pdf.file = pdf.file, make.png = TRUE,delete.pdf = TRUE))
  }
  img.files = paste0(tools::file_path_sans_ext(basename(files)),".png")

  # TO DO: set "" for non-existing image files
  # This can happen if there is a latex compilation error
  exists = file.exists(paste0(img.dir,"/",img.files))
  img.files[!exists] = ""

  run.df$out.img.file[run.rows[rows]] = img.files


  # svg graphs
  rows = endsWith(out.files, ".svg")
  files = out.files[rows]
  file.copy(files, to = img.dir,overwrite = TRUE)
  run.df$out.img.file[run.rows[rows]] = basename(files)

  return(run.df)
}

# If there is a timeout: we will change the error message of the
# last command to "Timeout when running do file"
adapt.run.df.for.timeout = function(run.df, dotab, project_dir, opts) {
  restore.point("adapt.run.df.for.timeout")
  donums = dotab$donum[is.true(dotab$timeout)]
  # No timeout
  if (length(donums)==0) return(run.df)
  rows = which(run.df$donum %in% donums & is.na(run.df$runerrcode))
  # Only set last row of each do file as timeout run
  rows = rows[!duplicated(run.df$donum[rows], fromLast=TRUE)]

  run.df$runerrcode[rows] = -1L
  run.df$runerrmsg[rows] = paste0("Timeout after ", opts$timeout, " sec. when running do file")
  repbox_problem(type="timeout", msg=paste0("Timeout (", opts$timeout," sec.) in ", length(donums)," do files."),fail_action = "msg")
  run.df
}


