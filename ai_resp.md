To understand why the regressions from `Overview_Aufbereitet.do` are duplicated in the `stata_run_cmd` parcel (i.e. runid duplicates), we have to look closely at how the replication pipeline evaluates do files that are called via `do` from a parent script. 

There are three interlocking bugs in the pipeline that collectively caused this file to be executed twice and its outputs mishandled:

1. **`clear all` resets the injection counter:**
Inside `Overview_Aufbereitet.do` there is a `clear all` command. The function `inject.do` intends to replace `clear all` with `clear` because `clear all` wipes out all Stata globals – including `$repbox_cmd_count`, which `repbox` uses to track execution. However, the condition checking for `clear all` (`tab$arg_str == "all"`) strictly fails if there is any trailing whitespace or comments. If `clear all` is executed, the tracking counter is wiped and starts over from 1.

2. **Case-Sensitive file matching on Windows:**
Stata paths and do file executions on Windows are case-insensitive. But R's `grepl` matching inside `add.includes.to.do.df` is case-sensitive by default. If the file on disk was capitalized slightly differently than in the script (or vice versa), the `is.included` flag evaluates to `FALSE`. This forces the pipeline to assume the do file was *not* run by `run_project.do`, and so it runs it individually a second time!

3. **`extract.stata.logs` ignoring included logs:**
When a do-file is successfully included, its log is written to `include_X.log`. However, `extract.stata.logs` and `rsr_extract_stata_reg_output` strictly look for `log_*.log` and completely ignore `include_*.log`. If the included do-file logs are ignored, the pipeline marks its execution as having no output, which can sometimes trigger the `rerun.failed.included.do` fallback (running it again!). 

I will apply four modifications to correct these bugs:
- Make `clear all` replacement robust against whitespaces and comments.
- Switch `grepl` and `%in%` to be case-insensitive for file path matching.
- Adjust log extraction to include both `log_*.log` and `include_*.log`.

!MODIFICATION inject.do in repboxStata/R/inject.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/inject.R"
function_name = "inject.do"
description = "Improve `clear all` detection to handle comments and whitespace, and include `include_*.log` in log extraction."
---
```r
inject.do = function(do, reg.cmds = get.regcmds(), save.changed.data=1, opts=rbs.opts()) {
  restore.point("inject.do")

  project_dir=do$project_dir
  id = tools::file_path_sans_ext(basename(do$file))
  repbox.dir = file.path(project_dir,"repbox/stata")

  tab = do$tab[[1]]
  tab$org_cmd = ""


  do$tab[[1]]$add.path.correction = tab$cmd %in% c("use","u","us","saveold", "save","sa","sav", "import","export","mkdir","erase","rm","guse","gsave","gzuse","gzsave") |
    !is.na(tab$using) |
    !is.na(tab$saving) |
    (tab$cmd %in% c("graph","gr","gra") & tab$cmd2 %in% c("export","save")) |
    (tab$cmd %in% c("estimates","est","estim","estimate") & tab$cmd2 %in% c("save","use")) |
    (tab$cmd %in% "adopath" & tab$cmd2 %in% c("+")) |
    (tab$cmd %in% c("putexcel") & tab$cmd2 %in% c("set")) |
    (tab$cmd == "cd" & trimws(tab$txt)!="cd")

  ph = do$ph[[1]]
  tab = do$tab[[1]]

  tab$run.max = NA_integer_
  if (!is.null(opts$loop.log.cmd.max)) {
    rows = tab$in.program == 1 | tab$in_loop == 1
    tab$run.max[rows] = opts$loop.log.cmd.max
  }
  do$tab[[1]] = tab


  tab$commented.out = FALSE
  tab$add.capture=FALSE

  org.txt = txt = replace.ph.keep.lines(tab$txt,ph)

  new.txt = txt

  block.rows = tab$opens_block & (!(is.na(tab$quietly)) | !is.na(tab$capture))
  if (sum(block.rows) >0) {
    cmds = tab$cmd[block.rows]
    rows = !is.na(tab$capture[block.rows])
    cmds[rows] = trimws(tab$capture[block.rows][rows])

    rows = !is.na(tab$quietly[block.rows])
    cmds[rows] = trimws(tab$quietly[block.rows][rows])

    new.txt[block.rows] = stringi::stri_replace_first(new.txt[block.rows],fixed=cmds, replacement = paste0(cmds, " noisily"))
  }

  rows = startsWith(trimws(new.txt),"quietly:") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly:") %>% trimws()

  rows = startsWith(trimws(new.txt),"quietly ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "quietly ") %>% trimws()
  rows = startsWith(trimws(new.txt),"qui ") & !block.rows
  new.txt[rows] = str.right.of(new.txt[rows], "qui ") %>% trimws()

  rows = which(tab$cmd == "table")
  if (length(rows)>0) {
    is.pre.table = is.pre.Stata17.table.command(tab$txt[rows])
    if (is.pre.table) {
      new.txt[rows] = paste0("version 16: ", new.txt[rows])
    }
  }

  lines = which(tab$add.path.correction)
  new.txt[lines] = inject.path.correction.change.cmd(new.txt[lines], lines, do=do)

  lines = which(!(
    is.true(tab$opens_block) | tab$in.program >= 2 |
      tab$cmd %in% c("}","foreach","forvalues","forval", "if","else","end", "while")
  ))

  new.txt[lines] = paste0("capture:  noisily: ",  new.txt[lines])
  tab$add.capture[lines] = TRUE

  do$tab[[1]] = tab

  no.study.lines = which( (trimws(tab$cmd) %in% c("}","end","if","else")) | tab$in.program >= 2 | endsWith(trimws(tab$txt),"}"))

  no.study.lines = union(no.study.lines,
    which(
      (tab$opens_block & tab$in.program == 1) |
      (tab$opens_block & lag(tab$in_loop %in% c(1,2)))
    )
  )

  if (!opts$report.inside.program) {
    no.study.lines = union(no.study.lines, which(tab$in.program == 1))
  }

  special.lines = NULL

  if (do$does.include & do$use.includes) {
    incl.df = do$incl.df[[1]]
    incl.df = adapt.incl.df.for.stata.vars(incl.df,do$project_dir)

    incl.do.df = filter(incl.df, cmd=="do" | cmd == "run")

    lines = incl.do.df$line

    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      incl.do.df$find.file.code,
      '\ncapture: noisily: do "',incl.do.df$repbox.file,'", nostop',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )

    incl.do.df = filter(incl.df, cmd=="include")
    lines = incl.do.df$line
    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      '\ninclude "',incl.do.df$repbox.file,'"',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )
  }

  before.inject.txt = new.txt

  # Improved `clear all` detection
  lines = which(tab$cmd == "clear" & startsWith(trimws(tab$arg_str), "all"))
  if (length(lines)>0) {
    cat(paste0("\nReplace ", length(lines)," 'clear all' command in ", do$dofile," with 'clear' to prevent loss of repbox global variables.\n"))
    # Make sure we keep any trailing comments by replacing only the "all" part
    new.txt[lines] = stringi::stri_replace_first_regex(new.txt[lines], "\\ball\\b", "")
  }


  lines = setdiff(
    which(tab$cmd %in% c("save", "sa", "sav", "saveold", "gsave", "gzsave","erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(
    which(tab$cmd %in% c("erase","rm")),
    no.study.lines
  )
  new.txt[lines] = paste0(
    inject.intermediate.data.pre(lines, do, opts),
    new.txt[lines]
  )

  lines = setdiff(which(tab$cmd %in% c("use","u","us", "save","sa", "sav", "saveold", "clear","import","guse","gsave","gzuse","gzsave","rm","erase")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.use.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # CACHE INJECTIONS
  cache_cmds = repbox_always_cache_cmd()
  lines = setdiff(which(tab$cmd %in% cache_cmds), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.cache_always(txt[lines], lines, do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)


  lines = setdiff(which(tab$cmd %in% c("preserve","restore")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.preserve.restore(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(tab$cmd %in% c("esttab") & !is.na(tab$using)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.esttab.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  lines = setdiff(which(tab$in_loop ==2), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.loop(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  gcmds = get.graphcmds()
  ngcmds = get.nographcmds()
  lines = setdiff(which(tab$cmd %in% gcmds & !(tab$cmd %in% ngcmds$cmd & tab$cmd2 %in% ngcmds)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.graph.save(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  if (opts$report.inside.program) {
    lines = which(tab$cmd == "program")
    new.txt[lines] = paste0(new.txt[lines],'\ndisplay "!.REPBOX.CUSTOM.PROGRAM>*"')
  }

  if (opts$extract.reg.info) {
    if (!require(repboxStataReg)) {
      cat("\nInjection of specific regression information is planned for a new package repboxReg. That package does not yet exist.\n")
      opts$extract.reg.info = FALSE
    }
  }

  if (opts$extract.reg.info) {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  } else {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg.simple(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  quasi_cmds = stata_cmds_quasireg()
  lines = quasi.rows = setdiff(which(tab$cmd %in% quasi_cmds), c(no.study.lines, special.lines))
  if (length(lines) > 0) {
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg.simple(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }


  if (isTRUE(opts$extract.scalar.vals)) {
    lines = reg.rows = setdiff(which(tab$cmd %in% "scalar"), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.scalar(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }

  lines = which(startsWith(new.txt, "set maxvar"))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("br","browse", "pause","cls","stop") | (tab$cmd == "set" & is.true(startsWith(tab$cmd2,"trace"))))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  lines = which(tab$cmd %in% c("log","translate") )
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  if (isTRUE(opts$comment.out.install)) {
    lines = which(has.substr(new.txt, "ssc ") & has.substr(new.txt, " install "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

    lines = which(has.substr(new.txt, "sysdir ") & has.substr(new.txt, " set "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

  }

  if (!do$use.includes) {
    lines = which(tab$cmd %in% c("do","include","run"))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE
  }

  lines = setdiff(seq_len(NROW(tab)), c(special.lines, no.study.lines))
  inj.txt = injection.other(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  lines = setdiff(which(!tab$commented.out), no.study.lines)
  inj.txt = pre.injection(txt[lines],lines,do)
  new.txt[lines] = paste0(inj.txt,new.txt[lines])

  lines = setdiff(which(!is.na(tab$run.max)), no.study.lines)
  new.txt[lines] = inject.loop.max.run(new.txt[lines], before.inject.txt[lines], lines, do)

  tab$new.txt = new.txt

  org.file = do$file
  do.dir = dirname(org.file)
  org.base = basename(org.file)
  new.base = paste0("repbox_", org.base)
  new.file = file.path(do.dir, new.base)

  log.file = normalizePath(file.path(repbox.dir,"logs", paste0("log_", do$donum,".log")), mustWork=FALSE,winslash = "/")

  incl.log.file = normalizePath(file.path(repbox.dir,"logs", paste0("include_", do$donum,".log")), mustWork=FALSE, winslash = "/")

  log.name = paste0("repbox_log_", do$donum)

  start.timer.file = paste0(project_dir,"/repbox/stata/timer/start.txt")
  end.timer.file = paste0(project_dir,"/repbox/stata/timer/end.txt")

  txt = c(paste0('
file open repbox_timer_file using "', start.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file

if "$repbox_cmd_count" == "" {
  set_defaults _all
  set more off
  global repbox_cmd_count = 0
  global repbox_root_donum = ', do$donum,'
  log using \"',log.file,'\", replace name(',log.name,')
  ', adopath.injection.code(project_dir),'
}
else {
  log using \"',incl.log.file,'\", replace name(',log.name,')
}
'),
          new.txt,
          paste0('
display "#~# FINISHED DO",
capture log close ', log.name,'

file open repbox_timer_file using "', end.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file
'
          ))

  writeLines(txt, new.file)
  return(list(do=do,txt=invisible(txt)))
}
```
!END_MODIFICATION inject.do in repboxStata/R/inject.R

!MODIFICATION add.includes.to.do.df in repboxStata/R/do_includes.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/do_includes.R"
function_name = "add.includes.to.do.df"
description = "Make file inclusion detection case-insensitive since Stata paths on Windows are case-insensitive."
---
```r
add.includes.to.do.df = function(do.df) {
  restore.point("add.includes.to.do.df")

  i = 2
  incl.li = lapply(seq_len(NROW(do.df)), function(i) {
    do = do.df[i,]
    tab = do$tab[[1]]
    incl.df = tab %>%
      ungroup() %>%
      filter(cmd %in% c("do","run","include")) %>%
      select(cmd, shortfile = arg_str, line, orgline) %>%
      mutate(parent.do = rep(do$dofile,n())) %>%
      mutate(shortfile = replace.placeholders(shortfile, do$ph[[1]]))

    if (NROW(incl.df)==0) return(NULL)
    files = incl.df$shortfile
    files = gsub("\\","/", files, fixed=TRUE)
    files = gsub('"','',files,fixed=TRUE) %>% trimws()
    ext = tools::file_ext(files)
    rows = ext==""
    files[rows] = paste0(files[rows],".do")

    incl.df$dofile = basename(files)
    incl.df$doid = ifelse(tools::file_ext(files)=="do", tools::file_path_sans_ext(incl.df$do.file),NA)

    dd.row = match(incl.df$dofile, do.df$dofile)
    long.file = do.df$file[dd.row]
    incl.df$do.dir = dirname(long.file)
    incl.df$repbox.file = paste0(incl.df$do.dir,"/repbox_",incl.df$dofile)

    incl.df$has.stata.var = has.substr(incl.df$shortfile,"`") | has.substr(incl.df$shortfile,"$")

    incl.df
  })

  do.df$incl.df = incl.li
  incl.df = bind_rows(incl.li)
  if (NROW(incl.df)==0) incl.df = NULL

  if (any(incl.df$has.stata.var)) {
    # Deal with rows that have a stata variable in the file
    rows = which(incl.df$has.stata.var)

    row = rows[1]
    mfiles = lapply(rows, function(row) {
      dofile = incl.df$dofile[row]
      file.pattern = glob2rx(replace.stata.var.in.string(dofile))
      matches = grepl(file.pattern, do.df$dofile, ignore.case = TRUE)
      do.df$dofile[matches]
    })

    all.files = c(incl.df$dofile[!incl.df$has.stata.var], unlist(mfiles))
    do.df$is.included = suppressWarnings(tolower(do.df$dofile) %in% tolower(all.files))
  } else {
    do.df$is.included = suppressWarnings(tolower(do.df$dofile) %in% tolower(incl.df$dofile))
  }

  do.df$does.include = !sapply(incl.li, is.null)


  do.df
}
```
!END_MODIFICATION add.includes.to.do.df in repboxStata/R/do_includes.R

!MODIFICATION extract.stata.logs in repboxStata/R/extract.R
scope = "function"
file = "/home/rstudio/repbox/repboxStata/R/extract.R"
function_name = "extract.stata.logs"
description = "Include `include_*.log` files in the log extraction."
---
```r
extract.stata.logs = function(project_dir) {
  restore.point("extract.stata.logs")
  dir = file.path(project_dir, "repbox/stata/logs")
  files = list.files(dir, pattern = "^(log|include)_.*\\.log$", full.names = TRUE)

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
```
!END_MODIFICATION extract.stata.logs in repboxStata/R/extract.R

!MODIFICATION rsr_extract_stata_reg_output in repboxStataReg/R/extract_reg_stata.R
scope = "function"
file = "/home/rstudio/repbox/repboxStataReg/R/extract_reg_stata.R"
function_name = "rsr_extract_stata_reg_output"
description = "Include `include_*.log` files in the log extraction for regression information."
---
```r
rsr_extract_stata_reg_output = function(project_dir, run.df=NULL, dotab=NULL, save=TRUE) {
  restore.point("rsr_extract_stata_reg_output")

  #if (is.null(runid_map)) {
  #  runid_map = readRDS(file.path(project_dir, "repbox/stata/runid_repbox_map.Rds"))
  #}

  if (is.null(run.df) | is.null(dotab)) {
    repbox_results = readRDS(file.path(project_dir, "repbox/stata/repbox_results.Rds"))
    run.df = repbox_results$run.df
    dotab = repbox_results$dotab
  }

  artid = basename(project_dir)
  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 1. Extract TSV information stored by esttab
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  res.dir = file.path(project_dir,"repbox/stata/tsv")
  files = list.files(res.dir,glob2rx(paste0("*.dta")),full.names = TRUE)

  bfiles = basename(files)
  donum = str.left.of(bfiles, "_") %>% as_integer()
  str = str.right.of(bfiles,"_")
  line = str.left.of(str, "_") %>% as_integer()
  str = str.right.of(str,"_")
  counter = str.remove.ends(str, right=4) %>% as_integer()

  regtab = tibble(regresfile=files,donum=donum,line=line,counter=counter) %>%
    arrange(donum, line, counter) %>%
    group_by(donum, line) %>%
    mutate(run = seq_len(n())) %>%
    ungroup()

  regtab$ct = lapply(regtab$regresfile, function(file) {
    restore.point("inner.read.regres")
    regres = haven::read_dta(file)
    old.cols = c("eq","parm","label","estimate","stderr","dof", "z","p","min95","max95")
    new.cols = c("eq","var","label", "coef","se","dof", "t","p","ci_low","ci_up")
    regres = rename.cols(regres, old.cols, new.cols)
    regres = regres[,intersect(new.cols, colnames(regres)), drop=FALSE]
    if (!"eq" %in% colnames(regres)) {
      regres$eq = rep("", NROW(regres))
    }
    regres
  })
  regtab = select(regtab, -regresfile)



  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 2. Extract regression information stored in logs
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  dir = file.path(project_dir, "repbox/stata/logs")
  log.files = list.files(dir, pattern = "^(log|include)_.*\\.log$", full.names = TRUE)

  reg.log = lapply(log.files, function(file) {
    log.txt = readLines(file,warn=FALSE)  %>% enc2utf8()
    bdf = extract.inject.blocks(log.txt, type="REG_ERETURN")
    bdf$er = lapply(bdf$str,parse.ereturn.injection)
    bdf
  }) %>% bind_rows()

  regtab = left_join(regtab, select(reg.log, donum, line, counter, er), by=c("donum","line","counter"))

  #++++++++++++++++++++++++++++++++++++++++++++++++++
  # 3. Merge with run.df
  #++++++++++++++++++++++++++++++++++++++++++++++++++

  regtab = left_join(regtab,run.df, by=c("donum","line","counter"))

  # UPDATE: Only consider regression where missing_data = FALSE
  # Otherwise we likely have faulty regressions that use an earlier data set
  # Also ignore regression results with run error
  regtab = regtab[regtab$has.data & !regtab$runerr,]


  regtab$artid = artid

  # merge with dotab to get doid
  regtab = regtab %>% left_join(dotab %>% select(donum, doid), by="donum")

  #colnames(regtab)
  cols = c("artid", "runid", "donum", "doid", "line", "counter","cmd", "cmdline","ct","er", "datasig", "timevar" ,"panelvar", "tdelta",        "runerr",        "runerrcode",    "runerrmsg",     "runsec", "orgline", "in.program", "has.data")
  regtab = regtab[,cols]


  if (save) {
    saveRDS(regtab, file.path(project_dir,"repbox/stata/regtab.Rds"))
  }

  regtab
}
```
!END_MODIFICATION rsr_extract_stata_reg_output in repboxStataReg/R/extract_reg_stata.R
