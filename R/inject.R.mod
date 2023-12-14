#
# This file contains functions to inject code into
# Stata codes that will allow to later extract all relevant information
#

example = function() {
  txt = "gen x=z^2"
  cat(pre.injection(txt))
}


inject.do = function(do, reg.cmds = get.regcmds(), save.changed.data=1, opts=rbs.opts()) {
  restore.point("inject.do")
  #if (do$doid=="config") stop()

  project.dir=do$project.dir
  id = tools::file_path_sans_ext(basename(do$file))
  repbox.dir = file.path(project.dir,"repbox/stata")

  tab = do$tab[[1]]
  tab$org_cmd = ""


  do$tab[[1]]$add.path.correction = tab$cmd %in% c("use","u","us","saveold", "save","sa","sav", "import","export","mkdir","erase","rm") |
    !is.na(tab$using) |
    !is.na(tab$saving) |
    (tab$cmd %in% c("graph","gr","gra") & tab$cmd2 %in% c("export","save")) |
    (tab$cmd %in% "adopath" & tab$cmd2 %in% c("+")) |
    (tab$cmd %in% c("putexcel") & tab$cmd2 %in% c("set")) |
    (tab$cmd == "cd" & trimws(tab$txt)!="cd")

  ph = do$ph[[1]]
  tab = do$tab[[1]]

  # Specify lines where we check that log output is written
  # a limited number of times
  tab$run.max = NA_integer_
  if (!is.null(opts$loop.log.cmd.max)) {
    rows = tab$in.program == 1 | tab$in_loop == 1
    tab$run.max[rows] = opts$loop.log.cmd.max
  }
  do$tab[[1]] = tab


  tab$commented.out = FALSE
  tab$add.capture=FALSE

  org.txt = txt = replace.ph.keep.lines(tab$txt,ph)

  # Old: static path correction
  #res = correct.do.paths(do = do,txt=txt,dir=project.dir)
  #new.txt = txt = res$txt; fph=res$ph

  new.txt = txt

  # Add noisily to quitely { or capture { blocks

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


  # Write version 16: before old table commands
  rows = which(tab$cmd == "table")
  if (length(rows)>0) {
    is.pre.table = is.pre.Stata17.table.command(tab$txt[rows])
    if (is.pre.table) {
      new.txt[rows] = paste0("version 16: ", new.txt[rows])
    }
  }


  # New: dynamic path correction
  # This replaces a file reference to a placeholder, e.g.
  # use mydata, clear -> use "`r(repbox_corrected_path)'", clear
  # Later we also add PRECMD code that computes r(repbox_corrected_path)
  lines = which(tab$add.path.correction)
  new.txt[lines] = inject.path.correction.change.cmd(new.txt[lines], lines, do=do)

  # Add capture noisily before all commands
  # otherwise one error inside a loop will end the complete loop
  # even if we run the script with nostop
  lines = which(!(
    is.true(tab$opens_block) | tab$in.program >= 2 |
    tab$cmd %in% c("}","foreach","forvalues","forval", "if","else","end", "while")
  ))

  new.txt[lines] = paste0("capture:  noisily: ",  new.txt[lines])
  tab$add.capture[lines] = TRUE

  do$tab[[1]] = tab

  no.study.lines = which( (trimws(tab$cmd) %in% c("}","end","if","else")) | tab$in.program >= 2 | endsWith(trimws(tab$txt),"}"))

  if (!opts$report.inside.program) {
    no.study.lines = union(no.study.lines, which(tab$in.program == 1))
  }

  special.lines = NULL

  # Update includes
  if (do$does.include & do$use.includes) {
    incl.df = do$incl.df[[1]]

    # Adapt incl.df such that we can also deal with
    # do commands that use local or global Stata variables
    incl.df = adapt.incl.df.for.stata.vars(incl.df,do$project.dir)

    # Update do commands
    incl.do.df = filter(incl.df, cmd=="do" | cmd == "run")

    lines = incl.do.df$line

    # A do command outputs all results from the do file
    # we want to ignore it to avoid double matches.

    # Note that we replace run commands with do commands
    # That is because inside a run command, our
    # output in the log files will not have the desired format
    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      incl.do.df$find.file.code,
      '\ncapture: noisily: do "',incl.do.df$repbox.file,'", nostop',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )

    # Update include commands
    incl.do.df = filter(incl.df, cmd=="include")
    lines = incl.do.df$line
    new.txt[lines] = paste0(
      '\ndisplay "#~# START INCLUDE INJECTION ',do$donum,"_", lines,
      '\ninclude "',incl.do.df$repbox.file,'"',
      '\ndisplay "#~# END INCLUDE INJECTION ',do$donum,"_", lines
    )

  }

  before.inject.txt = new.txt


  # Add injection after use, save, clear commands
  lines = setdiff(which(tab$cmd %in% c("use","u","us", "save","sa", "sav", "saveold", "clear","import")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.use.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # Add injection after preserve, restore
  lines = setdiff(which(tab$cmd %in% c("preserve","restore")), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.preserve.restore(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  # Add injection after esttab commands and other commands
  # that create tex tables
  lines = setdiff(which(tab$cmd %in% c("esttab") & !is.na(tab$using)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.esttab.etc(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # For loops we will add no end.injection
  lines = setdiff(which(tab$in_loop ==2), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.loop(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )


  # Add injection after known graphic commands
  # Save graphic as svg in the output folder
  gcmds = get.graphcmds()
  ngcmds = get.nographcmds()
  lines = setdiff(which(tab$cmd %in% gcmds & !(tab$cmd %in% ngcmds$cmd & tab$cmd2 %in% ngcmds)), no.study.lines)
  special.lines = c(special.lines, lines)
  inj.txt = injection.graph.save(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt )

  # Add injection after regression commands
  # injection code is in repboxReg
  if (opts$extract.reg.info) {
    if (!require(repboxReg)) {
      cat("\nInjection of specific regression information is planned for a new package repboxReg. That package does not yet exist.\n")
      opts$extract.reg.info = FALSE
    }
  }


  if (opts$extract.reg.info) {
    lines = reg.rows = setdiff(which(tab$cmd %in% reg.cmds), no.study.lines)
    special.lines = c(special.lines, lines)
    inj.txt = injection.reg(txt[lines],lines,do)
    new.txt[lines] = paste0(new.txt[lines], inj.txt)
  }


  # Comment out certain commands

  # Cannot set maxvar in all Stata versions
  lines = which(startsWith(new.txt, "set maxvar"))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  # Cannot use browse, pause, br, cls command from
  # Stata command line
  # Also dont set any tracing
  lines = which(tab$cmd %in% c("br","browse", "pause","cls") | (tab$cmd == "set" & is.true(startsWith(tab$cmd2,"trace"))))
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  # Comment out log related commands. Paths are not adapted anyways
  lines = which(tab$cmd %in% c("log","translate") )
  no.study.lines = c(no.study.lines, lines)
  new.txt[lines] = paste0("*", new.txt[lines])
  tab$commented.out[lines] = TRUE

  # Comment out ssc install commands. We will install the packages ourselves
  if (isTRUE(opts$comment.out.install)) {
    lines = which(has.substr(new.txt, "ssc ") & has.substr(new.txt, " install "))
    no.study.lines = c(no.study.lines, lines)
    new.txt[lines] = paste0("*", new.txt[lines])
    tab$commented.out[lines] = TRUE

    # Comment out sysdir set commands. We will install the packages ourselves
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

  # Inject code for other lines
  lines = setdiff(seq_len(NROW(tab)), c(special.lines, no.study.lines))
  inj.txt = injection.other(txt[lines],lines,do)
  new.txt[lines] = paste0(new.txt[lines], inj.txt)

  # Inject pre command code
  lines = setdiff(which(!tab$commented.out), no.study.lines)
  inj.txt = pre.injection(txt[lines],lines,do)
  new.txt[lines] = paste0(inj.txt,new.txt[lines])


  # Deal with maximum loop counter
  lines = setdiff(which(!is.na(tab$run.max)), no.study.lines)
  new.txt[lines] = inject.loop.max.run(new.txt[lines], before.inject.txt[lines], lines, do)


  # Save lines with injection in tab
  tab$new.txt = new.txt

  # Inject code at the beginning of do file e.g. to establish log files
  org.file = do$file
  do.dir = dirname(org.file)
  org.base = basename(org.file)
  new.base = paste0("repbox_", org.base)
  new.file = file.path(do.dir, new.base)
  #log.base = paste0(tools::file_path_sans_ext(new.base),".log")
  #log.file = file.path(do.dir, log.base)

  log.file = normalizePath(file.path(repbox.dir,"logs", paste0("log_", do$donum,".log")), mustWork=FALSE)

  incl.log.file = normalizePath(file.path(repbox.dir,"logs", paste0("include_", do$donum,".log")), mustWork=FALSE)

  # to deal with includes, we need unique lognames
  #log.name = gsub(".","_", log.base,fixed=TRUE)
  log.name = paste0("repbox_log_", do$donum)

  # This will only define repbox_cmd_count
  # and start a separate log if the do file
  # is not included in another do file

  start.timer.file = paste0(project.dir,"/repbox/stata/timer/start.txt")
  end.timer.file = paste0(project.dir,"/repbox/stata/timer/end.txt")

  txt = c(paste0('
file open repbox_timer_file using "', start.timer.file,'", write append
file write repbox_timer_file "', do$donum,';`c(current_time)\';`c(current_date)\'"
file write repbox_timer_file _n
file close repbox_timer_file

if "$repbox_cmd_count" == "" {
  set_defaults _all
  global repbox_cmd_count = 0
  global repbox_root_donum = ', do$donum,'
  log using \"',log.file,'\", replace name(',log.name,')
', adopath.injection.code(project.dir), '
}
else {
  log using \"',incl.log.file,'\", replace name(',log.name,')
}
datasignature set, reset
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

  # Write modified do file
  writeLines(txt, new.file)
  return(list(do=do,txt=invisible(txt)))
}


scalar.injection = function(lines, ...) {
  args = list(...)
  restore.point("scalar.injection")
  str = do.call(paste0,args)
  paste0('display "#~=~# ',lines,' `repbox_local_cmd_count\' ', str,'"')
}
start.injection = function(donum, lines, type, ...) {
  paste0('display "#~# INJECT ',type,' ',donum,' ', lines,' `repbox_local_cmd_count\'\n')
}
end.injection = function(donum, lines, type,...) {
  paste0('\ndisplay "#~# END INJECT ',type,' ',donum, ' ', lines,' `repbox_local_cmd_count\'\n')
}


post.injection = function(txt, lines, do, reset.datasig=FALSE, report.datasig=FALSE, report.xtset=FALSE) {
  restore.point("post.injection")
  rep.dir = file.path(do$project.dir,"repbox/stata")
  cmdfile = file.path(rep.dir,"cmd", paste0("postcmd_",do$donum,".csv"))
  tab = do$tab[[1]]
  errcode_str = ifelse(is.true(tab$add.capture[lines]),"`=_rc\'","")
  inj.txt = paste0(
'
qui {
', ifelse(reset.datasig, 'quietly datasignature set, reset',''), '
', ifelse(report.datasig, 'quietly datasignature report',''), '
file open repbox_cmd_file using "', cmdfile,'", write append
file write repbox_cmd_file `"', do$donum,';', lines,';`repbox_local_cmd_count\';$S_TIME;',errcode_str,';`r(fulldatasignature)\';','"\'',
# Add call xtset to set r(timevar) etc...
if (report.xtset) '\ncapture xtset',
'\nfile write repbox_cmd_file `"',
if (report.xtset) '`r(timevar)\';`r(panelvar)\';`r(tdelta)\'' else ';;',
'"\'','
file write repbox_cmd_file _n
file close repbox_cmd_file
',
# We add the display command to reset _rc to 0 if there was an error in xtset
if (report.xtset) 'capture display 0\n',
'\n}'
)
  # Don't inject code if a block is opened (if, foreach etc)
  inj.txt[tab$opens_block[lines]] = ""
  save.dta.code = save.dta.injection(txt, lines, do)
  inj.txt = paste0(inj.txt, save.dta.code)
  inj.txt
}

save.dta.injection = function(txt, lines, do, opts=rbs.opts()) {
  restore.point("save.dta.injection")
  store.data = opts$store.data
  if (is.null(store.data)) return(NULL)


  df = store.data %>%
    filter(donum == do$donum, line %in% lines) %>%
    group_by(line) %>%
    summarize(
      use.if = !is.na(counter),
      if.cond = paste0("`repbox_local_cmd_count' == ", counter, collapse = "|")
    )

  save.dta.files = paste0(do$project.dir, "/repbox/stata/dta/",do$donum,"_",df$line,"_`repbox_local_cmd_count'",".dta")

  code = ifelse(df$use.if,
    paste0('
if (', df$if.cond,') {
  save "',save.dta.files,'", replace
  global repbox_reg_datasig = r(fulldatasignature)
}
'),
    paste0('
save "',save.dta.files,'", replace
global repbox_reg_datasig = r(fulldatasignature)
')
  )

  all.code = rep("", length(lines))
  rows = match(df$line, lines)
  all.code[rows] = code
  all.code
}

pre.injection = function(txt, lines=seq_along(txt), do) {
  #restore.point("pre.injection")
  rep.dir = file.path(do$project.dir,"/repbox/stata")
  cmd.file = file.path(rep.dir,"cmd", paste0("precmd_",do$donum,".csv"))
  tab = do$tab[[1]]
  add.path.correction = tab$add.path.correction[lines]

  # The input command will have new lines
  # that we replace with spaces otherwise
  # we get an error in the repbox_cmdline assignment
  cmdline_txt = gsub("\n"," ", txt, fixed=TRUE)
  inj.txt = paste0('
* PRECOMAND INJECTION START LINE ', lines,'
qui {
global repbox_cmd_count = $repbox_cmd_count +1
local repbox_local_cmd_count = $repbox_cmd_count
local repbox_cmdline = `"',cmdline_txt,'"\'
')

  inj.txt[add.path.correction] = paste0(inj.txt[add.path.correction],"\n",inject.path.correction.pre(txt[add.path.correction],lines[add.path.correction],do))

  found.path = ifelse(add.path.correction,'`repbox_corrected_path\'','')
  wdir = ifelse(add.path.correction,"`c(pwd)'","")

  inj.txt = paste0(inj.txt,'
file open repbox_cmdlines using "', cmd.file,'", write append
file write repbox_cmdlines `"', do$donum,';', lines,';`repbox_local_cmd_count\';$repbox_root_donum;$S_TIME;',wdir,';',found.path,';`repbox_cmdline\'"\'
file write repbox_cmdlines _n
file close repbox_cmdlines
}
* PRECOMMAND INJECTION END
',
# Don't add RUNCMD line for beginning of loop
ifelse(tab$in_loop[lines]==2,"", start.injection(do$donum, lines, "RUNCMD", do))
  )
  inj.txt
}

inject.loop.max.run = function(txt, before.txt, lines, do) {
  restore.point("inject.loop.max.run")
  tab = do$tab[[1]]

  lcount_var = paste0('repbox_line_count_', do$donum, '_', lines)
  run_line_counter_code = ifelse(is.na(tab$run.max[lines]),"",paste0('
qui{
if ("$', lcount_var,'" == "") {
  global ',lcount_var,' = 1
}
else {
  global ',lcount_var,' = $',lcount_var, ' +1
}
}
'))

  rows = startsWith(before.txt,"capture:  noisily: ")
  before.txt[rows] = stringi::stri_replace_first_fixed(before.txt[rows],"capture:  noisily: ","capture:  ")

  inj.txt = paste0(
    run_line_counter_code, '
    if ($', lcount_var, ' <= ', tab$run.max[lines],') {\n',
txt,
    '\n}\nelse{\n',
before.txt
    ,"\n}"
  )
  inj.txt
}

# Inject dynamic path correction code
inject.path.correction.pre = function(txt, lines=seq_along(txt), do) {
  restore.point("inject.path.correction")
  #if (do$doid == "elaborations_money_partners_strangers") stop()
  project.dir = do$project.dir
  sup.dir = file.path(project.dir,"mod")

  tab = do$tab[[1]][lines,]
  default_ext = get.stata.default.file.extension(tab)

  txt
  r.script = file.path(project.dir, "repbox/stata/find_files.R")
  tab = do$tab[[1]][lines,]
  ph = do$ph[[1]]

  res = replace.files.and.paths.with.ph(tab,ph=ph)
  ph.txt = res$txt
  fph = res$ph
  if (any(duplicated(fph$line))) {
    restore.point("inject.path.correction.dupl")
    stop("Multiple file paths in a command cannot yet be dealt with.")
  }

  content = fph$content

  #content = gsub("\\","/",content, fixed=TRUE)
  content = gsub('"','', content, fixed = TRUE)

  # Don't remove ' here. This will cause problems for
  # local variables
  #content = gsub("'",'', content, fixed = TRUE)

  file_str = rep("", length(txt))
  file_str[fph$line] = content
  # For saving option we don't pass the actual command but "saving"
  cmd = ifelse(is.na(tab$saving), tab$cmd, "saving")

  # We need to replace \ with / in Stata since otherwise
  # paths with spliced-in variables that have a \
  # will not be handled correctly
  code = paste0(
'
local repbox_source_path = subinstr("',file_str,'","\\","/",.)
rcall vanilla: source("', r.script,'"); repbox_corrected_path = find.path("`repbox_source_path\'", "',sup.dir,'", "', cmd,'", "', default_ext,'", "`c(pwd)\'")')

  #new.content = '"`r(repbox_corrected_path)\'"'
  #fph$content = new.content

  # Need to also save r(repbox_corrected_path) in a local
  # stata variable if I want to access it after the main command
  # is run.
  txt = paste0(code, '
local repbox_corrected_path = "`r(repbox_corrected_path)\'"
display "`r(repbox_corrected_path)\'"', "\n"
    #replace.ph.keep.lines(ph.txt, fph)
  )
  txt
}

# This replaces the cmd e.g.
# use mydata, clear -> use "`r(repbox_corrected_path)'", clear
inject.path.correction.change.cmd = function(txt, lines=seq_along(txt), do) {
  restore.point("inject.path.correction")
  #if (do$doid == "elaborations_money_partners_strangers") stop()
  tab = do$tab[[1]][lines,]
  ph = do$ph[[1]]
  res = replace.files.and.paths.with.ph(tab,ph=ph)
  ph.txt = res$txt
  fph = res$ph
  if (any(duplicated(fph$line))) {
    restore.point("inject.path.correction.dupl")
    stop("Multiple file paths in a command cannot yet be dealt with.")
  }
  fph$content = '"`r(repbox_corrected_path)\'"'
  replace.ph.keep.lines(ph.txt, fph)
}


injection.use.etc = function(txt, lines=seq_along(txt), do, opts=rbs.opts()) {
  restore.point("injection.use.etc")
  tab = do$tab[[1]]
  paste0('
', end.injection(do$donum,lines, "RUNCMD", do),'
* ', toupper(tab$cmd[lines]),' INJECTION START
',post.injection(txt,lines,do=do,reset.datasig=TRUE, report.xtset=TRUE, report.datasig=opts$store.use.data.sig),'
global repbox_load_datasig = r(fulldatasignature)
* INJECTION END
')
}

# esttab generates a latex output file
# our goal is to copy it to the repbox
# folder
injection.esttab.etc = function(txt, lines=seq_along(txt), do) {
  restore.point("injection.esttab.etc")
  tab = do$tab[[1]]
  output.dir = file.path(do$project.dir,"repbox/stata/output")
  ext_code = 'local repbox_file_ext = substr("`repbox_corrected_path\'", strrpos("`repbox_corrected_path\'","."), strlen("`repbox_corrected_path\'"))'


  output.file = paste0(output.dir,"/", do$donum, '_', lines,"_`repbox_local_cmd_count'","`repbox_file_ext'")

  paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
* ', toupper(tab$cmd[lines]),' INJECTION START
', ext_code,'
copy "`repbox_corrected_path\'" "',output.file,'"
',post.injection(txt,lines,do=do),'
* global repbox_datasig = r(fulldatasignature)
* INJECTION END
')
}

injection.graph.save = function(txt, lines, do) {
  restore.point("injection.graph.save")
  tab = do$tab[[1]]
  output.dir = file.path(do$project.dir,"repbox/stata/output")
  output.file = paste0(output.dir,"/", do$donum,"_", lines,"_`repbox_local_cmd_count'",".svg")

  paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
* ', toupper(tab$cmd[lines]),' INJECTION START
',post.injection(txt,lines,do=do),'
capture noisily graph export "', output.file,'"
* INJECTION END
')
}


injection.preserve.restore = function(txt, lines=seq_along(txt), do) {
  restore.point("injection.preserve.restore")
  tab = do$tab[[1]]
  paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
* INJECTION START
',post.injection(txt,lines,do=do,reset.datasig=TRUE, report.xtset=TRUE),'
* INJECTION END
')

}


injection.loop = function(txt, lines, do) {
  restore.point("injection.loop")
  str = ""
}


injection.other = function(txt, lines, do, save.graphs=TRUE) {
  restore.point("injection.other")
  str = paste0('
', end.injection(do$donum, lines, "RUNCMD",do),'
',post.injection(txt,lines,do=do,reset.datasig=FALSE),'
')
}

# Check if a table command looks like in pre Stata17 format
is.pre.Stata17.table.command = function(txt) {
  opts = str.right.of(txt,",")
  is.pre = grepl("c((o)|(on)|(ont)|(ontent)|(ontents))?[ ]*\\(", txt) |
    grepl("by?[ ]*\\(", txt) |
    grepl("(cellwidth)|(csepwidth)|(stubwidth)|(scsepwidth)|(center)|(left)|(cw)|(row)|(column)|(concise)", txt)
  any(is.pre)
}

adopath.injection.code = function(project.dir) {
  ado.files = list.files(file.path(project.dir,"mod"),glob2rx("*.ado"),full.names = TRUE,recursive = TRUE)
  if (length(ado.files)==0) return("")

  ado.dir = unique(dirname(ado.files))

  paste0('adopath ++ "',ado.dir,'"', collapse="\n")


}
