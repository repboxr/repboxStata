# This code deals with do files that run or include other do files

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
    # Example: The main.do has a command
    # do "../Code/Stata Do Files/Data Analysis/Section`section'.do"
    #
    # We will suspect all files that match
    # Section*.do
    # to be included.
    #
    # Even if some of them are not included, they
    # will be run later again because they have no log file
    rows = which(incl.df$has.stata.var)

    row = rows[1]
    mfiles = lapply(rows, function(row) {
      dofile = incl.df$dofile[row]
      file.pattern = glob2rx(replace.stata.var.in.string(dofile))
      matches = grepl(file.pattern, do.df$dofile)
      do.df$dofile[matches]
    })

    all.files = c(incl.df$dofile[!incl.df$has.stata.var], unlist(mfiles))
    do.df$is.included = suppressWarnings(do.df$dofile %in% all.files)
  } else {
    do.df$is.included = suppressWarnings(do.df$dofile %in% incl.df$dofile)
  }

  do.df$does.include = !sapply(incl.li, is.null)


  do.df
}


replace.stata.var.in.string = function(txt, replace="*") {
  restore.point("replace.stata.var.in.string")
  # replace local stata var
  txt = stringi::stri_replace_all_regex(txt,"`[A-Za-z][A-Za-z0-9_]*'",replace)
  # replace global stata var
  txt = stringi::stri_replace_all_regex(txt,"\\$\\{[A-Za-z][A-Za-z0-9_]*\\}",replace)
  txt = stringi::stri_replace_all_regex(txt,"\\$[A-Za-z][A-Za-z0-9_]*",replace)
  txt
}

# An old function that removes all includes output from log
# Currently, we keep the output from includes but don't store a
# log file if the do file is included
remove.include.output.from.log = function(log.txt) {
  restore.point("remove.include.output.from.log")

  while(TRUE) {
    blocks = find.line.starts.blocks(log.txt, "#~# START INCLUDE INJECTION ","#~# END INCLUDE INJECTION ")
    if (NROW(blocks)==0) return(log.txt)

    start = blocks$start.line[1]
    end = blocks$end.line[1]
    if (is.na(end)) {
      warning("INCLUDE INJECTION blocks were not correctly closed in log files. Not yet clear what the reason for the problem is. We will ignore all further log lines")
      # Try out setting NA block endings to max line
      # in order to ignore all rest of code
      end = NROW(log.txt)
    }
    del = setdiff((start-1):end, c(start+1, start+2, start+3))
    log.txt[min(end,start+3)] = "\nOutput of do and include commands is not shown.\nLook in the corresponding do file that is run."
    log.txt = log.txt[-del]
    log.txt
  }
  log.txt
}

# Adapt incl.df such that we can also deal with
# do commands that use local or global Stata variables
adapt.incl.df.for.stata.vars = function(incl.df, project_dir) {
  restore.point("adapt.incl.df.for.stata.vars")
  incl.df$find.file.code = ""

  rows = which(has.substr(incl.df$shortfile,"`") | has.substr(incl.df$shortfile,"$"))
  if (length(rows)==0) return(incl.df)

  # Add dynamic path correction

  sup.dir = file.path(project_dir,"mod")
  r.script = file.path(project_dir, "repbox/stata/find_files.R")

  file_str = incl.df$shortfile[rows]
  file_str = gsub('"','', file_str, fixed = TRUE)

  cmd = incl.df$cmd[rows]
  default_ext = "do"
  code = paste0(
    '
local repbox_source_path = subinstr("',file_str,'","\\","/",.)
rcall vanilla: source("', r.script,'"); repbox_corrected_path = find.repbox.do.file("`repbox_source_path\'", "',sup.dir,'", "', cmd,'", "', default_ext,'", "`c(pwd)\'")')

  txt = paste0(code, '
display "`r(repbox_corrected_path)\'"', "\n")
  incl.df$find.file.code[rows] = txt
  incl.df$repbox.file[rows] = "`r(repbox_corrected_path)\'"
  return(incl.df)
}

