
stata.repbox.data.use.info = function(run.df, dotab) {
  restore.point("stata.repbox.data.use.info")

  # 1. Analyse run.df
  save.cmds = c("save","export","saveold","sav","sa","gsave","gzsave")
  load.cmds = c("use","u","us", "merge","import", "insheet","joinby","guse","gzuse")

  save.runs = run.df %>%
    filter(cmd %in% save.cmds) %>%
    mutate(
      base=basename(foundfile),
      parse_run = "run"
    )

  load.runs = run.df %>%
    filter(cmd %in% load.cmds) %>%
    mutate(
      base=basename(foundfile),
      parse_run = "run"
    )

  # 2. Analysis of parsed code
  mn = bind_rows(dotab$make.need.df)
  if (NCOL(mn)==0) {
    empty = data.frame(base = character(0), donum=integer(0), doid=character(0), from.parse=logical(0), from.run=logical(0), runs.err = integer(0), runs.noerr = integer(0))

    return(list(do_data_load = empty, do_data_save=empty))
  }
  mn$donum = match(mn$doid, dotab$doid)

  mn = mn %>%
    filter(!has.substr(dta.base, "`"),
           !has.substr(dta.base, "$")) %>%
    mutate(parse_run="parse", runerr=NA) %>%
    rename(base = dta.base)


  do_data_load = bind_rows(load.runs, mn %>% filter(need)) %>%
    group_by(base, donum) %>%
    summarize(
      doid = dotab$doid[donum],
      from.parse = any(parse_run=="parse"),
      from.run = any(parse_run=="run"),
      runs.err = sum(is.true(runerr)),
      runs.noerr = sum(is.true(!runerr))
    ) %>%
    ungroup()

  do_data_save = bind_rows(save.runs, mn %>% filter(make)) %>%
    group_by(base, donum) %>%
    summarize(
      doid = dotab$doid[donum],
      from.parse = any(parse_run=="parse"),
      from.run = any(parse_run=="run"),
      runs.err = sum(is.true(runerr)),
      runs.noerr = sum(is.true(!runerr))
    ) %>%
    ungroup()

  # Remove ' from base
  do_data_load$base = gsub("'","", do_data_load$base, fixed=TRUE)
  do_data_save$base = gsub("'","", do_data_save$base, fixed=TRUE)


  list(do_data_load = do_data_load, do_data_save=do_data_save)
}

# Extract information about used dta files just from the parsed code
#
# This will provide a first guess in which order the files should be run
static.do.use.dta.info = function(do, ph.df=do$ph[[1]], tab=do$tab[[1]], project.dir=do$project.dir, project=do$project[[1]], sup.dir = file.path(project.dir,"mod")) {
  # TO DO: PRESERVE / RESTORE ; SAVE AND USE OF SAME FILE IN A DO FILE

  restore.point("static.do.use.dta.info")
  dir = sup.dir
  rows = which(tab$cmd %in% c("use","u","us", "save", "sav", "sa","saveold", "import","guse","gsave","gzuse","gzsave") | !is.na(tab$using))
  if (length(rows)==0) return(
    list(found.dta.tab=NULL, need.dta.tab= NULL, dtacmd.df = NULL, make.need.df=NULL)
  )
  #tab$line = seq_len(NROW(tab))
  utab = tab[rows,]
  utab$arg_str = replace.ph.keep.lines(utab$arg_str,ph.df) %>% trimws()
  # import commands have structure "import excel" etc.
  rows = utab$cmd == "import" & is.na(utab$using)
  utab$arg_str[rows] = str.right.of(utab$arg_str[rows]," ") %>% trimws()

  utab$using = ifelse(!is.na(utab$using),replace.ph.keep.lines(utab$using,ph.df),NA)

  f = function(x) {
    #inds = startsWith(x,'"')
    #x[inds] = str.remove.ends(x[inds],1,1)
    x = gsub('"','',x,fixed=TRUE)
    x = gsub("\\","/",x,fixed=TRUE)
    x = trimws(x)
    x
  }

  utab$default.ext = get.stata.default.file.extension(utab)
  dtacmd.df = utab %>% mutate(
    dta.in.do = f(ifelse(!is.na(using), using, arg_str)),
    dta.base = basename(dta.in.do),
    org.ext = tools::file_ext(dta.base),
    ext = ifelse(org.ext=="", default.ext, org.ext),
  )  %>%
    select(cmd, line, dta.in.do, dta.base, ext, org.ext, default.ext) %>%
    mutate(dta.base = ifelse(ext=="", dta.base, paste0(tools::file_path_sans_ext(dta.base),".",ext)))



  # find existing dta files in supplement
  dta.files = list.project.data.files(dir)

  dta.bases = basename(dta.files)
  if (any(duplicated(dta.bases))) {
    dupl = dta.bases[duplicated(dta.bases)]
    rows = dta.bases %in% dupl
    cat(paste0("\nWarning: There are duplicated data files with the same name in ", dir,". The repbox code may not handle this correctly. Please ensure that each dta file only appears once in your supplement. The following files are duplicated:\n\n", paste0(dta.files[rows], collapse="\n")))
  }

  dtacmd.df$dta.file = dta.files[match(dtacmd.df$dta.base, dta.bases)]
  dtacmd.df$found.dta = !is.na(dtacmd.df$dta.file)

  make.need.df = dtacmd.df %>%
    filter(cmd %in% c("use", "u","merge","import","insheet", "save","sav","sa","saveold", "joinby","guse","gsave","gzuse","gzsave")) %>%
    mutate(make = cmd %in% c("save","sav","sa","saveold","gsave","gzsave"), need = !make) %>%
    group_by(make,need,dta.base) %>%
    summarize(
      project=project,
      doid = do$doid,
      found.dta = any(found.dta),
      num_files = n_distinct(dta.file),
      dta.file = first(dta.file)
    ) %>%
    ungroup()
  makes.dta = dtacmd.df$dta.base[dtacmd.df$cmd %in% c("save","sav","sa","saveold","gsave","gzsave")]
  needs.dta = dtacmd.df$dta.base[dtacmd.df$cmd %in% c("use","u", "merge","import","insheet", "joinby","guse","gzuse")]

  # Specify for every row of tab which data set is needed
  irows = which(dtacmd.df$cmd %in% c("use","u", "merge","import","insheet", "joinby","guse","gzuse"))
  loads.data = length(irows)>0
  if (loads.data) {
    max.row = NROW(tab)

    need.li = vector("list", length(irows))
    for (i in rev(seq_along(irows))) {
      ir = irows[i]
      tr = dtacmd.df$line[ir]
      rows = tr:max.row
      need.li[[i]] = as.data.frame(list(line=rows, need.dta=dtacmd.df$dta.base[ir]))
      if (dtacmd.df$cmd[ir] %in% c("use", "u","us"))
        max.row = tr-1
    }
    need.dta.tab = bind_rows(need.li) %>%
      left_join(select(dtacmd.df, dta.base, found.dta) %>% unique, by=c("need.dta"="dta.base"))
    found.dta.tab = need.dta.tab %>%
      group_by(line) %>%
      summarize(
        found.all.dta = all(found.dta)
      ) %>%
      arrange(line)

  } else {
    need.dta.tab = found.dta.tab = NULL
  }


  list(found.dta.tab=found.dta.tab, need.dta.tab= need.dta.tab, dtacmd.df = dtacmd.df, make.need.df=make.need.df, loads.data=loads.data)
}

list.project.data.files = function(sup.dir, full.names=TRUE) {
  data.ext = c("dta","xls","xlsx","csv","tab","sas7bdat","sav","dbf")
  regexp = paste0("(^.*\\.",data.ext,"$)",collapse = "|")
  data.files = list.files(sup.dir, regexp,full.names = full.names, recursive = TRUE)
  data.files
}

# TO DO: ADD MERGE
map.dataset.to.code.line = function(fph, lines, include.save=FALSE) {
  restore.point("map.dataset.to.code.line")
  mph = fph[fph$cmd %in% c("merge","joinby"),]
  if (!include.save) {
    uph = fph[fph$cmd %in% c("use","u","us"),]
  }  else {
    uph = fph[fph$cmd  %in% c("use","u","us") | fph$cmd %in% c("save","sav","sa","saveold"),]
  }
  uph$.row = seq_len(NROW(uph))
  # No merge commands, i.e. only a single dataset is used
  if (NROW(mph)>0) {
    warning("map.dataset.to.code.line is not yet implemented for code with merge commands. Currently merge commands are just ignored.")
  }
  ublock = findInterval(lines, uph$line)
  map.df = tibble(.row=ublock, line=lines) %>%
    left_join(select(uph,.row,dtafile=file, dtafound=found, dtaline=line), by=".row") %>%
    select(-.row)
  return(map.df)

}

