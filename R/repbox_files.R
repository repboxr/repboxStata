
get.stata.default.file.extension = function(tab) {
  restore.point("get.stata.default.file.extension")
  cmd = tab$cmd
  arg_str = trimws(tab$arg_str)
  case_when(
    cmd %in% c("use","u","us","saveold", "save", "sav", "sa","merge","append","reclink", "joinby", "guse","gsave") ~ "dta",
    cmd %in% c("gsave","gzuse","gzsave","gzappend") ~ "dta.gz",
    cmd == "import" & startsWith(arg_str,"del") ~ "csv",
    cmd == "import" & startsWith(arg_str,"sas") ~ "sas7bdat",
    cmd == "import" & startsWith(arg_str,"spss") ~ "sav",
    cmd == "shp2dta" ~ "shp",
    TRUE ~ ""
  )
}



correct.do.paths = function(do, txt = tab$txt, dir=getwd()) {
  restore.point("correct.do.paths")

  tab = do$tab[[1]]
  ph = do$ph[[1]]
  res = replace.files.and.paths.with.ph(tab,ph)
  txt = res$txt
  fph = res$ph
  fph$content = gsub('"','',fph$content,fixed=TRUE) %>% trimws()
  fph$content = gsub('\\','/',fph$content,fixed=TRUE)
  fph$found = TRUE
  rows = which(fph$cmd=="cd"); n=length(rows)
  if (n>0) {
    dirs = list.dirs(dir)
    for (i in 1:n) {
      r = rows[i]; line = fph$line[r]
      mdir = find.closest.dir(fph$content[r],dirs)
      if (is.null(mdir)) {
        supp.log(line=line,cmd=fph$cmd[r],type = "missing_dir",code=txt[line],msg="Could not find directory. Commented out cd command.")
        txt[line] = paste0("*",txt[line])
        fph$found[r] = FALSE
      } else {
        supp.log(line=line,cmd=fph$cmd[r],type = "change_dir",code=txt[line],msg=paste0("Changed directory to ", mdir))
        fph$content[r] = paste0('"',mdir,'"')
      }
    }
  }

  # dta files
  rows = which(fph$cmd %in% c("use","u","us","saveold", "save","sav","sa","merge","joinby"))
  n=length(rows)
  if (n > 0) {
    dta.files = list.files(dir,glob2rx("*.dta"),full.names = TRUE,recursive = TRUE)
    content = fph$content[rows]
    no.dta = which(!endsWith(content,".dta"))
    if (length(no.dta)>0) {
      content[no.dta] = paste0(content[no.dta],".dta")
    }
    content.base = basename(content)
    files.base = basename(dta.files)
    ma = match(content.base, files.base)
    no.match = which(is.na(ma))
    if (length(no.match)>0) {
      rno.match = rows[no.match]
      supp.log(line=fph$line[rno.match],cmd=fph$cmd[rno.match],type = "missing_dta",code=txt[fph$line[rno.match]],msg=paste0("Could not find ",content.base[no.match], " in any directory of the supplement."))
      fph$found[rno.match] = FALSE
    }
    content[!is.na(ma)] = paste0('"',dta.files[ma[!is.na(ma)]],'"')

    fph$content[rows] = content
  }

  # Simply comment out log commands
  #rows = fph$cmd == "log"; n = length(rows)
  #if (n>0) {
  #  lines = fph$line[rows]
  #  txt[lines] = paste0("*",txt[lines])
  #}

  # TO DO: CORRECT QUOTE CORRECTION
  #no.quote.row = which(!startsWith(fph$content,'"') & !startsWith(fph$content,'`"') & nchar(trimws(fph$content))>0)
  #fph$content[no.quote.row] = paste0('"',fph$content,'"')

  txt = replace.ph.keep.lines(txt, fph)
  fph$file = gsub('"','',fph$content,fixed=TRUE)
  list(txt=txt, ph=fph)
}

replace.files.and.paths.with.ph = function(tab, ph, txt=tab$txt) {
  restore.point("replace.files.and.paths.with.ph")
  txt = replace.ph.keep.lines(txt, ph)
  arg_str = replace.ph.keep.lines(tab$arg_str,ph)
  using = replace.ph.keep.lines(tab$using,ph)
  using[is.na(tab$using)] = NA

  saving = replace.ph.keep.lines(tab$saving,ph)
  saving[is.na(tab$saving)] = NA

  pph = tibble(ph=character(0), content=character(0), line=integer(0), cmd=character(0))
  if (NROW(tab)==0) {
    return(list(txt=txt, ph=pph))
  }
  using.rows = which(is.true(!is.na(using) & nchar(using)>0))

  rows = which(tab$cmd %in% c("use","u","us", "cd","saveold", "save","sav","sa", "mkdir","erase","rm","guse","gsave","gzuse","gzsave"));
  # if use is used together with "using" the first argument refers to variables
  rows = setdiff(rows, using.rows)
  n=length(rows)
  if (n>0) {
    content = trimws(arg_str[rows])
    npph = tibble(ph = paste0("#~use",1:n,"~#"),content=content, line=rows, cmd=tab$cmd[rows])

    for (i in seq_along(rows)) {
      if (nchar(content[i])>0) {
        txt[rows[i]] = sub(content[i],npph$ph[i],txt[rows[i]],fixed = TRUE)
      }
    }
    pph = bind_rows(pph, npph)
  }

  # Import and export commands
  # E.g. import delimit "myfile.csv"
  # The file argument is here after cmd2
  rows = which(
    tab$cmd %in% c("import","export") |
    (tab$cmd %in% c("graph","gr","gra") & tab$cmd2 %in% c("export","save")) |
    (tab$cmd %in% c("putexcel") & tab$cmd2 %in% c("set")) |
    (tab$cmd %in% "adopath" & tab$cmd2 %in% c("+"))
  );
  rows = setdiff(rows, using.rows)
  n=length(rows)
  if (n>0) {
    content = trimws(arg_str[rows]) %>% str.right.of(" ") %>% trimws()
    npph = tibble(ph = paste0("#~use",1:n,"~#"),content=content, line=rows, cmd=tab$cmd[rows])

    for (i in seq_along(rows)) {
      if (nchar(content[i])>0) {
        txt[rows[i]] = sub(content[i],npph$ph[i],txt[rows[i]],fixed = TRUE)
      }
    }
    pph = bind_rows(pph, npph)
  }


  # commands with using argument
  rows = which(is.true(!is.na(using) & nchar(using)>0)); n=length(rows)
  if (n>0) {
    content = trimws(using[rows])
    npph = tibble(ph = paste0("#~using",1:n,"~#"),content=content, line=rows, cmd=tab$cmd[rows])

    for (i in seq_along(rows)) {
      txt[rows[i]] = sub(content[i],npph$ph[i],txt[rows[i]],fixed = TRUE)
    }
    pph = bind_rows(pph, npph)
  }

  # commands with saving option
  rows = which(!is.na(saving)); n=length(rows)
  if (n>0) {
    long.content = paste0("saving",trimws(saving[rows]))
    content = str.between(long.content,"(",")")
    content = trimws(str.left.of(content, ","))
    npph = tibble(ph = paste0("#~saving",1:n,"~#"),content=content, line=rows, cmd=tab$cmd[rows])

    for (i in seq_along(rows)) {
      txt[rows[i]] = sub(paste0("saving(",content[i]),paste0("saving(",npph$ph[i]),txt[rows[i]],fixed = TRUE)
    }
    pph = bind_rows(pph, npph)
  }


  list(txt=txt, ph=pph)
}

# Find closest directory to dir starting from end
find.closest.dir = function(dir, dirs) {
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
