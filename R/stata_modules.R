example = function() {
  stata.is.cmd.installed(c("regress","ivreg2","bysort"))

  stata.install.module("ranktest")

  find.stata.command.module("ivreg2")


  setwd("~/statabox/supp/jpe_126_5_4/Replication Files/Stata")
  log.txt = readLines("statabox_table2.log")
  cmds = find.missing.stata.commands(log.txt)
  modules = find.stata.command.module(cmds[1])

  module = "estout"
  hot = find.stata.hot.modules(1000)
  write.csv(hot,"~/statabox/hot_modules.csv", row.names=FALSE)
  saveRDS(hot, "~/statabox/hot_modules.Rds")
  saveRDS(hot, "~/repbox/repbox/inst/misc/stata_hot_modules.Rds")
}

example.install.hot.modules = function() {
  library(repbox)
  hot = repbox.hot.modules() %>%
    arrange(desc(rank))


}

tab.add.cmd.installed = function(tab) {
  tab$colon_cmd1 = str.left.of(tab$colon1," ") %>% str.left.of(",")
  tab$colon_cmd2 = str.left.of(tab$colon2," ") %>% str.left.of(",")
  tab$colon_cmd3 = str.left.of(tab$colon3," ") %>% str.left.of(",")
  tab$cmd.installed = rep(TRUE,NROW(tab))
  all.cmds = setdiff(unique(c(tab$cmd, tab$colon_cmd1, tab$colon_cmd2, tab$colon_cmd3)),NA)

  idf = stata.is.cmd.installed(all.cmds)

  not.found = filter(idf, installed==FALSE)
  rows = which(
    tab$cmd %in% not.found$cmd |
      tab$colon_cmd1 %in% not.found$cmd |
      tab$colon_cmd2 %in% not.found$cmd |
      tab$colon_cmd3 %in% not.found$cmd
  )
  tab$cmd.installed[rows] = FALSE
  tab
}

# TO DO: ALSO INSTALL COLON COMMANDS
tab.install.missing.modules = function(tab, verbose=TRUE) {
  restore.point("tab.install.missing.modules")
  tab$cmd.installed =TRUE

  tab$colon_cmd1 = str.left.of(tab$colon1," ") %>% str.left.of(",")
  tab$colon_cmd2 = str.left.of(tab$colon2," ") %>% str.left.of(",")
  tab$colon_cmd3 = str.left.of(tab$colon3," ") %>% str.left.of(",")
  all.cmds = setdiff(unique(c(tab$cmd, tab$colon_cmd1, tab$colon_cmd2, tab$colon_cmd3)),NA)

  idf = stata.is.cmd.installed(all.cmds)

  org.cmds = unique(idf$cmd[!idf$installed])
  cmds = setdiff(org.cmds, c("{","}","begin","end","program","prog","pr","progr"))
  if (length(cmds)==0) return(tab)
  ires = lapply(cmds, function(cmd) {
    stata.install.cmd(cmd, verbose=verbose)
  }) %>% bind_rows()

  not.found = filter(ires, installed==FALSE)
  rows = which(
    tab$cmd %in% not.found$cmd |
    tab$colon_cmd1 %in% not.found$cmd |
    tab$colon_cmd2 %in% not.found$cmd |
    tab$colon_cmd3 %in% not.found$cmd
  )
  tab$cmd.installed[rows] = FALSE
  return(tab)
}

make.builtin.statacmds.list = function() {
  # To generate cmd text file, run in Stata
  # statacmds, saving("statacmds.txt")

  txt = readLines("statacmds.txt")
  cmd = str.left.of(txt," ")
  type = str.right.of(txt," ")
  all.df = tibble(cmd=cmd, type=type)
  unique(cmd.df$type)

  cmd.df = filter(all.df, type %in% c("adocommand","builtin")) %>% arrange(type)
  dupl = duplicated(cmd.df$cmd)
  cmd.df = cmd.df[!dupl,]

  fun.df = filter(all.df, !type %in% c("adocommand","builtin"))


  saveRDS(cmd.df, "~/repbox/repbox/inst/misc/stata_builtin_cmd.Rds")
  saveRDS(fun.df, "~/repbox/repbox/inst/misc/stata_builtin_fun.Rds")

}

stata.is.cmd.installed = function(cmd, base_ado_dir = get_base_ado_dir(), ado_dirs = get_ado_dirs(), builtin.cmds = repbox.stata.builtin.cmd()) {
  restore.point("stata.is.cmd.installed")
  first.char = substring(cmd,1,1)
  subpath = paste0("/",first.char,"/",cmd,".ado")

  in.base = cmd %in% filter(builtin.cmds,type=="builtin")$cmd

  in.base = in.base | file.exists(paste0(base_ado_dir,subpath))
  in.extern = rep(FALSE, length(cmd))
  for (dir in ado_dirs) {
    in.extern = in.extern | file.exists(paste0(dir,subpath))
  }
  tibble(cmd=cmd, installed=in.base | in.extern, in.base, in.extern)
}

update.no.install.cmds = function(cmd, glob.dir = opts$glob.dir, no.install.cmds = repbox.stata.noinstall.cmds(), opts=rbs.opts()) {
  if (cmd %in% no.install.cmds) return()
  no.install.cmds = c(no.install.cmds, cmd)
  options(repbox.stata.noinstall.cmds = no.install.cmds)
  file = file.path(glob.dir, "noinstall_cmds.txt")
  writeLines(no.install.cmds, file)
}

repbox.stata.noinstall.cmds = function(glob.dir=opts$glob.dir(), opts=rbs.opts()) {
  res = getOption("repbox.stata.noinstall.cmds")
  if (is.null(res)) {
    if (!is.null(glob.dir)) {
      file = file.path(glob.dir, "noinstall_cmds.txt")
    } else {
      file = system.file("misc/noinstall_cmds.txt", package="repboxStata")
    }
    res = readLines(file, warn=FALSE)
    options(repbox.stata.noinstall.cmds = res)
  }
  return(res)

}


repbox.stata.builtin.cmd = function() {
  res = getOption("repbox.stata.builtin.cmd")
  if (is.null(res)) {
    res = readRDS(system.file("misc/stata_builtin_cmd.Rds", package="repboxStata"))
    options(repbox.stata.builtin.cmd = res)
  }
  return(res)
}


repbox.stata.builtin.fun = function() {
  res = getOption("repbox.stata.builtin.fun")
  if (is.null(res)) {
    res = readRDS(system.file("misc/stata_builtin_fun.Rds", package="repboxStata"))
    options(repbox.stata.builtin.fun = res)
  }
  return(res)
}



repbox.hot.modules = function() {
  hot = getOption("repbox.hot.modules")
  if (is.null(hot)) {
    hot = readRDS(system.file("misc/stata_hot_modules.Rds", package="repboxStata"))
    options(repbox.stata.hot.modules = hot)
  }
  return(hot)
}



stata.install.cmd = function(cmd, hot = repbox.hot.modules(),verbose=TRUE,..., no.install.cmds = repbox.stata.noinstall.cmds()) {
  restore.point("stata.install.cmd")

  if (cmd %in% no.install.cmds | !is.na(as.numeric(cmd))) {
    cat("\nDue to blacklist don't install module for command", cmd)
    res = stata.is.cmd.installed(cmd)
    res$module = NA
    res$hot.rank = NA
    return(res)

  }

  modules = stata.find.command.module(cmd)
  if (length(modules)==0) {
    module = NA
    cat("\nCould not find a module for command", cmd)
    res = stata.is.cmd.installed(cmd)
    res$module = NA
    res$hot.rank = NA
    if (!res$installed) update.no.install.cmds(cmd)
    return(res)

  }

  hot.ind = match(modules, hot$module)
  hot.ind[is.na(hot.ind)] = Inf

  module.df = tibble(module=modules, hot.rank=hot.ind) %>%
    arrange(hot.rank) %>%
    filter(is.finite(hot.rank) | (!startsWith(module,"st")))

  for (i in seq_len(NROW(module.df))) {
    module = module.df$module[i]
    if (verbose) cat(paste0("\ninstall module ", module, " for ", cmd, "... "))
    stata.install.module(module)
    res = stata.is.cmd.installed(cmd)
    if (res$installed) {
      if (verbose) cat(paste0(" ... ok!"))
      res$module = module
      res$hot.rank = module.df$hot.rank[i]
      break
    } else {
      if (verbose) cat(paste0("... did not work."))
      res$module = NA
      res$hot.rank = NA
    }
  }
  if (is.na(res$module))
    update.no.install.cmds(cmd)
  res
}

# installs a stata module locally into the provided directory
stata.install.module = function(module, dir = get_ado_dirs()[1], verbose=FALSE) {
  restore.point("stata.install.module")
  oldwd = getwd()
  setwd(dir)
  stata.cmd = paste0('net set ado "', dir,'"\nssc install ', module)
  temp.do = "statabox__temp__.do"
  writeLines(stata.cmd, temp.do)
  res = system2("stata", args="statabox__temp__.do",stdout=TRUE, stderr=TRUE)
  if (verbose)
    cat(paste0(res[-c(1:15)], collapse="\n"))
  file.remove(temp.do)
  setwd(oldwd)
}

stata.find.hot.modules = function(n) {
  res = system2("stata", args=paste0("'ssc hot, n(",n,")'"), stdout=TRUE, stderr=TRUE)

  start.line = which(has.substr(res,"Rank") & has.substr(res," # hits"))[1]

  txt = res[start.line+2:length(res)]
  end.line = which(startsWith(trimws(txt),"--------"))[1]
  txt = txt[1:end.line]
  num = substr(txt,1,6) %>% trimws() %>% as.integer()
  lines = !is.na(num)
  txt = substring(txt[lines],7)
  num = num[lines]
  hits = as.numeric(substring(txt,1,9) %>% trimws())
  txt = substring(txt,10) %>% trimws()
  module = str.left.of(txt," ")

  dat = tibble(rank=num, hits=hits, module=module)
  dat
}

stata.find.command.module = function(stata.cmd) {
  res = system2("stata", args=paste0("findit ", stata.cmd), stdout=TRUE, stderr=TRUE)
  txt = res[has.substr(res, "from http")]
  modules=str.left.of(txt, " ")
  modules
}

stata.find.missing.commands.from.log = function(log.txt=readLines(log.file), log.file) {
  log.txt = trimws(log.txt)
  lines = which(startsWith(log.txt, "command ") & endsWith(log.txt, "is unrecognized"))
  if (length(lines)==0) return(NULL)
  cmds = str.between(log.txt[lines],"command ", "is unrecognized") %>% trimws()
  cmds
}


save.stata.module.descriptions = function() {
  hot = repbox.hot.modules()

}
