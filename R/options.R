# Create an options object that will by used by different functions
# when running an analysis

#' Options for Stata code supplement reproduction
#'
#' @param use.includes If TRUE (default) don't separately run do files that are included in another do file (or called via run or do). If FALSE comment out any do, run or include command and run all files separately.
#' @param just.files if not NULL (default=NULL) names of do files that only should be run. Other do files will be ignored.
#' @param verbose if TRUE more information will be printed during the analysis
#' @param extract.reg.info if TRUE inject calls to esttab after regression commands that store the regression results in a separate folder. Requires development of a repboxReg package.
#' @param keep.old.reg.info Will be only relevant if extract.reg.info = FALSE. Should potentially previously generated regression info be kept. Requires development of a repboxReg package.
#' @param extract.scalar.vals if TRUE save values of a scalar variable after its definition into a separate file. These scalar values might be neccessary for metareg replications, e.g. if the regression command bases an if condition on the scalar value. We don't have to save values of local or global macros as their value will be spliced in.
#' @param just.extract Don't run do files again but just extract the results from a previous run (can be used if some code in the analysis package was changed inbetween to save time).
#' @param timeout How many seconds is a single do file allowed to run before the evaluation shall be stopped with a timeout
#' @param all.do.timeout A global timout for all do files in the project. If the previous do files took longer than all.do.timeout in total then no more do file is run.
#' @param glob.dir A directory in which globally relevant information from the analysis of this package can be stored.
#' @param force Default = TRUE. If FALSE do not run if the Stata files were already analysized. Formally, we check whether in the repbox/stata folder the file "repbox_results.Rds" exists.
#' @param install.missing.modules Default = FALSE. Shall we try to install missing modules? Currently the approach is to just have all possibly relevant Stata modules being installed in the Docker container. Installing modules on a case by case basis is not well tested.
#' @param set.stata.defaults If TRUE (default) then add at the beginning of a do file the line set_defaults _all, permanently. Otherwise previous supplements may permanently change the default options, which can cause errors in later supplements. For example, we had the case that variable name abbreviation was turned off, which caused errors in later supplement. This is not neccessary.
#' @param store.data Possibility to store data sets after some commands. Either NULL (default), i.e. no data is stored or a data set with columns donum, line, counter. Makes only sense to specify if run.df is known for a run where no data sets were stored.
#' @param store.use.data.sig Shall the data signatures after use and other commands that load data be computed and stored. Makes sense to check how data sets change across commands, but takes time.
#' @param loop.log.max Not yet implemented. How many times shall a command inside a loop be logged at most? Some do files have super long loops. The problem is that this can lead to explosion of the cmd files. We try to adapt the code to stop writing in precmd and postcmd if a command was called too often inside a loop.
#' @param run.first a character string of dofiles that should be run first. Can be used to run install code first
#' @param rerun.failed.included.do Shall do files that are included in other do files (or called via do) run again separately if no log file exists. Default is TRUE, but if initial run failed the 2nd separate run may not work correctly since data sets or variable might be missing.
repbox_stata_opts = function(
  use.includes=TRUE,just.files=NULL,verbose=TRUE, extract.reg.info=FALSE, keep.old.reg.info = !extract.reg.info, extract.scalar.vals = extract.reg.info, just.extract=FALSE, timeout=20,all.do.timeout = 60*60*5, glob.dir=NULL, force=TRUE, install.missing.modules = FALSE, report.inside.program = TRUE, set.stata.defaults.perma = FALSE, store.data = NULL, store.use.data.sig = !is.null(store.data), loop.log.cmd.max=10000, comment.out.install=TRUE, check.stata.paths.and.ado=TRUE,rerun.failed.included.do=TRUE, ...) {
  opts = list(
    use.includes=use.includes,
    just.files = just.files,
    verbose=verbose,
    extract.reg.info = extract.reg.info,
    keep.old.reg.info = keep.old.reg.info,
    extract.scalar.vals = extract.scalar.vals,
    just.extract = just.extract,
    timeout=timeout,
    all.do.timeout = all.do.timeout,
    glob.dir=glob.dir,
    force=force,
    install.missing.modules = install.missing.modules,
    report.inside.program = report.inside.program,
    set.stata.defaults.perma = set.stata.defaults.perma,
    store.data = store.data,
    store.use.data.sig = store.use.data.sig,
    loop.log.cmd.max = loop.log.cmd.max,
    comment.out.install = comment.out.install,
    check.stata.paths.and.ado=check.stata.paths.and.ado,
    rerun.failed.included.do = rerun.failed.included.do,
    ...
  )
  opts
}


rbs.opts = function(name=NULL) {
  opts = getOption("repbox.stata.options")
  if (is.null(name)) {
    # If no options set use default options
    if (is.null(opts)) {
      opts = repbox_stata_opts()
      options(repbox.stata.options=opts)
    }
    return(opts)
  }
  opts[[name]]
}
