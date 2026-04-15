stata_cmds_postreg_comp = function() {
  unique(c("estimates","predict","test","matrix","est","testparm","lincom","margins", "estat","nlcom","mfx","boottest","center_estimates","estimate","est2vec","suest","fitstat","post","estpost","wild","rivtest","sigstar2","modl","sig_p","ttest","lincomestadd","eret2","cgmwildboot","vareffects","bootwildct","post_param","margin","vce2way","ivstack","predictnl","spatdiag","testnl","p_vals","inteff","meff","iv_stack","dfuller","ivhettest","dfbeta","margeff", "lincomest","mfx2",
    "testparm","testnl","lincom","nlcom","contrast","lrtest","nestreg",
"estat","dfbeta","predict","predictnl","xttest0","xttest3","sts","hotelling","brant","mlogtest","fitstat","listcoef","prchange","prvalue","asprvalue",
"ranktest","ivendog","weakivtest","xtoverid","coldiag2","collin","ivvif","xtserial"
))


}

stata_cmds_postreg = function() {
  c("outreg2","estadd","estimates","predict","test","matrix","est","outreg","eststo","testparm","lincom","esttab","margins", "estout","estat","nlcom","plotcoeffs","mfx","boottest","regsave","center_estimates","estimate","est2vec","suest","fitstat","parmest","post","estpost","savereg","wild","rivtest","sigstar2","modl","svmat","sig_p","ttest","lincomestadd","coefplot","eret2","get_coef","cgmwildboot","vareffects","bootwildct","post_param","avplot","addtotable","margin","vce2way","save_results","ivstack","predictnl","spatdiag","parmby","testnl","b_xt","V_xt","p_vals","inteff","est2tex","meff","marginsplot","iv_stack","dfuller","ivhettest","pValueFormatting","estwrite","dfbeta","margeff", "modltbl","outsheet","outtex","lincomest","mfx2","addstars")
}

stata_cmds_quasireg = function() {
  c("rd", "rdrobust","psmatch2","leebounds", "a2reg","xtabond2","altrdrobust","hausman","stcox", "xtivreg","ivreghdfe","condivreg","oprobit","xtpoisson","newey2","hetprob","reg2hdfespatial")
}

stata_cmds_reg = function() {
  get.regcmds()
}


get.regcmds = function() {
  restore.point("get.regcmds")
  reg.cmds = getOption("repbox.reg.cmds")
  if (!is.null(reg.cmds)) return(reg.cmds)
  file = system.file("misc/regression_cmds.txt", package="repboxStata")
  reg.cmds = readLines(file,warn = FALSE) %>% trimws()
  options(repbox.reg.cmds = reg.cmds)
  return(reg.cmds)
}

repbox_always_cache_cmd = function() {
  c("merge")
}
