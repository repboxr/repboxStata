stata_rename = function(dat, str) {
  restore.point("stata_rename")
  str = trimws(str) %>% trimws_around(" ")
  if (has.substr(str,"(")) {
    lhs.str = str %>% str.left.of(")") %>% str.right.of("(") %>% trimws()
    old = strplit(lhs.str," ")[[1]]

    rhs.str = str %>% str.right.of(")") %>% str.between("(",")") %>% trimws()
    new = strplit(rhs.str," ")[[1]]
  } else {
    old = str.left.of(str," ")
    new = str.right.of(str," ")
  }
  if (any(has.substr("new","*") | any(has.substr("new","?")))) {
    warning("rename with wildcards * or ? is not yet implemented.")
    return(dat)
  }
  old.cols = match(old, names(dat))
  names(dat)[old.cols] = new
  dat
}
