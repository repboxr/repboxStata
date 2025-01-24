#
# This file contains functions to parse Stata code in do files
#

example.repbox.parse = function() {
  library(repbox)
  restore.point.options(display.restore.point = FALSE)

  project = "testsupp"
  project_dir = file.path("~/repbox/projects_reg",project)
  run.project.with.reg(project_dir,store.reg.dataset = TRUE)

}

repbox.normalize.do = function(txt=readLines(do.file), do.file=NULL) {
  restore.point("repbox.normalize.do")
  #writeLines(txt,"~/repbox/temp.do")
  txt = merge.lines(txt)
  txt = paste0("\n",txt)
  pos.df = repa.pos.df(txt)
  s = repa.new.state(txt, pos.df)
  s$do.file = do.file

  pos.df = s$pos.df
  # UPDATE: stri_sub handles encoding problems better than substring
  pos.df$str = stringi::stri_sub(txt, pos.df$start,pos.df$end)
  #pos.df$str = str = substring(txt, pos.df$start,pos.df$end)

  #restore.point.options(display.restore.point = TRUE)
  r = 0
  #for (r in seq_len(NROW(s$pos.df))) {
  while(r < NROW(s$pos.df)) {
    r = r+1
    #cat(paste0("\n",r," row=",s$row+1," line=",s$line,  " prev.mode=", s$mode, " level=",s$level),"\norgline=",s$orgline, " start.orgline=",s$start.orgline)
    s = repa.next.token(s)
    #cat("\n  post: mode=", s$mode, " level=", s$level," type=",pos.df$type[r],"orgline=",s$orgline,"orgline: newtxt=\n",paste0(s$orglines[1:s$line],": ",s$newtxt[1:s$line],collapse="\n\t"))
  }
  s = repa.finish.s(s)
  rbind(s$orglines, s$end.orglines)

  #writeLines(s$newtxt,"~/repbox/temp2.do")
  return(s)
}

repa.pos.df = function(txt, oline.offset=0) {
  restore.point("repa.pos.df")
  loc.all = function(pattern,type=pattern,fixed=TRUE, perl=!fixed) {
    restore.point("loc.all")
    #pos = gregexpr(pattern, txt, fixed=fixed)[[1]]
    if (!fixed) {
      pos = stri_locate_all_regex(ltxt, pattern, omit_no_match = TRUE)[[1]]
    } else {
      pos = stri_locate_all_fixed(ltxt, pattern, omit_no_match = TRUE)[[1]]
    }
    # stringi code above is faster and more robust to utf-8 issues
    #pos = str.locate.all(ltxt, pattern, fixed=fixed,perl=perl)[[1]]
    if (NROW(pos)==0) return(NULL)
    as_tibble(list(type=type, pattern=pattern, start=pos[,1], end=pos[,2]))
  }
  ltxt = tolower(txt)
  pos.df = bind_rows(
    loc.all("\n[ \t]*","nl", fixed=FALSE),
    loc.all(";[ \t]*","semi",fixed=FALSE),
    loc.all("/*"),
    loc.all("*/"),
    loc.all("///","ce3"),
    # match // but not ///
    #loc.all("(?<!/)//(?!/)","ce2", fixed=FALSE),
    # // end of line comment
    # must be preceded with a space or tab
    # unless it is at the beginning of the line
    loc.all("(?<=[ \t\n])//(?!/)","ce2", fixed=FALSE),
    loc.all('`"', "dquote_start",fixed=TRUE),
    loc.all('"\'', "dquote_end",fixed=TRUE),
    loc.all('"', "quote",fixed=TRUE),
    loc.all('\n[ \t]*\\*',"nl_star",fixed=FALSE),
    loc.all(';[\n \t]*\\*',"semi_star",fixed=FALSE),
    loc.all('#[ \t]*((d)|(de)|(del)|(deli)|(delim)|(delimit))[ \t]*;',"del_semi",fixed=FALSE),
    loc.all('#[ \t]*((d)|(de)|(del)|(deli)|(delim)|(delimit))[ \t]+cr',"del_nl",fixed=FALSE),
    loc.all('#[ \t]*((d)|(de)|(del)|(deli)|(delim)|(delimit))[ \t]*\n',"del_switch",fixed=FALSE),
    #loc.all('#[ \t]*del(imit)?[ \t]*;',"del_semi",fixed=FALSE),
    #loc.all('#[ \t]*del(imit)?[ \t]+cr',"del_nl",fixed=FALSE),
  ) %>%
    arrange(start,end) %>%
    mutate(oline = cumsum(type=="nl")+oline.offset)
}

repa.new.state = function(txt, pos.df, init.ph = 1000, init.newtxt = NROW(pos.df)*10) {
  s = list(
    counts = c( quote=0, co=0, cl=0, ce2=0, ce3=0, dquote=0),
    ph.count = 0,
    ph.df = tibble(ph=rep("", init.ph), content = ""),
    newtxt = rep("", init.newtxt),
    orglines = rep(NA_integer_, init.newtxt),
    # Update 2024-12-07: Try to store end of orglines
    end.orglines = rep(NA_integer_, init.newtxt),
    pos.df = pos.df,
    txt = txt,
    start = 1,
    row = 0,
    line = 1,
    orgline = 0, # start with 0 because we add empty line
    start.orgline = 0,
    level = 0,
    delimit ="nl",
    mode = "cmd"
  )
  s
}

repa.finish.s = function(s) {
  restore.point("repa.finish.s")
  if (s$start <= nchar(s$txt)) {
    s = repa.write.prev(s,end=nchar(s$txt))
  }
  s$newtxt = trimws(s$newtxt[seq_len(s$line)])
  s$orglines = s$orglines[seq_len(s$line)]
  s$end.orglines = s$end.orglines[seq_len(s$line)]

  s$ph.df = s$ph.df[seq_len(s$ph.count),,drop=FALSE]
  s$mode = "finished"
  s
}

repa.next.token = function(s) {
  restore.point("repa.next.token")

  s$row = s$row+1
  if (s$row > NROW(s$pos.df)) {
    return(repa.finish.s(s))
  }
  if (length(s$newtxt)< s$line-5) {
    s$newtxt = c(s$newtxt, rep("", length(s$newtxt)))
    s$orglines = c(s$orglines, rep(NA_integer_,length(s$newtxt)))
    s$end.orglines = c(s$end.orglines, rep(NA_integer_,length(s$newtxt)))
  }

  type = s$pos.df$type[s$row]
  if (type=="nl") {
    s$orgline = s$orgline+1
  }
  if (s$mode == "co") {
    if (type != "*/") return(s)
    return(repa.end.co(s))
  } else if (s$mode == "cmd") {
    if (type == s$delimit) return(repa.end.line(s))
    if (type == "quote") return(repa.start.quote(s))
    if (type == paste0(s$delimit,"_star")) return(repa.start.cl(s))
    if (type == "/*") return(repa.start.co(s))
    if (type == "ce2") return(repa.start.ce2(s))
    if (type == "ce3") return(repa.start.ce3(s))
    if (type == "del_semi" | type == "del_nl" | type == "del_switch") return(repa.set.delimit(s))
    if (type == "dquote_start") return(repa.start.dquote(s))
    return(s)
  } else if (s$mode == "quote") {
    if (type == "nl" & s$delimit=="nl")
      return(repa.end.quote.with.nl(s))
    if (type != "quote") return(s)
    return(repa.end.quote(s))
  } else if (s$mode == "cl") {
    if (type != s$delimit) return(s)
    return(repa.end.cl(s))
  } else if (s$mode == "dquote") {
    #restore.point("double_quote_mode")
    #cat("\nmode==dquote  type==,",type)
    # Also compound quotes are sometimes ended with a newline
    if (type == "nl" & s$delimit=="nl")
      return(repa.end.dquote.with.nl(s))
    if (type == "dquote_start") {
      s$level = s$level+1
      return(s)
    }
    if (type != "dquote_end") return(s)
    if (s$level > 0) {
      s$level = s$level-1
      return(s)
    } else {
      return(repa.end.dquote(s))
    }
  } else if (s$mode == "ce2") {
    if (type != s$delimit) return(s)
    return(repa.end.ce2(s))
  } else if (s$mode == "ce3") {
    if (type != s$delimit) return(s)
    return(repa.end.ce3(s))
  } else {
    stop(paste0("Unknown parser mode ", s$mode))
  }
}

repa.parse.error = function(s, msg) {
  restore.point("repa.parse.error")
  start = s$start
  end =  s$pos.df$start[s$row]-1

  new = gsub("\n"," ",substring(s$txt, start, end), fixed=TRUE)
  code = paste0(s$newtxt[s$line],new)

  stop(paste0("Error when parsing do file ",s$do.file,"\n", msg,"\n\n",code),call. = FALSE)
}

repa.write.prev = function(s,mode=s$pos.df$type[s$row], start = s$start, end =  s$pos.df$start[s$row]-1, remove.nl=TRUE) {
  restore.point("repa.write.prev")
  pos.df = s$pos.df
  s$row
  if (start > end) {
    #stop("start bigger end")
    s$start = end+1
    s$mode = mode
    return(s)
  }

  new = gsub("\n"," ",substring(s$txt, start, end), fixed=TRUE)
  s$newtxt[s$line] = paste0(s$newtxt[s$line],new)

  if (is.na(s$orglines[s$line]))
    s$orglines[s$line] = min(s$start.orgline + (s$delimit=="semi"),s$pos.df$oline[s$row])

  s$end.orglines[s$line] = if (s$delimit=="semi") s$pos.df$oline[s$row] else s$start.orgline
  #stop("jjshdshd")


  s$start = end+1
  s$mode = mode
  s
}

repa.write.ph = function(s, type=s$pos.df$type[s$row], start = s$start, end =  s$pos.df$end[s$row]) {
  restore.point("repa.write.ph")
  if (start > end) stop("start bigger end")

  count = s$counts[type]+1
  s$counts[type] = count
  s$ph.count = s$ph.count+1
  ph = paste0("#~",type,"_",count,"~#")
  content = substring(s$txt, start, end)
  if (NROW(s$ph.df)<s$ph.count) {
    s$ph.df = bind_rows(s$ph.df, tibble(ph=rep("", NROW(s$ph.df)), content = ""))
  }
  s$ph.df$ph[s$ph.count] = ph
  s$ph.df$content[s$ph.count] = content
  s$newtxt[s$line] = paste0(s$newtxt[s$line], ph)

  if (is.na(s$orglines[s$line]))
    s$orglines[s$line] = min(s$start.orgline + (s$delimit=="semi"),s$pos.df$oline[s$row])

  s$end.orglines[s$line] = s$pos.df$oline[s$row]


  s$start = end+1
  s$mode = "cmd"
  s
}


# "my text" quote
repa.start.quote = function(s) {
  restore.point("repa.start.quote")
  s = repa.write.prev(s,"quote")
  return(s)
}

repa.end.quote = function(s) {
  restore.point("repa.end.quote")
  s = repa.write.ph(s,"quote")
  return(s)
}

# Stata is not completely consistent with tackeling quotes at end of line
# The following command causes an error:
# gen x = "a
# > too few quotes
# The following command works as if the quote was closed at the end of the line
# label var x "mylabel
# So we just copy the code as it is and end the quote
repa.end.quote.with.nl = function(s) {
  restore.point("repa.end.quote.with.nl")
  s = repa.write.ph(s,"quote")
  s$start = s$pos.df$end[s$row]+1
  s$mode = "cmd"
  s$line = s$line+1
  s$start.orgline = s$orgline
  return(s)

  #If we wanted to throw an error
  #return(repa.parse.error(s, "Quote is not closed at end of line. (If you want a line break in a string constant, you can set '#delimit ;')"))

}


# `"my text"'  compound double quote
repa.start.dquote = function(s) {
  restore.point("repa.start.dquote")
  s = repa.write.prev(s,"dquote")
  s$level = 0
  return(s)
}

repa.end.dquote = function(s) {
  restore.point("repa.end.quote")
  s = repa.write.ph(s,"dquote")
  return(s)
}

# Compound quotes `" "' have similar inconsistent handling as quotes
# with respect to end of lines
# (see repa.end.quote.with.nl)
repa.end.dquote.with.nl = function(s) {
  restore.point("repa.end.dquote.with.nl")
  s = repa.write.ph(s,"dquote")
  s$start = s$pos.df$end[s$row]+1
  s$mode = "cmd"
  s$line = s$line+1
  s$start.orgline = s$orgline
  return(s)
}


# /* ... */ comment
repa.start.co = function(s) {
  restore.point("repa.start.co")
  s = repa.write.prev(s,"co")
  substring(s$txt, s$start,s$start)
  return(s)
}

repa.end.co = function(s) {
  restore.point("repa.end.co")
  s = repa.write.ph(s,"co")
  return(s)
}


# // comment at end of line
repa.start.ce2 = function(s) {
  restore.point("repa.start.ce2")
  s = repa.write.prev(s,"ce2")
  return(s)
}

repa.end.ce2 = function(s) {
  restore.point("repa.end.ce2")
  s = repa.write.ph(s,"ce2")
  s$line = s$line+1
  s$start.orgline = s$orgline
  return(s)
}


# /// comment at end of line: merges lines
repa.start.ce3 = function(s) {
  restore.point("repa.start.ce3")
  s = repa.write.prev(s,"ce3")
  return(s)
}

repa.end.ce3 = function(s) {
  restore.point("repa.end.ce3")
  s = repa.write.ph(s,"ce3")
  return(s)
}

# complete line comment starting with *
repa.start.cl = function(s) {
  restore.point("repa.start.cl")
  #repa.write.prev(s)
  s$mode = "cl"
  return(s)
}

repa.end.cl = function(s) {
  restore.point("repa.end.cl")
  s = repa.write.ph(s,"cl")
  s$line = s$line+1
  s$start.orgline = s$orgline
  return(s)
}

repa.end.line = function(s) {
  restore.point("repa.end.cmd")
  #if (s$line > 1) stop()
  end =  s$pos.df$start[s$row]-1
  if (end >= s$start)
    s = repa.write.prev(s,"cmd", end=end)
  s$start = s$pos.df$end[s$row]+1
  s$mode = "cmd"
  s$line = s$line+1
  s$start.orgline = s$orgline
  return(s)
}

repa.set.delimit = function(s) {
  restore.point("repa.set.delimit")
  type = s$pos.df$type[s$row]

  if (type == "del_semi") {
    s$delimit = "semi"
  } else if (type == "del_switch") {
    if (isTRUE(s$delimit=="semi")) {
      s$delimit = "nl"
    } else {
      s$delimit = "semi"
    }
  }else {
    s$delimit = "nl"
    restore.point("repa.set.delimit2")
  }
  s$mode = "cmd"
  #new = paste0("* ",substring(s$txt, s$pos.df$start[s$row], s$pos.df$end[s$row]))
  #s$newtxt[s$line] = new

  s$start = s$pos.df$end[s$row]+1

  return(s)
}

repbox.do.table = function(s=NULL,txt=s$newtxt, ph.df = s$ph.df) {
  restore.point("repa.do.table")

  #orgline.marker = ifelse(is.na(s$orglines),"",paste0("#~oline",s$orglines,"~#"))
  orgline.marker = ifelse(is.na(s$orglines),"",paste0("#~oline",s$orglines,"-", s$end.orglines, "~#"))
  newtxt = paste0(orgline.marker,s$newtxt)
  txt = merge.lines(newtxt)

  # Remove comment placeholders
  co.ph.df = ph.df %>%
    filter(startsWith(ph,"#~c")) %>%
    mutate(content = "")
  txt = replace.placeholders(txt, co.ph.df)

  # Set brackets () into ph
  pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br"))
  if (is(pho,"try-error")) {
    pho = stepwise.blocks.to.placeholder(txt, ph.df)
  }
  txt = pho$str; br.ph.df = pho$ph.df
  if (any(duplicated(br.ph.df$ph))) {
    stop("Parsing error bracket place holders are duplicated. Need to correct placeholder block code.")
  }


  # Find Mata blocks and replace with placeholder
  mata_pos = locate_mata_blocks(txt)
  if (NROW(mata_pos)>0) {
    pho = pos.to.placeholder(txt, mata_pos,ph.prefix = "#~mata_pa ", ph.df=ph.df)
    txt = pho$str; ph.df = pho$ph.df
    # pho = blocks.to.placeholder(txt, start="{", end="}",before.start = c("mata ","mata"),ph.df = ph.df, ph.prefix="#~mata_pa")
    #
    # txt = pho$str; ph.df = pho$ph.df
    #
    # txt = trimws(sep.lines(txt))
    # pos = start.end.line.blocks(txt,start = "mata",end="end",multi.end = TRUE)
    # pho = line.blocks.to.placeholder(txt,pos,ph.df = ph.df, ph.prefix="#mata_lb")
    # txt = pho$str; ph.df = pho$ph.df
  }


  #cat(txt)
  txt = sep.lines(txt)
  has.orgline = startsWith(txt,"#~oline")
  orgline_txt = ifelse(has.orgline, str.between(txt,"#~oline","~#"),"")
  orgline_start = ifelse(has.orgline,as.integer(str.left.of(orgline_txt,"-")),NA_integer_)
  orgline_end = ifelse(has.orgline,as.integer(str.right.of(orgline_txt,"-")),NA_integer_)

  #orgline = ifelse(has.orgline, str.between(txt,"#~oline","~#") %>% as.integer(),NA_integer_)
  txt[has.orgline] = str.right.of(txt[has.orgline],"~#")

  str = txt

  # Replace tabs with spaces
  # Otherwise we wont correctly store the cmd
  # variable
  str = gsub("\t"," ", str, fixed=TRUE)

  saving = str.right.of(str,"saving#~br",not.found = NA)
  srows = which(!is.na(saving))
  if (length(srows)>0) {
    saving[srows] = str.left.of(saving[srows],"~#")
    saving[srows] = paste0("#~br", saving[srows],"~#")
  }

  quietly = rep(NA_character_, length(str))
  rows = startsWith(str, "quietly:")
  quietly[rows] = "quietly:"
  str[rows] = str.right.of(str[rows],"quietly:")
  rows = startsWith(str, "quietly ")
  quietly[rows] = "quietly "
  str[rows] = trimws(str.right.of(str[rows],"quietly "))
  rows = startsWith(str, "qui ")
  quietly[rows] = "qui "
  str[rows] = trimws(str.right.of(str[rows],"qui "))

  capture = rep(NA_character_, length(str))
  rows = startsWith(str, "capture:")
  capture[rows] = "capture:"
  str[rows] = str.right.of(str[rows],"capture:")
  rows = startsWith(str, "capture ")
  capture[rows] = "capture "
  str[rows] = trimws(str.right.of(str[rows],"capture "))
  rows = startsWith(str, "cap ")
  capture[rows] = "cap "
  str[rows] = trimws(str.right.of(str[rows],"cap "))


  # change :\ ad :/ as this is part of file path

  str =gsub(":\\","~;~\\", str, fixed=TRUE)
  str =gsub(":/","~;~\\", str, fixed=TRUE)

  str = trimws(str)
  opens_block = endsWith(str, "{")
  closes_block = str == "}"

  colon1 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon2 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon3 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  str = gsub("~;~\\",":\\", str, fixed=TRUE)

  # Some commands use : in a different way. Then don't store colon stuff
  no.colon = which(startsWith(txt, "merge"))
  if (length(no.colon) > 0) {
    colon1[no.colon] = colon2[no.colon] = colon3[no.colon] = NA
    str[no.colon] = txt[no.colon]
  }

  str = gsub(","," ,", str, fixed=TRUE)
  cmd = str.left.of(str," ")
  str = paste0(" ",str.right.of(str," "))
  cmd_br = str.right.of(cmd,"#~br",not.found=NA)
  cmd_br = ifelse(is.na(cmd_br),NA,paste0("#~br",cmd_br))
  cmd = str.left.of(cmd, "#")
  cmd = str.left.of(cmd,"{")

  opts = str.right.of(str,",",not.found=NA) %>% trimws()
  str = str.left.of(str,",")

  # Extracting weight variables [myweight] got more complicated:
  # if conditions can also contain [] like if id=id[_n-1]
  # we thus suppose that a weight string must have a space before
  # need to check whether that is indeed always the case
  weight = rep("", length(str))
  weight_start = stri_locate_first_regex(str,"(?<![a-z0-9A-Z_])\\[")[,1]
  wrows = which(!is.na(weight_start))
  if (length(wrows)>0) {
    weight_start = weight_start[wrows]
    rstr = substring(str[wrows], weight_start)
    weight_end = stri_locate_first_regex(rstr,"\\](?![a-z0-9A-Z_])")[,1]
    #weight_end = stri_locate_first_fixed(rstr, "]")[,1]
    use_wrows = !is.na(weight_end)
    weight[wrows[use_wrows]] = stri_sub(rstr[use_wrows],2,weight_end[use_wrows]-1)
    str[wrows[use_wrows]] = stri_sub(str[wrows[use_wrows]], weight_start[use_wrows])
  }
  #weight = str.between(str,"[","]", not.found=NA) %>% trimws()
  #str = str.left.of(str,"[")


  # Default order is if, in, using
  # but sometimes different order is used like using, if

  # using = str.right.of(str," using ",not.found=NA)  %>% trimws()
  # str = str.left.of(str," using ")
  # in_arg = str.right.of(str," in ",not.found=NA)  %>% trimws()
  # str = str.left.of(str," in ")
  # if_arg = str.right.of(str," if ",not.found=NA)  %>% trimws()
  # str = str.left.of(str," if ")

  res = extract.if.in.using(str)
  str = trimws(res$str)
  using = res$parts$using
  in_arg = res$parts[["in"]]
  if_arg = res$parts[["if"]]




  exp = str.right.of(str,"=",not.found=NA_character_)  %>% trimws()
  str = str.left.of(str,"=")
  arg_str = str

  cmd2 = str.left.of(trimws(arg_str)," ", not.found=NA_character_) %>% trimws()
  cmd2[startsWith(cmd2,"#~")] = NA_character_



  program = ifelse(startsWith(txt, "program define "), str.between(txt,"program define ", " "), NA)

  txt = replace.ph.keep.lines(txt, br.ph.df)
  arg_str = replace.ph.keep.lines(arg_str, br.ph.df)
  exp = replace.ph.keep.lines(exp, br.ph.df)
  cmd_br = replace.ph.keep.lines(cmd_br, br.ph.df)
  opts = replace.ph.keep.lines(opts, br.ph.df)

  na.rows = which(is.na(saving))
  saving = replace.ph.keep.lines(saving, br.ph.df)
  saving[na.rows] = NA_character_


  tab = data.frame(cmd,cmd_br=cmd_br,arg_str, exp, if_arg, in_arg, using, opts, cmd2, saving, txt, colon1, colon2,colon3, program, opens_block, closes_block, quietly, capture,orgline=orgline_start, orgline_start=orgline_start, orgline_end=orgline_end)
  tab = filter(tab, nchar(trimws(tab$txt))>0)

  # In do files with #delimit ; commands not always a unique
  # orgline is determined. We want to set that line to orgline
  # in which the cmd starts
  rows = which(tab$orgline_start != tab$orgline_end)
  if (length(rows)>0) {
    # remove first line which is empty and not accounted
    # for in orgline
    org_txt = sep.lines(s$txt)[-1]
    for (r in rows) {
      cmd = tab$cmd[r]
      olines = tab$orgline_start[r]:tab$orgline_end[r]
      # prefer later lines: idea is that more likely
      # a comment before the line contains the command
      # than a comment below the line
      points = startsWith(org_txt[olines],cmd) + has.substr(org_txt[olines],cmd) + olines*1e-6
      tab$orgline[r] = olines[which.max(points)]
    }
  }




  # Special treatment for outdated 'for any' command. Like
  # for any y1 y2: reg X z1
  # We will say that cmd="for" because we cannot handle for any
  # when analysing regressions

  # Special treatment for outdated "for any" command in combi with regression. Like
  # for any y1 y2: reg X z1
  # We cant well handle those regressions and thus change the command name
  rows = which(startsWith(tab$colon1,"for any"))
  if (length(rows)>0) {
    regcmds = get.regcmds()
    rows = which(startsWith(tab$colon1,"for any") & tab$cmd %in% regcmds)
    tab$cmd[rows] = paste0("__for_any_", tab$cmd[rows])
  }


  tab = tab.repair.colon.local(tab)
  tab = tab.repair.input.cmds(tab)
  tab = tab.replace.texdoc.do(tab)
  tab = tab.add.block.end(tab)

  tab$line = seq_len(NROW(tab))
  if (any(is.na(tab$orgline))) {
    stop("Parsing of orgline was not correct. As tab$orgline has NA. Pleas debug parsing code.")
  }

  tab = tab.add.in.program(tab)
  tab  = tab.add.in.loop(tab)
  list(tab=tab, ph.df = ph.df)
}

extract.if.in.using = function(str) {
  restore.point("extract.if.in.using")
  #str = c("dothings x using x if a>5","", "dothings x if a>5 using x")


  patterns = c(" if ", " in ", " using ")
  keys = c("if", "in", "using")

  pos = pos.end = matrix(NA, nrow=length(str), ncol=length(keys))
  #colnames(pos) = keys
  col = 1
  for (col in seq_along(keys)) {
    col.pos = str.locate.first(str, patterns[col])
    pos[,col] = col.pos[,1]
    pos.end[,col] = col.pos[,2]
  }
  pos[is.na(pos)] = -Inf
  pos.start = pos
  max.col(pos)

  col = 3
  key.str = replicate(length(keys), rep(NA_character_, length(str)),simplify = FALSE)
  names(key.str) = keys

  changed = FALSE
  while(TRUE) {
    for (col in rev(seq_along(keys))) {
      has.col = is.finite(pos[,col]) & max.col(pos) == col
      if (any(has.col)) {
        changed = TRUE
      } else {
        next
      }
      key.str[[col]][has.col] = substring(str[has.col], pos.end[has.col,col]+1) %>% trimws()
      str[has.col] = substring(str[has.col],1,pos[has.col,col]-1)
      pos = pos[,-col, drop=FALSE]
      pos.start[has.col, col] = -Inf
    }
    if (!changed) break
    changed = FALSE
    pos = pos.start
  }

  return(list(str = str,parts=key.str))

}

normalized.cmdlines.to.tab = function(txt, ph.df, orglines=NULL) {
  restore.point("normalized.cmdlines.with.ph.to.tab")

  str = txt
  quietly = rep(NA_character_, length(str))
  rows = startsWith(str, "quietly:")
  quietly[rows] = "quietly:"
  str[rows] = str.right.of(str[rows],"quietly:")
  rows = startsWith(str, "quietly ")
  quietly[rows] = "quietly "
  str[rows] = trimws(str.right.of(str[rows],"quietly "))
  rows = startsWith(str, "qui ")
  quietly[rows] = "qui "
  str[rows] = trimws(str.right.of(str[rows],"qui "))

  # change :\ ad :/ as this is part of file path

  str =gsub(":\\","~;~\\", str, fixed=TRUE)
  str =gsub(":/","~;~\\", str, fixed=TRUE)

  colon1 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon2 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":")
  colon3 = str.left.of(str, ":",not.found = NA) %>% trimws()
  str = str.right.of(str, ":") %>% trimws()
  str = gsub("~;~\\",":\\", str, fixed=TRUE)

  # Some commands use : in a different way. Then don't store colon stuff
  no.colon = which(startsWith(txt, "merge"))
  if (length(no.colon) > 0) {
    colon1[no.colon] = colon2[no.colon] = colon3[no.colon] = NA
    str[no.colon] = txt[no.colon]
  }

  str = gsub(","," ,", str, fixed=TRUE)
  cmd = str.left.of(str," ")
  str = paste0(" ",str.right.of(str," "))
  cmd_br = str.right.of(cmd,"#~br",not.found=NA)
  cmd_br = ifelse(is.na(cmd_br),NA,paste0("#~br",cmd_br))
  cmd = str.left.of(cmd, "#")

  opts = str.right.of(str,",",not.found=NA) %>% trimws()
  str = str.left.of(str,",")

  # Extracting weight variables [myweight] got more complicated:
  # if conditions can also contain [] like if id=id[_n-1]
  # we thus suppose that a weight string must have a space before
  # need to check whether that is indeed always the case
  weight = rep("", length(str))
  weight_start = stri_locate_first_regex(str,"(?<![a-z0-9A-Z_])\\[")[,1]
  wrows = which(!is.na(weight_start))
  if (length(wrows)>0) {
    weight_start = weight_start[wrows]
    rstr = substring(str[wrows], weight_start)
    weight_end = stri_locate_first_fixed(rstr, "]")[,1]
    use_wrows = !is.na(weight_end)
    weight[wrows[use_wrows]] = stri_sub(rstr[use_wrows],2,weight_end-1)
    str[wrows[use_wrows]] = stri_sub(str[wrows[use_wrows]], weight_start[use_wrows])
  }


  #weight = str.between(str,"[","]", not.found=NA) %>% trimws()
  #str = str.left.of(str,"[")


  # Default order is if, in, using
  # but sometimes different order is used like using, if

  res = extract.if.in.using(str)
  str = res$str
  using = res$parts$using
  in_arg = res$parts[["in"]]
  if_arg = res$parts[["if"]]

  exp = str.right.of(str,"=",not.found=NA)  %>% trimws()
  str = str.left.of(str,"=")
  arg_str = str

  program = ifelse(startsWith(txt, "program define "), str.between(txt,"program define ", " "), NA)

  txt = replace.ph.keep.lines(txt, ph.df)
  arg_str = replace.ph.keep.lines(arg_str, ph.df)
  exp = replace.ph.keep.lines(exp, ph.df)
  cmd_br = replace.ph.keep.lines(cmd_br, ph.df)
  opts = replace.ph.keep.lines(opts, ph.df)

  tab = data.frame(cmd,cmd_br=cmd_br,arg_str, exp, if_arg, in_arg, using, opts,txt, colon1, colon2,colon3, program, quietly)

  # Special treatment for outdated "for any" command in combi with regression. Like
  # for any y1 y2: reg X z1
  # We cant well handle those regressions and thus change the command name
  rows = which(startsWith(tab$colon1,"for any"))
  if (length(rows)>0) {
    regcmds = get.regcmds()
    rows = which(startsWith(tab$colon1,"for any") & tab$cmd %in% regcmds)
    tab$cmd[rows] = paste0("__for_any_", tab$cmd[rows])
  }


  tab = tab.repair.input.cmds(tab)

  if (is.null(orglines))
    tab$orgline = orglines
  tab = filter(tab, nchar(trimws(tab$txt))>0)
  tab$line = seq_len(NROW(tab))

  tab
}

# Translates command lines to tab structure
# can be called on re.df
# E.g. to extract data sets
# Assume there are no comments, no placeholders
# no Mata blocks.
repbox.re.cmdlines.to.tab = function(txt) {
  restore.point("repa.do.table")

  txt = merge.lines(txt)

  # Set quotes "" into ph
  pho = try(blocks.to.placeholder(txt, start=c('"'), end=c('"'), ph.prefix = "#~qu"))
  if (is(pho,"try-error")) {
    pho=stepwise.blocks.to.placeholder(txt, ph.df)
  }
  txt = pho$str; ph.df = pho$ph.df

  # Set brackets () into ph
  pho = try(blocks.to.placeholder(txt, start=c("("), end=c(")"), ph.prefix = "#~br",ph.df = ph.df))
  if (is(pho,"try-error")) {
    pho=stepwise.blocks.to.placeholder(txt, ph.df)
  }
  txt = pho$str; ph.df = pho$ph.df

  txt = sep.lines(txt)

  tab = normalized.cmdlines.to.tab(txt, ph.df)
  tab
}


# Commands like
# local x : var1
# global x : var1
# should have local or global as cmd not var1
tab.repair.colon.local = function(tab) {
  restore.point("tab.colon.local")
  if (NROW(tab)==0) return(tab)

  str = tab$colon1
  pos = stri_locate_first_regex(str, "(?<=(^|[ \t]))[ ]*(local|global)[ \t]")
  rows = which(!is.na(pos[,1]))
  if (length(rows)==0) return(tab)

  pos = pos[rows,,drop=FALSE]
  str = str[rows]
  lhs = stri_sub(str,1,pos[,1]-1) %>% trimws()
  cmd = stri_sub(str,pos[,1], pos[,2]) %>% trimws()
  rhs = stri_sub(str,pos[,2]+1) %>% trimws()

  tab$cmd[rows] = cmd
  tab$colon1[rows] = lhs
  tab$arg_str[rows] = paste0(rhs," ", tab$arg_str[rows]) %>% trimws()

  tab
}

# Stata input commands are nasty to parse.
# Consider the following valid do file code
#
# input str8 x
# hello
# list
# end
#
#
# input str4 z
# hi
# list
#
# The first input command is relatively nice, since it ends with "end"
# yet the second command generating z is also valid. It just takes as
# many inputs as there are rows in the data set before stopping
# the execution
# Whether list is then treated as a member of z or already the next
# command depends on whether data set in memory just has one observation
# or more.
tab.repair.input.cmds = function(tab) {
  restore.point("tab.repair.input.cmds")
  starts = which(tab$cmd %in% c("input","inp","inpu"))
  if (length(starts)==0) return(tab)
  # a list of all stata cmds with at least 3 letters
  cmds = readRDS(system.file("misc/cmd_list.Rds",package = "repboxStata"))
  tab$known_cmd = tab$cmd %in% cmds
  #ends = which(tab$cmd == "end")
  ignore = rep(FALSE,NROW(tab))
  for (s in starts) {
    # Find next row in tab that has a known proper Stata cmd
    # "end" is also included in cmds
    rows = which(tab$known_cmd & (1:NROW(tab))>s)
    if (length(rows)>0) {
      end = min(rows)
      if (tab$cmd[end]!="end") {
        end = end-1
      }
    } else {
      end = NROW(tab)
    }

    tab$txt[s] = paste0(tab$txt[s:end], collapse="\n")
    ignore[setdiff(s:end,s)] = TRUE
  }
  tab = tab[!ignore,]
  tab
}

tab.add.in.program = function(tab) {
  restore.point("tab.add.in.program")
  tab$in.program = rep(0,NROW(tab))
  starts = which(tab$cmd %in% c("program","prog","pr","progr") & !is.true(tab$cmd2 %in% c("drop","dir","list")))
  ends = which(tab$cmd == "end")
  for (s in starts) {


    end = min(ends[ends > s])
    tab$in.program[(s+1):(end-1)] = 1
    tab$in.program[s] = 2
    tab$in.program[end] = 3
  }
  tab
}

tab.add.block.end = function(tab) {
  restore.point("tab.add.in.block")
  tab$block_end_line = rep(NA_integer_,NROW(tab))
  starts = which(tab$opens_block)
  ends = which(tab$closes_block)

  ma = match.blocks.start.end(starts, ends)
  tab$block_end_line[starts] = ends[ma[,2]]
  tab
}


tab.add.in.loop = function(tab) {
  restore.point("tab.add.in.loop")
  tab$in_loop = rep(0, NROW(tab))
  starts = which(tab$cmd %in% c("foreach","forvalues","forv","forva","forval", "forvalu","forvalue", "while"))
  for (s in starts) {
    end_line = tab$block_end_line[s]
    if (is.na(end_line)) next
    tab$in_loop[s] = 2
    tab$in_loop[end_line] = 3
    if (end_line - s <= 1) next
    tab$in_loop[(s+1):(end_line-1)] = 1
  }
  tab
}

tab.replace.texdoc.do = function(tab) {
  restore.point("tab.replace.texdoc.do")
  rows = which(tab$cmd == "texdoc" & tab$cmd2 == "do")
  if (length(rows)==0) return(tab)

  tab$cmd[rows] = "do"
  tab$cmd2[rows] = NA_character_
  tab$arg_str[rows] = str.left.of(tab$arg_str[rows],",") %>% str.right.of("do") %>% trimws()
  tab$txt[rows] = paste0("do ", tab$arg_str[rows])
  tab

}
