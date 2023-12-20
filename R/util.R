add.col = function(x, name, val) {
  x[[name]] = rep(val, NROW(x))
  x
}



shorten.str = function(str,nchar=30) {
  restore.point("shorten.str")
  rows = nchar(str)>nchar
  str[rows] = paste0(substring(str[rows],1,nchar-4)," ...")
  str
}

examples = function() {
  time.diff("15:21:00","14:23:00")
}

readRDS.or.null = function(file) {
  if (!file.exists(file)) return(NULL)
  readRDS(file)
}

create.dir = function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
}

clear.and.create.dir = function(clear.dir,recursive=TRUE) {
  restore.point("clear.and.create.dir")
  if (dir.exists(clear.dir)) {
    unlink(clear.dir, recursive=TRUE)
  }
  dir.create(clear.dir, recursive = TRUE)
}

is.true.vec = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}

seconds.from.midnight = function(time.str) {
  h = as.integer(substring(time.str,1,2))
  m = as.integer(substring(time.str,4,5))
  s = as.integer(substring(time.str,7,8))
  h*3600+m*60+s
}

find.line.starts.blocks = function(txt, start, end) {
  restore.point("reove.include.output.from.log")
  start.lines = which(startsWith(txt,start))
  if (length(start.lines)==0) return(NULL)
  end.lines = which(startsWith(txt,end))

  start.id = str.right.of(txt[start.lines], start)
  end.id = str.right.of(txt[end.lines], end)

  end.pos = match(start.id, end.id)

  tibble(start.line = start.lines, end.line = end.lines[end.pos], arg=start.id)
}

time.diff = function(stime, etime) {
  restore.point("time.diff")
  ssec = seconds.from.midnight(stime)
  esec = seconds.from.midnight(etime)
  diff = esec-ssec
  rows = is.true(diff<0)
  diff[rows] = 86400+diff[rows]
  diff
}

has.col = function(x, col) {
  col %in% names(x)
}

deparse1 = function (call, collapse = "") {
  paste0(deparse(call, width = 500), collapse = collapse)
}

is.true = function(x) {
  val = x == TRUE
  val[is.na(x)] = FALSE
  val
}



trimws_around = function(str, around, whitespace="[\t ]*") {
  pattern = paste0(whitespace,around,whitespace)
  str = gsub(pattern, around, str)
  str
}

no.finite.val = function(x, val) {
  x[!is.finite(x)] = val
  x
}

na.false = function(x) {
  x[is.na(x)] = FALSE
  x
}

na.val = function(x, na.val=0) {
  x[is.na(x)] = na.val
  x
}

is_empty = function(x) {
  if (is.null(x) | all(is.na(x))) return(TRUE)
  if (isTRUE(x=="")) return(TRUE)
  return(FALSE)
}


paste.df.cols = function (mat, cols = 1:NCOL(mat),sep="", empty.sep=FALSE, ...) {
  restore.point("paste.df.cols")
  if (NROW(cols) == 2) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]], ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],sep=sep, ...))
    }
  } else if (NROW(cols) == 3) {
    if (empty.sep) {
      sep1 = ifelse(!empty.sep | nchar(mat[[cols[1]]])>0 | nchar(mat[[cols[2]]])>0, sep,"")
      sep2 = ifelse(!empty.sep | nchar(mat[[cols[2]]])>0 | nchar(mat[[cols[3]]])>0, sep,"")
      return(paste0(mat[[cols[1]]],sep1, mat[[cols[2]]],sep2, mat[[cols[3]]],
                    ...))
    } else {
      return(paste(mat[[cols[1]]],mat[[cols[2]]],mat[[cols[3]]],sep=sep, ...))
    }
  } else {
    if (is.character(cols))
      cols = match(cols, colnames(mat))
    code = paste("mat[[", cols, "]]", collapse = ",sep,")
    code = paste("paste0(", code, ",...)", sep = "")
    return(eval(parse(text = code)))
  }
}

copy.dir = function(from, to, ...) {
  if (!dir.exists(to)) dir.create(to, recursive = TRUE)
  res = file.copy(list.files(from, full.names = TRUE), to, recursive = TRUE, ...)
  invisible(all(res))
}

as.path.in.project_dir = function(rel.path,wdir=getwd(), project_dir) {
  abs.path = getAbsolutePath(rel.path, wdir, expandTilde=TRUE)
  project_dir = getAbsolutePath(project_dir, wdir, expandTilde=TRUE)

  res.path = str.right.of(abs.path,project_dir)

  if (startsWith(res.path,"/mod/")) {
    res.path = substring(res.path, 6)
  } else if (startsWith(res.path,"/org/")) {
    res.path = substring(res.path, 6)
  } else if (startsWith(res.path, "/")) {
    res.path = substring(res.path, 2)
  }
  res.path

}

match.blocks.start.end = function(start, end) {
  restore.point("match.blocks.start.end")

  end_pos = start_stack = rep(NA, length(start))
  start_stack_ind = 1
  start.i = 1
  end.i= 1
  start_stack[1] = 1
  start = c(start, Inf)
  while (TRUE) {
    top_ind = start_stack[start_stack_ind]

    # Add next start.i to start stack
    start.i = start.i+1

    # Try to clear start_stack
    while (end[end.i]<start[start.i] & end.i <= length(end)) {
      if (start[top_ind]>end[end.i]) {
        warning(paste0("A block closes in position (line) ", end[end.i], " but there is no open block."))
        end.i = end.i +1
        next
      }

      end_pos[top_ind] = end.i
      start_stack_ind = start_stack_ind-1
      end.i = end.i+1

      #cat("\ndel start_stack: ", paste0(start_stack[1:start_stack_ind],
      #"(",start[start_stack[1:start_stack_ind]],")"))

      if (start_stack_ind == 0) break

      top_ind = start_stack[start_stack_ind]
    }

    if (start.i >= length(start)) break

    start_stack_ind = start_stack_ind+1
    start_stack[start_stack_ind] = start.i
    #cat("\nadd start_stack: ", paste0(start_stack[1:start_stack_ind],
    #  "(",start[start_stack[1:start_stack_ind]],")"))

  }
  cbind(start_ind=seq_along(start[-length(start)]), end_ind=end_pos)
}

