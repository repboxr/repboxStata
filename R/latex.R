example = function() {
  project_dir = "/home/rstudio/statabox/supp/restud_88_1_8"
  compile.project.tex.fragments(project_dir)


  project_dir = "~/statabox/supp/testsupp"
  compile.project.tex.fragments(project_dir)



  project_dir = "~/repbox/projects/friberg"
  sup.dir = "~/repbox/projects/friberg/mod"
  dir = "~/repbox/projects/friberg/repbox"

  tex.files = fragment.files =  find.tex.fragments(dir)
  tex.file = tex.files[1]

  file.copy(tex.file, "~/repbox/test.tex")

  compile.tex.fragment("~/repbox/test.tex")
  setwd(dirname(first(tex.files)))
  getwd()

  rstudioapi::filesPaneNavigate(getwd())
  #rstudioapi::filesPaneNavigate("~/repbox/repbox/R")

  tex.file = tex.files[1]
  compile.tex.fragment(tex.file, ignore.full.doc = FALSE)
  combine.tex.fragments.to.pdf(tex.files)
}

compile.project.tex.fragments = function(project_dir, make.png=FALSE, ignore.full.doc=FALSE) {
  sup.dir = file.path(project_dir,"mod")
  fragment.files =  find.tex.fragments(sup.dir)
  for (f in fragment.files) {
    try(compile.tex.fragment(f, make.png=make.png, ignore.full.doc=ignore.full.doc))
  }
}

find.tex.fragments = function(dir) {
  tex.files = list.files(dir, glob2rx("*.tex"),full.names = TRUE, recursive = TRUE)
}


compile.tex.fragment = function(fragment.file,pdf.file=gsub("tex$", "pdf", fragment.file), png.file = gsub("pdf$", "png", pdf.file), make.png=TRUE, ignore.full.doc=TRUE, delete.pdf = FALSE) {
  restore.point("compile.tex.fragment")
  library(tinytex)
  tex = merge.lines(readLines(fragment.file, warn=FALSE))

  use.booktabs = has.substr(tex,"toprule") | has.substr(tex,"bottomrule") |
    has.substr(tex,"midrule")

  is.fragment = has.substr(tex, "&") & !has.substr(tex,"tabular")
#  \\newcommand{\\sym}[1]{#1}

  if (!is.fragment) {
    txt = paste0('
\\documentclass[preview, border={10pt 10pt 10pt 10pt}, varwidth=\\maxdimen]{standalone}
', if(use.booktabs) '\\usepackage{booktabs}\n',
                 '\\begin{document}
\\thispagestyle{empty}
\\renewcommand\\thetable{}
',tex,'
\\end{document}')

  } else if (is.fragment) {
    txt = paste0('
\\documentclass[preview, border={10pt 10pt 10pt 10pt}, varwidth=\\maxdimen]{standalone}
', if(use.booktabs) '\\usepackage{booktabs}\n',
'\\begin{document}
\\thispagestyle{empty}
\\def\\sym#1{\\ifmmode^{#1}\\else\\(^{#1}\\)\\fi}
\\begin{tabular}{*{40}{l}}
',tex,'
\\end{tabular}
\\end{document}')

  }

  if (has.substr(tex,"\\begin{document")) {
    if (ignore.full.doc) {
      cat("\n", fragment.file," looks like a complete tex document and is ignored.")
      return(invisible())
    } else {
      txt = tex
    }
  }


  tempfile = tempfile("latex_",fileext=".tex")
  writeLines(txt, tempfile)
  tinytex::pdflatex(tempfile,pdf_file=pdf.file)

  #svg.file = gsub("pdf$", "svg", pdf.file)
  if (make.png) {
    library(magick)
    library(pdftools)
    #pdftools::pdf_convert(pdf.file, dpi=72, pages=1, filenames=png.file)
    # We had the case that an error occurs in magick
    # that breaks everything
    try({
      img = image_read_pdf(pdf.file)
      png = magick::image_convert(img,format = "png")
      image_write(png,png.file)
    })
    #svg = magick::image_convert(img, format= "svg")
    #image_write(svg,svg.file)
  }
  if (delete.pdf) {
    file.remove(pdf.file)
  }
}


combine.tex.fragments.to.pdf = function(fragment.files, out.tex="repbox_latex_fragments.tex", out.pdf = gsub("tex$", "pdf", out.tex)) {
  restore.point("combine.tex.fragments.to.pdf")
  txt.li = lapply(fragment.files,function(file) {
    merge.lines(readLines(file, warn=FALSE))
  })

  is.fragment = sapply(txt.li, function(str) {
    !has.substr(str,"\\begin{document")
  })
  txt.li = txt.li[is.fragment]
  fragment.files = fragment.files[is.fragment]


  library(tinytex)
  tex = paste0('
\\documentclass{article}
\\begin{document}
\\renewcommand\\thetable{}
',
paste0('
\\verb|',fragment.files,'|
',txt.li, collapse="\n"),'
\\end{document}')
  writeLines(tex, out.tex)
  suppressWarnings(tinytex::pdflatex(out.tex,out.pdf))
  #tinytex::xelatex(out.tex,out.pdf)

}
