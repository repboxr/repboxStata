file.copy("~/repbox/repboxStata/inst/misc/ado/r/reghdfe.ado","/home/rstudio/ado/plus/r")


url = "http://www.trfetzer.com/wp-content/uploads/reg2hdfespatial-id1id2.ado"
download.file(url, "~/repbox/repboxStata/inst/misc/ado/reg2hdfespatial.ado")
file.copy("~/repbox/repboxStata/inst/misc/ado/reg2hdfespatial.ado","/home/rstudio/ado/plus/r")

url = "https://economics.uwo.ca/people/conley_docs/data_GMMWithCross_99/x_ols.ado"
download.file(url, "~/repbox/repboxStata/inst/misc/ado/x_ols.ado")
file.copy("~/repbox/repboxStata/inst/misc/ado/x_ols.ado","/home/rstudio/ado/plus/x")

url = "https://eml.berkeley.edu/~jmccrary/DCdensity/DCdensity.ado"
download.file(url, "~/repbox/repboxStata/inst/misc/ado/DCdensity.ado")
file.copy("~/repbox/repboxStata/inst/misc/ado/DCdensity.ado","/home/rstudio/ado/plus/d")


tempdir = tempdir()
url = "https://ifs.org.uk/sites/default/files/output_url_files/bootwildct.zip"
download.file(url, file.path(tempdir,"bootwildct.zip"))
unzip(file.path(tempdir,"bootwildct.zip"),exdir = tempdir)
list.files(file.path(tempdir,"Web Upload"))
file.copy(file.path(tempdir,"Web Upload","bootwildct.ado"),"/home/rstudio/ado/plus/b")

