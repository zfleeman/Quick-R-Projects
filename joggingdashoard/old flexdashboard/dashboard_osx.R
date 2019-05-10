library(RCurl)
library(rmarkdown)
render("~/Dropbox/R/joggingdash.Rmd", output_file = "~/Dropbox/R/joggingdash.html")
ftpUpload("~/Dropbox/R/joggingdash.html", "ftp://zfleeman:_____PW_____@zfleeman.com/~/www/zfleeman.com/jogging.html")
