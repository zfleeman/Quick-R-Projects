library(RCurl)
library(rmarkdown)

render("C:/Users/zfleeman/Dropbox/R/joggingdash.Rmd", output_file = "C:/Users/zfleeman/Dropbox/R/joggingdash.html")

ftpUpload("C:/Users/zfleeman/Dropbox/R/joggingdash.html", "ftp://zfleeman:______PW_______@zfleeman.com/~/www/zfleeman.com/jogging.html")
