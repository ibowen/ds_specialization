library(devtools)
install_github('slidify', 'ramnathv')
install_github('slidifyLibraries', 'ramnathv')
library(slidify)
author("Bowen Liu")
slidify("./Bowen Liu/index.Rmd")
publish(user="ibowen", repo="https://github.com/ibowen/data-prodct.git", host = "github")
publish(title = 'mytitle', 'index.html', host = 'rpubs')

