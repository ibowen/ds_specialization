# question 2
install.packages("pryr")
library(pryr)
ftype(mean)
ftype(predict)
ftype(show)
ftype(lm)
ftype(colSums)
ftype(dgamma)

# question 3
showMethods("show")
getMethod(show)

# question 4
download.file("https://d396qusza40orc.cloudfront.net/devdataprod/DDPQuiz3_1.0.zip", destfile = "quiz3_1.0.zip")
unzip("./quiz3_1.0.zip")
dir()
