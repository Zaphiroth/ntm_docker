
library(plumber)

pr <- plumb("./Reports.R")

pr$run(host = '0.0.0.0', port = 8000)

# http://127.0.0.1:8000/ntm/5c7ce8b1421aa9907926eb71/5c4552455ee2dd7c36a94a9e