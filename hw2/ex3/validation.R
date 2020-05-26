path   <- "/home/arnau/MIRI/SMDE/hw2"
system <- read.csv(paste(path, "ex1/dataset.csv", sep="/"),header=TRUE,sep=",")$answer
model  <- read.csv(paste(path, "ex4/model_data.csv", sep="/"),header=TRUE,sep=",")
t.test(system_small, model)