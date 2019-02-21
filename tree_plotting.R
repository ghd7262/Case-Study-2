setwd("~/Desktop/workspace/math189/cs2/")
data <- read.table("videodata.txt", header=TRUE)

library(rpart)
library(rpart.plot)


data$dis_like<- rep(NA, dim(data)[1])

data = data[ which( data$work != 99) , ]
data = data[ which( data$educ != 99) , ]


for(i in 1:dim(data)[1]){
  like <- data[i, 'like']
  if(like==1 || like==4 || like==5 || like==3){
    data[i, 'dis_like'] = 0
  }else{
    data[i, 'dis_like'] = 1
  }
}

# Create a decision tree model
tree <- rpart(dis_like~educ+sex+age+home+math+work+own+cdrom+grade, data=data, cp=.02)
# Visualize the decision tree with rpart.plot
rpart.plot(tree, box.palette="RdBu", shadow.col="gray", nn=TRUE)
title("Classification Decision Tree on Liking Video Games", line=2.5)