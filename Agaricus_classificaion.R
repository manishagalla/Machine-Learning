agaricus.lepiota.data <- read.csv("/Users/manishagalla/Documents/R/agaricus-lepiota.data.txt", header=FALSE)

fields <- c("class",
            "cap_shape",
            "cap_surface",
            "cap_color",
            "bruises",
            "odor",
            "gill_attachment",
            "gill_spacing",
            "gill_size",
            "gill_color",
            "stalk_shape",
            "stalk_root",
            "stalk_surface_above_ring",
            "stalk_surface_below_ring",
            "stalk_color_above_ring",
            "stalk_color_below_ring",
            "veil_type",
            "veil_color",
            "ring_number",
            "ring_type",
            "spore_print_color",
            "population",
            "habitat")
colnames(agaricus.lepiota.data) <- fields
head(agaricus.lepiota.data)


myData=agaricus.lepiota.data

#Data cleaning part.Removing missing values, dropping the levels (in each column) which has fewer number of instances.
myData = myData[myData$stalk_root != '?',]
table(myData$stalk_root)
myData$stalk_root = droplevels(myData$stalk_root)
table(myData$stalk_root)

myData$veil_type <- NULL

myData$gill_attachment= droplevels(myData$gill_attachment)
myData$gill_attachment <- NULL
myData$veil_color <- NULL


myData = myData[myData$cap_shape != 'c',]
table(myData$cap_shape)
myData$cap_shape = droplevels(myData$cap_shape)
table(myData$cap_shape)

table(myData$cap_surface)
myData = myData[myData$cap_surface != 'g',]
myData$cap_surface = droplevels(myData$cap_surface)
table(myData$cap_surface)

table(myData$cap_color)
myData = myData[myData$cap_color != 'c',]
myData = myData[myData$cap_color != 'r',]
myData = myData[myData$cap_color != 'u',]
myData$cap_color = droplevels(myData$cap_color)
table(myData$cap_color)

table(myData$odor)
myData = myData[myData$odor != 'm',]
myData$odor = droplevels(myData$odor)
table(myData$odor)

table(myData$gill_color)
myData = myData[myData$gill_color != 'r',]
myData = myData[myData$gill_color != 'y',]
myData$gill_color = droplevels(myData$gill_color)
table(myData$gill_color)

table(myData$stalk_surface_above_ring)
myData = myData[myData$stalk_surface_above_ring != 'y',]
table(myData$stalk_surface_above_ring)
myData$stalk_surface_above_ring = droplevels(myData$stalk_surface_above_ring)
table(myData$stalk_surface_above_ring)

table(myData$stalk_color_above_ring)
myData$stalk_color_above_ring = droplevels(myData$stalk_color_above_ring)
table(myData$stalk_color_above_ring)

table(myData$stalk_color_below_ring)
myData$stalk_color_below_ring= droplevels(myData$stalk_color_below_ring)
table(myData$stalk_color_below_ring)

table(myData$ring_number)
myData$ring_number= droplevels(myData$ring_number)
table(myData$ring_number)

table(myData$ring_type)
myData = myData[myData$ring_type != 'f',]
myData$ring_type=droplevels(myData$ring_type)
table(myData$ring_type)

table(myData$habitat)
myData$habitat= droplevels(myData$habitat)
table(myData$habitat)

table(myData$spore_print_color)
myData$spore_print_color= droplevels(myData$spore_print_color)
table(myData$spore_print_color)

table(myData$cap_shape)
myData = myData[myData$cap_shape != 'k',]
myData$cap_shape = droplevels(myData$cap_shape)
table(myData$cap_shape)

table(myData$population)
myData = myData[myData$population != 'c',]
myData$population = droplevels(myData$population)
table(myData$population)

train = sample(5512,4135)#splitting the data train 75% and 25% for testing
#Random Forest
bag.mushroom = randomForest(class~., data=myData,subset=train,mtry=19,importance=TRUE)
bag.muhsroom.pred = predict(bag.mushroom,myData[-train,],type="class")
table(bag.muhsroom.pred,myData[-train,]$class)

#Random Forest with 6 predictors
rf.mushroom.six = randomForest(class~., data=myData,subset=train,mtry=6,importance=TRUE)
rf.mush.pred.six = predict(rf.mushroom.six,myData[-train,],type="class")
table(rf.mush.pred.six,myData[-train,]$class)

