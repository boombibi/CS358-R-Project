
### Reade file .CSV
mushroomData <- read.csv("C:/Users/tsb/Desktop/R Project/mushroom-classification/mushrooms.csv");

### Structure dataset
str(mushroomData)

### Assign new attribute name
colnames(mushroomData) <- c("edibility", "cap_shape", "cap_surface",
                              + "cap_color", "bruises", "odor",
                              + "gill_attachement", "gill_spacing", "gill_size",
                              + "gill_color", "stalk_shape", "stalk_root",
                              + "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring",
                              + "stalk_color_below_ring", "veil_type", "veil_color",
                              + "ring_number", "ring_type", "spore_print_color",
                              + "population", "habitat")

### Level new attribue factor
levels(mushroomData$edibility) <- c("edible", "poisonous")
levels(mushroomData$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushroomData$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(mushroomData$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushroomData$bruises) <- c("no", "yes")
levels(mushroomData$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushroomData$gill_attachement) <- c("attached", "free")
levels(mushroomData$gill_spacing) <- c("close", "crowded")
levels(mushroomData$gill_size) <- c("broad", "narrow")
levels(mushroomData$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow")
levels(mushroomData$stalk_shape) <- c("enlarging", "tapering")
levels(mushroomData$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushroomData$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroomData$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushroomData$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(mushroomData$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow")
levels(mushroomData$veil_type) <- "partial"
levels(mushroomData$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushroomData$ring_number) <- c("none", "one", "two")
levels(mushroomData$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushroomData$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow")
levels(mushroomData$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushroomData$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

### Partitition Data - Use partitionData function in Function.R
set.seed(1420)
ParttionnedData <- partitionData(mushroomData)
trainingData <- ParttionnedData$trainingData
testingData <- partitionedData$testingData

### Check prop after partition data
round(prop.table(table(testingData$edibility)), 2)
round(prop.table(table(trainingData$edibility)), 2)
round(prop.table(table(mushroomData$edibility)), 2)

### Information Gain of each Attribute
InformationGain(table(trainingData[,c('habitat','edibility')]))
InformationGain(table(trainingData[,c('population','edibility')]))
InformationGain(table(trainingData[,c('spore_print_color','edibility')]))
InformationGain(table(trainingData[,c('ring_type','edibility')]))
InformationGain(table(trainingData[,c('ring_number','edibility')]))
InformationGain(table(trainingData[,c('veil_color','edibility')]))
InformationGain(table(trainingData[,c('veil_type','edibility')]))
InformationGain(table(trainingData[,c('stalk_color_below_ring','edibility')]))
InformationGain(table(trainingData[,c('stalk_color_above_ring','edibility')]))
InformationGain(table(trainingData[,c('stalk_surface_below_ring','edibility')]))
InformationGain(table(trainingData[,c('stalk_surface_above_ring','edibility')]))
InformationGain(table(trainingData[,c('stalk_root','edibility')]))
InformationGain(table(trainingData[,c('stalk_shape','edibility')]))
InformationGain(table(trainingData[,c('gill_color','edibility')]))
InformationGain(table(trainingData[,c('gill_size','edibility')]))
InformationGain(table(trainingData[,c('gill_spacing','edibility')]))
InformationGain(table(trainingData[,c('gill_attachement','edibility')]))
InformationGain(table(trainingData[,c('odor','edibility')]))
InformationGain(table(trainingData[,c('bruises','edibility')]))
InformationGain(table(trainingData[,c('cap_color','edibility')]))
InformationGain(table(trainingData[,c('cap_surface','edibility')]))
InformationGain(table(trainingData[,c('cap_shape','edibility')]))

### Attribute Visualization Odor
ggplot(mushroom, aes(x = edibility, y = odor, col = edibility)) geom_jitter(alpha = 0.5) +
  + scale_color_manual(breaks = c("edible", "poisonous"),
                       + values = c("green", "orange"))

### Attribute visualization Odor-Spore print color
ggplot(mushroom, aes(x = spore_print_color , y = odor, col = edibility)) +
  + geom_jitter(alpha = 0.5) +
  + scale_color_manual(breaks = c("edible", "poisonous"),
                       + values = c("green", "orange"))

### Create Tree model
tree_model <- rpart(edibility ~ ., data = trainingData,method = "class", cp = 0.0001)

### Print CP of Tree model
printcp(tree_model)

### Plot tree model
rpart.plot(tree_model, extra = 105, box.palette = "Red", branch.lty = 3, shadow.col = "gray", nn = TRUE)

### Create tree test model
tree_test <- predict(tree_model, newdata = testingData)

### Calculate cross-tabulation of trainingData 
caret::confusionMatrix(data=predict(tree_model, type = "class"),
                       + reference = trainingData$edibility,
                       + positive="edible")

### Calculate cross-tabulation of testingData
caret::confusionMatrix(data = predict(tree_model, newdata = testingData, type = "class"),
                       + reference = testingData$edibility,
                       + positive = "edible")
