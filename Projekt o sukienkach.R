### projekt o sukienkach
library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(readxl)
library(InformationValue)
library(randomForest)
library(MASS)

# wczytanie danych
dane_surowe <- read_xlsx("/Users/adekkiezun/Downloads/Attribute DataSet.xlsx", col_names = TRUE)

##### CZYSZCZENIE DANYCH ######

# usuniecie kolumny ID
dane <- dane_surowe[,-1]

# zamiana NA na null
for(i in 1:ncol(dane)){
  for(j in 1:nrow(dane)){
    if(is.na(dane[j,i])){
      dane[j,i] <- "null"
    }else{
      dane[j,i] <- dane[j,i]
    }
  }
}

# usuwanie powtorzen
unique(dane$Style)
for(i in 1:nrow(dane)){
  if(dane[i,1] == "sexy"){
    dane[i,1] <- "Sexy"
  }
}
unique(dane$Price)
for(i in 1:nrow(dane)){
  if(dane[i,2] == "low"){
    dane[i,2] <- "Low"
  }
  if(dane[i,2] == "high"){
    dane[i,2] <- "High"
  }
}

unique(dane$Rating)

unique(dane$Size)
for(i in 1:nrow(dane)){
  if(dane[i,4] == "s"){
    dane[i,4] <- "S"
  }
  if(dane[i,4] == "small"){
    dane[i,4] <- "S"
  }
}
unique(dane$Season)
for(i in 1:nrow(dane)){
  if(dane[i,5] == "spring"){
    dane[i,5] <- "Spring"
  }
  if(dane[i,5] == "winter"){
    dane[i,5] <- "Winter"
  }
  if(dane[i,5] == "summer"){
    dane[i,5] <- "Summer"
  }
  if(dane[i,5] == "Automn"){
    dane[i,5] <- "Autumn"
  }
}

unique(dane$NeckLine)
for(i in 1:nrow(dane)){
  if(dane[i,6] == "peterpan-collor"){
    dane[i,6] <- "peterpan-collar"
  }
  if(dane[i,6] == "turndowncollor"){
    dane[i,6] <- "turndowncollar"
  }
  if(dane[i,6] == "mandarin-collor"){
    dane[i,6] <- "mandarin-collar"
  }
  if(dane[i,6] == "sqare-collor"){
    dane[i,6] <- "sqare-collar"
  }
  if(dane[i,6] == "Sweetheart"){
    dane[i,6] <- "sweetheart"
  }
  if(dane[i,6] == "NULL"){
    dane[i,6] <- "null"
  }
  if(dane[i,6] == "sqare-collar"){
    dane[i,6] <- "square-collar"
  }
}

unique(dane$SleeveLength)
for(i in 1:nrow(dane)){
  if(dane[i,7] == "sleevless"){
    dane[i,7] <- "sleeveless"
  }
  if(dane[i,7] == "sleeevless"){
    dane[i,7] <- "sleeveless"
  }
  if(dane[i,7] == "sleveless"){
    dane[i,7] <- "sleeveless"
  }
  if(dane[i,7] == "cap-sleeves"){
    dane[i,7] <- "capsleeves"
  }
  if(dane[i,7] == "threequater"){
    dane[i,7] <- "threequarter"
  }
  if(dane[i,7] == "thressqatar"){
    dane[i,7] <- "threequarter"
  }
  if(dane[i,7] == "half"){
    dane[i,7] <- "halfsleeve"
  }
  if(dane[i,7] == "turndowncollor"){
    dane[i,7] <- "turndowncollar"
  }
  if(dane[i,7] == "urndowncollor"){
    dane[i,7] <- "turndowncollar"
  }
  if(dane[i,7] == "NULL"){
    dane[i,7] <- "null"
  }
}

unique(dane$waiseline)
unique(dane$Material)
for(i in 1:nrow(dane)){
  if(dane[i,9] == "sill"){
    dane[i,9] <- "silk"
  }
  if(dane[i,9] == "model"){
    dane[i,9] <- "modal"
  }
  if(dane[i,9] == "other"){
    dane[i,9] <- "null"
  }
  if(dane[i,9] == "shiffon"){
    dane[i,9] <- "chiffonfabric"
  }
}
unique(dane$FabricType)
for(i in 1:nrow(dane)){
  if(dane[i,10] == "other"){
    dane[i,10] <- "null"
  }
  if(dane[i,10] == "sattin"){
    dane[i,10] <- "satin"
  }
  if(dane[i,10] == "wollen"){
    dane[i,10] <- "woolen"
  }
  if(dane[i,10] == "shiffon"){
    dane[i,10] <- "chiffon"
  }
  if(dane[i,10] == "flannael"){
    dane[i,10] <- "flannel"
  }
  if(dane[i,10] == "knitting"){
    dane[i,10] <- "knitted"
  }
}

unique(dane$Decoration)
for(i in 1:nrow(dane)){
  if(dane[i,11] == "none" | dane[i,11] == "null"){
    dane[i,11] <- "no"
  }else{
    dane[i,11] <- "yes"
  }
}

unique(dane$`Pattern Type`)
for(i in 1:nrow(dane)){
  if(dane[i,12] == "leapord"){
    dane[i,12] <- "animal"
  }
  if(dane[i,12] == "leopard"){
    dane[i,12] <- "animal"
  }
  if(dane[i,12] == "solid"){
    dane[i,12] <- "none"
  }
  if(dane[i,12] == "null"){
    dane[i,12] <- "none"
  }
  if(dane[i,12] == "splice"){
    dane[i,12] <- "patchwork"
  }
  if(dane[i,12] == "character"){
    dane[i,12] <- "print"
  }
}

#####
#grupowanie zmiennych
#####

# Style
for(i in 1:nrow(dane)){
  if(dane[i,1] == "fashion" | dane[i,1] == "OL"){
    dane[i,1] <- "1"
  }
  if(dane[i,1] == "work" | dane[i,1] == "Brief"){
    dane[i,1] <- "1"
  }
  if(dane[i,1] == "vintage" | dane[i,1] == "Novelty" | dane[i,1] == "Casual" | dane[i,1] == "Sexy"){
    dane[i,1] <- "2"
  }
  if(dane[i,1] == "cute" | dane[i,1] == "Flare" | dane[i,1] == "bohemian"){
    dane[i,1] <- "3"
  }
  if(dane[i,1] == "party"){
    dane[i,1] <- "4"
  }
}

# Price
for(i in 1:nrow(dane)){
  if(dane[i,2] == "very-high" | dane[i,2] == "null"){
    dane[i,2] <- "very-high/null"
  }
}

# Rating - nic

# Size 
for(i in 1:nrow(dane)){
  if(dane[i,4] == "XL" | dane[i,4] == "S"){
    dane[i,4] <- "S/XL"
  }
}

# Season
for(i in 1:nrow(dane)){
  if(dane[i,5] == "null" | dane[i,5] == "Spring"){
    dane[i,5] <- "Spring/null"
  }
}

# NekcLine
for(i in 1:nrow(dane)){
  if(dane[i,6] == "backless" | dane[i,6] == "mandarin-collar" | dane[i,6] == "peterpan-collar" | dane[i,6] == "bowneck" | dane[i,6] == "square-collar" | dane[i,6] == "turndowncollar" | dane[i,6] == "open"){
    dane[i,6] <- "1"
  }
  if(dane[i,6] == "o-neck" | dane[i,6] == "slash-neck"){
    dane[i,6] <- "2"
  }
  if(dane[i,6] == "v-neck" | dane[i,6] == "Scoop"){
    dane[i,6] <- "3"
  }
  if(dane[i,6] == "null" | dane[i,6] == "boat-neck" | dane[i,6] == "sweetheart" | dane[i,6] == "halter" | dane[i,6] == "ruffled"){
    dane[i,6] <- "4"
  }
}

# sleevelength
for(i in 1:nrow(dane)){
  if(dane[i,7] == "butterfly" | dane[i,7] == "Petal" | dane[i,7] == "short"){
    dane[i,7] <- "1"
  }
  if(dane[i,7] == "threequarter" | dane[i,7] == "halfsleeve"){
    dane[i,7] <- "1"
  }
  if(dane[i,7] == "sleeveless" | dane[i,7] == "null" | dane[i,7] == "turndowncollar" | dane[i,7] == "full" | dane[i,7] == "capsleeves"){
    dane[i,7] <- "2"
  }
}

# waiseline
for(i in 1:nrow(dane)){
  if(dane[i,8] == "null" | dane[i,8] == "dropped" | dane[i,8] == "princess"){
    dane[i,8] <- "other"
  }
}
# material
for(i in 1:nrow(dane)){
  if(dane[i,9] == "acrylic" | dane[i,9] == "knitting" | dane[i,9] == "lace" | dane[i,9] == "linen" | dane[i,9] == "wool"){
    dane[i,9] <- "1"
  }
  if(dane[i,9] == "milksilk" | dane[i,9] == "polyster" | dane[i,9] == "chiffonfabric" | dane[i,9] == "lycra" | dane[i,9] == "microfiber" | dane[i,9] == "mix"){
    dane[i,9] <- "1"
  }
  if(dane[i,9] == "spandex" | dane[i,9] == "cotton" | dane[i,9] == "silk" | dane[i,9] == "modal" | dane[i,9] == "nylon" | dane[i,9] == "viscos" | dane[i,9] == "null"){
    dane[i,9] <- "2"
  }
  if(dane[i,9] == "rayon" | dane[i,9] == "cashmere"){
    dane[i,9] <- "2"
  }
}
# FabricType
for(i in 1:nrow(dane)){
  if(dane[i,10] == "Corduroy" | dane[i,10] == "flannel" | dane[i,10] == "broadcloth"){
    dane[i,10] <- "1"
  }
  if(dane[i,10] == "null" | dane[i,10] == "chiffon"){
    dane[i,10] <- "2"
  }
  if(dane[i,10] == "worsted" | dane[i,10] == "dobby" | dane[i,10] == "knitted" | dane[i,10] == "tulle" | dane[i,10] == "satin" | dane[i,10] == "jersey"){
    dane[i,10] <- "3"
  }
  if(dane[i,10] == "woolen" | dane[i,10] == "batik" | dane[i,10] == "lace" | dane[i,10] == "organza" | dane[i,10] == "poplin" | dane[i,10] == "terry"){
    dane[i,10] <- "3"
  }
}

# `Pattern Type`
for(i in 1:nrow(dane)){
  if(dane[i,12] == "floral" | dane[i,12] == "plaid" | dane[i,12] == "geometric"| dane[i,12] == "striped"){
    dane[i,12] <- "1"
  }
  if(dane[i,12] == "print" | dane[i,12] == "patchwork" | dane[i,12] == "animal"){
    dane[i,12] <- "2"
  }
  if(dane[i,12] == "none" | dane[i,12] == "dot"){
    dane[i,12] <- "3"
  }
}


# Zamienienie danych na czynniki i zmiany nazw kolumn

dane <- dane %>%
  mutate(Recommendation = as.factor(Recommendation),
         Style = as.factor(Style),
         Price = as.factor(Price),
         Rating = as.numeric(Rating),
         Size = as.factor(Size),
         Season = as.factor(Season),
         NeckLine = as.factor(NeckLine),
         SleeveLength = as.factor(SleeveLength),
         waiseline = as.factor(waiseline),
         Material = as.factor(Material),
         FabricType = as.factor(FabricType),
         Decoration = as.factor(Decoration),
         `Pattern Type` = as.factor(`Pattern Type`)) %>%
  rename(Waistline = waiseline,
         PatternType = `Pattern Type`)



#### Rozklady zmiennych do grupowania wartosci


dane_wykres <- dane %>%
  group_by(dane[,c(12)])  %>%
  summarize(Liczba = n())
dane_wykres_klasy <- dane %>%
  group_by(dane[,c(12,13)]) %>%
  summarize(liczba_klasy = n())
data <- inner_join(dane_wykres_klasy, dane_wykres)
data <- data %>%
  arrange(desc(Recommendation)) %>%
  distinct(`Pattern Type`, .keep_all = TRUE) %>%
  mutate(odsetek_1 = if(Recommendation == 1){
    liczba_klasy/Liczba
  }else{
    1-liczba_klasy/Liczba
  }) %>%
  select(-Recommendation) %>%
  arrange(odsetek_1)

ggplot(data) +
  geom_bar(aes(x=`Pattern Type`, y = Liczba), stat = "identity") +
  geom_line(aes(x=(1:3), y = odsetek_1*200), col = "red") +
  scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Odsetek klasy pozytywnej")) +
  theme_bw(base_size = 12)

data$Style <- factor(data$Style, levels = c("fashion", "OL", "work", "Brief", "vintage", "Novelty", "Casual", "Sexy", "cute", "Flare", "bohemian", "party"))
data$Price <- factor(data$Price, levels = c("Average", "Low", "High", "Medium", "very-high/null"))
data$Size <- factor(data$Size, levels = c("S/XL", "L", "M", "free"))
data$Season <- factor(data$Season, levels = c("Autumn", "Summer", "Winter", "Spring/null"))
data$NeckLine <- factor(data$NeckLine, levels = c("backless", "mandarin-collar", "peterpan-collar", "bowneck", "square-collar", "turndowncollar", "open", "o-neck", "slash-neck", "v-neck", "Scoop", "null", "boat-neck", "sweetheart", "halter", "ruffled"))
data$SleeveLength <- factor(data$SleeveLength, levels = c("butterfly", "Petal", "short", "threequarter", "halfsleeve", "sleeveless", "null", "turndowncollar", "full", "capsleeves"))
data$Material <- factor(data$Material, levels = c("acrylic", "knitting", "lace", "linen", "wool", "milksilk", "polyster", "chiffonfabric", "lycra", "microfiber", "mix", "spandex", "cotton", "silk", "modal", "nylon", "viscos", "null", "rayon", "cashmere"))
data$FabricType <- factor(data$FabricType, levels = c("Corduroy", "flannel", "broadcloth", "null", "chiffon", "worsted", "dobby", "knitted", "tulle", "satin", "jersey", "woolen", "batik", "lace", "organza", "poplin", "terry"))
data$waiseline <- factor(data$waiseline, levels = c("natural", "empire", "null", "dropped", "princess"))
data$`Pattern Type` <- factor(data$`Pattern Type`, levels = c("floral", "plaid", "geometric", "striped", "print", "patchwork", "animal", "none", "dot"))

# Sila zmiennych
IV(dane_trening$Style, dane_trening$Recommendation)
IV(dane_trening$Price, dane_trening$Recommendation)
IV(dane_trening$Rating, dane_trening$Recommendation)
IV(dane_trening$Size, dane_trening$Recommendation)
IV(dane_trening$Season, dane_trening$Recommendation)
IV(dane_trening$NeckLine, dane_trening$Recommendation)
IV(dane_trening$SleeveLength, dane_trening$Recommendation)
IV(dane_trening$Waistline, dane_trening$Recommendation)
IV(dane_trening$Material, dane_trening$Recommendation)
IV(dane_trening$FabricType, dane_trening$Recommendation)
IV(dane_trening$Decoration, dane_trening$Recommendation)
IV(dane_trening$PatternType, dane_trening$Recommendation)

# podzial na testowy i treningowy

set.seed(19044)

odsetek <- 0.25
liczba <- floor(nrow(dane)*odsetek)
dane_przelosowane <- dane[sample(nrow(dane)),]
dane_test <- dane_przelosowane[1:liczba,]
dane_trening <- dane_przelosowane[((liczba+1):nrow(dane)),]


# drzewo

tree <- rpart(Recommendation ~ .,
                  data = dane_trening ,
                  method ="class")

tree_do4 <- rpart(Recommendation ~ .,
              data = dane_trening ,
              method ="class",
              control = rpart.control(maxdepth = 4))
tree_cp <- rpart(Recommendation ~ .,
              data = dane_trening ,
              method ="class",
              control = rpart.control(cp = 0.05))

tree_zmienne <- rpart(Recommendation ~ Style + Season + NeckLine + Price,
                data = dane_trening,
                method = "class")

tree_zmienne2 <- rpart(Recommendation ~ Style + Season + NeckLine + Price + SleeveLength + Material + FabricType + PatternType,
                      data = dane_trening,
                      method = "class")

tree_zmienne2przyciete <- rpart(Recommendation ~ Style + Season + NeckLine + Price + SleeveLength + Material + FabricType + PatternType,
                       data = dane_trening,
                       method = "class",
                       control = rpart.control(minbucket = 25))

tree_zmienne2split <- rpart(Recommendation ~ Style + Season + NeckLine + Price + SleeveLength + Material + FabricType + PatternType,
                                data = dane_trening,
                                method = "class",
                                control = rpart.control(minsplit = 150))



rpart.plot(tree_zmienne2przyciete,  tweak = 1.2)

regresja <- glm(Recommendation ~.,
                data = dane_trening,
                family = "binomial")

regresja_zmienne <- regresja %>%
  stepAIC(trace = FALSE)

las <- randomForest(Recommendation ~.,
                    data = dane_trening)


CM <- list()
CM[["regresja"]] <- table(ifelse(predict(regresja, new = dane_test, type = "response") > 0.5, 1, 0), dane_test$Recommendation)
CM[["regresja_zmienne"]] <- table(ifelse(predict(regresja_zmienne, new = dane_test, type = "response") > 0.5, 1, 0), dane_test$Recommendation)

### Drzewa
CM[["tree"]] <- table(predict(tree, new = dane_test, type = "class"), dane_test$Recommendation)
CM[["tree_do4"]] <- table(predict(tree_do4, new = dane_test, type = "class"), dane_test$Recommendation)
CM[["tree_cp"]] <- table(predict(tree_cp, new = dane_test, type = "class"), dane_test$Recommendation)
CM[["tree_zmienne"]] <- table(predict(tree_zmienne, new = dane_test, type = "class"), dane_test$Recommendation)
CM[["tree_zmienne2"]] <- table(predict(tree_zmienne2, new = dane_test, type = "class"), dane_test$Recommendation)
### Las
CM[["las"]] <- table(predict(las, new = dane_test, type = "class"), dane_test$Recommendation)
CM[["tree_zmienne2_train"]] <- table(predict(tree_zmienne2, new = dane_trening, type = "class"), dane_trening$Recommendation)
CM[["tree_zmienne2przyciete"]] <- table(predict(tree_zmienne2przyciete, new = dane_test, type = "class"), dane_test$Recommendation)

EvaluateModel <- function(classif_mx){
  true_positive <- classif_mx[2, 2]
  true_negative <- classif_mx[1, 1]
  condition_positive <- sum(classif_mx[ , 2])
  condition_negative <- sum(classif_mx[ , 1])
  predicted_positive <- sum(classif_mx[2, ])
  predicted_negative <- sum(classif_mx[1, ])
  
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_negative) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
}

sapply(CM, EvaluateModel)

# do ROC
preds <- list()

preds[["regresja"]] <- as.vector(predict(regresja, newdata = dane_test, type = "response"))
### Drzewa
preds[["tree"]] <- as.vector(predict(tree, newdata = dane_test)[, 2])
preds[["tree_cp"]] <- as.vector(predict(tree_cp, newdata = dane_test)[, 2])
preds[["tree_do4"]] <- as.vector(predict(tree_do4, newdata = dane_test)[, 2])
preds[["tree_zmienne"]] <- as.vector(predict(tree_zmienne, newdata = dane_test)[, 2])
preds[["tree_zmienne2"]] <- as.vector(predict(tree_zmienne2, newdata = dane_test)[, 2])
### Las
preds[["las"]] <- as.vector(predict(las, newdata = dane_test, type = "prob")[, 2])
preds[["tree_zmienne2przyciete"]] <- as.vector(predict(tree_zmienne2przyciete, newdata = dane_test)[, 2])

plot(performance(prediction(preds[["tree"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_cp"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_do4"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["regresja"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["las"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_zmienne"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_zmienne2"]], dane_test$Recommendation), "tpr", "fpr")) 
plot(performance(prediction(preds[["tree_zmienne2przyciete"]], dane_test$Recommendation), "tpr", "fpr")) 

# AUC
(performance(prediction(preds[["tree"]], dane_test$Recommendation), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_cp"]], dane_test$Recommendation), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_do4"]], dane_test$Recommendation), "auc")@y.values[[1]])
(performance(prediction(preds[["regresja"]], dane_test$Recommendation), "auc")@y.values[[1]])
(performance(prediction(preds[["las"]], dane_test$Recommendation), "auc")@y.values[[1]])
(performance(prediction(preds[["tree_zmienne"]], dane_test$Recommendation), "auc")@y.values[[1]]) 
(performance(prediction(preds[["tree_zmienne2"]], dane_test$Recommendation), "auc")@y.values[[1]]) 
(performance(prediction(preds[["tree_zmienne2przyciete"]], dane_test$Recommendation), "auc")@y.values[[1]])   
  
# lift
plot(performance(prediction(preds[["tree_zmienne2przyciete"]], dane_test$Recommendation), "lift", "rpp"))

