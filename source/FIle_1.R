
# loading data ------------------------------------------------------------

data <- read.csv("data/train_dataset.csv")
head(data, 10)
str(data)
library(tidyverse)
data <- as.tibble(data)
glimpse(data)
data <- data %>% mutate(
  hearing.left. = as.factor(hearing.left.),
  hearing.right. = as.factor(hearing.right.),
  dental.caries = as.factor(dental.caries),
  smoking = as.factor(smoking)
)
sum(is.na(data)) # no NA data



# EDA ---------------------------------------------------------------------

data %>%
  mutate(HDL_LDL_Sum = HDL + LDL) %>%
  select(HDL_LDL_Sum, Cholesterol) %>%
  plot(HDL_LDL_Sum ~ Cholesterol, data = .) 
data %>%
  mutate(HDL_LDL_Sum = HDL + LDL) %>%
  select(HDL_LDL_Sum, Cholesterol) %>%
  lm(Cholesterol ~ HDL_LDL_Sum, data = .) %>%
  summary() %>%
  .$r.squared
# LDL e HDL are strictly related to Cholesterol: this leads to a 
# collinearity issue

# smoking, age(!):
ggplot(data) + geom_boxplot(aes(x = smoking, y = age))

# smoking, height.cm(!):
ggplot(data) + geom_boxplot(aes(x = smoking, y = height.cm.))

# smoking, weight.kg.(!):
ggplot(data) + geom_boxplot(aes(x = smoking, y = weight.kg.))

# smoking, waist.cm.:
ggplot(data) + geom_boxplot(aes(x = smoking, y = waist.cm.))

# smoking, eyesight.left.:
ggplot(data) + geom_boxplot(aes(x = smoking, y = eyesight.left.))

# smoking, eyesight.right.:
ggplot(data) + geom_boxplot(aes(x = smoking, y = eyesight.right.))

# smoking, hearing.left.:
data %>% select(hearing.left., smoking) %>% table() %>% prop.table(margin = 2)

# smoking, hearing.right.:
data %>% select(hearing.left., smoking) %>% table() %>% prop.table(margin = 2)

# smoking, systolic:
ggplot(data) + geom_boxplot(aes(x = smoking, y = systolic))

# smoking, relaxation:
ggplot(data) + geom_boxplot(aes(x = smoking, y = relaxation))

# smoking, fasting.blood.sugar:
ggplot(data) + geom_boxplot(aes(x = smoking, y = fasting.blood.sugar))

# smoking, Cholesterol:
ggplot(data) + geom_boxplot(aes(x = smoking, y = Cholesterol))

# smoking, triglyceride(!):
ggplot(data) + geom_boxplot(aes(x = smoking, y = triglyceride))

# smoking, HDL:
ggplot(data) + geom_boxplot(aes(x = smoking, y = HDL))

# smoking, LDL:
ggplot(data) + geom_boxplot(aes(x = smoking, y = LDL))

# smoking, hemoglobin(!):
ggplot(data) + geom_boxplot(aes(x = factor(smoking), y = hemoglobin))

# smoking, Urine.protein:
data %>% select(Urine.protein, smoking) %>% table %>% prop.table(margin = 2)

# smoking, serum.creatinine:
ggplot(data) + geom_boxplot(aes(x = factor(smoking), y = serum.creatinine))

# smoking, AST:
ggplot(data) + geom_boxplot(aes(x = smoking, y = AST))

# smoking, ALT:
ggplot(data) + geom_boxplot(aes(x = smoking, y = ALT))

# smoking, Gtp:
ggplot(data) + geom_boxplot(aes(x = smoking, y = Gtp))

# smoking, dental.caries(!):
data %>% select(dental.caries, smoking) %>% table %>% prop.table(margin = 2)

library(corrplot)
data %>%
  select(where(is.numeric)) %>% 
  cor(use = "pairwise.complete.obs") %>%  
  corrplot(method = "ellipse", type = "upper", tl.col = "black", tl.srt = 45)



# variable selection ------------------------------------------------------



# data split --------------------------------------------------------------

library(dplyr)
target <- "smoking"
data[[target]] <- factor(data[[target]], levels = c(1, 0), 
                         labels = c("yes", "no"))

set.seed(123) 
data <- data %>% mutate(split = ifelse(runif(n()) < 0.7, "train", "test")) # qui
                             # la funzione n() è una funzione scorciatoia della
                             # libreria dplyr, nel senso che tiene conto del 
                             # numero di righe del dataframe che viene passato
train <- data %>% filter(split == "train") %>% select(-split)
test <- data %>% filter(split == "test") %>% select(-split)

n <- nrow(train)



# stepwise regression -----------------------------------------------------

# modello con tutte le variabili:
glm1 <- glm(smoking ~ ., family = binomial, data = train)
# modello nullo:
glm0 <- glm(smoking ~ 1, family = binomial, data = train)

# approccio bacward
step_back <- glm1 %>% stats::step(direction = "backward")
# Nei passaggi sono state rimosse in sequenza le seguenti variabili: 
# AST, eyesight.right., LDL.
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# Il messaggio di warning suggerisce problemi di multicollinearità.

# approccio forward
step_forw <- glm0 %>% stats::step(direction = "forward", 
                                  scope = formula(glm1))
# Sono state aggiunte tutte le variabili fuorchè le seguenti:
# LDL, eyesight.right., AST.
# Il modello trovato dunque, è identico a ciò che abbiamo ottenuto sopra.
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred
# Il messaggio di warning suggerisce problemi di multicollinearità.

# approccio ibrido
# partendo dal modello con tutte le variabili:
step_both1 <- glm1 %>% stats::step(direction = "both")
summary(step_both1)
formula(step_both1)
# partendo dal modello nullo:
step_both0 <- glm1 %>% stats::step(direction = "both",
                                   scope = formula(glm1))
summary(step_both0)
formula(step_both0)
# Con l'approccio ibrido, in entrambi i casi, si arrivano alle 
# conclusioni viste sopra. Rimane il seguente messaggio di warning:
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 



# BSS ---------------------------------------------------------------------

library(leaps)
bss <- leaps::leaps(y = train %>% pull(smoking),
                    x = train %>% select(-smoking) %>% as.matrix(),
                    nbest = 1) # nbest indica quanti modelli migliori per 
                               # ciascun numero di variabili devono essere 
                               # restituiti; in questo caso viene restituito 
                               # solamente il modello migliore per ciascun
                               # numero di variabili
image(t(bss$which)) # 



# CART --------------------------------------------------------------------

if (!require(caret)) install.packages("caret", dependencies = TRUE)
if (!require(rpart)) install.packages("rpart", dependencies = TRUE)
library(caret)
library(rpart)

target <- 'smoking'

train[[target]] <- as.factor(train[[target]])
X <- train[, setdiff(names(train), target)] # considera tutti i nomi di
                      # colonna del dataset train fuorchè quelli che 
                      # coincidono con il nome individuato dalla 
                      # variabile target
X <- as.data.frame(X) # viene fatto per evitare warning di row names in caret
                      # (cosa che succede se utilizziamo un tibble)
y <- train[[target]] 

target <- "smoking"
set.seed(123)
cv_ctrl <- trainControl(
  method          = "cv", # utilizza la validazione incrociata come 
                          # metodo di resampling
  number          = 5, # impiega una 5-fold cross validation
  savePredictions = "final", # salva le predizioni fatte nel fold finale
  classProbs      = TRUE # il modello deve calcolare anche le probabilità di 
                         # classe oltre alle etichetta predette (utile per poi
                         # costruire una curva ROC o AUC)
) # impostazioni di controllo per la cross-validation quando si usa caret

cp_values <- seq(0.0001, 0.001, by = 0.00001) # insieme di possibili valori
                                      # per il parametro di complessità
cp_grid   <- expand.grid(cp = cp_values)

cv_model <- train(
  x          = X,
  y          = y,
  method     = "rpart",
  trControl  = cv_ctrl,
  tuneGrid   = cp_grid,
  metric     = "Accuracy"
)
print(cv_model) 
best_cp <- cv_model$bestTune$cp # estrae il miglior cp
cat("Best cp:", best_cp)
plot(cv_model)

final_model <- rpart(
  formula = as.formula(paste(target, "~ .")),
  data    = train,
  control = rpart.control(cp = best_cp)
)
print(final_model) # struttura: regole di split e statistiche
plot(final_model, uniform = TRUE, margin = 0.1)
X_test <- test %>% select(-smoking)
preds <- predict(final_model, newdata = X_test, type = "class")
confusionMatrix(preds, test[[target]])

library(pROC)
# probabilità predette
probs <- predict(final_model, newdata = test, type = "prob")
# curva ROC
roc_obj <- roc(response = test[[target]], predictor = probs[,"yes"], 
               levels = c("no", "yes"), direction = "<")
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC")
# calcolo AUC
auc_value <- auc(roc_obj)
print(auc_value)



# random forest  ----------------------------------------------------------

library(randomForest)

set.seed(123)  
rf_model <- randomForest(
  smoking ~ .,
  data = train,
  ntree = 1000,
  importance = TRUE
)
rf_probs <- predict(rf_model, newdata = test, type = "prob")[, "yes"]
rf_preds <- ifelse(rf_probs >= 0.5, "yes", "no")
rf_preds <- factor(rf_preds, levels = c("yes", "no"))
conf_matrix <- confusionMatrix(rf_preds, test$smoking, positive = "yes")
print(conf_matrix)

# confronto con l'albero trovato prima:
confusionMatrix(preds, test[[target]])

# l'adattamento è decisamente migliorato, anche per quanto riguarda 
# la sensibilità (rapporto fumatori identificati su fumatori) che è ciò 
# che ci interessa di più

roc_obj <- roc(response = test$smoking, predictor = rf_probs, levels = c("yes", "no"))
plot(roc_obj, col = "blue", lwd = 2, main = "Curva ROC - Random Forest")
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")


