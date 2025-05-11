
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

# curva ROC
library(yardstick)
dtr <- data.frame(Y = factor(train$smoking, 
                             levels = c("yes", "no"), labels = c(1, 0)),
                  p = 1 - step_both0$fitted.values,
                  type = "Train")
prob <- predict(step_both0, newdata = test %>% select(-smoking), 
                type = "response")
dte <- data.frame(Y = factor(test$smoking, 
                             levels = c("yes", "no"), labels = c(1, 0)),
                  p = 1 - prob,
                  type = "Test")
bind_rows(dtr, dte) %>% group_by(type) %>% roc_curve(Y, p) %>% autoplot()
bind_rows(dtr, dte) %>% group_by(type) %>% roc_auc(Y, p)

# per un confronto finale tra tutti i modelli mi salvo l'MSE sul test:
smoking.train.bin <- ifelse(train$smoking == "yes", 1, 0)
smoking.test.bin <- ifelse(test$smoking == "yes", 1, 0)
err.stepwise <- mean((smoking.test.bin - prob)^2)



# ridge -------------------------------------------------------------------

xtrain <- train %>% select(!smoking) %>% as.matrix()
xtest <- test %>% select(!smoking) %>% as.matrix()

library(glmnet)
ridge1 <- glmnet(y = smoking.train.bin,
                 x = xtrain,
                 alpha = 0, lambda.min.ratio = 1e-10, nlambda = 200)
# sui dati di test individuiamo il lambda che genera il minore MSE
previsione.ridge <- predict(ridge1, 
                      newx = xtest, type = "response")
err.ridge <- colMeans((smoking.test.bin - previsione.ridge)^2)
index.lambda.ridge <- err.ridge %>% which.min()

# ridge con cross-validation
set.seed(123)
ridge.cv <- cv.glmnet(y = smoking.train.bin,
                      x = xtrain,
                      alpha = 0, nfolds = 500)
prev_ridge.min <- predict(ridge.cv, 
                          newx = xtest,
                          s = "lambda.min",
                          type = "response")
prev_ridge.1se <- predict(ridge.cv, 
                          newx = xtest,
                          s = "lambda.1s",
                          type = "response")

# scegliamo il lambda ottimale per la ridge come quello che minimizza l'MSE
# sui dati di test:
c(err.ridge[index.lambda.ridge],
  mean((smoking.test.bin - prev_ridge.min)^2),
  mean((smoking.test.bin - prev_ridge.1se)^2)
  )

# il lambda scelto è quello calcolato senza cross-validation; calcoliamo 
# ROC e AUC per il modello ridge scelto:
dtr.ridge <- data.frame(Y = factor(ifelse(train$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(predict(ridge1, xtrain, 
                              s = ridge1$lambda[index.lambda.ridge],
                              type = "response")),
                     type = "Train")
dte.ridge <- data.frame(Y = factor(ifelse(test$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(previsione.ridge[, index.lambda.ridge]),
                     type = "Test")
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% 
  roc_curve(Y, p, event_level = "first") %>% autoplot()
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% roc_auc(Y, p) # leggermente
                        # peggio del risultato ottenuto per procedura stepwise



# lasso -------------------------------------------------------------------

lasso1 <- glmnet(y = smoking.train.bin,
                 x = xtrain,
                 alpha = 1, lambda.min.ratio = 1e-10, nlambda = 200)
# sui dati di test individuiamo il lambda che genera il minore MSE
previsione.lasso <- predict(lasso1, 
                            newx = xtest, type = "response")
err.lasso <- colMeans((smoking.test.bin - previsione.lasso)^2)
index.lambda.lasso <- err.lasso %>% which.min()

# ridge con cross-validation
set.seed(123)
lasso.cv <- cv.glmnet(y = smoking.train.bin,
                      x = xtrain,
                      alpha = 1, nfolds = 500)
prev_lasso.min <- predict(lasso.cv, 
                          newx = xtest,
                          s = "lambda.min",
                          type = "response")
prev_lasso.1se <- predict(lasso.cv, 
                          newx = xtest,
                          s = "lambda.1s",
                          type = "response")

# scegliamo il lambda ottimale per la ridge come quello che minimizza l'MSE
# sui dati di test:
c(err.ridge[index.lambda.lasso],
  mean((smoking.test.bin - prev_lasso.min)^2),
  mean((smoking.test.bin - prev_lasso.1se)^2)
)

# il lambda scelto è quello calcolato minimo calcolato con cross-validation; 
# calcoliamo ROC e AUC per il modello lasso scelto:
dtr.lasso <- data.frame(Y = factor(ifelse(train$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(predict(lasso.cv, newx = xtrain, 
                                              s = "lambda.min",
                                              type = "response")),
                        type = "Train")
dte.lasso <- data.frame(Y = factor(ifelse(test$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(prev_lasso.min),
                        type = "Test")
bind_rows(dtr.lasso, dte.lasso) %>% group_by(type) %>% 
  roc_curve(Y, p, event_level = "first") %>% autoplot()
bind_rows(dtr.lasso, dte.lasso) %>% group_by(type) %>% roc_auc(Y, p) # stessi 
                                        # risultati della regressione ridge

# per capire quali variabili seleziona il lasso:
predict(lasso.cv, type = "coefficients", s = "lambda.min") # non porta a 0 
                                                      # alcuna variabile



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


