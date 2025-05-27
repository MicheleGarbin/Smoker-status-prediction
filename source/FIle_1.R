
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
# LDL e HDL sono strettamente legate a Cholesterol: questo causa un problema 
# di multicollinearità

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

# eliminazione degli outlier nel dataset di train
train <- train %>% filter(triglyceride < 500,
                          LDL < 500,
                          AST < 500,
                          ALT < 500)



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

# matrice di confusione
library(caret)
pred_class <- factor(ifelse(prob < 0.5, "yes", "no"), levels = c("yes", "no"))
confusionMatrix(pred_class, test$smoking)



# ridge -------------------------------------------------------------------

smoking.test.bin <- ifelse(test$smoking == "yes", 1, 0)
smoking.train.bin <- ifelse(train$smoking == "yes", 1, 0)
xtest <- test %>% select(-smoking) %>% as.matrix()

# divido il dataset di train standardizzato in train1 e validation, il secondo 
# dei quali lo utilizzo per scegliere il lambda ottimale
set.seed(1234)
train <- train %>% mutate(split = ifelse(runif(n()) < 0.7, 
                                         "train1", "validation"))

train1 <- train %>% filter(split == "train1") %>% select(-split) 
validation <- train %>% filter(split == "validation") %>% select(-split)  

smoking.train1.bin <- ifelse(train1$smoking == "yes", 1, 0)
smoking.validation.bin <- ifelse(validation$smoking == "yes", 1, 0)

xtrain1 <- train1 %>% select(!smoking) %>% as.matrix()
xvalidation <- validation %>% select(!smoking) %>% as.matrix()
xtrain <- train %>% select(-smoking, -split) %>% as.matrix()

# ridge con cross-validation
library(glmnet)
set.seed(123)
ridge.cv <- cv.glmnet(y = smoking.train1.bin,
                      x = xtrain1,
                      alpha = 0, nfolds = 100,
                      family = "binomial")
prev_ridge.min <- predict(ridge.cv, 
                          newx = xvalidation,
                          s = "lambda.min",
                          type = "response")
prev_ridge.1se <- predict(ridge.cv, 
                          newx = xvalidation,
                          s = "lambda.1s",
                          type = "response")

# scegliamo il lambda ottimale per la ridge come quello che minimizza la 
# neg-entropy sui dati di validation:
c(-sum(smoking.validation.bin * log(prev_ridge.min)),
  -sum(smoking.validation.bin * log(prev_ridge.1se))
  )

# una volta scelto il metodo con cui ottenere il minore lambda, ri-fitto 
# il modello utilizzando tutti i dati di train
ridge1 <- cv.glmnet(y = smoking.train.bin,
                       x = xtrain,
                       alpha = 0, nfolds = 100,
                       family = "binomial")
prob_train.ridge1 <- predict(ridge1, 
                            newx = xtrain, 
                            s = "lambda.min",
                            type = "response")

# il lambda scelto è quello calcolato senza cross-validation; calcoliamo 
# ROC e AUC per il modello ridge scelto:
prob_ridge1.test <- predict(ridge1, xtest, 
                      s = "lambda.min",
                      type = "response")
dtr.ridge <- data.frame(Y = factor(ifelse(train$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(prob_train.ridge1),
                     type = "Train")
dte.ridge <- data.frame(Y = factor(ifelse(test$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(prob_ridge1.test),
                     type = "Test")
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% 
  roc_curve(Y, p, event_level = "first") %>% autoplot()
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% roc_auc(Y, p) # leggermente
                        # peggio del risultato ottenuto per procedura stepwise

# matrice di confusione
pred_ridge_class <- factor(ifelse(prob_ridge1.test > 0.5, "yes", "no"), 
                           levels = c("yes", "no"))
confusionMatrix(pred_ridge_class, test$smoking)



# lasso -------------------------------------------------------------------

# lasso con cross-validation
set.seed(456)
lasso.cv <- cv.glmnet(y = smoking.train1.bin,
                      x = xtrain1,
                      alpha = 1, nfolds = 100,
                      family = "binomial")
prev_lasso.min <- predict(lasso.cv, 
                          newx = xvalidation,
                          s = "lambda.min",
                          type = "response")
prev_lasso.1se <- predict(lasso.cv, 
                          newx = xvalidation,
                          s = "lambda.1s",
                          type = "response")

# scegliamo il lambda ottimale per la lasso come quello che minimizza la 
# neg-entropy sui dati di validation:
c(-sum(smoking.validation.bin * log(prev_lasso.min)),
  -sum(smoking.validation.bin * log(prev_lasso.1se))
)

# una volta scelto il metodo con cui ottenere il minore lambda, ri-fitto 
# il modello utilizzando tutti i dati di train
lasso1 <- cv.glmnet(y = smoking.train.bin,
                    x = xtrain,
                    alpha = 1, nfolds = 100,
                    family = "binomial")
prob_train.lasso1 <- predict(lasso1, 
                             newx = xtrain, 
                             s = "lambda.min",
                             type = "response")

# il lambda scelto è quello calcolato senza cross-validation; calcoliamo 
# ROC e AUC per il modello lasso scelto:
prob_lasso1.test <- predict(lasso1, xtest, 
                            s = "lambda.min",
                            type = "response")
dtr.lasso <- data.frame(Y = factor(ifelse(train$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(prob_train.lasso1),
                        type = "Train")
dte.lasso <- data.frame(Y = factor(ifelse(test$smoking == "yes", "1", "0"), 
                                   levels = c("1", "0")),
                        p = as.vector(prob_lasso1.test),
                        type = "Test")
bind_rows(dtr.lasso, dte.lasso) %>% group_by(type) %>% 
  roc_curve(Y, p, event_level = "first") %>% autoplot()
bind_rows(dtr.lasso, dte.lasso) %>% group_by(type) %>% roc_auc(Y, p) 

# matrice di confusione
pred_lasso_class <- factor(ifelse(prob_lasso1.test > 0.5, "yes", "no"), 
                           levels = c("yes", "no"))
confusionMatrix(pred_lasso_class, test$smoking)

# per capire quali variabili seleziona il lasso:
predict(lasso1, type = "coefficients", s = "lambda.min") 







