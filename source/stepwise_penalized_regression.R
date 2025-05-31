
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

# eliminazione degli outlier nel dataset di train
train <- train %>% filter(triglyceride < 500,
                          LDL < 500,
                          AST < 500,
                          ALT < 500)



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

# curva ROC e calcolo AUC
library(yardstick)
dtr <- data.frame(Y = train$smoking,
                  p = step_both0$fitted.values,
                  type = "Train")
prob <- predict(step_both0, newdata = test %>% dplyr::select(-smoking), 
                type = "response")
dte <- data.frame(Y = test$smoking,
                  p = prob,
                  type = "Test")
bind_rows(dtr, dte) %>% group_by(type) %>% 
  mutate(Y = factor(Y)) %>% 
  roc_curve(Y, p, event_level = "second") %>% autoplot() +
  labs(title = "Curva ROC per la selezione stepwise")
bind_rows(dtr, dte) %>% group_by(type) %>% 
  mutate(Y = factor(Y)) %>% 
  roc_auc(Y, p, event_level = "second")

# matrice di confusione
library(caret)
pred_class <- factor(ifelse(prob < 0.5, "yes", "no"), levels = c("yes", "no"))
confusionMatrix(pred_class, test$smoking)



# ridge -------------------------------------------------------------------

smoking.test.bin <- ifelse(test$smoking == "yes", 1, 0)
smoking.train.bin <- ifelse(train$smoking == "yes", 1, 0)
xtest <- test %>% select(-smoking) %>% as.matrix()
xtrain <- train %>% select(-smoking, -split) %>% as.matrix()

# ridge con cross-validation sul train set:
ridge1 <- cv.glmnet(y = smoking.train.bin,
                       x = xtrain,
                       alpha = 0, nfolds = 100,
                       family = "binomial")
prob_train.ridge1 <- predict(ridge1, 
                            newx = xtrain, 
                            s = "lambda.min",
                            type = "response")

plot(ridge1, sign.lambda = 1)
# siccome "lambda.min" è il minore lambda possibile, proseguiamo utilizzando 
# "lambda.1se" per applicare una penalità maggiore ed allontanarci da 
# quanto visto nella procedura stepwise

prob_ridge1.test <- predict(ridge1, xtest, 
                      s = "lambda.1se",
                      type = "response")
dtr.ridge <- data.frame(Y = train$smoking,
                        p = c(prob_train.ridge1),
                     type = "Train")
dte.ridge <- data.frame(Y = test$smoking,
                        p = c(prob_ridge1.test),
                     type = "Test")
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% 
  mutate(Y = factor(Y)) %>% 
  roc_curve(Y, p) %>% autoplot() +
    labs(title = "Curva ROC per la regressione penalizzata lasso")
bind_rows(dtr.ridge, dte.ridge) %>% group_by(type) %>% roc_auc(Y, p) 

# matrice di confusione
pred_ridge_class <- factor(ifelse(prob_ridge1.test > 0.5, "yes", "no"), 
                           levels = c("yes", "no"))
confusionMatrix(pred_ridge_class, test$smoking)



# lasso -------------------------------------------------------------------

# lasso con cross-validation sul train set
lasso1 <- cv.glmnet(y = smoking.train.bin,
                    x = xtrain,
                    alpha = 1, nfolds = 100,
                    family = "binomial")
prob_train.lasso1 <- predict(lasso1, 
                             newx = xtrain, 
                             s = "lambda.min",
                             type = "response")

plot(lasso1, sign.lambda = 1)
# per lo stesso ragionamento visto sopra, utilizziamo lambda.1se, sperando di 
# effettuare una maggiore selezione delle variabili

prob_lasso1.test <- predict(lasso1, xtest, 
                            s = "lambda.1se",
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
predict(lasso1, type = "coefficients", s = "lambda.1se") 
# le variabili portate a 0 sono: age, waist.cm., eysight.left., eyesight.right.,
# hearing.left., hearing.right., relaxation, Urine.protein, AST



# glmnet ------------------------------------------------------------------

# per diversi valori di alpha, provo a calcolarmi le metriche
# accuracy, sensitivity e specificity sul test set ponendo nfolds = 50 per 
# quanto riguarda la cross-validation sul train set

# mi salvo i risultati in una matrice di tre colonne (una per ogni metrica)
# e nelle righe ho i vari alpha
res_glmnet <- matrix(NA, 20, 3)
colnames(res_glmnet) <- c("Accuracy", "Sensitivity", "Specificity")
alpha_val <- seq(0, 1, length = 20)
for(i in 1:20){
  glmnet_cv <- cv.glmnet(x = xtrain,
                         y = smoking.train.bin,
                         alpha = alpha_val[i],
                         nfolds = 50)
  pred_glmnet <- predict(glmnet_cv, xtest, 
                         s = "lambda.1se", type = "response")
  pred_lasso_class <- factor(ifelse(pred_glmnet > 0.5, "yes", "no"), 
                             levels = c("yes", "no"))
  cm <- confusionMatrix(pred_lasso_class, test$smoking)
  res_glmnet[i, ] <- c(
    cm$overall["Accuracy"],
    cm$byClass["Sensitivity"],
    cm$byClass["Specificity"]
  )
}
res_glmnet <- cbind(alpha_val, res_glmnet)
# risultati in ordine di accuracy:
res_glmnet[order(res_glmnet[, 2]), ]
# risultati in ordine di sensitivity:
res_glmnet[order(res_glmnet[, 3]), ]
# risultati in ordine di specificity:
res_glmnet[order(res_glmnet[, 4]), ]






