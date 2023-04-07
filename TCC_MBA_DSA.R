##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################

#Pacotes utilizados

pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
             "reshape2","jtools","stargazer","lmtest","caret","pROC","ROCR","nnet",
             "magick","cowplot","globals","equatiomatic","haven","readxl","performance")

options(rgl.debug = TRUE)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


##################################################################################
#                                IMPORTANDO DADOS                                #
##################################################################################

# Importando dados

data <- read_excel("dados.xlsx")

glimpse(data)


##################################################################################
#                                ESTIMANDO MODELO                                #
##################################################################################

# Modelo com todas as variáveis


model <- glm(formula = evento ~ pib + ind_transf + serv_transp + fbcf + selic + ipca + ps_empr + pap_ond + ibov, 
             data = data, 
             family = "binomial")

summary(model)
summ(model, confint = T, digits = 3, ci.width = .95)
logLik(model)


# Procedimento stepwise

model_step <- step(object = model, 
                   k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(model_step)
summ(model_step, confint = T, digits = 3, ci.width = .95)
logLik(model_step)


# Verificação de alteração no ajuste - Likelihood-ratio test

lrtest(model, model_step)


# Verificação de multicolineariedade - VIF e Tolerance

check_collinearity(model_step)



#####    Buscando modelo sem multicolineariedade    #####

# Retirando pib

model_no_pib <- glm(formula = evento ~ ind_transf + serv_transp + fbcf + selic + ipca + ps_empr + pap_ond + ibov, 
             data = data, 
             family = "binomial")
model_step_no_pib <- step(object = model_no_pib, 
                   k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_pib)


# Retirando ind_transf

model_no_ind_transf <- glm(formula = evento ~  pib + serv_transp + fbcf + selic + ipca + ps_empr + pap_ond + ibov, 
                    data = data, 
                    family = "binomial")
model_step_no_ind_transf <- step(object = model_no_ind_transf, 
                          k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_ind_transf)


# Retirando serv_transp

model_no_serv_transf <- glm(formula = evento ~  pib + ind_transf + fbcf + selic + ipca + ps_empr + pap_ond + ibov, 
                           data = data, 
                           family = "binomial")
model_step_no_serv_transf <- step(object = model_no_serv_transf, 
                                 k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_serv_transf)


# Retirando fbcf

model_no_fbcf <- glm(formula = evento ~  pib + ind_transf + serv_transp + selic + ipca + ps_empr + pap_ond + ibov, 
                            data = data, 
                            family = "binomial")
model_step_no_fbcf <- step(object = model_no_fbcf, 
                                  k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_fbcf)


# Retirando selic

model_no_selic <- glm(formula = evento ~  pib + ind_transf + serv_transp + fbcf + ipca + ps_empr + pap_ond + ibov, 
                     data = data, 
                     family = "binomial")
model_step_no_selic <- step(object = model_no_selic, 
                           k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_selic)


# Retirando ipca

model_no_ipca <- glm(formula = evento ~  pib + ind_transf + serv_transp + fbcf + selic + ps_empr + pap_ond + ibov, 
                      data = data, 
                      family = "binomial")
model_step_no_ipca <- step(object = model_no_ipca, 
                            k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_ipca)


# Retirando ps_empr

model_no_ps_empr <- glm(formula = evento ~  pib + ind_transf + serv_transp + fbcf + selic + ipca + pap_ond + ibov, 
                     data = data, 
                     family = "binomial")
model_step_no_ps_empr <- step(object = model_no_ps_empr, 
                           k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_ps_empr)


# Retirando pap_ond

model_no_pap_ond <- glm(formula = evento ~  pib + ind_transf + serv_transp + fbcf + selic + ipca + ps_empr + ibov, 
                        data = data, 
                        family = "binomial")
model_step_no_pap_ond <- step(object = model_no_pap_ond, 
                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))
check_collinearity(model_step_no_pap_ond)


# Retirando ibov

model_no_ibov <- glm(formula = evento ~  pib + ind_transf + serv_transp + fbcf + selic + ipca + ps_empr + pap_ond, 
                        data = data, 
                        family = "binomial")

summary(model_no_ibov)
summ(model_no_ibov, confint = T, digits = 3, ci.width = .95)
logLik(model_no_ibov)

model_step_no_ibov <- step(object = model_no_ibov, 
                              k = qchisq(p = 0.05, df = 1, lower.tail = FALSE))

summary(model_step_no_ibov)
summ(model_step_no_ibov, confint = T, digits = 3, ci.width = .95)
logLik(model_step_no_ibov)
lrtest(model_no_ibov, model_step_no_ibov)
check_collinearity(model_step_no_ibov)


##################################################################################
#                                   CURVA ROC                                    #
##################################################################################

ROC <- roc(response = data$evento, 
           predictor = model_step_no_ibov$fitted.values)

ggplotly(
  ggroc(ROC, color = "#440154FF", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                 color="grey40",
                 size = 0.2) +
    labs(x = "Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:",
                       round(ROC$auc, 3),
                       "|",
                       "Coeficiente de Gini",
                       round((ROC$auc[1] - 0.5) / 0.5, 3))) +
    theme_bw()
)


##################################################################################
#                           ANÁLIDE DE SENSSIBILIDADE                            #
##################################################################################


#função prediction do pacote ROCR

predicoes <- prediction(predictions = model_step_no_ibov$fitted.values, 
                        labels = as.factor(data$evento))


#função performance do pacote ROCR

dados_curva_roc <- ROCR::performance(predicoes, measure = "sens")

sensitividade <- (ROCR::performance(predicoes, measure = "sens"))@y.values[[1]] 

especificidade <- (ROCR::performance(predicoes, measure = "spec"))@y.values[[1]]

precisao <- (ROCR::performance(predicoes, measure = "prec"))@y.values[[1]]

acuracia <- (ROCR::performance(predicoes, measure = "acc"))@y.values[[1]]

#Extraindo os cutoffs

cutoffs <- dados_curva_roc@x.values[[1]]

dados_plotagem <- cbind.data.frame(cutoffs, especificidade, sensitividade, precisao, acuracia)

dados_plotagem$f1 <- 2*(dados_plotagem$sensitividade*dados_plotagem$precisao)/(dados_plotagem$sensitividade+dados_plotagem$precisao)

# Plotando gráfico

ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = especificidade)) +
           geom_line(aes(color = "Especificidade"),
                     size = 1) +
           geom_point(color = "#95D840FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = sensitividade, color = "Sensitividade"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = sensitividade),
                      color = "#440154FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Sensitividade/Especificidade") +
           scale_color_manual("Legenda:",
                              values = c("#95D840FF", "#440154FF")) +
           theme_bw())


##################################################################################
#                         MAXIMIZANDO ACURÁCIA E F1 SCORE                        #
##################################################################################

# Maximizando acurácia

ind_max_acc = which.max( dados_plotagem$acuracia )
dados_plotagem[ind_max_acc,]
cutoff_max_acc = dados_plotagem[ind_max_acc,1]
confusionMatrix(table(predict(model_step_no_ibov, type = "response") >= cutoff_max_acc,
                      data$evento == 1)[2:1, 2:1])

# Maximizando F1 score

ind_max_f1 = which.max( dados_plotagem$f1 )
dados_plotagem[ind_max_f1,]
cutoff_max_f1 = dados_plotagem[ind_max_f1,1]
confusionMatrix(table(predict(model_step_no_ibov, type = "response") >= cutoff_max_f1,
                      data$evento == 1)[2:1, 2:1])

# Plotando gráfico

ggplotly(dados_plotagem %>%
           ggplot(aes(x = cutoffs, y = acuracia)) +
           geom_line(aes(color = "Acuácia"),
                     size = 1) +
           geom_point(color = "#440154FF",
                      size = 1.9) +
           geom_line(aes(x = cutoffs, y = dados_plotagem$f1, color = "F1 score"),
                     size = 1) +
           geom_point(aes(x = cutoffs, y = dados_plotagem$f1),
                      color = "#95D840FF",
                      size = 1.9) +
           labs(x = "Cutoff",
                y = "Acurácia/F1 score") +
           scale_color_manual("Legenda:",
                              values = c("#440154FF", "#95D840FF")) +
           theme_bw())

