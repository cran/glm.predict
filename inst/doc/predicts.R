## ----setup, echo = FALSE, message = FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(glm.predict)
set.seed(1848)

## -----------------------------------------------------------------------------
df_selects = selects2015
logit_model = glm(participation ~ age * gender, family = binomial, data = df_selects)
summary(logit_model)

## -----------------------------------------------------------------------------
betas = coef(logit_model)
vcov = vcov(logit_model)
betas_simulated = MASS::mvrnorm(1000, betas, vcov)

## -----------------------------------------------------------------------------
x = c(1, 30, 1, 30)
yhat = betas_simulated %*% x

## -----------------------------------------------------------------------------
predProb = exp(yhat) / (1 + exp(yhat))

## -----------------------------------------------------------------------------
mean(predProb)
quantile(predProb, probs = c(0.025, 0.975))

## -----------------------------------------------------------------------------
boot = function(x, model){
  data = model.frame(model)
  data_sample = data[sample(seq_len(nrow(data)), replace = TRUE), ]
  coef(update(model, data = data_sample))
}
betas_boot = do.call(rbind, lapply(1:1000, boot, logit_model))

## -----------------------------------------------------------------------------
x = c(1, 30, 1, 30)
yhat = betas_boot %*% x
predProb = exp(yhat) / (1 + exp(yhat))
mean(predProb)
quantile(predProb, probs = c(0.025, 0.975))

## -----------------------------------------------------------------------------
df_selects = selects2015
library(MASS)
ologit_model = polr(opinion_eu_membership ~ vote_choice * lr_self + age + gender, 
                    data = df_selects, Hess = TRUE)
summary(ologit_model)

## ---- echo=FALSE--------------------------------------------------------------
df_pred = predicts(ologit_model, "F;0,5,10;mean;mode", doPar = FALSE)
head(df_pred)

## ---- eval=FALSE--------------------------------------------------------------
#  df_pred = predicts(ologit_model, "F;0,5,10;mean;mode")
#  head(df_pred)

## ---- echo = FALSE------------------------------------------------------------
df_dc = predicts(ologit_model, "F(1,6);0-10;mean;mode", position = 1, doPar = FALSE)
head(df_dc)

## ---- eval = FALSE------------------------------------------------------------
#  df_dc = predicts(ologit_model, "F(1,6);0-10;mean;mode", position = 1)
#  head(df_dc)

## -----------------------------------------------------------------------------
library(ggplot2)
# put the levels in the right order
df_dc$level = factor(df_dc$level, levels = levels(df_selects$opinion_eu_membership))
ggplot(df_dc, aes(x = lr_self, y = dc_mean, ymin = dc_lower, ymax = dc_upper)) +
  geom_ribbon(alpha = 0.5) + geom_line() + facet_wrap(~level) +
  theme_minimal() + geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  ylab("discrete change between SP and SVP") + xlab("left-right position") +
  ggtitle("Opinion if Switzerland should join the EU")

