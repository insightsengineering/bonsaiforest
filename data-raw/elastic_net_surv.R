y <- elastic_net_fit_surv$y
order_resp <- order(y$resp)
resp_un <- y$resp[order_resp]

### Some important variables
fit <- elastic_net_fit_surv$fit
lp <- predict(fit, fit$lambda.min, newx = elastic_net_fit_surv$design_matrix)
lp_un <- lp[order_resp]
status_un <- y$status[order_resp]


### Cumulative baseline hazard at some points
bh <- gbm::basehaz.gbm(t = resp_un, delta = status_un, f.x = lp_un,
                       t.eval = resp_un, smooth = TRUE, cumulative = TRUE)
ind_time <- which(status_un == 1)
h0 <- bh[ind_time]
est_coef_surv <- as.matrix(coef(fit, s = fit$lambda.min))
elastic_net_surv <- list(h0 = h0, est_coef = est_coef_surv,
                         x = elastic_net_fit_surv$design_matrix)
usethis::use_data(elastic_net_surv, overwrite = TRUE)
