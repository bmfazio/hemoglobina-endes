# custom fitting
fit <- function(data_, formula_) {
  brm(formula_,
      data = data_,
      control = list(adapt_delta = 0.99, max_treedepth = 20))
}

###
m1.2016 <- endes2016 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"))

m1_adj.2016 <- endes2016 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb_adj ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"))

m2.2016 <- endes2016 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region)

m2_r.2016 <- endes2016 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region +
        (1|cluster))

m3.2016 <- endes2016 %>%
  filter(!is.na(hb)) %>%
  mutate(rtotal = rtotal > 0) %>%
  fit(hb ~
        t2(total, altura, by = rtotal) + s(edad, bs = "cr") +
        s(nse, bs = "cr") + region + (1|cluster))

save(list = c("m1.2016", "m1_adj.2016", "m2.2016", "m2_r.2016", "m3.2016"),
     file = "out/m2016.rds")
rm(list = c("m1.2016", "m1_adj.2016", "m2.2016", "m2_r.2016", "m3.2016"))

m1.2017 <- endes2017 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"))

m1_adj.2017 <- endes2017 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb_adj ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"))

m2.2017 <- endes2017 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region)

m2_r.2017 <- endes2017 %>%
  filter(!is.na(hb) & rtotal > 0) %>%
  fit(hb ~
        t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region +
        (1|cluster))

m3.2017 <- endes2017 %>%
  filter(!is.na(hb)) %>%
  mutate(rtotal = rtotal > 0) %>%
  fit(hb ~
        t2(total, altura, by = rtotal) + s(edad, bs = "cr") +
        s(nse, bs = "cr") + region + (1|cluster))

save(list = c("m1.2017", "m1_adj.2017", "m2.2017", "m2_r.2017", "m3.2017"),
     file = "out/m2017.rds")
rm(list = c("m1.2017", "m1_adj.2017", "m2.2017", "m2_r.2017", "m3.2017"))