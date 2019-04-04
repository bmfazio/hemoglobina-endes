# Cargar niños 6 - 60 meses

## 2016
# RECH0 - Household's basic data
rech0_16 <- read_sav(DATADIR %p0% "2016/548-Modulo64/RECH0.SAV")
# RECH2+3 - Datos hogar
rech23_16 <- read_sav(DATADIR %p0% "2016/548-Modulo65/RECH23.SAV")
# REC01+11 - Datos mujer
rec0111_16 <- read_sav(DATADIR %p0% "2016/548-Modulo66/REC0111.SAV")
# REC21 - Reproduction (sexo nino)
rec21_16 <- read_sav(DATADIR %p0% "2016/548-Modulo67/REC21.SAV")
# REC43: Salud nino
rec43_16 <- read_sav(DATADIR %p0% "2016/548-Modulo70/REC43.SAV")
# REC44: Antropometria y anemia
rec44_16 <- read_sav(DATADIR %p0% "2016/548-Modulo74/REC44.SAV")
# REC94: Country specific - Maternity (detalle de nacimiento)
rec94_16 <- read_sav(DATADIR %p0% "2016/548-Modulo69/REC94.SAV")
# REC95: Country specific - Maternity (detalle de suplementacion)
rec95_16 <- read_sav(DATADIR %p0% "2016/548-Modulo70/REC95.SAV")

## 2017
# RECH0 - Household's basic data
rech0_17 <- read_sav(DATADIR %p0% "2017/Modulo64/RECH0.SAV")
# RECH2+3 - Datos hogar
rech23_17 <- read_sav(DATADIR %p0% "2017/Modulo65/RECH23.SAV")
# REC01+11 - Datos mujer
rec0111_17 <- read_sav(DATADIR %p0% "2017/Modulo66/REC0111.SAV")
# REC21 - Reproduction (sexo nino)
rec21_17 <- read_sav(DATADIR %p0% "2017/Modulo67/REC21.SAV")
# REC43: Salud nino
rec43_17 <- read_sav(DATADIR %p0% "2017/Modulo70/REC43.SAV")
# REC44: Antropometria y anemia
rec44_17 <- read_sav(DATADIR %p0% "2017/Modulo74/REC44.SAV")
# REC94: Country specific - Maternity (detalle de nacimiento)
rec94_17 <- read_sav(DATADIR %p0% "2017/Modulo69/REC94.SAV")
# REC95: Country specific - Maternity (detalle de suplementacion)
rec95_17 <- read_sav(DATADIR %p0% "2017/Modulo70/REC95.SAV")

# Merge
rec43 %>%
  left_join(
    rec44,
    by = c("CASEID" = "CASEID", "HIDX" = "HWIDX")
  ) %>%
  left_join(
    rec94,
    by = c("CASEID" = "CASEID", "HIDX" = "IDX94")
  ) %>%
  left_join(
    rec95,
    by = c("CASEID" = "CASEID", "HIDX" = "IDX95")
  ) %>%
  left_join(
    rec0111,
    by = "CASEID"
  ) %>%
  mutate(HHID = CASEID %>% substr(1,15)) %>%
  left_join(
    rech0,
    by = "HHID"
  ) %>% 
  left_join(
    rech23,
    by = "HHID"
  ) -> endes2017

endes2017 %>%
  transmute(
    # Hemoglobina (sin ajuste)
    hb = HW53,
    # Hemoglobina (ajuste por altura)
    hb_adj = HW56,
    # Diagnostico de anemia (basado en ajuste por altura)
    anemia = HW57,
    # Suplementos de hierro recibidos (ult 12 meses)
    rjarabe = S465DD_AR %>% ifelse(is.na(.), 0, .),
    rgotas = S465DD_BR %>% ifelse(is.na(.), 0, .),
    rchisp = S465DD_CR %>% ifelse(is.na(.), 0, .),
    rtotal = rjarabe + rgotas + rchisp,
    # Consumo reportado de suplementos de hierro (ult 12 meses)
    jarabe = S465DD_AC %>% ifelse(is.na(.), 0, .),
    gotas = S465DD_BC %>% ifelse(is.na(.), 0, .),
    chisp = S465DD_CC %>% ifelse(is.na(.), 0, .),
    total = jarabe+gotas+chisp,
    # covar
    edad = HW1,
    nse = V191,
    altura = V040,
    # clustering
    region = as.factor(V023),
    cluster = as.factor(V001),
    hogar = as.factor(HHID)
  ) -> listo

### Examinar clustering
# endes2017 %>% group_by(V023, V022, V001, HHID) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023, V022, V001) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023, V022) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023) %>% summarise(obs = n()) %>% nrow