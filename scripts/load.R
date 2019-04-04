# ENDES path structure
data.frame(
  year = c(2016, 2017),
  prefix = c("2016/548-", "2017/")
) -> endes.year

endes.files <- read.csv("in/endes_files.csv")

endes_pick <- function(year_, name_) {
  endes.year %>%
    filter(year == year_) %>%
    pull(prefix) -> prefix
  endes.files %>%
    filter(name == name_) %>%
    pull(path) -> path
  prefix %p0% path
}

endes_format <- function(path_, format_ = ".SAV") {
  target <- DATADIR %p0% path_ %p0% format_
  
  if(format_ == ".SAV") fun_ <- read_sav
  
  fun_(target)
}

endes_load <- function(x, y) endes_format(endes_pick(x, y))


# Load
  # Cargar niños 6 - 60 meses
## 2016
endes_load(2016, "Health") %>%
  left_join(
    endes_load(2016, "Height and Weight"),
    by = c("CASEID" = "CASEID", "HIDX" = "HWIDX")
  ) %>%
  left_join(
    endes_load(2016, "Country specific - Maternity"),
    by = c("CASEID" = "CASEID", "HIDX" = "IDX94")
  ) %>%
  left_join(
    endes_load(2016, "Country specific - Health"),
    by = c("CASEID" = "CASEID", "HIDX" = "IDX95")
  ) %>%
  left_join(
    endes_load(2016, "Respondent's basic data"),
    by = "CASEID"
  ) %>%
  mutate(HHID = CASEID %>% substr(1,15)) %>%
  left_join(
    endes_load(2016, "Household's basic data"),
    by = "HHID"
  ) %>% 
  left_join(
    endes_load(2016, "Household characteristics"),
    by = "HHID"
  ) %>%
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
  ) -> endes2016

## 2017
## 2016
endes_load(2017, "Health") %>%
  left_join(
    endes_load(2017, "Height and Weight"),
    by = c("CASEID" = "CASEID", "HIDX" = "HWIDX")
  ) %>%
  left_join(
    endes_load(2017, "Country specific - Maternity"),
    by = c("CASEID" = "CASEID", "HIDX" = "IDX94")
  ) %>%
  left_join(
    endes_load(2017, "Country specific - Health"),
    by = c("CASEID" = "CASEID", "HIDX" = "IDX95")
  ) %>%
  left_join(
    endes_load(2017, "Respondent's basic data"),
    by = "CASEID"
  ) %>%
  mutate(HHID = CASEID %>% substr(1,15)) %>%
  left_join(
    endes_load(2017, "Household's basic data"),
    by = "HHID"
  ) %>% 
  left_join(
    endes_load(2017, "Household characteristics"),
    by = "HHID"
  ) %>%
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
  ) -> endes2017

### Examinar clustering
# endes2017 %>% group_by(V023, V022, V001, HHID) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023, V022, V001) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023, V022) %>% summarise(obs = n()) %>% nrow
# endes2017 %>% group_by(V023) %>% summarise(obs = n()) %>% nrow