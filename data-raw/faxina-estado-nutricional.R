library(tidyverse)
estado_nutricional <- readr::read_rds("data-raw/estado_nutricional.rds")
glimpse(estado_nutricional)

muda_numero <- function(x){
  as.numeric(
    str_remove_all(x,"%|-")
  )
}

# crianças ----------------------------------------------------------------
cri_PI <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Peso X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    peso_muito_baixo_q = X6 %>% muda_numero(),
    peso_muito_baixo_p = X7 %>% muda_numero(),
    peso_baixo_q = X8 %>% muda_numero(),
    peso_baixo_p = X9 %>% muda_numero(),
    peso_adequado_q = X10 %>% muda_numero(),
    peso_adequado_p = X11 %>% muda_numero(),
    peso_elevado_q = X12 %>% muda_numero(),
    peso_elevado_p = X13 %>% muda_numero(),
    total = X14 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_PI %>%  glimpse()
readr::write_rds(cri_PI,"data/cri_peso_idade.rds")
writexl::write_xlsx(cri_PI,"data/cri_peso_idade.xlsx")

cri_PA <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Peso X Altura") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_PA %>%  glimpse()
readr::write_rds(cri_PA,"data/cri_peso_altura.rds")
writexl::write_xlsx(cri_PA,"data/cri_peso_altura.xlsx")

cri_AI <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "Altura X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    altura_muito_baixa_q = X6 %>% muda_numero(),
    altura_muito_baixa_p = X7 %>% muda_numero(),
    altura_baixa_q = X8 %>% muda_numero(),
    altura_baixa_p = X9 %>% muda_numero(),
    altura_adequada_q = X10 %>% muda_numero(),
    altura_adequada_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_AI %>%  glimpse()
readr::write_rds(cri_AI,"data/cri_altura_idade.rds")
writexl::write_xlsx(cri_AI,"data/cri_altura_idade.xlsx")

cri_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Criança",
         indice == "IMC X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
cri_IMC %>%  glimpse()
readr::write_rds(cri_IMC,"data/cri_imc.rds")
writexl::write_xlsx(cri_IMC,"data/cri_imc.xlsx")

# adolescente -------------------------------------------------------------
adol_AI <- estado_nutricional %>%
  filter(fase_da_vida == "Adolescente",
         indice == "Altura X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    altura_muito_baixa_q = X6 %>% muda_numero(),
    altura_muito_baixa_p = X7 %>% muda_numero(),
    altura_baixa_q = X8 %>% muda_numero(),
    altura_baixa_p = X9 %>% muda_numero(),
    altura_adequada_q = X10 %>% muda_numero(),
    altura_adequada_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adol_AI %>%  glimpse()
readr::write_rds(adol_AI,"data/adolescente_altura_idade.rds")
writexl::write_xlsx(adol_AI,"data/adolescente_altura_idade.xlsx")

adol_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Adolescente",
         indice == "IMC X Idade") %>%
  mutate(
    ano = ano %>% muda_numero(),
    magreza_ac_q = X6 %>% muda_numero(),
    magreza_ac_p = X7 %>% muda_numero(),
    magreza_q = X8 %>% muda_numero(),
    magreza_p = X9 %>% muda_numero(),
    adequado_q = X10 %>% muda_numero(),
    adequado_p = X11 %>% muda_numero(),
    risco_sobrepeso_q = X12 %>% muda_numero(),
    risco_sobrepeso_p = X13 %>% muda_numero(),
    sobrepeso_q = X14 %>% muda_numero(),
    sobrepeso_p = X15 %>% muda_numero(),
    obesidade_q = X16 %>% muda_numero(),
    obesidade_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adol_IMC %>%  glimpse()
readr::write_rds(adol_IMC,"data/adolescente_imc.rds")
writexl::write_xlsx(adol_IMC,"data/adolescente_imc.xlsx")

# adultos -----------------------------------------------------------------
adulto_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Adulto") %>%
  mutate(
    ano = ano %>% muda_numero(),
    baixo_peso_q = X6 %>% muda_numero(),
    baixo_peso__p = X7 %>% muda_numero(),
    adequado_peso__q = X8 %>% muda_numero(),
    adequado_peso__p = X9 %>% muda_numero(),
    sobrepeso_q = X10 %>% muda_numero(),
    sobrepeso_p = X11 %>% muda_numero(),
    obesidade_I_q = X12 %>% muda_numero(),
    obesidade_I_p = X13 %>% muda_numero(),
    obesidade_II_q = X14 %>% muda_numero(),
    obesidade_II_p = X15 %>% muda_numero(),
    obesidade_III_q = X16 %>% muda_numero(),
    obesidade_III_p = X17 %>% muda_numero(),
    total = X18 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
adulto_IMC %>%  glimpse()
readr::write_rds(adulto_IMC,"data/adulto_imc.rds")
writexl::write_xlsx(adulto_IMC,"data/adulto_imc.xlsx")

# Idosos ------------------------------------------------------------------
idoso_IMC <- estado_nutricional %>%
  filter(fase_da_vida == "Idoso") %>%
  mutate(
    ano = ano %>% muda_numero(),
    baixo_peso_q = X6 %>% muda_numero(),
    baixo_peso__p = X7 %>% muda_numero(),
    adequado_peso__q = X8 %>% muda_numero(),
    adequado_peso__p = X9 %>% muda_numero(),
    sobrepeso_q = X10 %>% muda_numero(),
    sobrepeso_p = X11 %>% muda_numero(),
    total = X12 %>% muda_numero()
  ) %>%
  select(-(X6:X18), -indice_cri, -indice_ado) %>%
  rename(
    regiao = X1,
    codigo_uf = X2,
    uf = X3,
    codigo_ibge = X4,
    municipio = X5
  )
idoso_IMC %>%  glimpse()
readr::write_rds(idoso_IMC,"data/idoso_imc.rds")
writexl::write_xlsx(idoso_IMC,"data/idoso_imc.xlsx")
