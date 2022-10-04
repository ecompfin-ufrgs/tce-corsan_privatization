
##### SET UP #####
library(data.table)
library(lubridate)
library(stringr)
library(plyr)
library(fBasics)
library(tidyverse)

options(OutDec = ".",
        scipen = 8)


##### Diretórios #####
path_proj      <- "G:/Meu Drive/e-CompFin/Projetos/TCE_Saneamento/"
path_func_base <- paste0(path_proj, "Scripts/Functions/")

path_wd        <- paste0(path_proj, "Dados/")
path_input     <- paste0(path_wd, "Input/")
path_process   <- paste0(path_wd, "Processing/")
path_output    <- paste0(path_wd, "Output/")


setwd(path_input)
getwd()

##### Parâmetros para Função (quando for colocado em forma decente) #####
produto_cartesiano <- F
limpa_snis         <- F
limpa_emergencial  <- T 
tipo_servico       <- "Agua"


##### Funções #####

fnc_rm_accent <- function(str, pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}


source(paste0(path_func_base, "fnc_converte.R"))

##### Arquivos #####
file_indices  <- "Deflatores_Acumulados_Valores_2021_12.csv"
file_indices  <- paste0(path_input, file_indices)
file_deflator <- "deflator implicito pib - ipeadata[01-07-2022-02-34].csv"
file_deflator <- paste0(path_input, file_deflator)
file_indices_prontos <- "Indices_de_Inflacao_Acumulados_Valores_Dez_2021_ok.csv"
file_indices_prontos <- paste0(path_input, file_indices_prontos)


# Arquivo de Índices
anos        <- c(1995:2021)
indice_ref  <- "Deflator_PIB"
names(anos) <- "Ano_Ref"

# Arquivos de Apoio - Índices de Preços
indices <- setDT(read.csv2(file_deflator, dec = ","))
# deflator_implicito <- setDT(read.csv(file_deflator))

t(names(indices))
str(indices)

names(indices)[names(indices) %like% "Data"] <- "Ano_Ref"


indices <- indices[Ano_Ref %in% anos]
indices[, Ano_Ref       := as.integer(Ano_Ref)]
indices[, Deflator_PIB  := 1 - (as.numeric(Deflator_Implicito_PIB)/100)]
indices[, Deflator_Temp := 1 + (as.numeric(Deflator_Implicito_PIB)/100)]

x <- indices[order(Ano_Ref, decreasing = T)]$Deflator_Temp
x <- c(1, x)
y <- rep(NA, length(x))

for(i in 1:nrow(indices)) {
  
  if(i == 1) {
    
    y[i] <- x[1]
    
  } else {
    
    y[i] <- x[i] * y[i-1]
    
  }
  
  if(i == nrow(indices)) {
    
    y <- rev(y[!is.na(y)])
    
  }
 
}

indices[, Deflator_PIB_Acum := y]


# Arquivo de Saneamento
rs_bruto <- setDT(read.csv2("Desagregado-20220615111409-utf-8.csv",
                            fileEncoding = "UTF-8"))
head(rs_bruto)
dim(rs_bruto)
str(rs_bruto)

# Copia o data set
rs <- copy(rs_bruto)

# Campos de referência
rs[, ID := 1:nrow(rs_bruto)]
rs[, Linha_Original := T]

# Formata e corrige valores de variáveis
rs[, Cod_Municipio := fnc_rm_accent(trimws(Código.do.Município))]
rs[, Municipio     := fnc_rm_accent(trimws(Município))]
rs[, Cod_Prestador := trimws(Código.do.Prestador)]
rs[, Prestador := fnc_rm_accent(trimws(Prestador))]
rs[, Ano_Ref   := trimws(Ano.de.Referência)]
rs[, Tipo_Serv := fnc_rm_accent(trimws(Tipo.de.serviço))]
rs[, Tipo_Serv := fnc_rm_accent(ifelse(Tipo_Serv=="Esgotos", "Esgoto", Tipo_Serv))]

rs[, Nat_Jur := as.factor(fnc_rm_accent(Natureza.jurídica))]

rs[, AG001 := as.numeric(fnc_converte(AG001...População.total.atendida.com.abastecimento.de.água))]
rs[, ES001 := as.numeric(fnc_converte(ES001...População.total.atendida.com.esgotamento.sanitário))]

rs[, POP_TOT := 
     fnc_converte(POP_TOT...População.total.do.município.do.ano.de.referência..Fonte..IBGE..)]
rs[, POP_URB := 
     fnc_converte(POP_URB...População.urbana.do.município.do.ano.de.referência..Fonte..IBGE..)]

rs[, FN001 := fnc_converte(FN001...Receita.operacional.direta.total)]
rs[, FN002 := fnc_converte(FN002...Receita.operacional.direta.de.água)]
rs[, FN003 := fnc_converte(FN003...Receita.operacional.direta.de.esgoto)]
rs[, FN006 := fnc_converte(FN006...Arrecadação.total)]
rs[, FN007 := fnc_converte(FN007...Receita.operacional.direta.de.água.exportada..bruta.ou.tratada.)]
rs[, FN023 := fnc_converte(FN023...Investimento.realizado.em.abastecimento.de.água.pelo.prestador.de.serviços)]
rs[, FN024 := fnc_converte(FN024...Investimento.realizado.em.esgotamento.sanitário.pelo.prestador.de.serviços)]
rs[, FN038 := fnc_converte(FN038...Receita.operacional.direta...esgoto.bruto.importado)]
rs[, FN001_Calc   := rowSums(rs[, .(FN002, FN003, FN007, FN038)], na.rm = T, dims = 1)]
rs[, FN001_Indicador := round(FN001/FN001_Calc, 3)]
rs[, Valida_FN001 := ifelse(FN001_Indicador == 1.000, T, F)]
rs[Valida_FN001 == F, FN001 := FN001_Calc]


rs[Valida_FN001==F, 
   .(ID, Ano_Ref, Municipio, 
     FN001, FN002, FN003, FN007, FN038, FN001_Calc, FN001_Indicador, Valida_FN001,
     FN001...Receita.operacional.direta.total,
     FN002...Receita.operacional.direta.de.água,
     FN003...Receita.operacional.direta.de.esgoto,
     FN007...Receita.operacional.direta.de.água.exportada..bruta.ou.tratada.,
     FN038...Receita.operacional.direta...esgoto.bruto.importado)][order(-ID)]

rs[, FN001_Indicador := round(FN001/FN001_Calc, 3)]
rs[, Valida_FN001 := ifelse(FN001_Indicador == 1.000, T, F)]

rs[Valida_FN001==F, 
   .(ID, Ano_Ref, Municipio, 
     FN001, FN002, FN003, FN007, FN038, FN001_Calc, FN001_Indicador, Valida_FN001,
     FN001...Receita.operacional.direta.total,
     FN002...Receita.operacional.direta.de.água,
     FN003...Receita.operacional.direta.de.esgoto,
     FN007...Receita.operacional.direta.de.água.exportada..bruta.ou.tratada.,
     FN038...Receita.operacional.direta...esgoto.bruto.importado)][order(-ID)]

# Remove linhas de totalizadores
unique(rs[Município != "---" | Ano.de.Referência != "---", .(Município, Ano.de.Referência)])
rs <- rs[Município != "---" | Ano.de.Referência != "---"]
nrow(rs)

rs[, Ano_Ref := as.integer(Ano.de.Referência)]


# Produto cartesiano

    municipios <- sort(unique(rs$Cod_Municipio))
  anos       <- sort(unique(rs$Ano.de.Referência))

if(produto_cartesiano==T) {  
  prod_cart  <- setDT(expand.grid(Cod_Municipio = municipios,
                                  Ano_Ref = as.integer(anos[anos!="2021"])))
  prod_cart  <- prod_cart[order(Cod_Municipio, Ano_Ref)]
  
  
  rs <- merge(prod_cart,
              rs,
              by = c("Cod_Municipio", "Ano_Ref"),
              all.x = T, all.y = F)
  
  rs[is.na(Linha_Original), Linha_Original := FALSE]
  
}

# Inclui índices usados no deflacionamento
rs <- merge(rs,
            indices,
            by = "Ano_Ref",
            all.x = T, all.y = F)


rs <- rs[order(Municipio, Ano_Ref, Tipo_Serv)]


contagem <- rs[, .(ID, Linha_Original, Municipio, Cod_Municipio,
                   POP_TOT, POP_URB, Ano_Ref, 
                   Cod_Prestador, Prestador, Tipo_Serv, Nat_Jur,
                   AG001, ES001, FN001, FN002, FN003, FN006, FN007,
                   FN023, FN024, FN038, 
                   Deflator_PIB_Acum)]

resumo_rs1 <-
  rs[Ano_Ref >= 2000 
           & Tipo_Serv %in% c("Agua", "Agua e Esgoto"), 
           .(POP_TOT = mean(POP_TOT, na.rm = T),
             AG001 = sum(AG001, na.rm = T),
             QTD = .N),
           by = c("Ano_Ref", "Municipio")][order(Ano_Ref, Municipio)]

resumo_rs2 <-
  resumo_rs1[Ano_Ref >= 2000, 
           .(POP_TOT = sum(POP_TOT, na.rm = T),
             POP_ATENDIDA = sum(AG001, na.rm = T),
             QTD = .N),
           by = c("Ano_Ref")][order(Ano_Ref)]


resumo1 <-
  contagem[Prestador == "Companhia Rio-Grandense de Saneamento"
           & Ano_Ref >= 2000, 
           .(POP_TOT = sum(POP_TOT, na.rm = T),
             POP_ATENDIDA = sum(AG001, na.rm = T),
             QTD = .N),
           by = "Ano_Ref"][order(Ano_Ref)]

resumo2 <-
  contagem[Prestador %like% "Prefeitura Municipal de "
           & Ano_Ref >= 2000, 
           .(POP_TOT = sum(POP_TOT, na.rm = T),
             POP_ATENDIDA = sum(AG001, na.rm = T),
             QTD = .N),
           by = "Ano_Ref"][order(Ano_Ref)]


# Contagem de NAs
contagem_nas <-
  contagem[Linha_Original==T, 
           .(Geral   = sum(!is.na(Tipo_Serv))/length(unique(municipios)),
             POP_TOT = sum(!is.na(POP_TOT))/length(unique(municipios)),
             POP_URB = sum(!is.na(POP_URB))/length(unique(municipios)),
             AG001 = sum(!is.na(AG001))/length(unique(municipios)),
             ES001 = sum(!is.na(ES001))/length(unique(municipios)),
             FN001 = sum(!is.na(FN001))/length(unique(municipios)),
             FN002 = sum(!is.na(FN002))/length(unique(municipios)),
             FN003 = sum(!is.na(FN003))/length(unique(municipios)),
             FN006 = sum(!is.na(FN006))/length(unique(municipios)),
             FN007 = sum(!is.na(FN007))/length(unique(municipios)),
             FN023 = sum(!is.na(FN023))/length(unique(municipios)),
             FN024 = sum(!is.na(FN024))/length(unique(municipios)),
             FN038 = sum(!is.na(FN038))/length(unique(municipios))),
           by = "Ano_Ref"]

contagem_nas <- contagem_nas[order(Ano_Ref)]; contagem_nas

write.csv(contagem_nas, "Contagem_de_NAs.csv", row.names = F)

sapply(contagem[Ano_Ref >= 1995], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 1995]))
sapply(contagem[Ano_Ref >= 1999], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 1999]))
sapply(contagem[Ano_Ref >= 2001], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2001]))
sapply(contagem[Ano_Ref >= 2005], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2005]))
sapply(contagem[Ano_Ref >= 2009], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2009]))

sapply(contagem[Ano_Ref >= 1995], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 1995]))
sapply(contagem[Ano_Ref >= 1999], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 1999]))
sapply(contagem[Ano_Ref >= 2001], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2001]))
sapply(contagem[Ano_Ref >= 2005], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2005]))
sapply(contagem[Ano_Ref >= 2009], function(x) sum(is.na(x))/nrow(contagem[Ano_Ref >= 2009]))

contagem_geral <- contagem[complete.cases(contagem)]

snis <- copy(contagem[Linha_Original==T])
snis <- snis[order(Cod_Municipio, Ano_Ref)]

if(limpa_snis == T) {
  
  ids <- snis[Municipio == "Campo Novo" & 
              Prestador != "Prefeitura Municipal de Campo Novo"]$ID
  
  ids <- c(ids,
           snis[Municipio == "Acegua" & Ano_Ref %in% c(2016, 2018, 2019) &
                Prestador == "Prefeitura Municipal de Acegua"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Agua Santa" & Ano_Ref == 2010 &
                Prestador == "Prefeitura Municipal de Agua Santa"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Arroio dos Ratos" & Ano_Ref %in% c(2009, 2011)  &
                Prestador == "Prefeitura Municipal de Arrio dos Ratos"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Barao" & Ano_Ref %in% c(2016, 2019, 2020)  &
                Prestador == "Prefeitura Municipal de Barao"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Bom Jesus" & Ano_Ref %in% c(2013:2017)  &
                Prestador == "Prefeitura Municipal de Bom Jesus"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Camaqua" & Ano_Ref %in% c(2013)  &
                Prestador == "Prefeitura Municipal de Camaqua"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Cambara do Sul" & Ano_Ref %in% c(2011:2012)  &
                Prestador == "Prefeitura Municipal de Campestre da Serra"]$ID)

  ids <- c(ids,
           snis[Municipio == "Campinas das Missoes" & Ano_Ref %in% c(2010:2016, 2018:2020)  &
                Prestador == "Prefeitura Municipal de Campinas das Missoes"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Candelaria" & Ano_Ref %in% c(2012)  &
                Prestador == "Prefeitura Municipal de Candelaria"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Cangucu" & Ano_Ref %in% c(2009:2020)  &
                Prestador == "Prefeitura Municipal de Cangucu"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Capela de Santana" & Ano_Ref %in% c(2010)  &
                Prestador == "Prefeitura Municipal de Capela de Santana"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Carlos Barbosa" & Ano_Ref %in% c(2011:2020)  &
                Prestador == "Prefeitura Municipal de Carlos Barbosa"]$ID)
    
  ids <- c(ids,
           snis[Municipio == "Campestre da Serra" & Ano_Ref %in% c(2014:2020)  &
                Prestador == "Prefeitura Municipal de Bom Jesus"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Catuipe" & Ano_Ref %in% c(2011:2020)  &
                Prestador == "Prefeitura Municipal de Catuipe"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Cerrito" & Ano_Ref %in% c(2018)  &
                Prestador == "Prefeitura Municipal de Cerrito"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Cerro Grande do Sul" & Ano_Ref %in% c(2011)  &
                Prestador == "Prefeitura Municipal de Cerro Grande do Sul"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Cotipora" & Ano_Ref %in% c(2010)  &
                Prestador == "Prefeitura Municipal de Cotipora"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Dileramando de Aguiar" & Ano_Ref %in% c(2013:2016)  &
                Prestador == "Prefeitura Municipal de Dileramando de Aguiar"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Dois Irmaos" & Ano_Ref %in% c(2013:2019)  &
                Prestador == "Prefeitura Municipal de Dois Irmaos"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Dom Feliciano" & Ano_Ref %in% c(2018)  &
                Prestador == "Prefeitura Municipal de Dom Feliciano"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Itapuca"& Ano_Ref %in% c(2014:2017) &
             Prestador != "Prefeitura Municipal de Itapuca"]$ID)
  
  
  ids <- c(ids, 
           snis[Municipio == "Herval"& Ano_Ref %in% c(2015:2020) &
                  Prestador != "Prefeitura Municipal de Herval"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Estrela"& Ano_Ref %in% c(2010:2013, 2016:2020) &
                Prestador != "Prefeitura Municipal de Estrela"]$ID)

  ids <- c(ids, 
           snis[Municipio == "Fagundes Varela"& Ano_Ref %in% c(2012, 2013) &
                  Prestador != "Prefeitura Municipal de Fagundes Varela"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Faxinal do Soturno"& Ano_Ref %in% c(2015:2017) &
                  Prestador != "Prefeitura Municipal de Faxinal do Soturno"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Flores da Cunha"& Ano_Ref %in% c(2012) &
                  Prestador != "Prefeitura Municipal de Flores da Cunha"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Frederico Westfalen"& Ano_Ref %in% c(2010, 2012, 2019, 2020) &
                  Prestador != "Prefeitura Municipal de Frederico Westfalen"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Garibaldi"& Ano_Ref %in% c(2011:2020) &
                  Prestador != "Prefeitura Municipal de Garibaldi"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Guapore"& Ano_Ref %in% c(2009, 2016:2020) &
                  Prestador != "Prefeitura Municipal de Guapore"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Horizontina"& Ano_Ref %in% c(2009, 2016, 2017, 2020) &
                  Prestador != "Prefeitura Municipal de Horizontina"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Ijui"& Ano_Ref %in% c(2014) &
                  Prestador != "Prefeitura Municipal de Ijui"]$ID)
  
  ids <- c(ids, 
           snis[Municipio == "Ipe"& Ano_Ref %in% c(2013, 2014, 2016, 2017) &
                  Prestador != "Prefeitura Municipal de Ipe"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Sao Francisco de Paula" & Ano_Ref == 2011 &
                Prestador != "Sanea Projetos e Construcoes Ltda"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Seberi" & Ano_Ref %in% c(2011:2020) &
                Prestador != "Prefeitura Municipal de Seberi"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Teutonia" & Ano_Ref %in% c(2011, 2012, 2017:2020) &
                Prestador != "Prefeitura Municipal de Teutotnia"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Uruguaiana" & Ano_Ref == 2011 &
                Prestador == "Companhia Rio-Grandense de Saneamento"]$ID)
  
  ids <- c(ids,
           snis[Municipio == "Tupandi" & Ano_Ref %in% c(2019, 2020) &
                Prestador %like% "Associacao de Desenvolvimento Comunitario de Tupandi"]$ID)
  
  snis <- snis[!ID %in% ids]
  
}

# if(tipo_sevico == "Agua") {
#   
#   snis <- snis[Tipo_Serv == "Agua"]
#   
# }


write.csv2(snis[Linha_Original==T & POP_TOT >= 100000,
                .(Municipio, POP_TOT, POP_URB, Ano_Ref, 
                  Prestador, Tipo_Serv, Nat_Jur,
                  AG001, ES001, FN001, FN002, FN003, FN006, #FN007,
                  FN023, FN024#, FN038
                  )], 
           file="Base_com_Variaveis_Reduzidas_sem_ID_etc_100mil.csv",
           row.names = F,
           fileEncoding = "UTF-8")


write.csv2(snis[Linha_Original==T], 
           file="Base_com_Variaveis_Reduzidas.csv",
           row.names = F,
           fileEncoding = "UTF-8")

write.csv2(snis[Linha_Original==T & POP_TOT >= 100000], 
           file="Base_com_Variaveis_Reduzidas_100MilHab.csv",
           row.names = F,
           fileEncoding = "UTF-8")

saveRDS(snis[Linha_Original==T], 
        file="Base_com_Variaveis_Reduzidas.rds")

