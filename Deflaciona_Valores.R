
##### SET UP #####
library(data.table)
library(stringr)
library(plyr)

setwd("G:/Meu Drive/e-CompFin/Projetos/TCE_Saneamento/Dados/Dados/Input/")


##### Arquivos #####

# Arquivo de Índices
anos        <- c(1995:2021)
indice_ref  <- "INCC"
names(anos) <- "Ano_Ref"

# Arquivos de Apoio - Índices de Preços
indices <- setDT(read.csv2("Deflatores_ate_2021.csv", dec = "."))
t(names(indices))
str(indices)

names(indices)[names(indices) %like% "Data"] <- "Ano"
names(indices)[names(indices) %like% "INPC"] <- "INPC"
names(indices)[names(indices) %like% "IPCA"] <- "IPCA"
names(indices)[names(indices) %like% "IGPM"] <- "IGPM"
names(indices)[names(indices) %like% "INCC"] <- "INCC"

indices <- subset(indices, Ano %in% anos)
indices

colunas <- names(indices)[names(indices) != "Ano"]


ajeita <- function(x) {
  
  x1 <- sub(",", ".", x)
  x2 <- as.numeric(x1)
  
}

teste <- copy(indices)

teste[ , (colunas) := lapply(.SD, ajeita), .SDcols = colunas]  # Modify data
str(teste)

x <- teste$INCC

acumula_infl <- function(x) {
  
  x1 <- 1 + (x/100)
  x2 <- NULL

  for(i in c(length(x2):1)) {
    
    if(i==length(x)) {
      
      x2[i] <- 1
      x3    <- 1

    } else {
      
      x2[i] <- x1[i]
      x3    <- x2[i]*x1[i-1]
      
    }
    
    x4[i] <- x3
    x4 <- c(x4, x3)
      
  } 
  
  x4 <- x3
  
}




acumula <- function(x) {
  x1 <- x
  x2 <- 1 + (x1/100) 
  
  for(i in c(length(x2):1)) {
    
    if(i==length(x2)) {
      x4 <- NULL
    } else {
      x4 <- x4
    }
    
    x3 <- x2[i]
    x4 <- c(x4, x3)
     
  }
  
  for(i in c(1:length(x2))) {
    
    if(i==1) {
      x5 <- 1
      x6 = NULL
      
    } else {
      x5 <- x4[i-1]*x4[i]
    }
    
    x6 <- c(x5, x6)
    
  }  
}


indices[ , IPCA := ajeita(IPCA)]


indices$INCC <- ajeita(indices$INCC)


if(indice_ref == "INCC") {
  indice_alvo <- as.numeric(sub(",", ".", indices$INCC))
} else if {
  indi
}



# indices[, Indice_Ref := 
#           as.numeric(sub(",", ".", ifelse(indice_ref == "INPC", INPC,
#                                           ifelse(indice_ref == "IPCA", IPCA,
#                                                  ifelse(indice_ref == "IGPM", IGPM,
#                                                         ifelse(indice_ref == "INCC", INCC,
#                                                                NA))))))
#         ]
# 
# 
# indices[, AnoMes := as.Date(paste0("12/31/", Ano_Ref), format = "%m/%d/%Y")]
# indices

# Arquivo de Saneamento
rs <- setDT(read.csv2("Dados_Saneamento_Completos_com_Prestador.csv"))
head(rs)

nomes <- names(rs)
nomes <- substr(nomes, start = 1, stop = 6)

names(rs) <- nomes
head(rs)

nomes_fin <- names(rs)[(names(rs) %like% "FN" |
                          names(rs) %like% "Ano")]

fin <- rs[, nomes_fin, with=FALSE]

names(fin)[substr(names(fin), start = 6, stop = 6) == "."]


nomes_fin <- names(fin)[substr(names(fin), start = 6, stop = 6) == "." |
                          names(fin) %like% "Ano"]





fin[, Ano2 := paste0("01","/", "12", "/", Ano)]
fin[, Ano3 := as.Date(Ano2, format = "%d/%m/%Y")]

fin2 <- merge(fin,
              indices,
              by.x = "Ano",
              by.y = "Ano_Ref",
              all = T)

fin <- lapply(fin[, nomes_fin], gsub(".", ""))



teste <- fin[Ano==2000]
set.seed(99)

teste[, FN_Teste := rnorm(n = nrow(teste), mean = 1000, sd=100)]

teste[, FN001_Defl := deflate(FN_Teste,
                              nominal_dates = Ano3,
                              real_date = "12/2020")]


summary(fin)
head(fin)
ncol(fin)
fin <- lapply(fin[2:ncol(fin)], as.numeric)

summary(fin)




