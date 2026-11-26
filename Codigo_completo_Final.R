
# Parte 0: preparação 

rm(list = ls())   
stopifnot(dir.exists("Dados/brutos"))
library(lmtest)     # coeftest(), coefci() e waldtest(): testes t, intervalos e testes F (Bloco 3)
library(sandwich)   # vcovHC(): matriz de variância robusta, tipo HC1 (Bloco 3)
library(sf)         # pacote que entende e desenha mapas (Bloco 2)
library(geobr)      # pacote que baixa as malhas do IBGE; precisa de internet (Bloco 2)




### BLOCO 1: MONTAR A BASE 


## Primeira parte do código: Davi, aqui é pra ler as tabelas da SIDRA

# Tabela 10264: % de ocupados que não contribuem para a previdência 
t10264 <- read.csv2("Dados/brutos/tabela10264.csv", skip = 8, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t10264) <- c("cod_ibge", "municipio", "informal")


# Tabela 10261: ocupados por posição na ocupação 
t10261 <- read.csv2("Dados/brutos/tabela10261.csv", skip = 7, nrows = 645, header = FALSE, fileEncoding = "UTF-8", na.strings = "-")
names(t10261) <- c("cod_ibge", "nome", "ocupados", "priv_sem_carteira", "dom_sem_carteira",
                   "empregador_sem_cnpj", "conta_propria_sem_cnpj", "familiar_auxiliar")
sum(is.na(t10261))           # conta quantos "-" viraram NA 
t10261[is.na(t10261)] <- 0   # no SIDRA, "-" = zero absoluto (nota do IBGE no rodapé da tabela): troca NA por 0


# Tabela 10295: rendimento domiciliar per capita, médio e mediano
t10295 <- read.csv2("Dados/brutos/tabela10295.csv", skip = 7, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t10295) <- c("cod_ibge", "nome", "renda_pc", "renda_mediana") 


# Tabela 9517: força de trabalho e desocupados, 14 anos ou mais
t9517 <- read.csv2("Dados/brutos/tabela9517.csv", skip = 8, nrows = 645, header = FALSE, fileEncoding = "UTF-8", na.strings = "-")
names(t9517) <- c("cod_ibge", "nome", "forca_trabalho", "desocupados")
sum(is.na(t9517))                   
t9517[is.na(t9517$desocupados), ]     # mostra quais municípios têm "-"
t9517[is.na(t9517)] <- 0              # no SIDRA, "-" = zero absoluto (nota do IBGE no rodapé da tabela): troca NA por 0



# Tabela 10061: % de pessoas de 18 anos ou mais com superior completo
t10061 <- read.csv2("Dados/brutos/tabela10061.csv", skip = 8, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t10061) <- c("cod_ibge", "nome", "superior")



# Tabela 9514: homens de 15 a 29 anos, em três faixas de idade
t9514 <- read.csv2("Dados/brutos/tabela9514.csv", skip = 7, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t9514) <- c("cod_ibge", "nome", "h15_19", "h20_24", "h25_29")


# Tabela 4714: população, área (km²) e densidade (hab/km²)
t4714 <- read.csv2("Dados/brutos/tabela4714.csv", skip = 4, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t4714) <- c("cod_ibge", "nome", "pop", "area", "dens")



# Tabela 9515: razão de sexo (homens por 100 mulheres)
t9515 <- read.csv2("Dados/brutos/tabela9515.csv", skip = 4, nrows = 645, header = FALSE, fileEncoding = "UTF-8")
names(t9515) <- c("cod_ibge", "nome", "razao_sexo")




## Segunda parte: Juntar as tabelas do SIDRA pelo código IBGE (não da pelo nome porque pode ter erro na forma que está escrito)

base <- merge(t10264, t10261[, -2], by = "cod_ibge")   # junta 10264 com a ocupação por posição (10261, sem a coluna de nome)
base <- merge(base, t10295[, -2], by = "cod_ibge")
base <- merge(base, t9517[, -2], by = "cod_ibge")
base <- merge(base, t10061[, -2], by = "cod_ibge")
base <- merge(base, t9514[, -2], by = "cod_ibge")
base <- merge(base, t4714[, -2], by = "cod_ibge")
base <- merge(base, t9515[, -2], by = "cod_ibge")



## Terceira Parte: serva pra resolver o problema de nome 
## padroniza o nome do município para juntar bases sem código IBGE (SSP e SENATRAN)

chave <- function(x) {
  x <- sub(" \\(SP\\)", "", x)                                                  # tira o " (SP)" do nome do IBGE
  x <- chartr("áàâãéêíóôõúüçÁÀÂÃÉÊÍÓÔÕÚÜÇ", "aaaaeeiooouucAAAAEEIOOOUUC", x)   # tira os acentos
  x <- gsub("-", " ", x)                                                       # troca hífen por espaço
  toupper(x)                                                                   # passa tudo para maiúsculas
}

base$chave <- chave(base$municipio)   # cria a coluna chave na base, a partir do nome do IBGE

head(base[, c("municipio", "chave")])   # mostra o nome original e a chave lado a lado. Serve pra verificar se a chave está deixando os nomes tratados referentes a suas cidades pré tratamento.   



## Quarta Parte: SSP-SP: roubos, furtos e homicídios de 2022 por município (soma dos 12 meses)
ssp <- read.csv2("Dados/brutos/ssp_2022_municipios.csv", fileEncoding = "UTF-8")

# Três municípios têm grafia diferente na SSP e no IBGE (a chave não resolve). Ou seja, to corrigindo para a grafia do IBGE
ssp$municipio[ssp$municipio == "Florínia"] <- "Florínea"
ssp$municipio[ssp$municipio == "Santa Rosa do Viterbo"] <- "Santa Rosa de Viterbo"
ssp$municipio[ssp$municipio == "São Luís do Paraitinga"] <- "São Luiz do Paraitinga"

ssp$chave <- chave(ssp$municipio)   # vAI usar a função feita na terceira parte pra padronizar os nomes. 
setdiff(ssp$chave, base$chave)   # Esse é pra descobrir se algum nome  não encontra par na base -- OBS para relembrar pra prova qualquer coisa: setdiff(a, b) devolve os elementos de a que não estão em b

base <- merge(base, ssp[, -1], by = "chave")   # junta os crimes à base pela chave de nome




## Quinta parte: Senatran 
frota <- read.csv2("Dados/brutos/frota_senatran_dez2022_sp.csv", fileEncoding = "UTF-8")
frota$chave <- chave(frota$municipio)                   #Chave lá de cima 
setdiff(base$chave, frota$chave)                       #Precisa sair vazio
base <- merge(base, frota[, -1], by = "chave")           #OBS para a prova qualquer coisa: o merge() mantém só as linhas cuja chave existe nas duas tabelas. Quem não tem par, de qualquer um dos lados, sai.




##Sexta Parte: Região Metropolitana 
rm_ibge <- read.csv2("Dados/brutos/rm_ibge_2022_sp.csv", fileEncoding = "UTF-8")
base <- merge(base, rm_ibge, by = "cod_ibge", all.x = TRUE)
base$regiao_metropolitana[is.na(base$regiao_metropolitana)] <- "Fora de RM"   # NA aqui significa "fora de RM", não dado faltante
table(base$regiao_metropolitana)


grep("regiao", names(base), value = TRUE)   # lista as colunas da base que têm "regiao" no nome
table(base$regiao_metropolitana)
ncol(base)



##Sétima parte: Dicionário

# Y: roubos + furtos de veículos por 10 mil veículos da frota
base$taxa_veic <- (base$roubo_veiculo + base$furto_veiculo) / base$frota_total * 10000


# Y alternativo (robustez): mesmos crimes por 100 mil habitantes
base$taxa_veic_hab <- (base$roubo_veiculo + base$furto_veiculo) / base$pop * 100000

# Y de robustez: outros roubos e furtos por 100 mil habitantes
base$taxa_outros <- (base$roubo_outros + base$furto_outros) / base$pop * 100000


# Contraste: homicídios dolosos por 100 mil habitantes
base$taxa_homic <- base$homicidio_doloso / base$pop * 100000

# X de robustez: informalidade pela definição do IBGE (% dos ocupados)
base$informal_ibge <- (base$priv_sem_carteira + base$dom_sem_carteira + base$empregador_sem_cnpj + base$conta_propria_sem_cnpj + base$familiar_auxiliar) / base$ocupados * 100

# Desigualdade: renda média dividida pela mediana
base$desig <- base$renda_pc / base$renda_mediana

# Taxa de desocupação (% da força de trabalho)
base$desocup <- base$desocupados / base$forca_trabalho * 100


# % de homens de 15 a 29 anos na população
base$homens_jovens <- (base$h15_19 + base$h20_24 + base$h25_29) / base$pop * 100

base$municipio[which.max(base$homens_jovens)]   # nome do município com a maior % de homens jovens


# Tamanho e densidade em log
base$ln_pop <- log(base$pop)
base$ln_dens <- log(base$dens) 

# Presídio: 1 se há mais de 120 homens por 100 mulheres 
base$presidio <- as.numeric(base$razao_sexo > 120)
sum(base$presidio)                                   



##Última Parte: Só organizar a base e oq vai sair no final (Colunas) 
base <- base[order(base$cod_ibge), ]   # ordena os municípios pelo código IBGE

# Ordem das colunas: identificação, Y, X, controles e, no fim, as contagens brutas que deram origem às taxas
base <- base[, c("cod_ibge", "municipio", "regiao_metropolitana",
                 "taxa_veic", "taxa_veic_hab", "taxa_outros", "taxa_homic",
                 "informal", "informal_ibge",
                 "renda_pc", "desig", "desocup", "superior", "homens_jovens",
                 "ln_pop", "ln_dens", "presidio", "razao_sexo",
                 "pop", "area", "dens", "frota_total",
                 "roubo_veiculo", "furto_veiculo", "roubo_outros", "furto_outros", "homicidio_doloso",
                 "ocupados", "priv_sem_carteira", "dom_sem_carteira", "empregador_sem_cnpj",
                 "conta_propria_sem_cnpj", "familiar_auxiliar",
                 "renda_mediana", "forca_trabalho", "desocupados", "h15_19", "h20_24", "h25_29")]


ncol(base) #Se bater 39 ta certo pq antes dava 40 mas aqui tem que ser sem a coluna chave 
nrow(base) #Tem que dar 645. Ou seja, 645 municípios. 

sum(is.na(base))   # conta os NA na base inteira: deve dar 0
summary(base[, c("taxa_veic", "informal", "renda_pc", "desig", "desocup", "superior",
                 "homens_jovens", "ln_pop", "ln_dens", "presidio")])   # resumo das variáveis do modelo
write.csv2(base, "Dados/base_final.csv", row.names = FALSE, fileEncoding = "UTF-8")   # salva a base final







### BLOCO 2: DESCRITIVAS, GRÁFICOS E MAPAS 


## Primeira Parte: Só puxando a base e criando a pasta dos gráficos

# Relê a base salva no Bloco 1: as descritivas usam exatamente o arquivo salvo
# (e este bloco pode ser rodado sozinho, se a base já estiver pronta)
base <- read.csv2("Dados/base_final.csv", fileEncoding = "UTF-8")  
dim(base)                                     
dir.create("Figuras", showWarnings = FALSE)   


## Segunda Parte: estatísticas descritivas de Y, X e dos controles
v <- c("taxa_veic", "informal", "renda_pc", "desig", "desocup", "superior",
       "homens_jovens", "ln_pop", "ln_dens", "presidio")  
summary(base[, v])                           
round(sapply(base[, v], sd), 2)              
sum(base$taxa_veic == 0)                     # OBS para a prova: $ pega uma coluna da base. O == 0 compara cada um dos 645 valores com zero e devolve TRUE ou FALSE

# Porte dos municípios e informalidade do estado inteiro (números citados nas p. 4 a 6 do artigo)
pequeno <- base$pop < 5000                                         
grande  <- base$pop > 100000                                       
sum(pequeno)                                                    
c(mean(base$informal[pequeno]), mean(base$taxa_veic[pequeno]))     
c(mean(base$informal[grande]),  mean(base$taxa_veic[grande]))      
sum(base$taxa_veic == 0 & pequeno)                                 
sum(base$informal * base$ocupados) / sum(base$ocupados)            
sum(base$informal_ibge * base$ocupados) / sum(base$ocupados)       

## Terceira Parte: Histogramas 

png("Figuras/fig1_hist_taxa_veic.png", width = 2000, height = 850, res = 220)  
par(mfrow = c(1, 2), las = 1, bty = "l", mar = c(4.5, 4.5, 2.5, 1))           # Esse serve para colocar os 2 gráficos lado a lado
hist(base$taxa_veic, breaks = 40, col = "gray55", border = "white", ylim = c(0, 200),
     main = "(a) Em nível", xlab = "Roubos e furtos por 10 mil veículos",
     ylab = "Número de municípios")                                            # histograma em nível
hist(log(1 + base$taxa_veic), breaks = 40, col = "gray55", border = "white", ylim = c(0, 80),
     main = "(b) Em log(1 + Y)", xlab = "log(1 + roubos e furtos por 10 mil veículos)",
     ylab = "Número de municípios")                                            # histograma em log(1 + Y) --> Ver se vamos utilizar mesmo depois com o Davi 
dev.off()                                                                      


##Quarta Parte: Dispersão de X contra Y 

#Grupos de região: cada um terá um tom de cinza e um símbolo
grupo <- ifelse(base$regiao_metropolitana == "RM São Paulo", "RM de São Paulo",
                ifelse(base$regiao_metropolitana == "Fora de RM", "Fora de RM", "Outras RMs"))
table(grupo)                                                   # nº de municípios em cada grupo
cores    <- c("RM de São Paulo" = "black", "Outras RMs" = "gray45", "Fora de RM" = "gray72")
simbolos <- c("RM de São Paulo" = 16, "Outras RMs" = 17, "Fora de RM" = 15)
ordem <- order(match(grupo, c("Fora de RM", "Outras RMs", "RM de São Paulo")))  # Região Metropolitana de SP desenhada por último

#Desenhar a dispersão e calcular a correlação simples
png("Figuras/fig2_dispersao_y_x.png", width = 1800, height = 1300, res = 220) 
par(las = 1, bty = "l", mar = c(4.5, 4.5, 1, 1))                               
plot(base$informal[ordem], base$taxa_veic[ordem],
     col = cores[grupo[ordem]], pch = simbolos[grupo[ordem]], cex = 0.9,
     xlab = "Informalidade (% de ocupados não contribuintes)",
     ylab = "Roubos e furtos por 10 mil veículos")                            
points(base$informal[base$presidio == 1], base$taxa_veic[base$presidio == 1],
       pch = 1, cex = 1.7, col = "black")                                      
abline(lm(taxa_veic ~ informal, data = base), lwd = 2)                         
legend("topright", bty = "n", cex = 0.85,
       legend = c("RM de São Paulo", "Outras RMs", "Fora de RM", "Município com presídio", "Reta de MQO (Modelo 1)"),
       col = c(cores, "black", "black"), pch = c(simbolos, 1, NA),
       lty = c(NA, NA, NA, NA, 1), lwd = c(NA, NA, NA, NA, 2))                 
dev.off()                                                                      

cor(base$taxa_veic, base$informal)                                             


## Quinta Parte: Y por região metropolitana (justificativa das dummies de RM)

#Ordenar as regiões pela mediana de Y e montar os rótulos

med <- sort(tapply(base$taxa_veic, base$regiao_metropolitana, median))    
regiao <- factor(base$regiao_metropolitana, levels = names(med))            
rotulos <- sub("^RM ", "", names(med))                                     
rotulos <- sub("Vale do Paraíba e Litoral Norte", "Vale do Paraíba e L. Norte", rotulos)  
rotulos <- paste0(rotulos, " (", table(regiao), ")")                        
med                                                                        
rotulos    

# Desenhar o boxplot de Y por região

png("Figuras/fig3_boxplot_regiao.png", width = 1900, height = 1250, res = 220)  
par(las = 1, bty = "l", mar = c(4.5, 14, 1, 1))                                
boxplot(base$taxa_veic ~ regiao, horizontal = TRUE, names = rotulos,
        col = "gray85", border = "gray15", pch = 16, cex = 0.5,
        xlab = "Roubos e furtos por 10 mil veículos", ylab = "")               
dev.off()                                                                      


## Sexta Parte: correlações e multicolinearidade

#Matriz de correlação e a relação das regiões com a informalidade
round(cor(base[, v]), 2)                                                     # matriz de correlação das 10 variáveis
cor(base$informal, as.numeric(base$regiao_metropolitana != "Fora de RM"))    
round(tapply(base$informal, base$regiao_metropolitana, mean), 1)             # média da informalidade em cada região

#Multicolinearidade: quanto da informalidade os controles conseguem prever
aux <- lm(informal ~ renda_pc + desig + desocup + superior + homens_jovens + ln_pop +
            ln_dens + presidio + factor(regiao_metropolitana), data = base)  # regressão auxiliar: X nos controles
summary(aux)$r.squared                                                     # R^2 da informalidade nos controles
1 / (1 - summary(aux)$r.squared)                                           



## Sétima Parte: variação da informalidade dentro das RMs
table(base$regiao_metropolitana)                                     # municípios por região
round(tapply(base$informal, base$regiao_metropolitana, sd), 2)          # desvio-padrão da informalidade em cada região
sd(base$informal)                                                # desvio-padrão total
sd(base$informal - ave(base$informal, base$regiao_metropolitana))         # desvio-padrão dentro das regiões
cor(base$taxa_veic - ave(base$taxa_veic, base$regiao_metropolitana),
    base$informal  - ave(base$informal,  base$regiao_metropolitana)) # correlação de Y e X dentro das regiões


## Oitava Parte: presídios, comparação com e sem os 27 municípios

sem27 <- base[base$presidio == 0, ]          # municípios sem presídio
so27  <- base[base$presidio == 1, ]          # municípios com presídio
nrow(sem27)                                  
nrow(so27)                                   
v2 <- c("taxa_veic", "informal", "homens_jovens", "ln_pop", "ln_dens", "renda_pc", "superior", "desocup")
round(rbind(todos        = sapply(base[, v2], median),
            sem_presidio = sapply(sem27[, v2], median),
            so_presidio  = sapply(so27[, v2], median)), 2)   # medianas dos três grupos

# Correlações com os 645 e sem os 27 com presídio}

cor(base$homens_jovens, base$taxa_veic)      # homens jovens × Y, com os 27
cor(sem27$homens_jovens, sem27$taxa_veic)    # homens jovens × Y, sem os 27
cor(base$informal, base$taxa_veic)           # informalidade × Y, com os 27
cor(sem27$informal, sem27$taxa_veic)         # informalidade × Y, sem os 27
cor(base$informal, base$homens_jovens)       # informalidade × homens jovens, com os 27
cor(sem27$informal, sem27$homens_jovens)     # informalidade × homens jovens, sem os 27


## Nona Parte: gráfico de homens jovens × Y, com e sem os presídios

#Retas de MQO simples com os 645 e sem os 27
m645 <- lm(taxa_veic ~ homens_jovens, data = base)    # reta com todos os municípios
m618 <- lm(taxa_veic ~ homens_jovens, data = sem27)   # reta sem os 27 com presídio
coef(m645)[2]                                         # inclinação com os 27
coef(m618)[2]                                         # inclinação sem os 27

#Desenhar os pontos e as duas retas
png("Figuras/fig4_homens_jovens_presidios.png", width = 1800, height = 1300, res = 220)  
par(las = 1, bty = "l", mar = c(4.5, 4.5, 1, 1))                                         
plot(sem27$homens_jovens, sem27$taxa_veic, pch = 16, cex = 0.8, col = "gray60",
     xlim = range(base$homens_jovens),
     xlab = "Homens de 15 a 29 anos (% da população)",
     ylab = "Roubos e furtos por 10 mil veículos")                    # municípios sem presídio
points(so27$homens_jovens, so27$taxa_veic, pch = 17, cex = 1.1, col = "black")  # municípios com presídio
abline(m645, lwd = 2, lty = 2)                                        # reta com os 645 (tracejada)
x618 <- range(sem27$homens_jovens)                                    # faixa observada sem os presídios
lines(x618, coef(m618)[1] + coef(m618)[2] * x618, lwd = 2)            # reta sem os 27, só nessa faixa
legend("topright", bty = "n", cex = 0.85,
       legend = c("Município sem presídio", "Município com presídio",
                  "Reta de MQO com os 645", "Reta de MQO sem os 27 com presídio"),
       col = c("gray60", "black", "black", "black"), pch = c(16, 17, NA, NA),
       lty = c(NA, NA, 2, 1), lwd = c(NA, NA, 2, 2))                  
dev.off()                                                             

## Décima Parte: os "-" do SIDRA que viraram zero
base[base$desocup == 0, c("municipio", "pop", "forca_trabalho", "desocupados", "presidio")]  
head(sort(base$desocup), 5)                  
summary(base$informal_ibge)                  
cor(base$informal, base$informal_ibge)       # Isso responde se as duas medidas de informalidade andam juntas


## Décima Primeira Parte: mapas de Y e da informalidade por município

# Pacotes sf e geobr: já carregados na Parte 0, lá no começo do código

sp <- read_municipality(code_muni = "SP", year = 2022, showProgress = FALSE)  # Malha dos 645 municípios de SP

mapa <- merge(sp, base, by.x = "code_muni", by.y = "cod_ibge")  # Junta a malha com a base pelo código do IBGE
nrow(mapa)                                   # deve dar 645

# Classes por quintis: cada cor tem 1/5 dos municípios. Quintis em vez de escala contínua porque Y é muito assimétrico:
# com escala contínua, quase todo o mapa ficaria claro.
cinzas <- c("gray92", "gray75", "gray55", "gray35", "gray12")

classes <- function(x) {
  q <- quantile(x, probs = seq(0, 1, 0.2))
  q[1] <- q[1] - 1e-9                        # garante que o mínimo entre na 1ª classe
  list(cl = cut(x, q, include.lowest = TRUE), q = q)
}

# Rótulos da legenda no formato "0,0 a 3,5" (vírgula decimal)
# (este nome substitui os rótulos do boxplot da Quinta Parte, que já foi desenhado)
rotulos <- function(q) {
  f <- function(v) format(round(v, 1), decimal.mark = ",", nsmall = 1)
  paste0(f(q[-length(q)]), " a ", f(q[-1]))
}

cy <- classes(mapa$taxa_veic)                # quintis de Y
cx <- classes(mapa$informal)                 # quintis de X
round(cy$q, 1)                               # limites das classes de Y
round(cx$q, 1)                               # limites das classes de X

class(rotulos)    # agora tem de dar "function"

# Figura em cinza, sem título geral (pela ABNT o título vai no LaTeX);
# dentro da imagem ficam só os rótulos dos painéis (a) e (b).
png("Figuras/fig5_mapas.png", width = 2600, height = 1050, res = 260)
par(mfrow = c(1, 2), mar = c(0.3, 0.3, 1.6, 0.3))

plot(st_geometry(mapa), col = cinzas[as.integer(cy$cl)], border = "white", lwd = 0.15)
title("(a) Roubos e furtos por 10 mil veículos", cex.main = 0.95, font.main = 1)
legend("bottomleft", legend = rotulos(cy$q), fill = cinzas,
       bty = "n", cex = 0.75, border = "gray40")

plot(st_geometry(mapa), col = cinzas[as.integer(cx$cl)], border = "white", lwd = 0.15)
title("(b) Informalidade (% de ocupados não contribuintes)", cex.main = 0.95, font.main = 1)
legend("bottomleft", legend = rotulos(cx$q), fill = cinzas,
       bty = "n", cex = 0.75, border = "gray40")

dev.off()











### BLOCO 3: REGRESSÕES, ROBUSTEZ E TESTES 


## Primeira Parte: base e pasta de resultados (os pacotes lmtest e sandwich estão na Parte 0)

# Relê a base salva no Bloco 1 (mesmo motivo do Bloco 2)
base <- read.csv2("Dados/base_final.csv", fileEncoding = "UTF-8")   
dim(base)                                                            
dir.create("Resultados", showWarnings = FALSE)                       


## Segunda Parte: os três modelos

m1 <- lm(taxa_veic ~ informal, data = base)                                     # Modelo 1: só Y contra a informalidade
m2 <- lm(taxa_veic ~ informal + renda_pc + desig + desocup + superior, data = base)   # Modelo 2: + controles socioeconômicos
m3 <- lm(taxa_veic ~ informal + renda_pc + desig + desocup + superior +
           homens_jovens + ln_pop + ln_dens + presidio + factor(regiao_metropolitana), data = base)  # Modelo 3: + o resto e as dummies de RM

coeftest(m1, vcov. = vcovHC, type = "HC1")   # coeficientes do Modelo 1 com erro-padrão HC1
coeftest(m2, vcov. = vcovHC, type = "HC1")   # para o Modelo 2
coeftest(m3, vcov. = vcovHC, type = "HC1")   # para o Modelo 3


## Terceira Parte: comparar o b1 nos três modelos

pega_b1 <- function(m) {                                   # Para o Davi: cria a função, o m é o modelo que nós quisermos
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))       # tabela t do modelo m com erro-padrão HC1
  c(b1 = ct[2, 1], ep_hc1 = ct[2, 2], p = ct[2, 4],        # linha 2 (informal): coeficiente, erro-padrão e p-valor
    n = nobs(m), r2 = summary(m)$r.squared)                # número de municípios usados e R² do modelo
}

principais <- rbind(Modelo1 = pega_b1(m1), Modelo2 = pega_b1(m2), Modelo3 = pega_b1(m3))   
round(principais, 4)                                                                      

## Quarta Parte: tamanho do b1 no Modelo 3 e intervalo de confiança

coef(m3)["informal"] * sd(base$informal)                                                # variação em Y para +1 desvio-padrão de informalidade
coef(m3)["informal"] * (quantile(base$informal, 0.75) - quantile(base$informal, 0.25))  # variação em Y indo do 1º ao 3º quartil de informalidade
coef(m3)["informal"] * sd(base$informal) / mean(base$taxa_veic)                         # o mesmo +1 desvio-padrão, em proporção da média de Y

ic_m3 <- coefci(m3, vcov. = vcovHC(m3, type = "HC1"))["informal", ]   # IC de 95% do b1 com erro-padrão HC1 (só a linha da informal)
ic_m3                                                                 # mostra os dois extremos do IC
ic_m3 * sd(base$informal) / mean(base$taxa_veic)                      # o IC na mesma escala: +1 desvio-padrão, em proporção da média de Y


#OBS para a prova: Dualidade entre teste e IC: se o IC de 95% contém o zero, não se rejeita H0: b1 = 0 a 5%, e vice-versa.
#OBS para a prova: Transformação linear do IC: multiplicar os dois extremos do IC por uma constante dá o IC da quantidade multiplicada.


## Quinta Parte: tabela com todos os coeficientes dos 3 modelos (csv)

tab <- function(m) {                                                  # transforma um modelo numa tabela
  ct <- coeftest(m, vcov. = vcovHC(m, type = "HC1"))                  # tabela t com erro-padrão HC1
  data.frame(variavel = rownames(ct), coef = ct[, 1], ep_hc1 = ct[, 2], p = ct[, 4])   
}
write.csv2(rbind(cbind(modelo = "M1", tab(m1)), cbind(modelo = "M2", tab(m2)), cbind(modelo = "M3", tab(m3))),
           "Resultados/tabela_principal.csv", row.names = FALSE, fileEncoding = "UTF-8")   # empilha os 3 e salva o csv



## Sexta Parte: decomposição do viés 

controles <- c("renda_pc", "desig", "desocup", "superior", "homens_jovens", "ln_pop", "ln_dens", "presidio")  # 8 controles sem RM 
rm_dummies <- model.matrix(~ factor(regiao_metropolitana), data = base)[, -1]   # as 9 dummies de RM como colunas 0/1 )
W <- cbind(base[, controles], rm_dummies)                                       # junta os 17 controles do Modelo 3 numa tabela só

beta  <- coef(m3)[-(1:2)]                                                       # beta_k: coeficientes dos controles no Modelo 3 (tira intercepto e informal)
delta <- sapply(W, function(w) coef(lm(w ~ base$informal))[2])                  # delta_k: inclinação de cada controle regredido na informalidade
termo <- beta * delta                                                           # beta_k × delta_k: quanto cada controle mexeu no b1

decomp <- data.frame(controle = c(controles, "dummies de RM (soma)"),   
                     beta  = c(beta[1:8], NA),                        
                     delta = c(delta[1:8], NA),                         
                     termo = c(termo[1:8], sum(termo[9:17])))           
decomp                                                                  
sum(termo)                                                              
coef(m1)["informal"] - coef(m3)["informal"]                             
write.csv2(decomp, "Resultados/decomposicao_vies.csv", row.names = FALSE, fileEncoding = "UTF-8")   



## Sétima Parte: tratamentos dos presídios 

sem_trat  <- update(m3, . ~ . - presidio)                              # 645 municípios, sem a dummy de presídio (sem tratamento)

sem_27    <- update(sem_trat, data = base[base$presidio == 0, ])       # mesma fórmula, só nos municípios sem presídio (tira os 27)

base$presidio110 <- as.numeric(base$razao_sexo > 110)                  # nova dummy: 1 se a razão de sexo passa de 110, 0 se não

dummy_110 <- update(sem_trat, . ~ . + presidio110)                     # Modelo 3 com a dummy no corte de 110 no lugar da de 120

sem_33    <- update(sem_trat, data = base[base$presidio110 == 0, ])    # mesma fórmula, tirando os marcados pelo corte de 110

d11 <- rbind(sem_tratamento = pega_b1(sem_trat), com_dummy = pega_b1(m3), sem_os_27 = pega_b1(sem_27),
             dummy_corte110 = pega_b1(dummy_110), sem_os_33 = pega_b1(sem_33))  

## Sétima Parte ainda: agora é para os coeficiente de homens jovens 

hj <- sapply(list(sem_trat, m3, sem_27, dummy_110, sem_33), function(m)
  coeftest(m, vcov. = vcovHC(m, type = "HC1"))["homens_jovens", c(1, 2, 4)])   # para cada versão: coef, EP HC1 e p de homens_jovens
d11 <- cbind(d11, hj_coef = hj[1, ], hj_ep = hj[2, ], hj_p = hj[3, ])           # cola essas 3 colunas ao lado das do b1
round(d11, 4)                                                                   # mostra a tabela com 4 casas
write.csv2(data.frame(versao = rownames(d11), d11), "Resultados/d11_presidios.csv",
           row.names = FALSE, fileEncoding = "UTF-8")                           # salva o csv com o nome de cada versão numa coluna

## Oitava Parte: robustezes todas a partir do Modelo 3

r_pequenos <- update(m3, data = base[base$pop >= 5000, ])                 
r_capital  <- update(m3, data = base[base$cod_ibge != 3550308, ])          
r_ibge     <- lm(taxa_veic ~ informal_ibge + renda_pc + desig + desocup + superior +
                   homens_jovens + ln_pop + ln_dens + presidio + factor(regiao_metropolitana), data = base)  
r_hab      <- update(m3, taxa_veic_hab ~ .)                               
r_outros   <- update(m3, taxa_outros ~ .)                                
r_homic    <- update(m3, taxa_homic ~ .)                                  
r_sem_rm   <- update(m3, . ~ . - factor(regiao_metropolitana))            
r_log      <- update(m3, log(1 + taxa_veic) ~ .)                          

robustez <- rbind(principal_M3 = pega_b1(m3), sem_pequenos = pega_b1(r_pequenos), sem_capital = pega_b1(r_capital),
                  informal_ibge = pega_b1(r_ibge), Y_por_habitante = pega_b1(r_hab), Y_outros = pega_b1(r_outros),
                  Y_homicidio = pega_b1(r_homic), sem_dummies_RM = pega_b1(r_sem_rm), Y_log1p = pega_b1(r_log))   
round(robustez, 4)                                                        

# Magnitude das robustezes com outra variável dependente: +1 desvio-padrão de informalidade, em proporção da média de cada Y
coef(r_hab)["informal"] * sd(base$informal) / mean(base$taxa_veic_hab)     
coef(r_outros)["informal"] * sd(base$informal) / mean(base$taxa_outros)    

# Hipótese H-: Modelo 3 sem a desocupação (o b1 deixa de manter o desemprego fixo)
r_sem_desoc <- update(m3, . ~ . - desocup)     
pega_b1(r_sem_desoc)                            

# Sem as dummies de RM e sem a Baixada Santista
r_sem_rm_bs <- update(r_sem_rm, data = base[base$regiao_metropolitana != "RM Baixada Santista", ])  
pega_b1(r_sem_rm_bs)                                                                                


## Nona Parte: IC e magnitude sem os pequenos e csv das robustezes

ic_peq <- coefci(r_pequenos, vcov. = vcovHC(r_pequenos, type = "HC1"))["informal", ]   # IC de 95% do b1 sem os pequenos (HC1)
ic_peq                                                                                 # mostra os dois extremos
coef(r_pequenos)["informal"] * sd(base$informal[base$pop >= 5000]) /
  mean(base$taxa_veic[base$pop >= 5000])                                               # +1 desvio-padrão, em proporção da média de Y (só nos 495)
write.csv2(data.frame(especificacao = rownames(robustez), robustez), "Resultados/robustez.csv",
           row.names = FALSE, fileEncoding = "UTF-8")                                  # salva a tabela das robustezes

## Décima Parte: testes

waldtest(m3, r_sem_rm, vcov = vcovHC(m3, type = "HC1"), test = "F")      # H0: as 9 dummies de RM são zero juntas
m3_sem_socio <- update(m3, . ~ . - renda_pc - desig - desocup - superior) # Modelo 3 sem o bloco socioeconômico
waldtest(m3, m3_sem_socio, vcov = vcovHC(m3, type = "HC1"), test = "F")  # H0: o bloco socioeconômico é zero junto
waldtest(m3, m2, vcov = vcovHC(m3, type = "HC1"), test = "F")            # H0: o que o Modelo 3 acrescenta ao 2 é zero

bptest(m3)                                                                # Breusch-Pagan: H0 = homocedasticidade
