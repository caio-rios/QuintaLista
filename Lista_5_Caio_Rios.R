library("tidyverse")
library("magrittr")
options(scipen=999)
#Primeira Lista de Exercícios

#Questão 5

Empresa <- c(1:15)
Meses <- c(8,9,4,5,3,6,8,6,6,8,5,5,6,4,4)
Setor <- c("C","C","I","I","I","C","C",
           "I","I","C","C","I","C","I","I")
Tamanho <- c("G","M","G","M","M","P",
             "G","M","P","M","P","P","M","M","G")
df_empresa <- data.frame(Empresa,Meses,Setor,Tamanho)

# Q5b) 

df_empresa_Setor <- df_empresa %>% group_by(Setor) %>% 
  summarise('Média de Meses' = mean(Meses), 
            'Mediana de Meses' = median(Meses),
            'Desvio Padrão' = sd(Meses)
            )

print(df_empresa_Setor)

# Resposta: As empresas de comércio possuem média e mediana de quantidade
# de meses com crescimento  mais elevadas do que as empresas
# industriais.

#_______________________________________________________________#

# Q5c) Resposta: O grupo das empresas industriais são mais homogênios
# em relação ao número de meses com crescimento possuindo desvio
# padrão de 1,06 enquanto as empresas comérciais possuim um desvio 
# padrão de 1,46

#_______________________________________________________________#

#Q5d)

print(quantile(df_empresa$Meses))

# Resposta: O número máximo de meses apresentando crescimento é de 
# 4,5 meses para receber o incentivo. A medida descritiva que mostra
# essa informação são os quartis, mais especificamente o primeiro 
# quartil

#_______________________________________________________________#

#Q5e)

df_empresa_tamanho <- df_empresa %>% group_by(Tamanho) %>% 
  summarise('Média de Meses' = mean(Meses), 
            'Mediana de Meses' = median(Meses),
            'Desvio Padrão' = sd(Meses)
  )

print(df_empresa_tamanho)

# Resposta: A diferença de média e mediana não são tão expressivas
# Empresas grandes apresentam média de crescimento (6) ligeiramente
# maior porém o desvio padrão (2,31) é elevado. A quantidade de 
# meses de crescimento das empresas pequenas é mais consistente 
# apresentando média 6 e desvio padrão de 0,577

#_______________________________________________________________#

#Questão 6

Cidade <- c("A","B","C","D","E","F","G","H","I","J")
Investimento <- c(26,16,14,10,19,15,19,16,19,18)
cidade_investimento <- data.frame(Cidade, Investimento)

#6a)

print(mean(cidade_investimento$Investimento))

#6b)

cidade_investimento$ProgramaEspecial <- cidade_investimento$Investimento < 
  mean(cidade_investimento$Investimento) - 2*sd(cidade_investimento$Investimento)

print(cidade_investimento)

# Resposta: Nenhuma cidade receberia o Programa Especial

cidade_investimento_sel <- cidade_investimento %>%
  filter(Investimento > mean(Investimento) - 2*sd(Investimento),
         Investimento < mean(Investimento) + 2*sd(Investimento))

mean(cidade_investimento_sel$Investimento)

# Resposta: O investimento básico será de 16,2. Uma das cidades (A) possui 
# investimento mais elevado do que a média mais dois desvios padrões, logo,
# ela ficou de fora do cálculo do investimento básico. Por isso este é menor do 
# a média original do investimento (17,2)

#Questão 7

Ind <- c(1:20)
A <- c(55,2,13,11,23,2,15,12,14,28,12,45,19,30,16,12,7,13,1,7)
B <- c(20,7,6,5,3,25,5,3,3,10,8,5,1,35,9,8,12,2,26)

#Q7a)

Estimulo <- c("A","B")
Média <- c(mean(A), mean(B))
Mediana <- c(median(A), median(B))
Variância <- c(var(A), var(B))
'Desvio Padrão' <- c(sd(A), sd(B))

reacao_desc <- data.frame(Estimulo,Média,Mediana,Variância,`Desvio Padrão`)

print(reacao_desc)

#Resposta: O Estímulo B apresenta, na média, tempo de reação mais curto. Porém
# o desvio padrão do estímula A foi maior pois existem casos com tempode reação
# muito curto e outros muito longos.

#Q7b) 

theme_set(theme_classic())
B <- c(20,7,6,5,3,25,5,3,3,10,8,5,1,35,9,8,12,2,26,NA)
reacao <- data.frame(Ind,A,B)

long_reacao <- reacao %>% gather(estimulo, Tempo, A:B)

boxplot_reacao <- ggplot(long_reacao, aes(x = estimulo, y = Tempo)) +
  geom_boxplot()

boxplot_reacao

#Questão 8

fam <- c("A","B","C","D","E","F","G","H","I","J")
renda_sm <- c(12,16,18,20,28,30,40,48,50,54)
saude_perc <- c(7.2,7.4,7,6.5,6.6,6.7,6,5.6,6,5.5)

gasto_saude <- data.frame(fam,renda_sm,saude_perc)

#8a)
Var <- c("renda_sm","saude_perc")
Mean <- c(mean(gasto_saude$renda_sm), mean(gasto_saude$saude_perc))
dp <- c(sd(gasto_saude$renda_sm), sd(gasto_saude$saude_perc))

gasto_saude_desc <- data.frame(Var, Mean, dp)

print(gasto_saude_desc)

cov(gasto_saude$renda_sm, gasto_saude$saude_perc)
cor.test(gasto_saude$renda_sm, gasto_saude$saude_perc)

# Resposta: As variáveis renda e gasto com a saude apresentam uma correlação
# negativa de -0.94 o que indica que quanto maior a renda, menor a proporção de
# de gasto com saude. O resultado foi significativo a um p-valor < 0,000.

#Questão 9

aluno <- c("A","B","C","D","E","F","G","H","I")
P1 <- c(7.5,8.2,8.5,8.7,8.8,9.1,9.2,9.3,10)
P2 <- c(8.2,8.0,8.3,8.5,9.4,9.6,9.0,9.3,9.7)

notas <- data.frame(aluno,P1,P2)

#9a) e b)

cor.test(notas$P1, notas$P2)

# Resposta: A correlação foi positiva de 0,83 o que sugere que quanto melhor o 
# desempenho na primeira prova, melhor o desempenho na segunda. O resultado foi
# estatisticamente significante apresentando um p-valor < 0,01
