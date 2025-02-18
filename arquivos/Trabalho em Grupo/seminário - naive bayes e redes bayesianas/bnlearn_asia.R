# Pacotes ----
# install.packages("bnlearn")
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
library(bnlearn)
library(Rgraphviz)
# o pacote Rgraphviz está com algum problema e a única forma que consegui
# instala-lo foi assim


# data Asia por bnlearn ----
# Os mantenedores do bnlearn fornecem um repositório com alguns data sets
# http://www.bnlearn.com/bnrepository/

# O conjunto de dados asia é um pequeno conjunto de dados sintéticos de Lauritzen 
# e Spiegelhalter que tenta implementar um modelo de diagnóstico para doenças 
# pulmonares (tuberculose, câncer de pulmão ou bronquite) após uma visita à Ásia
load(url("http://www.bnlearn.com/bnrepository/asia/asia.rda"))
asia <- bn

# D: dispneia.
# T: tuberculose.
# L: câncer de pulmão.
# B: bronquite.
# A: visita à Ásia.
# S: tabagismo.
# X: raio-X de tórax.
# E: Ou tuberculose ou câncer de pulmão. (Either)

# Olhando a estrutura
bn.net(asia)

# Grafo
graphviz.plot(asia)

# Olhando as probabilidades
asia

asia$smoke

bn.fit.barchart(asia$smoke)
bn.fit.barchart(asia$dysp)
bn.fit.barchart(asia$xray)
bn.fit.barchart(asia$either)

# Modificando uma tabela de probabilidade condicional
cpt <- coef(asia$smoke)
cpt[] <- c(0.2, 0.8)
asia$smoke <- cpt
asia$smoke

# data Asia ----
# agora buscaremos modelar redes bayesianas carregando o data Asia sem as 
# probabilidade condicionais
data(asia)
head(asia)

#### podemos adicionar ainda definir a estrutura e as probabilidade manualmente
asia.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
asia.dag

lv = c("yes", "no")
A.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))
S.prob = array(c(0.01, 0.99), dim = 2, dimnames = list(A = lv))
T.prob = array(c(0.05, 0.95, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(T = lv, A = lv))
L.prob = array(c(0.1, 0.9, 0.01, 0.99), dim = c(2, 2),
               dimnames = list(L = lv, S = lv))
B.prob = array(c(0.6, 0.4, 0.3, 0.7), dim = c(2, 2),
               dimnames = list(B = lv, S = lv))
D.prob = array(c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9), dim = c(2, 2, 2),
               dimnames = list(D = lv, B = lv, E = lv))
E.prob = array(c(1, 0, 1, 0, 1, 0, 0, 1), dim = c(2, 2, 2),
               dimnames = list(E = lv, T = lv, L = lv))
X.prob = array(c(0.98, 0.02, 0.05, 0.95), dim = c(2, 2),
               dimnames = list(X = lv, E = lv))
cpt = list(A = A.prob, S = S.prob, T = T.prob, L = L.prob, B = B.prob,
           D = D.prob, E = E.prob, X = X.prob)
bn = custom.fit(asia.dag, cpt)
bn

asia.dag = rename.nodes(
  asia.dag, 
  names = c("Asia", "Bronchitis", "Dyspnea", "Either", "Lung Cancer", "Smoking", 
            "Tuberculosis", "X-ray")
  )

plot(asia.dag)
graphviz.plot(asia.dag)

rm(list=ls())
data(asia)
head(asia)

## Aprendizagem da estrutura ----
# relacionados a Constraint-based structure
# https://www.bnlearn.com/documentation/man/structure.learning.html
# https://www.bnlearn.com/documentation/man/constraint.html
# https://www.bnlearn.com/documentation/man/conditional.independence.tests.html
# relacionados a Score-based structure
# https://www.bnlearn.com/documentation/man/hc.html
# https://www.bnlearn.com/documentation/man/network.scores.html
# relacionados a Hybrid structure
# https://www.bnlearn.com/documentation/man/hybrid.html


### Constraint-based structure ----
# PC: pc.stable()
# Grow-Shrink: gs()
# Incremental Association: iamb()
# Fast Incremental Association: fast.iamb()
# Interleaved Incremental Association: inter.iamb()
# Incremental Association with FDR: iamb.fdr()
# Outros algoritmos de descoberta local (baseados em restrições)
# Max-Min Parents and Children: mmpc()
# Hiton Parents and Children: si.hiton.pc()
# Hybrid Parents and Children: hpc()

# usando PC
pc.dag = pc.stable(asia, undirected = F) # teste default mi - informação mutua
pc.dag
# usando Hiton Parents and Children
hiton_pc.dag = si.hiton.pc(asia, undirected = F) # teste default mi - informação mutua
hiton_pc.dag


par(mfrow = c(1, 3))
true.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# grafos usando PC e Hiton Parents and Children
graphviz.plot(true.dag)
graphviz.plot(pc.dag, highlight = list(arcs = arcs(true.dag)))
graphviz.plot(hiton_pc.dag, highlight = list(arcs = arcs(true.dag)))



# usando PC e Hiton Parents and Children com o teste mc-mi
# mc-mi: teste de permutação de Monte Carlo
pc.dag2 = pc.stable(asia, test = "mc-mi", undirected = F) 
pc.dag2

hiton_pc.dag2 = si.hiton.pc(asia, test = "mc-mi", undirected = F) 
hiton_pc.dag2

par(mfrow = c(1, 3))
true.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# grafos
graphviz.plot(true.dag)
graphviz.plot(pc.dag2, highlight = list(arcs = arcs(true.dag)))
graphviz.plot(hiton_pc.dag2, highlight = list(arcs = arcs(true.dag)))


# Podemos calcular "manualmente" os testes de independência marginal ou condicional
ci.test(x = "S", y = "E", z = "L", data = asia, test = "mc-mi")


### Score-based structure ----
# Hill-Climbing: hc()
# Tabu Search: tabu()

# usando Hill-Climbing
hc.dag = hc(asia) # score default bic - Bayesian Information Criterion
hc.dag

par(mfrow = c(1, 2))
true.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# grafos
graphviz.plot(true.dag)
graphviz.plot(hc.dag, highlight = list(arcs = arcs(true.dag)))

# valor de BIC
BIC(object = hc.dag, data = asia)


### Hybrid structure ----
# Restricted Maximization: rsmax2(x, restrict = "si.hiton.pc", maximize = "hc")
# Max-Min Hill-Climbing: mmhc()
# Hybrid HPC: h2pc()

# usando Restricted Maximization
rm.dag = rsmax2(asia, restrict = "mmpc", maximize = "hc")
rm.dag
# usando Max-Min Hill-Climbing
mmhc.dag = mmhc(asia)
mmhc.dag

par(mfrow = c(1, 3))
true.dag = model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")

# grafos
graphviz.plot(true.dag)
graphviz.plot(rm.dag, highlight = list(arcs = arcs(true.dag)))
graphviz.plot(mmhc.dag, highlight = list(arcs = arcs(true.dag)))

### Comparando estruturas ----
# https://www.bnlearn.com/examples/compare-dags/

#### Summaries/Resumo ----
# all.equal(): verifica se duas redes possuem a mesma estrutura
# compare(): coloca uma rede como referência e calcula o número de arcos 
# verdadeiro positivo (tp), falso positivo (fp) e falso negativo (fn)

all.equal(hc.dag, true.dag)
unlist(compare(hc.dag, true.dag))

#### Distâncias estruturais ----
# hamming(): calcula a distância de Hamming entre duas redes
# shd(): calcula a distância de Hamming estrutural (SHD) entre duas redes

hamming(hc.dag, true.dag)
shd(hc.dag, true.dag)

#### Comparação visual ----
# graphviz.compare()

par(mfrow = c(2, 2))
graphviz.compare(true.dag, hiton_pc.dag, hc.dag, mmhc.dag)


## Aprendizagem dos parametros (distribuições) ----
# https://www.bnlearn.com/documentation/man/bn.fit.html

# usando EMV
fitted_mle = bn.fit(hc.dag, asia, method = "mle")
fitted_mle
# usando MAP
fitted_bayes = bn.fit(hc.dag, asia, method = "bayes")
fitted_bayes










