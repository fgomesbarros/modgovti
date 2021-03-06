#
# analyse_practices.R - fun��o que analisa os dados das pr�ticas de GovTI
#

AnalysePractices <- function(input = "data//tidy//praticas-GovTI.csv",
                             output.1 = "data//analysis//sele-descritiva.csv",
                             output.2 = "data//analysis//alea-descritiva.csv",
                             output.3 = "data//analysis//sele-correlacao.csv",
                             output.4 = "data//analysis//alea-correlacao.csv",
                             output.5 = "data//analysis//sele-rank-relev.csv",
                             output.6 = "data//analysis//sele-rank-exist.csv",
                             output.7 = "data//analysis//alea-rank-relev.csv",
                             output.8 = "data//analysis//alea-rank-exist.csv",
                             output.9 = "data//analysis//sele-inter-conf.csv",
                             output.10 = "data//analysis//alea-inter-conf.csv",
                             output.11 = "data//analysis//boxplot.png") {
  
  ### Sub-funcoes
  
  ## Funcao que ajusta as colunas da estatistica descritiva e da correlacao
  AdjustColumns <- function(df){ 
    
    variable.name <- row.names(df)
    df <- cbind(variable.name, df)
    row.names(df) <- 1:dim(df)[1]
    return(df)      
    
  }
 
  RankMean <- function(df){
    rank <- df[order(df$mean, decreasing = TRUE), c("variable.name", "mean", 
                                                    "sd", "median")]
    rank <- cbind(position = 1:10, rank)
    return(rank)
  }
  ### Script
  
  ## 1 - Testar a exist�ncia do arquivo de dados
  if(!file.exists(input)) {
      stop("Arquivo de dados n�o encontrado.") 
  }
  
  ## 2 - Declarar os pacotes requeridos
  require(psych)
  
  ## 3 - Ler os arquivos de dados e separar as duas amostras
  practices <- read.csv(file = input, header = TRUE, sep = ";")
  interview <- practices[practices$tipo.amostra == "selecionada", 3:22]
  survey <- practices[practices$tipo.amostra == "aleatoria", 3:22]
  
  ## 4 - Criar datasets para cada um dos tipos de variaveis por amostra
  relev.interview <- interview[, seq(1, 19, 2)]
  exist.interview <- interview[, seq(2, 20, 2)]
  relev.survey <- survey[, seq(1, 19, 2)]
  exist.survey <- survey[, seq(2, 20, 2)]
  
  ## 5 - Realizar estat�stica descritiva
  
  desc.interview <- describe(interview)
  desc.survey <- describe(survey)
  desc.relev.interview <- describe(relev.interview)
  desc.exist.interview <- describe(exist.interview)
  desc.relev.survey <- describe(relev.survey)
  desc.exist.survey <- describe(exist.survey)
  
  # Ajustar colunas dos datasets 
  desc.interview <- AdjustColumns(desc.interview)
  desc.survey <- AdjustColumns(desc.survey)
  desc.relev.interview <- AdjustColumns(desc.relev.interview)
  desc.exist.interview <- AdjustColumns(desc.exist.interview)
  desc.relev.survey <- AdjustColumns(desc.relev.survey)
  desc.exist.survey <- AdjustColumns(desc.exist.survey)
  
  ## 6 - Elaborar ranking das m�dias de relev�ncia e exist�ncia das 
  ## duas amostras
  
  # Amostra = selecionada, ranking = relev�ncia
  rank.relev.interview <- RankMean(desc.relev.interview)

  # Amostra = selecionada, ranking = exist�ncia
  rank.exist.interview <- RankMean(desc.exist.interview)
  
  # Amostra = aleatoria, ranking = relev�ncia
  rank.relev.survey <- RankMean(desc.relev.survey) 
  
  # Amostra = aleatoria, ranking = exist�ncia
  rank.exist.survey <- RankMean(desc.relev.survey)
  
  ## 7 - Calcular a correla��o entre as vari�veis
  cor.interview <- cor(interview, use = "complete.obs")
  cor.survey <- cor(survey, use = "complete.obs")
  
  # Ajustar colunas dos datasets de correlacao 
  cor.interview <- AdjustColumns(cor.interview)
  cor.survey <- AdjustColumns(cor.survey)
  
  ## 8 - Calcular os intervalos de confian�a, assumindo o n�vel de confian�a 
  ## igual a a 95%
  cor.test.interview <- corr.test(x = interview, use = "complete", 
                                  method = "pearson", alpha = 0.98)
  cor.test.survey <- corr.test(x = survey, use = "complete", 
                               method = "pearson", alpha = 0.98)
  
  # Ajustas as colunas dos dataset dos intervalos de confianca
  cor.test.interview <- AdjustColumns(cor.test.interview$r)
  cor.test.survey <- AdjustColumns(cor.test.survey$r)
    
  ## Gravar os resultados nos arquivos de sa�da.
  write.table(desc.interview, file = output.1, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(desc.survey, file = output.2, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(cor.interview, file = output.3, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(cor.survey, file = output.4, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(rank.relev.interview, file = output.5, row.names = FALSE, 
              sep = ";", dec = ",")
  write.table(rank.exist.interview, file = output.6, row.names = FALSE, 
              sep = ";", dec = ",")
  write.table(rank.relev.survey, file = output.7, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(rank.exist.survey, file = output.8, row.names = FALSE, sep = ";", 
              dec = ",")
  write.table(cor.test.interview, file = output.9, row.names = FALSE, sep = ";",
              dec = ",")
  write.table(cor.test.survey, file = output.10, row.names = FALSE, sep = ";", 
              dec = ",")
    
  ## Gerar boxplot das vari�veis, separadas em relev�ncia e exist�ncia e 
  ## gravar num arquivo PNG
   
  # Abrir dispositivo PNG
  png(filename = output.11, width = 1280, height = 960, units = "px")
  par(mfrow = c(4,1))
  
  # Boxplots de relev�ncia 
  boxplot(relev.interview)
  title("Relev�ncia das pr�ticas de GovTI - Amostra selecionada")
  boxplot(relev.survey)
  title("Relev�ncia das pr�ticas de GovTI - Amostra aleat�ria")
  
  # Boxplots de exist�ncia
  boxplot(exist.interview)
  title("Exist�ncia das pr�ticas de GovTI - Amostra selecionada")
  boxplot(exist.survey)
  title("Exist�ncia das pr�ticas de GovTI - Amostra aleat�ria")
  dev.off()  
}