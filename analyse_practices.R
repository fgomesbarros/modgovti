#
# analyse_practices.R - fun��o que analisa os dados das pr�ticas de GovTI
#

AnalysePractices <- function(input = "data//tidy//praticas-GovTI.csv",
                             output.1 = "data//analysis//sele-descritiva.csv",
                             output.2 = "data//analysis//alea-descritiva.csv",
                             output.3 = "data//analysis//sele-correlacao.csv",
                             output.4 = "data//analysis//alea-correlacao.csv",
                             output.5 = "data//analysis//sele-inter-conf.csv",
                             output.6 = "data//analysis//alea-inter-conf.csv",
                             output.7 = "data/analysis/boxplot.png") {
    
    ## Testar a exist�ncia do arquivo de dados
    if(!file.exists(input)) {
        stop("Arquivo de dados n�o encontrado.") 
    }
    
    ## Declarar os pacotes requeridos
    require(psych)
    
    ## Ler os arquivos de dados e separar as duas amostras
    practices <- read.csv(file = input, header = TRUE, sep = ";")
    interview <- practices[practices$tipo.amostra == "selecionada", 3:22]
    survey <- practices[practices$tipo.amostra == "aleatoria", 3:22]
    
    ## Calcular par�metros estat�sticos
    
    # Realizar a estatistica descritiva das vari�veis
    desc.interwiew <- describe(interview)
    desc.survey <- describe(survey)
    
    # Calcular a correla��o entre as vari�veis
    cor.interwiew <- cor(interview, use = "complete.obs")
    cor.survey <- cor(survey, use = "complete.obs")
    
    # Calcular os intervalos de confian�a, assumindo o n�vel de confian�a igual 
    # a 95%
    cor.test.interview <- corr.test(x = interview, use = "complete", 
                                    method = "pearson", alpha = 0.98)
    cor.test.survey <- corr.test(x = survey, use = "complete", 
                                 method = "pearson", alpha = 0.98)
    
    ## Gravar os resultados nos arquivos de sa�da.
    write.table(desc.interwiew, file = output.1, dec = ",", sep = ";")
    write.table(desc.survey, file = output.2, dec = ",", sep = ";")
    write.table(cor.interwiew, file = output.3, dec = ",", sep = ";")
    write.table(cor.survey, file = output.4, dec = ",", sep = ";")
    write.table(cor.test.interview$r, file = output.5, dec = ",", sep=";")
    write.table(cor.test.survey$r, file = output.6, dec = ",", sep=";")
    
    ## Gerar boxplot das vari�veis, separadas em relev�ncia e exist�ncia e 
    ## gravar num arquivo PNG
    
    # Criar data frames para cada um dos tipos de variaveis por amostra
    relev.interview <- interview[, seq(1, 19, 2)]
    exist.interview <- interview[, seq(2, 20, 2)]
    relev.survey <- survey[, seq(1, 19, 2)]
    exist.survey <- survey[, seq(2, 20, 2)]
    
    # Abrir dispositivo PNG
    png(filename = output.7, width = 1280, height = 960, units = "px")
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