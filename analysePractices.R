#
# analysePractices.R - função que analisa os dados das práticas de GovTI
#

analysePractices <- function(input = "data//tidy//praticas-GovTI.csv",
                             output1 = "data//analysis//sele-descritiva.csv",
                             output2 = "data//analysis//alea-descritiva.csv",
                             output3 = "data//analysis//sele-correlacao.csv",
                             output4 = "data//analysis//alea-correlacao.csv",
                             output5 = "data//analysis//sele-inter-conf.csv",
                             output6 = "data//analysis//alea-inter-conf.csv",
                             output7 = "data/analysis/boxplot.png") {
    
    ## Testar a existência do arquivo de dados
    if(!file.exists(input)) {
        stop("Arquivo de dados não encontrado.")
    }
    
    ## Declarar os pacotes requeridos
    require(psych)
    
    ## Ler os arquivos de dados e separar as duas amostras
    practices <- read.csv(file = input, header = TRUE, sep = ";")
    pInterview <- practices[practices$TipoAmostra == "selecionada", 3:22]
    pSurvey <- practices[practices$TipoAmostra == "aleatoria", 3:22]
    
    ## Calcular parâmetros estatísticos
    
    # Calcular a estatistica descrita das variáveis
    descInterwiew <- describe(pInterview)
    descSurvey <- describe(pSurvey)
    
    # Calcular a correlação entre as variáveis
    corInterwiew <- cor(pInterview, use = "complete.obs")
    corSurvey <- cor(pSurvey, use = "complete.obs")
    
    # Calcular os intervalos de confiança, assumindo o nível de confiança igual 
    # a 95%
    corTestInterview <- corr.test(x = pInterview, use = "complete", 
                                  method = "pearson", alpha = 0.98)
    corTestSurvey <- corr.test(x = pSurvey, use = "complete", 
                               method = "pearson", alpha = 0.98)
    
    ## Gravar os resultados nos arquivos de saída.
    write.table(descInterwiew, file = output1, dec = ",", sep="; ", na = "")
    write.table(descSurvey, file = output2, dec = ",", sep=";")
    write.table(corInterwiew, file = output3, dec = ",", sep=";")
    write.table(corSurvey, file = output4, dec = ",", sep=";")
    write.table(corTestInterview$r, file = output5, dec = ",", sep=";")
    write.table(corTestSurvey$r, file = output6, dec = ",", sep=";")
    
    ## Gerar boxplot das variáveis, separadas em relevância e existência e 
    ## gravar num arquivo PNG
    
    # Criar data frames 
    pInterviewRelev <- pInterview[, seq(1, 19, 2)]
    pInterviewExist <- pInterview[, seq(2, 20, 2)]
    pSurveyRelev <- pSurvey[, seq(1, 19, 2)]
    pSurveyExist  <- pSurvey[, seq(2, 20, 2)]
    
    # Abrir dispositivo PNG
    png(filename = output7, width = 1280, height = 960, units = "px")
    par(mfrow = c(4,1))
    
    # Boxplots de relevância 
    boxplot(pInterviewRelev)
    title("Relevância das práticas de GovTI - Amostra selecionada")
    boxplot(pSurveyRelev)
    title("Relevância das práticas de GovTI - Amostra aleatória")
    
    # Boxplots de existência
    boxplot(pInterviewExist)
    title("Existência das práticas de GovTI - Amostra selecionada")
    boxplot(pSurveyExist)
    title("Existência das práticas de GovTI - Amostra aleatória")
    dev.off()  
}