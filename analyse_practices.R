#
# analyse_practices.R - função que analisa os dados das práticas de GovTI
#

AnalysePractices <- function(input = "data//tidy//praticas-GovTI.csv",
                             output.1 = "data//analysis//sele-descritiva.csv",
                             output.2 = "data//analysis//alea-descritiva.csv",
                             output.3 = "data//analysis//sele-correlacao.csv",
                             output.4 = "data//analysis//alea-correlacao.csv",
                             output.5 = "data//analysis//sele-inter-conf.csv",
                             output.6 = "data//analysis//alea-inter-conf.csv",
                             output.7 = "data/analysis/boxplot.png") {
    
    ## Testar a existência do arquivo de dados
    if(!file.exists(input)) {
        stop("Arquivo de dados não encontrado.") 
    }
    
    ## Declarar os pacotes requeridos
    require(psych)
    
    ## Ler os arquivos de dados e separar as duas amostras
    practices <- read.csv(file = input, header = TRUE, sep = ";")
    interview <- practices[practices$tipo.amostra == "selecionada", 3:22]
    survey <- practices[practices$tipo.amostra == "aleatoria", 3:22]
    
    ## Calcular parâmetros estatísticos
    
    # Realizar a estatistica descritiva das variáveis
    desc.interwiew <- describe(interview)
    desc.survey <- describe(survey)
    
    # Calcular a correlação entre as variáveis
    cor.interwiew <- cor(interview, use = "complete.obs")
    cor.survey <- cor(survey, use = "complete.obs")
    
    # Calcular os intervalos de confiança, assumindo o nível de confiança igual 
    # a 95%
    cor.test.interview <- corr.test(x = interview, use = "complete", 
                                    method = "pearson", alpha = 0.98)
    cor.test.survey <- corr.test(x = survey, use = "complete", 
                                 method = "pearson", alpha = 0.98)
    
    ## Gravar os resultados nos arquivos de saída.
    write.table(desc.interwiew, file = output.1, dec = ",", sep = ";")
    write.table(desc.survey, file = output.2, dec = ",", sep = ";")
    write.table(cor.interwiew, file = output.3, dec = ",", sep = ";")
    write.table(cor.survey, file = output.4, dec = ",", sep = ";")
    write.table(cor.test.interview$r, file = output.5, dec = ",", sep=";")
    write.table(cor.test.survey$r, file = output.6, dec = ",", sep=";")
    
    ## Gerar boxplot das variáveis, separadas em relevância e existência e 
    ## gravar num arquivo PNG
    
    # Criar data frames para cada um dos tipos de variaveis por amostra
    relev.interview <- interview[, seq(1, 19, 2)]
    exist.interview <- interview[, seq(2, 20, 2)]
    relev.survey <- survey[, seq(1, 19, 2)]
    exist.survey <- survey[, seq(2, 20, 2)]
    
    # Abrir dispositivo PNG
    png(filename = output.7, width = 1280, height = 960, units = "px")
    par(mfrow = c(4,1))
    
    # Boxplots de relevância 
    boxplot(relev.interview)
    title("Relevância das práticas de GovTI - Amostra selecionada")
    boxplot(relev.survey)
    title("Relevância das práticas de GovTI - Amostra aleatória")
    
    # Boxplots de existência
    boxplot(exist.interview)
    title("Existência das práticas de GovTI - Amostra selecionada")
    boxplot(exist.survey)
    title("Existência das práticas de GovTI - Amostra aleatória")
    dev.off()  
}