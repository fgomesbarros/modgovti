#
# getcleanDataPractices.R - função que obtém e lê os dados das práticas de um 
#                           arquivo XML do Excel
#

getcleanDataPractices <- function(input = "data//raw//Consolidacao dos dados v1.1.xlsx",
                                  output = "data//tidy//praticas-GovTI.csv") {
    # Declarar os pacotes requeridos
    require(xlsx)
    
    ### Passo 1: Obter os dados
    
    # Ler os dados das práticas obtidos nas entrevistas 
    if(!file.exists(input)) {
        stop("Arquivo XML Excel não encontrado.")
    }
    
    sheetIndex <- 1
    colNames <- c("Entidade", "TipoEntidade", "P01.Relev", "P01.Exist", 
                  "P02.Relev", "P02.Exist", "P03.Relev", "P03.Exist", 
                  "P04.Relev", "P04.Exist", "P05.Relev", "P05.Exist", 
                  "P06.Relev", "P06.Exist", "P07.Relev", "P07.Exist", 
                  "P08.Relev", "P08.Exist", "P09.Relev", "P09.Exist", 
                  "P10.Relev", "P10.Exist")
    colIndex <- c(1, 3, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27, 
                  29, 30, 32, 33, 35, 36)
    colClasses <- c("character", "character", rep("numeric", 20))
    rowIndex <- 7:22
    interview <- read.xlsx(file = input, sheetIndex = sheetIndex,
                           rowIndex = rowIndex, colIndex = colIndex,
                           colClasses = colClasses, as.data.frame = TRUE, 
                           header = FALSE, stringsAsFactors = FALSE)  
    colnames(interview) <- colNames
    interview$TipoAmostra <- "selecionada"
            
    # Ler os dados das práticas obtidos no survey
    sheetIndex <- 2
    rowIndex <- 7:78
    colIndex <- c(1, 2, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19, 21, 22, 24, 25, 27,
                  28, 30, 31, 33, 34) 
    survey <- read.xlsx(file = input, sheetIndex = sheetIndex,
                        rowIndex = rowIndex, colIndex = colIndex, 
                        colClasses = colClasses, as.data.frame = TRUE, 
                        header = FALSE, stringsAsFactors = FALSE)
    colnames(survey) <- colNames
    survey$TipoAmostra <- "aleatoria"
        
    # Juntar os dados
    practices <- rbind(interview, survey)
    
    # Ordenar os dados por entidade
    practices <- practices[order(practices$Entidade), ]
    
    ### Passo 2: Limpar os dados inválidos
    
    # Remover linhas com entidade em branco
    practices <- practices[!is.na(practices$Entidade == ""), ]
    
    ### Passo 3: Gravar os dados no arquivo de saída
    write.table(practices, file = output, row.names = FALSE, sep = ";")
}
    