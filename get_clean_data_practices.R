#
# get_clean_data_practices.R - função que obtém e lê os dados das práticas de um 
#                           arquivo XML do Excel
#

GetCleanDataPractices <- function(input = "data//raw//Consolidacao dos dados v1.1.xlsx",
                                  output = "data//tidy//praticas-GovTI.csv") {
  # Declarar os pacotes requeridos
  require(xlsx)
  
  ### Passo 1: Obter os dados
  
  # Ler os dados das práticas obtidos nas entrevistas 
  if(!file.exists(input)) {
    stop("Arquivo XML Excel não encontrado.")
  }

  # Configurar os parâmetros dos dados da planilha das entrevistas
  sheet.idx <- 1
  row.idx <- 7:22
  col.idx <- c(1, 3, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 23, 24, 26, 27,
               29, 30, 32, 33, 35, 36)
  col.classes <- c("character", "character", rep("numeric", 20))
  col.names <- c("sigla.entidade", "tipo.entidade", "P01.relev.riscos", 
                "P01.exist.riscos", "P02.relev.alin.estr", 
                "P02.exist.alin.estr", "P03.relev.monit.desemp", 
                "P03.exist.desemp.exist", "P04.relev.conformidade",
                "P04.exist.conformidade", "P05.relev.espec.dir", 
                "P05.exist.espec.dir", "P06.relev.comite", "P06.exist.comite", 
                "P07.relev.portifolio", "P07.exist.portifolio", 
                "P08.relev.aval.uso", "P08.exist.aval.uso", 
                "P09.relev.envolvimento", "P09.exist.envolvimento", 
                "P10.relev.sis.com.trans", "p10.relev.sis.com.trans")
    
  # Ler a planilha das entrevistas
  interview <- read.xlsx(file = input, sheetIndex = sheet.idx,
                         rowIndex = row.idx, colIndex = col.idx,
                         colClasses = col.classes, as.data.frame = TRUE, 
                         header = FALSE, stringsAsFactors = FALSE)  
  
  # Modificar e adicionar campos a planilha
  colnames(interview) <- col.names
  interview$tipo.amostra <- "selecionada"
          
  # Configurar os parâmetros dos dados da planilha do survey
  sheet.idx <- 2
  row.idx <- 7:78
  col.idx <- c(1, 2, 6, 7, 9, 10, 12, 13, 15, 16, 18, 19, 21, 22, 24, 25, 27,
                28, 30, 31, 33, 34)
  
  # Ler a planilha do survey
  survey <- read.xlsx(file = input, sheetIndex = sheet.idx,
                      rowIndex = row.idx, colIndex = col.idx, 
                      colClasses = col.classes, as.data.frame = TRUE, 
                      header = FALSE, stringsAsFactors = FALSE)
  
  # Modificar e adicionar campos a planilha
  colnames(survey) <- col.names
  survey$tipo.amostra <- "aleatoria"
      
  # Juntar os dados das duas amostras 
  practices <- rbind(interview, survey)
  
  # Ordenar os dados por tipo entidade e entidade
  practices <- practices[order(practices$sigla.entidade), ]
    
  ### Passo 2: Limpar os dados inválidos
      
  # Remover linhas com entidade em branco
  practices <- practices[!is.na(practices$sigla.entidade == ""), ]
      
  # Remover ocorrência do SISP
  practices <- practices[practices$sigla.entidade != "SISP", ]    
    
  
  ### Passo 3: Gravar os dados no arquivo de saída
  write.table(practices, file = output, row.names = FALSE, sep = ";")
  
  return(practices)
}
    