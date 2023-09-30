# Script para distribuição de tarefas, principalmente fichas. Cria uma planilha
# com o número da ficha, uma atribuição para revisão e outra atribuição para
# checagem.

# MODO DE USAR ----------------------------------------------------------------
# 1. definir do número de fichas  e atualizar o nome das pessoas que vão dividir
# as tarefas 
n_fichas <- 55
pessoas <- c("Pedro", 
             "Rafael",
             "Isllane",
             "Mônica")

# 2. Criar a função, rodando o código abaixo
dist_fichas <- function(n_fichas, pessoas) {
  if (suppressWarnings(!require(dplyr, warn.conflicts = FALSE, quietly = TRUE))) install.packages(dplyr)
  
  revisao <- rep(sample(pessoas, length(pessoas)), 
                 ceiling(n_fichas/length(pessoas)))[1:n_fichas]
  
  checagem <- dplyr::lag(revisao)
  checagem[1] <-  sample(pessoas, 1)
  
  while (checagem[1] == revisao[1]) {
    checagem[1] <-  sample(pessoas, 1)
  }
  
  # Criando e salvando a tabela
  tarefas <- tibble(ficha = 1:n_fichas,
                    revisao = revisao,
                    checagem = checagem)
  
  write.csv2(tarefas, "tarefas.csv", row.names = FALSE)
  
  # Checar se há alguma ficha com mesmo revisor e checador
  if (any(tarefas$revisao == tarefas$checagem)) {
    warning("IMPORTANTE: Há fichas com o mesmo nome na revisão e na checagem. Verifique a planilha e faça as adaptações necessárias.")
  } else {
    cat("\nSucesso! A distribuição das tarefas já foi avaliada e não há nenhuma ficha na qual a mesma pessoa faz a revisão e a checagem.")
  }
  
  return(cat("\n \nA planilha 'tarefas.csv' foi gravada na pasta:\n", 
             getwd(), "\n \n", sep = ""))
}

# 3. Rodar a função. O R dirá se a distribuição foi feita com sucesso, e 
# indicará a pasta onde foi salva a planilha
dist_fichas(n_fichas, pessoas)
