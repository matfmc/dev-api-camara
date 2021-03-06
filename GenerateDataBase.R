requiredPackages = c('jsonlite','dplyr','httr','lubridate')
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p, repos='http://cran.us.r-project.org')
  library(p,character.only = TRUE)
}

setwd(choose.dir(default = "", caption = "Selecione a pasta para o Banco de Dados"))
options(stringsAsFactors = FALSE)
options(warn=-1)
idLegislatura <- '56'
itens <- 100
dataInicio <- '2019-01-01'
ano <- '2019,2020'
dataFim <- as.character(Sys.Date())

parametros <- data.frame(idLegislatura,itens,dataInicio,ano,dataFim)
write.table(parametros , "parametros.txt", sep = ';')


library(congressoR)



GenerateBlocos <- function(){
  cat('Baixando as informações dos Blocos', sep = '\n')
  
  lista <- GETBlocos(idLegislatura = idLegislatura)
  blocos <- lista[["dados"]]
  write.table(blocos,'cd_blocos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Blocos Completos',sep = '\n')
}

GenerateDeputados <- function(){
  
  ### DOWNLOAD DOS DEPUTADOS
  cat('Baixando as informações dos Deputados', sep = '\n')
  
  deputados <- data.frame()
  for(k in 1:1000){
    lista <- GETDeputados(dataInicio = dataInicio, dataFim = dataFim, itens = itens , pagina = k) # DEPUTADOS DA 56 legislatura ATIVOS/NÃO ATIVOS
    if(length(lista[["dados"]]) != 0 ){
      pagina <- lista[["dados"]]
      deputados <- bind_rows(deputados , pagina)
      
    }else{
      break
    }
  } 
  
  ListaIdDeputados <- deputados$id
  
  
  # Detalhes dos Deputados
  
  deputadosDetalhes <- data.frame()
  cat('Baixando os Detalhes',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  
  for(id in 1:length(ListaIdDeputados)){
    #cat(id, sep = '\n')
    setTxtProgressBar(pb, id)
    
    lista <- GETDeputadosId(ListaIdDeputados[id])
    deputado <- lista[["dados"]] %>% unlist(recursive = T,use.names = T) %>% data.frame() %>% t() %>% data.frame(row.names = NULL)
    deputadosDetalhes <- bind_rows(deputadosDetalhes,deputado)
  }
  close(pb)
  cat('Detalhes Completo',sep = '\n')
  
  
  
  # Despesas dos Deputados
  
  cat('Baixando as Despesas',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  despesas <- data.frame()
  for(id in 1:length(ListaIdDeputados)){
    #cat(id, sep = '\n')
    setTxtProgressBar(pb, id)
    despesa <- data.frame()
    
    for(k in 1:1000){
      lista <- GETDeputadoDespesa(id = ListaIdDeputados[id], ano = ano,itens = itens, pagina = k)
      
      if(length(lista[["dados"]]) != 0 ){
        pagina <- lista[["dados"]]
        pagina <- data.frame(idDeputado = ListaIdDeputados[id] , pagina)
        despesa <- bind_rows(despesa , pagina)
        
      }else{
        break
      }
    }
    
    
    despesas <- bind_rows(despesas, despesa)
    
  }
  close(pb)
  cat('Despesas Completo',sep = '\n')
  
  # Discursos dos Deputados 
  
  cat('Baixando os Discursos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  
  discursos <- data.frame()
  for(i in 1:length(ListaIdDeputados)){
    #cat(ListaIdDeputados[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    discurso <- data.frame()
    
    for(k in 1:1000){
      lista <- GETDeputadoDiscurso(id = ListaIdDeputados[i], dataInicio = dataInicio, dataFim = dataFim ,itens = itens, pagina = k)
      
      if(length(lista[["dados"]]) != 0 ){
        pagina <- do.call(data.frame,lista[["dados"]])
        pagina <- data.frame(idDeputado = ListaIdDeputados[i] , pagina)
        discurso <- bind_rows(discurso , pagina)
        
      }else{
        break
      }
    }
    
    
    discursos <- bind_rows(discursos, discurso)
    
  }
  close(pb)
  cat('Discursos Completo',sep = '\n')
  
  # Eventos dos Deputados 
  
  cat('Baixando os Eventos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  eventos <- data.frame()
  for(i in 1:length(ListaIdDeputados)){
    #cat(ListaIdDeputados[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    
    evento <- data.frame()
    
    for(k in 1:1000){
      lista <- GETDeputadoEventos(id = ListaIdDeputados[i],dataInicio = dataInicio,dataFim = dataFim ,itens = itens, pagina = k)
      
      if(length(lista[["dados"]]) != 0 ){
        
        pagina <- data.frame()
        for(j in 1:length(lista[["dados"]][["id"]]) ){
          
          a <- data.frame(lista[["dados"]][1:8][j,] , lista[["dados"]][["orgaos"]][[j]] , lista[["dados"]][["localCamara"]][j,])
          pagina <- bind_rows(pagina ,a)
        }
        pagina <- data.frame(idDeputado = ListaIdDeputados[i] , pagina)
        evento <- bind_rows(evento , pagina)
        
      }else{
        break
      }
    }
    
    
    eventos <- bind_rows(eventos, evento)
    
  }
  close(pb)
  cat('Eventos Completo',sep = '\n')
  
  
  
  # Frentes dos Deputados 
  cat('Baixando as Frentes',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  
  frentes <- data.frame()
  for(i in 1:length(ListaIdDeputados)){
    #cat(ListaIdDeputados[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    lista <- GETDeputadoFrentes(id = ListaIdDeputados[i])
    if( length(lista[["dados"]]) != 0 ){
      frente <- data.frame(idDeputado = ListaIdDeputados[i], lista[["dados"]][,-2])
    }else{ 
      frente <- data.frame(idDeputado = ListaIdDeputados[i])
    }
    
    frentes <- bind_rows(frentes , frente)
  }
  
  close(pb)
  cat('Frentes Completo',sep = '\n')
  
  # Orgaos dos Deputados 
  cat('Baixando os Órgãos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdDeputados), style = 3)
  
  orgaos <- data.frame()
  for(i in 1:length(ListaIdDeputados)){
    #cat(ListaIdDeputados[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    orgao <- data.frame()
    
    for(k in 1:1000){
      lista <- GETDeputadoOrgaos(id = ListaIdDeputados[i],dataInicio = dataInicio, dataFim = dataFim, pagina = k,itens = itens)
      
      if(length(lista[["dados"]]) != 0 ){
        pagina <- lista[["dados"]]
        pagina <- data.frame(idDeputado = ListaIdDeputados[i] , pagina)
        orgao <- bind_rows(orgao , pagina)
        
      }else{
        break
      }
    }
    
    
    orgaos <- bind_rows(orgaos, orgao)
    
  }
  close(pb)
  cat('Deputados Completo',sep = '\n')
  cat('Salvando Arquivos',sep = '\n')
  write.table(deputados,'cd_deputados.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(deputadosDetalhes,'cd_deputados_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(despesas,'cd_deputados_despesas.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(discursos,'cd_deputados_discursos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(eventos,'cd_deputados_eventos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(frentes,'cd_deputados_frentes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(orgaos,'cd_deputados_orgaos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Arquivos salvos com sucesso !',sep = '\n')
}

GenerateFrentes <- function(){
  cat('Baixando informações das Frentes',sep = '\n')
  
  lista <- GETFrentes(idLegislatura = idLegislatura, pagina = 1)
  frentes   <- lista[["dados"]]
  ListaIdFrentes <- frentes$id
  
  # DETALHES FRENTES
  cat('Baixando os Detalhes',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdFrentes), style = 3)
  
  frentes <- data.frame()
  for(i in 1:length(ListaIdFrentes)){
    #cat(ListaIdFrentes[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    lista <- GETFrenteId(id = ListaIdFrentes[i])
    frente <- lista[["dados"]] %>% unlist(recursive = T,use.names = T) %>% data.frame() %>% t() %>% data.frame()
    frentes <- bind_rows(frentes, frente)
  }
  
  close(pb)
  cat('Detalhes Completo',sep = '\n')
  
  # MEMBROS FRENTES
  
  cat('Baixando os Membros',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdFrentes), style = 3)
  frentesMembros <- data.frame()
  for(i in 1:length(ListaIdFrentes)){
    #cat(ListaIdFrentes[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    lista <- GETFrenteMembros(id = ListaIdFrentes[i])
    frente <- lista[["dados"]] 
    frentesMembros <- bind_rows(frentesMembros, frente)
  }
  close(pb)
  cat('Membros Completo',sep = '\n')
  
  cat('Frentes Completo',sep = '\n')
  cat('Salvando Arquivos',sep = '\n')
  write.table(frentesMembros,'cd_frentes_membros.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  #write.table(frentes,'cd_frentes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(frentes,'cd_frentes_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Arquivos salvos com sucesso !',sep = '\n')
  
}

GeneratePartidos <- function(){
  cat('Baixando informações dos Partidos',sep = '\n')
  ### DOWNLOAD PARTIDOS
  
  lista <- GETPartidos(dataInicio = dataInicio,dataFim = dataFim,itens = itens,pagina = 1)
  partidos  <- lista[["dados"]][,-4]
  
  ListaIdPartidos <- partidos$id
  
  cat('Baixando os Detalhes',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdPartidos), style = 3)
  
  # DETALHES PARTIDOS
  partidosDetalhes <- data.frame()
  for(i in 1:length(ListaIdPartidos)){
    #cat(ListaIdPartidos[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    lista <- GETPartidoId(id = ListaIdPartidos[i])
    partido <- lista[["dados"]] %>% unlist(recursive = T,use.names = T) %>% data.frame() %>% t() %>% data.frame()
    partidosDetalhes <- bind_rows(partidosDetalhes, partido)
  }
  close(pb)
  cat('Detalhes Completo',sep = '\n')
  
  # MEMBROS PARTIDOS
  
  cat('Baixando os Membros',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(ListaIdPartidos), style = 3)
  partidosMembros <- data.frame()
  for(i in 1:length(ListaIdPartidos)){
    #cat(ListaIdPartidos[i],i, sep = '\n')
    setTxtProgressBar(pb, i)
    lista <- GETPartidoMembros(id = ListaIdPartidos[i],dataInicio = dataInicio,dataFim = dataFim,itens = itens)
    partido <- lista[["dados"]] 
    partidosMembros <- bind_rows(partidosMembros, partido)
  }
  close(pb)
  cat('Membros Completo',sep = '\n')
  
  cat('Partidos Completo',sep = '\n')
  cat('Salvando Arquivos',sep = '\n')
  write.table(partidos,'cd_partidos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(partidosDetalhes,'cd_partidos_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(partidosMembros,'cd_partidos_membros.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Arquivos salvos com sucesso !',sep = '\n')
}

GenerateProposicoes <- function(){
  cat('Baixando informações das Proposições',sep = '\n')
  ### DOWNLOAD DAS PROPOSICOES
  
  
  # LISTA GERAL DE PROPOSICOES # UTILIZAR PARA O UPDATE E O BACKUP
  
  cat('Baixando as Proposições',sep = '\n')
  
  proposicoes <- data.frame()
  for(k in 1:1000){
    #cat(k, sep='\n')
    
    lista <- GETProposicoes(dataInicio = dataInicio,dataFim = dataFim, ano = ano ,dataApresentacaoInicio = dataInicio ,dataApresentacaoFim = dataFim,pagina = k,itens = itens)
    if( length(lista[["dados"]]) != 0 ){
      pagina <- lista[["dados"]]
      proposicoes <- bind_rows(proposicoes, pagina)
    }else{
      break
    }
  }
  
  cat('Proposições Completa',sep = '\n')
  
  
  
  proposicoesId <- proposicoes$id
  
  
  # DETALHES DAS PROPOSICOES
  cat('Baixando os Detalhes',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  
  proposicoesDetalhes <- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETProposicaoId(proposicoesId[i])
    proposicao <- lista[["dados"]] %>% unlist(recursive = T,use.names = T) %>% data.frame() %>% t() %>% data.frame()
    proposicoesDetalhes <- bind_rows(proposicoesDetalhes,proposicao)
  }
  
  close(pb)
  cat('Detalhes Completo',sep = '\n')
  
  # AUTORES DAS PROPOSICOES
  cat('Baixando os Autores',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  
  proposicoesAutores<- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    
    lista <- GETProposicaoAutores(proposicoesId[i])
    
    if( length(lista[["dados"]]) != 0){
      proposicao <- data.frame(idProposicao = proposicoesId[i] , lista[["dados"]] )
    }else{
      proposicao <- data.frame(idProposicao = proposicoesId[i])
    }
    
    proposicoesAutores <- bind_rows(proposicoesAutores,proposicao)
  }
  close(pb)
  cat('Autores Completo',sep = '\n')
  
  
  
  
  
  # TEMAS DAS PROPOSICOES
  cat('Baixando os Temas',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  proposicoesTemas <- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    
    lista <- GETProposicaoTemas(proposicoesId[i])
    
    if( length(lista[["dados"]]) != 0){
      proposicao <- data.frame(idProposicao = proposicoesId[i] , lista[["dados"]] )
    }else{
      proposicao <- data.frame(idProposicao = proposicoesId[i])
    }
    
    proposicoesTemas <- bind_rows(proposicoesTemas,proposicao)
  }
  
  close(pb)
  cat('Temas Completo',sep = '\n')
  
  
  
  # VOTACOES DAS PROPOSICOES
  
  cat('Baixando as Votações',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  votacoes <- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETProposicaoVotacoes(id = proposicoesId[i])
    
    if( length(lista[["dados"]]) != 0){
      votacao <- data.frame(idProposicao = proposicoesId[i], lista[["dados"]])
    }else{
      votacao <- data.frame(idProposicao = proposicoesId[i])
    }
    
    
    votacoes <- bind_rows(votacoes,votacao)
  }
  close(pb)
  cat('Votacoes Completo',sep = '\n')
  
  
  
  # TRAMITACAO DAS PROPOSICOES
  cat('Baixando as Tramitações',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  tramitacoes <- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETProposicaoTramitacoes(id = proposicoesId[i])
    
    if( length(lista[["dados"]]) != 0){
      tramitacao <- data.frame(idProposicao = proposicoesId[i], lista[["dados"]])
    }else{
      tramitacao <- data.frame(idProposicao = proposicoesId[i])
    }
    
    
    tramitacoes <- bind_rows(tramitacoes,tramitacao)
  }
  close(pb)
  cat('Tramitações Completo',sep = '\n')
  
  
  # RELACIONADAS DAS PROPOSICOES
  cat('Baixando as Relacionadas',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(proposicoesId), style = 3)
  relacionadas <- data.frame()
  for(i in 1:length(proposicoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    
    lista <- GETProposicaoRelacionadas(id = proposicoesId[i])
    
    if( length(lista[["dados"]]) != 0){
      relacionada <- data.frame(idProposicao = proposicoesId[i], lista[["dados"]])
    }else{
      relacionada <- data.frame(idProposicao = proposicoesId[i])
    }
    
    
    relacionadas <- bind_rows(relacionadas,relacionada)
  }
  
  close(pb)
  cat('Relacionadas Completo',sep = '\n')
  
  cat('Proposições Completo',sep = '\n')
  cat('Salvando Arquivos',sep = '\n')
  write.table(proposicoes,'cd_proposicoes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(proposicoesDetalhes,'cd_proposicoes_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(proposicoesAutores,'cd_proposicoes_autores.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(proposicoesTemas,'cd_proposicoes_temas.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(votacoes,'cd_proposicoes_votacoes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(tramitacoes,'cd_proposicoes_tramitacoes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(relacionadas,'cd_proposicoes_relacionadas.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Arquivos salvos com sucesso!',sep = '\n')
  
  
}

GenerateVotacoes <- function(){
  cat('Baixando informações sobre as Votações',sep = '\n')
  
  
  listavotacoes <- data.frame()
  for(k in 1:1000){
    cat(k, sep='\n')
    lista <- GETVotacoes(dataInicio = '2019-01-01', dataFim = '2019-12-31', pagina = k)
    if( length(lista[["dados"]]) != 0 ){
      pagina <- lista[["dados"]]
      listavotacoes <- bind_rows(listavotacoes, pagina)
    }else{
      break
    }
  }
  
  listavotacoes2020 <- data.frame()
  for(k in 1:1000){
    cat(k, sep='\n')
    lista <- GETVotacoes(dataInicio = '2020-01-01', pagina = k)
    if( length(lista[["dados"]]) != 0 ){
      pagina <- lista[["dados"]]
      listavotacoes2020 <- bind_rows(listavotacoes2020, pagina)
    }else{
      break
    }
  }
  listavotacoescompleto <- bind_rows(listavotacoes,listavotacoes2020)
  
  
  votacoesId <- listavotacoescompleto$id
  
  # DETALHES DAS VOTACOES
  cat('Baixando os Detalhes das Votações',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(votacoesId), style = 3)
  
  parte1 <- data.frame()
  parte2 <- data.frame()
  parte3 <- data.frame()
  parte4 <- data.frame()
  parte5 <- data.frame()
  for(i in 1:length(votacoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETVotacaoId(id = votacoesId[i])
    
    p1 <- lista[["dados"]][1:14] %>% unlist() %>% data.frame() %>% t() %>% data.frame()
    
    #if( !is.null(lista[["dados"]][["efeitosRegistrados"]]$dataHoraRegistro) ){
    # p2 <- data.frame(idVotacao = votacoesId[i], lista[["dados"]][["ultimaApresentacaoProposicao"]])
    #}else{
    #  p2 <- data.frame(idVotacao = votacoesId[i])
    #}
    
    if( length( lista[["dados"]][["efeitosRegistrados"]] ) != 0  ){
      p3 <- data.frame(idVotacao = votacoesId[i], lista[["dados"]][["efeitosRegistrados"]])
    }else{
      p3 <- data.frame(idVotacao = votacoesId[i])
    }
    
    if( length( lista[["dados"]][["objetosPossiveis"]] ) != 0){
      p4 <- data.frame(idVotacao = votacoesId[i], lista[["dados"]][["objetosPossiveis"]])
    }else{
      p4 <- data.frame(idVotacao = votacoesId[i])
    }
    
    if( length( lista[["dados"]][["proposicoesAfetadas"]] ) != 0){
      p5 <- data.frame(idVotacao = votacoesId[i], lista[["dados"]][["proposicoesAfetadas"]] )
    }else{
      p5 <- data.frame(idVotacao = votacoesId[i])
    }
    
    
    parte1 <- bind_rows(parte1, p1)
    #parte2 <- bind_rows(parte2, p2)
    parte3 <- bind_rows(parte3, p3)
    parte4 <- bind_rows(parte4, p4)
    parte5 <- bind_rows(parte5, p5)
  }
  close(pb)
  cat('Detalhes Completo',sep = '\n')
  
  # ORIENTACOES DAS VOTACOES
  
  cat('Baixando as Orientações das Votações',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(votacoesId), style = 3)
  
  posicionamento <- data.frame()
  for(i in 1:length(votacoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETVotacaoOrientacao(id = votacoesId[i])
    
    if( length( lista[["dados"]] ) != 0){
      posicao <- data.frame(idVotacao = votacoesId[i], lista[["dados"]] )
    }else{
      posicao <- data.frame(idVotacao = votacoesId[i])
    }
    posicionamento <- bind_rows(posicionamento, posicao)
  }
  close(pb)
  cat('Orientações Completo',sep = '\n')
  
  
  # VOTOS DAS VOTACOES 
  cat('Baixando os Votos das Votações',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(votacoesId), style = 3)
  votos <- data.frame()
  for(i in 1:length(votacoesId)){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETVotacaoVotos(id = votacoesId[i])
    
    if( length( lista[["dados"]] ) != 0){
      a <-  do.call(data.frame, lista[["dados"]])
      voto <- data.frame(idVotacao = votacoesId[i], a )
    }else{
      voto <- data.frame(idVotacao = votacoesId[i])
    }
    
    votos <- bind_rows(votos,voto)
  }
  close(pb)
  cat('Votos Completo',sep = '\n')
  cat('Votações Completa',sep = '\n')
  cat('Salvando Arquivos',sep = '\n')
  write.table(listavotacoescompleto,'cd_votacoes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(parte1,'cd_votacoes_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(parte3,'cd_votacoes_detalhes_efeitos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(parte4,'cd_votacoes_detalhes_objetosPossiveis.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(parte5,'cd_votacoes_detalhes_proposicoesAfetadas.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(posicionamento,'cd_votacoes_posicionamento.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  write.table(votos,'cd_votacoes_votos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8')
  cat('Arquivos salvos com sucesso !',sep = '\n')
  
}

GenerateOrgaos <- function(){
  ### ORGAOS
  cat('Baixando os Orgaos',sep = '\n')
  
  orgaos <- data.frame()
  for(k in 1:1000){
    #cat(k, sep='\n')
    lista <- GETOrgaos(dataInicio = dataInicio , dataFim = dataFim, itens = itens, pagina = k)
    
    if( length(lista[["dados"]]) != 0 ){
      pagina <- lista[["dados"]]
      orgaos <- bind_rows(orgaos, pagina)
    }else{
      break
    }
  }
  
  
  # DETALHES DOS ORGAOS
  orgaosId <- orgaos$id
  cat('Baixando os Detalhes dos Orgaos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(orgaosId), style = 3)
  orgaosdetalhes <- data.frame()
  for(i in 1:length(orgaosId) ){
    #cat(i, sep='\n')
    setTxtProgressBar(pb, i)
    lista <- GETOrgaosId(id = orgaosId[i])
    orgao <- lista[["dados"]] %>% unlist() %>% data.frame() %>% t() %>% data.frame()
    orgaosdetalhes <- bind_rows(orgaosdetalhes , orgao)
    
  }
  close(pb)
  # MEMBROS DOS ORGAOS
  cat('Baixando os Membros dos Orgaos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(orgaosId), style = 3)
  membros_orgaos <- data.frame()
  for(i in 1:length(orgaosId)){
    #cat(i, sep = '\n')
    setTxtProgressBar(pb, i)
    page <- data.frame()
    for(k in 1:1000){
      lista <-  GETOrgaoMembros(id = orgaosId[i], dataInicio = dataInicio, dataFim = dataFim, pagina = k, itens = itens)
      a <- data.frame(lista[["dados"]])
      
      if( length(a) != 0){ 
        page <- bind_rows(page, a) 
      }else{
        break
      }
      
    }
    
    
    
    if( length(page) != 0){
      page <- data.frame(idOrgaos = orgaosId[i] , page)
      
    }else{ page <- data.frame(idOrgaos = orgaosId[i])}
    
    
    membros_orgaos <- bind_rows(membros_orgaos, page)
  }
  
  close(pb)
  
  # VOTACOES DOS ORGAOS
  cat('Baixando as Votacoes dos Orgaos',sep = '\n')
  pb <- txtProgressBar(min = 0, max = length(orgaosId), style = 3)
  
  votacoes_orgaos <- data.frame()
  for(i in 1:length(orgaosId)){
    #cat(i, sep = '\n')
    setTxtProgressBar(pb, i)
    receiver <- data.frame()
    for(k in 1:1000){
      lista <- GETOrgaoVotacoes(id = orgaosId[i], dataInicio = (as.Date(parametros$dataFim) - 7), pagina = k, itens = itens)
      a <- data.frame(lista[["dados"]])
      
      if( length(a) != 0){ 
        receiver <- bind_rows(receiver, a) 
      }else{
        break
      }
      cat(k, sep='\n')
      
    }
    
    
    
    if( length(receiver) != 0){
      receiver <- data.frame(idOrgao = orgaosId[i] , receiver)
      
    }else{ receiver <- data.frame(idOrgao = orgaosId[i])}
    
    
    votacoes_orgaos <- bind_rows(votacoes_orgaos, receiver)
    
  }
  close(pb)
  write.table(orgaos,'cd_orgaos.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8', append = T)
  write.table(orgaosdetalhes,'cd_orgaos_detalhes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8', append = T)
  write.table(membros_orgaos,'cd_orgaos_membros.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8', append = T)
  write.table(votacoes_orgaos,'cd_orgaos_votacoes.csv',sep = ';', row.names = FALSE,fileEncoding = 'UTF-8', append = T)
}

GenerateBlocos()
GenerateDeputados()
GenerateFrentes()
GeneratePartidos()
GenerateProposicoes()
GenerateVotacoes()
GenerateOrgaos()




############### REFERENCIAS