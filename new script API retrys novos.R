
# CAMARA NOVA API
# BLOCOS


GETBlocos <- function( id = NULL, idLegislatura = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','blocos') , query = list(id = id , idLegislatura = idLegislatura , 
                                                     pagina = pagina, itens = itens , ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  return(lista)
  }

GETBlocosId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','blocos', id) , query = list() )  %>%  content(as = 'text') %>% fromJSON()
  return(lista)
}


# DEPUTADOS

GETDeputados <- function( id = NULL, nome = NULL, idLegislatura = NULL , siglaUf = NULL, 
                          siglaPartido = NULL, siglaSexo = NULL, pagina = NULL, itens = NULL, dataInicio = NULL , dataFim = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados') , query = list(id = id, nome = nome, 
                                                                                           idLegislatura = idLegislatura , siglaUf = siglaUf, 
                                                                                           siglaPartido = siglaPartido, siglaSexo = siglaSexo, 
                                                                                           pagina = pagina, itens = itens, 
                                                                                           dataInicio = dataInicio , dataFim = dataFim, 
                                                                                           ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETDeputadosId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id) , query = list()) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETDeputadoDespesa <- function( id ,idLegislatura = NULL, ano = NULL, mes = NULL , cnpjCpfFornecedor = NULL, 
                                pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'despesas') , query = list( idLegislatura = idLegislatura, ano = ano,
                                                                                                            mes = mes , cnpjCpfFornecedor = cnpjCpfFornecedor, 
                                                                                                            pagina = pagina, itens = itens, 
                                                                                                            ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETDeputadoDiscurso <- function( id ,idLegislatura = NULL, dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'discursos') , query = list( idLegislatura = idLegislatura, dataInicio = dataInicio, 
                                                                                                             dataFim = dataFim, pagina = pagina, 
                                                                                                             itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETDeputadoEventos <- function( id , dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'eventos') , query = list(  dataInicio = dataInicio, 
                                                                                                              dataFim = dataFim, pagina = pagina, 
                                                                                                              itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}




GETDeputadoFrentes <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'frentes') , query = list()) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETDeputadoOrgaos <- function( id , dataInicio = NULL, dataFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','deputados', id , 'orgaos') , query = list(  dataInicio = dataInicio, 
                                                                                                            dataFim = dataFim, pagina = pagina, 
                                                                                                            itens = itens, ordem = ordem, ordenarPor = ordenarPor)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}




############## EVENTOS

GETEventos <- function( id = NULL , codTipoEvento = NULL, codSituacao = NULL, codTipoOrgao = NULL, idOrgao = NULL, dataInicio = NULL, dataFim = NULL, horaInicio = NULL , horaFim = NULL, pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos') , query = list( id = id , codTipoEvento = codTipoEvento, 
                                                                                          codSituacao = codSituacao, codTipoOrgao = codTipoOrgao, 
                                                                                          idOrgao = idOrgao, dataInicio = dataInicio, 
                                                                                          dataFim = dataFim, horaInicio = horaInicio , 
                                                                                          horaFim = horaFim, pagina = pagina, 
                                                                                          itens = itens, ordem = ordem, ordenarPor = ordenarPor )) %>% content(as = 'text') %>% fromJSON()
 
  return(lista)
}


GETEventoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETEventoDeputados <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'deputados') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETEventoOrgaos <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'orgaos') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETEventoPauta <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'pauta') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETEventoVotacoes <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','eventos', id, 'votacoes') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

########## FRENTES

GETFrentes <- function( idLegislatura = NULL , pagina = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes') , query = list(idLegislatura = idLegislatura , pagina = pagina)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETFrenteId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes', id) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETFrenteMembros <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','frentes', id, 'membros') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}



############# LEGISLATURAS
 
GETLegislatura <- function( id = NULL, data = NULL, pagina  = NULL, itens  = NULL, ordem  = NULL, ordenarPor  = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas') , query = list(id = NULL, data = NULL, 
                                                                                                        pagina  = NULL, itens  = NULL, 
                                                                                                        ordem  = NULL, ordenarPor  = NULL )) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETLegislaturaId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas', id ) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETLegislaturaMesa <- function( idLegislatura , dataInicio = NULL, dataFim = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','legislaturas', idLegislatura , 'mesa') , query = list(  dataInicio = dataInicio, 
                                                                                                                        dataFim = dataInicio)) %>%  content(as = 'text') %>% fromJSON()
  
  return(lista)
}

############### PARTIDOS 


GETPartidos <- function( sigla = NULL, dataInicio = NULL, dataFim  = NULL, idLegislatura  = NULL, pagina  = NULL, itens  = NULL, ordem = NULL , ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos') , query = list(sigla = sigla, dataInicio = dataInicio, 
                                                                                          dataFim  = dataFim, idLegislatura  = idLegislatura, 
                                                                                          pagina  = pagina, itens  = itens, ordem = ordem , ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETPartidoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETPartidoMembros <- function( id , dataInicio = NULL, dataFim  = NULL, idLegislatura  = NULL, pagina  = NULL, itens  = NULL, ordem = NULL , ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','partidos', id, 'membros') , query = list(dataInicio = dataInicio, dataFim  = dataFim, 
                                                                                                         idLegislatura  = idLegislatura, pagina  = pagina, 
                                                                                                         itens  = itens, ordem = ordem , ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


###################### PROPOSICOES

GETProposicoes <- function( id = NULL, siglaTipo = NULL, 
                               numero  = NULL, ano  = NULL, 
                               idDeputadoAutor  = NULL, autor  = NULL, 
                               siglaPartidoAutor = NULL , idPartidoAutor = NULL, 
                               siglaUfAutor = NULL, keywords = NULL, tramitacaoSenado = NULL, 
                               dataInicio = NULL, dataFim = NULL, dataApresentacaoInicio = NULL, 
                               dataApresentacaoFim = NULL, codSituacao = NULL, codTema = NULL, 
                               pagina = NULL, itens = NULL, ordem = NULL, ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes') , query = list(id = id, siglaTipo = siglaTipo, 
                                                                                             numero  = numero, ano  = ano, 
                                                                                             idDeputadoAutor  = idDeputadoAutor, autor  = autor, 
                                                                                             siglaPartidoAutor = siglaPartidoAutor , idPartidoAutor = idPartidoAutor, 
                                                                                             siglaUfAutor = siglaUfAutor, keywords = keywords, 
                                                                                             tramitacaoSenado = tramitacaoSenado, dataInicio = dataInicio, 
                                                                                             dataFim = dataFim, dataApresentacaoInicio = dataApresentacaoInicio, 
                                                                                             dataApresentacaoFim = dataApresentacaoFim, codSituacao = codSituacao, 
                                                                                             codTema = codTema, pagina = pagina, itens = itens, 
                                                                                             ordem = ordem, ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETProposicaoId <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id) , query = list() ) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETProposicaoAutores <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'autores') , query = list() ) %>% content(as = 'text') %>% fromJSON()
                 
                 return(lista)
}

GETProposicaoRelacionadas <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'relacionadas') , query = list() ) %>% content(as = 'text') %>% fromJSON()
                 
                 return(lista)
}

GETProposicaoTemas <- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id, 'temas') , query = list() ) %>% content(as = 'text') %>% fromJSON()
                 
                 return(lista)
}


GETProposicaoTramitacoes <- function( id , dataInicio = NULL, dataFim = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id , 'tramitacoes') , query = list(dataInicio = dataInicio, dataFim = dataFim ) ) %>% content(as = 'text') %>% fromJSON()
                 
                 return(lista)
}

GETProposicaoVotacoes <- function( id , ordem = NULL, ordenarPor = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','proposicoes', id , 'votacoes') , query = list(ordem = ordem, ordenarPor = ordenarPor ) ) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


################ VOTACOES 



GETVotacoes<- function( id = NULL, idProposicao = NULL, 
                        idEvento  = NULL, idOrgao  = NULL, 
                        dataInicio  = NULL, dataFim  = NULL, 
                        pagina = NULL , itens = NULL, 
                        ordem = NULL, ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", progress() ,url = base_url, path = list('api','v2','votacoes') , query = list(id = id, idProposicao = idProposicao, 
                                                                                             idEvento  = idEvento, idOrgao  = idOrgao, 
                                                                                             dataInicio  = dataInicio, dataFim  = dataFim, 
                                                                                             pagina = pagina , itens = itens, 
                                                                                             ordem = ordem, ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETVotacaoId<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETVotacaoOrientacao<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id, 'orientacoes') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETVotacaoVotos<- function( id ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','votacoes', id, 'votos') , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


####################### ORGAOS

GETOrgaos <- function( id = NULL, sigla = NULL, 
                       codTipoOrgao  = NULL, dataInicio  = NULL, 
                       dataFim  = NULL, pagina  = NULL, 
                       itens = NULL , ordem = NULL, 
                       ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos') , query = list(id = id, sigla = sigla, 
                                                                                          codTipoOrgao  = codTipoOrgao, dataInicio  = dataInicio, 
                                                                                          dataFim  = dataFim, pagina  = pagina, 
                                                                                          itens = itens , ordem = ordem, 
                                                                                          ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETOrgaosId <- function( id){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id) , query = list()) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}

GETOrgaoEventos <- function( id , idTipoEvento = NULL,
                              dataInicio  = NULL, 
                              dataFim  = NULL, pagina  = NULL, 
                              itens = NULL , ordem = NULL, 
                              ordenarPor = NULL){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'eventos') , query = list(idTipoEvento = idTipoEvento, dataInicio  = dataInicio, 
                                                                                            dataFim  = dataFim, pagina  = pagina, 
                                                                                            itens = itens , ordem = ordem, 
                                                                                            ordenarPor = ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}


GETOrgaoMembros <- function( id , 
                              dataInicio  = NULL, 
                              dataFim  = NULL, pagina  = NULL, 
                              itens = NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'membros') , query = list(dataInicio  = dataInicio, 
                                                                                            dataFim  = dataFim, pagina  = pagina, 
                                                                                            itens = itens)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}



GETOrgaoVotacoes <- function( id , 
                              idProposicao  = NULL, 
                              dataInicio  = NULL, dataFim  = NULL, 
                              pagina = NULL, itens = NULL, ordem = NULL, ordenarPor= NULL ){
  base_url <- "https://dadosabertos.camara.leg.br"
  lista <- RETRY("GET", url = base_url, path = list('api','v2','orgaos', id, 'votacoes') , query = list(idProposicao  = idProposicao, 
                                                                                                       dataInicio  = dataInicio, dataFim  = dataFim, 
                                                                                                       pagina = pagina, itens = itens, ordem = ordem, ordenarPor= ordenarPor)) %>% content(as = 'text') %>% fromJSON()
  
  return(lista)
}









############### REFERENCIAS