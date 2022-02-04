
ui <- navbarPage("ANALISADOR DE DADOS DE MATRÍCULAS REALIZADAS NA EDUCAÇÃO DE JOVENS E ADULTOS NO ESTADO DO RIO GRANDE DO NORTE",
                 tabPanel("Home", tabName = "home", icon = icon("home"),
                          fluidPage(theme = shinytheme("flatly"),
                                    # Título
                                    titlePanel("BEM-VINDO!"),
                                    hr(),
                                    print(h4("Este é um aplicativo Shiny, o qual permite verificar o número de matrículas na 
                                    modalidade de Ensino para Jovens e Adultos (EJA) no estado do Rio Grande do Norte,
                                    através de gráficos e mapas.")),
                                    
                                    print(h4("Para a utilização dessa plataforma, se faz necessário informar dados oficiais extraídos
                                             do Censo Escolar, disponibilizados pelo Instituto Nacional de Estudos e Pesquisas Educacionais
                                             Anísio Teixeira (INEP).")),
                                          
                                    print(h4("O desenvolvimento deste aplicativo é resultante de uma consultoria realizada no
                                             Laboratório de Estatística Aplicada (LEA) da UFRN, pelos alunos consultores Ana Gabriela
                                             e Jocikleyton. Teve como orientação a Profª. Drª. 
                                             Vivacqua e co-orientação o Prof. Dr. Marcus Alexandre Nunes.")),
                                    
                                    print(h4("Com esta ferramenta, esperamos oferecer uma maneira fácil e prática de 
                                             visualizar a taxa de matrículas na EJA no Rio Grande do Norte.")),
                                    
                                    hr(),
                                    
                                    print(h2("Desenvolvedores")),
                                    print(h4(strong("Ana Gabriela Costa Sobrinho de Almeida"))),
                                    print(h5("Graduanda em Estatística pela Universidade Federal do Rio Grande do Norte")),
                                    strong("e-mail:"), em("ana.sobrinho.703@ufrn.edu.br"),
                                    
                                    print(h4(strong("Jocikleyton Ferreira de Oliveira"))),
                                    print(h5("Graduando em Estatística pela Universidade Federal do Rio Grande do Norte")),
                                    strong("e-mail:"), em("kleyton-ferreira1@hotmail.com"),
                                    
                                    br(),
                                    
                                    print(h2("Orientadores")),
                                    print(h4(strong("Carla Almeida Vivacqua"))),
                                    print(h5("Doutora em Engenharia Industrial pela University of Wisconsin - Madison")),
                                    print(h5("Mestre em Engenharia Industrial pela University of Wisconsin - Madison")),
                                    print(h5("Mestre em Estatística pela Universidade Estadual de Campinas")),
                                    print(h5("Bacharel em Estatística pela Escola Nacional de Ciências Estatísticas")),
                                   
                                    print(h4(strong("Marcus Alexandre Nunes"))),
                                    print(h5("Doutor em Estatística pela Penn State University")),
                                    print(h5("Mestre em Matemática Pura pela Universidade Federal do Rio Grande do Sul")),
                                    print(h5("Bacharel em Matemática Aplicada e Computacional pela Universidade Federal 
                                             do Rio Grande do Sul"))
                                    
                          )),
                 
                 tabPanel("Sobre os dados reais", tabName = "sobreDados", icon = icon("info-circle"),
                          titlePanel("Sobre os dados reais"),
                          print(h4("Para realizar a análise dos seus dados, clique em 'Baixar modelo dos dados reais' 
                     e preencha a tabela com as informações da EJA que desejar.")),
                          print(h3("Importante:")),
                          print(h4("- é mais seguro trabalhar com o modelo dos dados reais, fornecidos por este app;")),
                          print(h4("- o arquivo fornecido deverá ser .csv separado por ponto e vírgula;")),
                          print(h4("- não mude a ordem das colunas;")),
                          print(h4("- não mude o nome das variáveis;")),
                          print(h4("- respeite as letras em caixa alta e caixa baixa;")),
                          print(h4("- todas as categorias devem ser apresentadas como na figura a seguir.")),
                          
                          tags$img(src="tabelaModelo.png", height = 300, width = 800),
                          
                          titlePanel("Sobre as variáveis:"),
                          print(h4("Ano: ano em que os dados do estabelecimento foram inseridos;")),
                          print(h4("DIRED: Diretoria Regional de Educação, Cultura e Desportos. DIRED a que o estabelecimento pertence;")),
                          print(h4("Municipio: município a que o estabelecimento pertence;")),
                          print(h4("Dep.Adm: dependência administrativa do estabelecimento;")),
                          print(h4("Codigo: código de identificação do estabelecimento;")),
                          print(h4("Estabelecimento: nome da escola;")),
                          print(h4("Zona: zona a que o estabelecimento pertence;")),
                          print(h4("Matriculas: total de matrículas do estabelecimento;")),
                          print(h4("Modalidade.Ensino: presencial, semipresencial ou EaD;")),
                          print(h4("Grau.Ensino: fundamental ou médio.")),
                          
                          downloadButton(
                            outputId = "downloadtable",
                            label = "Baixar modelo dos dados reais",
                            icon = icon("download")
                          )),
                 
                 
                 tabPanel("Importação dos dados", tabName = "importDados", icon = icon("folder-open"),
                          titlePanel("Carregamento dos dados"),
                          hr(),
                          fileInput("file1", "Selecione um arquivo .csv",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          #br(),
                          #tableOutput("table1"),
                          helpText(print(h4("Atenção! Para utilizar dados reais, você deverá fornecer as informações
                               necessárias para que o aplicativo realize a análise. Vale ressaltar que existe um 
                               formato específico de planiha aceita por esta ferramenta. Caso tenha
                               dúvidas, consulte a aba 'Sobre os dados reais'.")),
                                   
                                   print(h4("Após o carregamento dos dados, será possível visualizar as informações das dez primeiras 
                             linhas em uma tabela nesta mesma seção. Em seguida, vá para a aba 'Análise dos dados' e
                             observe os resultados gerados."))), 
                          tableOutput("TabelaDados")
                         
                 ),
                 
                 
                 navbarMenu("Análise dos dados", icon = icon("bar-chart-o"),
                            
                            tabPanel("Dependência administrativa, grau de ensino e zona",
                                     fluidPage(theme = shinytheme("flatly"),
                                               titlePanel("Gráficos da quantidade de matrículas na EJA no RN"),
                                               hr(),
                                               
                                               sidebarLayout(
                                                 sidebarPanel(width = 3,
                                                              selectInput(inputId = "depAdm",
                                                                          label = "Dependência Administrativa:",
                                                                          choices = c("Todas","Municipal","Estadual","Federal","Privada"),
                                                                          selected = "Todas",
                                                                          width = 250),
                                                              selectInput(inputId = "grauEnsino",
                                                                          label = "Grau de Ensino:",
                                                                          choices = c("Todos","Fundamental","Médio"),
                                                                          selected = "Todos",
                                                                          width = 250),
                                                              selectInput(inputId = "zona",
                                                                          label = "Zona:",
                                                                          choices = c("Todas","Rural","Urbana"),
                                                                          selected = "Todas",
                                                                          width = 250)),
                                                 
                                                 mainPanel(width = 7,
                                                           withSpinner(plotOutput("plot"), color = getOption("spinner.color", "#008080"),
                                                                       type = getOption("spinner.type",4))
                                                 )))
                            ),
                            
                            tabPanel("DIRED",
                                     fluidPage(titlePanel("Mapa da quantidade de matrículas nas DIREDs do RN"),
                                               hr(),
                                               tmapOutput("mapRN", width = "100%", height = 500),
                                               
                                               absolutePanel(id = "controls", class = "panel panel-default",
                                                             top = 320, left = 43, width = 180, fixed=TRUE, height = "auto",
                                                             
                                                             span(tags$i(h6("Selecione a DIRED e o ano.")), 
                                                                  style="color:#045a8d"),
                                                             selectInput("listaDIRED", label = "DIRED", 
                                                                         choices = list("Todas as DIREDs","1","2","3",
                                                                                        "4","5","6","7","8","9","10","11",
                                                                                        "12","13","14","15","16")),
                                                             numericInput("listaAno", "Ano", value = ifelse(exists("dados"), max(dados$Ano), 2020), min = 1990, 
                                                                          max = 3000))
                                               
                                     ))
                 ))


