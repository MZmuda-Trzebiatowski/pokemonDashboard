library(shiny)
library(shinydashboard)
library(plotly)

# adding some comments... 

dashboardPage( 
  skin = "red",
  
  dashboardHeader(
      title = "Pokemon Dashboard",
      tags$li(
        class="dropdown",
        tags$a(
          href = "#",
          class = "dropdown-toggle",
          `data-toggle` = "dropdown",
          tags$i(class = "fa fa-question-circle"),
          style = "color: #fff;"
        ),
        tags$ul(
          class = "dropdown-menu",
          style = "min-width: 300px;",
          tags$li(
            tags$div(
              style = "padding: 10px;",
              "This dashboard is used to explore, compare and enjoy Pokemons!",
              tags$p("Featured tools: "),
              tags$ul(
                tags$li("In the first tab You can choose a Pokemon you would like to inspect!"),
                tags$li("There will be a picture displayed, next to it a histogram with chosen Pokemon stats also a table with the exact values. And in the bottom right You can see a radar chart that compares given Pokemon to it's type average in different stats."),
                tags$li("In the second tab You can find much more general information!"),
                tags$li("Firsty you can choose which type of Pokemon would you like to inspect (or just leave it blank and you'll see every type at once) on the sina plot to the right."),
                tags$li("Below that you can see a treemap containg the distribution of different types of Pokemons, next below it you can see a heatmap showing which combinations of types are the most common and a bar plot comparing the legendary with non-legendary pokemons accross all stats!"),
                tags$li("And last but not least a jitter plot where you can see the info about the differnt pokemons's attack and health, you can choose wether you want to see all Pokemons or just the legendary ones or the non-legandary.")
              )
            )
          )
        )
      )
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Chosen Pokemon", tabName = "tab1", icon = icon("dashboard")),
      menuItem("General plots", tabName = "tab2", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .dataTables_filter input {
          width: 200px;
          padding: 10px;
          font-size: 16px;
          border: 1px solid #ddd;
          border-radius: 5px;
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.12);
        }
        table.dataTable {
          background-color: #ffe6e6;
          color: #990000;
        }
        table.dataTable thead {
          background-color: #cc0000;
          color: white;
        }
        table.dataTable tbody tr {
          background-color: #ffe6e6;
        }
        table.dataTable tbody tr:hover {
          background-color: #ffcccc;
        }
        .pokemon-image {
          text-align: center;
        }
        .pokemon-name {
          text-align: center;
          font-size: 24px;
          font-weight: bold;
          color: #cc0000;
        }
                      "
                      )
                 )
    ),
    
    tabItems(
      tabItem(
        tabName = "tab1",
        fluidRow(
          box(
            width = 3,
            DT::dataTableOutput("searchPokemon")
          ),
          box(
            width = 4,
            htmlOutput("pokemonName1"),
            tags$div(class = "pokemon-image", imageOutput("imagePokemon", height = "auto"))
          ),
          box(
            width = 5,
            htmlOutput("pokemonName2"),
            tableOutput("PokemonStatsTable1"),
            tableOutput("PokemonStatsTable2")
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotOutput("pokemonStatsPlot")
          ),
          box(
            width = 6,
            plotOutput("radarChartComparision")
          )
        ),
        fluidRow(
          box(
            width = 12,
            height = 286,
            plotOutput("logoPuttab1")
          )
        )
      ),
      
      tabItem(
        tabName = "tab2",
        fluidRow(
          box(
            width = 3,
            DT::dataTableOutput("searchType")
          ),
          box(
            width = 9,
            plotlyOutput("pokemonStatsSinaPlot")
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("pokemonTypesTreeMap")
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotOutput("heatmap")
          ),
          box(
            width = 6,
            plotOutput("legendary")
          )
        ),
        fluidRow(
          box(
            width = 3, 
            selectInput("legendary_select", "Select Pokemon Type:",
                          c("All" = "all", 
                            "Non-Legendary" = "non_legendary", 
                            "Legendary" = "legendary"),
                        selectize = FALSE)),
          box(
            width = 9,
            plotlyOutput("legend_plot")
          )
        ),
        fluidRow(
          box(
            width = 12,
            height = 286,
            plotOutput("logoPuttab2")
          )
        )
      )
    )
  )
)