library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(treemap)
library(plotly)
library(fmsb)
library(ggiraph)


function(input, output, session) {
  Pokemons <- read.csv("data/Pokemon.csv")
  Types <- read.csv("data/Types.csv")
  
  selected_pokemon <- reactive({
    req(input$searchPokemon_rows_selected)
    Pokemons[input$searchPokemon_rows_selected, ]
  })
  
  selected_type <- reactive({
    if (is.null(input$searchType_rows_selected)) {
      return(NULL)
    } else {
      return(Pokemons$Type.1[input$searchType_rows_selected])
    }
  })
  
  # first tab
  
  output$searchPokemon <- DT::renderDataTable({
    DT::datatable(data.frame(Pokemons$Name),
                  options = list(scrollY = 300,
                                 paging = FALSE,
                                 pageLength = 1000,
                                 lengthChange = FALSE,
                                 info = FALSE,
                                 searching = TRUE,
                                 colnames = c("Pokemon Name"),
                                 columnDefs = list(list(className = "dt-center", targets = "_all"))
                                 ),
                  selection = list(mode = "single", target = "row"),
                  rownames = FALSE)
  })
  
  output$pokemonStatsPlot <- renderPlot({
    stats <- selected_pokemon() %>% 
      select(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>%
      pivot_longer(everything(), names_to = "Stat", values_to = "Value")
    
    ggplot(stats, aes(x = Stat, y = Value, fill = Stat)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      ggtitle(paste("Stats for:", selected_pokemon()$Name)) +
      theme(plot.title = element_text(size = 24, face = "bold", color = "#cc0000", hjust = 0.5)) +
      scale_fill_manual(values = c("HP" = "red", "Attack" = "orange", 
                                   "Defense" = "blue", "Sp..Atk" = "yellow",
                                   "Sp..Def" = "purple", "Speed" = "green"))
  })
  
  output$PokemonStatsTable1 <- renderTable({
    selected_pokemon() %>% 
      select(Name, Type.1, Type.2, Generation, Legendary)
  })
  
  output$PokemonStatsTable2 <- renderTable({
    selected_pokemon() %>% 
      select(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed)
  })
  
  output$pokemonName1 <- renderUI({
    selected_pokemon() %>% {
      tags$div(class = "pokemon-name", .$Name)
    }
  })
  
  output$pokemonName2 <- renderUI({
    selected_pokemon() %>% {
      tags$div(class = "pokemon-name", .$Name)
    }
  })
  
  output$imagePokemon <- renderImage({
    pokemon_image <- list(src = paste0("./data/pokemon_images/", 
                                   tolower(selected_pokemon()$Name), 
                                   ".png"),
                      contentType = "image/png",
                      width = 256,
                      height = 256,
                      alt = "Pokemon"
                      )
    return(pokemon_image)
  }, deleteFile = FALSE)
  
  output$radarChartComparision <- renderPlot({
    row <- selected_pokemon() %>%
      select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed)
    type_avg <- Pokemons %>%
      filter(Type.1 == selected_pokemon()$Type.1) %>%
      summarize(across(c(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed), mean))
    row <- rbind(rep(255, 6), rep(0, 6), row, type_avg)
    
    radarchart(
      row, axistype = 1,
      cglcol = "grey", cglty = 1, axislabcol = "grey", plwd = 3, plty = 1,
      caxislabels = c(0, 50, 100, 150, 200, 250), cglwd = 0.8, vlcex = 0.8,
      pcol = c(rgb(0.9,0.2,0.2,0.9), rgb(0.1,0.8,0.1,0.9)),
      pfcol = c(rgb(0.9,0.2,0.2,0.6), rgb(0.1,0.8,0.1,0.2)),
      title = paste("Stats Comparison for", selected_pokemon()$Name),

    )
    
    legend("bottomleft", legend = c(selected_pokemon()$Name, paste(selected_pokemon()$Type.1,"type average")), 
           col = c(rgb(0.9,0.2,0.2,0.9), rgb(0.1,0.8,0.1,0.9)),
           pch = 15, pt.cex = 2, bty = "n", cex = 0.8, title = "Groups")
  })
  
  # second tab
  
  output$searchType <- DT::renderDataTable({
    DT::datatable(data.frame(Types$Type),
                  options = list(scrollY = 300,
                                 paging = FALSE,
                                 pageLength = 20,
                                 lengthChange = FALSE,
                                 info = FALSE,
                                 searching = TRUE,
                                 colnames = c("Pokemon Type"),
                                 columnDefs = list(list(className = "dt-center", targets = "_all"))
                  ),
                  selection = list(mode = "single", target = "row"),
                  rownames = FALSE)
  })
  
  output$pokemonStatsSinaPlot <- renderPlotly({
    filtered_data <- if (is.null(selected_type())) {
      Pokemons
    } else {
      Pokemons %>%
        filter(Type.1 == selected_type() | Type.2 == selected_type())
    }
    
    plot_data <- filtered_data %>%
      select(HP, Defense, Attack, Sp..Atk, Sp..Def, Speed) %>%
      pivot_longer(cols = everything(), names_to = "stat", values_to = "value")
    
    p <- ggplot(plot_data, aes(x = stat, y = value, fill = stat)) +
      geom_violin() +
      geom_jitter(alpha = 0.5) +
      geom_boxplot(width = 0.1, fill = "white", color = "black", position = position_dodge(0.9)) +
      theme_minimal() +
      theme(legend.position = "none") +
      labs(x = "Stat", y = "Value", title = "Overall Pokemon Stats")
    
    ggplotly(p)
  })
  
  output$pokemonTypesTreeMap <- renderPlotly({
    pokemon_data_treemap <- Pokemons %>%
      group_by(Type.1, Type.2) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(Type.2 = ifelse(Type.2 == "", Type.1, Type.2)) %>%
      mutate(Type.1 = ifelse(Type.2 == Type.1, "", Type.1)) %>%
      rename(label = Type.2, parent = Type.1) %>%
      mutate(label = ifelse(parent == "", label, paste(parent, label, sep = " - ")))
    
    cols = c("Bug" = "red",
             "Dark" = "green",
             "Dragon" = "blue",
             "Electric" = "coral",
             "Fairy" = "cyan",
             "Fighting" = "gold",
             "Fire" = "firebrick",
             "Flying" = "gold4",
             "Ghost" = "lightblue",
             "Grass" = "pink",
             "Ground" = "grey",
             "Ice" = "yellow",
             "Normal" = "magenta",
             "Poison" = "orange",
             "Psychic" = "brown",
             "Rock" = "purple",
             "Steel" = "darkblue",
             "Water" = "aquamarine")
    
    plot <- pokemon_data_treemap %>%
      plot_ly(
        labels = ~label,
        parents = ~parent,
        values = ~count,
        type = "treemap",
        colors = cols,
        domain = list(column = 0),
        hovertemplate = paste(
          "<b>%{label}</b><br>",
          "Count: %{value}<br>"
        )
      )%>%
      layout(title = "Pokémon Type Distribution")
    
    plot <- event_register(plot, "plotly_click")
    
    plot
  })
  
  output$heatmap <- renderPlot({
    type_combinations <- Pokemons %>%
      count(Type.1, Type.2) %>%
      rename(Count = n)
    
    ggplot(type_combinations, aes(x = Type.1, y = Type.2, fill = Count)) +
      geom_tile() +
      theme_minimal() +
      labs(x = "Primary Type", y = "Secondary Type", title = "Heatmap of Pokemon Type Combinations") +
      scale_fill_gradient(low = "white", high = "red")
  })
  
  output$legendary <- renderPlot({
    Pokemons %>%
      pivot_longer(cols = c(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed), 
                   names_to = "Stat", values_to = "Value") %>%
      ggplot(aes(x = Stat, y = Value, fill = Legendary)) +
      geom_boxplot() +
      theme_minimal() +
      labs(x = "Stat", y = "Value", title = "Legendary vs Non-Legendary Pokémon Stats") +
      scale_fill_manual(values = c("True" = "#ff0000", "False" = "#0000ff"))
  })
  
  
  output$legend_plot <- renderPlotly({
    pokemon_type <- as.character(input$legendary_select)
    pokemon <- switch(pokemon_type,
                      "all" = Pokemons,
                      "non_legendary" = Pokemons[Pokemons$Legendary == "False", ],
                      "legendary" = Pokemons[Pokemons$Legendary == "True", ])
    
    g <- ggplot(pokemon, aes(x = HP, y = Attack, color = Type.1, text = Name)) +
      geom_jitter(width = 0.2, height = 0.2) +
      labs(x = "Health", y = "Attack", title = "Pokemon Health vs Attack") +
      theme_minimal() +
      theme(legend.position = "right") +
      scale_x_continuous(limits = c(0, 190)) +
      scale_y_continuous(limits = c(0, 255))
    
    p <- ggplotly(g, tooltip = "text") %>% partial_bundle() 
    p %>% htmlwidgets::onRender("
    function(el, x) {
      // when hovering over an element, do something
      el.on('plotly_hover', function(d) {

        // extract tooltip text
        txt = d.points[0].data.text;
        // image is stored locally
        image_location = 'data/images/bulbasaur.png';

        // define image to be shown
        var img = {
          // location of image
          source: image_location,
          // top-left corner
          x: 0,
          y: 1,
          sizex: 0.2,
          sizey: 0.2,
          xref: 'paper',
          yref: 'paper'
        };

        // show image and annotation 
        Plotly.relayout(el.id, {
            images: [img] 
        });
      })
    }
    ")
    g
  })
  
  # PUT logos
  
  output$logoPuttab1 <- renderImage({
    put_image <- list(src = "./data/PUT_logo.png",
                      contentType = "image/png",
                      alt = "PUT logo")
  }, deleteFile = FALSE)
  
  output$logoPuttab2 <- renderImage({
    put_image <- list(src = "./data/PUT_logo.png",
                      contentType = "image/png",
                      alt = "PUT logo")
  }, deleteFile = FALSE)
}

