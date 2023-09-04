server_genetic_drift <- function(input, output, session) {
  
  ##### Genetic drift ####
  
  simulation <- eventReactive(input$go, {
    ## generate allele frequencies
    ##
    N <- round(input$N)
    n_gen <- 20 * N
    two_n <- 2 * N
    
    df <- data.frame(
      replicate(n = input$n_pops, expr = {
        sims <- rep(NA, n_gen)
        sims[1] <- input$p
        i <- 1
        while (sims[i] > 0 & sims[i] < 1) {
          i <- i + 1
          sims[i] <- rbinom(n = 1, size = two_n, prob = sims[i-1]) /
            two_n
        }
        return(sims)
      })
    ) %>% 
      filter_all(any_vars(!is.na(.))) %>%
      mutate(t = 0:(nrow(.) - 1)) %>% 
      gather(pop, p, -t) 
    
    df <- left_join(df, df %>%
                      filter(!is.na(p)) %>%
                      group_by(pop) %>%
                      summarize(p_final = last(p)), by = "pop"
    ) %>%
      mutate(p = ifelse(is.na(p), p_final, p))
    
    ## construct data frame for plotly
    ##
    n_gen <- max(df$t)
    d <- crossing(
      df,
      frame = 0:n_gen
    ) %>%
      filter(frame >= t)
    
    d
    
  })
  
  output$allele_frequency_plot <- renderPlotly({
    
    d <- simulation()
    n_gen <- max(d$t)
    
    ## plot it
    ##
    if (n_gen > 100) {
      x <- round(seq(0, n_gen, length.out = 100))
    } else {
      x <- 0:n_gen
    }
    
    gp <- ggplot(filter(d, t %in% x), aes(x = t, y = p, color = pop, frame = frame)) +
      geom_line() +
      scale_y_continuous(limits = c(0, 1)) +
      scale_color_brewer(palette = "Set1") + 
      xlab("Time (generations)") +
      ylab("Allele Frequency, p") +
      theme_bw() +
      theme(legend.position = "none")
    
    p_plot <- gp %>%
      ggplotly() %>%
      animation_opts(
        frame = 1000 / n_gen,
        transition = 0,
        redraw = FALSE
      ) %>%
      animation_slider(
        hide = TRUE
      )
  })
  
  output$summary_table <- renderTable({
    d <- simulation()
    d %>%
      filter(frame == max(frame)) %>%
      group_by(pop) %>%
      summarize(
        `Initial allele frequency` = first(p),
        `Final allele frequency` = last(p),
        `Time to fixation` = min(which(p %in% c(0, 1)))
      ) %>%
      rename(Population = pop) %>%
      mutate(N = input$N, Population = 1:nrow(.)) %>%
      select(Population, N, `Initial allele frequency`, `Final allele frequency`, `Time to fixation`)
  })
  
  output$copy1 <- renderPrint({
    
    simulation() %>%
      filter(frame == max(frame)) %>%
      group_by(pop) %>%
      summarize(p = last(p)) %>% 
      pull(p) %>%
      str_c(collapse = ",") %>%
      cat()
    
  })
  
  output$copy2 <- renderPrint({
    
    simulation() %>%
      filter(frame == max(frame)) %>%
      group_by(pop) %>%
      summarize(t = min(which(p %in% c(0, 1)))) %>% 
      pull(t) %>%
      str_c(collapse = ",") %>%
      cat()
    
  })
  
}