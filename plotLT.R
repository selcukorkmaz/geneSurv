plotLT <- function(x, y=NULL, surv.col = 'gg.def', lty.est = 1,
                    back.white = TRUE, interactive = TRUE, xlab = 'Time Interval',
                    ylab = 'Cumulative Survival Probability', main = '', ...){



  group = NULL; surv = NULL

  s2 = x$testResult$lifeTable

  if(class(s2) == "list"){
    strata = length(s2)
  }else(strata = 0)


  #strata <- ifelse(is.null(s2$strata) == T, 1, length(s2$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)

  pl <- if(strata == 0) {


    dat <- data.frame(
      time = c(s2[,1]),
      surv = as.numeric(c(s2[,8]))
    )


    col <- ifelse(surv.col == 'gg.def', 'black', surv.col)

    pl <- ggplot(dat, aes(x = time, y = surv)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(col = col, lty = lty.est)


  } else {

    n = as.data.frame(lapply(s2, FUN = function(x){nrow(x)}))
    groups <- factor(names(s2))

    gr.name <-  "factor"
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,n); n.ind <- cumsum(n.ind)
    for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]

    for(i in 1:strata){

      gr.df[[i]] <- data.frame(
        time = c(0, s2[[i]][1]),
        surv = as.numeric(c(1, s2[[i]][8])[[2]]),
        group = rep(groups[i], n[i])
      )

    }

    dat <- do.call(rbind, gr.df)
    colnames(dat)[-1] = c("time", "surv", "group")
    # dat.cens <- subset(dat, cens != 0)

    pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
      xlab(xlab) + ylab(ylab) + ggtitle(main) +
      geom_step(aes(col = group, lty = group))

    col <- if(length(surv.col == 1)){
      scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
    } else{
      scale_colour_manual(name = gr.name, values = surv.col)
    }

    pl <- if(surv.col[1] != 'gg.def'){
      pl + col
    } else {pl + scale_colour_discrete(name = gr.name)}

    line <- if(length(lty.est) == 1){
      scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
    } else {scale_linetype_manual(name = gr.name, values = lty.est)}

    pl <- pl + line

  }

  pl <- if(back.white) {pl + theme_bw()
  } else (pl)

  pl <- if(interactive) {
    plotly::ggplotly(pl)
  } else (pl)
  pl


}


