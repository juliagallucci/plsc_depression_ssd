depColors <- list()
depColors$oc <- as.matrix(as.character(recode_factor(antidepressant_info$antidepressants, yes = 'red', no = 'blue')))
depColors$gc <- as.matrix(c(yes = 'red', no = 'blue'))

varColors <- list()
varColors$oc[[1]] <- as.matrix(rep("purple", 7664))
varColors$oc[[2]] <- as.matrix(c(rep("purple", 8), rep("olivedrab3",4)))

antidepressant_info <-
  antidepressant_info %>%
  mutate(col = case_when(
    antidepressants =="yes" ~ "red",
    antidepressants == "no" ~ "blue"
  ))

descr <- antidepressant_info %>% 
  select(antidepressants,col)


####
check.dim = 2
lxly <- cbind(pls.res_regress$TExPosition.Data$lx[,check.dim], pls.res_regress$TExPosition.Data$ly[,check.dim])
colnames(lxly) <- c(paste0("LV", check.dim, c(".Brain", ".Clinical")))

lxly.boot <- Boot4Mean(lxly, descr$antidepressants, niter = 1000)
colnames(lxly.boot$GroupMeans) <- colnames(lxly.boot$BootCube) <- c(paste0("LV", check.dim, c(".Brain", ".Clinical")))

lxly.all <- createFactorMap(lxly,
                            title = paste0("Latent variables"),
                            col.background = NULL,
                            col.axes = "gray60",
                            alpha.axes = 0.5,
                            col.points = depColors$oc,
                            alpha.points = 0.5)

lxly.avg <- createFactorMap(lxly.boot$GroupMeans,
                            col.points = depColors$gc[rownames(lxly.boot$GroupMeans),],
                            col.labels =  depColors$gc[rownames(lxly.boot$GroupMeans),], 
                            pch = 17, alpha.points = 0.5, text.cex = 6)

lxly.CI <- MakeCIEllipses(lxly.boot$BootCube,
                          col =  depColors$gc[rownames(lxly.boot$BootCube),],
                          names.of.factors = c(paste0("LV", check.dim, c(".Brain", ".Clinical"))), alpha.ellipse = 0.1, line.size = 1.5)

lxly.out <- lxly.all$zeMap_background + lxly.all$zeMap_dots + lxly.CI + lxly.avg$zeMap_dots + lxly.avg$zeMap_text + coord_cartesian()

plotly::ggplotly(lxly.out, dynamicTicks = TRUE)


ggplotify::as.ggplot(lxly.out)









lx_ly = as.data.frame(lxly)
dep_lxly = cbind(lx_ly,antidepressant_info$antidepressants)
dep_lxly = cbind(dep_lxly, antidepressant_info[,1])
colnames(dep_lxly) <- c("LX","LY","STATUS", "SUB")
dep_lxly <- dep_lxly %>% 
  gather(key = "LXLY", value = "lxly_val", LX, LY) %>%
  convert_as_factor(STATUS, LXLY)
get_anova_table(anova_test(data = dep_lxly, dv = lxly_val,between = STATUS, within = LXLY, wid = SUB))

