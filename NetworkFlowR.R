library(RNetLogo)
library(ggplot2)

nl_run <- function(ticks = 10, inputs = 30, outputs = 30, balance = 0.2) {
  # Runs NetLogo model:
  #   Start NetLogo, load NetworkFlow model 
  #   Set globals (inputs, outputs, link-balance)
  #   Runs model
  # Returns data.frame with diversity adjusted flow on system output 
  #   for every tick
  nl_path <- "c:/Program Files (x86)/NetLogo 5.1.0"
  nl_model_path <- "C:/Users/dare/Documents/NetLogo/NetworkFlow"
  nl_model <- "NetworkFlow/NetworkFlow v 1.5.nlogo"
  nl_instance <- NLStart(nl_path, gui = FALSE )
  NLLoadModel( file.path(nl_model_path, nl_model) )
  
  # setup
  NLCommand( sprintf( "set total-inputs %d", inputs))
  NLCommand( sprintf( "set total-outputs %d", outputs))
  NLCommand( sprintf( "set link-balance %s", balance))
  NLCommand( sprintf( "set initial-links %d", 3))
  NLCommand( "setup")
  
  #run
  end_condition <- sprintf("ticks < %d and flow-diversity <= %d", ticks, outputs - 1)
  sim_report <- NLDoReportWhile( 
    end_condition, 
    command = "go", 
    reporter = c( "ticks", 
                  "count processes", 
                  "count links",
                  "sum [ node-flow ] of outputs",
                  "sum [ node-flow * count p-inputs / total-inputs ] of outputs"
    ),
    as.data.frame = TRUE,
    df.col.names = c("tick", "processes", "links", "f", "h")
  )
  NLCommand( sprintf('export-view "%s"', file.path(nl_model_path, "ExportView.png") ))
  NLCommand( sprintf('export-world "%s"', file.path(nl_model_path, "ExportWorld.csv") ))
  
  # close netlogo instance
  NLQuit()
  return(sim_report)  
}


plotSim <- function(sim_report) {
  p <- ggplot(sim_report,aes(x = tick))
  p <- p + geom_line(aes(y = 30), linetype = "longdash", color="grey")
  p <- p + geom_step(aes(y = f), linetype = "dashed" )
  p <- p + geom_step(aes(y = h), linetype = "solid")
  #p <- p + geom_step(aes(y = processes), linetype = "solid", color="grey")
  p <- p + ylab("Pretok") + xlab("Čas")
  p <- p + theme_bw()
  #p <- p + geom_smooth(aes(y = h), method = "loess")
  #  p + ggtitle("Pretok")
  return(p)
}

# simple run
inputs <- 30
outputs <- 30
balance <- 0.18
sim_report <- nl_run(1000, inputs, outputs, balance)
plotSim(sim_report[1:400,])
plotSim(sim_report)
plotComp(sim_report[1:400,])

tail(sim_report)
  
