

# promvis.R ----------------------------------------------------------
# Header
# Filename:      promvis.R
# Description:   This module provides functions for visualising a process.
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    04 May 2018
# Last Revision: 06 June 2018
# Version:       0.0.3

# Version   Date               Action
# -----------------------------------
# 0.0.1     04 May 2018        Initial issue
# 0.0.2     06 June 2018       Functions addDiagrammeRGraph() modified: Warns if graph tables are not built
# 0.0.3     06 June 2018       Functions plot.process() modified: Warns if plotter = 'DiagrammeR' and no DiagrammeR graph is built.
# -----------------------------------


# addDiagrammeRGraph = function(obj){
#   if(inherits(obj$nodes, c('data.frame', 'tibble', 'data.table')) & inherits(obj$links, c('data.frame', 'tibble', 'data.table'))){
#     nodes = obj$nodes
#     edges = obj$links
#     support('dplyr', 'DiagrammeR')
#     nodes %<>%
#       mutate(weight = scales::rescale(weight, from = c(0, max(weight)))) %>%
#       mutate(weight = ifelse(skill == 'End', Inf, weight))
#
#     if(is.null(obj$DiagrammeRGraph)){
#       create_node_df(n = nrow(nodes),
#                      label = nodes$label,
#                      shape = nodes$shape,
#                      color_level = nodes$weight,
#                      style = "rounded,filled",
#                      fontcolor = nodes$fontcolor,
#                      color = nodes$color,
#                      tooltip = nodes$tooltip,
#                      penwidth = 1.5,
#                      fixedsize = FALSE,
#                      fontname = "Arial") -> nodes_df
#
#       min_level <- min(nodes_df$color_level)
#       max_level <- max(nodes_df$color_level[nodes_df$color_level < Inf])
#
#       create_edge_df(from = edges$from_id,
#                      to = edges$to_id,
#                      label = edges$weight,
#                      penwidth = edges$width,
#                      color = chif(obj$settings$link.weight != "frequency", "red4", "dodgerblue4"),
#                      fontname = "Arial") -> edges_df
#
#       create_graph(nodes_df, edges_df) %>%
#         add_global_graph_attrs(attr = "rankdir", value = "TB",attr_type = "graph") %>%
#         add_global_graph_attrs(attr = "layout", value = "dot", attr_type = "graph") %>%
#         colorize_node_attrs(node_attr_from = "color_level",
#                             node_attr_to = "fillcolor",
#                             palette = chif(obj$settings$link.weight != "frequency", "Reds", "PuBu"),
#                             default_color = "white",
#                             cut_points = seq(min_level-0.1, max_level+.1, length.out = 9)) -> obj$DiagrammeRGraph
#     }} else {
#     warnif(T, "from function addDiagrammeRGraph(): Graph tables are not built! Please add using function 'addGraphTables()' (No graph generated).")
#   }
#   return(obj)
# }
#
#
# plot.process = function(obj, plotter = 'grviz', direction = 'top.down', node_colors = 'navy', edge_colors = 'black', onClickShinyInput = NULL){
#   if(inherits(obj$nodes, c('data.frame', 'tibble', 'data.table')) & inherits(obj$links, c('data.frame', 'tibble', 'data.table'))){
#     cfg = list(point.size = 10, minLinkWidth = 1, maxLinkWidth = 5, shinyInput.click = onClickShinyInput,
#                palette = list(color = c('white', node_colors), linkColor = c('gray', edge_colors)),
#                direction = direction)
#     cat('\n', 'Started plotting the map ...')
#     net = list(nodes = obj$nodes %>% as.data.frame %>% column2Rownames('from_id'), links = obj$links) %>%
#       niraPlot(label = 'label', shape = 'shape', color = 'weight', tooltip = 'tooltip',
#                source = 'from_id', target = 'to_id', linkLabel = 'weight', linkColor = list(edgeColor = 'weight'),
#                linkWidth = list(edgeWidth = 'weight'),
#                plotter = plotter, type = 'graph', config = cfg)
#
#     cat('Done!', '\n')
#     return(net)
#   }
#   else {
#     warnif(T, "from function plot.process(): Graph tables are not built! Please add using function 'addGraphTables()' (No graph generated).")
#   }
#
#
#   # if(plotter == 'DiagrammeR'){
#   #   obj %<>% addDiagrammeRGraph
#   #   if(is.null(obj$DiagrammeRGraph)){
#   #     warnif(T, "from function plot.process(): Function addDiagrammeRGraph() did not generate any DiagrammeR graph (NULL returned).")
#   #   } else {
#   #     return(obj$DiagrammeRGraph %>% renderDiagrammeRGraph)
#   #     # return(obj$DiagrammeRGraph %>% render_graph)
#   #     # return(obj$DiagrammeRGraph)
#   #   }
#   # } else if (plotter == 'visNetwork'){
#   #   nodes = obj$nodes
#   #   edges = obj$links
#   #
#   #   nodes %<>%
#   #     mutate(weight = scales::rescale(weight, from = c(0, max(weight)))) %>%
#   #     mutate(weight = ifelse(skill == 'End', Inf, weight))
#   #
#   #
#   #   net = list(nodes = obj$nodes %>% as.data.frame %>% column2Rownames('from_id'), links = obj$links) %>%
#   #     niraPlot(label = 'label', shape = 'shape', color = 'weight', tooltip = 'tooltip', size = 50,
#   #              source = 'from_id', target = 'to_id', linkLabel = 'weight', linkColor = 'weight',
#   #              config = list(point.size = 10, palette = list(color = c('white', node_colors), linkColor = c('white', edge_colors)), direction = direction),
#   #              plotter = 'visNetwork', type = 'graph')
#   #
#   #   return(net %>% visHierarchicalLayout(sortMethod = 'directed', levelSeparation = 200, edgeMinimization = T, blockShifting = T, parentCentralization = T, direction = 'UD') %>% visNodes(physics = F) %>% visEdges(smooth = T))
#   # }
#
#   # tbls = obj %>% graphTables %>%
#   #   niraPlot(label = 'label', shape = 'shape', color = 'color', labelColor = 'fontcolor', tooltip = 'tooltip',
#   #            linkSource = 'from_id', linkTarget = 'to_id', linkWidth = 'penwidth',
#   #            config = list(point.border.width = 1.5, label.font = 'Arial', node.style = "rounded,filled",
#   #                          link.color = ifelse(perspective_edges == "performance", "red4", "dodgerblue4"),
#   #                          linkLabel.font = "Arial"))
# }
#
# plot.caseIdleTime = function(obj, caseIDs = NULL, unit = "hours", nbars = NULL, descending = T,
#                              plotter = 'plotly', type = 'bar', horizontal = T,
#                              title = 'Total case process idle time' %>% paste0(' (', unit, ')'), config = NULL, ...){
#   if(plotter == 'graphics'){obj$bupaobj %>% idle_time("case", units = unit) %>% plot}
#   else {
#     if (is.null(cases)){cases = unique(obj$cases)}
#     obj$bupaobj %>% idle_time("case", units = unit) %>% filter(caseID %in% cases) -> IDL
#     if(descending){IDL %<>% arrange(desc(idle_time))} else {IDL %<>% arrange(idle_time)}
#     nbars %<>% verify(c('numeric', 'integer'), domain = c(0, nrow(IDL)), default = nrow(IDL), fix = T)
#     IDL = IDL[sequence(nbars), ]
#     cfg = config %<==>% list(title = title, xAxis.label = unit, yAxis.label = 'Case ID',
#                              yAxis.margin.left = 10*(IDL$caseID %>% nchar %>% max(na.rm = T)))
#     IDL %>% niraPlot(
#       x = chif(horizontal, 'idle_time', 'caseID'),
#       y = chif(horizontal, 'caseID', 'idle_time'),
#       type = type, plotter = plotter, config = cfg, ...)
#   }
# }
#
# plot.skillProcTime = function(obj, measure = 'Average', skills = NULL, unit = 'hours', nbars = NULL, descending = T,
#                               plotter = 'plotly', type = 'bar', horizontal = T,
#                               title = measure %>% paste('skill processing time') %>% paste0(' (', unit, ')'), config = NULL, ...){
#
#   measures = c(Minimum = 'min', 'First quartile' = 'q1', Average = 'mean', Mean = 'mean', Median = 'median', 'Second quartile' = 'median', 'Third quartile' = 'q3', Maximum = 'max', 'Standard deviation' = 'st_dev', 'Total' = 'total')
#   if (is.null(skills)){skills = unique(obj$skills)}
#   obj$bupaobj %>% processing_time("activity", units = unit) %>% filter(taskType %in% skills) %>% as.data.frame -> SPT
#
#   ord = SPT[,measures[measure]] %>% as.numeric %>% order(decreasing = descending)
#   nr = nrow(SPT)
#   nbars %<>% verify(c('numeric', 'integer'), domain = c(0, nr), default = nr, fix = T)
#   SPT = SPT[ord[nbars %>% sequence],]
#   SPT$taskType %<>% as.character
#
#   cfg = config %<==>% list(title = title, xAxis.label = unit, yAxis.label = 'Skill (Task Type)',
#                            yAxis.margin.left = 10*(SPT$taskType %>% nchar %>% max(na.rm = T)))
#
#   SPT %>% niraPlot(
#     x = chif(horizontal, measures[measure], 'taskType'),
#     y = chif(horizontal, 'taskType', measures[measure]),
#     type = type, plotter = plotter, config = cfg, ...)
# }


# prosim.R -----------------------------------------------

# Header
# Filename:      prosim.R
# Description:   This module provides functions for simulating the process including queue simulation and transition simulations
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@gmail.com
# Start Date:    09 March 2018
# Last Revision: 21 June 2018
# Version:       0.0.5

# Version   Date               Action
# -----------------------------------
# 0.0.1     09 March 2018      Initial issue with function simQueue()
# 0.0.2     27 April 2018      Function simQueue() modified: Text progress bar added with argument show_process
# 0.0.3     27 April 2018      Function simQueue() modified: Generates precalculated arrays for process times and wrap times
# 0.0.4     02 May 2018        Function generateTasks() added
# 0.0.5     21 June 2018       Function generateTasks() modified: skips skills(task types) if they are empty



# This is a single queue-multi agent queue simulator:
#' @export
simQueue = function(taskEventLog, callEventLog, from, until, agents, settings, show_progress = T, show_details = F){
  # todo: add argument and setting verifications and defaults

  addCall = function(calls, id, time, task, agent, skill){
    calls[id, 'callID']   = id
    calls[id, 'callTime'] = time
    calls[id, 'taskID']   = task
    calls[id, 'agent']    = agent
    calls[id, 'skill']    = skill
    return(calls)
  }

  TL <- taskEventLog %>% nameColumns(
    columns = list(taskID = settings$taskEventLog$taskID_col, skill = settings$taskEventLog$skill_col, agent = settings$taskEventLog$agent_col, priority = settings$taskEventLog$priority_col, arrTime = settings$taskEventLog$arrivalTime_col, startTime = settings$taskEventLog$startTime_col, compTime = settings$taskEventLog$completedTime_col),
    classes = list(taskID = 'character', skill = 'character', agent = 'character', priority = 'numeric', arrTime = 'POSIXct', startTime = 'POSIXct', compTime = 'POSIXct')) %>% column2Rownames('taskID', remove = F)

  GNC <- callEventLog %>% nameColumns(
    columns = list(callID = settings$callEventLog$callID_col, agent = settings$callEventLog$agent_col, taskID = settings$callEventLog$taskID_col, skill = settings$callEventLog$skill_col, callTime = settings$callEventLog$callTime_col, compTime = settings$callEventLog$completedTime_col),
    classes = list(callID = 'character', agent = 'character', taskID = 'character', skill = 'character', callTime = 'POSIXct', compTime = 'POSIXct')) %>% filter(agent %in% agents) %>% column2Rownames('callID', remove = F)
  # Assumptions:
  # 1- All agents are available and productive 100% of the simulation time. todo: scheduled time can be given as an input table
  # 2- The simulator does not generate task arrivals, so does not require arrival rate! All task arrivals should be given in the input table taskEventLog

  agents = rownames(settings$agentSkillAPT) %^% rownames(settings$agentSkillAWT)
  skills = colnames(settings$agentSkillAPT) %^% colnames(settings$agentSkillAWT)

  settings$agentSkillAPT = settings$agentSkillAPT[agents, skills, drop = F]
  settings$agentSkillAWT = settings$agentSkillAWT[agents, skills, drop = F]

  skills = skills[which(colSums(settings$agentSkillAPT, na.rm = T) > 0) %^% which(colSums(settings$agentSkillAWT, na.rm = T) > 0)]

  settings$agentSkillAPT = settings$agentSkillAPT[agents, skills, drop = F]
  settings$agentSkillAWT = settings$agentSkillAWT[agents, skills, drop = F]

  settings$agentSkillAPT[settings$agentSkillAPT < settings$minAPT] <- settings$minAPT
  settings$agentSkillAWT[settings$agentSkillAWT < settings$minAWT] <- settings$minAWT

  procRate = 1.0/(settings$agentSkillAPT*60)
  wrapRate = 1.0/(settings$agentSkillAWT*60)

  # Agent Aaverage Wrap Rate:
  AAWR = wrapRate %>% rowMeans(na.rm = T)


  Na = length(agents)
  Ns = length(skills)
  N  = Na*Ns*1000

  fpt = numeric(N) %>% array(dim = c(Na, Ns, 1000), dimnames = list(agents, skills, 1:1000))
  fwt = fpt
  awt = numeric(Na*1000) %>% array(dim = c(Na, 1000), dimnames = list(agents, 1:1000))

  for(i in agents){
    for(j in skills[which(!is.na(procRate[i, ]))]){
      fpt[i,j, ] <- rexp(1000, rate = procRate[i, j])
      fwt[i,j, ] <- rexp(1000, rate = wrapRate[i, j])
    }
    awt[i,] <- rexp(1000, rate = AAWR[i])
  }
  smpl = sample(1:1000, 10000, replace = T)

  simend   = verify(until, 'POSIXct', lengths = 1, null_allowed = F)
  # now      = min(TL$arrTime[is.na(TL$startTime)]) could serve as default value for argument 'from'

  cntr = 1
  # If there is no call event triggered, each agent must trigger one:
  for(e in agents){
    if(GNC %>% filter(agent == e) %>% filter(is.na(taskID)) %>% nrow < 1){
      nextCallTime = from + awt[e, smpl[cntr]]
      cntr = cntr + 1
      if(nextCallTime < simend){GNC %<>% addCall(id = e %>% paste(1, sep = '.'), time = nextCallTime, agent = e, skill = NA, task = NA)}
    }
  }

  if(show_progress){pb = txtProgressBar(min = as.numeric(from), max = as.numeric(simend), style = 3)}
  while((GNC$taskID %>% is.na %>% sum) > 0){
    if(cntr > 9990){smpl = sample(1:1000, 10000, replace = T);cntr = 1}

    cl = GNC[which(is.na(GNC$taskID)),] %>% filter(callTime == min(callTime))
    for (i in cl$callID){
      e = GNC[i, 'agent']
      if(show_progress){setTxtProgressBar(pb, as.numeric(GNC[i, 'callTime']))}
      if(show_details){
        cat('Agent: ', e, ' Called at: ', as.character(GNC[i, 'callTime']), ',')
      }
      # What skills does this agent have?
      skls = colnames(procTime)[which(!is.na(procTime[e, ]))]
      tl = TL %>% filter(is.na(agent)) %>% filter(arrTime < GNC[i, 'callTime']) %>% filter(skill %in% skls) %>% filter(arrTime == min(arrTime))
      if(nrow(tl) > 0){
        TL[tl$taskID[1], 'agent'] <- e
        GNC[i, 'taskID'] <- tl[1, 'taskID']
        GNC[i, 'skill']  <- tl[1, 'skill']
        GNC[i, 'compTime'] <- GNC[i, 'callTime'] + fpt[e, tl[1, 'skill'], smpl[cntr]]

        if(show_details){
          cat(' Completed at: ', as.character(GNC[i, 'compTime']), ',')
        }

        cntr = cntr + 1;
        TL[tl$taskID[1], 'startTime'] <- GNC[i, 'callTime']
        TL[tl$taskID[1], 'compTime']  <- GNC[i, 'compTime']
        # Add a row to the getNext call eventlog:
        nextCallTime = TL[tl$taskID[1], 'compTime'] + fwt[e, tl[1, 'skill'], smpl[cntr]]

        if(show_details){
          cat(' Next Call at: ', as.character(nextCallTime), '\n')
        }

        cntr = cntr + 1;

        if(nextCallTime < simend){
          nCalls    = sum(GNC$agent == e & !is.na(GNC$taskID))
          GNC %<>% addCall(id = e %>% paste(nCalls + 1, sep = '.'), agent = e, time = nextCallTime, task = NA, skill = NA)
        }
      } else {
        # If by the time an agent calls getNext and no unallocated task matching his/her skills are found, there are two possible scenarios:
        # 1: call getNext again after a randomly generated time with rate wrapRate. A new row is added with skill = 'idle' for the unsuccessful getNext call
        # 2: the next task matching skills arriving will be immidiately allocated to the agent when arrived
        # Currently we model option 1
        nIdleCalls    = sum(GNC$agent == e & GNC$skill == 'idle', na.rm = T)
        GNC[i, 'taskID'] <- e %>% paste('idle', nIdleCalls + 1, sep = '.')
        GNC[i, 'skill']  <- 'idle'
        nextCallTime = GNC[i, 'callTime'] + awt[e, smpl[cntr]]
        cntr = cntr + 1

        if(show_details){
          cat(' No tasks found! Next Call at: ', as.character(nextCallTime), '\n')
        }

        GNC[i, 'compTime'] <- nextCallTime

        if(nextCallTime < simend){
          nCalls    = sum(GNC$agent == e & !is.na(GNC$taskID))
          GNC %<>% addCall(id = e %>% paste(nCalls + 1, sep = '.'), agent = e, time = nextCallTime, task = NA, skill = NA)
        }
      }
    }
  }

  if(show_progress){setTxtProgressBar(pb, as.numeric(simend));close(pb)}

  output = list(taskEventLog = TL %>% arrange(arrTime), callEventLog = GNC %>% arrange(callTime))

  return(output)
}



# This function simulates arrival of tasks and generates a tasklist
# from: start time (Date)
# to: end time  (Date)
# rate: arrival rate in (tasks per hour) must be a named numeric vector where names specify task types or skills
#' @export
generateTasks = function(tasklist, from, to, rate){
  maxcount = as.integer(2*difftime(to, from, units = 'hour')*rate)
  skills   = names(rate)
  names(maxcount) <- skills

  for (sk in skills){
    if(!is.empty(sk)){
      TL = data.frame(intrArrival = rexp(n = maxcount[sk], rate = rate[sk]/3600.0), taskID = sk %>% paste(1:maxcount[sk], sep = '.'), skill = sk, queue = NA, agent = NA, priority = NA, startTime = as.POSIXct(NA), compTime = as.POSIXct(NA), stringsAsFactors = F) %>% mutate(arrTime = from + cumulative(intrArrival)) %>% filter(arrTime < to)
      tasklist %<>% rbind(TL)
    }
  }
  return(tasklist)
}


# transys.R --------------------------------------------------------------------

# Header
# Description:   This module introduces a reference class named as TRANSYS which only keeps the status changes.
#                Each case can have only one status at a time.
#                This model can also be used for processes which do not have concurrent activities.
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    08 November 2016
# Last Revision: 06 November 2018
# Version:       0.7.3

# Version   Date               Action
# -----------------------------------
# 0.0.1     08 November 2016   Initial issue
# 0.1.0     09 November 2016   Class TS.CASE modified: All methods defined within the class
# 0.2.0     15 November 2016   Queries added to SIMPLE.PROCESS objects. case_IDs should be first set as a query, then call property methods to get various process properties
# 0.2.1     15 November 2016   Method getTimeAdjacency() added to class TS.CASE
# 0.3.0     15 November 2016   Major changes in class SIMPLE.PROCESS: Property 'data' renamed to 'history', properties adjacency, visNetwork, igraph and all other process metrics moved to list property 'data'
# 0.3.1     15 November 2016   Method getTimeAdjacency() added to class SIMPLE.PROCESS
# 0.4.0     18 November 2016   Thanks to dplyr, all the sorting, next status duration is computed in the first feed. Methods feed(), fillCases() modified!
# 0.4.1     08 February 2017   SIMPLE.PROCESS renamed to TRANSYS. TS.CASE renamed to TS.CASE
# 0.4.2     08 February 2017   Filename changed to transys.R
# 0.4.3     16 February 2017   Method getAdjacency() and getTimeAdjacency() removed from class TRANSYS and replaced by method getAdjacencies()
# 0.4.4     16 February 2017   Method getStatusVolume() added. Returns an object of class TIME.SERIES
# 0.5.0     06 September 2017  Method feedStatusHistory() modified: Argument add_ends added! If TRUE, statuses START and END will be added to the beginning and end of processes!
# 0.5.1     11 September 2017  Method feedStatusHistory() modified: Columns paths and selected added
# 0.5.2     14 September 2017  Property query removed.
# 0.5.3     14 September 2017  Function computeFullAdjacencies() added
# 0.5.4     14 September 2017  Functions getOutlierCases() and applyCaseFilter() added
# 0.5.5     14 September 2017  Function getAdjacencies modified.
# 0.5.6     14 September 2017  Function getSimpleAdjacencies added.
# 0.6.0     30 June 2018       Fundamental changes: All methods removed and renamed.
# 0.6.1     02 July 2018       Function feedStatusHistory() modified
# 0.6.3     03 July 2018       Function get.volumeIn(), get.volumeOut() and get.backlog() modified. Method compute.volumes.daily() removed
# 0.6.6     14 September 2018  Functions get.volumeIn(), get.volumeOut() and get.backlog() now can give with hourly timeseries.
# 0.6.7     12 October 2018    All plotting functions transferred to tsvis.R
# 0.7.0     23 October 2018    Properties 'modelStart' and 'modelEnd' added to class TRANSYS, functions get.volumeIn() & get.volumeOut() modified accordingly.
# 0.7.2     26 October 2018    Method filter.cases() modified: two filtering arguments added: startStatuses, endStatuses
# 0.7.3     06 November 2018   Generic function summary.TRANSYS() modified: collapses multiple statuses in the summary string


# Good information for working with R functions:
# http://adv-r.had.co.nz/Functions.html

# fast Data Manipulation in R:
# https://www.analyticsvidhya.com/blog/2015/12/faster-data-manipulation-7-packages/
# fast Table Reshape:
# http://seananderson.ca/2013/10/19/reshape.html
# http://stackoverflow.com/questions/26536251/comparing-gather-tidyr-to-melt-reshape2


# Required Libraries:
library(dplyr)

empt = Sys.time()[-1]
empd = Sys.Date()[-1]

# This global variable is repeated in prom, can transfer to niragen
#' @export
timeUnitCoeff = c(hour = 3600, second = 1, minute = 60, day = 24*3600, week = 7*24*3600, year = 24*3600*365)


#' Reference class for modelling Transition Systems
#'
#' TRANSYS is a reference class containing some properties and methods required for analysing, modelling and visualising a Transition Sytem.
#' A Transistopn System is a Markov Chain model descibing a system which can change status over time.
#'
#' @field history data.frame holding history data of status transitions
#'
#' @export TRANSYS
TRANSYS = setRefClass('TRANSYS',
                      fields = list(
                        modelStart    = 'POSIXct',
                        modelEnd      = 'POSIXct',
                        history       = 'data.frame',
                        nodes         = 'data.frame',
                        links         = 'data.frame',
                        nodes.full    = 'data.frame',
                        links.full    = 'data.frame',
                        settings      = 'list',
                        cases         = 'list',
                        timeseries    = 'list',
                        tables        = 'list',
                        metrics       = 'list',
                        plots         = 'list',
                        timeseries.full  = 'list',
                        caseIDs       = 'character',
                        statuses      = 'character',
                        caseIDs.full  = 'character',
                        statuses.full = 'character'
                      ),

                      methods = list(

                        clear = function(){
                          nodes      <<- data.frame()
                          links      <<- data.frame()
                          statuses   <<- character()
                          tables     <<- list()
                          plots      <<- list()
                          timeseries <<- list()
                          metrics    <<- list()
                        },

                        initialize = function(start = NULL, end = NULL, ...){
                          callSuper(...)
                          settings$include_case_measures <<- F
                          settings$filter <<- list(complete = NULL, minLoops = NULL, maxLoops = NULL, IDs = NULL, startStatuses = NULL, endStatuses = NULL, freqThreshold = NULL)

                          history <<- data.frame(caseID = character(), status = character(), nextStatus = character(), startTime = empt, endTime = empt, caseStart = logical(), caseEnd = logical(),
                                                 selected = logical(), startDate = empd, endDate = empd, creation = empt, duration = numeric(), eventAge = numeric(), path = character(),
                                                 stringsAsFactors = F)
                          cases$profile <<- data.frame(firstDtatus = character(), lastStatus = character(), startTime = empt, endTime = empt, caseStart = logical(), caseEnd = logical(), duration = numeric(),
                                                       stringsAsFactors = F)

                          modelStart <<- start %>% as.time
                          modelEnd   <<- end %>% as.time
                        },

                        # remove_sst: remove same status transitions
                        # caseStartFlag_col
                        feedStatusHistory = function(dataset, caseID_col = 'caseID', status_col = 'status', startTime_col = 'startTime', caseStartFlag_col = NULL, caseEndFlag_col = NULL, caseStartTag = NULL, caseEndTag = NULL, sort_startTime = T, add_start = T, remove_sst = F){
                          # verifications
                          dataset %<>%
                            nameColumns(columns = list(caseID = caseID_col , status = status_col , startTime = startTime_col, caseStart = caseStartFlag_col, caseEnd = caseEndFlag_col),
                                        classes = list(caseID = 'character', status = 'character', startTime = 'POSIXct', caseStart = 'logical', caseEnd = 'logical')) %>%
                            mutate(selected = T)

                          # dataset$status = gsub("[[:space:]]", "", as.character(dataset$status))

                          tbd = is.na(dataset$status) |
                            is.na(dataset$caseID) |
                            is.na(dataset$startTime) |
                            (dataset$status == "") |
                            (dataset$caseID == "") |
                            is.na(dataset$caseStart) |
                            is.na(dataset$caseEnd)

                          tbd = which(tbd)
                          if(length(tbd) > 0){
                            warnif(T, length(tbd) %++% ' events removed due to missing values in one of these critical columns: caseID, status, startTime, caseStart or caseEnd!')
                            dataset <- dataset[- tbd, ]
                          }

                          support('dplyr')
                          dataset %<>% dplyr::mutate(nextStatus = status, endTime = startTime) %>%
                            # dplyr::distinct(caseID, startTime, .keep_all = TRUE) %>% # remove duplicated combination of caseID and startTime
                            dplyr::arrange(caseID, startTime) %>% column.shift.up(c('nextStatus', 'endTime'), keep.rows = T)

                          # Removing cases with no transition (all cases who remained in their first status until the end of the dataset are removed!)
                          dp = which(!duplicated(dataset$caseID))

                          if(is.null(dataset$caseStart)){
                            if(is.null(caseStartTag)){dataset$caseStart = F} else {
                              startedcases = dataset$caseID[dataset$status %in% caseStartTag] %>% unique
                              dataset$caseEnd = dataset$caseID %in% startedcases}}
                          if(is.null(dataset$caseEnd)){
                            if(is.null(caseEndTag)){dataset$caseEnd = F} else {
                              endedcases = dataset$caseID[dataset$status %in% caseEndTag] %>% unique
                              dataset$caseEnd = dataset$caseID %in% endedcases}}

                          dataset %<>% select(caseID, status, nextStatus, startTime, endTime, caseStart, caseEnd, selected)

                          endindx = c(dp[-1] - 1, nrow(dataset))
                          dataset[endindx, 'nextStatus'] = ifelse(dataset[endindx, 'caseEnd'], 'END', 'EXIT')
                          dataset[endindx, 'endTime'] = dataset[endindx, 'startTime'] + 0.1

                          if(add_start){
                            rb           = dataset[dp,]
                            rb$endTime   = rb$startTime
                            rb$startTime = rb$startTime - 0.1
                            rb$nextStatus = rb$status
                            rb$status     = ifelse(rb$caseStart, 'START', 'ENTER')

                            dataset %<>% rbind(rb) %>% dplyr::arrange(caseID, startTime)
                            dp = which(!duplicated(dataset$caseID))
                            endindx = c(dp[-1] - 1, nrow(dataset))
                          }

                          dataset[dp, c('caseID', 'startTime', 'status', 'caseStart', 'caseEnd')] %>%
                            inner_join(dataset[endindx, c('caseID', 'endTime', 'nextStatus')], by = 'caseID') %>%
                            dplyr::select(caseID, firstStatus = status, lastStatus = nextStatus, startTime, endTime, caseStart, caseEnd) %>%
                            dplyr::mutate(duration = as.numeric(endTime - startTime)) %>%
                            as.data.frame %>% column2Rownames('caseID') ->> cases$profile

                          if(remove_sst){dataset = dataset[dataset$status != dataset$nextStatus, ]}

                          dataset %<>%
                            mutate(startDate = startTime %>% as.Date(tz = attr(startTime, "tzone")),
                                   endDate   = endTime   %>% as.Date(tz = attr(endTime, "tzone")))

                          caseIDs.full  <<- rownames(cases$profile)
                          statuses.full <<- as.character(dataset$status %U% dataset$nextStatus)
                          caseIDs       <<- caseIDs.full
                          statuses      <<- statuses.full

                          cs = as.character(dataset$caseID)

                          dataset$creation   <- cases$profile[cs, 'startTime']
                          dataset$duration   <- as.numeric(dataset$endTime - dataset$startTime)
                          dataset$eventAge   <- as.numeric(dataset$startTime - dataset$creation)

                          dataset$selected = TRUE
                          dataset$path     = dataset$status %++% '-' %++% dataset$nextStatus

                          if(is.empty(modelStart)){modelStart <<- min(dataset$startTime)}
                          if(is.empty(modelEnd)){modelEnd <<- max(dataset$endTime)}

                          history <<- dataset

                          filter.cases(complete = settings$filter$complete, minLoops = settings$filter$minLoops, maxLoops = settings$filter$maxLoops, IDs = settings$filter$IDs, startStatuses = settings$filter$startStatuses, endStatuses = settings$filter$endStatuses, freqThreshold = settings$filter$freqThreshold)
                        },

                        # tables in cases are always full and do not depend on the filtering
                        get.cases.path = function(){
                          if(is.empty(cases$path)){
                            cat('\n', 'Aggregating cases to find paths ...')
                            history %>% dplyr::group_by(caseID) %>%
                              dplyr::summarise(path = paste(status, collapse = '-') %>% paste(last(nextStatus), sep = '-'), startStatus = status[2], endStatus = last(status), transCount = length(status), loops = sum(duplicated(status), na.rm = T)) %>%
                              # dplyr::summarise(minTime = min(startTime), maxTime = max(endTime), startFlag = caseStart[1], endFlag = caseEnd[1], ) %>%
                              # dplyr::mutate(duration2 = as.numeric(maxTime - minTime)) %>%
                              dplyr::ungroup() %>% as.data.frame %>% column2Rownames('caseID') ->> cases$path

                            cids = rownames(cases$path) %^% rownames(cases$profile)
                            cases$path[cids, 'duration']  <<- cases$profile[cids, 'duration']
                            cases$path[cids, 'completed'] <<- cases$profile[cids, 'caseStart'] & cases$profile[cids, 'caseEnd']
                            cat('Done!', '\n')
                          }
                          return(cases$path)
                        },

                        # returns the Case Status Time (CST) matrix containing amount of time each case spent on each status. NA means the case never met the status
                        get.cases.statusTime = function(){
                          if(is.null(cases$statusTime)){
                            cat('\n', 'Aggregating cases to find status durations ...')
                            if(nrow(history) == 0){
                              cases$statusTime <<- data.frame()
                            } else if (is.null(cases$statusTime)){
                              history %>% dplyr::group_by(caseID, status) %>%
                                dplyr::summarise(duration = sum(duration, na.rm = T)) %>%
                                reshape2::dcast(caseID ~ status, value.var = 'duration') %>% column2Rownames('caseID') ->> cases$statusTime
                            }
                            cat('Done!', '\n')
                          }
                          return(cases$statusTime)
                        },

                        get.cases.statusFreq = function(){
                          if(nrow(history) == 0){
                            cases$statusFreq <<- data.frame()
                          } else if(is.null(cases$statusFreq)){
                            history %>% dplyr::group_by(caseID, status) %>%
                              dplyr::summarise(freq = length(duration)) %>%
                              reshape2::dcast(caseID ~ status, value.var = 'freq') %>% na2zero %>% column2Rownames('caseID') ->> cases$statusFreq
                          }
                          return(cases$statusFreq)
                        },

                        get.volumeIn = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily  = {
                                   if(full){null = is.null(timeseries.full$volin.daily)} else {null = is.null(timeseries$volin.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volin = new('TS.DAILY')} else {
                                       B.entr = hist %>% dplyr::group_by(startDate, status) %>% dplyr::summarise(length(caseID))

                                       names(B.entr) <- c('Date', 'Status', 'Entry')

                                       B.entr %<>% reshape2::dcast(Date ~ Status, sum, value.var = 'Entry')

                                       modelStartDate <- as.Date(modelStart %>% setTZ('GMT'))
                                       modelEndDate   <- as.Date(modelEnd %>% setTZ('GMT'))

                                       volin <- new('TS.DAILY', from = modelStartDate, until = modelEndDate)
                                       volin$feedData(B.entr, date_col = 'Date')
                                       volin$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volin.daily <<- volin
                                     else      timeseries$volin.daily     <<- volin
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volin.daily)} else {return(timeseries$volin.daily)}}
                                   else             {if(full){return(timeseries.full$volin.daily$data)} else {return(timeseries$volin.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$volin.hourly)} else {null = is.null(timeseries$volin.hourly)}
                                   if(null){
                                     # compute.volumes.hourly(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volin = new('TS.HOURLY')} else {
                                       B.entr = hist %>% mutate(startHour = cut(startTime, breaks = 'hour')) %>%
                                         dplyr::group_by(startHour, status) %>% dplyr::summarise(length(caseID)) %>% dplyr::ungroup()

                                       names(B.entr) <- c('Hour', 'Status', 'Entry')

                                       B.entr %<>% reshape2::dcast(Hour ~ Status, sum, value.var = 'Entry')

                                       B.entr %<>% dplyr::mutate(Hour = as.POSIXct(Hour))

                                       volin <- new('TS.HOURLY', from = modelStart, until = modelEnd)
                                       volin$feedData(B.entr, hour_col = 'Hour')
                                       volin$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volin.hourly <<- volin
                                     else      timeseries$volin.hourly     <<- volin
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volin.hourly)} else {return(timeseries$volin.hourly)}}
                                   else             {if(full){return(timeseries.full$volin.hourly$data)} else {return(timeseries$volin.hourly$data)}}
                                 })
                        },

                        get.volumeOut = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(timeseries.full$volout.daily)} else {null = is.null(timeseries$volout.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volout <- new('TS.DAILY')} else {
                                       B.exit = hist %>% dplyr::group_by(endDate, status)   %>% dplyr::summarise(length(caseID))

                                       names(B.exit) <- c('Date', 'Status', 'Exit')

                                       B.exit %<>% reshape2::dcast(Date ~ Status, sum, value.var = 'Exit')

                                       modelStartDate <- as.Date(modelStart %>% setTZ('GMT'))
                                       modelEndDate   <- as.Date(modelEnd %>% setTZ('GMT'))

                                       volout <- new('TS.DAILY', from = modelStartDate, until = modelEndDate)
                                       volout$feedData(B.exit, date_col = 'Date')
                                       volout$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volout.daily <<- volout
                                     else      timeseries$volout.daily     <<- volout
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volout.daily)} else {return(timeseries$volout.daily)}}
                                   else             {if(full){return(timeseries.full$volout.daily$data)} else {return(timeseries$volout.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$volout.hourly)} else {null = is.null(timeseries$volout.hourly)}
                                   if(null){
                                     # compute.volumes.hourly(full = full)
                                     if(full){hist = history} else {hist = history %>% filter(selected)}
                                     if(is.empty(hist)){volout <- new('TS.HOURLY')} else {
                                       B.exit = hist %>% dplyr::mutate(endHour = cut(endTime, breaks = 'hour')) %>%
                                         dplyr::group_by(endHour, status) %>% dplyr::summarise(length(caseID)) %>% dplyr::ungroup()

                                       names(B.exit) <- c('Hour', 'Status', 'Exit')

                                       B.exit %<>% reshape2::dcast(Hour ~ Status, sum, value.var = 'Exit')
                                       B.exit %<>% dplyr::mutate(Hour = as.POSIXct(Hour))

                                       volout <- new('TS.HOURLY', from = modelStart, until = modelEnd)
                                       volout$feedData(B.exit, hour_col = 'Hour')
                                       volout$data %<>% na2zero
                                     }

                                     if(full) timeseries.full$volout.hourly <<- volout
                                     else      timeseries$volout.hourly     <<- volout
                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$volout.hourly)} else {return(timeseries$volout.hourly)}}
                                   else             {if(full){return(timeseries.full$volout.hourly$data)} else {return(timeseries$volout.hourly$data)}}
                                 })
                        },

                        get.backlog = function(period = c('daily', 'weekly', 'hourly', 'monthly', 'annual'), full = F, as_timeseries = F){
                          period = match.arg(period)
                          assert(period %in% c('daily', 'hourly')) # todo: write for other periods
                          switch(period,
                                 daily = {
                                   if(full){null = is.null(timeseries.full$backlog.daily)} else {null = is.null(timeseries$backlog.daily)}
                                   if(null){
                                     # compute.volumes.daily(full = full)

                                     volout = get.volumeOut(full = full) %>% as.data.frame %>% column2Rownames('date')
                                     volin  = get.volumeIn(full = full) %>% as.data.frame %>% column2Rownames('date')

                                     dt = rownames(volout) %U% rownames(volin)
                                     st = colnames(volout) %^% colnames(volin)

                                     BL = (volin[dt, st] %>% as.matrix) - (volout[dt, st] %>% na2zero %>% as.matrix)

                                     BL <- BL %>% cumulative

                                     # assert((sum(BL[nrow(BL),]) == 0) & (min(BL) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])

                                     BL %<>% as.data.frame %>% rownames2Column('Date')
                                     BL$Date %<>% as.Date
                                     firstDay <- min(BL$Date)
                                     lastDay  <- max(BL$Date)

                                     BL %<>% column.shift.up('Date', keep.rows = T)
                                     BL <- rbind(BL[1, ], BL)
                                     BL[1, -1] <- 0
                                     BL[1, 'Date'] <- firstDay
                                     BL[nrow(BL), 'Date'] <- lastDay + 1

                                     backlog <- new('TS.DAILY', from = min(BL$Date), until = max(BL$Date))
                                     backlog$feedData(BL, date_col = 'Date')

                                     if(full){
                                       timeseries.full$backlog.daily <<- backlog
                                     } else{
                                       timeseries$backlog.daily <<- backlog
                                     }

                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$backlog.daily)} else {return(timeseries$backlog.daily)}}
                                   else             {if(full){return(timeseries.full$backlog.daily$data)} else {return(timeseries$backlog.daily$data)}}
                                 },
                                 hourly = {
                                   if(full){null = is.null(timeseries.full$backlog.hourly)} else {null = is.null(timeseries$backlog.hourly)}
                                   if(null){
                                     volout = get.volumeOut(full = full, period = period) %>% as.data.frame %>% column2Rownames('time')
                                     volin  = get.volumeIn(full = full, period = period) %>% as.data.frame %>% column2Rownames('time')

                                     dt = rownames(volout) %U% rownames(volin)
                                     st = colnames(volout) %^% colnames(volin)

                                     BL = (volin[dt, st] %>% as.matrix) - (volout[dt, st] %>% na2zero %>% as.matrix)

                                     BL <- BL %>% cumulative

                                     # assert((sum(BL[nrow(BL),]) == 0) & (min(BL) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])

                                     BL %<>% as.data.frame %>% rownames2Column('Hour')
                                     BL$Hour %<>% as.POSIXct

                                     firstHour <- min(BL$Hour)
                                     lastHour  <- max(BL$Hour)

                                     BL %<>% column.shift.up('Hour', keep.rows = T)
                                     BL <- rbind(BL[1, ], BL)
                                     BL[1, -1] <- 0
                                     BL[1, 'Hour'] <- firstHour
                                     BL[nrow(BL), 'Hour'] <- lastHour + 3600

                                     backlog <- new('TS.HOURLY', from = min(BL$Hour), until = max(BL$Hour))
                                     backlog$feedData(BL, hour_col = 'Hour')

                                     if(full){
                                       timeseries.full$backlog.hourly <<- backlog
                                     } else{
                                       timeseries$backlog.hourly <<- backlog
                                     }

                                   }
                                   if(as_timeseries){if(full){return(timeseries.full$backlog.hourly)} else {return(timeseries$backlog.hourly)}}
                                   else             {if(full){return(timeseries.full$backlog.hourly$data)} else {return(timeseries$backlog.hourly$data)}}
                                 })
                        },

                        get.metric = function(measure = c('freq', 'totComp', 'avgComp', 'totTT', 'avgTT', 'medTT', 'sdTT', 'totLoops', 'avgLoops', 'medLoops',  'totTrans', 'avgTrans'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
                          nontime   = c('freq', 'totLoops', 'avgLoops', 'medLoops', 'totComp', 'avgComp', 'totTrans', 'avgTrans')
                          measure   = match.arg(measure)
                          time_unit = match.arg(time_unit)
                          k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])

                          if(is.null(metrics[[measure]])){
                            if(measure %in% c('totLoops', 'avgLoops', 'medLoops')){get.cases.path()}
                            switch(measure,
                                   'freq' = {metricval = length(get.caseIDs()) %>% as.integer},
                                   'totTT' = {metricval = cases$profile[get.caseIDs(), 'duration'] %>% sum(na.rm = T)},
                                   'avgTT' = {metricval = cases$profile[get.caseIDs(), 'duration'] %>% mean(na.rm = T)},
                                   'sdTT' = {metricval = cases$profile[get.caseIDs(), 'duration'] %>% sd(na.rm = T)},
                                   'medTT' = {metricval = cases$profile[get.caseIDs(), 'duration'] %>% median(na.rm = T)},
                                   'totLoops' = {metricval = cases$path[get.caseIDs(), 'loops'] %>% sum(na.rm = T) %>% as.integer},
                                   'avgLoops' = {metricval = cases$path[get.caseIDs(), 'loops'] %>% mean(na.rm = T)},
                                   'medLoops' = {metricval = cases$path[get.caseIDs(), 'loops'] %>% median(na.rm = T) %>% as.integer},
                                   'totTrans' = {metricval = cases$path[get.caseIDs(), 'transCount'] %>% sum(na.rm = T) %>% as.integer},
                                   'avgTrans' = {metricval = cases$path[get.caseIDs(), 'transCount'] %>% mean(na.rm = T)},
                                   'totComp' = {metricval = (cases$profile[get.caseIDs(), 'caseStart'] &  cases$profile[get.caseIDs(), 'caseEnd']) %>% sum(na.rm = T) %>% as.integer},
                                   'avgComp' = {metricval = (cases$profile[get.caseIDs(), 'caseStart'] &  cases$profile[get.caseIDs(), 'caseEnd']) %>% mean(na.rm = T)}
                            )
                            metrics[[measure]] <<- metricval
                          }
                          return(round(k*metrics[[measure]], digits = 2))
                          # add: loops, completion percentage, most frequent longest status,    distribution of cases over their longest status, distribution of total time over status
                        },

                        # valid.link.weights = c('totFreq', 'meanCaseFreq', 'medCaseFreq', 'sdCaseFreq', 'totalTime', ...)
                        # totalFreq:    Total transition frequency
                        # meanCaseFreq: average transition count per case
                        # medCaseFreq:  median case transition frequency (half of cases have transition frequency higher than this value)
                        # sdCaseFreq:   standard deviation of transition frequencies among cases
                        # totalTime:    Total transition time
                        # meanCaseTime: average transition time per case
                        # medCaseTime:  median case transition time (half of cases have transition time higher than this value)
                        # sdCaseTime:   standard deviation of transition time among cases
                        # meanTime:average transition time per transition
                        # medTime :median transition time (half of transitions have transition time higher than this value)
                        # sdTime  :standard deviation of transition time among transitions(half of transitions have transition time higher than this value)
                        # meanCaseEntryFreq: how many times in average a case has entered this status
                        # For nodes (statuses):
                        # totExitFreq: Total count of transitions from this status to other statuses
                        get.nodes = function(full = F){
                          if(full){
                            if(!is.empty(nodes.full)){return(nodes.full)} else {H = history}
                          } else {
                            if(!is.empty(nodes)){return(nodes)} else {H = history %>% dplyr::filter(selected)}
                          }

                          cat('\n', 'Aggregating nodes ...')

                          if(settings$include_case_measures){
                            A.node = H %>% dplyr::group_by(caseID, status) %>% dplyr::summarise(count = sum(selected, na.rm = T), duration = sum(duration, na.rm = T))
                            B.node = A.node %>% dplyr::group_by(status) %>%
                              dplyr::summarise(meanExitCaseFreq = mean(count, na.rm = T)   , medExitCaseFreq = median(count, na.rm = T)   , sdExitCaseFreq = sd(count, na.rm = T),
                                               meanCaseDuration = mean(duration, na.rm = T), medCaseDuration = median(duration, na.rm = T), sdCaseDuration = sd(duration, na.rm = T))
                          }

                          C.node = H %>% dplyr::group_by(status) %>%
                            dplyr::summarise(totalExitFreq = sum(selected, na.rm = T), totalDuration = sum(duration, na.rm = T),
                                             meanDuration = mean(duration, na.rm = T), medDuration = median(duration, na.rm = T), sdDuration = sd(duration, na.rm = T))

                          D.node = H %>% dplyr::group_by(nextStatus) %>% select(status = nextStatus, count = selected) %>%
                            dplyr::summarise(totalEntryFreq = sum(count, na.rm = T)) %>% full_join(C.node, by = 'status')

                          if(settings$include_case_measures){
                            D.node %<>% full_join(B.node, by = 'status')
                          }
                          nsel = sum(history$selected)
                          nrow = nrow(history)
                          if(full  | nsel == nrow){nodes.full <<- D.node}
                          if(!full | nsel == nrow){nodes <<- D.node}
                          if(is.empty(D.node)){
                            data.frame(status = character(), totalEntryFreq = numeric(), totalExitFreq = numeric(), totalDuration = numeric(), meanDuration = numeric(), medDuration = numeric(), sdDuration = numeric(), stringsAsFactors = F) %>% as_tibble -> D.node
                          }
                          cat('Done!', '\n')
                          return(D.node)
                        },

                        get.links = function(full = F){
                          if(full){
                            if(!is.empty(links.full)){return(links.full)} else {H = history}
                          } else {
                            if(!is.empty(links)){return(links)} else {H = history %>% dplyr::filter(selected)}
                          }

                          cat('\n', 'Aggregating links ...')

                          if(settings$include_case_measures){
                            A.edge = H %>% dplyr::group_by(caseID, status, nextStatus) %>% dplyr::summarise(count = sum(selected, na.rm = T), duration = sum(duration, na.rm = T))
                            B.edge = A.edge %>% dplyr::group_by(status, nextStatus) %>%
                              dplyr::summarise(meanCaseFreq = mean(count, na.rm = T)   , medCaseFreq = median(count, na.rm = T)   , sdCaseFreq = sd(count, na.rm = T),
                                               meanCaseTime = mean(duration, na.rm = T), medCaseTime = median(duration, na.rm = T), sdCaseTime = sd(duration, na.rm = T))
                          }

                          C.edge = H %>% dplyr::group_by(status, nextStatus) %>%
                            dplyr::summarise(totalFreq = sum(selected, na.rm = T), totalTime = sum(duration, na.rm = T), meanTime = mean(duration, na.rm = T), medTime = median(duration, na.rm = T), sdTime = sd(duration, na.rm = T))

                          if(settings$include_case_measures){
                            C.edge %<>% full_join(B.edge, by = c('status', 'nextStatus'))
                          }
                          if(is.empty(C.edge)){
                            data.frame(status = character(), nextStatus = character(), totalFreq = numeric(), totalTime = numeric(), meanTime = numeric(), medTime = numeric(), sdTime = numeric(), stringsAsFactors = F) %>% as_tibble -> C.edge
                          }

                          nsel = sum(history$selected)
                          nrow = nrow(history)

                          if(full  | nsel == nrow){links.full <<- C.edge}
                          if(!full | nsel == nrow){links <<- C.edge}
                          cat('Done!', '\n')
                          return(C.edge)
                        },

                        # Incomplete functions
                        get.status.metric = function(statusID, measure = c('freq', 'totTime', 'loopRate', 'caseRatio')){
                          measure = match.arg(measure)
                          switch(measure, 'freq' = {
                            CSF <- get.cases.statusFreq()
                            val = CSF[,statusID] %>% sum(na.rm = T) %>% as.integer
                          }, 'totTime' = {
                            CST <- get.cases.statusTime()
                            val = CST[,statusID] %>% sum(na.rm = T)
                          }, 'loopRate' = {
                            CSF <- get.cases.statusFreq()
                            entry = CSF[, statusID] %>% na.omit %>% zero.omit
                            val = sum(entry > 1)/length(entry)
                          }, 'caseRatio' = {
                            CSF <- get.cases.statusFreq()
                            entry = CSF[, statusID]
                            val = sum(entry > 0)/length(entry)
                          })
                          return(val %>% round(digits = 2))
                        },

                        get.status.volumes = function(statusID, period = c('daily', 'hourly'), as_timeseries = T){
                          period = match.arg(period)
                          listnm = statusID %>% paste('volumes', period, sep = '.')
                          if(is.null(timeseries[[listnm]])){
                            vin   = get.volumeIn(as_timeseries = F, period = period) %>% pull(statusID)
                            vinc  = vin %>% cumulative
                            vout  = get.volumeOut(as_timeseries = F, period = period) %>% pull(statusID)
                            voutc = vout %>% cumulative
                            back  = get.backlog(as_timeseries = F, period = period) %>% pull(statusID)
                            dt    = get.backlog(as_timeseries = F, period = period) %>% pull(chif(period == 'daily', 'date', 'time'))

                            vol   = data.frame(time = dt, volumeIn = c(vin, NA), volumeOut = c(vout, NA), volumeInCum = c(vinc, NA), volumeOutCum = c(voutc, NA), backlog = back)
                            switch(period, 'daily' = {
                              volts = new('TS.DAILY', from = min(dt), until = max(dt))
                              volts$feedData(vol, date_col = 'time')
                            }, 'hourly' = {
                              volts = new('TS.HOURLY', from = min(dt), until = max(dt))
                              volts$feedData(vol, hour_col = 'time')
                            })
                            timeseries[[listnm]] <<- volts
                          }
                          if(as_timeseries){return(timeseries[[listnm]])} else {return(timeseries[[listnm]]$data)}
                        },

                        get.case.metric   = function(caseID, measure = c('freq', 'time'), aggregator = c('sum', 'mean', 'median', 'sd')){},
                        get.trace.metric  = function(traceID, measure = c('freq', 'time'), aggregator = c('sum', 'mean', 'median', 'sd')){},

                        get.nodes.full = function(){},

                        get.links.full = function(){},

                        get.adjacency = function(measure = c('freq', 'time'), full = F, aggregator = c('sum', 'mean', 'median', 'sd')){
                          measure      = match.arg(measure)
                          aggregator   = match.arg(aggregator)
                          tabName      = 'adjacency' %>% paste(measure, aggregator, chif(full, 'full', ''), sep = '.')

                          if(is.null(tables[[tabName]])){
                            timevar = c(sum = 'totalTime', mean = 'meanTime', median = 'medTime', sd = 'sdTime')
                            edges   = get.links(full = full)
                            if(is.empty(edges)){return(data.frame())}
                            E = edges %>%
                              reshape2::dcast(status ~ nextStatus, value.var = chif(measure == 'freq', 'totalFreq', timevar[aggregator])) %>%
                              column2Rownames('status') %>% na2zero

                            for (i in get.statuses(full = full) %-% colnames(E)){E[, i] <- 0}
                            for (i in get.statuses(full = full) %-% rownames(E)){E[i, ] <- 0}

                            E[get.statuses(full = full), get.statuses(full = full)] ->> tables[[tabName]]
                          }
                          return(tables[[tabName]])
                        },

                        get.statuses = function(full = F){
                          if(full){
                            if(is.empty(statuses.full)){
                              statuses.full <<- unique(c(history$status, history$nextStatus))
                            }
                            return(statuses.full)
                          } else {
                            if(is.empty(statuses)){
                              statuses <<- history$status[history$selected] %U% history$nextStatus[history$selected]
                            }
                            return(statuses)
                          }
                        },

                        get.caseIDs = function(full = F){
                          if(full){
                            if(is.empty(caseIDs.full)){
                              caseIDs.full <<- unique(history$caseID)
                            }
                            return(caseIDs.full)
                          } else {
                            if(is.empty(caseIDs)){
                              caseIDs <<- unique(history[history$selected, 'caseID'])
                            }
                            return(caseIDs)
                          }
                        },

                        get.traces = function(){
                          if(is.null(tables$traces)){
                            casePath = get.cases.path()
                            cat('\n', 'Aggregating traces ...')
                            tables$traces <<- casePath[get.caseIDs(), ] %>% dplyr::group_by(path) %>%
                              dplyr::summarise(freq = length(path), totTime = sum(duration, na.rm = T), avgTime = mean(duration, na.rm = T), medTime = median(duration, na.rm = T), sdTime = sd(duration, na.rm = T), complete = completed[1]) %>%
                              dplyr::ungroup() %>% dplyr::arrange(freq) %>%
                              dplyr::mutate(variation = 'Variation ' %++% rev(sequence(nrow(.))))
                            cat('Done!', '\n')
                          }
                          return(tables$traces)
                        },

                        filter.reset = function(){
                          if(nrow(history) > 0){history$selected <<- T}
                          clear()
                          caseIDs    <<- caseIDs.full
                          statuses   <<- statuses.full
                          nodes      <<- nodes.full
                          links      <<- links.full
                          timeseries <<- timeseries.full
                          settings$filter <<- list()
                        },

                        filter.cases = function(complete = NULL, minLoops = NULL, maxLoops = NULL, statusDomain = NULL, startStatuses = NULL, endStatuses = NULL, IDs = NULL, freqThreshold = NULL, reset = T){
                          if(reset){filter.reset()}
                          if(nrow(history) > 0){
                            if(!is.null(complete %>% verify('logical'))){
                              chosen = cases$profile$caseStart & cases$profile$caseEnd
                              if(!complete){chosen = !chosen}
                              caseIDs <<- get.caseIDs() %^% rownames(cases$profile)[chosen]
                              history$selected <<- history$caseID %in% caseIDs
                              settings$filter$complete <<- complete
                            }

                            if(!is.null(minLoops) | !is.null(maxLoops)){
                              cp = get.cases.path()
                              if(is.null(minLoops)){minLoops = min(cp$loops, na.rm = T)}
                              if(is.null(maxLoops)){maxLoops = max(cp$loops, na.rm = T)}
                              chosen = cp$loops >= minLoops & cp$loops <= maxLoops
                              caseIDs <<- get.caseIDs() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% caseIDs
                              settings$filter$minLoops <<- minLoops
                              settings$filter$maxLoops <<- maxLoops
                            }

                            if(!is.null(statusDomain)){
                              history$selected <<- history$selected & (history$status %in% statusDomain)
                              caseIDs         <<- history$caseID[history$selected] %>% unique
                              settings$filter$statusDomain <<- statusDomain
                            }

                            if(!is.null(startStatuses %>% verify('character'))){
                              cp      = get.cases.path()
                              chosen  = cp$startStatus %in% startStatuses
                              caseIDs <<- get.caseIDs() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% caseIDs
                              settings$filter$startStatuses <<- startStatuses
                            }

                            if(!is.null(endStatuses %>% verify('character'))){
                              cp      = get.cases.path()
                              chosen  = cp$endStatus %in% endStatuses
                              caseIDs <<- get.caseIDs() %^% rownames(cp)[chosen]
                              history$selected <<- history$caseID %in% caseIDs
                              settings$filter$endStatuses <<- endStatuses
                            }

                            if(!is.null(IDs %>% verify('character'))){
                              caseIDs <<- caseIDs %^% IDs
                              settings$filter$IDs <<- IDs
                            }

                            history$selected <<- history$caseID %in% caseIDs

                            if(!is.null(freqThreshold %>% verify(c('numeric','integer'), domain = c(0, 1), lengths = 1))){
                              adjcy = get.adjacency()
                              if(!is.empty(adjcy)){
                                adjcy %>% apply(1, vect.normalise) %>% t %>% as.data.frame %>% rownames2Column('status') %>%
                                  reshape2::melt(id.vars = 'status', variable.name = "nextStatus") %>% filter(value < 1 - freqThreshold) -> paths

                                histsel  <- history[history$selected, ]
                                histsel  <- histsel[histsel$path %in% (paths$status %++% '-' %++% paths$nextStatus),]
                                outliers <- histsel$caseID %>% unique
                                caseIDs <<- caseIDs %-% outliers
                                history$selected <<- history$caseID %in% caseIDs
                                settings$filter$freqThreshold <<- freqThreshold
                              }
                            }
                            clear()
                          }
                        },

                        # Among the cases in the case list, returns IDs of cases who have ever been in the given 'status'
                        casesInStatus = function(status){
                          casedf  = history %>% filter(status == status) %>% extract('caseID') %>% unique
                          return(casedf$caseID)
                        },

                        casesInTransition = function(source, target){
                          casedf  = history %>% filter(path == source %++% '-' %++% target) %>% extract('caseID') %>% unique
                          return(casedf$caseID)
                        }

                      )
)

# You can provide roxygen documentation just like any other fuction:
# Summarize by class and volume
#
# @name TRANSYS_compute. ...
# @param include_name logical if TRUE include a column for the data set name
# @param save_file logical if TRUE saves as CSV
# @param filename character if save_file is TRUE then save to this file
# @return a data frame of 'UserLabel', 'Count' and 'Volume' or NULL

# TRANSYS$methods(
# compute.volumes.daily = function(full = F){
#
#   if(full){hist = history} else {hist = history %>% filter(selected)}
#
#   hist$startDate <- as.Date(hist$startTime, tz = attr(hist$startTime, "tzone"))
#   hist$endDate   <- as.Date(hist$endTime,   tz = attr(hist$endTime, "tzone"))
#
#   B.entr = hist %>% dplyr::group_by(startDate, status) %>% dplyr::summarise(length(caseID))
#   B.exit = hist %>% dplyr::group_by(endDate, status)   %>% dplyr::summarise(length(caseID))
#
#   names(B.entr) <- c('Date', 'Status', 'Entry')
#   names(B.exit) <- c('Date', 'Status', 'Exit')
#
#   B = merge(B.entr, B.exit, by = c('Date', 'Status'), all = T) # same as dplyr::full_join
#   B[is.na(B)] <- 0
#   E.entr = reshape2::dcast(B, Date ~ Status, mean, value.var = 'Entry')
#   E.exit = reshape2::dcast(B, Date ~ Status, mean, value.var = 'Exit')
#   rownames(E.entr)   <- as.character(E.entr$Date)
#   rownames(E.exit)   <- as.character(E.exit$Date)
#   E.entr = E.entr[, -1, drop = F]
#   E.exit = E.exit[, -1, drop = F]
#
#   w      = c(which(!duplicated(hist$caseID))[-1] - 1, nrow(hist))
#   E.stay = hist[w, c('caseID', 'nextStatus', 'endDate')] %>%
#     dplyr::group_by(nextStatus, endDate) %>%
#     dplyr::summarise(length(caseID)) %>%
#     reshape2::dcast(endDate ~ nextStatus, mean, value.var = 'length(caseID)')
#   rownames(E.stay) <- E.stay$endDate
#   E.stay           <- E.stay[,-1, drop = F]
#
#   sts = colnames(E.entr) %U% colnames(E.exit) %U% colnames(E.stay)
#   dts = as.character(seq(from = min(B$Date), to = max(B$Date), by = 1))
#
#
#   E.entr[, sts %-% colnames(E.entr)] <- 0
#   E.entr[dts %-% rownames(E.entr), ] <- 0
#   E.exit[, sts %-% colnames(E.exit)] <- 0
#   E.exit[dts %-% rownames(E.exit), ] <- 0
#   E.stay[, sts %-% colnames(E.stay)] <- 0
#   E.stay[dts %-% rownames(E.stay), ] <- 0
#
#   E.entr = E.entr[dts,sts]
#   E.exit = E.exit[dts,sts]
#   E.stay = E.stay[dts,sts]
#
#   E.entr[is.na(E.entr)] <- 0
#   E.exit[is.na(E.exit)] <- 0
#   E.stay[is.na(E.stay)] <- 0
#
#   volin <- new('TS.DAILY', from = min(B$Date), until = max(B$Date))
#   volin$feedData(E.entr %>% rownames2Column('Date'), timeCol = 'Date')
#
#   volout <- new('TS.DAILY', from = min(B$Date), until = max(B$Date))
#   volout$feedData(E.exit %>% rownames2Column('Date'), timeCol = 'Date')
#
#   E.entr = cumulative(E.entr)
#   E.exit = cumulative(E.exit)
#   E.stay = cumulative(E.stay)
#
#   V <- E.entr - E.exit
#   assert((sum(V[nrow(V),]) == 0) & (min(V) >= 0), "Something goes wrong! Volume cannot be negative!", match.call()[[1]])
#   V <- V + E.stay
#
#
#   backlog <- new('TS.DAILY', from = min(B$Date), until = max(B$Date))
#   backlog$feedData(V %>% rownames2Column('Date'), timeCol = 'Date')
#
#   if(full){
#     timeseries.full$volin.daily   <<- volin
#     timeseries.full$volout.daily  <<- volout
#     timeseries.full$backlog.daily <<- backlog
#   } else{
#     timeseries$volin.daily   <<- volin
#     timeseries$volout.daily  <<- volout
#     timeseries$backlog.daily <<- backlog
#   }
# }
# )

#' @export
summary.TRANSYS = function(obj){
  ff = obj$settings$filter %>% unlist
  "Prcoess time range from: " %>% paste0(
    obj$modelStart %>% as.character,
    " until: ",
    obj$modelEnd %>% as.character,
    " with ",
    nrow(obj$history),
    " events (",
    sum(obj$history$selected, na.rm = T),
    " filtered) and ",
    obj$get.statuses(full = T) %>% length,
    " stasuses (",
    obj$get.statuses(full = F) %>% length,
    " filtered) impacting ",
    obj$get.caseIDs(full = T) %>% length,
    " cases (",
    obj$get.caseIDs(full = F) %>% length,
    " filtered). Filter settings: ",
    chif(is.empty(ff), 'No filtering applied', (names(ff) %++% '= ' %++% (ff %>% paste(collapse = ' , '))) %>% paste(collapse = ' , ')))
  # "32,400 filtered) and 24 statuses (13 filtered) impacting 2,450 cases (83 filtered). Filter on completed cases with relative frequency threshold of 0.25 and loops range between 0 and 12"
}

# tstools.R -------------------------------------------------------------------------

# Header
# Filename:       tstools.R
# Description:    Contains utility functions for process modelling.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     06 July 2018
# Last Revision:  16 October 2018
# Version:        0.0.2
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     06 July 2018      Initial issue
# 0.0.2     16 October 2018   Function buildProcessMapPackage() modified: argument linkLabel added

buildProcessMapPackage = function(obj, nodeSize = 'totalEntryFreq', nodeColor = 'totalEntryFreq', linkColor = 'totalFreq', linkWidth = 'totalFreq', linkLabel = NULL, linkLength = NULL, config = NULL){
  # todo: build tooltip, link label
  obj %>% verify('list', names_identical = c('nodes', 'links'), null_allowed = F)

  nodes = obj$nodes
  links = obj$links

  # if(is.empty(nodes)){return(NULL)}
  if(!inherits(nodeSize, c('numeric', 'integer'))){
    nodeSize  %<>% verify('character', lengths = 1, domain = numerics(nodes), default = 100)
  }

  nodeColor %<>% verify('character', lengths = 1, default = 'lightblue')
  if(nodeColor %in% names(nodes)){nodes %<>% mutate_(color = nodeColor)} else {nodes %<>% mutate(color = nodeColor)}

  linkColor %<>% verify('character', lengths = 1, default = 'blue')
  if(linkColor %in% names(links)){links %<>% mutate_(color = linkColor)} else {links %<>% mutate(color = linkColor)}

  nodes %<>% mutate_(size = nodeSize, tooltip = 'status', label = 'status') %>%
    mutate(shape = ifelse(status %in% c('CASE START', 'CASE END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle'))) %>%
    select(ID = status, label, size, shape, color, tooltip) %>% na2zero

  if(nodeSize == 'totalEntryFreq'){nodes$size[nodes$label == 'CASE START'] <- nodes$size[nodes$label == 'CASE END'] %>% sum}

  if(!inherits(linkWidth, c('numeric', 'integer'))){
    linkWidth  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  if(!inherits(linkLength, c('numeric', 'integer'))){
    linkLength  %<>% verify('character', lengths = 1, domain = numerics(links), default = 40)
  }

  links %<>% mutate_(width = linkWidth, length = linkLength, label = linkLabel) %>%
    select(source = status, target = nextStatus, color, length, width, label) %>% na2zero

  cfg = config %<==>%
    list(link.width.max = 5, link.width.min = 1, link.smooth = list(enabled = T, type = 'curvedCCW'),
         node.label.size = 25, node.physics.enabled = T, layout = 'hierarchical', direction = 'up.down')

  return(list(data = list(nodes = nodes, links = links), config = cfg))
}

buildTreeTable = function(traces){
  bindrow = function(df, rn, row){
    df[rn, sequence(length(row))] <- row
    return(df)
  }
  aa = traces %>% pull(path) %>% strsplit('-')
  df = data.frame.na(nrow = length(aa), ncol = aa %>% sapply(length) %>% max)
  for (i in sequence(length(aa))){
    df %<>% bindrow(i, aa[[i]])
  }
  colnames(df) <- 'Step' %>% paste(sequence(ncol(df)), sep = '.')
  rownames(df) <- traces$variation
  df$freq      <- traces$freq
  return(df)
}





# tsvis.R -------------------------------------------------------------------

# Header
# Filename:       tsvis.R
# Description:    Visualisation functions for transys objects
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     12 October 2018
# Last Revision:  06 November 2018
# Version:        0.1.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     12 October 2018    Initial issue transferred from transys.R
# 0.1.0     16 October 2018    Function plot.statuses.box() added.
# 0.1.1     18 October 2018    Function plot.process.map() modified: does not use plot package any more but prepares customized nodes & links tables
# 0.1.2     05 November 2018   Function plot.process.map() modified: uses package grviz (renamed from diagramer)
# 0.1.3     06 November 2018   Function plot.traces.sunburst() modified: builds chartname to avoid re-plotting with same spec

############### Process Overview Visualisations:

# plots process map for the transition system
#' @export plot.process.map
plot.process.map = function(obj, measure = c('freq', 'time'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = c('grviz', 'visNetwork'), config = NULL){
  plotter   = match.arg(plotter)
  measure   = match.arg(measure)
  time_unit = match.arg(time_unit)
  nontime   = 'freq'
  k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])
  plotname  = 'process.map' %>% paste(measure, plotter, sep = '.')
  if(measure != 'freq'){plotname %<>% paste(time_unit, sep = '.')}

  if(!is.null(obj$plots[[plotname]])){return(obj$plots[[plotname]])}

  nodes = obj$get.nodes()
  links = obj$get.links()

  cfg = list(link.width.max = 12, link.width.min = 2, link.smooth = list(enabled = T, type = 'curvedCCW'),
             node.label.size = 40, link.label.size = 25, node.physics.enabled = T, layout = 'hierarchical', direction = 'up.down',
             node.size.min = 2, node.size.max = 4, node.size = 3, node.size.ratio = 0.6)

  nodes %<>%
    dplyr::mutate(shape = ifelse(status %in% c('START', 'END'), 'circle', ifelse(status %in% c('ENTER', 'EXIT'), 'diamond', 'rectangle')))

  if(measure == 'freq'){

    nodes$totalEntryFreq[nodes$status == 'START'] <- nodes$totalEntryFreq[nodes$status == 'END'] %>% sum(na.rm = T)

    nodes %<>%
      dplyr::mutate(label = status %>% paste0('\n', '(', totalEntryFreq, ')')) %>%
      dplyr::mutate(id = status, size = totalEntryFreq, color = totalEntryFreq)

    links %<>%
      dplyr::mutate(linkLabel = ' ' %++% totalFreq, linkTooltip = status %>% paste(nextStatus, sep = '-')) %>%
      dplyr::mutate(source = status, target = nextStatus, linkColor = totalFreq, linkWidth = totalFreq)

    list(nodes = nodes, links = links) %>%
      niraPlot(key = 'id', shape = 'shape', label = 'label', color = 'color', source = 'status', target = 'nextStatus', linkColor = 'linkColor', linkWidth = 'linkWidth', linkLabel = 'linkLabel', linkTooltip = 'linkTooltip', config = cfg, plotter = plotter, type = 'graph') -> obj$plots[[plotname]]
  } else {
    cfg$palette$color = c('white', 'red')

    nodes %<>%
      dplyr::mutate(meanDuration = k*meanDuration) %>%
      dplyr::mutate(label = status %>% paste0('\n', '(', meanDuration %>% round(digits = 2), ' ', time_unit %>% substr(1,1), ')'))

    links %<>%
      dplyr::mutate(meanTime = k*meanTime) %>%
      dplyr::mutate(label = ' ' %++% (meanTime %>% round(digits = 2) %>% paste(time_unit %>% substr(1,1))), tooltip = status %>% paste(nextStatus, sep = '-'))

    list(nodes = nodes, links = links) %>%
      niraPlot(key = 'status', shape = 'shape', label = 'label', color = list(color = 'totalDuration'), source = 'status', target = 'nextStatus', linkColor = list(color = 'totalTime'), linkWidth = 'totalTime', linkLabel = 'label', linkTooltip = 'tooltip', config = cfg, plotter = plotter, type = 'graph') -> obj$plots[[plotname]]

  }
  return(obj$plots[[plotname]])
}

#' plots process tree for the transition system
#' @export plot.process.tree
plot.process.tree = function(obj, plotter = 'sankeytree'){
  plotter = match.arg(plotter)
  tbl = obj$get.traces() %>% buildTreeTable()
  NNN = names(tbl) %-% 'freq'

  tbl %>% sankeytree.tree(label = as.list(NNN[sequence(min(8, length(NNN)))]), size = 'freq')
}

#' @export plot.process.sankey
plot.process.sankey = function(obj){
  list(nodes = obj$get.nodes(), links = obj$get.links()) %>%
    niraPlot(key = 'status', label = list(label = 'status'), source = list(source = 'status'), target = 'nextStatus', linkWidth = 'totalFreq', type = 'sankey', plotter = 'networkD3', config = list(node.label.size = 20, node.width = 25))
}

############### Trace Overview Visualisations:

#' @export plot.traces.bar
plot.traces.bar  = function(obj, measure = c('freq', 'time'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), aggregator = c('sum', 'mean', 'median', 'sd'), plotter = 'plotly'){
  agg = c('sum' = 'tot', 'mean' = 'avg', 'median' = 'med', 'sd' = 'sd')
  ags = c('sum' = 'Total', 'mean' = 'Average', 'median' = 'Median', 'sd' = 'Standard Deviation of')

  plotter    <- match.arg(plotter)
  measure    <- match.arg(measure)
  time_unit  <- match.arg(time_unit)
  k          <- 1.0/timeUnitCoeff[time_unit]
  aggregator <- match.arg(aggregator)

  TBL        <- obj$get.traces()

  if(measure == 'freq'){
    sumfreq = sum(TBL$freq, na.rm = T)

    TBL %>%
      dplyr::mutate(freqper = 100*freq/sumfreq) %>%
      dplyr::mutate(tooltip = path %>% paste0('  Freq: ', freq, ' (', freqper %>% round(digits = 1), '%)')) %>%
      dplyr::arrange(freq) %>%
      niraPlot(x = 'freq', y = 'variation', tooltip = 'tooltip', type = 'bar', plotter = plotter)

  } else {
    coln = agg[aggregator] %++% 'Time'
    TBL[, coln] <- TBL[, coln]*k
    TBL$tooltip = TBL$path %>% paste0('  ', ags[aggregator], ' Process Time: ', TBL %>% pull(agg[aggregator] %++% 'Time') %>% round(digits = 2), ' ',time_unit, 's')
    TBL %>%
      dplyr::arrange_(coln) %>%
      niraPlot(x = coln, y = 'variation', tooltip = 'tooltip', type = 'bar', plotter = plotter, color = niragen::color.mean('red', 'white'))
  }
}

#' @export plot.traces.sunburst
plot.traces.sunburst = function(obj, min_freq = 10, plotter = c('sunburstr', 'd2b'), width = 600, height = 600){
  plotter   = match.arg(plotter)
  chartname = paste('traces', 'sunburst', plotter, min_freq, sep = '.')
  if(is.null(obj$plots[[chartname]])){
    tbl = obj$get.traces() %>%
      dplyr::select(path, freq) %>% dplyr::filter(freq > min_freq - 1)
    if(!is.empty(tbl)){
      if(plotter == 'sunburstr'){
        tbl %>% sunburstR::sunburst(count = T, width = width, height = height, legend = list(h = 30, w = 120, r = 10, s = 5)) -> obj$plots[[chartname]]
      } else if (plotter == 'd2b'){
        tbl %>% sunburstR::sund2b() -> obj$plots[[chartname]]
      }
    }
  }

  return(obj$plots[[chartname]])
}

############### Status Overview Visualisations:

#' @export plot.statuses.bar
plot.statuses.bar = function(obj, measure = c('freq', 'totTime'), time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
  colname   = c(freq = 'totalEntryFreq', totTime = 'totalDuration')
  vrs       = c(freq = 'Total Entry Freq', totTime = 'Total Duration')
  nontime   = 'freq'
  measure   = match.arg(measure)
  time_unit = match.arg(time_unit)
  k         = chif(measure %in% nontime, as.integer(1), 1.0/timeUnitCoeff[time_unit])
  tus       = chif(measure == 'freq', '', ' ' %++% time_unit %++% 's')

  ND = obj$get.nodes() %>% as.data.frame

  if(measure %in% 'totTime'){ND[, colname[measure]] <- k*ND[, colname[measure]]}

  cfg = list(title = chif(measure == 'freq', 'Status Entry Frequencies', 'Status Durations'),
             xAxis.label = chif(measure == 'freq', 'Frequency', 'Duration'),
             yAxis.label = 'Status',
             point.color = chif(measure == 'freq', NULL, niragen::color.mean('white', 'red')))

  ND[, colname[measure]] <- k*ND[, colname[measure]]
  ND$tooltip <- ND$status %>% paste0('  (', vrs[measure],': ', ND %>% pull(colname[measure]) %>% round(digits = 2), tus, ')')

  ND %>% arrange_(colname[measure]) %>%
    niraPlot(x = colname[measure], y = 'status', tooltip = 'tooltip', plotter = 'plotly', type = 'bar', config = cfg)
}

#' @export plot.statuses.box
plot.statuses.box = function(obj, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year')){
  time_unit = match.arg(time_unit)
  k         = 1.0/timeUnitCoeff[time_unit]

  H = obj$history[obj$history$selected, ]
  H$duration  <- (k*H$duration) %>% round(digits = 2)

  cfg = list(title = 'Status Durations', xAxis.label = 'Duration' %>% paste0(' (', substr(time_unit,1,1), ')'), yAxis.label = 'Status')
  H %>% niraPlot(x = 'duration', y = 'status', plotter = 'plotly', type = 'box', config = cfg)
}

############### Case Overview Visualisations:

#' @export plot.cases.status.pie
plot.cases.status.pie = function(obj, measure = 'time', aggregator = 'sum', time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), trim = NULL){
  measure    = match.arg(measure)
  aggregator = match.arg(aggregator)
  time_unit  = match.arg(time_unit)

  cst          <- obj$get.cases.statusTime()
  aa           <- cst[obj$get.caseIDs(), ] %>% colSums(na.rm = T) %>% as.data.frame %>% rownames2Column('status')
  names(aa)[2] <- 'duration'
  if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim'))){
    sm <- sum(aa$duration, na.rm = T)
    aa <- aa[aa$duration >= trim*sm, ]
  }
  aa$duration  <- aa$duration/timeUnitCoeff[time_unit]

  aa %>% niraPlot(theta = 'duration', label = 'status', config = list(legend.enabled = F), type = 'pie', plotter = 'plotly')
}

#' @export plot.cases.table
plot.cases.table = function(obj, time_unit = c('second', 'minute', 'hour', 'day', 'week', 'year'), plotter = 'DT', config = list(paging.length = 20)){
  plotter   = match.arg(plotter)
  time_unit = match.arg(time_unit)
  k         = 1.0/timeUnitCoeff[time_unit]

  tbl = obj$cases$profile[obj$get.caseIDs(), ]
  tbl$completed = tbl$caseStart & tbl$caseEnd
  tbl$duration = (k*tbl$duration) %>% round(digits = 2)

  tbl = tbl[, c('startTime', 'endTime', 'completed', 'duration')]
  cln = list( 'Started at' = 'startTime', 'Ended at' = 'endTime', Completed = 'completed', Duration = 'duration')
  names(cln)[4] %<>% paste0(" (", substr(time_unit,1,1), ")")

  tbl %>% niraPlot(label = cln, plotter = 'DT', type = 'table', config = config, filter = 'top')
}

############### Status Card Visualisations:

#' @export plot.status.gauge
plot.status.gauge = function(obj, statusID, measure = c('loopRate', 'caseRatio')){
  measure = match.arg(measure)
  # cfg = list(thetaAxis.zone = list( list(min = 0, max = 30, color = 'green'), list(min = 30, max = 60, color = 'yellow'), list(min = 60, max = 100, color = 'red')))
  c3.gauge(100*obj$get.status.metric(statusID, measure))
}

#' @export plot.status.next.pie
plot.status.next.pie = function(obj, statusID, trim = NULL, plotter = 'plotly'){
  if(verify(statusID, 'character', varname = 'statusID') %in% obj$get.statuses()){
    AD = obj$get.adjacency()
    SN = AD[statusID, ] %>% t %>% as.data.frame %>% rownames2Column('status') %>%
      dplyr::select_('status', freq = 'statusID')
    if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim')))
    {SN = SN[SN$freq/sum(SN$freq, na.rm = T) > trim, ]}
  } else {
    SN = data.frame(status = character(), freq = numeric(), stringsAsFactors = F)
  }
  SN %>% na.omit %>% niraPlot(label = 'status', theta = 'freq', type = 'pie', plotter = plotter, config = list(title = 'Next Status Distribution'))
}

#' @export plot.status.prev.pie
plot.status.prev.pie = function(obj, statusID, trim = NULL, plotter = 'plotly'){
  if(verify(statusID, 'character', varname = 'statusID') %in% obj$get.statuses()){
    AD = obj$get.adjacency()
    SP = AD[, statusID] %>% as.data.frame
    SP$status = rownames(AD)
    names(SP)[1] <- 'freq'
    if(!is.null(trim %>% verify('numeric', lengths = 1, domain = c(0, 1), varname = 'trim')))
    {SP = SP[SP$freq/sum(SP$freq, na.rm = T) > trim, ]}
  } else {
    SP = data.frame(status = character(), freq = numeric(), stringsAsFactors = F)
  }

  SP %>%  na.omit %>% niraPlot(label = 'status', theta = 'freq', type = 'pie', plotter = plotter, config = list(title = 'Previous Status Distribution'))
}


############### Volume Time Series Visualisations:

#' @export plot.volumes.area
plot.volumes.area = function(obj, period = 'daily', trim = 0.01, plotter = 'streamgraph'){
  period = match.arg(period)
  back   = obj$get.backlog(as_timeseries = T, period = period)
  colns  = names(back$data) %-% c('date', 'START', 'END', 'ENTER', 'EXIT')
  sumps  = colSums(back$data[, colns]) %>% vect.normalise
  colns  = names(sumps)[which(sumps >= trim)]
  back$data[, c('date', colns)] %>%
    niraPlot(x = 'date', y = colns %>% as.list, plotter = plotter, type = 'tsarea')
}





# DESCRIPTION ------------------------------------------------------------
Package: niraprom
Type: Package
Title: This package will be a complete toolbox for process mining, management, prediction, simulation and optimization
Imports: niragen
Version: 1.5.4
Date: 06 November 2018
Author: Nima Ramezani
Maintainer: Nima Ramezani <nima.ramezani@gmail.com>
  Description: The package is still under construction. Documentation incomplete! Use at your own risk!
  License: MIT
LazyData: TRUE
RoxygenNote: 6.0.1
Collate:
  'tstools.R'
  'prosim.R'
  'promtools.R'
  'prom.R'
  'transys.R'
  'niraprom.R'
  'promvis.R'
  'tsvis.R'


# visuals ---------------------------------------------------------------------

  # Panel,Title,Chart,plotter,Description,Used for selection,Responds to selected
  # Case Card,Case Trace(map),Graph or ?,,,,
  # Activity Card,Activity to Case Ratio (rework rate),Gauge,,Shows average activity frequency per case,--,Activity
  # Activity Overview,Activity frequency,bar/pie,,shows distribution of tasks over activities (task types),Activity,
  # Case Card,Case lifetime view,timeline,timevis,Time line view of the case from birth to death,,Case
  # Case Card,Case (Total/Mean/Median/…) (Throughput/Idle/Waiting/Process/Suspension) Time,infobox,,,,
  # Case Overview,Case List,Table,DT,"Shows list of cases in a table. Columns: Case ID, Total Waiting time, total processing time, start, end, first activity, last activity, trace, …",Case,
  # Task Overview,Task list,Table,DT,"Shows list of tasks for selected activites, agents",Tasks,
  # Case Overview,StatusTime bar chart for each case,bar,,Horizontal Stack bar-chart showing status sequence and time spent on each status (one bar for each case),,
  # Case Overview,StatusTime bar chart for each trace,bar,,Horizontal Stack bar-chart showing status sequence and average/total time spent on each status (one bar for each trace),,
  # Case Overview,"Volume-Age time series (Status, Activity, Team, Agent Levels)",line/bar/area,,Shows volume of cases in each status/activity/team at each age,,
  # Activity Card,Next/Previous activity distribution,Pie,,,,Activity
  # Process Overview,Activity Adjacency Matrix,Heatmap,,"Shows: relative_antecedent, relative_consequent and absoulte frequency depending on user's selection",--,--
  #   Process Overview,Process Flow (Activity Level),Sankey,networkD3,,,Relative Frequency Threshold
  # Process Overview,Total Processing Time,infobox,,,,
  # Process Overview,Average processing time,infobox,,,,
  # Process Overview,"Process Flow (Status, Team, agent,activity, actGroup Level)",Sankey,networkD3,,,
  # Agent Overview,Agent Social Work Handover Map,Graph,diagramer,"Shows the employee network, user can select between time and frequency",Agent,RFT for Agent Map
  # Agent Overview,Agent Adjacency Matrix (Social Work Handover Matrix),Heatmap,,,,
  # Activity Overview,Activity Processing Time,Box,,"Shows min, 1st quartile, median, 3rd quartile, max and average activity processing time in a box plot ",Activity,
  # Activity Overview,Activity Idle Time (Waiting time),Box,,"Shows min, 1st quartile, median, 3rd quartile, max and average activity idle time in a box plot  (requres arrival events to be given in event log)",Activity,
  # Agent Overview,Agent Processing Time,Box,,"Shows min, 1st quartile, median, 3rd quartile, max and average agent productive time in a box plot ",Agent,
  # Agent Overview,Agent Idle Time,Box,,,Agent,
  # Process Overview,process.map,Graph,diagramer,process map on status/team/agent/activity/actGroup Level showing all transitions in a graph,,
  # Process Overview,process.map,Graph,visNetwork,process map on status/team/agent/activity/actGroup Level showing all transitions in a graph,,
  # Process Overview,"Process Map (Time Series) (Team, Status, agent, activity)",Graph,,"A set of process maps where edge widths and node sizes vary over time, node size reflects backlog and edge width reflect volume in/out",,
  # Process Overview,process.tree,SankeyTree,,"In a process tree, repeating nodes will have a different label (status/team/activity/agent levels)",,
  # Activity Overview,Activity Volin/Volout/Backlog History,tsline/tsbar/tsarea,,"Shows count of live tasks (Backlog) per activity (time series aggregated: hourly, daily, weekly, monthly)",,
  # Activity Overview,Volin/volout/backlog History per activity,tsline/tsbar/tsarea,,Timeseries showing history of task volume-in distributed over activities,,
  # Agent Card,Agent Processing/idle Time per activity,Heatmap/bar,,"Similar as Agent-Skill matrix in SO, shows each agent's average processing time per activity",,
  # Agent Card,Agent (Total/mean/…) time distribution over activities (percentage and absolute),pie/donut//treemap,,Distribution of time spent over activities (idle time is also shown like an activity) (percentage is only used for total time),,
  # Agent Card,Agent (Total/mean/…) time distribution over activities as time series (Weekly/Daily/…) (percentage and absolute),pie/donut/treemap,,"A series of pie charts showing distribution of time spent over activities per day for example (idle time is also shown like an activity). For example, on each week, how much time spent on each activity and how much was idle?   (percentage is only used for total time)",,
  # Agent Card,Agent (Total/mean/…) Time spent history (distributed per activity+idle),tsline/tsbar/tsarea,,,,
  # Agent Card,Agent Volume-out distribution per activity,pie,,,,
  # Agent Card,Agent Volume-out History,tsline/tsbar/tsarea,,,,
  # Team Card,Team Volin/Volout/Backlog History,tsline/tsbar/tsarea,,"Shows history of count of live tasks per team (time series aggregated: hourly, daily, weekly, monthly): helps to find bottlenecks and evaluate team backlogs",,
  # Process Overview,process.sunburst,Sunburst,sunburstR/d2b,,,
  # Activity Card,Activity loop rate,Gauge,,"For each activity, ratio(percentage) of entries that follow itself, over total entry-frequency ",,
  # Loop Overview,loop distribution,Table/bar?,,How many loops have been observed? (Shows all the loops with associated case frequencies),,
  # Team Card,Team Idle/Process/Waiting Time per activity/agent,Heatmap/bar,,,,
  # Agent Overview,Agent Volume-out distribution per activity,stack bar,,,,
  # Task Card,processing time variability,Box,,,,Tasks
  # Team Overview,Team Work Handover map,Graph,diagramer,,,
  # Team Overview,Team Adjacency Matrix (Team work handover matrix),,,,,
  # Team Overview,Team Processing Time Variability,,,,,
  # Team Overview,Team Idle time,,,,,
  # Team Overview,Team vol-out distribution per activity,,,,,
  # Team Overview,Team Processing Time per activity,,,,,
  # Team Card,Team Processing Time per activity,Heatmap/bar,,,,
  # Team Card,Team Volume-in History,tsline/tsbar/tsarea,,,,
  # Team Card,Team Volume-out distributed by agent,Pie,,,Team,Date
  # Team Card,Team Volume-out History,tsline/tsbar/tsarea,,,,
  # Team Overview,Process Flow (Team Level),Sankey,networkD3,,,
  # Process Overview,process.statusTime,pie,plotly,Overall/Throughput time distribution over statuses,,
  # Process Overview,process.caseFreq,infobox,,How many cases do we have in this process?,,
  # Process Overview,process.caseCompleted,infobox,,How many completed cases do we have in this process,,
  # Process Overview,process.caseLoops,infobox,,How many loops do we have in the process + mean/median/sd per case,,
  # Status Card,Status to Case Ratio (rework rate),Gauge,,Shows average status frequency per case,--,Status
  # Status Card,status.process.map,Graph,diagramer/visNetwork,Shows sub-process map in activity level for selected status (user can select between time and frequency),,
  # Status Card,status.next.pie,Pie,plotly,Next Status Distribution,,
  # Status Card,status.previous.pie,Pie,plotly,,,
  # Status Card,status.duration,Guage,plotly,,,
  # Status Card,status.loopRate,Gauge,,Percentage of entries to each status that follow the same status per entry frequency,,
  # Status Overview,statuses.duration,box,plotly,"Boxplot showing distribution of duration (min, max, med, mean and quartiles) for each status",,
  # Time Card,Live tasks Distribution,pie,,Count of live tasks for a given time stamp distributed over activities(task types),,
  # Activity Card,Activity Processing Time Distribution,Histogram,,,,
  # Activity Card,Activity Processing/Waiting/suspension/idle/throughput Time Distribution,Histogram,,requires arrival events to be given,,
  # Time Card,Working Employees,bar,,How many employees are working in each team (percentage),,
  # Time Card,Live tasks per team,bar,,How many tasks are live in each team?,,
  # Trace Card,Case Frequency,infobox,,,,
  # Activity Card,Volin/volout/backlog History,tsline/tsbar/tsarea/calheatmap,,Timeseries showing history of task volume-out for selected activity,,
  # Trace Card,Flow change over age (percentage and absolute count),Funnel,,A series of funnels one for each age showing process flow ,,
  # Trace Overview,trace sunburst,Sunburst,sunburstr,,Trace,
  # Trace Overview,Trace bar,stack bar ?,,Shows all traces as a sequence of activities,,
  # Trace Overview,Trace Distribution,bubble,bubbles,"Shows frequency of cases distributed on traces in a bubble chart: each bubble is a trace, size reflects frequency of cases following that trace(path or process variation)",,
  # Transition Card,Case Frequency,infobox,,How many cases had this transition?,,
  # Activity card,"Processing/Waiting/suspension/idle time series (total/mean/median,…) (Weekly/daily/…)",tsline/tsbar/tsarea/calheatmap,,For example how much was total waiting time for each day,,
  # Agent Card,"Process/idle time series (total/mean/median,…) (Weekly/daily/…)",tsline/tsbar/tsarea,,,,
  # Team Card,"Process/Waiting/suspension/idle time series (total/mean/median,…) (Weekly/daily/…)",tsline/tsbar/tsarea,,,,
  # Activity Card,isolated map,Graph,,"An isolated process map for a given time range showing the selected node in center and all inputs and outputs with counts as labels of edges.
  # For completed tasks, next arrival shows the output node. For arrived tasks, previous completion determines the input node. A suspend, meeting interval ends is also a node. ",,
  # Team Card,isolated map,Graph,,"An isolated process map for a given time range showing the selected node in center and all inputs and outputs with counts as labels of edges.
  # For completed tasks, next arrival shows the output node. For arrived tasks, previous completion determines the input node. A suspend, meeting interval ends is also a node. ",,
  # Activity Card,hourly arrival/start/completion time distribution for given date,tsline/tsbar/tsarea/calheatmap,,"for a given time range, a time series showing for example volume in (count of arrivals) for each hour",,



