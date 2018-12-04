# amCharts.R ----------------------------------------------------------------
# Header
# Filename:       amCharts.R
# Description:    Contains functions for plotting various charts from package amCharts using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     28 November 2016
# Last Revision:  24 July 2018
# Version:        1.1.9
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 November 2016   Initial issue
# 1.1.0     03 April 2017      Fundamental changes with standard format: Calls prepare4Plot & prepareAusthetics from visgen.R
# 1.1.1     03 April 2017      amCharts.defset
# 1.1.2     19 April 2017      amCharts.tsline() added
# 1.1.3     20 June 2018       All config verifications tranferred to visgen. Function amCharts.prepareConfig() removed. Function verifyConfig() is called in each plot
# 1.1.4     20 June 2018       amCharts.gauge() added
# 1.1.5     30 June 2018       amCharts.gauge() modified.
# 1.1.6     30 June 2018       amCharts.funnel() added.
# 1.1.7     30 June 2018       amCharts.box() added.
# 1.1.8     30 June 2018       amCharts.bullet() added.
# 1.1.9     24 July 2018       amCharts.tsline() added.


symbols = c('point', 'circle', 'bubble', 'square', 'rhombus', 'diamond', 'delta', 'right', 'left')
ltypes  = c('line', 'dashLine')
valid.amCharts.shapes = c('line', paste(ltypes[1], symbols, sep = '.'), 'dashLine', paste(ltypes[2], symbols, sep = '.'))

amCharts.bullet = character()
amCharts.bullet[valid.amCharts.shapes] = c(NA, rep('bubble', 3), 'square', rep('diamond',2), 'triangleUp', 'triangleRight', 'triangleLeft') %>% rep(2)

amCharts.linetype = numeric()
amCharts.linetype[valid.amCharts.shapes] = c(rep(0, 10), rep(1, 10))

amCharts.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer"),
    y = c("character", "factor", "numeric", "integer")),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y'),
  xLabelsRotation = 0
)

amCharts.pie.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c("numeric", "integer"),
    label = c("character", "factor")),
  multiples  = c(),
  essentials = c('theta', 'label')
)

amCharts.tsline.defset = defset %>% list.edit(
  dimclass   = list(
    x = c('POSIXct', 'POSIXlt'),
    y = 'numeric',
    high = 'numeric',
    low  = 'numeric',
    shape = 'character',
    color = 'character'),
  multiples  = c('y', 'shape', 'color', 'high', 'low'),
  essentials = c('x', 'y')
)

amCharts.gauge.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = 'theta',
  aggregator = mean,
  theta.min = 0,
  theta.max = 100,
  thetaAxis.tick.step = 20
  # todo: set in settings table
)

amCharts.bullet.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c('numeric', 'integer'),
    y = c('numeric', 'integer')
  ),
  multiples  = c(),
  essentials = 'x or y',
  aggregator = mean,
  x.min = 0,
  x.max = 100,
  xAxis.tick.step = 20
  # todo: set in settings table
)

amCharts.funnel.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    label = c('character', 'factor'),
    y     = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('y', 'label'),
  direction = 'up.down'
  # todo: add to settings
)

amCharts.box.defset = defset %>% list.edit(
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer"),
    y = c("character", "factor", "numeric", "integer")),
  multiples  = c(),
  essentials = c('x', 'y')
)

amCharts.applyConfig = function(obj, config){
  amOptions(obj, theme = chif(is.null(config$theme), 'none', config$theme),
            legend = config$legend.enabled, legendPosition = config$legend.position, legendAlign = "left",
            creditsPosition = 'top-right', main = config$title, mainColor = config$title.color,
            mainSize = 15, zoom = T, scrollbar = config$yAxis.scrollbar.enabled, scrollbarHeight = config$yAxis.scrollbar.width,
            valuescrollbar = config$xAxis.scrollbar.enabled, valuescrollbarHeight = config$yAxis.scrollbar.width, 
            labelRotation = config$xAxis.label.rotation)
}

amCharts.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)
  
  hor     = isHorizontal(obj, L$x, L$y)
  Ly      = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  clrvect = getColorVect(Ly, L$color, config)
  
  amBarplot(data = obj, x = Lx, y = Ly, horiz = hor,
            stack_type    = chif(is.null(config$barMode), NULL, chif(config$barMode == gndcd(37,31,60,154,82), NULL, 'regular')), 
            groups_color  = clrvect, ...) %>% amCharts.applyConfig(config)
}

amCharts.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics
  
  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)
  
  names(obj) <- c('label', 'value')
  amPie(data = obj, ...)
}

amCharts.tsline = function(obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, high = NULL, low = NULL, config = NULL, ...){
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, size = size, color = color, high = high, low = low, extend = c('shape', 'size', 'color'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'size', 'color')
  
  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)
  
  NSeries = length(L$y)
  if(is.null(config$point.size)){bulsize = a$aesthetics[[gndcd(144,118,66,130)]] %>% unlist} else {bulsize = config$point.size %>% vect.extend(NSeries)}
  if(is.null(config$line.width)){linewid = a$aesthetics[[gndcd(144,118,66,130)]] %>% unlist} else {linewid = config$line.width %>% vect.extend(NSeries)}
  if(is.null(linewid)){linewid = 1}
  if(is.null(L$shape)){
    bul = NULL
    ltp = NULL
  } else {
    bul = amCharts.bullet[L$shape]
    ltp = amCharts.linetype[L$shape]
    bul[is.na(bul)] <- 'round'
    ltp[is.na(ltp)] <- 0
  }
  
  if(is.null(L$high) | is.null(L$low)){dat = L$y} else {
    dat = list()
    for (i in seq(L$y)){
      if(is.empty(L$high[i]) | is.empty(L$low[i])){
        dat[[i]] = L$y[i]
      } else {
        dat[[i]] = c(L$low[i], L$y[i], L$high[i])
      }
    }
  }
  
  amTimeSeries(obj, L$x, dat, bullet = bul, linetype = ltp, linewidth = linewid, bulletSize = bulsize,
               color  = chif(is.null(L$color), c("#2E2EFE", "#31B404", "#FF4000", "#AEB404"), L$color),
               legend = chif(is.null(config[['legend']]), T, config[['legend']]),
               main   = chif(is.null(config[['title']]), '', config[['title']]),
               ylab   = chif(is.null(config$yAxis.label), '', config$yAxis.label), ...)
}

amCharts.gauge = function(obj = data.frame(), theta = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.gauge.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(is.empty(obj)){return(NULL)}
  
  df = data.frame(min = numeric(), max = numeric(), color = character())
  for(e in config$thetaAxis.zone){df %<>% rbind(e %>% list.extract(c('min', 'max', 'color')) %>% as.data.frame)}
  colnames(df) <- c('start', 'end', 'color')
  
  config$aggregator %>% do.call(list(obj[, L$theta])) %>% 
    amAngularGauge(start = config$theta.min, end = config$theta.max, 
                   step  = config$thetaAxis.tick.step,
                   bands = df)
}

amCharts.funnel = function(obj, y = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  label %<>% renameSeries('description')
  y     %<>% renameSeries('value')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(y = y, label = label)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(is.empty(obj)){return(NULL)}
  
  obj %>% amFunnel(inverse = config$direction == 'down.up', ...)
}


amCharts.box = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.box.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}
  
  if(hor){frml = as.formula(L$x %++% '~' %++% L$y)} else {frml = as.formula(L$y %++% '~' %++% L$x)}
  amBoxplot(frml, data = obj, horiz = hor, ...)
}


amCharts.bullet = function(obj = data.frame(), x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
  config = amCharts.bullet.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'amCharts')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(is.empty(obj)){return(NULL)}
  
  hor = is.null(L$y)
  
  assert(hor | is.null(x), 'For bullet chart, only one of dimensions x or y should be selected. Both of them cannot have values!')
  
  config$aggregator %>% do.call(list(obj[, chif(hor, L$x, L$y)])) %>% 
    amBullet(min   = chif(hor, config$x.min, config$y.min), 
             max   = chif(hor, config$x.max, config$y.max),
             steps = chif(hor, config$xAxis.step.enabled, config$yAxis.step.enabled),
             horiz = hor)
}

















# old functions:

#' amCharts.gauge.settings <- list(colorder = c('red', 'yellow', "#00CC00", 'purple', 'aqua', 'blue', 'yellow', 'magenta', 'cyan', 'black', 'grey', 'orange'))
#' 
#' 
#' amCharts.gauge.old = function(theta, legend = list(), config = NULL, ...){
#'   assert(require(rAmCharts), "Package rAmCharts is not installed!", err_src = match.call()[[1]])
#' 
#'   legend$min  = verify(legend$min , 'numeric', varname = 'legend$min', default = 0.00)
#'   legend$max  = verify(legend$max , 'numeric', varname = 'legend$max', domain = c(legend$min, Inf), default = 180.00)
#'   legend$percentage  = verify(legend$percentage , 'logical', varname = 'legend$percentage', domain = c(T, F), default = F)
#' 
#'   if (legend$percentage){
#'     legend$levels  = verify(legend$levels , 'numeric', varname = 'legend$levels', domain = c(0.0, 100.0), default = c(0.0, 30.0, 70.0, 100.0))
#'   } else {
#'     legend$levels  = verify(legend$levels , 'numeric', varname = 'legend$levels', default = c(legend$min, legend$min + 0.3*(legend$max - legend$min), legend$min + 0.7*(legend$max - legend$min), legend$max))
#'   }
#' 
#'   legend$levels = sort(legend$levels)
#'   nn     = length(legend$levels)
#'   assert(nn > 1, "Argument 'legend$levels' needs at least two elements!", err_src = match.call()[[1]])
#' 
#'   L  = legend$levels[nn] - legend$levels[1]
#' 
#'   if (is.null(config)){config = amCharts.gauge.settings}
#' 
#'   if (legend$percentage){
#'     bands = data.frame(start = legend$levels[- nn], end = legend$levels[- 1], color = config$colorder[sequence(nn-1)], stringsAsFactors = FALSE)
#'     ag    = amAngularGauge(x = round(100*(theta - legend$min)/(legend$max - legend$min), 2), start = 0, end = 100, step = 20, bands = bands, text = "%", ...)
#'     ag    = amCharts.gauge.apply.settings(ag, config)
#'   } else {
#'     bands = data.frame(start = legend$levels[- nn], end = legend$levels[-1], color = config$colorder[sequence(nn-1)], stringsAsFactors = FALSE)
#'     ag    = amAngularGauge(x = round(theta, 2), start = legend$levels[1], end = legend$levels[nn], step = L/5, bands = bands, ...)
#'     ag    = amCharts.gauge.apply.settings(ag, config)
#'     # %>% setProperties(fontSize = 10, adjustSize = T) #%>% amOptions(theme = 'dark', mainSize = 2, scrollbarHeight = 2)
#'   }
#'   return(ag)
#' 
#' }
#' 
#' amCharts.gauge.apply.settings = function(plt, config){
#'   props2bSet = c('fontSize', 'adjustSize')
#'   eval(parse(text = paste0('plt %>% setProperties(', list2Script(config, fields = props2bSet, fields_remove = 'colorder'), ')')))
#'   eval(parse(text = paste0('plt %>% amOptions(', list2Script(config, fields_remove = c('colorder', props2bSet)), ')')))
#'   return(plt)
#' }




# billboarder.R ---------------------------------------------------------------- 

# Header
# Filename:       billboarder.R
# Description:    Contains functions for plotting various billboarder charts from package billboarder using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     05 March 2018
# Last Revision:  17 September 2018
# Version:        0.1.1
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     05 March 2018       Initial issue
# 0.0.2     25 June 2018        All config validations transferred to visgen
# 0.0.3     25 June 2018        Function applyConfig() modified: more config properties supported
# 0.0.4     25 June 2018        Function applyColors() added: applies property config$point.color
# 0.0.5     25 June 2018        Function applyColors() added: applies property config$point.color if argument color is NULL
# 0.0.6     25 June 2018        pie chart added
# 0.0.7     25 June 2018        tsline chart added
# 0.0.8     18 July 2018        Config property point.color renamed to color 
# 0.0.9     24 July 2018        billboarder.bar.molten() embedded in added function billboarder.bar() 
# 0.1.0     24 July 2018        billboarder.bb_opt() removed: applyConfig() and applyColor() now directly modifies object property and data lists
# 0.1.1     17 Spetember 2018   billboarder.tsarea() added

billboarder.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric','integer', 'character', 'factor'),
    group = c('factor', 'character')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

# valid.yAxis.label.positions = c('top', 'middle', 'bottom') 
# valid.yAxis.label.positions %<>% c(
#   paste('inner', valid.yAxis.label.positions, sep = '-'),
#   paste('outer', valid.yAxis.label.positions, sep = '-'), 'inner', 'outer')
# 
# valid.xAxis.label.positions = c('left', 'right') 
# valid.xAxis.label.positions %<>% c(
#   paste('inner', valid.xAxis.label.positions, sep = '-'),
#   paste('outer', valid.xAxis.label.positions, sep = '-'), 'inner', 'outer')
# valid.legend.positions = c('right', 'bottom', 'top-right', 'top-left', 'bottom-right', 'bottom-left')



billboarder.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric','integer'),
    y     = c('numeric','integer'),
    group = c('factor', 'character')),
  multiples  = c(),
  essentials = c('x', 'y')
)

billboarder.pie.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    label = 'character',
    theta = c('numeric','integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

billboarder.tsline.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c('Date', 'POSIXct'),
    y = c('numeric','integer')),
  multiples  = c('y'),
  essentials = c('x', 'y'),
  subchart.enabled = TRUE
)



billboarder.applyColors <- function(bb, colorlist, opacity = 1) {
  bb$x$bb_opts[["billboarderspecials"]] %<>% list.edit(opacity = opacity) 
  bb$x$bb_opts[["data"]] %<>% list.edit(colors = colorlist)
  return(bb)
}

billboarder.applyConfig = function(bb, config){
  nin = config$legend.position %in% c('buttom', 'right')
  if(!is.null(config$point.label.enabled)){
    bb %<>% bb_data(labels = config$point.label.enabled)
  }
  
  # Apply xAxis and yAxis label formats:  
  if     (!is.null(config$xAxis.tick.label.format)){xAxTckFrmt = config$xAxis.tick.label.format}
  else if(!is.null(config$xAxis.tick.label.suffix)){xAxTckFrmt = billboarder.format.suffix.js(config$xAxis.tick.label.suffix)} 
  else {xAxTckFrmt = NULL}
  
  if     (!is.null(config$yAxis.tick.label.format)){yAxTckFrmt = config$yAxis.tick.label.format}
  else if(!is.null(config$yAxis.tick.label.suffix)){yAxTckFrmt = billboarder.format.suffix.js(config$yAxis.tick.label.suffix)} 
  else {yAxTckFrmt = NULL}
  
  bb$x$bb_opts[["grid"]] %<>% list.edit(
    y = list(show = config$yAxis.grid.enabled),
    x = list(show = config$xAxis.grid.enabled)
  )
  
  bb$x$bb_opts[["axis"]] %<>% list.edit(
    y = list(tick  = list(format = yAxTckFrmt, fit = FALSE),
             label = list(text = config$yAxis.label, position = config$yAxis.label.position)),
    x = list(tick  = list(format = xAxTckFrmt, fit = FALSE),
             label = list(text = config$xAxis.label, position = config$xAxis.label.position))
  )
  
  bb$x$bb_opts[["legend"]] %<>% list.edit(
    show     = config$legend.enabled,
    position = chif(nin, config$legend.position, 'inset'), 
    inset    = chif(nin, NULL, list(anchor = config$legend.position))
  )
  
  bb$x$bb_opts[["title"]] %<>% list.edit(
    text     = config$title,
    position = config$title.position
    # todo: padding doesn't work! check why?
    # padding  = list(top = config$title.padding.top, right = config$title.padding.right, left = config$title.padding.left, buttom = config$title.padding.buttom)
  ) %>% list.clean
  
  bb$x$bb_opts[["caption"]] %<>% list.edit(
    text = config$subtitle 
  )  
  
  if(config$stack.enabled){
    bb$x$bb_opts[["data"]] %<>% list.edit(groups  = config$stack.groups)
  }
  
  bb$x$bb_opts[["point"]] %<>% list.edit(
    r = config$size
  )
  
  bb$x$bb_opts[["subchart"]] %<>% list.edit(
    show = config$subchart.enabled, 
    size = list(height = config$subchart.height)
  )
  
  return(bb)
}  

billboarder.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  
  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  (billboarder.bar.defset %<==>% config) %>% 
    verifyConfig(plotter = 'billboarder') %>% 
    verifyConfigDimProperties(dims = 'color') -> config
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  obj %<>% prepare4Plot(A, config = config)
  
  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}
  
  # apply series colors:
  Ly      = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  clrlist = getColorList(Ly, L$color, config)
  
  if(!is.null(L$group)){
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    bb = billboarder(data = obj) %>% 
      bb_aes_string(x = Lx, y = Ly, group = L$group) %>% bb_barchart(rotate = hor, ...)
  } else {
    if(length(Ly) > 1){
      bb = billboarder() %>% bb_barchart(data = obj[, c(Lx, Ly)], color = L$color, stacked = config$stack.enabled, rotate = hor, ...)
    } else {
      bb = billboarder(data = obj) %>% 
        bb_aes_string(x = Lx, y = Ly) %>% bb_barchart(rotate = hor, color = L$color[1], ...)
    }
  }
  bb %>% billboarder.applyColors(clrlist) %>% billboarder.applyConfig(config)
}

billboarder.scatter = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  
  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  billboarder() %>% 
    bb_scatterplot(data = obj, x = L$x, y = L$y, group = L$group) %>% billboarder.applyConfig(config)
  
}

billboarder.pie = function(obj, label = NULL, theta = NULL, config = NULL, ...){
  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% verifyConfig(plotter = 'billboarder')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics
  
  if ('rownames' %in% L$label){obj %<>% rownames2Column('rownames') %>% as.data.frame}
  
  obj %<>% prepare4Plot(A, config = config)
  
  # Verifications:
  # if (is.empty(obj)){return(NULL)}
  
  billboarder() %>% 
    bb_piechart(data = obj) %>% 
    bb_labs(title   = config$title,
            caption = config$subtitle)
}

billboarder.tsLineChart = function(obj, x = NULL, y = NULL, color = NULL, shape = 'spline', config = NULL, ...){
  
  assert(require('billboarder'), "Package 'billboarder' is not installed!", err_src = match.call()[[1]])
  config = billboarder.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'billboarder') %>% 
    verifyConfigDimProperties(dims = 'color') 
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  if ('rownames' %in% L$y){obj %<>% rownames2Column('rownames') %>% as.data.frame}
  
  if(is.null(config$stack.groups)){config$stack.groups <- list(as.list(L$y))}
  
  obj %<>% prepare4Plot(A, config = config)
  
  # apply series colors:
  if(!is.null(L$color)){clrlist = L$color %>% vect.extend(length(L$y)) %>% as.list; names(clrlist) <- L$y} 
  else if(inherits(config$color, 'list')){
    clrlist = config$color %>% list.extract(L$y)
  }
  else {clrlist = list()}
  
  billboarder() %>% bb_linechart(data = obj, type = shape) %>% 
    billboarder.applyColors(clrlist) %>% 
    billboarder.applyConfig(config)
}

billboarder.tsline = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  obj %>% billboarder.tsLineChart(x = x, y = y, color = color, config = config, ...)  
}

billboarder.tsarea = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  obj %>% billboarder.tsLineChart(x = x, y = y, color = color, shape = 'area', config = config, ...)
}


# bpexploder.R ----------------------------------------------------------------


# Header
# Filename:       bpexploder.R
# Description:    Contains functions for plotting interactive box plots from bpexploder javascript package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     23 May 2018
# Last Revision:  18 July 2018
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     23 May 2018         Initial issue
# 0.0.2     18 July 2018        bpexploder.box.molten() renamed to bpexploder.box: Changed argument group to x

# this is a molten chart:
bpexploder.box.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    y = c('numeric','integer'),
    x = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = c( 'y', 'x'),
  palette = list(color = NULL)
)

bpexploder.box = function(obj, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  # if (is.empty(obj)){return(NULL)}
  assert(require(bpexploder), "Package bpexploder is not installed!", err_src = match.call()[[1]])
  config = bpexploder.box.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  config$feedAdditionalColumns %<>% c(config$point.tooltip %>% names)
  
  # Preparing Aesthetics:
  a = prepareAesthetics(y = y, x = x)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  
  bpexploder(data = obj,
             settings = list(
               groupVar = L$x,
               levels   = levels(obj[, L$x]),
               yVar     = L$y,
               tipText = config$point.tooltip %>% verify('list', names_domain = names(obj), default = list()),
               relativeWidth = config$point.size %>% verify('numeric', domain = c(0,1), default = 0.75))
  )
}



# bubbleCloud.R ----------------------------------------------------------------


# Header
# Filename:       bubbleCloud.R
# Description:    Contains functions for plotting various bubble charts from js package 'bubbleForce' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     21 June 2018
# Last Revision:  21 June 2018
# Version:        0.1.0
#
# Version   Date               Action
# -----------------------------------
# 0.1.0     21 June 2018       Initial issue with function bubbleCloud.bubble.molten()

# Default settings for package bubbleCloud:
bubbleCloud.bubble.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    size    = valid.numeric.classes,
    group   = valid.classes,
    tooltip = 'character'),
  multiples = c(),
  essentials = c('size')
)

bubbleCloud.bubble.molten <- function(obj, size = NULL, group = NULL, tooltip = NULL, config = bubbleCloud.bubble.molten.defset, ...){
  if (is.empty(obj)){return(NULL)}
  
  # Verifications:
  assert(require(bubbleCloud), "Package bubbleCloud is not installed!", err_src = match.call()[[1]])
  config = bubbleCloud.bubble.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, group = group, tooltip = tooltip)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if (is.null(obj$hover)){obj$hover <- ''}
  
  obj$x <- runif(nrow(obj),1,400)
  obj$y <- runif(nrow(obj),1,400)
  
  if (is.null(obj$radius)){
    obj$radius <- 5
  }
  
  if (is.null(obj$group)){
    obj$clusterName <- ""
    obj$cluster <- 0
  } else{
    obj$cluster <- as.numeric(factor(obj$group)) - 1
    obj$clusterName <- obj$group
  }
  
  clusters <- ddply(obj, .(cluster), function(r){
    r <- r[with(r, order(radius)),]
    head(r,1)
  })
  
  n <- nrow(obj)
  m <- nrow(clusters)
  
  x = list(
    n = n,
    m = m,
    data = obj,
    clusters = clusters,
    settings = config
  )
  
  # create widget
  htmlwidgets::createWidget(
    name = "bubbleforce",
    x,
    width = width,
    height = height,
    package = 'niravis',
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      browser.fill = TRUE
    )
  )
}


bubbleCloudOutput <- function(outputId, width = '100%', height = '500px'){
  shinyWidgetOutput(outputId, 'bubbleforce', width, height, package = 'niravis')
}

renderBubbleCloud <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, bubbleCloudOutput, env, quoted = TRUE)
}




# bubbles.R ----------------------------------------------------------------


# Header
# Filename:       bubbles.R
# Description:    Contains functions for plotting various bubble charts from package 'bubbles' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     18 October 2016
# Last Revision:  31 March 2017
# Version:        1.1.2
#
# Version   Date               Action
# -----------------------------------
# 1.0.0     18 October 2016    Initial issue
# 1.0.1     14 March 2017      Modifications in bubbles.bubble(): Calls verifyPlotInputs() to change the table
# 1.0.2     21 March 2017      Modifications in bubbles.bubble(): Color Preparations eliminated. Will be done by verifyplotInputs() from visgen.R
# 1.1.0     27 March 2017      Major modifications in bubbles.bubble():
#                              Uses function prepare4Plot()
#                              all non-aesthetic arguments comes from config now.
# 1.1.2     31 March 2017      Calls prepareAusthetics() from visgen.R

# Shiny Inputs:
# chartID_click

# Default settings for package bubbles:
bubbles.defset = defset %>% list.edit(
  # Percentage threshold for showing the label in the bubble
  point.label.threshold = 0.0,
  # Valid classes for all dimensions
  dimclass = list(size  = valid.numeric.classes,
                  color = valid.classes,
                  label = valid.nominal.classes,
                  tooltip = 'character',
                  labelColor = valid.classes)
)

bubbles.defset$palette %<>% list.edit(
  labelColor = c("#FB1108", "#FA7806","#FBE426","#FCFB8F", "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1")
)


bubbles.bubble <- function(obj, size = NULL, color = NULL, label = NULL, labelColor = NULL, tooltip = NULL, config = bubbles.defset, ...){
  
  if (is.empty(obj)){return(NULL)}
  
  # Verifications:
  assert(require(bubbles), "Package bubbles is not installed!", err_src = match.call()[[1]])
  config = bubbles.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, color = color, label = label, labelColor = labelColor, tooltip = tooltip)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if ((!is.null(L$label)) & (!is.null(L$size))){
    if (!is.null(config$point.label.threshold)){
      obj[, L$label] %<>% as.character
      obj[which(vect.normalise(obj[, L$size]) < config$point.label.threshold), L$label] <- ''
    }
  }
  
  bubbles(value = obj[, L$size], label = obj[, L$label],
          color     = chif(is.null(color)     , "#EEEEEE", obj[, L$color]) ,
          key       = rownames(obj),
          tooltip   = chif(is.null(tooltip)   , "", obj[, L$tooltip]),
          textColor = chif(is.null(labelColor), "#333333", obj[, L$labelColor]), ...)
}




# c3.R ----------------------------------------------------------------



# Header
# Filename:       c3.R
# Description:    Contains functions for plotting various charts from package 'c3' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     14 April 2017
# Last Revision:  17 September 2018
# Version:        0.1.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue
# 0.0.2     05 March 2017      Function c3.combo() modified: extends vector shape to the count of series, function c3.bar() added
# 0.0.3     19 June 2018       Function c3.applyConfig() modified: Does not apply xAxis.label and yAxis labels if NULL 
# 0.0.4     19 June 2018       Function c3.gauge() added
# 0.0.5     19 July 2018       Function c3.tsline() added
# 0.0.6     24 July 2018       Function c3.bar() is re-written: needed to call c3_bar because combo does not accept argument 'rotated'
# 0.0.9     24 July 2018       Functions c3.line() and c3.area() and c3.tsarea() added
# 0.1.0     17 September 2018  Functions c3.tsarea() modified

c3.combo.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'Date'),
    y       = 'numeric',
    shape   = 'character'),
  multiples  = c('y', 'shape'),
  essentials = 'y'
)

# don't use integer for c3!
c3.bar.defset = defset %>% list.edit(
  dimclass   = list(
    x     = c('numeric', 'character', 'factor'),
    y     = c('numeric', 'character', 'factor'),
    group = c('factor', 'character')),
  multiples  = c('x', 'y'),
  essentials = c('y')
)

c3.tsline.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'Date', # POSIXct does not work. Convert to character
    y       = 'numeric',
    shape   = 'character'),
  multiples  = c('y', 'shape'),
  essentials = c('x', 'y')
)

c3.pie.defset = defset %>% list.edit(
  dimclass   = list(
    label   = 'character',
    theta   = 'numeric'),
  multiples  = c(),
  essentials = c('label', 'theta')
)


c3.scatter.molten.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    group   = valid.nominal.classes),
  multiples = c(),
  essentials = c('x', 'y')
)

c3.applyConfig = function(chart, config){
  if(!is.null(config$xAxis.label)){chart %<>% xAxis(label = config$xAxis.label)}
  if(!is.null(config$yAxis.label)){chart %<>% yAxis(label = config$yAxis.label)}
  return(chart)
}

c3.applyColors <- function(ct, colorvect) {
  ct$x$color$pattern <- colorvect
  # You can change all other properties here
  # reference: https://c3js.org/reference.html
  return(ct)
}

c3.combo = function(obj, x = NULL, y = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])
  
  config = c3.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'c3')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, extend = c('y', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config)
  
  # assert(length(L$y) > 1, "THE MOST STUPID PACKAGE I HAVE SEEN!!")
  
  if(obj[, L$x] %>% duplicated %>% sum > 0 & !is.null(config$aggregator)){
    # if(inherits(config$aggregator, 'function')){config$aggregator = as.character(substitute(config$aggregator))}
    config$aggregator %>% verify('character')
    obj %<>% dplyr::group_by_(L$x)
    scr = "obj %>% dplyr::summarise("
    N   = length(L$y)
    for (i in sequence(N)){
      scr %<>% paste0("`", L$y[i], "` = ", config$aggregator, "(`", L$y[i], "`)", chif(i < N, ", ", ")"))
    }
    obj = parse(text = scr) %>% eval %>% as.data.frame
  } 
  
  if (!is.null(L$x)){
    if (inherits(obj[,L$x], 'Date')){ct = obj %>% c3(x = L$x)} else {
      ct = obj %>% c3 %>% xAxis(type = 'category', categories = obj[, L$x] %>% as.character)
    }
  } else {ct = obj %>% c3}
  if(!is.null(L$shape)){names(L$shape) <- L$y}
  if(is.null(L$shape)){
    if(!is.null(config$shape)){
      L$shape = config$shape  
    } else {L$shape = 'line'}
  }
  
  L$shape %<>% vect.extend(length(L$y))
  if(is.empty(config$stack.groups)){config$stack.groups = L$y}
  options(warn = -1)
  ct %<>% c3_mixedGeom(type = most.common(L$shape), types = L$shape %>% as.list, stacked = chif(config$stack.enabled, config$stack.groups, NULL))
  options(warn = 1)
  
  return(ct %>% c3.applyConfig(config))
}

c3.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% c3.combo(x = x, y = y, shape = 'line', config = config, ...)
}

c3.tsarea = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'c3')
  
  obj %>% c3.combo(x = x, y = y, shape = 'area', config = config, ...)
}

c3.area = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'c3')
  
  obj %>% c3.combo(x = x, y = y, shape = 'area', config = config, ...)
}

c3.line = function(obj, x = NULL, y = NULL, config = NULL, ...){
  config = c3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'c3')
  
  obj %>% c3.combo(x = x, y = y, shape = 'line', config = config, ...)
}

c3.scatter.molten = function(obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])
  
  config = c3.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config)
  
  ct = obj %>% c3(x = L$x, y = L$y, group = L$group)
  
  if(is.null(L$shape)){L$shape = 'point'}
  if(L$shape == 'point'){ct %<>% c3_scatter} else if (L$shape == 'bar') {ct %<>% c3_bar}
  return(ct %>% c3.applyConfig(config))
}

c3.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  
  assert(require('c3'), "Package 'c3' is not installed!", err_src = match.call()[[1]])
  (c3.bar.defset %<==>% config) %>% 
    verifyConfig(plotter = 'c3') %>% 
    verifyConfigDimProperties(dims = 'color') -> config
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  obj %<>% prepare4Plot(A, config = config)
  
  hor = isHorizontal(obj, L$x, L$y)
  Ly = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  
  getColorVect(Ly, L$color, config)
  
  obj[, c(Lx, Ly)] %>% c3 %<>% c3_bar(stacked = config$stack.enabled, rotated = hor, bar_width = 0.6, zerobased = TRUE) %>% 
    xAxis(type = 'category', categories = obj[, Lx] %>% as.character) %>% 
    c3.applyConfig(config) %>% c3.applyColors(clrvect)
}

c3.pie = function(obj, theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(c3), "Package c3 is not installed!", err_src = match.call()[[1]])
  
  config = c3.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'c3')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  df = t(obj[, L$theta]) %>% as.data.frame
  colnames(df) <-   obj[, L$label]
  df %>% c3 %>% c3_donut(title = config$title)
}

c3.gauge = function(theta, config = NULL, ...){
  theta %>% verify('numeric') %>% data.frame %>% c3 %>% c3_gauge
}


# calheatmap.R ----------------------------------------------------------------



# Header
# Filename:       calheatmap.R
# Description:    Contains functions for plotting calendar charts from js package 'calheatmap' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     30 June 2018
# Last Revision:  30 June 2018
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     30 June 2018       Initial issue

calheatmap.calendar.defset = defset %>% list.edit(
  dimclass   = list(
    t        = 'Date',
    color    = 'numeric'),
  multiples  = c(),
  essentials = 'y'
)

# look at here for configurations:
# http://cal-heatmap.com/
# todo: provide config settings

calheatmap.calendar = function(obj, t = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rChartsCalmap), "Package rChartsCalmap is not installed!", err_src = match.call()[[1]])
  
  config = calheatmap.calendar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'calheatmap')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(t = t, color = color)
  L = a$labels
  A = a$aesthetics
  
  config$colorize = F 
  
  obj %<>% prepare4Plot(A, config)
  
  calheatmap(x = L$t, y = L$color,
             data = obj, 
             domain = 'month',
             start = min(obj[, L$t], na.rm = T),
             ...
  )
}



# candela.R ----------------------------------------------------------------


# Header
# Filename:       candela.R
# Description:    Contains functions for plotting various dimple charts from rCharts package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     13 February 2018
# Last Revision:  19 July 2018
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     13 February 2018    Initial issue
# 0.0.2     19 July 2018        bar.molten renamed to bar

candela.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor'),
    y     = c('numeric','integer'),
    color = c('numeric','integer', 'character', 'factor')),
  multiples = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

candela.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric','integer'),
    y     = c('numeric','integer'),
    color = c('numeric','integer', 'character', 'factor'),
    shape = c('character', 'factor'),
    size  = c('numeric','integer')),
  essentials = c('x', 'y'),
  colorize   = F
)

candela.type = c(numeric = 'quantitative', integer = 'quantitative', character = 'nominal', factor = 'nominal')

# color is a grouping dimension, so it is a molten chart:
candela.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package candela is not installed!")
  config = candela.bar.defset %<==>% (config %>% verify('list', default = list()))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color) #, extend = 'y')
  L = a$labels
  A = a$aesthetics %>% list.remove(color)
  
  obj %<>% prepare4Plot(A, config = config)
  
  # todo: check for other possible values for this argument
  
  candela('BarChart', data = obj, x = L$x, y = L$y, color = L$color,
          xType = candela.type[class(obj[, L$x])[1]] %>% unname,
          yType = candela.type[class(obj[, L$y])[1]] %>% unname,
          colorType = candela.type[class(obj[, L$color])[1]] %>% unname,
          aggregate = config$aggregator.function.string %>% verify('character', domain = c('value', 'mean', 'sum'), default = 'value')
  )
  # Todos:
  # How to change xLabel and yLabel?
  # add other charts: line, ...
  
}

candela.scatter = function(obj, x = NULL, y = NULL, shape = NULL, size = NULL, color = NULL, config = NULL, ...){
  if (shape == 'bar'){return(candela.bar(x = x, y = y, color = color, config = config))}
  
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(candela), "Package 'candela' is not installed!")
  config = candela.scatter.defset %<==>% (config %>% verify('list', default = list()))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  # todo: check for other possible values for this argument
  # todo: add extra columns to the object
  
  candela('ScatterPlot', data = obj, x = L$x, y = L$y, color = L$color, size = L$size, shape = L$shape,
          xType = candela.type[class(obj[, L$x])[1]] %>% unname,
          yType = candela.type[class(obj[, L$y])[1]] %>% unname,
          sizeType = candela.type[class(obj[, L$size])[1]] %>% unname,
          shapeType = candela.type[class(obj[, L$shape])[1]] %>% unname,
          colorType = candela.type[class(obj[, L$color])[1]] %>% unname)
  
}





# coffeewheel.R ----------------------------------------------------------------



# Header
# Filename:       coffeewheel.R
# Description:    Contains functions for plotting pie charts in form of zoomable sunbursts using CoffeeWheel package from D3.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     14 April 2017
# Last Revision:  29 August 2017
# Version:        0.0.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue
# 0.0.2     20 July 2017       widget embedded in the package
# 0.0.3     29 August 2017     config$tree.function added to apply user's desired function to nira tree elements: leafs and branches

#' @include visgen.R

coffeewheel.sunburst.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta   = 'numeric',
    label   = c('character', 'factor'),
    color   = valid.classes),
  multiples  = 'label',
  essentials = c('theta', 'label'),
  aggregator.function.string = 'sum',
  labelTheta = T
)

tree2cwTree = function(tr, attributes = c('colour', 'value')){
  cwt = list()
  for (e in tr){
    if(inherits(e, gndcd(160,152,6,80))){
      nwitm = list(name = e$tree_name)
      for (attr in attributes){nwitm[[attr]] = e[[attr]]}
      nwitm$children = tree2cwTree(e, attributes)
      cwt %<>% list.add(nwitm)
    } else if (inherits(e, 'list')){
      nwitm = list(name = e$leaf_name)
      for (attr in attributes){nwitm[[attr]] = e[[attr]]}
      cwt %<>% list.add(nwitm)
    }
  }
  return(cwt)
}

coffeewheel.prepareConfig = function(config){
  config$title  %<>% verify('character', default = '', varname = 'config$title')
  config$width  %<>% verify(c('integer', 'numeric'), default = 1200, varname = 'config$width') %>% as.integer
  config$height %<>% verify(c('integer', 'numeric'), default = 800, varname = 'config$height') %>% as.integer
  
  return(config)
}

coffeewheel <- function(treeData, width=400, height=400, main="", partitionAttribute="value") {
  x <- list(
    treeData = treeData,
    main = main,
    partitionAttribute = partitionAttribute
  );
  
  # create widget
  htmlwidgets::createWidget(
    name = 'coffeewheel',
    x,
    width = width,
    height = height,
    package = 'niravis'
  );
}

## Widget output function for use in Shiny
coffeewheelOutput <- function(outputId, width= 400, height= 400) {
  shinyWidgetOutput(outputId, 'coffeewheel', width, height, package = 'niravis');
}


## Widget render function for use in Shiny
rendercoffeewheel <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, coffeewheelOutput, env, quoted = TRUE);
}

coffeewheel.sunburst = function(obj, theta = NULL, label = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  # assert(require(coffeewheel), "Package highcharter is not installed!", err_src = match.call()[[1]])
  
  config = coffeewheel.sunburst.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  if (is.null(color)){
    color = list(colour = theta[[1]])
  } else {
    names(color) = 'colour'
    color %<>% as.list
  }
  
  
  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj       %<>% prepare4Plot(A, config)
  config    %<>% coffeewheel.prepareConfig
  cwt = obj %>% df2tree(id_cols = L$label, var_cols = c(L$theta, L$color), func = c(config$aggregator.function.string, 'color.mean'), name = 'ROOT')
  if(!is.null(config$tree.function)){cwt %<>% treeApply(func = config$tree.function)}
  
  cwt %>% tree2cwTree(attributes = c(L$theta, L$color)) %>%
    coffeewheel(width = config$width, height = config$height, main = config$title, partitionAttribute = L$theta)
}


# csscripts.R -------------------------------------------------------------------


# Header
# Filename:       csscripts.R
# Description:    Contains functions generating various R scripts.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     22 November 2017
# Last Revision:  29 November 2017
# Version:        0.0.2
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     22 November 2017    Initial issue
# 0.0.2     29 November 2017    Function style.css() added and exported


valid.font.weights  = c('normal', 'bold', 'bolder', 'lighter')
serif.fonts         = c('georgia', 'times', 'times new roman', 'palatino linotype', 'book antiqua', 'palatino')
sans_serif.fonts    = c('arial', 'arial black', 'verdana', 'geneva', 'trebuchet ms', 'helvetica', 'tahoma', 'geneva', 'lucida sans unicode', 'lucida grande', 'impact', 'charcoal', 'comic sans ms', 'cursive')
monospace.fonts     = c('courier', 'courier new', 'lucida console', 'monaco', 'monospace')
valid.fonts         = c(serif.fonts, sans_serif.fonts, monospace.fonts)
font.generic        = c(rep('serif', length(serif.fonts)), rep('sans-serif', length(sans_serif.fonts)), rep('monospace', length(monospace.fonts)))
names(font.generic) = valid.fonts

main.header.style.css = function(font = NULL, weight = NULL, size = NULL, color, background, hover.color, hover.background){
  font %<>% verify('character', default = 'times new roman', varname = 'font', err_src = 'main.header.font.css') %>%
    tolower %>% verify(domain = valid.fonts)
  
  weight %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 'normal') %>% as.character
  size   %<>% verify(c('character', 'factor', 'integer', 'numeric'), default = 24) %>% as.character
  
  gnr = font.generic[font[1]]
  if(is.na(gnr)){gnr = 'sans-serif'}
  tags$head(tags$style(HTML('
                            .main-header .logo {
                            font-family: ' %++% paste('\"', font, '\"', collapse = ', ') %++% gnr %++% ';
                            font-weight: ' %++% weight %++% ';
                            font-size: '%++% size %++% 'px;
                            }
                            ')))
}

skin.style.css = function(skin = 'blue', font = NULL, font.weight = NULL, font.size = 'NULL', color = NULL, background.color = NULL, hover.font = NULL, hover.font.weight = NULL, hover.font.size = 'NULL', hover.color = NULL, hover.background.color = NULL){
  color %>% verify('character') %>% col2Hex
  background.color %>% verify('character') %>% col2Hex
  scr = '.skin-' %++% skin %++% ' .main-header .logo {'
  if(!is.null(color)){scr %<>% paste0('color: ', color, '; \n')}
  if(!is.null(background.color)){scr %<>% paste0('background-color: ', background.color, '; \n')}
  
  if(!is.empty(font)){
    font %<>% verify('character') %>% tolower
    gnr = font.generic[font[1]]
    if(is.na(gnr)){gnr = 'sans-serif'}
    fontscr = paste('\"', font, '\"', collapse = ', ') %++% ', '  %++% gnr
    scr %<>% paste0('font-family: ', fontscr, '; \n')
  }
  
  if(!is.empty(font.weight)){
    font.weight %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-weight: ', font.weight, '; \n')
  }
  
  if(!is.empty(font.size)){
    font.size %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-size: ', font.size, '; \n')
  }
  
  scr %<>% paste0('} \n')
  
  hover.color %>% verify('character') %>% col2Hex
  hover.background.color %>% verify('character') %>% col2Hex
  scr %<>% paste0('.skin-blue .main-header .logo:hover {')
  if(!is.null(hover.color)){scr %<>% paste0('color: ', hover.color, '; \n')}
  if(!is.null(hover.background.color)){scr %<>% paste0('background-color: ', hover.background.color, '; \n')}
  
  if(!is.empty(hover.font)){
    hover.font %<>% verify('character') %>% tolower
    gnr = font.generic[hover.font[1]]
    if(is.na(gnr)){gnr = 'sans-serif'}
    font.scr = paste('\"', hover.font, '\"', collapse = ', ')  %++% ', ' %++% gnr
    scr %<>% paste0('font-family: ', fontscr, '; \n')
  }
  
  if(!is.empty(hover.font.weight)){
    hover.font.weight %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-weight: ', hover.font.weight, '; \n')
  }
  
  if(!is.empty(hover.font.size)){
    hover.font.size %<>% verify(c('character', 'factor', 'integer', 'numeric')) %>% as.character
    scr %<>% paste0('font-size: ', hover.font.size, '; \n')
  }
  scr %<>% paste0('} \n')
  
  return(scr)
}

# Simple function: needs to be modified later
#' @export
style.css = function(...){
  arglist = list(...)
  arglist %>% names %>% paste0(': ', arglist %>% unlist %>% as.character) %>% paste(collapse = '; ')
}



# d3plus.R ----------------------------------------------------------------


# Header
# Filename:       d3plus.R
# Description:    Contains functions for plotting various charts from package 'd3plus' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     28 March 2017
# Last Revision:  25 July 2018
# Version:        1.1.6
#
# Version   Date               Action
# -----------------------------------
# 1.0.0     26 October 2016    Initial issue by adding d3plus.defset
# 1.1.0     13 May 2017        Function d3plus.bubble() modified: Accommodating modifications in visgen
# 1.1.1     13 May 2017        Function d3plus.bubble.moten() added
# 1.1.2     13 May 2017        Function d3plus.tree() added
# 1.1.5     01 July 2018       Functions d3plus.bar.molten(), d3plus.tsline.molten() and d3plus.tsbar.molten() added using new htmlwidget: D3plusR
# 1.1.6     25 July 2018       Function d3plus.bar.molten() converted to d3plus.bar() plots both wide and molten

d3plus.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(x = 'numeric',
                   y = 'numeric',
                   size = 'numeric',
                   label = 'character')
)

d3plus.bubble.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    size = 'numeric',
    label = 'character'),
  multiples  = c('size', 'label'),
  essentials = c('size', 'label')
)

d3plus.bubble.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    size  = 'numeric',
    label = 'character',
    group = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = c('size', 'label')
)

d3plus.bar.defset = defset %>% list.edit(
  dimclass  = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric', 'integr', 'character', 'factor'),
    group = 'factor'),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

d3plus.bar.molten.defset = defset %>% list.edit(
  dimclass  = list(
    x     = 'character',
    y     = 'numeric',
    group = 'factor'),
  multiples  = c(),
  essentials = c('x', 'y', 'group'),
  shape = 'bar'
)
d3plus.line.molten.defset = d3plus.bar.molten.defset %>% list.edit(shape = 'line')
d3plus.area.molten.defset = d3plus.bar.molten.defset %>% list.edit(shape = 'area')

d3plus.tree.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    label = 'character',
    size  = 'numeric',
    color = valid.classes),
  multiples  = c(),
  essentials = c('size', 'label'),
  colorize = F
)

d3plus.tsline.molten.defset = defset %>% list.edit(
  dimclass  = list(
    x     = 'Date',
    y     = 'numeric',
    group = 'factor'),
  multiples  = c(),
  essentials = c('x', 'y', 'group'),
  shape = 'line'
)

d3plus.tsline.defset = defset %>% list.edit(
  dimclass  = list(
    x     = 'Date',
    y     = 'numeric',
    group = 'factor'),
  multiples  = 'y',
  essentials = c('x', 'y'),
  shape = 'line'
)

d3plus.tsbar.molten.defset  = d3plus.tsline.molten.defset %>% list.edit(shape = 'bar')
d3plus.tsarea.molten.defset = d3plus.tsline.molten.defset %>% list.edit(shape = 'area')

d3plus.treemap.defset = defset %>% list.edit(
  dimclass  = list(
    label = 'character',
    size  = 'numeric',
    color = c('character', 'numeric')),
  multiples  = 'label',
  essentials = c('label', 'size')
)


d3plus.shape = c(bar = 'bar', line = 'line', area = 'stacked')
# Check this function sholud not be working!!!
d3plus.scatter = function(obj, x = NULL, y = NULL, size = NULL, label = NULL, config = NULL, ...){
  
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  x %<>% nameList('x')
  y %<>% nameList('y')
  size %<>% nameList('size')
  label %<>% nameList('label')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, label = label, config = config)
  L = a$labels
  A = a$aesthetics
  
  # xL         = names(x)
  # yL         = names(y)
  # sizeL      = names(size)
  
  obj %<>% prepare4Plot(A, config = config)
  
  d3plus('scatter', obj, ...)
}

d3plus.bubble = function(obj, size = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.bubble.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(size = size, label = label, extend = c('size', 'label'))
  L = a$labels
  A = a$aesthetics
  
  # Function d3plus with type 'bubbles' uses melted table for multiple series of bubbles
  # This is what they called as Grouped bubbles. The series name comes in a column named as group between label and size columns
  obj %<>% prepare4Plot(A, config = config) %>%
    melt(id.vars = unique(L$label))
  
  names(L$label) <- L$size
  obj = cbind(name = obj[, L$label[obj$variable]] %>% as.matrix %>% diag, obj[, c('variable', 'value')]) %>% na.omit
  
  d3plus('bubbles', obj, ...)
}

d3plus.bubble.molten = function(obj, size = NULL, label = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.bubble.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  if(is.null(group)){group = 1}
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, group = group, size = size)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  d3plus('bubbles', obj, ...)
}

d3plus.tree = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(d3plus), "Package d3plus is not installed!", err_src = match.call()[[1]])
  config = d3plus.tree.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  d3plus('tree', obj, ...)
}

d3plus.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'd3plus')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  obj %<>% prepare4Plot(A, config = config)
  hor = isHorizontal(obj, L$x, L$y)
  Sx  = chif(hor, T, NULL) ; Sy = chif(hor, NULL, T)
  Ly  = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  if(!is.null(L$group)){
    clrlist = getColorList(obj[, L$group] %>% as.character %>% unique, L$color, config)
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    d3 = d3plus(data = obj, id = L$group, type = 'bar') %>% 
      d3plusX(value = L$x, scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>% 
      d3plusY(value = L$y, scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
    Lgroup = L$group
  } else {
    if(length(Ly) > 1){
      d3 = d3plus(data = obj %>% reshape2::melt(id.vars = Lx, measure.vars = Ly), id = 'variable', type = 'bar') %>% 
        d3plusX(value = chif(hor, 'value', L$x), scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>% 
        d3plusY(value = chif(hor, L$y, 'value'), scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
      Lgroup = 'variable'
    } else {
      d3 = d3plus(data = obj, type = 'bar') %>% 
        d3plusX(value = L$x, scale = chif(hor, 'linear', 'discrete'), stacked = Sx) %>% 
        d3plusY(value = L$y, scale = chif(hor, 'discrete', 'linear'), stacked = Sy)
      Lgroup = Ly
    }
    clrlist = getColorList(Ly, L$color, config)
  }
  attributes = clrlist %>% 
    as.data.frame %>% t %>% rownames2Column(Lgroup) %>% as.data.frame %>% list
  names(attributes) <- Lgroup
  
  d3 %>% d3plusAttrs(value = attributes) %>% d3plusColor(value = "V1") %>% 
    d3plus.applyConfig(config)
}

d3plus.applyConfig = function(chart, config){
  chart %>% d3plusLegend(value = config$legend.enabled, size = config$legend.size, data = config$legend.tooltip.enabled) %>% 
    d3plusTooltip(value = config$tooltip) %>% d3plusTitle(config$title)
}

# to be removed
d3plus.bar.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.bar.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'd3plus')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  attributes = getColorList(obj[, L$group] %>% unique, L$color, config) %>% 
    as.data.frame %>% t %>% rownames2Column(L$group) %>% as.data.frame %>% list
  names(attributes) <- L$group
  
  d3plus(data = obj, id = L$group, type = 'bar') %>% 
    d3plusX(value = L$x) %>% 
    d3plusY(value = L$y) %>% 
    d3plusAttrs(value = attributes) %>% 
    d3plusColor(value = "V1") %>% 
    d3plus.applyConfig(config)
}

d3plus.line.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.line.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.bar.molten(x = NULL, y = NULL, group = NULL, config = NULL, ...)
}

d3plus.area.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.area.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.bar.molten(x = NULL, y = NULL, group = NULL, config = NULL, ...)
}

d3plus.tsline = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'd3plus')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(length(L$y) > 1){
    obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y) %>% 
      d3plus.tsline.molten(x = L$x, y = 'value', group = 'variable', config = config, ...)
  } else {
    obj %>% d3plus.tsline.molten(x = L$x, y = L$y, config = config, ...)
  }
  
}

d3plus.tsline.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.tsline.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  L = list(x = x, y = y, group = group)
  obj[, L$x]  %<>% format("%Y/%m/%d")
  
  date_filter = obj[, L$x]
  if(!is.null(config$xAxis.min)){date_filter = date_filter %^% obj[, L$x][obj[, L$x] >= config$xAxis.min %>% verify('Date') %>% format("%Y/%m/%d")]}
  if(!is.null(config$xAxis.max)){date_filter = date_filter %^% obj[, L$x][obj[, L$x] <= config$xAxis.max %>% verify('Date') %>% format("%Y/%m/%d")]}
  
  labelFormat = config$label.format %>% unlist
  
  d3plus(data = obj, id = L$group,
         type         = d3plus.shape[config$shape] %>% unname,
         percent_var  = names(labelFormat)[which(labelFormat == 'percentage')],
         currency     = config$currency,
         currency_var = names(labelFormat)[which(labelFormat == 'currency')],
         height = config$height,
         width  = config$width) %>% 
    d3plusX(value = L$x, grid = config$xAxis.grid.enabled) %>% 
    d3plusY(value = L$y, grid = config$yAxis.grid.enabled) %>% 
    d3plusTime(value = L$x, solo = date_filter ) %>% 
    d3plusTooltip(value = config$tooltip) %>% 
    d3plusTitle(config$title)
}

d3plus.tsbar.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsbar.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.tsline.molten(x = x, y = y, group = group, config = config, ...)
}

d3plus.tsarea.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  config = d3plus.tsarea.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  obj %>% d3plus.tsline.molten(x = x, y = y, group = group, config = config, ...)
}

d3plus.treemap = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(D3plusR), "Package D3plusR is not installed!", err_src = match.call()[[1]])
  config = d3plus.treemap.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'd3plus')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  d3p = d3plus(data = obj,
               type   = "tree_map",
               id     = L$label,
               width  = config$width,
               height = config$height) %>% 
    d3plusSize(value = L$size) %>% 
    d3plusLegend(value = config$legend.enabled, order = list(sort = "desc", value = "size"))
  if(!is.null(L$color)){d3p %<>% d3plusColor(L$color)}
  
  d3p %>% d3plusDepth(0) %>% 
    d3plusLabels(value = TRUE, valign = "top")
}

# dashboard.R ---------------------------------------------------------------- 



# Header
# Filename:      dashboard.R
# Description:   This project, aims to create a ref class containing multiple objects
#                that can issue a shiny dashboard
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    22 January 2016
# Last Revision: 06 November 2018
# Version:       2.5.6

# Version History:

# Version   Date                Action
# ---------------------------------------
# 0.6.0     04 July 2016        A number of changes made. Refer to dashboard.R (version 0.6)

# Changes from version 0.5:
# 1- method io.str() transferred from dash.tools.R
# 2- Starting letters of input and cloth types changed to lowercase to match shiny namespace: Example: radioButtons changed to radioButtons
# 3- Valid Container types separated from valid input types
# 4- class properties 'inputs' and 'outputs' consolidated to one single list called 'items' which must be a named list
# 5- layouts must be specified by item names not numbers
# 6- field ID removed and replaced by item name

# 0.6.2     08 September 2016   Argument inline added to checkboxGroupInput.
# 0.6.3     12 October 2016     Wordcloud2 plots added.
# 0.7.0     15 May 2017         tabPanel and many other containers can also get a list as layout
# 0.7.1     15 May 2017         textInput added
# 0.7.2     15 May 2017         TFD3Output added
# 0.7.3     15 May 2017         container tabsetPanel added
# 0.7.4     15 May 2017         tabPanel container can accept list as layout
# 0.7.4     15 May 2017         tabPanel added as cloth
# 0.7.5     15 May 2017         highcharterOutput added
# 0.7.6     15 May 2017         d3plusOutput added
# 0.7.7     15 May 2017         observers added as list (todo: named list items should become observeEvents)
# 0.7.8     15 May 2017         property 'prescript' added to the class
# 0.8.0     17 May 2017         Start adding syncing feature: property 'object' renamed to sync for synced items.
# 0.9.0     23 May 2017         syncing added for TFD3: reactive list properties 'sync' and 'report' added.
# 0.9.1     31 May 2017         morrisjsOutput added.
# 0.9.2     31 May 2017         coffeewheelOutput added.
# 0.9.3     01 June 2017        cloth column service modified: bug removed for column width and offset!
# 0.9.4     20 July 2017        support('coffeewheel') removed as the widget is embedded
# 0.9.5     25 July 2017        Added re-plotting trigger for TFD3 in sync reactives
# 0.9.6     31 July 2017        Added re-plotting trigger for TFD3 in sync reactives
# 0.9.7     03 August 2017      Default widths and heights of plot outputs made equal for all plots
# 0.9.8     30 August 2017      actionButton, accept both character and icon types for argument icon
# 1.1.0    11 October 2017     property sessions added to the object. This property is a list and keeps all the sessions
# 1.1.2    11 October 2017     Methods updateItem4Session() and updateItem() addded: Currently works only for type selectInput
# 1.1.3    13 October 2017     pivotTable added to supported plotters using package rpivotTable
# 1.1.4    16 October 2017     Method io.str.clothed() modified: cloth is no longer passed as argument. reads directly from items[[i]]$cloth
# 1.1.5    17 October 2017     rpivotTable renamed to pivot
# 2.0.0     10 November 2017    Fundamental Change to the package: Added shinyjs features: Methods enableItems(), disableItems(), showItems() and hideItems() added!
# 2.0.0     22 November 2017    CSS customized skin style for shinydashboard header added.
# 2.1.1     24 November 2017    actionLink and passwordInput item types added
# 2.1.2     27 November 2017    Simple container 'bag' added to containers.
# 2.1.3     27 November 2017    Type 'loginInput' added.
# 2.1.4     27 November 2017    Function loginVerify() added
# 2.1.5     27 November 2017    Property 'messages' added to class DASHBOARD. It is a named character vector
# 2.2.0     27 November 2017    Fundamental Change: Creating client specific dataset(objects and recative values) for each session
# 2.2.1     27 November 2017    Property 'objects' renamed to 'global', but argument 'objects' still works for method initialize(): replacing value of 'global' by given argument 'objects'
# 2.2.2     27 November 2017    Property 'values' added to class DASHBOARD. Generates and keeps initial values of reactives in 'sync'.
# 2.2.3     27 November 2017    A copy of list 'objects' is generated as initial values for non-reactive list 'local' associated with each session
# 2.2.4     27 November 2017    A copy of list 'values' is generated as initial values for reactive list 'sync' associated with each session
# 2.2.5     29 November 2017    Output item types: downloadButton and downloadLink added.
# 2.2.6     29 November 2017    Argument 'style' added to item types downloadButton and downloadLink
# 2.2.7     06 December 2017    Property 'settings' added to class DASHBOARD. settings item 'saveSession' added and specifies whether the session data needs to be saved after logout and loaded after login by each user.
#                               If settings$saveSession is TRUE, a .rds file for each user is created in the working directory after the user logs out to keep the session data (local and sync). Session data is loaded again when the user logs in.
# 2.2.8     08 December 2017    Property 'global' renamed to 'objects' again
# 2.2.9     08 December 2017    A copy of objects is created in session$userData in each session
# 2.3.0     15 December 2017    Uses encrypted password check from package: bcrypt
# 2.3.1     15 December 2017    Added property 'passEncryption' to the dashboard settings. Specifies how the password should be encrypted. Value 'none' means no encryption is required!
# 2.3.2     19 December 2017    Function loginVerify() modified: Small bug rectified by converting the encrypted password from factor to character
# 2.3.3     16 January 2018     A typo is corrected: Login versification failed converted to login verification failed.
# 2.3.4     29 January 2018     settings property 'keepSessions' added: sessions are kept in memory only if this property is TRUE
# 2.3.5     12 February 2018    settings property 'savePath' added: specify where in the server sessions should be saved
# 2.3.6     02 June 2018        Container 'bsModal' added
# 2.3.7     02 June 2018        arguments 'options' added for dataTableOutput, default for height changed to 'auto' from '400px' for all outputs
# 2.3.9     04 June 2018        Containers 'bsCollapse' and 'bsCollapsePanel' added.
# 2.4.0     04 June 2018        Output widths and heights returned to package defaults.
# 2.4.2     04 June 2018        'tooltip' and 'popover' are now properties which can be attached to each item. Thanks to package 'shinyBS'
# 2.4.3     06 June 2018        'grviz' added to outputs
# 2.4.4     08 June 2018        Arguments 'title', 'choices', 'selected' for all input items, are defined in one place
# 2.4.7     08 June 2018        Input items 'actionbttn', 'actionGroupButtons', 'airDatepickerInput' added
# 2.4.8     08 June 2018        shinyWidget spinners added to plotOutput. Can we add it to other plots as well??
# 2.4.9     15 June 2018        tutorBox added to containers
# 2.5.0     16 June 2018        Arguments 'tutor.lesson', 'tutor.hint' and 'tutor.step' can be used for to all items
# 2.5.1     22 June 2018        Method getItemObject() modified: arguments 'selected' and 'choices' are converted to integer if they are numeric.
# 2.5.2     18 September 2018   Method self.verify() modified: varname passed to function verify() changed
# 2.5.3     26 October 2018     Input item type dateRangeInput modified: All arguments added.
# 2.5.4     29 October 2018     Input item type actionButton modified: argument weight re-added 
# 2.5.5     05 November 2018    grviz is now independent
# 2.5.6     06 November 2018    Argument 'width' added for selectInput 

support('shiny', 'shinydashboard', 'htmlwidgets')

valid.box.statuses = c('primary', # Blue (sometimes dark blue),
                       'success', # Green
                       'info',    # Blue
                       'warning', # Orange
                       'danger'  # Red
)
valid.colors = c('red', 'yellow', 'aqua', 'blue', 'light-blue', 'green', 'navy', 'teal', 'olive', 'lime', 'orange',
                 'fuchsia', 'purple', 'maroon', 'black', 'navy')

valid.input.types = c("textInput", "radioButtons", "sliderInput", "actionButton", "actionBttn", "actionLink", "checkboxInput", "checkboxGroupInput", "selectInput", "dateInput", "airDatepickerInput",
                      "dateRangeInput", "fileInput", "numericInput", "passwordInput")

valid.output.types = c("uiOutput", "dynamicInput", "loginInput", "plotOutput", "verbatimTextOutput", "textOutput", "tableOutput", "dataTableOutput", "htmlOutput",
                       "gglVisChartOutput", "rChartsdPlotOutput", 'dygraphOutput', 'plotlyOutput', 'amChartsOutput',
                       "leafletOutput", "infoBoxOutput", "valueBoxOutput", "pivotOutput", "wordcloud2Output", 'bubblesOutput', 'd3plusOutput', 'plotlyOutput',
                       "highcharterOutput", "TFD3Output", "grvizOutput", "c3Output", "sunburstOutput", "sankeyNetworkOutput", "morrisjsOutput", 
                       "coffeewheelOutput", "billboarderOutput", 'sankeytreeOutput', "rHandsonTableOutput", "downloadButton", "downloadLink", "static")

valid.container.types = c("column", "box", "bag", "fluidPage", "dashboardPage", "tabsetPanel",
                          "sidebarLayout", "navbarPage", "navbarMenu", "tabPanel", "wellPanel", 
                          "bsModal", 'bsCollapse', 'bsCollapsePanel', 'actionGroupButtons', 'tutorBox')


valid.cloth.types = c("box", "infoBox", "valueBox", "column", "wellPanel", "tabPanel")

valid.navbar.positions = c("static-top", "fixed-top", "fixed-bottom")

valid.dashboard.skins  = c("blue", "black", "purple", "green", "red", "yellow")

# Returns the number of elements of the given list which are either 'list' or 'character'
nListOrCharItems = function(lst){
  sum = 0
  for (i in lst){
    if (inherits(i, 'list') | inherits(i, 'character')){sum = sum + 1}
  }
  return(sum)
}

# 1: Username and Password don't match
# 2: Username does not exist
loginVerify = function(logtable, username, password, encryption = 'none'){
  username %>% verify('character', lengths = 1, null_allowed = F)
  password %>% verify('character', lengths = 1, null_allowed = F)
  if(username %in% rownames(logtable)){
    switch(encryption,
           'none'   = {if(identical(password, logtable[username,'password'])){res = username} else {res = 1}},
           'bcrypt' = {support('bcrypt'); if(bcrypt::checkpw(password, logtable[username,'password'] %>% as.character)){res = username} else {res = 1}}
    )
  } else {return(2)}
}


#' @exportClass DASHBOARD
DASHBOARD <- setRefClass("DASHBOARD",
                         fields = list(
                           name        = "character",
                           items       = "list",
                           objects     = 'list',
                           values      = "list",
                           sessions    = 'list',
                           settings    = 'list',
                           king.layout = "list",
                           loginTable  = "data.frame",
                           prescript   = 'character',
                           observers   = "character",
                           messages    = "character"
                         ),
                         
                         methods = list(
                           # Class constructor
                           initialize = function(name = "niravis Dashboard", objects = list(), ...){
                             support('shiny')
                             callSuper(...)
                             # Field assignment:
                             name    <<- name
                             objects <<- objects
                             settings$keepSessions   <<- verify(settings$keepSessions,   'logical', domain = c(T,F), default = F, varname = 'settings$keepSessions')
                             settings$saveSession    <<- verify(settings$saveSession,    'logical', domain = c(T,F), default = T, varname = 'settings$saveSession')
                             settings$savePath       <<- verify(settings$savePath,       'character', default = '')
                             settings$passEncryption <<- verify(settings$passEncryption, 'character', default = 'none')
                             settings$tutorMode      <<- F
                             self.verify()
                             if(!('initial'      %in% names(messages))){messages['initial']       <<- ""}
                             if(!('loginFail'    %in% names(messages))){messages['loginFail']     <<- "Login verification failed! Please try again."}
                             if(!('usernameNotFound' %in% names(messages))){messages['usernameNotFound']  <<- "Username not found!"}
                             if(!('loginSuccess' %in% names(messages))){messages['loginSuccess']  <<- "Successful login verification."}
                           },
                           
                           updateItem4Session = function(item, session, ...){
                             verify(item, 'character', domain = names(items), varname = 'item', err_src = 'DASHBOARD Method updateItem4Session')
                             switch(items[[item]]$type,
                                    'selectInput' = {updateSelectInput(session, item, ...)})
                             
                           },
                           
                           # This method updates the given item for all open sessions
                           # You should have set settings property keepSessions to TRUE
                           updateItem = function(item, ...){
                             for (ssn in sessions){
                               if(!ssn$closed){
                                 updateItem4Session(item, ssn, ...)
                               }
                             }
                           },
                           
                           self.verify = function(){
                             verify(items,  'list', varname = 'items')
                             for (i in names(items)){
                               verify(items[[i]], 'list', names_include = c('type'), varname = "items['" %++% i %++% "']")
                               verify(items[[i]]$type, 'character', domain = c(valid.input.types, valid.container.types, valid.output.types), varname = "items['" %++% i %++% "']$type")
                             }
                           },
                           
                           io.str = function(i){
                             assert(i %in% names(items), "Element '" %++% i %++% "' has been mentioned but doesn't exist in the list of elements!", err_src = 'io.str')
                             if (items[[i]]$type %in% valid.output.types){
                               scr = paste0(gndcd(148,21, 9, 200, 112), "[['", i ,"']]$", gndcd(60,142,34,162,18,136))
                             } else if (items[[i]]$type %in% c(valid.input.types, valid.container.types)) {
                               scr = paste0(gndcd(37,9, 25,107,196,130,10,114,142,15,94,88,197),"('", i,"')")
                             } else {return("")}
                             return(scr)
                           },
                           
                           hideItems = function(...){
                             itms = c(...)
                             verify(itms, 'character', domain = names(items), varname = 'Given arguments', null_allowed = F)
                             for (i in itms){
                               if (items[[i]]$type == 'tabPanel'){
                                 slctr = paste0('#', items[[i]]$container, ' li a[data-value = ', items[[i]]$value,']')
                                 shinyjs::hideElement(selector = slctr)} else {shinyjs::hideElement(i)}
                             }
                           },
                           
                           showItems = function(...){
                             itms = c(...)
                             verify(itms, 'character', domain = names(items), varname = 'Given arguments', null_allowed = F)
                             for (i in itms){
                               if (items[[i]]$type == 'tabPanel'){
                                 slctr = paste0('#', items[[i]]$container, ' li a[data-value = ', items[[i]]$value,']')
                                 shinyjs::showElement(selector = slctr)} else {shinyjs::showElement(i)}
                             }
                           },
                           
                           disableItems = function(...){
                             itms = c(...)
                             verify(itms, 'character', domain = names(items), varname = 'Given arguments', null_allowed = F)
                             for (i in itms){
                               if (items[[i]]$type == 'tabPanel'){
                                 slctr = paste0('#', items[[i]]$container, ' li a[data-value = ', items[[i]]$value,']')
                                 shinyjs::disable(selector = slctr)} else {shinyjs::disable(i)}
                             }
                           },
                           
                           
                           enableItems = function(...){
                             itms = c(...)
                             verify(itms, 'character', domain = names(items), varname = 'Given arguments', null_allowed = F)
                             for (i in itms){
                               if (items[[i]]$type == 'tabPanel'){
                                 slctr = paste0('#', items[[i]]$container, ' li a[data-value = ', items[[i]]$value,']')
                                 shinyjs::enable(selector = slctr)} else {shinyjs::enable(i)}
                             }
                           },
                           
                           io.clothed.str = function(i){
                             s = io.str(i)
                             if (is.null(items[[i]]$cloth)){return(s)} else {cloth = items[[i]]$cloth}
                             if(!is.null(items[[i]]$title)){
                               items[[i]]$cloth$title <<- items[[i]]$title
                               cloth$title = items[[i]]$title
                             }
                             verify(cloth, "list")
                             verify(cloth$type, "character", domain = valid.cloth.types)
                             
                             cloth.str = "items[['" %++% i %++% "']]$cloth"
                             
                             switch(cloth$type,
                                    "box"       = {scr = "box("
                                    if (!is.null(cloth$title)){
                                      verify(cloth$title, 'character')
                                      scr = paste0(scr, "title = ", cloth.str, "$title,")
                                    }
                                    if (!is.null(cloth$footer)){
                                      verify(cloth$footer, 'character')
                                      scr = paste0(scr, "title = ", cloth.str, "$title,")
                                    }
                                    if (!is.null(cloth$status)){
                                      verify(cloth$status, 'character', domain = valid.box.statuses)
                                      scr = paste0(scr, "status = ", cloth.str, "$status,")
                                    }
                                    if (!is.null(cloth$solidHeader)){
                                      verify(cloth$solidHeader, 'logical', domain = c(T,F))
                                      scr = paste0(scr, "solidHeader = ", cloth.str, "$solidHeader,")
                                    }
                                    if (!is.null(cloth$background)){
                                      verify(cloth$background, 'character', domain = valid.colors)
                                      scr = paste0(scr, "background = ", cloth.str, "$background,")
                                    }
                                    
                                    if (!is.null(cloth$weight)){
                                      verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12))
                                      scr = paste0(scr, "width = ", cloth.str, "$weight,")
                                    }
                                    
                                    if (!is.null(cloth$height)){
                                      verify(cloth$height, 'character')
                                      scr = paste0(scr, "height = ", cloth.str, "$height,")
                                    }
                                    
                                    if (!is.null(cloth$collapsible)){
                                      verify(cloth$collapsible, 'logical', domain = c(T,F))
                                      scr = paste0(scr, "collapsible = ", cloth.str, "$collapsible,")
                                    }
                                    if (!is.null(cloth$collapsed)){
                                      verify(cloth$collapsed, 'logical', domain = c(T,F))
                                      scr = paste0(scr, "collapsed = ", cloth.str, "$collapsed,")
                                    }
                                    scr = scr %++% s %++% ")"},
                                    "tabPanel"  = {
                                      scr = "tabPanel(" %++% "title= '" %++%  verify(cloth$title, 'character', varname = "cloth$title") %++% "'"
                                      if (!is.null(cloth$icon)){
                                        verify(cloth$icon, 'character')
                                        scr   = paste0(scr, "icon = shiny::icon(", cloth.str, "$icon),")
                                      }
                                      scr = scr %++% list2Script(cloth, fields_remove = c('type', 'title', 'icon'), arguments = c(weight = 'width'))
                                    },
                                    "infoBox"   = {scr = "infoBox("
                                    ttl = verify(cloth$title, 'character', default = '')
                                    scr = paste0(scr, "title = ", "'", ttl, "', ")
                                    
                                    #if (!is.null(cloth$title)){
                                    #verify(cloth$title, 'character')
                                    #scr = paste0(scr, "title = ", cloth.str, "$title,")
                                    #} else {}
                                    if (!is.null(cloth$subtitle)){
                                      verify(cloth$subtitle, 'character')
                                      scr = paste0(scr, "subtitle = ", cloth.str, "$subtitle,")
                                    }
                                    if (!is.null(cloth$icon)){
                                      verify(cloth$icon, 'character')
                                      scr   = paste0(scr, "icon = shiny::icon(", cloth.str, "$icon),")
                                    }
                                    if (!is.null(cloth$color)){
                                      verify(cloth$color, 'character', domain = valid.colors)
                                      scr = paste0(scr, "color = ", cloth.str, "$color,")
                                    }
                                    if (!is.null(cloth$weight)){
                                      verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12))
                                      scr = paste0(scr, "width = ", cloth.str, "$weight,")
                                    }
                                    if (!is.null(cloth$href)){
                                      verify(cloth$href, 'character')
                                      scr = paste0(scr, "href = ", cloth.str, "$href,")
                                    }
                                    if (!is.null(cloth$fill)){
                                      verify(cloth$fill, 'logical', domain = c(T,F))
                                      scr = paste0(scr, "fill = ", cloth.str, "$fill,")
                                    }
                                    scr = paste0(scr, "value = ", s,")")},
                                    "valueBox"  = {scr = "valueBox("
                                    if (!is.null(cloth$title)){
                                      verify(cloth$subtitle, 'character')
                                      scr = paste0(scr, "subtitle = ", cloth.str, "$title,")
                                    } else {scr = paste0(scr, "subtitle = '',")}
                                    if (!is.null(cloth$icon)){
                                      verify(cloth$icon, 'character')
                                      scr   = paste0(scr, "icon = shiny::icon(", cloth.str, "$icon),")
                                    }
                                    if (!is.null(cloth$color)){
                                      verify(cloth$color, 'character', domain = valid.colors)
                                      scr = paste0(scr, "color = ", cloth.str, "$color,")
                                    }
                                    if (!is.null(cloth$weight)){
                                      verify(cloth$weight, c('numeric', 'integer'), domain = c(1,12))
                                      scr = paste0(scr, "width = ", cloth.str, "$weight,")
                                    }
                                    if (!is.null(cloth$href)){
                                      verify(cloth$href, 'character')
                                      scr = paste0(scr, "href = ", cloth.str, "$href,")
                                    }
                                    scr = paste0(scr, "value = ", s,")")},
                                    "column"    = {
                                      args = list2Script(cloth, fields = c('offset', 'align'))
                                      scr  = "column(" %>% paste0(chif(is.null(cloth$weight %>% verify(c('numeric', 'integer'), domain = c(1,12))), "width = 12,", paste0("width = ", cloth.str, "$weight,")),
                                                                  args, chif(is.empty(args), '', ", "), s %++% ")")
                                    },
                                    "wellPanel" = {scr = paste0("wellPanel(", s, ")")}
                             )
                             return(scr)
                           },
                           
                           # only row layout is supported for the sidebar
                           # lst.side is a vector of numerics
                           # lst.main is a vector of numerics
                           layscript.sidebar = function(s = '', lst.side, lst.main){
                             s = s %++% gndcd(144,148,150,9,8,39,184,3,11,97,60,19,196) %++% "("
                             
                             N.side = length(lst.side)
                             N.main = length(lst.main)
                             
                             if (N.side > 0){
                               s = s %++% gndcd(112,70,115,130,8,170,1,179,39,27,29,199) %++% "("
                               s = insert.io.strs(s, lst.side)
                               s = s %++% "),"
                             }
                             
                             if (N.main > 0){
                               s = s %++% gndcd(43,11,118,158,32,170,14,9,64) %++% "("
                               s = insert.io.strs(s, lst.main)
                               s = s %++% ")"
                             }
                             s = s %++% ")"
                             return (s)
                           },
                           
                           layscript.dashboard = function(s = '', lst.head, lst.side, lst.body, header.title = NULL, header.title.width = NULL, sidebar.width = NULL, skin = 'blue',
                                                          header.title.color = NULL, header.title.background.color = NULL, header.title.hover.color = NULL, header.title.hover.background.color = NULL,
                                                          header.title.font = NULL, header.title.font.weight = NULL, header.title.font.size = NULL,
                                                          header.title.hover.font = NULL, header.title.hover.font.weight = NULL, header.title.hover.font.size = NULL){
                             N.head = length(lst.head)
                             N.side = length(lst.side)
                             N.body = length(lst.body)
                             
                             s = s %++% gndcd(7,170,144,51,8,12,39,31,115,44,162,39,150,130,184) %++% "("
                             if (!is.null(header.title)){
                               s = paste0(s, "title = '", header.title, "'")
                               if (N.head > 0 | !is.null(header.title.width)){s = s %++% ', '}
                             }
                             
                             if (!is.null(header.title.width)){
                               s = paste0(s, "titleWidth = ", header.title.width)
                               if (N.head > 0){s = s %++% ', '}
                             }
                             
                             if (N.head > 0){s = insert.io.strs(s, lst.head)} else if (is.null(header.title) & is.null(header.title.width)){s = s %++% "disable = TRUE"}
                             s = s %++% "),"
                             
                             s = s %++% gndcd(115,2,98,38,142,143,170,1,20,131,4,150.94,29,190,11,110) %++% "("
                             if (!is.null(sidebar.width)){s %<>% paste0('width = ', sidebar.width, ', ')}
                             if (N.side > 0){s = insert.io.strs(s, lst.side)} else {s = s %++% "disable = TRUE"}
                             s = s %++% "),"
                             
                             s %<>% paste0(gndcd(150,11,112,86,8,60,39,1,150,48,143,115,138), "(tags$head(tags$style(HTML('",
                                           skin.style.css(skin, header.title.font, header.title.font.weight, header.title.font.size, header.title.color, header.title.background.color, header.title.hover.font, header.title.hover.font.weight, header.title.hover.font.size , header.title.hover.color, header.title.hover.background.color),
                                           "')))")
                             if(!is.empty(lst.body)){s = s %++% ", "}
                             s = insert.io.strs(s, lst.body)
                             s = s %++% ")"
                             return (s)
                           },
                           
                           layscript = function(layout){
                             # todo: verify layout is a list of characters
                             N.item = length(layout)
                             s = ''
                             for (i in sequence(N.item)){
                               s = s %++% "getItemObject('" %++% layout[[i]] %++% "'"
                               if (i < N.item){s = s %++% ','}
                             }
                             return(s)
                           },
                           
                           layscript.RCPanel = function(s = "", lst, title = '', is.row = T, col.panel = F){
                             N.items = nListOrCharItems(lst)
                             N.list  = length(lst)
                             for (i in sequence(N.list)){
                               its.list = inherits(lst[[i]], 'list')
                               its.char = inherits(lst[[i]], 'character')
                               if (its.list | its.char){
                                 if (is.row){s = paste0(s, gndcd(33,55,154,4, 150,119,143,28) %++% "(")} else {
                                   if (its.list){
                                     if (is.null(lst[[i]]$weight)){
                                       ww = floor(12/N.items)
                                     } else {ww = lst[[i]]$weight}
                                     if (is.null(lst[[i]]$offset)){
                                       ofst = 0
                                     } else {ofst = lst[[i]]$offset}
                                   } else if (its.char) {
                                     wght = items %>% list.extract.field(lst[[i]], field_name = 'weight')
                                     ww   = chif(is.empty(wght), floor(12/N.items), wght %>% mean(na.rm = T) %>% floor)
                                     ofst = items %>% list.extract.field(lst[[i]], field_name = 'offset')
                                     if (is.empty(ofst)){
                                       ofst = 0
                                     } else {
                                       ofst = ofst %>% mean(na.rm = T) %>% floor
                                     }
                                   }
                                   
                                   s = paste0(s, gndcd(88, 60, 64, 19, 10, 27) %++% "(" %++% gndcd(12, 33, 151, 112, 162, 196) %++%  " = ", as.character(ofst), ", ")
                                   s = paste0(s, "width  = ",as.character(ww), ", ")
                                   if (col.panel){s = paste0(s, 'wellPanel(')}
                                 }
                                 
                                 if (its.list){s = layscript.RCPanel(s, lst[[i]], is.row = !is.row, col.panel = col.panel)}
                                 else if (its.char) {s = insert.io.strs(s, lst[[i]])}
                                 s = paste0(s, ')')
                                 if (col.panel & !is.row){s = paste0(s, ')')}
                                 s = paste0(s, ',')
                               }
                             }
                             NC = nchar(s)
                             if(substr(s, NC, NC) == ','){s %<>% substr(1,NC - 1)}
                             return (s)
                           },
                           
                           insert.io.strs = function(s, vct){
                             # vct must be a vector of numerics or characters
                             M = length(vct)
                             for (j in sequence(M)){
                               s = s %++% io.clothed.str(vct[j])
                               if (j < M){s = s %++% ','}
                             }
                             return(s)
                           },
                           
                           getItemObject = function(i){
                             if (is.null(items[[i]]$object)){
                               if (is.null(items[[i]]$type)){return(NULL)}
                               if(items[[i]]$type %in% valid.input.types){
                                 lbl = items[[i]]$title %>% verify('character', default = "", varname = "items[['" %++% i %++% "']]$title")
                               }
                               if(items[[i]]$type %in% c('actionButton', 'actionBttn', 'actionLink')){
                                 if(is.null(items[[i]]$icon)){icn = NULL} else if (items[[i]]$icon %>% inherits('character')){icn = items[[i]]$icon %>% icon} else {icn = items[[i]]$icon}
                               }
                               if(items[[i]]$type %in% c('radioButtons', 'checkboxGroupInput', 'selectInput')) {
                                 chs = items[[i]]$choices %>% verify(c('character', 'factor', 'logical', 'integer', 'numeric'), varname = "items[['" %++% i %++% "']]$choices")
                                 slc = items[[i]]$selected %>% verify(c('character', 'factor', 'logical', 'integer', 'numeric'), varname = "items[['" %++% i %++% "']]$selected")
                                 if(inherits(chs, 'numeric')){chs %<>% as.integer}
                                 if(inherits(slc, 'numeric')){slc %<>% as.integer}
                               }
                               switch(items[[i]]$type,
                                      "radioButtons"   = {
                                        inl  = verify(items[[i]]$inline, 'logical', varname = "items[['" %++% i %++% "']]$inline", default = F)
                                        assert(length(chs) > 1, "radioButtons input must have at least two choices!")
                                        # if (is.null(names(chs))){names(chs) = chs}
                                        obj = radioButtons(i, label = lbl, choices = chs, selected = slc, inline = inl, width = items[[i]]$width)},
                                      "textInput"      = {
                                        obj = textInput(i,
                                                        label = lbl,
                                                        value = items[[i]]$value %>% verify('character', default = "", varname = "items[['" %++% i %++% "']]$value"),
                                                        width = items[[i]]$width %>% verify('character', varname = "items[['" %++% i %++% "']]$width"),
                                                        placeholder = items[[i]]$placeholder %>% verify('character', varname = "items[['" %++% i %++% "']]$placeholder"))},
                                      "passwordInput"  = {
                                        obj = passwordInput(i,
                                                            label = lbl,
                                                            value = items[[i]]$value %>% verify('character', default = "", varname = "items[['" %++% i %++% "']]$value"),
                                                            width = items[[i]]$width %>% verify('character', varname = "items[['" %++% i %++% "']]$width"),
                                                            placeholder = items[[i]]$placeholder %>% verify('character', varname = "items[['" %++% i %++% "']]$placeholder"))},
                                      "sliderInput"    = {
                                        mn = items[[i]]$min   %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = "items[['" %++% i %++% "']]$min"  , default = 0)
                                        mx = items[[i]]$max   %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = "items[['" %++% i %++% "']]$max"  , default = 1, domain = c(mn, Inf))
                                        vl = items[[i]]$value %>% verify(c('numeric', 'integer', 'logical', valid.time.classes), varname = "items[['" %++% i %++% "']]$value", default = 0, domain = c(mn, mx))
                                        an = items[[i]]$animate %>% verify(c("list", "logical"), default = F)
                                        
                                        obj = sliderInput(i, label = lbl, min = mn, max = mx, value = vl, step = items[[i]]$step,
                                                          sep  = items[[i]]$sep, pre = items[[i]]$pre, post = items[[i]]$post, timeFormat = items[[i]]$timeFormat, animate = an)},
                                      "actionButton"   = {
                                        obj = actionButton(i, label = lbl, icon = icn, width = items[[i]]$width) %>% buildStyle(inline = items[[i]]$inline, vertical_align = items[[i]]$vertical_align, float = items[[i]]$float, width = items[[i]]$width)},
                                      "actionBttn"     = {
                                        support('shinyWidgets')
                                        stl = items[[i]]$style  %>% verify('character', domain = c('simple', 'bordered', 'minimal', 'stretch', 'jelly', 'gradient', 'fill', 'material-circle', 'material-flat', 'pill', 'float', 'unite'), default = 'unite', varname = "items[['" %++% i %++% "']]$style")
                                        sts = items[[i]]$status %>% verify('character', domain = c('default', 'primary', 'warning', 'danger', 'success', 'royal'), default = 'unite', varname = "items[['" %++% i %++% "']]$status")
                                        sz  = items[[i]]$size   %>% verify('character', domain = c('xs', 'sm', 'md', 'lg'), default = 'md', varname = "items[['" %++% i %++% "']]$size")
                                        blk = items[[i]]$fullwidth   %>% verify('logical', domain = c(T, F), default = F, varname = "items[['" %++% i %++% "']]$fullwidth")
                                        nol = items[[i]]$no_outline  %>% verify('logical', domain = c(T, F), default = T, varname = "items[['" %++% i %++% "']]$no_outline")
                                        obj = actionBttn(i, label = lbl, icon = icn, style= stl, color = sts, size = sz, block = blk, no_outline = nol)},
                                      "actionGroupButtons"     = {
                                        support('shinyWidgets')
                                        ids = items[[i]]$inputIds   %>% verify('character', varname = "items[['" %++% i %++% "']]$inputIds", null_allowed = F)
                                        lbl = items[[i]]$labels     %>% verify(c('character', 'list'), varname = "items[['" %++% i %++% "']]$labels", lengths = length(ids), default = 'Button' %>% paste(sequence(length(ids))))
                                        sts = items[[i]]$status     %>% verify('character', domain = c('default', 'primary', 'warning', 'danger', 'success', 'royal'), default = 'default', varname = "items[['" %++% i %++% "']]$status")
                                        sz  = items[[i]]$size       %>% verify('character', domain = c('xs', 'sm', 'normal', 'lg'), default = 'normal', varname = "items[['" %++% i %++% "']]$size")
                                        inl = items[[i]]$inline     %>% verify('logical', domain = c(T, F), default = T, varname = "items[['" %++% i %++% "']]$inline")
                                        fwt = items[[i]]$fullwidth  %>% verify('logical', domain = c(T, F), default = F, varname = "items[['" %++% i %++% "']]$fullwidth")
                                        obj = actionGroupButtons(ids, labels = lbl, status = sts, size = sz, direction = chif(inl, 'horizontal', 'vertical'), fullwidth = fwt)},
                                      
                                      "actionLink"     = {obj = actionLink(i, label = lbl, icon = icn)},
                                      "checkboxInput"  = {
                                        vlu = verify(items[[i]]$value, 'logical', domain = c(T,F), varname = "items[['" %++% i %++% "']]$value", default = F)
                                        obj = checkboxInput(i, label = lbl, value = vlu) %>% buildStyle(inline = items[[i]]$inline, vertical_align = items[[i]]$vertical_align, float = items[[i]]$float)},
                                      "checkboxGroupInput" = {
                                        inl  = verify(items[[i]]$inline, 'logical', domain = c(T,F), varname = "items[['" %++% i %++% "']]$inline", default = F)
                                        obj  = checkboxGroupInput(i, label = lbl, choices = chs, selected = slc, inline = inl, width = items[[i]]$width, choiceNames = items[[i]]$choiceNames, choiceValues = items[[i]]$choiceValues)},
                                      "selectInput"    = {
                                        mltpl = verify(items[[i]]$multiple, 'logical', varname = "items[['" %++% i %++% "']]$multiple", default = F)
                                        slctz = verify(items[[i]]$selectize, 'logical', varname = "items[['" %++% i %++% "']]$selectize", default = T)
                                        obj   = selectInput(i, label = lbl, choices = chs, selected = slc, multiple = mltpl, selectize = slctz, width = items[[i]]$width)},
                                      "dateInput"      = {obj = dateInput(i, label = lbl, value = items[[i]]$value, min = items[[i]]$min, max = items[[i]]$max)},
                                      # todo: change input name for airDatepickerInput! define separate input types like monthInput, yearInput, monthRangeInput, dateTimeInput, ...
                                      "airDatepickerInput" = {
                                        scr = "airDatepickerInput(i, " %>% 
                                          paste(list2Script(items[[i]], fields = c(title = 'label', 'value', 'multiple', 'range', 'timePicker', 'separator', 'placeholder', 'dateFormat', min = 'minDate', max = 'maxDate', 'disabledDates', 'view', 'minView', 'monthsField', 'clearButton', 'todayButton', 'autoClose', 'timepickerOpts', 'position', 'update_on', 'addon', 'language', 'inline', 'width')), ")")
                                        obj = eval(parse(text = scr))
                                      },
                                      "dateRangeInput" = {
                                        obj = dateRangeInput(i, label = lbl, 
                                                             start     = items[[i]]$start, end = items[[i]]$end, min = items[[i]]$min, max = items[[i]]$max, 
                                                             format    = items[[i]]$format %>% verify('character', default = "yyyy-mm-dd"), 
                                                             startview = items[[i]]$startview %>% verify('character', domain = c("month", "year", "decade"), default = "month"), weekstart = 0,
                                                             language  = "en", separator = " to ", width = items[[i]]$width, 
                                                             autoclose = items[[i]]$autoclose %>% verify('logical', domain = c(T, F), default = T))},
                                      "fileInput"      = {
                                        mltpl = verify(items[[i]]$multiple   , 'logical'  , varname = "items[['" %++% i %++% "']]$multiple", default = F)
                                        btlbl = verify(items[[i]]$buttonLabel, 'character', varname = "items[['" %++% i %++% "']]$buttonLabel", default = "Browse...")
                                        phldr = verify(items[[i]]$placeholder, 'character', varname = "items[['" %++% i %++% "']]$placeholder", default = "No file selected")
                                        obj   = fileInput(i, label = lbl, multiple = mltpl, accept = items[[i]]$accept, width = items[[i]]$width, buttonLabel = btlbl, placeholder = phldr)},
                                      "numericInput"   = {
                                        if (is.null(items[[i]]$step)){items[[i]]$step <<- NA}
                                        if (is.null(items[[i]]$max)){items[[i]]$max <<- NA}
                                        if (is.null(items[[i]]$min)){items[[i]]$min <<- NA}
                                        obj = numericInput(i, label = lbl, value = items[[i]]$value, step = items[[i]]$step, min = items[[i]]$min, max = items[[i]]$max)},
                                      
                                      "column" = {
                                        scr = "column("
                                        wdth = verify(items[[i]]$weight, 'numeric' , default = 12)
                                        ofst = verify(items[[i]]$offset, 'numeric', default = 0)
                                        scr = paste0(scr,'width = ', wdth, ', offset = ',ofst, ',')
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))},
                                      
                                      "wellPanel" = {
                                        scr = "wellPanel("
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))},
                                      
                                      "box" = {
                                        scr = "shinydashboard::box("
                                        wdth = verify(items[[i]]$weight, 'numeric' , default = 12, varname    = paste0("items[['",i,"']]$weight"))
                                        ttle = verify(items[[i]]$title, 'character' , default = '', varname  = paste0("items[['",i,"']]$title"))
                                        fotr = verify(items[[i]]$footer, 'character' , default = '', varname = paste0("items[['",i,"']]$footer"))
                                        stus = verify(items[[i]]$status, 'character' , domain = valid.box.statuses, varname = paste0("items[['",i,"']]$status"))
                                        shdr = verify(items[[i]]$solidHeader, 'logical' , default = 'F', domain = c(T, F), varname = paste0("items[['",i,"']]$solidHeader"))
                                        bgrd = verify(items[[i]]$background, 'character' , domain = valid.colors, default = 'blue', varname = paste0("items[['",i,"']]$background"))
                                        cpbl = verify(items[[i]]$collapsible, 'logical' , default = 'F', domain = c(T, F), varname = paste0("items[['",i,"']]$collapsible"))
                                        cpsd = verify(items[[i]]$collapsed, 'logical' , default = 'F', domain = c(T, F), varname = paste0("items[['",i,"']]$collapsed"))
                                        
                                        if(!is.null(stus)){scr = paste0(scr,"status = '", stus, "',")}
                                        scr = paste0(scr,'width = ', wdth, ',')
                                        scr = paste0(scr,"title = '", ttle, "',")
                                        scr = paste0(scr,"footer = '", fotr, "',")
                                        scr = paste0(scr,"background = '", bgrd, "',")
                                        if (shdr){scr = paste0(scr,'solidHeader = T,')}
                                        if (cpbl){scr = paste0(scr,'collapsible = T,')}
                                        if (cpsd){scr = paste0(scr,'collapsed = T,')}
                                        
                                        # list2Script(items[[i]], fields_remove = c('type'))
                                        
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))
                                      },
                                      
                                      "bag" = {obj = eval(parse(text = "div(" %>% insert.io.strs(items[[i]]$layout) %>% paste0(")")))},
                                      
                                      "fluidPage" = {
                                        scr = "fluidPage("
                                        if (!is.null(items[[i]]$title)){scr = paste0(scr, "titlePanel('", items[[i]]$title, "', windowTitle = '", items[[i]]$wintitle, "'),")}
                                        cpl = verify(items[[i]]$col.framed, 'logical', varname = paste0("items[['",i,"']]$col.framed"), domain = c(T,F), default = F)
                                        scr = layscript.RCPanel(s = scr, lst = items[[i]]$layout, col.panel = cpl)
                                        scr = scr %++% ')'
                                        obj = eval(parse(text = scr))},
                                      
                                      "dashboardPage" = {
                                        scr = "dashboardPage("
                                        ttl = verify(items[[i]]$title,  'character', varname = paste0("items[['",i,"']]$title"))
                                        skn = verify(items[[i]]$skin, 'character', varname = paste0("items[['",i,"']]$skin"), domain = valid.dashboard.skins)
                                        if (!is.null(ttl)){scr %<>% paste0("title = '", ttl, "', ")}
                                        if (!is.null(skn)){scr %<>% paste0("skin  = '", skn, "', ")}
                                        scr %<>% layscript.dashboard(lst.head = items[[i]]$layout.head, lst.side = items[[i]]$layout.side, lst.body = items[[i]]$layout.body, header.title = items[[i]]$header.title, header.title.width = items[[i]]$header.title.width, sidebar.width = items[[i]]$sidebar.width,
                                                                     header.title.color = items[[i]]$header.title.color, header.title.background.color = items[[i]]$header.title.background.color, header.title.hover.color = items[[i]]$header.title.hover.color, header.title.hover.background.color = items[[i]]$header.title.hover.background.color,
                                                                     header.title.font = items[[i]]$header.title.font, header.title.font.weight = items[[i]]$header.title.font.weight, header.title.font.size = items[[i]]$header.title.font.size,
                                                                     header.title.hover.font = items[[i]]$header.title.hover.font, header.title.hover.font.weight = items[[i]]$header.title.hover.font.weight, header.title.hover.font.size = items[[i]]$header.title.hover.font.size)
                                        scr = scr %++% ')'
                                        obj = eval(parse(text = scr))},
                                      
                                      "tutorBox" = {
                                        scr = "rintrojs::introBox(" %>% insert.io.strs(items[[i]]$layout) %>% 
                                          paste0(", ", list2Script(items[[i]], 
                                                                   fields    = c('tutor.lesson', 'tutor.step', 'tutor.hint'), 
                                                                   arguments = c(tutor.lesson = 'data.intro', tutor.step = 'data.step', tutor.hint = 'data.hint')), ")") 
                                        obj = eval(parse(text = scr))
                                      },
                                      
                                      "sidebarLayout" = {
                                        scr = "fluidPage("
                                        if (!is.null(items[[i]]$title)){scr = paste0(scr, "titlePanel('", items[[i]]$title, "', windowTitle = '", items[[i]]$wintitle, "'),")}
                                        scr = layscript.sidebar(s = scr, lst.side = items[[i]]$layout.side, lst.main = items[[i]]$layout.main)
                                        scr = scr %++% ')'
                                        obj = eval(parse(text = scr))},
                                      
                                      "navbarPage" = {  # todo: write specific layscript for this type of container so that it creates tabPanels and menus based on a layout of type list
                                        scr = "navbarPage("
                                        if (is.null(items[[i]]$title)){items[[i]]$title <<- ''}
                                        scr = scr %++% "title = '" %++% verify(items[[i]]$title, 'character')  %++% "', "
                                        if (!is.null(items[[i]]$position)){scr = scr %++% "position = '" %++% verify(items[[i]]$position, 'character', domain = valid.navbar.positions)  %++% "', "}
                                        if (!is.null(items[[i]]$header)){scr = scr %++% "header = '" %++% verify(items[[i]]$header, 'character')  %++% "', "}
                                        if (!is.null(items[[i]]$footer)){scr = scr %++% "footer = '" %++% verify(items[[i]]$footer, 'character')  %++% "', "}
                                        if (!is.null(items[[i]]$wintitle)){scr = scr %++% "windowTitle = '" %++% verify(items[[i]]$wintitle, 'character')  %++% "', "}
                                        if (!is.null(items[[i]]$icon)){scr = scr %++% "icon = icon('" %++% verify(items[[i]]$icon, 'character')  %++% ")', "}
                                        if (!is.null(i)){scr = scr %++% "id = '" %++% verify(i, 'character')  %++% "', "}
                                        clp = verify(items[[i]]$collapsible, 'logical', domain = c(T,F), default = F)
                                        fld = verify(items[[i]]$fluid, 'logical', domain = c(T,F), default = T)
                                        if (clp) {scr = scr %++% "collapsible = TRUE, "}
                                        if (!fld){scr = scr %++% "fluid = FASLE, "}
                                        if (!is.null(items[[i]]$theme)){scr = scr %++% "theme = '" %++% verify(items[[i]]$theme, 'character')  %++% "', "}
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))},
                                      
                                      "navbarMenu" = {
                                        scr = "navbarMenu("
                                        if (is.null(items[[i]]$title)){items[[i]]$title <<- ''}
                                        scr = scr %++% "title = '" %++% verify(items[[i]]$title, 'character')  %++% "', "
                                        if (!is.null(items[[i]]$icon)){scr = scr %++% "icon = icon('" %++% verify(items[[i]]$icon, 'character')  %++% ")', "}
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))},
                                      
                                      "bsModal" = {
                                        scr = items[[i]]$type %>% 
                                          paste0("(id = '", i, "' ,", list2Script(items[[i]], fields = c('title', 'trigger', 'size'))) %>% paste0(",") %>% 
                                          insert.io.strs(items[[i]]$layout) %>% paste0(")")
                                        obj = eval(parse(text = scr))
                                      },
                                      
                                      "bsCollapse" = {
                                        scr = items[[i]]$type %>% 
                                          paste0("(id = '", i, "' ,", list2Script(items[[i]], fields = c('multiple', 'open'))) %>% paste0(",") %>% 
                                          insert.io.strs(items[[i]]$layout) %>% paste0(")")
                                        obj = eval(parse(text = scr))
                                      },
                                      
                                      "bsCollapsePanel" = {
                                        scr = items[[i]]$type %>% 
                                          paste0("(", list2Script(items[[i]], fields = c('title', 'value', 'style'))) %>% paste0(",") %>% 
                                          insert.io.strs(items[[i]]$layout) %>% paste0(")")
                                        obj = eval(parse(text = scr))
                                      },
                                      
                                      "tabsetPanel" = {
                                        scr = "tabsetPanel("
                                        if (!is.null(items[[i]]$selected)){scr %<>% paste0("selected = '", items[[i]]$selected %>% verify('character', varname = 'selected'), "', ")}
                                        if (!is.null(items[[i]]$shape)){scr %<>% paste0("type = '", items[[i]]$shape %>% verify('character', domain = c("tabs", "pills"), varname = 'shape'), "', ")}
                                        scr = insert.io.strs(scr, items[[i]]$layout)
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))},
                                      
                                      "tabPanel" = {
                                        scr = "tabPanel("
                                        scr = scr %++% "id = '" %++% i %++% "', "
                                        if (is.null(items[[i]]$title)){items[[i]]$title <<- i}
                                        if (is.null(items[[i]]$value)){items[[i]]$value <<- i}
                                        scr = scr %++% "title = '" %++% verify(items[[i]]$title, 'character')  %++% "', "
                                        scr = scr %++% "value = '" %++% verify(items[[i]]$value, 'character')  %++% "', "
                                        if (!is.null(items[[i]]$icon)){scr = scr %++% "icon = icon('" %++% verify(items[[i]]$icon, 'character')  %++% ")', "}
                                        cpl = verify(items[[i]]$col.framed, 'logical', varname = paste0("items[['",i,"']]$col.framed"), domain = c(T,F), default = F)
                                        if      (inherits(items[[i]]$layout, 'list')){scr = layscript.RCPanel(s = scr, lst = items[[i]]$layout, col.panel = cpl)}
                                        else if (inherits(items[[i]]$layout, 'character')){scr = insert.io.strs(scr, items[[i]]$layout)} else {stop("Invalid type for argument 'layout'!")}
                                        
                                        scr = scr %++% ")"
                                        obj = eval(parse(text = scr))}
                                      
                               )
                               if(!is.null(items[[i]]$tutor.lesson)){
                                 settings$tutorMode <<- T
                                 if(items[[i]]$type != 'tutorBox'){
                                   obj %<>% rintrojs::introBox(
                                     data.intro = items[[i]]$tutor.lesson %>% verify('character') %>% paste(collapse = '\n'), 
                                     data.hint  = items[[i]]$tutor.hint %>% verify('character') %>% paste(collapse = '\n'),
                                     data.step  = items[[i]]$tutor.step)
                                 }
                               }
                               return(obj)
                             } else {return(items[[i]]$object)}
                           },
                           
                           dashboard.ui = function(){
                             # Determine container name of each item and put it in property 'container':
                             for (i in names(items)){
                               if (items[[i]]$type %in% valid.container.types){
                                 members = character()
                                 if(!is.null(items[[i]]$layout.side)){
                                   members = c(members, items[[i]]$layout.side %^% names(items))
                                 }
                                 if(!is.null(items[[i]]$layout.main)){
                                   members = c(members, items[[i]]$layout.main %^% names(items))
                                 }
                                 
                                 if(!is.null(items[[i]]$layout)){
                                   if(inherits(items[[i]]$layout, 'list')){members = items[[i]]$layout %>% list.flatten %>% as.character %>% intersect(names(items))}
                                   else {members = items[[i]]$layout %^% names(items)}
                                 } 
                                 
                                 for (j in members){items[[j]]$container <<- i}
                               }
                             }
                             # Important TODO: check and return error if any dynamic item is used twice. This leads to all services being inactivated!!!!
                             
                             # Add tooltips and popovers if there are any:
                             for (i in names(items)){
                               # tooltips
                               if(!is.null(items[[i]]$tooltip)){
                                 tooltip.name = paste(i, 'tooltip', sep = '.')
                                 items[[tooltip.name]] <<- list(type = 'static', 
                                                                object = bsTooltip(id = i, 
                                                                                   title     = items[[i]]$tooltip %>% verify('character') %>% paste(collapse = '\n'), 
                                                                                   placement = items[[i]]$tooltip.placement %>% verify('character', lengths = 1, domain = c('top', 'bottom', 'left', 'right')   , default = 'bottom'),
                                                                                   trigger   = items[[i]]$tooltip.trigger   %>% verify('character', lengths = 1, domain = c('hover', 'focus', 'click', 'manual'), default = 'hover'),
                                                                                   options   = items[[i]]$tooltip.options))
                                 if(items[[items[[i]]$container]]$type %in% c('sidebarLayout', 'dashboardPage')){
                                   if(i %in% items[[items[[i]]$container]]$layout.side){
                                     items[[items[[i]]$container]]$layout.side     <<- c(items[[items[[i]]$container]]$layout.side, tooltip.name)}
                                   else {items[[items[[i]]$container]]$layout.main <<- c(items[[items[[i]]$container]]$layout.main, tooltip.name)}
                                 } else {items[[items[[i]]$container]]$layout      <<- c(items[[items[[i]]$container]]$layout,      tooltip.name)}
                                 # todo: what about layout.head? only for containers of type 'dashboardPage'
                               }
                               
                               # popovers /commented because HTML does not work with with bsPopover in the UI
                               # if(!is.null(items[[i]]$popover)){
                               #   popover.name = paste(i, 'popover', sep = '.')
                               #   items[[popover.name]] <<- list(type = 'static', 
                               #      object = bsPopover(id = i, 
                               #        title     = items[[i]]$popover.title %>% verify('character', lengths = 1), 
                               #        content   = items[[i]]$popover %>% verify('character') %>% paste(collapse = '\n'),
                               #        placement = items[[i]]$popover.placement %>% verify('character', lengths = 1, domain = c('top', 'bottom', 'left', 'right')   , default = 'bottom'),
                               #        trigger   = items[[i]]$popover.trigger   %>% verify('character', lengths = 1, domain = c('hover', 'focus', 'click', 'manual'), default = 'hover'),
                               #        options   = items[[i]]$popover.options))
                               #   if(items[[items[[i]]$container]]$type %in% c('sidebarLayout', 'dashboardPage')){
                               #     if(i %in% items[[items[[i]]$container]]$layout.side){
                               #       items[[items[[i]]$container]]$layout.side     <<- c(items[[items[[i]]$container]]$layout.side, popover.name)}   
                               #     else {items[[items[[i]]$container]]$layout.main <<- c(items[[items[[i]]$container]]$layout.main, popover.name)} 
                               #   } else {items[[items[[i]]$container]]$layout      <<- c(items[[items[[i]]$container]]$layout,      popover.name)}
                               # }
                               
                             }
                             
                             # Create output objects:
                             for (i in names(items)){
                               # Outputs:
                               if (items[[i]]$type %in% valid.output.types){
                                 if (!is.null(items[[i]]$type)){
                                   fields = names(items[[i]])
                                   if(items[[i]]$type %in% c('plotOutput', 'TFD3Output', 'amChartsOutput', 'dygraphOutput', 'leafletOutput', 'wordcloud2Output', 'plotlyOutput', 'highcharterOutput', 'morrisjsOutput', 'billboarderOutput', 'sankeytreeOutput')){
                                     wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%"  , varname = "items[['" %++% i %++% "']]$width")
                                     hght = items[[i]]$height %>% verify('character', lengths = 1, default = "400px" , varname = "items[['" %++% i %++% "']]$height")
                                   }
                                   
                                   switch(items[[i]]$type,
                                          
                                          "downloadButton"   = {items[[i]]$object <<- downloadButton(i, label = items[[i]]$title, class = items[[i]]$class, style = items[[i]]$style)},
                                          "downloadLink"     = {items[[i]]$object <<- downloadLink(i, label = items[[i]]$title, class = items[[i]]$class, style = items[[i]]$style)},
                                          "dynamicInput"     = {items[[i]]$object <<- uiOutput(i, inline = items[[i]]$inline %>% verify('logical' , domain = c(T,F), default = F , varname = "items[['" %++% i %++% "']]$inline"))},
                                          "loginInput"       = {items[[i]]$object <<- uiOutput(i, inline = items[[i]]$inline %>% verify('logical' , domain = c(T,F), default = F , varname = "items[['" %++% i %++% "']]$inline"))},
                                          "uiOutput"         = {
                                            items[[i]]$object <<- uiOutput(i)
                                            if (!is.null(items[[i]]$cloth)  &  !is.null(items[[i]]$title)){items[[i]]$cloth$title <<- items[[i]]$title}
                                          },
                                          "plotOutput" = {
                                            if ('brush' %in% fields) {brsh  = brushOpts(id = items[[i]]$brush)} else {brsh = NULL}
                                            items[[i]]$object <<- plotOutput(i, width  = wdth, height = hght, click  = items[[i]]$click, brush  = brsh)
                                            spn = items[[i]]$spinner %>% verify('character', domain = c('circle', 'bounce', 'folding-cube', 'rotating-plane', 'cube-grid', 'fading-circle', 'double-bounce', 'dots', 'cube'), varname = "items[['" %++% i %++% "']]$spinner")
                                            if(!is.null(spn)){
                                              support('shinyWidgets')
                                              clr = items[[i]]$spinner.color %>% verify('character', varname = "items[['" %++% i %++% "']]$spinner.color", default = '#112446')
                                              dus = try(col2rgb(clr), silent = T) %>% inherits('try-error')
                                              items[[i]]$object <<- addSpinner(items[[i]]$object, spin = spn, color = chif(dus, '#112446', clr))
                                            }
                                          },
                                          
                                          "verbatimTextOutput" = {
                                            phldr = items[[i]]$placeholder %>% verify('logical', lengths = 1, default = F, varname = "items[['" %++% i %++% "']]$placeholder")
                                            items[[i]]$object <<- verbatimTextOutput(i, placeholder = phldr)},
                                          # todo: add arguments container and inline for textOutput and htmlOutput
                                          "textOutput"         = {items[[i]]$object <<- textOutput(i)},
                                          "tableOutput"        = {items[[i]]$object <<- tableOutput(i)},
                                          "dataTableOutput"    = {
                                            support('DT')
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "auto" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- DT::dataTableOutput(i, width = wdth, height = hght)},
                                          "TFD3Output"= {items[[i]]$object <<- TFD3Output(i, width  = wdth, height = hght)},
                                          "grvizOutput" = {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "400px" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- grvizOutput(i, width = wdth, height = hght)},
                                          "c3Output" = {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- c3::c3Output(i, width = wdth, height = hght)
                                          },
                                          "sankeyNetworkOutput" = {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "500px" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- networkD3::sankeyNetworkOutput(i, width = wdth, height = hght)},
                                          "sunburstOutput" = {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "400px" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- sunburstR::sunburstOutput(i, width = wdth, height = hght)},
                                          "rHandsonTableOutput"= {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%"  , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "100%" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- rhandsontable::rHandsontableOutput(i, width  = wdth, height = hght)},
                                          "htmlOutput"         = {items[[i]]$object <<- htmlOutput(i)},
                                          "amChartsOutput"     = {
                                            support('rAmCharts')
                                            # todo: add argument 'type', better to change the argument to 'plotType'
                                            items[[i]]$object <<- amChartsOutput(i, width = wdth, height = hght)},
                                          "pivotOutput"     = {
                                            wdth = items[[i]]$width  %>% verify('character', lengths = 1, default = "100%"  , varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify('character', lengths = 1, default = "500px" , varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- pivotOutput(i, width = wdth, height = hght)},
                                          "dygraphOutput"      = {
                                            support('dygraphs')
                                            items[[i]]$object <<- dygraphs::dygraphOutput(i, width = wdth, height = hght)},
                                          "gglVisChartOutput"  = {items[[i]]$object <<- htmlOutput(i)},
                                          "leafletOutput"      = {
                                            support('leaflet')
                                            items[[i]]$object <<- leafletOutput(i, width = wdth, height = hght)},
                                          "wordcloud2Output"   = {
                                            support('wordcloud2')
                                            items[[i]]$object <<- wordcloud2Output(i, width = wdth, height = hght)},
                                          "infoBoxOutput"      = {items[[i]]$object <<- infoBoxOutput(i,  width = items[[i]]$weight)},
                                          "valueBoxOutput"     = {items[[i]]$object <<- valueBoxOutput(i, width = items[[i]]$weight)},
                                          "plotlyOutput"       = {
                                            support('plotly')
                                            items[[i]]$object <<- plotlyOutput(i, width  = wdth, height = hght)},
                                          "highcharterOutput"  = {
                                            support('highcharter')
                                            items[[i]]$object <<- highchartOutput(i, width  = wdth, height = hght)},
                                          "morrisjsOutput"  = {
                                            support('morrisjs')
                                            items[[i]]$object <<- morrisjsOutput(i, width  = wdth, height = hght)},
                                          "billboarderOutput"  = {
                                            items[[i]]$object <<- billboarder::billboarderOutput(i, width  = wdth, height = hght)},
                                          "sankeytreeOutput"   = {
                                            items[[i]]$object <<- sankeytreeR::sankeytreeOutput(i, width  = wdth, height = hght)},
                                          "coffeewheelOutput"  = {
                                            wdth = items[[i]]$width  %>% verify(c('numeric', 'character'), lengths = 1, default = 400, varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify(c('numeric', 'character'), lengths = 1, default = 400, varname = "items[['" %++% i %++% "']]$height")
                                            
                                            items[[i]]$object <<- coffeewheelOutput(i, width  = wdth, height = hght)},
                                          "bubblesOutput"      = {
                                            support('bubbles') # will be embedded
                                            wdth = items[[i]]$width  %>% verify(c('character'), lengths = 1, default = '600px', varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify(c('character'), lengths = 1, default = '600px', varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- bubblesOutput(i, width  = wdth, height = hght)},
                                          "d3plusOutput" = {
                                            support('d3plus') # will be embedded
                                            wdth = items[[i]]$width  %>% verify(c('character'), lengths = 1, default = '100%', varname = "items[['" %++% i %++% "']]$width")
                                            hght = items[[i]]$height %>% verify(c('character'), lengths = 1, default = '500px', varname = "items[['" %++% i %++% "']]$height")
                                            items[[i]]$object <<- d3plusOutput(i, width  = wdth, height = hght)},
                                          "rChartsdPlotOutput" = {items[[i]]$object <<- showOutput(i, "dimple")})
                                   
                                   if(!is.null(items[[i]]$tutor.lesson)){
                                     settings$tutorMode <<- T
                                     items[[i]]$object <<- rintrojs::introBox(
                                       items[[i]]$object, 
                                       data.intro = items[[i]]$tutor.lesson %>% verify('character') %>% paste(collapse = '\n'), 
                                       data.hint  = items[[i]]$tutor.hint %>% verify('character') %>% paste(collapse = '\n'),
                                       data.step  = items[[i]]$tutor.step)
                                   }
                                 }
                               }
                             }
                             
                             # Create input objects:
                             for (i in names(items)){
                               # Inputs & Containers
                               if (items[[i]]$type %in% c(valid.input.types, valid.container.types)){
                                 items[[i]]$object <<- getItemObject(i)
                               }
                             }
                             
                             scr.text = layscript(layout = king.layout) %++% ")"
                             
                             ui.obj <- eval(parse(text = scr.text))
                             return(ui.obj)
                           },
                           
                           dashboard.server = function(){
                             srv_func = function(input, output, session) {
                               # a list of objects which are synced with dashboard inputs and
                               # provide service for dashboard outputs
                               if(settings$keepSessions){sessions <<- c(sessions, session)}
                               sync    = reactiveValues(user = session$user, message = messages['initial'])
                               report  = reactiveValues()
                               for(i in names(objects)){session$userData[[i]] = objects[[i]]}
                               
                               for (i in names(values)){sync[[i]] = values[[i]]}
                               
                               itns = names(items)
                               for (i in itns){
                                 if(inherits(items[[i]]$sync, c('logical', 'numeric', 'integer')) & !is.empty(items[[i]]$sync)){
                                   if(items[[i]]$sync){
                                     switch(items[[i]]$type,
                                            "TFD3Output"  =
                                            {   cfg_i = items[[i]]$config %>% TFD3.config.verify
                                            tbl_i = items[[i]]$data %>% verify('data.frame', null_allowed = F, err_msg = "todo: Write something!")
                                            # reporting and commanding values both client to server and server to client:
                                            sync[[i]]   <- tbl_i
                                            report[[i]] <- tbl_i
                                            sync[[i %++% '_trigger']] = T
                                            if(!is.null(cfg_i$column.footer)){
                                              sync[[i %++% '_column.footer']] = cfg_i$column.footer
                                              observers <<- c(observers, TFD3.observer.column.footer.R(i))
                                            }
                                            if(!is.null(cfg_i$column.editable)){
                                              sync[[i %++% '_column.editable']] = cfg_i$column.editable
                                              report[[i %++% '_lastEdits']] = TFD3.lastEdits.empty
                                              observers <<- c(observers, TFD3.observer.edit.R(i), TFD3.observer.column.editable.R(i))
                                            }
                                            if(!is.null(cfg_i$selection.mode)){
                                              sync[[i %++% '_selected']]  = cfg_i$selected
                                              report[[i %++% '_selected']]  = cfg_i$selected
                                              observers <<- c(observers, TFD3.observer.selected.C2S.R(i), TFD3.observer.selected.S2C.R(i))
                                            }
                                            if (cfg_i$column.filter.enabled){
                                              sync[[i %++% '_column.filter']] = cfg_i$column.filter
                                              report[[i %++% '_filtered']]  = tbl_i %>% TFD3.filteredRows(cfg_i)
                                              observers <<- c(observers, TFD3.observer.filter.C2S.R(i), TFD3.observer.filter.S2C.R(i))
                                            }
                                            sync[[i %++% '_row.color']] = cfg_i$row.color
                                            observers <<- c(observers, TFD3.observer.color.S2C.R(i))
                                            observers <<- c(observers, TFD3.observer.table.S2C.R(i))
                                            items[[i]]$service <<- TFD3.service(i)
                                            }
                                     )
                                   }
                                 }
                               }
                               
                               if(!is.null(prescript)){eval(parse(text = prescript))}
                               
                               for (i in names(items)){
                                 if (items[[i]]$type %in% valid.output.types){
                                   if (items[[i]]$type != 'static'){
                                     arguments = ''
                                     switch(items[[i]]$type,
                                            "dynamicInput"       = {script.func = gndcd(1,9, 14, 20, 9,  1, 35, 42)},
                                            "loginInput"         = {
                                              script.func = gndcd(1,9, 14, 20, 9,  1, 35, 42)
                                              items[[i]]$service <<- loginUIService
                                            },
                                            "uiOutput"           = {script.func = gndcd(31,29, 14, 20, 29, 31, 47, 9, 50, 25)},
                                            "plotOutput"         = {script.func = gndcd(31,29, 14, 20, 9, 31, 32, 55, 60, 21)},
                                            "verbatimTextOutput" = {script.func = gndcd(110,94,14,7,94,1, 5,110, 118,27,25)},
                                            "textOutput"         = {script.func = gndcd(31, 29, 183,7,130,121,160,94, 167,196)},
                                            "tableOutput"        = {
                                              script.func = 'renderTable';
                                              arguments   = list2Script(items[[i]], fields = c('striped', 'hover', 'bordered', 'spacing', gndcd(56,4,150,196,86), 'align', 'rownames', 'colnames', 'digits', 'na'))
                                            },
                                            "dataTableOutput"    = {
                                              script.func = gndcd(67,47) %++% "::" %++% gndcd(110,9,134,7,162,121,61,39,25,11,160,2,190,171,130)
                                              arguments   = list2Script(items[[i]], fields = c('options'))
                                            },
                                            "TFD3Output"         = {script.func = 'renderTFD3'},
                                            "grvizOutput"        = {script.func = 'renderGrviz'},
                                            "c3Output"           = {script.func = 'c3::renderC3'},
                                            "sankeyNetworkOutput"= {script.func = 'networkD3::renderSankeyNetwork'},
                                            "sunburstOutput"     = {script.func = 'sunburstR::renderSunburst'},
                                            "rHandsonTableOutput"= {script.func = 'renderRHandsontable'},
                                            "pivot"              = {script.func = 'renderPivot'},
                                            "htmlOutput"         = {script.func = 'renderUI'},
                                            "dygraphOutput"      = {script.func = 'dygraphs::renderDygraph'},
                                            "gglVisChartOutput"  = {script.func = 'renderGvis'},
                                            "leafletOutput"      = {script.func = 'renderLeaflet'},
                                            "wordcloud2Output"   = {script.func = 'renderWordcloud2'},
                                            "infoBoxOutput"      = {script.func = 'renderInfoBox'},
                                            "valueBoxOutput"     = {script.func = 'renderValueBox'},
                                            "amChartsOutput"     = {script.func = 'renderAmCharts'},
                                            "plotlyOutput"       = {script.func = 'plotly::renderPlotly'},
                                            "highcharterOutput"  = {script.func = 'highcharter::renderHighchart'},
                                            "morrisjsOutput"     = {script.func = 'morrisjs::renderMorrisjs'},
                                            "billboarderOutput"  = {script.func = 'billboarder::renderBillboarder'},
                                            "sankeytreeOutput"   = {script.func = 'sankeytreeR::renderSankeytree'},
                                            "coffeewheelOutput"  = {script.func = 'rendercoffeewheel'},
                                            "bubblesOutput"      = {script.func = 'bubbles::renderBubbles'},
                                            "d3plusOutput"       = {script.func = 'd3plus::renderD3plus'},
                                            "rChartsdPlotOutput" = {script.func = 'renderChart2'}
                                     )
                                     if(items[[i]]$type %in% c("downloadButton", "downloadLink")){
                                       fn = items[[i]]$filename %>% verify('character', lengths = 1, default = paste0("'", i, "'"), varname = 'filename')
                                       script = paste0("output$", i, " <- downloadHandler(filename = function(){", fn, "}",
                                                       ", content = function(file){ \n", items[[i]]$service, '\n }', chif(arguments == '', '', ', '), arguments, ')')
                                     } else {
                                       script = paste0('output$', i, ' <- ', script.func, '({', items[[i]]$service, '}', chif(arguments == '', '', ', '), arguments, ')')
                                     }
                                     if(items[[i]]$type == "loginInput"){script %<>% paste('\n', loginUIExtraService, '\n')}
                                     # cat(i, ' --> ', script, '\n')
                                     eval(parse(text = script))
                                   }
                                 }
                               }
                               
                               # service popovers: HTML works with addPopover from the server but not with bsPopover in the UI!!! bug in shinyBS!
                               
                               for (i in names(items)){
                                 if(!is.null(items[[i]]$popover)){
                                   addPopover(session, id = i, 
                                              title     = items[[i]]$popover.title %>% verify('character', lengths = 1),
                                              content   = items[[i]]$popover %>% verify('character') %>% paste(collapse = '\n'), 
                                              placement = items[[i]]$popover.placement %>% verify('character', lengths = 1, domain = c('top', 'bottom', 'left', 'right')   , default = 'bottom'),
                                              trigger   = items[[i]]$popover.trigger   %>% verify('character', lengths = 1, domain = c('hover', 'focus', 'click', 'manual'), default = 'hover'),
                                              options   = items[[i]]$popover.options)
                                 }
                               }
                               
                               # observeEvents  (input service functions)
                               for (i in names(items)){
                                 if (items[[i]]$type %in% valid.input.types){
                                   if (!is.null(items[[i]]$service)){
                                     if(items[[i]]$isolate %>% verify('logical', domain = c(T,F), default = F, varname = 'isolate')){
                                       isop = gndcd(70, 144, 143, 55, 170, 196, 9) %++% '({'
                                       isoc = '})'
                                     } else {
                                       isop = ''
                                       isoc = ''
                                     }
                                     script = paste0('observeEvent(input$', i, ',{', isop, items[[i]]$service, isoc, '})')
                                     eval(parse(text = script))
                                   }
                                 }
                               }
                               
                               # observers
                               # for (obs in observers){eval(parse(text = "observe({" %++% pre.run %++% '\n' %++% obs %++% "})"))}
                               for (obs in observers){eval(parse(text = gndcd(12,142,112,130,31,100,94) %++% "({" %++% obs %++% "})"))}
                             }
                             return(srv_func)
                           }
                         )
)

# Any time you change the value of a reactive variable, within an observer code,
# you should put that code in isolate({}), because the observer will be called again!



# dashtools.R ---------------------------------------------------------------- 


# Header
# Filename:      dash.tools.R
# Description:   An ensemble of functions required for creation of elegant plots for a shiny dashboard
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    22 January 2016
# Last Revision: 19 September 2018
# Version:       0.0.5

# Version History:

# Version     Date               Action
# ----------------------------------
# 0.0.1       22 January 2016    Initial issue with name tools.R
# 0.0.2       04 July 2016       Module renamed from tools.R to dashtools.R
# 0.0.3       04 July 2016       Function io.str() works as a method of class DASHBOARD and transferred to dashboard.R
# 0.0.4       07 September 2016  Functions map.zoom() and plot.sg.map() transferred to nira.plotters.R and renamed.
# 0.0.5       19 September 2018  Functions buildStyle() added.


"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

"%|W|%" <- function(a, b) {
  if (!is.waive(a)) a else b
}

is.waive <- function(x) inherits(x, "waiver")

#' @export
plotHtml = function(obj){
  browseURL2 = function(url, height){
    browseURL(url)
  }
  options(viewer = browseURL2)
  show(obj)
}

buildStyle = function(object, inline = NULL, vertical_align = NULL, width = NULL, float = NULL){
  if(is.null(inline) & is.null(vertical_align) & is.null(width)){return(object)}
  
  style = "display:" %++% 
    chif(inline %>% verify('logical', default = F), " inline-block;", "") %++% 
    chif(is.null(vertical_align), "", " vertical-align:" %++% vertical_align %++% ";") %++% 
    chif(is.null(float), "", " float:" %++% float %++% ";") %++% 
    chif(is.null(width), "", " width:" %++% width %++% ";")
  
  div(style = style, object)  
}


# obj = actionButton(i, label = lbl, icon = icn, width = items[[i]]$width) %>% buildStyle(inline = items[[i]]$inline, vertical_align = items[[i]]$vertical_align)},

# dc.R ---------------------------------------------------------------- 



# Header
# Filename:       dc.R
# Description:    Contains functions for plotting various charts from package 'dc' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     28 July 2017
# Last Revision:  28 July 2018
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     28 July 2018      Initial issue

dc.bar.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character'),
    y       = 'numeric'),
  multiples  = c(),
  essentials = 'x'
)

dc.line.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('numeric', 'integer', 'character'),
    y       = 'numeric',
    group   = c('character', 'factor', 'integer')),
  multiples  = c(),
  essentials = 'x'
)

dc.prepareConfig = function(config){
  config %>% list.edit(
    yAxis.elastic  = T,
    yAxis.padding  = '5%',
    yAxis.offset   = 0,
    xAxis.elastic  = T,
    offset         = 0,
    legend.item.height = 13, 
    legend.item.width  = 70
  )
}

dc.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dcr), "Package dcr is not installed!", err_src = match.call()[[1]])
  
  config = dc.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'dcr')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  if(ncol(obj) == 1){obj$key = as.integer(sequence(nrow(obj)))}
  
  obj %>% dcr::dcr(type = 'column', x = x, y = y, config = config %>% dc.prepareConfig)
}

dc.scatter = function(obj, x = NULL, y = NULL, group = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dcr), "Package dcr is not installed!", err_src = match.call()[[1]])
  
  config = dc.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'dcr')
  
  shape %<>% verify('character', lengths = 1, domain = c('line', 'point'), default = 'point')
  
  if(is.null(group)){group = 'Sery'}
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  if(is.null(config$xAxis.min)){config$xAxis.min = min(obj[, L$x])}
  if(is.null(config$xAxis.max)){config$xAxis.max = max(obj[, L$x])}
  
  if(shape == 'line'){type = 'scatterLine'} else {type = 'scatterPoint'}
  
  obj %>% dcr::dcr(type = type, x = L$x, y = L$y, group = L$group, config = config %>% dc.prepareConfig)
  
}


# dialogs.R ---------------------------------------------------------------- 


# Header
# Filename:      dialogs.R
# Description:   Contains functions for building dialog boxes as niravis containers which can be embedded in a UI or dashboard
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    01 June 2018
# Last Revision: 01 June 2018
# Version:       0.1.1

# Version History:

# Version   Date                Action
# ---------------------------------------
# 0.1.0     01 June 2018        Initial issue with function build.container.get.table()
# 0.1.1     01 June 2018        Argument containerType added, specifying container type 


# This functions builds a niravis container for a dialog box in order to get a CSV table and issue out a table with desired column names and classes. 
# The returned container includes all service functions for reading and modifying the input table as well as error handling.
# todo: add Excel tables should be added
# the function returns a list of items that needs to be appended to any UI(dashboard) on which the dialog box is going to appear.
# name: name or ID of the container element
# classes: a named list containing acceptable classes of each column. list names should refer to the desired name of each column.
# outputvar: specifies the name of the final output variable. This can refer to a reactive or non-reactive global 
# variable which can be defined anywhere in global.R and is accessible in all service functions of the UI.
# There are two error (fail) messages. The error messages are '<name>.failmsg' and '<name>.failmsg2' will appear in list 'temp' which is an element of the 'userData' property of the session:
# For example, if the name of the dialogbox is 'my_dialog', error messages will be found at:
# session$userData$temp$failmsg & session$userData$temp$failmsg2
# failmsg is NULL if no file has been specified yet or the dialogbox is reset.
# failmsg is 'Ok' if table CSV file is read with no problem (No checking or verification on the contents yet)
# failmsg is 'One and only one file must be selected!' if more than one file is selected by the user
# failmsg is 'Please choose a CSV file.' if a file with extention other than .csv is selected.
# failmsg is 'Selected table has no column names' if the selected table has no column labels.
# failmsg is 'File read failed for some reason!' if the selected file is corrupt and cannot be read.
# failmsg2 is 'Ok' if all the columns inherit desired classes or have been successfully coerced to the first desired class.
# failmsg2 is 'Column x cannot be coerced to class y!' if any of the columns does not inherit desired classes and cannot be coerced to the first desired class for that column.
# arguments ... will be passes to the container list
#' @export
build.container.get.table = function(name, classes, outputvar = name %>% paste('out', sep = '.') , fileSelectTitle = 'Select input CSV file', loadButtonTitle = 'Load selected file', containerType = 'fluidPage', ...){
  colns = names(classes)
  
  items = list()
  items[[name]] = list(type = containerType %>% verify('character', lengths = 1, domain = valid.container.types, default = 'fluidPage'), ...)
  items[[name %>% paste0('.get.file')]] = list(type = 'fileInput', title = fileSelectTitle, accept = c('.csv'))
  if(containerType == 'fluidPage'){lay = list(list(name %>% paste0('.get.file'), offset = 1))} else {lay = name %>% paste0('.get.file')}
  checkstr = character()
  for(col in colns){
    itmID = paste(name, 'get', col, sep = '.')
    items[[itmID]] = list(type = 'selectInput', title = paste(col, 'Column', 'Label'))
    if(containerType == 'fluidPage'){lay %<>% list.add(list(itmID, offset = 1))} else {lay %<>% c(itmID)}
    checkstr %<>% c(paste0("if(!fail){
                           if(!inherits(session$userData$temp$TBL$", col, " , c(", paste0("'", classes[[col]],"'") %>% paste(collapse = ','), "))){
                           fail = try({session$userData$temp$TBL$", col, " %<>% coerce('", classes[[col]][1], "')}, silent = T) %>% inherits('try-error')
                           if(fail){session$userData$temp$", name, ".failmsg2 = 'Column ", col, "  cannot be coerced to ", classes[[col]][1], "!'}}}"))
}
  checkstr %<>% paste(collapse = '\n')
  updatescr = paste0("updateSelectInput(session, '", name, ".get.", colns, "', choices = ctgrs)") %>% paste(collapse = '\n')
  updatescr = paste("\n", updatescr, "\n")
  
  if(containerType == 'fluidPage'){lay %<>% list.add('loadfile')} else {lay %<>% c('loadfile')}
  items$loadfile       = list(type = 'actionButton', title = loadButtonTitle)
  items$cancel         = list(type = 'actionButton', title = 'Cancel')
  items[[name]]$layout = lay
  scr = paste0(
    "
    session$userData$temp = list(", name, ".failmsg = 'Ok')
    # Verifications
    # if(length(input$", name, ".get.file$datapath) != 1){session$userData$temp$", name, ".failmsg = 'One and only one file must be selected!'}
    ssp = strsplit(input$", name, ".get.file$name, '.', fixed = T)[[1]]
    nnn = length(ssp)
    if(ssp[nnn] != 'csv'){session$userData$temp$", name, ".failmsg = 'Please choose a CSV file.'}
    if(session$userData$temp$", name, ".failmsg == 'Ok'){
    session$userData$temp$ORGTBL = try(read.csv(input$", name, ".get.file$datapath, as.is = T), silent = T)
    if(inherits(session$userData$temp$ORGTBL, 'data.frame')){
    ctgrs = names(session$userData$temp$ORGTBL)
    if(!is.null(ctgrs)){", updatescr,
    "} else {session$userData$temp$", name, ".failmsg = 'Selected table has no column names'}
    } else {session$userData$temp$", name, ".failmsg = 'File read failed for some reason!'}
    }
    ")
  items[[name %>% paste0('.get.file')]]$service = scr
  
  inputcolnstr = paste0("input$", name, ".get.", colns) %>% paste(collapse = ' , ')  
  colnstr      = paste0("'", colns, "'") %>% paste(collapse = ' , ')  
  
  
  items$loadfile$service = paste0(
    "
    cat('Reading file: ', session$userData$temp$", name, ".failmsg, '\n')
    if(!is.null(session$userData$temp$", name, ".failmsg)){
    if(session$userData$temp$", name, ".failmsg == 'Ok'){
    session$userData$temp$", name, ".failmsg2 = 'Ok'
    session$userData$temp$TBL = session$userData$temp$ORGTBL[, c(", inputcolnstr, ")]
    names(session$userData$temp$TBL) <- c(", colnstr, ") 
    fail = F", "\n", checkstr, "
    cat('Coercing Classes: ', session$userData$temp$", name, ".failmsg2, '\n')
    if(!fail){
    OUT = session$userData$temp$TBL
    session$userData$temp$TBL <- NULL
    print(head(OUT))}}}")
  
  return(items)
}

# # Example:
# classes = list(caseID = c('character', 'factor'), activity = c('character', 'factor'), status = c('character', 'factor'), timestamp = 'POSIXct')
# 
# dash   <- new('DASHBOARD', items = build.container.get.table(name = 'my_dialog', classes = classes, containerType = 'wellPanel'), king.layout = list('my_dialog'))
# ui     <- dash$dashboard.ui()
# server <- dash$dashboard.server()
# shinyApp(ui, server)



# dimple.R ----------------------------------------------------------------


# Header
# Filename:       dimple.R
# Description:    Contains functions for plotting various dimple charts from rCharts package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     12 May 2017
# Last Revision:  27 July 2017
# Version:        1.0.5
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R
# 1.0.1     29 May 2018         Function dimple.bar.molten() added
# 1.0.2     29 May 2018         config property 'barMode' applied.
# 1.0.3     24 July 2018        Function dimple.applyConfig() added.
# 1.0.4     24 July 2018        Function dimple.combo() added.
# 1.0.5     27 July 2018        Function dimple.scatter() added.

dimple.combo.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c("character", "factor", "Date", "POSIXct", "numeric", "integer"),
    y     = c("numeric", "integer", "character", "factor", "Date", "POSIXct"),
    t     = 'factor',
    group = 'factor',
    size  = 'numeric'),
  multiples  = c('x', 'y', 'size', 'group'),
  essentials = c('x', 'y'),
  legend.enabled = F,
  colorize = F
)

dimple.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = "numeric",
    y     = "numeric",
    color = 'factor',
    t     = 'factor',
    size  = 'numeric'),
  multiples  = c('y', 'size', 'shape', 'color'),
  essentials = c('x', 'y')
)

dimple.applyConfig = function(rc, config){
  if(config$legend.enabled %>% verify('logical', default = T)){
    rc$legend(x = chif(is.null(config$legend.position.x), 0, config$legend.position.x),
              y = chif(is.null(config$legend.position.y), 0, config$legend.position.y),
              width = chif(is.null(config$legend.width), 100, config$legend.width),
              height = chif(is.null(config$legend.height), 20, config$legend.height),
              horizontalAlign = config$legend.horizontalAlign %>% verify('character', domain = c('right', 'left'), default = 'right'))
  }
  return(rc)
}


dimple.combo = function(obj, x = NULL, y = NULL, t = NULL, size = NULL, group = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  
  if(is.null(shape)){shape = 'line'}
  
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = dimple.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, t = t, size = size, group = group, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'color')
  
  if ('rownames' %in% c(L$group, L$x, L$y)){obj %<>% rownames2Column('rownames') %>% as.data.frame}
  
  obj %<>% prepare4Plot(A, config = config)
  
  xnum = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% is.numeric}) %>% sum
  xdte = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('Date')}) %>% sum
  xtim = length(L$x) == obj[, L$x, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('POSIXct')}) %>% sum
  ynum = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% is.numeric}) %>% sum
  ydte = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('Date')}) %>% sum
  ytim = length(L$y) == obj[, L$y, drop = F] %>% names %>% sapply(function(col){obj[, col] %>% inherits('POSIXct')}) %>% sum
  
  if(xdte | xtim){for (col in L$x){obj[, col] %<>% as.character}}
  if(ydte | ytim){for (col in L$y){obj[, col] %<>% as.character}}
  
  ycat = !ynum & !ydte & !ytim
  hor  = xnum & ycat
  Ly   = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x);
  Lgroup  = L$group
  
  if(!is.null(L$group)){clrvect = getColorVect(obj[, L$group %>% last] %>% unique, L$color, config)}
  
  N = length(Ly)
  if(N > 1){
    obj %<>% reshape2::melt(id.vars = Lx, measure.vars = Ly)
    series = obj[, 'variable'] %>% unique
    L$shape %<>% vect.extend(length(series))
    shapes = L$shape %>% unique
    nshape = length(shapes)
    
    dp <- dPlot(x      = chif(hor, 'value', Lx),
                y      = chif(hor, Lx, 'value'),
                z      = L$size[1],
                data   = obj %>% filter(variable %in% series[which(L$shape == shapes[1])]),
                groups = 'variable',
                type   = shapes[1], ...)
    if(nshape > 1){
      for(i in 2:nshape){
        dp$layer(x      = chif(hor, 'value', Lx),
                 y      = chif(hor, Lx, 'value'),
                 z      = L$size[i %>% min(length(L$size))],
                 data   = obj %>% filter(variable %in% series[which(L$shape == shapes[i])]),
                 groups = 'variable',
                 type   = shapes[i])
      }
    }
    clrvect = getColorVect(obj[, 'variable'] %>% unique, L$color, config)
    Lgroup  = 'variable'
  } else {
    dp <- dPlot( x      = L$x,
                 y      = L$y,
                 z      = L$size,
                 data   = obj,
                 groups = L$group,
                 type   = L$shape, ...)
    if(is.null(L$group)){clrvect = getColorVect(L$y, L$color, config)}
  }
  
  if      (xnum){dp$xAxis(type = "addMeasureAxis")} 
  else if (xdte){
    dp$xAxis(type = "addTimeAxis", inputFormat = "%Y-%m-%d", outputFormat = chif(is.null(config$xAxis.tick.label.format), "%Y-%m-%d", config$xAxis.tick.label.format))
  }
  else if (xtim){
    dp$xAxis(type = "addTimeAxis",
             inputFormat  = "%Y-%m-%d %H:%M:%S",
             outputFormat = chif(is.null(config$xAxis.tick.label.format), "%Y-%m-%d %H:%M:%S", config$xAxis.tick.label.format))
  } 
  else {for(col in L$x){obj[, col] %<>% as.character}; dp$xAxis(type = "addCategoryAxis")}
  
  if      (ynum){dp$yAxis(type = "addMeasureAxis")} 
  else if (ydte){
    dp$yAxis(type = "addTimeAxis", inputFormat = "%Y-%m-%d", outputFormat = chif(is.null(config$yAxis.tick.label.format), "%Y-%m-%d", config$yAxis.tick.label.format))
  }
  else if (ytim){
    dp$yAxis(type = "addTimeAxis",
             inputFormat  = "%Y-%m-%d %H:%M:%S",
             outputFormat = chif(is.null(config$yAxis.tick.label.format), "%Y-%m-%d %H:%M:%S", config$yAxis.tick.label.format))
  } 
  else {for(col in L$y){obj[, col] %<>% as.character}; dp$yAxis(type = "addCategoryAxis")}
  
  if(!is.null(size)){
    # Felan hamash numerice
    if(inherits(obj[, L$size], c('numeric', 'integer'))){dp$zAxis(type = "addMeasureAxis", overrideMax = config$maxSizeOverride)} else {dp$zAxis(type = "addCategoryAxis")}
  }
  dp %<>% dimple.applyConfig(config)
  dp$setTemplate(afterScript = dimple.js(field_name = Lgroup))
  if(!is.null(L$t)){dp$set(storyboard = L$t)}
  if(!is.null(clrvect)){dp$defaultColors(clrvect)}
  dp
}

dimple.scatter = function(obj, x = NULL, y = NULL, t = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if(is.null(shape)){shape = 'bubble'}
  if(is.null(color)){color = 'Series 1'}
  
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = dimple.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, t = t, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config = config);  if (is.empty(obj)){return(NULL)}
  
  obj$ID = obj %>% nrow %>% sequence
  clrvect = NULL
  
  
  N = length(L$y)
  if(N > 1){
    obj %<>% reshape2::melt(id.vars = c(L$x, L$size), measure.vars = L$y)
    obj$ID = obj %>% nrow %>% sequence
    
    dp = dPlot(
      x = L$x,
      y = 'value',
      z = L$size[1],
      groups = c('ID', 'variable'),
      data = obj,
      type = L$shape
    )
    Lgroup = 'variable'
  } else{
    dp = dPlot(
      x = L$x,
      y = L$y[1],
      z = L$size[1],
      groups = c('ID', L$color),
      data = obj,
      type = L$shape
    )
    Lgroup = L$color
  }
  
  dp$xAxis(type = "addMeasureAxis")
  dp$yAxis(type = "addMeasureAxis")
  dp %<>% dimple.applyConfig(config)
  dp$setTemplate(afterScript = dimple.js(field_name = Lgroup))
  if(!is.null(L$t)){dp$set(storyboard = L$t)}
  if(!is.null(clrvect)){dp$defaultColors(clrvect)}
  dp
}


show = function(rc) 
{
  dir.create(temp_dir <- tempfile(pattern = "rCharts"))
  writeLines(rc$render(static = F), tf <- file.path(temp_dir, "index.html"))
  suppressMessages(copy_dir_(rc$LIB$url, file.path(temp_dir, rc$LIB$name)))
  visfun = options('viewer')[[1]]
  visfun(tf)
}


# DT.R ----------------------------------------------------------------


# Header
# Filename:       DT.R
# Description:    Contains functions for plotting various table charts from DT package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     25 December 2016
# Last Revision:  28 February 2018
# Version:        1.1.6
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     25 December 2016    Initial issue transferred from niraPlotter.R
# 1.1.0     27 March 2017       Fundamental changes with standard arguments. Calls function prepare4Plot to generate considerations for special dimensions
# 1.1.1     28 March 2017       DT.defset added
# 1.1.4     23 February 2018    Functions DT.applyConfig2Obj() and DT.applyConfig() and DT.options() added. Currently only take care of property 'links'
# 1.1.5     28 February 2018    config property 'paging.length' added.
# 1.1.5     28 February 2018    config property 'autoWidth' added.

#' @include visgen.R

# Default settings for package DT:
DT.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(label = valid.classes),
  multiples = 'label',
  withRowNames  = T
)

# Extra Arguments for function DT.table:
# Legend for setting argument class:
# display:	    Short-hand for the stripe, hover, row-border and order-column classes.
# cell-border:  Border around all four sides of each cell
# compact:      Reduce the amount of white-space the default styling for the DataTable uses, increasing the information density on screen (since 1.10.1)
# hover:        Row highlighting on mouse over
# nowrap:       Disable wrapping of content in the table, so all text in the cells is on a single line (since 1.10.1)
# order-column: Highlight the column that the table data is currently ordered on
# row-border:   Border around only the top an bottom of each each (i.e. for the rows).
# stripe:       Row striping
#
# You can use multiple options together. For example: style = 'cell-border stripe'. Note cell-border and row-border are mutually exclusive and cannot be used together.


# New function for plotting DT tables
DT.table = function(obj, label = NULL, config = NULL, sessionobj = NULL, ...){
  #if (is.empty(obj)){return(NULL)}
  
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  assert(require(DT), "Package DT is not installed!", err_src = match.call()[[1]])
  config = DT.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  label %<>% nameList(gndcd(171,39,69,162,199))
  
  labelL        = names(label)
  
  obj %<>% prepare4Plot(list(label = label), config = config) %>% DT.applyConfig2Obj(config)
  
  DT::datatable(obj, escape = F, options = config %>% DT.options(obj, sessionobj), ...) %>% DT.applyConfig(config)
}



# This function changes the data.frame according to the config and returns the modified data.frame
# input argument obj must be the data.frame which is going to be plotted.
# This function currently only takes care of property 'links' in the config
# If any other properties impact the data.frame to be plotted, you should add it here
DT.applyConfig2Obj = function(obj, config){
  if(!is.null(config$links)){
    verify(config$links, 'list')
    for (i in sequence(length(config$links))){
      link = config$links[[i]]
      nms  = names(link)
      # verify(link, 'list', names_include = c('colName'), varname = 'link')
      # verify(link$column.name, 'character')
      
      scr = '<a '
      
      if ('class.name' %in% nms){scr = paste0(scr, 'class="', link$class.name, '" ')}
      if ('href'       %in% nms){scr = paste0(scr, 'href="', link$href, '" ')}
      
      if ('inputs'       %in% nms){
        for (j in names(link$inputs)){
          scr = paste0(scr, ' data-', j, "=\"", obj[,link$inputs[j]], "\"")
        }
      }
      scr = paste0(scr, '>')
      if ('icon' %in% nms){scr = paste0(scr, '<i class="', link$icon, '"></i>')}
      if ('text' %in% nms){scr = paste0(scr, link$text)}
      scr = paste0(scr, '</a>')
      
      if(nrow(obj) == length(scr)){obj[, link$column.name] = scr}
    }
  }
  return(obj)
}


# This function applies config to the DT object
DT.applyConfig = function(obj, config){
  return(obj)
}

# This function modifies the options argument that needs to be passed to the plotter function
# in order to apply  some of the config properties to the plot.
# todo: Create appropriate config parameters to add these options https://datatables.net/reference/option/
# Currently only respects property 'links'
DT.options = function(config, df, sessionobj = NULL){
  opt = config %>% list.extract('autoWidth')
  if(!is.null(config$links) & !is.null(sessionobj)){
    action   <- DT::dataTableAjax(sessionobj, df)
    opt$ajax %<>% verify('list', default = list()) %>% list.edit(url = action)
  }
  
  opt$pageLength = config$paging.length %>% verify(c('integer', 'numeric'), lengths = 1, domain = c(1,Inf), default = 10)
  return(opt)
}

# Old functions
#' @export
DT.Table = function(x, links = NULL, session = NULL, options = list(), rownames = T){
  TBL = x
  opt = options
  
  if (!is.null(options$header)){
    scr = paste0("$(this.api().table().header()).css(", list2Json(options$header, quotation = T), ");")
    opt$initComplete = JS("function(settings, json) {", scr, "}")
    opt$header = NULL
  }
  
  if (!is.null(links)){
    verify(links, 'list')
    for (i in sequence(length(links))){
      link = links[[i]]
      nms  = names(link)
      # verify(link, 'list', names_include = c('colName'), varname = 'link')
      # verify(link$column.name, 'character')
      
      scr = '<a '
      
      if ('class.name' %in% nms){scr = paste0(scr, 'class="', link$class.name, '" ')}
      if ('href'       %in% nms){scr = paste0(scr, 'href="', link$href, '" ')}
      
      if ('inputs'       %in% nms){
        for (j in names(link$inputs)){
          scr = paste0(scr, ' data-', j, "=\"", TBL[,link$inputs[j]], "\"")
        }
      }
      scr = paste0(scr, '>')
      if ('icon' %in% nms){scr = paste0(scr, '<i class="', link$icon, '"></i>')}
      if ('text' %in% nms){scr = paste0(scr, link$text)}
      scr = paste0(scr, '</a>')
      
      if(nrow(TBL) == length(scr)){TBL[, link$column.name] = scr}
    }
    action   <- DT::dataTableAjax(session, TBL)
    opt$ajax <- list(url = action)
  }
  return(DT::datatable(TBL, escape = FALSE, options = opt, rownames = rownames))
}

# Functions for customized modifications: (Old Function)
#' @export
DT.Table.Classify.bold = function(x, classColumn, boldValues = 'TRUE'){
  assert(inherits(x, 'datatables'), "Class verification failed! Argument 'x' must be of class 'datatables'!")
  nms  = names(x$x$data)
  lvls = unique(x$x$data[,classColumn])
  vals = rep('normal', length(lvls))
  vals[lvls %in% boldValues] = 'bold'
  DT::formatStyle(x, classColumn, target = 'row', fontWeight = DT::styleEqual(levels = lvls, values = vals))
}



# dygraphs.R ----------------------------------------------------------------

# Header
# Filename:       dygraphs.R
# Description:    Contains functions for plotting various time series charts from dygraphs package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     29 December 2016
# Last Revision:  19 July 2017
# Version:        1.2.4
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     29 December 2016    Initial issue transferred from niraPlotter.R
# 1.0.1     02 February 2017    Function dygraphs.tsline.timeBreak.plot() modified: inputs a data.frame() instead of a TIME.SERIES object.
# 1.1.0     02 February 2017    Function dygraphs.tsline.timeBreak.plot() removed and function dygraphs.line.plot() added
# 1.1.1     09 March 2017       Function dygraphs.line.plot() renamed to dygraphs.line()
# 1.2.0     28 March 2017       Fundamental changes: Calling prepare4Table() and using standard inputs
# 1.2.1     28 March 2017       dygraphs.defset added.
# 1.2.2     31 March 2017       Calls prepareAesthetics() for preparation of aesthetics before creating the table
# 1.2.3     19 July 2018        shape and color removed from A$aesthetics list
# 1.2.4     19 July 2018        subchart feature added configured by a set of properties in config 
# 1.2.4     28 September 2018   Function() asISO8601Time() modified: Forces time to GMT

#' @include jscripts.R

dygraphs.combo.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("POSIXct", "Date", "character", "factor", "numeric", "integer"),
    y = 'numeric',
    y2 = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y or y2')
)

dygraphs.tsline.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("POSIXct", "Date"),
    y = 'numeric',
    y2 = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y or y2')
)

asISO8601Time <- function(x) {
  x %<>% lubridate::force_tz('GMT')
  if (!inherits(x, "POSIXct"))
    x <- as.POSIXct(x, tz = "GMT")
  format(x, format = "%04Y-%m-%dT%H:%M:%OS3Z", tz = 'GMT')
}

dychart = function (data, main = NULL, xlab = NULL, ylab = NULL, periodicity = NULL, 
                    group = NULL, elementId = NULL, width = NULL, height = NULL) 
{
  if (xts::xtsible(data)) {
    if (!xts::is.xts(data)) 
      data <- xts::as.xts(data)
    format <- "date"
  }
  else if (is.list(data) && is.numeric(data[[1]])) {
    if (is.null(names(data))) 
      stop("For numeric values, 'data' must be a named list or data frame")
    format <- "numeric"
  }
  else {
    stop("Unsupported type passed to argument 'data'.")
  }
  if (format == "date") {
    if (is.null(periodicity)) {
      if (nrow(data) < 2) {
        periodicity <- defaultPeriodicity(data)
      }
      else {
        periodicity <- xts::periodicity(data)
      }
    }
    time <- time(data)
    data <- zoo::coredata(data)
    data <- unclass(as.data.frame(data))
    timeColumn <- list()
    timeColumn[[periodicity$label]] <- asISO8601Time(time)
    data <- append(timeColumn, data)
  }
  else {
    data <- as.list(data)
  }
  attrs <- list()
  attrs$title <- main
  attrs$xlabel <- xlab
  attrs$ylabel <- ylab
  attrs$labels <- names(data)
  attrs$legend <- "auto"
  attrs$retainDateWindow <- FALSE
  attrs$axes$x <- list()
  attrs$axes$x$pixelsPerLabel <- 60
  x <- list()
  x$attrs <- attrs
  x$scale <- if (format == "date") 
    periodicity$scale
  else NULL
  x$group <- group
  x$annotations <- list()
  x$shadings <- list()
  x$events <- list()
  x$format <- format
  attr(x, "time") <- if (format == "date") 
    time
  else NULL
  attr(x, "data") <- data
  attr(x, "autoSeries") <- 2
  names(data) <- NULL
  x$data <- data
  htmlwidgets::createWidget(name = "dygraphs", x = x, width = width, 
                            height = height, htmlwidgets::sizingPolicy(viewer.padding = 10, 
                                                                       browser.fill = TRUE), elementId = elementId)
}

dygraphs.applyConfig = function(d, config){
  if(config$subchart.enabled){
    d %<>% dyRangeSelector(dateWindow = c(config$xAxis.min, config$xAxis.max), height = config$subchart.height,
                           fillColor = '#' %++% config$subchart.background, strokeColor = '#' %++% config$subchart.color, 
                           keepMouseZoom = config$subchart.zoom.enabled,
                           retainDateWindow =  FALSE)
  }
  return(d)
}

dygraphs.combo = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(dygraphs), "Package dygraphs is not installed!", err_src = match.call()[[1]])
  config = dygraphs.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'dygraphs') %>% verifyConfigDimProperties(dims = 'color')
  
  scatterMode = T
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if (!inherits(obj[, L$x], c('numeric', 'integer'))){
    dates = try(as.Date(obj[, L$x]), silent = T)
    if(inherits(dates,'Date')){
      obj %<>% distinct(.keep_all = T)
      rownames(obj) <- obj[, L$x]
    } else {
      getLabel <- 'function(d){
      var labels = [' %++% paste(paste0('"', obj[, L$x], '"'), collapse = ',') %++% '];
      if (Math.round(d) == d){return labels[d - 1]} else {return ""}
    }'
      obj[, L$x] <- sequence(nrow(obj))
      scatterMode = F
  }
}
  
  d = dychart(obj, main = config$title, ...)
  
  dygraphs.strokePattern = c(line.dot = 'dotted', line.dash = 'dashed', line.dash.dot = 'dotdash')
  
  for(i in L$y){
    d %<>% dySeries(i,
                    label         = i,
                    color         = config$color[[i]],
                    axis          = 'y',
                    stepPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.step'),
                    stemPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'bar.point'),
                    fillGraph     = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == gndcd(2,1,162,170)),
                    drawPoints    = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.point'),
                    pointSize     = config$size[[i]],
                    strokePattern = chif(config$shape[[i]] %in% names(dygraphs.strokePattern), dygraphs.strokePattern[config$shape[[i]]], NULL),
                    plotter       = chif(config$shape[[i]] == 'bar', dyPlotter[['bar']], NULL)
    )
  }
  # package has a bug! colors are not assigned to series names correctly!!
  for(i in L$y2){
    d %<>% dySeries(i,
                    label         = i,
                    color         = config$color[[i]],
                    axis          = 'y2',
                    stepPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.step'),
                    stemPlot      = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'bar.point'),
                    fillGraph     = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == gndcd(2,1,162,170)),
                    drawPoints    = chif(is.null(config$shape[[i]]), NULL, config$shape[[i]] == 'line.point'),
                    pointSize     = config$size[[i]],
                    strokePattern = chif(config$shape[[i]] %in% names(dygraphs.strokePattern), dygraphs.strokePattern[config$shape[[i]]], NULL),
                    plotter       = chif(config$shape[[i]] == 'bar', dyPlotter[['bar']], NULL)
    )
  }
  if(!scatterMode){d %<>% dyAxis("x", valueFormatter=JS(getLabel), axisLabelFormatter = JS(getLabel))}
  return(d %>% dygraphs.applyConfig(config))
  }

dygraphs.tsline = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  config = dygraphs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  config$shape = NULL
  dygraphs.combo(obj, x = x, y = y, y2 = y2, config = config, ...)
}

# OLD FUNCTIONS:

# dygraphs.line = function(obj, x = NULL, y = NULL, rownamesAsX = T, ...){
#   y = verify(y, 'character', domain = numerics(obj), default = numerics(obj), varname = 'y')
#   if (rownamesAsX){
#     lbl = rownames(obj)
#     obj = cbind(seq(lbl), obj[, y, drop = F])
#     names(obj)[1] <- x
#   } else {
#     lbl = obj[, x]
#     obj = obj[, y]
#     obj[, x] <- seq(lbl)
#   }
# 
#   getLabel <- 'function(d){
#   var labels = [' %++% paste(paste0('"', lbl, '"'), collapse = ',') %++% '];
#   return labels[d - 1];
# }'
# 
#   dygraph(obj, ...) %>% dyAxis("x", valueFormatter=JS(getLabel), axisLabelFormatter = JS(getLabel))
#   }
# 
# dygraphs.tsline.settings = list(
#   title = NULL, xlab = NULL, ylab = NULL, width = NULL, height = NULL,
#   # legend = list(show  = "auto", width = 250, showZeroValues = TRUE, labelsDiv = NULL, labelsSeparateLines = FALSE, hideOnMouseOut = TRUE),
#   RangeSelector = list(dateWindow = NULL, height = 40, fillColor = " #A7B1C4", strokeColor = "#808FAB", keepMouseZoom = TRUE, retainDateWindow = FALSE),
#   Roller = list(showRoller = TRUE, rollPeriod = 1)
# )
# 
# #' This function gets a dygraph plot and dygraph configuration as input and
# #' Returns the modified plot affected by given configuration
# dygraphs.tsline.apply.settings = function(plt, config){
#   if (!is.null(config$legend)){
#     plt = plt %>% dyLegend(show = config$legend$show, width = config$legend$width, showZeroValues = config$legend$showZeroValues, labelsDiv = config$legend$labelsDiv, labelsSeparateLines = config$legend$labelsSeparateLines, hideOnMouseOut = config$legend$hideOnMouseOut)
#   }
#   if (!is.null(config$RangeSelector)){
#     plt = plt %>% dyRangeSelector(dateWindow = config$RangeSelector$dateWindow, height = config$RangeSelector$height, fillColor = config$RangeSelector$fillColor, strokeColor = config$RangeSelector$strokeColor, keepMouseZoom = config$RangeSelector$keepMouseZoom, retainDateWindow = config$RangeSelector$retainDateWindow)
#   }
#   if (!is.null(config$Roller)){
#     plt = plt %>% dyRoller(showRoller = config$Roller$showRoller, rollPeriod = config$Roller$rollPeriod)
#   }
# 
#   plt = plt %>% dyOptions(
#     stackedGraph = verify(config$stackedGraph, 'logical', default = F, varname = 'config$stackedGraph'),
#     fillGraph    = verify(config$fillGraph   , 'logical', default = F, varname = 'config$fillGraph'),
#     colors       = verify(config$colors, 'character', varname = 'config$colors')
#     # todo: write all the settings tuned by function dyOptions()
#   )
#   return(plt)
# }
# 
# # Customized modifications for dygraphs plots:
# dygraph.Highlight.1 = function(x){
#   verify(x, 'dygraphs')
#   dyHighlight(x, highlightCircleSize = 5,
#               highlightSeriesBackgroundAlpha = 0.2,
#               hideOnMouseOut = TRUE)
# }
# 



##########################################################################
# Java plotters:

dyPlotter = list(bar = dygraphs.shape.bar.js, multiBar = dygraphs.shape.multibar.js, candle = dygraphs.shape.candle.js)



# echarts.R ----------------------------------------------------------------



# Header
# Filename:       echarts.R
# Description:    Contains functions for plotting various charts from js package 'echarts' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     19 June 2018
# Last Revision:  19 June 2018
# Version:        0.1.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     19 June 2018       Initial issue with bar, funnel, wordCloud and gauge

echarts.bar.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'character',
    y       = 'numeric'),
  multiples  = 'y',
  essentials = c('x', 'y')
)

echarts.funnel.defset = defset %>% list.edit(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric'),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.wordCloud.defset = defset %>% list.edit(
  dimclass   = list(
    label    = 'character',
    size     = 'numeric',
    color    = c('character', 'numeric', 'integer', 'factor')),
  multiples  = c(),
  essentials = c('label', 'size')
)

echarts.build_data <- function(e, ...){
  e$x$data %>% 
    dplyr::select(...) %>% 
    unname(.) -> data
  
  apply(data, 1, function(x){
    list(value = unlist(x, use.names = FALSE))
  }) 
  
}
echarts.add_bind <- function(e, l, bind, col = "name"){
  e$x$data %>% 
    dplyr::select(bind) %>% unname() %>% unlist() -> bind
  
  for(i in 1:length(l)){
    l[[i]][[col]] <- bind[i]
  }
  l
}

echarts.get_data <- function(e, serie){
  e$x$data %>% 
    dplyr::select(serie) %>% 
    unname() %>% 
    .[[1]]
}


ebar <- function(e, serie, bind = NULL, name = NULL, legend = TRUE, y.index = 0, x.index = 0, ...){
  if(missing(e))
    stop("must pass e", call. = FALSE)
  
  if(missing(serie))
    stop("must pass serie", call. = FALSE)
  
  if(is.null(name)) # defaults to column name
    name <- serie
  
  if(y.index != 0)
    e <- .set_y_axis(e, serie, y.index)
  
  if(x.index != 0)
    e <- .set_x_axis(e, x.index)
  
  # build JSON data
  echarts.build_data(e, e$x$mapping$x, serie) -> vector
  
  if(!is.null(bind))
    vector <- echarts.add_bind(e, vector, bind)
  
  serie <- list(
    name = name,
    type = "bar",
    data = vector,
    yAxisIndex = y.index,
    xAxisIndex = x.index,
    ...
  )
  
  if(isTRUE(legend))
    e$x$opts$legend$data <- append(e$x$opts$legend$data, list(name))
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
}

efunnel <- function(e, values, labels, name = NULL, legend = TRUE, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(values) || missing(labels))
    stop("missing values or labels", call. = FALSE)
  
  # remove axis
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  # build JSON data
  funnel <- echarts.build_data(e, values)
  
  funnel <- echarts.add_bind(e, funnel, labels)
  
  serie <- list(
    name = name,
    type = "funnel",
    data = funnel,
    ...
  )
  
  # addlegend
  
  if(isTRUE(legend)){
    legend <- echarts.get_data(e, labels) %>% as.character()
    e$x$opts$legend$data <- append(e$x$opts$legend$data, legend)
  }
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  e
  
}

ecloud <- function(e, word, freq, color = NULL, rm.x = TRUE, rm.y = TRUE, ...){
  
  if(missing(e))
    stop("missing e", call. = FALSE)
  
  e <- .rm_axis(e, rm.x, "x")
  e <- .rm_axis(e, rm.y, "y")
  
  data <- echarts.build_data(e, freq)
  data <- echarts.add_bind(e, data, word)
  
  if(!is.null(color)){
    color <- echarts.get_data(e, color)
    for(i in 1:length(data)){
      col <- list(
        normal = list(
          color = color[i]
        )
      )
      data[[i]]$textStyle <- col
    }
  }
  
  serie <- list(
    type = "wordCloud",
    data = data,
    ...
  )
  
  e$x$opts$series <- append(e$x$opts$series, list(serie))
  
  e
  
}

egauge <- function(e, theta, label, config = NULL, ...){
  if(missing(theta) || missing(label))
    stop("missing label, or theta", call. = FALSE)
  
  if(!inherits(theta, "numeric"))
    stop("must pass numeric or interger", call. = FALSE)
  
  # remove axis
  e %<>% .rm_axis(T, "x") %>% .rm_axis(T, "y")
  
  e$x$opts$series <- list(
    list(
      type = "gauge",
      data = list(list(value = theta, name = label)),
      ...
    )
  )
  e
}

echarts.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])
  
  config = echarts.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  obj %>% e_charts_(L$x) -> e
  for(i in L$y){
    e %<>% ebar(i)
  }
  return(e)
}

echarts.funnel = function(obj, label = NULL, size = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])
  
  config = echarts.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  # todo: apply grioup_by with  config$aggrigator.function, not required if NULL
  # todo: check for column names with space
  obj %>% e_charts() %>% efunnel(L$size, L$label, ...)
}

echarts.wordCloud = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(echarts4r), "Package echarts4r is not installed!", err_src = match.call()[[1]])
  
  config = echarts.wordCloud.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'echarts')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  obj %>% e_charts() %>% ecloud(L$label, L$size, L$color, shape = config$cloud.shape, sizeRange = c(config$point.size.min, config$point.size.max))
}



# ggvis.R ----------------------------------------------------------------


# Header
# Filename:       ggvis.R
# Description:    Contains functions for plotting various charts from package 'gvis' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     14 April 2017
# Last Revision:  14 April 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     14 April 2017      Initial issue


ggvis.shape = c('+' = 'cross', plus = 'cross')


ggvis.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

ggvis.histogram.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric'),
  multiples  = c(),
  essentials = c('x')
)

ggvis.scatter = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(ggvis), "Package ggvis is not installed!", err_src = match.call()[[1]])
  
  config = ggvis.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color, extend = c('y', 'x', 'size', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  # if(is.null(L$shape)){L$shape = 'point'}
  # L$shape = translateShape[L$shape]
  # names(L$shape) <- NULL
  symbol = L$shape
  
  
  scr = paste("obj %>% ggvis(x = ~", L$x, ", y = ~", L$y,
              chif(is.null(config$point.color),
                   chif(is.null(L$color), "",
                        chif(inherits(obj[, L$color], 'character'),
                             ", fill := '" %++% obj[1, L$color] %++% "'",
                             ", fill = ~" %++% L$color)),
                   ", fill  := '" %++% config$point.color %++% "'"),
              chif(is.null(config$point.size ),
                   chif(is.null(L$size ), "" ,
                        ", size = ~" %++% L$size),
                   ", size  :=  " %++% config$point.size),
              chif(is.null(config$point.shape),
                   chif(is.null(L$shape), "" ,
                        chif(inherits(obj[, L$shape], 'character'),
                             ", shape := '" %++% ggvis.shape[obj[1, L$shape]]  %++% "'",
                             ", shape = ~" %++% L$shape)),
                   ", size  :=  '" %++% ggvis.shape[config$point.shape]  %++% "'"),
              chif(is.null(config$point.border.color), "" , ", stroke := '" %++% config$point.border.color %++% "'"),
              chif(is.null(config$point.opacity), "" , ", opacity := " %++% config$point.opacity), ", ...)",
              chif(is.null(config$point.tooltip), "" , "%>% add_tooltip(" %++% config$point.tooltip %++% ")"),
              "%>% layer_points")
  
  parse(text = scr) %>% eval
}

ggvis.histogram = function(obj, x = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(ggvis), "Package ggvis is not installed!", err_src = match.call()[[1]])
  
  config = ggvis.histogram.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  g   = obj %>% ggvis(as.formula('~' %++% L$x))
  scr = paste("layer_histograms(g ",
              chif(is.null(config$hist.bar.width), "", ", width = " %++% config$hist.bar.width),
              chif(is.null(config$hist.bar.center), "", ", center = " %++% config$hist.bar.center), ")")
  parse(text = scr) %>% eval
  
}


# googleVis.R ----------------------------------------------------------------

# Header
# Filename:       googleVis.R
# Description:    Contains functions for plotting various googleVis charts using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     06 October 2016
# Last Revision:  20 June 2018
# Version:        1.3.4
#

# Version History:

# Version   Date               Action
# -----------------------------------
# 1.0.0     06 October 2016    Initial issue transferred from niraPlotter.R
# 1.1.0     25 November 2016   function googleVis.lineChart() modified: Turned to standard horizontal barChart and lineChart inputs: currently only takes x, y where x is nominal and y is a set of numeric figures
# 1.2.0     29 November 2016   Sankey chart added: Function googleVis.sankeyChart()
# 1.2.1     01 March 2017      Function googleVis.lineChart() renamed to googleVis.line() and modified: Calls verifyPlotInputs() in visgen.R to verify inputs.
# 1.2.2     01 March 2017      Function googleVis.barChart() renamed to googleVis.bar() and modified: Calls verifyPlotInputs() in visgen.R to verify inputs.
# 1.2.3     01 March 2017      Function googleVis.gauge() modified: Calls verifyPlotInputs() in visgen.R to verify inputs, also supports multiple columns for argument 'theta'
# 1.3.0     02 April 2017      Fundamental changes to the functions: Using standard visgen.R fucnctions prepare4Plot() and prepareAesthetics()
# 1.3.2     19 June 2018       Function googleVis.gauge() added.
# 1.3.4     20 June 2018       Function googleVis.bar() and googleVis.line() modified: y2 is now a new diemnsion


googleVis.line.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = 'character',
    y     = 'numeric',
    y2    = 'numeric'),
  multiples  = c('y', 'y2'),
  essentials = c('x', 'y')
)

googleVis.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'numeric', 'factor', 'integer'),
    y     = c('numeric', 'character', 'factor', 'integer')),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

googleVis.gauge.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    label = 'character',
    theta = c('numeric', 'integer')),
  multiples  = c(),
  essentials = c('label', 'theta')
)

googleVis.sankey.defset = defset %>% list.edit(
  dimclass = list(
    key          = 'character',
    source       = 'character',
    target       = 'character',
    linkWidth    = 'numeric'),  
  multiples  = c(),
  essentials = c('key', 'source', 'target')
)

googleVis.options = function(config, Ly, Ly2){
  opt = list()
  
  # Specify y axis sides and labels:
  yy = c(L$y,L$y2)
  N = length(yy)
  srs = '['
  vax = '['
  for(i in sequence(N)){
    srs %<>% paste0(chif(srs == '[', "", ","), "{targetAxisIndex:", (yy[i] %in% L$y2) %>% as.integer, "}")
    if((yy[i] %in% L$y) & (!is.null(config$yAxis.label))){
      vax %<>% paste0(chif(vax == '[', "", ","), "{title:'", config$yAxis.label, "'}")
    }  
    if((yy[i] %in% L$y2) & (!is.null(config$y2Axis.label))){
      vax %<>% paste0(chif(vax == '[', "", ","), "{title:'", config$y2Axis.label, "'}")
    }  
  }
  opt$series = srs %>% paste0("]")
  opt$vAxes  = vax %>% paste0("]")
  return(opt)
}

googleVis.line = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  gvisLineChart(obj, xvar = L$x, yvar = yy, options = googleVis.options(config, L$y, L$y2), ...)
}

googleVis.area = function(obj, x = NULL, y = NULL, y2 = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, y2 = y2, extend = c('y', 'y2'))
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(138,166,118,20,94))
  
  obj %<>% prepare4Plot(A, config = config)
  
  gvisAreaChart(obj, xvar = L$x, yvar = L$y, options = googleVis.options(config, L$y, L$y2), ...)
}

googleVis.bar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, extend = c())
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(138,166,118,20,94))
  
  obj %<>% prepare4Plot(A, config = config)
  
  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor'))}
  
  options = list()
  
  if(hor){
    g = gvisBarChart(obj, yvar = L$x, xvar = L$y, options = options, ...)}
  else {
    g = gvisColumnChart(obj, xvar = L$x, yvar = L$y, options = options, ...)
  }
  return(g)
}


googleVis.gauge = function(obj = data.frame(), theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  config = googleVis.gauge.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(is.empty(obj)){return(NULL)}
  
  obj[, c(L$label, L$theta)] %>% 
    gvisGauge(
      options = list(
        min = config$theta.min,
        max = config$theta.max,
        greenFrom    = config$thetaAxis.zone[[1]]$min,
        greenTo      = config$thetaAxis.zone[[1]]$max,
        yellowFrom   = config$thetaAxis.zone[[2]]$min,
        yellowTo     = config$thetaAxis.zone[[2]]$max,
        redFrom      = config$thetaAxis.zone[[3]]$min,
        redTo        = config$thetaAxis.zone[[3]]$max, ...)
    )
}

# It is a sankeytree not a sankey!!!!
googleVis.sankey = function(obj, key = NULL, source = NULL, target = NULL, linkWidth = NULL, config = NULL){
  obj %>% verify('list', lengths = 2, names_identical = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'googleVis.sankey')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'googleVis.sankey')
  
  assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
  
  config = googleVis.sankey.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'googleVis')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  A = a$aesthetics
  
  obj$nodes %<>% prepare4Plot(A %>% list.extract('key'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkWidth'), config)
  
  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'googleVis.sankey')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'googleVis.sankey')
  
  obj$links %>% gvisSankey(from = L$source, to = L$target, weight = L$linkWidth)
  
}



























# OLD FUNCTIONS
# gglvis.click.jscript = function(input_id){
#   "var row = chart.getSelection()[0].row + 1;" %++%
#     "Shiny.onInputChange('" %++% input_id %++% "', row)"
# }
# 
# 
# # Default Settings:
# 
# # default.gglvis.tsline.settings = list(width="800px", height="600px", displayExactValues=TRUE, scaleType='maximized') # todo transfer to settings
# gglvis.tsline.settings = list(displayExactValues = TRUE, scaleType = 'maximized')
# 
# gglvis.column.settings <- list(width = "600px", height = "600px", vAxis.minValue = 0)
# 
# gglvis.sankey.settings <- list(width = 800, height = 600)
# 
# gglvis.calendar.settings <- list(
#   height    = 'auto',
#   yearLabel = list(fontName = 'Times-Roman', fontSize = 32, color = '#1A8763', bold = TRUE),
#   cellSize  =  10,
#   cellColor = list(stroke ='red', strokeOpacity = 0.2),
#   focusedCellColor = list(stroke = 'red'))
# 
# gglVis.gauge.settings = list(width = 400, height = 300)
# 
# gglvis.motionChart.settings <- list(width="800px", height="600px", displayExactValues=TRUE, scaleType='maximized')
# 
# 
# # All Java function scripts:
# getMonth <- 'function(d){
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# return monthNames[d.getMonth()];
# }'
# 
# getMonthLabel <- 'function(d){
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# var S    = monthNames[d - 1];
# if (S == null){return "";} else {return S;}
# }'
# 
# #the x values are passed as milliseconds, turn them into a date and extract month and day
# getMonthDay <- 'function(d) {
# var monthNames = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
# date = new Date(d);
# return monthNames[date.getMonth()] + " " +date.getUTCDate(); }'
# 
# # googleVis Plots:
# 
# # Argument 'x' can only refer to a categorical
# # Argument 'y' must refer to one or more than one numeric columns of obj
# googleVis.line.old = function(obj, x = NULL, y = NULL, func = mean, options = list(), ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, x, y, package = 'googleVis', type = 'line')
# 
#   if (!is.null(func)){
#     if (length(y) == 1){frm = as.formula(paste0(y,' ~ ', x))} else {frm = as.formula(paste0('cbind(', paste(y,collapse = ','), ') ~ ', x))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
# 
#   g = gvisLineChart(obj, xvar = x, yvar = y, options = options, ...)
# 
#   return(g)
# }
# 
# googleVis.bar.old = function(obj, x = NULL, y = NULL, func = mean, options = list(), horizontal = F, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, x, y, package = 'googleVis', type = 'line')
# 
#   if (!is.null(func)){
#     if (length(y) == 1){frm = as.formula(paste0(y,' ~ ', x))} else {frm = as.formula(paste0('cbind(', paste(y,collapse = ','), ') ~ ', x))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
#   if(horizontal){
#     g = gvisBarChart(obj, xvar = x, yvar = y, options = options, ...)}
#   else {
#     g = gvisColumnChart(obj, xvar = x, yvar = y, options = options, ...)
#   }
#   return(g)
# }
# 
# # package.plotType.objectClass.whatToPlot
# 
# # molten table required!
# googleVis.gauge.old = function(obj, theta = NULL, label = NULL, func = mean, thetaLegend = NULL, config = NULL, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   obj = verifyPlotInputs(obj, theta = theta, label = label, package = 'googleVis', type = 'gauge')
# 
#   if (is.null(theta)){theta = numerics(obj)[1]}
# 
#   thetaLegend = verifyThetaLegend(thetaLegend, obj, theta)
# 
# 
#   if(length(theta) > 1){
#     assert(is.null(label), "In gauge charts, dimension 'label' cannot be defined when multiple columns are chosen! In this case, column names, will be the labels.", match.call()[[1]])
#     obj = melt(obj[,theta])
#     label = 'variable'
#     theta = 'value'
#   }
# 
#   if (!is.null(func)){
#     if (length(theta) == 1){frm = as.formula(paste0(theta,' ~ ', label))} else {frm = as.formula(paste0('cbind(', paste(theta,collapse = ','), ') ~ ', label))}
#     obj = aggregate(frm, data = obj, FUN = func)
#   }
# 
#   if (thetaLegend$percentage){
#     thetaLegend$levels  = verify(thetaLegend$levels , 'numeric', varname = 'thetaLegend$levels' , lengths = 4, domain = c(0.0, 100.0), default = c(0.0, 30.0, 70.0, 100.0))
#     obj[,theta]    = 100*(obj[,theta] - thetaLegend$min)/(thetaLegend$max - thetaLegend$min)
#     thetaLegend$max     = 100.0
#     thetaLegend$min     = 0.0
#   } else {
#     thetaLegend$levels  = verify(thetaLegend$levels , 'numeric', varname = 'thetaLegend$levels' , lengths = 4, default = c(thetaLegend$min, thetaLegend$min + 0.3*(thetaLegend$max - thetaLegend$min), thetaLegend$min + 0.7*(thetaLegend$max - thetaLegend$min), thetaLegend$max))
#   }
# 
#   thetaLegend$levels = sort(thetaLegend$levels)
# 
#   if (is.null(config)){config = gglVis.gauge.settings}
# 
#   config$min        = thetaLegend$min
#   config$max        = thetaLegend$max
#   config$redFrom    = thetaLegend$levels[1]
#   config$redTo      = thetaLegend$levels[2]
#   config$yellowFrom = thetaLegend$levels[2]
#   config$yellowTo   = thetaLegend$levels[3]
#   config$greenFrom  = thetaLegend$levels[3]
#   config$greenTo    = thetaLegend$levels[4]
# 
#   g = gvisGauge(obj, options = config)
#   return(g)
# }
# 
# googleVis.sankey.old = function(obj, linkSource = NULL, linkTarget = NULL, linkWidth = NULL, config = NULL, ...){
#   assert(require(googleVis), "Package googleVis is not installed!", err_src = match.call()[[1]])
#   argEdges = list(table = obj,
#                   linkSource = list(colName = linkSource, type = 'nominal'),
#                   linkTarget = list(colName = linkTarget, type = 'nominal'),
#                   linkWidth  = list(colName = linkWidth,  type = 'numeric'))
#   links    = visPrepare(argEdges)
# 
#   if(is.null(config)){config = gglvis.sankey.settings}
# 
#   gvisSankey(links, from = linkSource,
#              to = linkTarget, weight = linkWidth,
#              options = config, ...)
# }
# 



# grvis.R ----------------------------------------------------------------

# Header
# Filename:       grviz.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using diagrammeR package.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     28 April 2018
# Last Revision:  05 November 2018
# Version:        0.1.7
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.1.0     28 April 2018      Initial issue
# 0.1.1     18 October 2018    function grviz.graph() modified
# 0.1.2     24 October 2018    function grviz.graph() modified: bug rectified when empty nodes and links tables are passed
# 0.1.4     05 November 2018   functions grviz.createNodeDF() & .createEdgeDF() added.
# 0.1.6     05 November 2018   functions grviz.createGraph() & .generateDot() added.
# 0.1.7     05 November 2018   functions replaceInSpec() added.


grviz.graph.defset = defset %>% list.edit(
  dimclass   = list(
    key            = c('character', 'factor', 'integer'),
    label          = c('character', 'factor'),
    group          = c('character', 'factor'),
    shape          = 'character',
    size           = 'numeric',
    color          = valid.classes,
    borderColor    = valid.classes,
    linkColor      = valid.classes,
    tooltip        = 'character',
    source         = c('character', 'factor', 'integer'),
    target         = c('character', 'factor', 'integer'),
    labelColor     = valid.classes,
    linkTooltip    = 'character',
    linkLength     = 'numeric',
    linkWidth      = 'numeric',
    linkLabel      = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize  = 'numeric'
  ),
  multiples  = c(),
  palette = list(color           = c('white', 'navy'),
                 linkColor       = c('gray',  'black'),
                 labelColor      = c('black', 'black'),
                 linkLabelColor  = c('black', 'black'),
                 borderColor     = c('black', 'black')),
  
  essentials = c('key', 'source', 'target')
)


replaceInSpec <- function(spec){
  # Directive for marking subscripted text in a label or tooltip '@_'
  if (grepl("@_", spec)) {spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@_\\{(.*?)\\}(.*?)\'', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>', spec, perl = TRUE)}
  # Directive for marking superscripted text in a label or tooltip '@_'
  if (grepl("@\\^", spec)) {spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@\\^\\{(.*?)\\}(.*?)\'', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>', spec, perl = TRUE)}
  # Make a second pass to add subscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.+?)\\}(.*?)>', spec)) {
    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.*?)\\}(.*?)>','\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>', spec, perl = TRUE)
  }
  # Make a second pass to add superscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.+?)\\}(.*?)>', spec)) {
    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.*?)\\}(.*?)>', '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>', spec, perl = TRUE)
  }
  # Directive for substitution of arbitrary specification text '@@'
  if (grepl("@@", spec)) {
    # Extract the spec into several pieces: first being the body, subsequent pieces belonging the replacement references
    spec_body <- unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[1]
    spec_references <- paste0("[1]:", unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[2])
    # Split the references into a vector of R statements
    split_references <- gsub("\\[[0-9]+\\]:[ ]?", "", unlist(strsplit(x = spec_references, "\\n")))
    # Evaluate the expressions and save into a list object
    for (i in 1:length(split_references)) {
      if (i == 1) {eval_expressions <- list()}
      eval_expressions <- c(eval_expressions, list(eval(parse(text = split_references[i]))))
    }
    # Make replacements to the spec body for each replacement that has no hyphen
    for (i in 1:length(split_references)) {
      while (grepl(paste0("@@", i, "([^-0-9])"), spec_body)) {
        spec_body <- gsub(paste0("'@@", i, "'"), paste0("'", eval_expressions[[i]][1], "'"), spec_body)
      }
    }
    # If the replacement has a hyphen, then obtain the digit(s) immediately following and return the value from that index
    for (i in 1:length(split_references)) {
      while (grepl(paste0("@@", i, "-", "[0-9]+"), spec_body)) {
        the_index <- as.numeric(gsub("^([0-9]+)(.*)", "\\1", strsplit(spec_body, paste0("@@", i, "-"))[[1]][2]))
        if (the_index > length(eval_expressions[[i]])) {
          spec_body <- gsub(paste0("@@", i, "-", the_index, "([^0-9])"), paste0(eval_expressions[[i]][length(eval_expressions[[i]])], "\\1"), spec_body)
        } else {
          spec_body <- gsub(paste0("@@", i, "-", the_index, "([^0-9])"), paste0(eval_expressions[[i]][the_index], "\\1"), spec_body)
        }
      }
    }
    # Return the updated spec with replacements evaluated
    return(spec_body)
  }
  if (grepl("@@", spec) == FALSE) {return(spec)}
}

grviz.direction = c(up.down = 'TD', left.right = 'LR', right.left = 'RL', down.up = 'DT')

grviz.applyConfig = function(net, config){
  # shdw = list(enabled = chif(is.null(config$node.shadow.enabled), T, config$node.shadow.enabled),
  #             size    = chif(is.null(config$node.shadow.size), 10, config$node.shadow.size))
  # scl  = list(min = config$node.size.min, max = config$node.size.max)
  
  dir  = grviz.direction[config$direction %>% verify('character', domain = valid.visNetwork.directions, default = 'up.down')] %>% unname
  lot  = config$layout %>% verify('character', domain = c('random', 'hierarchical', 'neato', 'twopi', 'circo'), default = 'dot') 
  
  if(lot == 'hierarchical'){lot = 'dot'} 
  if(lot == 'random'){lot = 'neato'} 
  
  net %>% 
    grviz.addAttributes(attr = "rankdir", value = dir, attr_type = "graph") %>% 
    grviz.addAttributes(attr = "layout" , value = lot, attr_type = "graph")
  
  # net %>% visNodes(shape = config$node.shape,
  #                  size  = config$node.size,
  #                  value = config$node.size,
  #                  title = config$node.tooltip,
  #                  label = config$node.label,
  #                  color = list(background = config$node.color,
  #                               border     = config$node.border.color,
  #                               highlight  = config$node.highlight.color),
  #                  shadow  = shdw,
  #                  scaling = chif(is.empty(scl), NULL, scl))
  # if (!is.null(sizeLegend)){v %<>% visNodes(scaling = sizeLegend)}
  # todo: fixed, physics, node.label.font, node image, node broken image, icon, shape properties
}

grviz.addActions = function(graph_log, version_id, function_used, time_modified, duration, nodes, edges, d_n = 0, d_e = 0) 
{
  if (inherits(time_modified, "POSIXct") == FALSE) {
    stop("The `time_modified` value must inherit from POSIXct.", 
         call. = FALSE)
  }
  graph_log_line <- data.frame(version_id = as.integer(version_id), 
                               function_used = as.character(function_used), time_modified = time_modified, 
                               duration = as.numeric(duration), nodes = as.integer(nodes), 
                               edges = as.integer(edges), d_n = as.integer(d_n), d_e = as.integer(d_e), 
                               stringsAsFactors = FALSE)
  dplyr::bind_rows(graph_log, graph_log_line)
}

grviz.addAttributes = function(graph, attr, value, attr_type){
  time_function_start <- Sys.time()
  fcn_name <- getCallingFunctionName()
  assert(graph %>% is.grviz.graph, "Given 'graph' object is not valid")
  value.x <- value.y <- NULL
  if (length(value) == 1) {
    if (inherits(value, "logical") & value %in% c(TRUE, FALSE)) {
      value <- tolower(as.character(value))
    }
  }
  global_attrs_to_add <- dplyr::tibble(attr = as.character(attr), value = as.character(value), attr_type = as.character(attr_type)) %>% 
    as.data.frame(stringsAsFactors = FALSE)
  graph$global_attrs %<>% 
    dplyr::full_join(global_attrs_to_add, by = c("attr", "attr_type")) %>% 
    dplyr::transmute(attr, attr_type, value = dplyr::coalesce(value.y, value.x)) %>% 
    dplyr::select(attr, value, attr_type)
  
  graph$graph_log %<>% 
    grviz.addActions(version_id = nrow(graph$graph_log) + 1, function_used = fcn_name, 
                     time_modified = time_function_start, duration = graphFunctionDuration(time_function_start), 
                     nodes = nrow(graph$nodes_df), edges = nrow(graph$edges_df))
  graph
}

# Function that gets the calling function as a formatted character string todo: transfer to niragen
#' @importFrom stringr str_replace_all
getCallingFcn <- function() {
  calling_fcn <- deparse(sys.call(-1))
  stringr::str_replace_all(calling_fcn, pattern = "([a-z0-9_]*)(.*)", replacement = "\\1")
}

# Function to add log line for a graph `action`
#' @importFrom dplyr bind_rows
addAction2Log <- function(graph_log, version_id, function_used, time_modified, duration, nodes, edges, d_n = 0, d_e = 0){
  if (inherits(time_modified, "POSIXct") == FALSE) {stop("The `time_modified` value must inherit from POSIXct.", call. = FALSE)}
  
  # Create a log line
  graph_log_line <- data.frame(version_id = as.integer(version_id), function_used = as.character(function_used), time_modified = time_modified,
                               duration = as.numeric(duration), nodes = as.integer(nodes), edges = as.integer(edges), d_n = as.integer(d_n), d_e = as.integer(d_e),
                               stringsAsFactors = FALSE)
  
  # Append the log line to `graph_log`
  dplyr::bind_rows(graph_log, graph_log_line)
}

# Function to get the time difference from the start of the function
graphFunctionDuration <- function(start_time) {
  end_time <- Sys.time()
  time_diff_s <- (end_time - start_time)[[1]]
  return(time_diff_s)
}

grviz.createNodeDF <- function(n, type = NULL, label = NULL, ...){
  fcn_name <- getCallingFcn()
  niragen::assert(inherits(n, "numeric") | inherits(n, "integer"), "The value supplied to `n` must be numeric", fcn_name)
  niragen::assert(length(n) <= 1, "The value supplied to `n` must be a single numeric value", fcn_name)
  
  if (is.null(type)) {type <- rep(as.character(NA), n)}
  if (!is.null(type)) {
    if (length(type) == 1) {type <- rep(type, n)}
    if (length(type) > 1 & length(type) < n) {type <- c(type, rep(as.character(NA), (n - length(type))))}
    if (length(type) > n) {type <- type[1:n]}
  }
  
  extras <- list(...)
  if (length(extras) > 0) {
    for (i in 1:length(extras)) {
      if (length(extras[[i]]) == 1) {extras[[i]] <- rep(extras[[i]], n)}
      if (length(extras[[i]]) > 1 & length(extras[[i]]) < n) {extras[[i]] <- c(extras[[i]], rep("", (n - length(extras[[i]]))))}
      if (length(extras[[i]]) > n) {extras[[i]] <- extras[[i]][1:n]}
    }
    extras <- as.data.frame(extras, stringsAsFactors = FALSE)
  }
  if (is.null(label)) {
    label <- rep(as.character(NA), n)
  } else if (inherits(label, "numeric") | inherits(label, "character")) {
    label <- as.character(label)
  } else if (inherits(label, "logical") & length(label) == 1) {
    if (label == TRUE) {label <- as.character(1:n)} else {label <- rep(as.character(NA), n)}
  }
  if (inherits(extras, "data.frame")) {
    nodes_df <- dplyr::bind_cols(data.frame(id = 1:n, type = type, label = label, stringsAsFactors = FALSE), extras)
  } else {
    nodes_df <- data.frame(id = 1:n, type = type, label = label, stringsAsFactors = FALSE)
  }
  
  nodes_df
}

grviz.createEdgeDF <- function(from, to, rel = NULL, ...) {
  stopifnot(length(from) == length(to))
  n <- length(from)
  from <- as.integer(from)
  to <- as.integer(to)
  if (is.null(rel)) {
    rel <- rep(as.character(NA), length(from))
  } else {
    rel <- as.character(rel)
  }
  if (!is.null(rel)) {
    if (length(rel) == 1) {
      rel <- rep(rel, length(from))
    }
    
    # Expand vectors with `length` > `1` and
    # `length` < `length(from)`
    if (length(rel) > 1 &
        length(rel) < length(from)) {
      rel <-
        c(rel,
          rep(as.character(NA),
              (length(from) - length(rel))))
    }
    
    # Trim vectors with number of values exceeding
    # the number of edges
    if (length(rel) > length(from)) {
      rel <- rel[1:length(from)]
    }
  }
  
  # Collect extra vectors of data as `extras`
  extras <- list(...)
  
  if (length(extras) > 0) {
    for (i in 1:length(extras)) {
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], length(from))
      }
      
      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < length(from)) {
        extras[[i]] <- c(extras[[i]], rep(as.character(NA), (length(from) - length(extras[[i]]))))
      }
      
      if (length(extras[[i]]) > length(from)) {
        extras[[i]] <- extras[[i]][1:length(from)]
      }
    }
    
    # Create a data frame from the `extras` list
    extras <-
      as.data.frame(
        extras, stringsAsFactors = FALSE)
  }
  
  if (inherits(extras, "data.frame")) {
    edges_df <- dplyr::bind_cols(data.frame(id = sequence(n), from = from, to = to, rel = rel, stringsAsFactors = FALSE), extras)
  } else {
    edges_df <- data.frame(id = sequence(n), from = from, to = to, rel = rel, stringsAsFactors = FALSE)
  }
  edges_df
}

grviz.graph = function(obj,
                       key = NULL, label  = NULL, shape  = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL, labelColor = NULL,
                       source = NULL, target = NULL, linkColor = NULL, linkTooltip = NULL, linkWidth = NULL,
                       linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL,
                       config = NULL, ...){
  
  # label %<>% renameSeries('label')
  # shape %<>% renameSeries('shape')
  # size %<>% renameSeries(gndcd(100,11,64,19,130))
  # color %<>% renameSeries('fillcolor')
  # borderColor %<>% renameSeries('color')
  # labelColor %<>% renameSeries('fontcolor')
  # labelSize %<>% renameSeries('fontsize')
  # 
  # tooltip %<>% renameSeries('tooltip')
  # source %<>% renameSeries(gndcd(33,110,12,200))
  # target %<>% renameSeries(gndcd(136,12))
  # linkWidth %<>% renameSeries('penwidth')
  # linkColor %<>% renameSeries('color')
  # linkLabel %<>% renameSeries('label')
  # linkTooltip %<>% renameSeries('tooltip')
  # linkLabelColor %<>% renameSeries('fontcolor')
  # linkLabelSize %<>% renameSeries('fontsize')
  
  # Verifications:
  obj %>% verify('list', lengths = 2, names_identical = c(gndcd(27,143,150,130,98), gndcd(55,4,158,13,144)), varname = 'obj', null_allowed = F, err_src = 'grviz.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'grviz.graph')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'grviz.graph')
  
  # nodeCharIDs = rownames(obj$nodes)
  # assert(!is.null(nodeCharIDs), "obj$nodes has not rownames! Rownames serve as node IDs.", err_src = 'grviz.graph')
  # 
  # nodeIDs = nodeCharIDs %>% as.factor %>% as.integer
  # names(nodeIDs) <- nodeCharIDs
  
  config = grviz.graph.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'grviz')
  
  # Check http://rich-iannone.github.io/DiagrammeR/ndfs_edfs.html for all node and edge attributes
  
  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, shape   = shape, size = size, color = color, borderColor = borderColor, tooltip = tooltip,
                        source = source, target = target, linkColor = linkColor, linkWidth = linkWidth, labelColor = labelColor,
                        linkLabel = linkLabel, linkLabelColor = linkLabelColor, linkLabelSize = linkLabelSize, linkTooltip = linkTooltip)
  L = a$labels
  A = a$aesthetics
  
  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label', 'shape', 'size', 'tooltip', 'color', 'borderColor', 'borderWidth', 'labelColor'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkColor', 'linkLength', 'linkWidth', 'linkLabel', 'linkLabelColor', 'linkLabelSize', 'linkTooltip'), config)
  
  # if (is.empty(obj$nodes)){obj$nodes <- data.frame(id = nodeIDs)} else {obj$nodes$id = nodeIDs}
  
  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'grviz.graph')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'grviz.graph')
  
  if(!inherits(obj$nodes[, L$key], 'integer')){
    nodemap = sequence(nrow(obj$nodes))
    names(nodemap) <- obj$nodes[, L$key]
    
    obj$links[, L$source] = nodemap[obj$links[, L$source]] %>% unname
    obj$links[, L$target] = nodemap[obj$links[, L$target]] %>% unname
    obj$nodes[, L$key]    = nodemap[obj$nodes[, L$key]] %>% unname
  }
  
  if (is.null(L$labelColor) & !is.null(L$color)){
    obj$nodes[, L$labelColor] = contrastColors(obj$nodes[, L$color])
  }
  
  if(!is.null(config$node.size.min) & !is.null(config$node.size.max) & !is.null(L$size)){
    obj$nodes[, L$size] %<>% vect.map(min = config$node.size.min, max = config$node.size.max)
    
  }
  
  if(!is.null(config$link.width.min) & !is.null(config$link.width.max) & !is.null(L$linkWidth)){
    obj$links[, L$linkWidth] %<>% vect.map(min = config$link.width.min, max = config$link.width.max)
  }
  
  if(!is.null(config$link.length.min) & !is.null(config$link.length.max) & !is.null(L$linkLength)){
    obj$links[, L$linkLength] %<>% vect.map(min = config$link.length.min, max = config$link.length.max)
  }
  
  if(is.empty(obj$nodes)){
    nodes_df = data.frame(n = integer(), label = character(), shape = character(), fillcolor = character(),fontcolor = character(), color = character(), width = numeric(), height = numeric(),
                          tooltip   = character(), penwidth  = numeric(), fixedsize = logical(), style = character(), fontsize  = numeric(), fontname  = character(),
                          stringsAsFactors = F)}
  else {
    grviz.createNodeDF(
      n         = nrow(obj$nodes),
      label     = obj$nodes[, L$label],
      shape     = obj$nodes[, L$shape], # todo: translate shapes
      fillcolor = obj$nodes[, L$color],
      fontcolor = obj$nodes[, L$labelColor],
      color     = obj$nodes[, L$borderColor],
      tooltip   = obj$nodes[, L$tooltip],
      penwidth  = obj$nodes[, L$borderWidth],
      fixedsize = F, # get from config, also fontname
      style     = "rounded,filled") -> nodes_df # todo: get from config
    
    config$node.size.ratio %<>% verify('numeric', domain = c(0,1), default = 0.7)
    
    if(is.null(config$node.width) & !is.null(config$node.size)){config$node.width  <- config$node.size}
    if(is.null(config$node.height) & !is.null(config$node.size)){config$node.height <- config$node.size.ratio*config$node.size}
    
    if(!is.null(L$size)){nodes_df$width    = obj$nodes[, L$size]} else {nodes_df$width <- config$node.width}
    if(!is.null(L$size)){nodes_df$height   = config$node.size.ratio*obj$nodes[, L$size]} else {nodes_df$height <- config$node.height}
    if(!is.null(L$size)){nodes_df$fontsize = (config$node.label.size/mean(obj$nodes[, L$size]))*obj$nodes[, L$size]} else {nodes_df$fontsize <- config$node.label.size}
    
    if(is.null(nodes_df$fontsize)){
      nodes_df$fontsize = config$node.label.size
    }
    if(is.null(nodes_df$fillcolor)){
      nodes_df$fillcolor = config$node.color
    }  
    if(is.null(nodes_df$shape)){
      nodes_df$shape = config$node.shape
    } 
    # todo: add sides, peripheries fontcolor, ... as dimensions
    
    nodes_df$distortion = config$node.distortion
    nodes_df$fixedsize  = config$node.fixedSize
    nodes_df$fontcolor  = config$node.label.color
    nodes_df$fontname   = config$node.label.font
    nodes_df$peripheries = config$node.border.peripheries # todo: line, and dblLines to be defined? convert to an integer here
    ns = names(nodes_df)
    if(!'shape' %in% ns & !is.null(config$node.shape)){nodes_df$shape = config$node.shape}
    
    nodes_df$shadow = T
  }
  
  if(is.empty(obj$links)){
    edges_df = data.frame(from = integer(), to = integer(), label = character(), penWidth = numeric(), color = character(), fontname = character(), 
                          stringsAsFactors = F)} 
  else {
    grviz.createEdgeDF(
      from      = obj$links[, L$source],
      to        = obj$links[, L$target],
      color     = obj$links[, L$linkColor], 
      label     = obj$links[, L$linkLabel],
      penwidth  = obj$links[, L$linkWidth],
      fontsize  = obj$links[, L$linkLabelSize],
      tooltip   = obj$links[, L$linkTooltip]) -> edges_df
    
    if(is.null(edges_df$fontsize)){
      edges_df$fontsize = config$link.label.size
    }
    
    edges_df$fontcolor = config$link.label.color
    edges_df$fontname  = config$link.label.font
    edges_df$arrowhead = config$link.arrow.head
    edges_df$arrowsize = config$link.arrow.size
    edges_df$arrowtail = config$link.arrow.tail
    edges_df$dir       = config$link.direction
    edges_df$headport  = NULL
    edges_df$tailport  = NULL
    
    edges_df$rel = 'leading_to'
    
    # todo: add more dimensions inarguments and when requried, also add additional dimensions in config, like hidden(visible), image, mass, borderWidth, borderWidthSelected, labelHighlightBold...
    # obj$nodes = obj$nodes[order(obj$nodes$id), ]
    
    # column headport and tailport can take these values: n, ne, e, se, s, sw, w, and nw
    # look at: https://www.rdocumentation.org/packages/DiagrammeR/versions/1.0.0/topics/edge_aes
    # https://www.graphviz.org/doc/info/attrs.html#h:undir_note
  }
  
  grviz.createGraph(nodes_df, edges_df, ...) %>% grviz.applyConfig(config) %>% 
    grviz.renderGraph(layout = config$layout %>% verify('character', default = 'dot'), 
                      title  = config$title %>% verify('character', lengths == 1),
                      shinyInput.click = config$shinyInput.click %>% verify('character', lengths = 1, varname = 'config$shinyInput.click'))
}


# todo: translate shapes to niravis standard
grviz.shape = c(square = 'square', delta = 'triangle', icon = 'icon', triangle = 'triangle', rectangle = 'rectangle', box = 'rectangle', dot = 'point', point = 'point', egg = 'egg', oval = 'oval', text = 'plaintext', bubble = 'circle', circle = gndcd(88,4,1,18,199,94),  star = 'star', ellipse = 'ellipse', cylinder = "database", diamond = 'diamond', rhombus = 'diamond')
valid.grviz.shapes = grviz.shape %>% names


#sizeColumn
#sizeLegend


#######################################################################################################################

# Function to check whether a graph object is valid
is.graph.valid <- function(graph) {
  # Check for all component names to be present
  if (!all(c("graph_info", "nodes_df", "edges_df", "global_attrs", "directed", "last_node", "last_edge", "node_selection", "edge_selection", "cache", "graph_log") %in% names(graph))){
    return(FALSE)
  }
  # Check for specific graph classes
  if (any(
    !inherits(graph$graph_info, "data.frame"),
    !inherits(graph$nodes_df, "data.frame"),
    !inherits(graph$edges_df, "data.frame"),
    !inherits(graph$global_attrs, "data.frame"),
    !inherits(graph$global_attrs$attr, "character"),
    !inherits(graph$global_attrs$value, "character"),
    !inherits(graph$global_attrs$attr_type, "character"),
    !inherits(graph$directed, "logical"),
    !inherits(graph$node_selection, "data.frame"),
    !inherits(graph$edge_selection, "data.frame"),
    !inherits(graph$cache, "list"),
    !inherits(graph$graph_log, "data.frame"))) {
    
    return(FALSE)
  }
  return(TRUE)
}

is.grviz.graph = function(graph){
  if (!all(c("graph_info", "nodes_df", "edges_df", "global_attrs", 
             "directed", "last_node", "last_edge", "node_selection", 
             "edge_selection", "cache", "graph_log") %in% names(graph))) {
    return(FALSE)
  }
  if (any(inherits(graph$graph_info, "data.frame") == FALSE, 
          inherits(graph$nodes_df, "data.frame") == FALSE, inherits(graph$edges_df, 
                                                                    "data.frame") == FALSE, inherits(graph$global_attrs, 
                                                                                                     "data.frame") == FALSE, inherits(graph$global_attrs$attr, 
                                                                                                                                      "character") == FALSE, inherits(graph$global_attrs$value, 
                                                                                                                                                                      "character") == FALSE, inherits(graph$global_attrs$attr_type, 
                                                                                                                                                                                                      "character") == FALSE, inherits(graph$directed, "logical") == 
          FALSE, inherits(graph$node_selection, "data.frame") == 
          FALSE, inherits(graph$edge_selection, "data.frame") == 
          FALSE, inherits(graph$cache, "list") == FALSE, inherits(graph$graph_log, 
                                                                  "data.frame") == FALSE)) {
    return(FALSE)
  }
  return(TRUE)
}

grviz.createGraph <- function(nodes_df = NULL, edges_df = NULL, directed = TRUE, graph_name = NULL, attr_theme = "default", write_backups = FALSE) {
  fcn_name   <- getCallingFcn()
  graph_time <- Sys.time()
  graph_tz   <- Sys.timezone()
  graph_id   <- replicate(8, sample(c(LETTERS, letters, 0:9), 1)) %>% paste(collapse = "")
  graph_info <- data.frame(
    graph_id = as.character(graph_id),
    graph_name = as.character(paste0("graph_", graph_id)),
    graph_time = graph_time,
    graph_tz = graph_tz,
    write_backups = as.logical(write_backups),
    stringsAsFactors = FALSE)
  
  # Insert a user-defined `graph_name` if supplied
  if (!is.null(graph_name)) {graph_info[1, 2] <- as.character(graph_name)}
  global_attrs <- data.frame(
    attr = as.character(NA),
    value = as.character(NA),
    attr_type = as.character(NA),
    stringsAsFactors = FALSE)[-1, ]
  
  if (inherits(attr_theme, "character")) {
    if (attr_theme == "default") {
      global_attrs <- data.frame(
        attr      = c("layout", "outputorder", "fontname", "fontsize", "shape", "fixedsize", "width", "style",
                      "fillcolor", "color", "fontcolor", "bgcolor", "fontname", "fontsize", "len", "color", "arrowsize"),
        value     = c("neato", "edgesfirst", "Helvetica", "10", "circle", "true", "0.5", "filled", "aliceblue", "gray70", "gray50",
                      "white", "Helvetica", "8", "1.5", "gray80", "0.5"),
        attr_type = c(rep("graph", 2), rep("node", 9), "graph", rep("edge", 5)),
        stringsAsFactors = FALSE)
    } else {niragen::assert(F, "The value for `attr_theme` doesn't refer to any available theme", fcn_name)}
  } else if (is.null(attr_theme)) {
    global_attrs <- data.frame(
      attr  = as.character(NA),
      value = as.character(NA),
      attr_type = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]
  }
  
  ndf <- data.frame(id = as.integer(NA), type = as.character(NA), label = as.character(NA), stringsAsFactors = FALSE)[-1, ]
  edf  <- data.frame(id = as.integer(NA), from = as.integer(NA), to = as.integer(NA), rel = as.character(NA), stringsAsFactors = FALSE)[-1, ]
  nsdf <- dplyr::tibble(node = as.integer(NA))[-1, ] %>% as.data.frame(stringsAsFactors = FALSE)
  esdf <- dplyr::tibble(edge = as.integer(NA), from = as.integer(NA), to = as.integer(NA))[-1, ] %>% as.data.frame(stringsAsFactors = FALSE)
  
  graph_actions <-  data.frame(action_index = as.integer(NA), action_name = as.character(NA), stringsAsFactors = FALSE)[-1, ]
  
  graph_log     <-  data.frame(version_id = as.integer(NA), function_used = as.character(NA), time_modified = graph_time,      duration = as.numeric(NA),
                               nodes = as.integer(NA), edges = as.integer(NA), d_n = as.integer(NA), d_e = as.integer(NA), stringsAsFactors = FALSE)[-1, ]
  
  cache <- list()
  
  graph <- list(
    graph_info = graph_info, nodes_df = ndf, edges_df = edf, global_attrs = global_attrs,
    directed = ifelse(directed, TRUE, FALSE), last_node = 0, last_edge = 0, node_selection = nsdf,
    edge_selection = esdf, cache = cache, graph_actions = graph_actions, graph_log = graph_log)
  
  attr(graph, "class") <- "dgr_graph"
  
  if (all(c(is.null(nodes_df), is.null(edges_df)))) {
    graph_log <- addAction2Log(graph_log = graph_log, version_id = 1,
                               function_used = fcn_name, time_modified = graph_time,
                               duration = graphFunctionDuration(graph_time), nodes = nrow(graph$nodes_df),
                               edges = nrow(graph$edges_df), d_n = nrow(graph$nodes_df), d_e = nrow(graph$edges_df))
  } else if (!is.null(nodes_df) & is.null(edges_df)) {
    if (inherits(nodes_df, "tbl_df")) {nodes_df <- nodes_df %>% as.data.frame(stringsAsFactors = FALSE)}
    
    for (i in 2:3) {nodes_df[, i] <- as.character(nodes_df[, i])}
    
    graph$nodes_df <- dplyr::bind_rows(graph$nodes_df, nodes_df)
    
    graph$last_node <- nrow(nodes_df)
    
    graph_log <- addAction2Log(
      graph_log = graph_log,
      version_id = 1,
      function_used = fcn_name,
      time_modified = graph_time,
      duration = graphFunctionDuration(graph_time),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nrow(graph$nodes_df),
      d_e = nrow(graph$edges_df))
    
  } else if (!is.null(nodes_df) & !is.null(edges_df)) {
    
    if (inherits(nodes_df, "tbl_df")) {nodes_df <- nodes_df %>% as.data.frame(stringsAsFactors = FALSE)}
    if (inherits(edges_df, "tbl_df")) {edges_df <- edges_df %>% as.data.frame(stringsAsFactors = FALSE)}
    for (i in 2:3) {nodes_df[, i] <- as.character(nodes_df[, i])}
    
    graph$nodes_df <- dplyr::bind_rows(graph$nodes_df, nodes_df)
    
    graph$last_node <- nrow(nodes_df)
    
    if (inherits(edges_df, "data.frame")) {
      if (ncol(edges_df) > 2) {edges_df$rel <- as.character(edges_df$rel)}
    }
    
    graph$edges_df <- dplyr::bind_rows(graph$edges_df, edges_df)
    
    graph$last_edge <- nrow(edges_df)
    
    graph_log <- addAction2Log(
      graph_log = graph_log,
      version_id = 1,
      function_used = fcn_name,
      time_modified = graph_time,
      duration = graphFunctionDuration(graph_time),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df),
      d_n = nrow(graph$nodes_df),
      d_e = nrow(graph$edges_df))
  }
  
  # Add the `graph_log` df to the graph object
  graph$graph_log <- graph_log
  
  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }
  
  # If neither an ndf nor both ndf & edf provided,
  # return the initialized graph with no nodes or edges
  return(graph)
}

grviz.generateDot <- function(graph) {
  fcn_name <- getCallingFcn()
  is.graph.valid(graph) %>% niragen::assert("The graph object is not valid", fcn_name)
  attr_type <- attr <- value <- string <- NULL
  # Extract objects from the graph objecct
  nodes_df <- graph$nodes_df
  edges_df <- graph$edges_df
  directed <- graph$directed
  global_attrs <- graph$global_attrs
  
  if ("graph" %in% global_attrs$attr_type) {
    graph_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "graph") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))
    graph_attrs <- graph_attrs %>% dplyr::pull(string)
  } else {graph_attrs <- NA}
  
  if ("node" %in% global_attrs$attr_type) {
    node_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "node") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))
    node_attrs <- node_attrs %>% dplyr::pull(string)
    
    for (i in 1:nrow(global_attrs %>% dplyr::filter(attr_type == "node"))) {
      node_attr_to_set <- (global_attrs %>% dplyr::filter(attr_type == "node"))[i, 1]
      if (node_attr_to_set %in% colnames(nodes_df)) {
        col_num <- which(colnames(nodes_df) == node_attr_to_set)
        nodes_df[which(is.na(nodes_df[, col_num])), col_num] <-
          (global_attrs %>% dplyr::filter(attr_type == "node"))[i, 2]
      }
    }
  } else {node_attrs <- NA}
  
  if ("edge" %in% global_attrs$attr_type) {
    edge_attrs <- global_attrs %>%
      dplyr::filter(attr_type == "edge") %>%
      dplyr::mutate(string = paste0(attr, " = '", value, "'"))
    
    edge_attrs <- edge_attrs %>% dplyr::pull(string)
    
    for (i in 1:nrow(global_attrs %>% dplyr::filter(attr_type == "edge"))) {
      edge_attr_to_set <- (global_attrs %>% dplyr::filter(attr_type == "edge"))[i, 1]
      if (edge_attr_to_set %in% colnames(edges_df)) {
        col_num <- which(colnames(edges_df) == edge_attr_to_set)
        edges_df[which(is.na(edges_df[, col_num])), col_num] <-
          (global_attrs %>% dplyr::filter(attr_type == "edge"))[i, 2]
      }
    }
  } else {edge_attrs <- NA}
  
  # Replace NA values with empty strings in `nodes_df`
  if (!is.null(nodes_df)) {
    if (ncol(nodes_df) >= 4) {
      for (i in 4:ncol(nodes_df)) {
        nodes_df[, i] <-  as.character(nodes_df[, i])
        nodes_df[, i] <-  ifelse(is.na(nodes_df[, i]), "", nodes_df[, i])
      }
    }
  }
  
  # Replace NA values with empty strings in `edges_df`
  if (!is.null(edges_df)) {
    if (ncol(edges_df) >= 4) {
      for (i in 4:ncol(edges_df)) {
        edges_df[, i] <-  ifelse(is.na(edges_df[, i]), "", edges_df[, i])
        edges_df[, i] <-  as.character(edges_df[, i])
      }
    }
  }
  
  # If `equation` column in `nodes_df`, ensure the right amount of escaping is present
  if ("equation" %in% colnames(nodes_df)) {
    equation_col <- which(colnames(nodes_df) == "equation")
    for (i in 1:nrow(nodes_df)) {
      if (grepl("^\\$.*\\$$", nodes_df[i, equation_col])) {
        nodes_df[i, equation_col] <- str_replace_all(
          nodes_df[i, equation_col], "\\\\", "\\\\\\\\")
      } else {nodes_df[i, equation_col] <- ""}
    }
  }
  
  # If `display` column in `nodes_df`, modify label column for this render
  if ("display" %in% colnames(nodes_df)) {
    display_col <- which(colnames(nodes_df) == "display")
    label_col   <- which(colnames(nodes_df) == "label")
    for (i in 1:nrow(nodes_df)) {
      if (nodes_df[i, display_col] != "") {
        nodes_df[i, label_col] <- nodes_df[i, which(colnames(nodes_df) == nodes_df[i, display_col])]
      } else {nodes_df[i, label_col] <- ""}
    }
  }
  
  # If `display` column in `edges_df`, modify label column for this render
  if ("display" %in% colnames(edges_df)) {
    display_col <- which(colnames(edges_df) == "display")
    if (!("label" %in% colnames(edges_df))) {
      edges_df <- edges_df %>% mutate(label = as.character(NA))
    }
    
    label_col <- which(colnames(edges_df) == "label")
    
    for (i in 1:nrow(edges_df)) {
      if (!is.na(edges_df[i, display_col]) ) {
        if (edges_df[i, display_col] != "") {
          
          edges_df[i, label_col] <-
            edges_df[
              i, which(colnames(edges_df) == edges_df[i, display_col])]
        }
      } else {
        edges_df[i, label_col] <- ""
      }
    }
  }
  
  graph_attributes <- c("layout", "bgcolor", "rankdir", "overlap", "outputorder", "fixedsize", "mindist", "nodesep", "ranksep", "stylesheet")
  node_attributes  <- c("shape", "style", "penwidth", "color", "fillcolor", "fontname", "fontsize", "fontcolor", "height", "width", "group", "tooltip", "xlabel", 
                        "URL", "distortion", "sides", "skew", "peripheries", "gradientangle", "label", "fixedsize", "labelloc", "margin", "orientation", "pos")
  edge_attributes  <- c("style", "penwidth", "color", "arrowsize", "arrowhead", "arrowtail", "fontname", "fontsize", "fontcolor", "len", "tooltip", "URL", "label", 
                        "labelfontname", "labelfontsize", "labelfontcolor", "labeltooltip", "labelURL", "edgetooltip", "edgeURL", "headtooltip", "headURL", "headclip", 
                        "headlabel", "headport", "tailtooltip", "tailURL", "tailclip",  "taillabel", "tailport", "dir", "decorate")
  
  if (nrow(nodes_df) == 0 & nrow(edges_df) == 0) {
    # Create DOT code with nothing in graph
    dot_code <- paste0(ifelse(directed, "digraph", "graph"), " {\n", "\n}")
  } else {
    # Create the DOT attributes block
    # Create the default attributes statement
    # for graph attributes
    if (!(any(is.na(graph_attrs)))) {
      graph_attr_stmt <- paste0("graph [", paste(graph_attrs, collapse = ",\n       "), "]\n")
    } else {graph_attr_stmt <- ""}
    
    # Create the default attributes statement
    # for node attributes
    if (!(any(is.na(node_attrs)))) {
      node_attr_stmt <-
        paste0("node [", paste(node_attrs,
                               collapse = ",\n      "),
               "]\n")
    } else {
      node_attr_stmt <- ""
    }
    
    # Create the default attributes statement
    # for edge attributes
    if (!(any(is.na(edge_attrs)))) {
      edge_attr_stmt <-
        paste0("edge [", paste(edge_attrs,
                               collapse = ",\n     "),
               "]\n")
    } else {
      edge_attr_stmt <- ""
    }
    
    # Combine default attributes into a single block
    combined_attr_stmts <-
      paste(
        graph_attr_stmt,
        node_attr_stmt,
        edge_attr_stmt, sep = "\n")
    
    #
    # Create the DOT node block
    #
    
    if (nrow(nodes_df) > 0) {
      
      # Determine whether positional (x,y)
      # data is included
      column_with_x <-
        which(colnames(nodes_df) %in% "x")[1]
      
      column_with_y <-
        which(colnames(nodes_df) %in% "y")[1]
      
      if (!is.na(column_with_x) & !is.na(column_with_y)) {
        
        pos <-
          data.frame(
            "pos" =
              paste0(
                nodes_df[, column_with_x],
                ",",
                nodes_df[, column_with_y],
                "!"))
        
        nodes_df$pos <- pos$pos
      }
      
      # Determine whether column 'alpha' exists
      if (any(grepl("$alpha^", colnames(nodes_df)))) {
        column_with_alpha_assigned <-
          grep("$alpha^", colnames(nodes_df))
      } else {
        column_with_alpha_assigned <- NA
      }
      
      if (!is.na(column_with_alpha_assigned)) {
        
        # Determine the number of color attributes in
        # the node data frame
        number_of_col_attr <-
          length(which(colnames(nodes_df) %in%
                         c("color", "fillcolor",
                           "fontcolor")))
        
        # If the number of color attrs in df is 1,
        # rename referencing alpha column
        if (number_of_col_attr == 1) {
          
          name_of_col_attr <-
            colnames(nodes_df)[
              which(colnames(nodes_df) %in%
                      c("color", "fillcolor",
                        "fontcolor"))]
          
          colnames(nodes_df)[column_with_alpha_assigned] <-
            paste0("alpha:", name_of_col_attr)
        }
      }
      
      # Determine whether column 'alpha' with
      # color attr exists
      if (any(grepl("alpha:.*", colnames(nodes_df)))) {
        
        alpha_column_no <- grep("alpha:.*", colnames(nodes_df))
        
        color_attr_column_name <-
          unlist(strsplit(colnames(nodes_df)[
            (which(grepl("alpha:.*", colnames(nodes_df))))
            ], ":"))[-1]
        
        color_attr_column_no <-
          which(colnames(nodes_df) %in% color_attr_column_name)
        
        # Append alpha value only if referenced
        # column is for color
        if (any(c("color", "fillcolor", "fontcolor") %in%
                colnames(nodes_df)[color_attr_column_no])) {
          
          # Append alpha for color values that are
          # X11 color names
          if (all(grepl("[a-z]*",
                        as.character(nodes_df[, color_attr_column_no]))) &
              all(as.character(nodes_df[, color_attr_column_no]) %in%
                  x11_hex()[, 1])) {
            
            for (i in 1:nrow(nodes_df)) {
              nodes_df[i, color_attr_column_no] <-
                paste0(x11_hex()[
                  which(x11_hex()[, 1] %in%
                          as.character(nodes_df[i, color_attr_column_no])), 2],
                  formatC(round(as.numeric(nodes_df[i, alpha_column_no]), 0),
                          flag = "0", width = 2))
            }
          }
          
          # Append alpha for color values that
          # are hex color values
          if (all(grepl("#[0-9a-fA-F]{6}$",
                        as.character(nodes_df[, color_attr_column_no])))) {
            
            for (i in 1:nrow(nodes_df)) {
              nodes_df[, color_attr_column_no] <-
                as.character(nodes_df[, color_attr_column_no])
              
              nodes_df[i, color_attr_column_no] <-
                paste0(nodes_df[i, color_attr_column_no],
                       round(as.numeric(nodes_df[i, alpha_column_no]), 0))
            }
          }
        }
      }
      
      # Determine which other columns correspond
      # to node attribute values
      other_columns_with_node_attributes <-
        which(colnames(nodes_df) %in% node_attributes)
      
      # Construct the 'node_block' character object
      for (i in 1:nrow(nodes_df)) {
        if (i == 1) {
          node_block <- vector(mode = "character", length = 0)
        }
        
        if (length(other_columns_with_node_attributes) > 0) {
          
          for (j in other_columns_with_node_attributes) {
            
            if (j == other_columns_with_node_attributes[1]) {
              attr_string <- vector(mode = "character", length = 0)
            }
            
            # Create the node attributes for labels
            # and tooltips when provided
            if (all(colnames(nodes_df)[j] %in%
                    c("label", "tooltip"),
                    is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(colnames(nodes_df)[j] %in%
                           c("label", "tooltip"),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            }
            attr_string <- c(attr_string, attribute)
          }
          
          if (j == other_columns_with_node_attributes[
            length(other_columns_with_node_attributes)]) {
            attr_string <- paste(attr_string, collapse = ", ")
          }
        }
        
        # Generate a line of node objects when an
        # attribute string exists
        if (exists("attr_string")) {
          line <- paste0("  '", nodes_df[i, 1], "'",
                         " [", attr_string, "] ")
        }
        
        # Generate a line of node objects when an
        # attribute string doesn't exist
        if (!exists("attr_string")) {
          line <-
            paste0("  '",
                   nodes_df[i, 1],
                   "'")
        }
        node_block <- c(node_block, line)
      }
      
      if ("rank" %in% colnames(nodes_df)) {
        node_block <-
          c(node_block,
            tapply(node_block,
                   nodes_df$rank, FUN = function(x) {
                     if(length(x) > 1) {
                       x <- paste0('subgraph{rank = same\n',
                                   paste0(x, collapse = '\n'),
                                   '}\n')
                     }
                     return(x)
                   }))
      }
      
      else if ('cluster' %in% colnames(nodes_df)) {
        clustered_node_block <- character(0)
        clusters <- split(node_block, nodes_df$cluster)
        for (i in seq_along(clusters)) {
          if (names(clusters)[[i]] == "") {
            # nodes not in clusters
            cluster_block <- clusters[[i]]
          } else {
            cluster_block <- paste0("subgraph cluster", i, "{\nlabel='",
                                    names(clusters)[[i]], "'\n",
                                    paste0(clusters[[i]], collapse="\n"), "}\n")
          }
          clustered_node_block <- c(clustered_node_block, cluster_block)
        }
        
        node_block <- clustered_node_block
        
        # cleanup variables
        rm(clustered_node_block, clusters, cluster_block)
      }
      
      # Construct the `node_block` character object
      node_block <- paste(node_block, collapse = "\n")
      
      # Remove the `attr_string` object if it exists
      if (exists("attr_string")) {
        rm(attr_string)
      }
      
      # Remove the `attribute` object if it exists
      if (exists("attribute")) {
        rm(attribute)
      }
    }
    
    #
    # Create the DOT edge block
    #
    
    if (nrow(edges_df) > 0) {
      
      # Determine whether `from` or `to` columns are
      # in `edges_df`
      from_to_columns <-
        ifelse(any(c("from", "to") %in%
                     colnames(edges_df)), TRUE, FALSE)
      
      # Determine which columns in `edges_df`
      # contain edge attributes
      other_columns_with_edge_attributes <-
        which(colnames(edges_df) %in% edge_attributes)
      
      # Determine whether the complementary set of
      # columns is present
      if (from_to_columns) {
        both_from_to_columns <-
          all(c(any(c("from") %in%
                      colnames(edges_df))),
              any(c("to") %in%
                    colnames(edges_df)))
      }
      
      # If the complementary set of columns is present,
      # determine the positions
      if (exists("both_from_to_columns")) {
        if (both_from_to_columns) {
          from_column <-
            which(colnames(edges_df) %in% c("from"))[1]
          to_column <-
            which(colnames(edges_df) %in% c("to"))[1]
        }
      }
      
      # Construct the `edge_block` character object
      if (exists("from_column") &
          exists("to_column")) {
        
        if (length(from_column) == 1 &
            length(from_column) == 1) {
          
          for (i in 1:nrow(edges_df)) {
            
            if (i == 1) {
              edge_block <-
                vector(mode = "character", length = 0)
            }
            
            if (length(other_columns_with_edge_attributes) > 0) {
              
              for (j in other_columns_with_edge_attributes) {
                
                if (j == other_columns_with_edge_attributes[1]) {
                  attr_string <- vector(mode = "character", length = 0)
                }
                
                # Create the edge attributes for labels
                # and tooltips when provided
                if (all(colnames(edges_df)[j] %in%
                        c("edgetooltip", "headtooltip",
                          "label", "labeltooltip",
                          "taillabel", "tailtooltip",
                          "tooltip"),
                        is.na(edges_df[i, j]))) {
                  attribute <- NULL
                } else if (all(colnames(edges_df)[j] %in%
                               c("edgetooltip", "headtooltip",
                                 "label", "labeltooltip",
                                 "taillabel", "tailtooltip",
                                 "tooltip"),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j],
                           "'")
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               is.na(edges_df[i, j]))) {
                  
                  attribute <- NULL
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j], "'")
                }
                attr_string <- c(attr_string, attribute)
              }
              
              if (j == other_columns_with_edge_attributes[
                length(other_columns_with_edge_attributes)]) {
                attr_string <- paste(attr_string, collapse = ", ")
              }
            }
            
            # Generate a line of edge objects when an
            # attribute string exists
            if (exists("attr_string")) {
              line <-
                paste0("'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       paste0(" [", attr_string, "] "))
            }
            
            # Generate a line of edge objects when an
            # attribute string doesn't exist
            if (!exists("attr_string")) {
              line <-
                paste0("  ",
                       "'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       " ")
            }
            edge_block <- c(edge_block, line)
          }
        }
      }
      
      # Construct the `edge_block` character object
      if (exists("edge_block")) {
        edge_block <- paste(edge_block, collapse = "\n")
      }
    }
    
    # Create the graph code from the chosen attributes,
    # and the nodes and edges blocks
    if (exists("combined_attr_stmts")) {
      if (exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block, edge_block,
                sep = "\n")
      }
      if (!exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block,
                sep = "\n")
      }
    }
    if (!exists("combined_attr_stmts")) {
      if (exists("edge_block")) {
        combined_block <- paste(node_block, edge_block,
                                sep = "\n")
      }
      if (!exists("edge_block")) {
        combined_block <- node_block
      }
    }
    
    # Create DOT code
    dot_code <-
      paste0(ifelse(directed, "digraph", "graph"),
             " {\n", "\n", combined_block, "\n}")
    
    # Remove empty node or edge attribute statements
    dot_code <- gsub(" \\[\\] ", "", dot_code)
  }
  
  dot_code
}

grviz.renderGraph = function (graph, layout = NULL, title = NULL, width = NULL, height = NULL, shinyInput.click = NULL) 
{
  if(nrow(graph$nodes_df) == 0){dotstr = ""} else {
    fcn_name <- getCallingFunctionName()
    is.grviz.graph(graph) %>% niragen::assert("The graph object is not valid", fcn_name)
    V1 <- V2 <- x <- y <- attr_type <- value_x <- NULL
    value <- hex <- fillcolor <- new_fillcolor <- NULL
    if (!is.null(title)) {
      graph %<>% grviz.addAttributes("label", title, "graph")
      graph %<>% grviz.addAttributes("labelloc", "t", "graph")
      graph %<>% grviz.addAttributes("labeljust", "c", "graph")
      graph %<>% grviz.addAttributes("fontname", "Helvetica", "graph")
      graph %<>% grviz.addAttributes("fontcolor", "gray30", "graph")
    }
    if (nrow(graph$nodes_df) > 0){
      if (!("fillcolor" %in% colnames(graph$nodes_df))) {
        if ("fillcolor" %in% graph$global_attrs$attr) {
          graph$nodes_df$fillcolor <- graph$global_attrs %>% 
            dplyr::filter(attr == "fillcolor" & attr_type == "node") %>% dplyr::select(value) %>% purrr::flatten_chr()
        }
        else {
          graph$nodes_df$fillcolor <- "white"
        }
      }
    }
    if (nrow(graph$nodes_df) > 0) {
      if ("fillcolor" %in% colnames(graph$nodes_df)) {
        if ("fillcolor" %in% graph$global_attrs$attr) {
          graph$nodes_df$fillcolor[which(is.na(graph$nodes_df$fillcolor))] <- graph$global_attrs[which(graph$global_attrs$attr == "fillcolor"), 2]
        }
      }
    }
    if (!("fontcolor" %in% colnames(graph$nodes_df)) & "fillcolor" %in% colnames(graph$nodes_df)) {
      graph$nodes_df$fontcolor <- graph$nodes_df$fillcolor %>% 
        dplyr::as_data_frame() %>% dplyr::mutate(value_x = contrastColors(color = value)) %>% 
        dplyr::pull(value_x)
    }
    if (!is.null(layout)) {
      if (layout %in% c("circle", "tree", "kk", "fr", "nicely")) {
        graph %<>% grviz.addAttributes(attr = "layout", value = "neato", attr_type = "graph")
        if ("x" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <- graph$nodes_df %>% dplyr::select(-x)
        }
        if ("y" %in% colnames(graph$nodes_df)) {
          graph$nodes_df <- graph$nodes_df %>% dplyr::select(-y)
        }
        if (layout == "circle") {
          coords <- graph %>% to_igraph() %>% igraph::layout_in_circle() %>% 
            dplyr::as_tibble() %>% 
            dplyr::rename(x = V1, y = V2) %>% dplyr::mutate(x = x * 1.25) %>% dplyr::mutate(y = y * 1.25)
        }
        if (layout == "tree") {
          coords <- (graph %>% to_igraph() %>% igraph::layout_with_sugiyama())[[2]] %>% 
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "kk") {
          coords <- graph %>% to_igraph() %>% igraph::layout_with_kk() %>% 
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "fr") {
          coords <- graph %>% to_igraph() %>% igraph::layout_with_fr() %>% 
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        if (layout == "nicely") {
          coords <- graph %>% to_igraph() %>% igraph::layout_nicely() %>% 
            dplyr::as_tibble() %>% dplyr::rename(x = V1, y = V2)
        }
        graph$nodes_df <- graph$nodes_df %>% dplyr::bind_cols(coords)
      }
    }
    dotstr = graph %>% grviz.generateDot}
  
  dotstr %>% grviz(width = width, height = height, shinyInput.click = shinyInput.click)
}

# task.js = JS('
#   $(document).on("click", "a", function(e){ console.log($(this).text())} )
# ')
# 
# 
# task.js = JS('
#   $(document).on("click", "a", function(e){ console.log($(this))} )
# ')
# 
# task.js2 = JS('
#   $(document).on("click", function(e){e.preventDefault(); 
#   Shiny.onInputChange("nima", Object.getOwnPropertyNames($(this).context.activeElement.childNodes[1].childNodes[1].))})
# ')
# 
# task.js2 = JS('
#   $(document).on("click", "a", function(e){e.preventDefault(); 
#   Shiny.onInputChange("nima", Math.random() + Object.getOwnPropertyNames($(this).context.childNodes[1].parentNode))})
# ')

grviz.task.js = JS('
                   $(document).on("click", "a", function(e){e.preventDefault(); 
                   Shiny.onInputChange("nima", Math.random() + " --> " + $(this)[0].childNodes[3].firstChild.data)})
                   ')


grviz = function(diagram = "", engine = "dot", allow_subst = TRUE, 
                 options = NULL, width = NULL, height = NULL, shinyInput.click = NULL) 
{
  if(!is.null(shinyInput.click)){
    # shinyInput.click %<>% verify('character', varname = 'shinyInput.click') Already verified!
    task.js = JS('$(document).on("click", "a", function(e){e.preventDefault(); 
                 Shiny.onInputChange("' %++% shinyInput.click %++% '", Math.random() + " --> " + $(this)[0].childNodes[3].firstChild.data)})')
} else {task.js = NULL}
  if (inherits(diagram, "connection") || file.exists(diagram)) {
    diagram <- readLines(diagram, encoding = "UTF-8", warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")
  }
  else {
    if (length(diagram) > 1) {diagram <- paste0(diagram, collapse = "\n")}
  }
  if (allow_subst == TRUE) {diagram <- replaceInSpec(diagram)}
  diagram <- gsub(x = diagram, "'", "\"")
  x <- list(diagram = diagram, config = list(engine = engine, options = options, tasks = task.js))
  # x <- list(diagram = diagram, config = list(engine = engine, options = options))
  viewer.suppress <- rstudioapi::isAvailable() && !rstudioapi::isAvailable("0.99.120")
  htmlwidgets::createWidget(name = "grviz", x = x, width = width, 
                            height = height, package = "niravis", htmlwidgets::sizingPolicy(viewer.suppress = viewer.suppress))
  }

grvizOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId = outputId, 'grviz', width, height, package = 'niravis')
}

renderGrviz <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shinyRenderWidget(expr, grvizOutput, env, quoted = TRUE)
}



# highcharter.R ----------------------------------------------------------------



# Header
# Filename:       highcharter.R
# Description:    Contains functions for plotting various charts from package 'highcharter' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     14 October 2016
# Last Revision:  24 July 2018
# Version:        1.3.9
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     14 October 2016    Initial issue
# 1.1.0     26 March 2017      Function highcharter.combo() added
# 1.1.1     02 April 2017      highcharter.combo.defset Added
# 1.1.2     02 April 2017      Function highcharter.combo() Modified
# 1.2.0     14 May 2017        Function highcharter.combo() Modified: drilldown for multiple categorical series added
# 1.2.1     01 June 2017       Function highcharter.combo() Modified: fixed bug in distinguishing whether or not drilldown conditions are met.
# 1.2.2     14 February 2018   Function highcharter.apply() renamed to highcharter.applyConfig() and modified: the background color 'white' defined as default for hc_theme, if thm is empty, nothing will be plotted!
# 1.2.3     04 June 2018       Function highcharter.bar() added.
# 1.2.4     05 June 2018       Function highcharter.applyConfig() modified: only property 'bordRadius' cannot be set to NULL because it ruins the plot!! Package Bug!! 
# 1.2.5     19 June 2018       Function highcharter.pie() added.
# 1.2.6     20 July 2018       Function highcharter.tsline() added.
# 1.2.7     20 July 2018       Function highcharter.heatmap() added.
# 1.2.8     20 July 2018       Function highcharter.addSeries.time() added
# 1.2.9     20 July 2018       Function highcharter.addSeries() modified: key column in the data is calculated correctly
# 1.3.0     20 July 2018       Function highcharter.combo() rewritten. addSeries removed
# 1.3.9     24 July 2018       Functions highcharter.line, area, bar, area.range, treemap, pie, pyramid, funnel, bubble, added.


# todo: add sunburst asap! take the example in tutorial


highcharter.line.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor', 'numeric', 'integer'),
    y       = c('numeric', 'integer', 'character', 'factor'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('x', 'y', 'color'),
  essentials = c('x', 'y')
)

highcharter.combo.defset = defset %>% list.edit(
  dimclass   = list(
    x         = c('character', 'factor', 'numeric', 'integer', 'Date', 'POSIXct'),
    y         = c('numeric', 'integer', 'character', 'factor', 'Date', 'POSIXct'),
    size      = c('numeric', 'integer'),
    shape     = c('character'),
    low       = c('numeric', 'integer'),
    high      = c('numeric', 'integer'),
    color     = c('character', 'factor', 'numeric', 'integer'),
    linkColor = c('character', 'factor', 'numeric', 'integer')),
  
  multiples  = c('x', 'y', 'low', 'high', 'size', 'color', 'linkColor', 'shape'),
  essentials = c('x', 'y')
)

highcharter.bubble.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor', 'numeric', 'integer'),
    y       = c('numeric', 'integer', 'character', 'factor'),
    size    = 'numeric',
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('x', 'y', 'color'),
  essentials = c('x', 'y')
)

highcharter.area.range.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('character', 'factor'),
    low     = c('numeric', 'integer'),
    high    = c('numeric', 'integer'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c('low', 'high', 'color'),
  essentials = c('x', 'low', 'high')
)


highcharter.funnel.defset = defset %>% list.edit(
  dimclass   = list(
    label   = c('character', 'factor'),
    size    = c('numeric', 'integer'),
    color   = c('character', 'factor', 'numeric', 'integer')),
  multiples  = c(),
  essentials = c('x', 'y')
)


highcharter.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c('x', 'y', 'size', 'color', 'shape'),
  essentials = c('x', 'y'),
  colorize   = F,
  yAxis.gridLine.width = 1
  
)

highcharter.scatter.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    size    = 'numeric',
    group   = c('factor', 'character'),
    color   = valid.classes),
  multiples  = c(),
  essentials = c('x', 'y'),
  colorize   = F
)

highcharter.shape = c(point = 'scatter', line.point = 'line', area.range = 'arearange', 
                      line.smooth = 'spline', area.smooth = 'areaspline', area.range.smooth = 'areasplinerange',
                      bar.range = 'columnrange')

tree2RootSeries = function(tr, value.var = 'value', idsuffix = ''){
  tr2 = list()
  for (e in tr){
    e2 = list()
    if      (inherits(e, 'TREE')){e2 = list(name = e$tree_name, y = e[[value.var]], drilldown = tolower(e$tree_name) %++% idsuffix)}
    else if (inherits(e, 'list')){e2 = list(e$leaf_name, e[[value.var]])}
    if(!is.empty(e2)){tr2[[length(tr2) + 1]] = e2}
  }
  return(tr2)
}

tree2DrilldownSeries = function(tr, dd = list(), type = NULL, series_name = NULL, value.var = 'value', idsuffix = ''){
  for (e in tr){
    e2 = list()
    if      (inherits(e, 'TREE')){
      dd %<>% list.add(
        list(
          id   = tolower(e$tree_name) %++% idsuffix,
          type = type,
          name = chif(is.null(series_name), e$tree_name, series_name),
          data = tree2RootSeries(e, value.var = value.var, idsuffix = idsuffix)
        )
      )
      dd = tree2DrilldownSeries(e, dd, type = type, series_name = series_name, value.var = value.var, idsuffix = idsuffix)
    }
  }
  return(dd)
}

highcharter.applyConfig = function(h, config = list()){
  # verifications:
  verify(h, 'highchart', varname = 'h')
  verify(config, 'list', varname = 'config')
  
  
  if(!is.null(config$title)){h = hc_title(h, text = config$title)}
  if(!is.null(config$subtitle)){h = hc_subtitle(h, text = config$subtitle)}
  
  thm = highcharter::hc_theme(
    chart = list(
      # todo: define default for all of them
      backgroundColor = config$background.color %>% verify('character', default = 'white'),
      borderColor     = config$border.color,
      borderRadius    = config$border.radius,
      borderWidth     = config$border.width
    ),
    title = list(
      style = list(
        color = config$title.color,
        fontFamily = config$title.font
      )
    ),
    subtitle = list(
      style = list(
        color = config$subtitle.color,
        fontFamily = config$subtitle.font
      )
    )
  ) %>% list.clean
  
  config$legend %<>% verify('logical', domain = c(T,F), default = F, varname = 'config$legend')
  if(config$legend){
    thm$legend = list(
      itemStyle = list(
        fontFamily = config$legend.item.font,
        color      = config$legend.item.color
      ),
      itemHoverStyle = list(
        color = config$legend.item.hover.color,
        fontFamily = config$legend.item.hover.font
      )
    )
  }
  
  if(!is.null(config$yAxis.gridLine.width)){thm$yAxis$gridLineWidth = config$yAxis.gridLine.width}
  if(!is.null(config$tooltip)){h %<>% hc_tooltip(headerFormat = config$tooltip[1],
                                                 pointFormat  = config$tooltip[2])}
  
  if(!is.empty(thm)){h %<>% highcharter::hc_add_theme(thm)}
  h %<>%
    highcharter::hc_xAxis(title = list(text = config$xAxis.label)) %>%
    highcharter::hc_yAxis(title = list(text = config$yAxis.label)) %>%
    highcharter::hc_legend(enabled = !is.null(config$legend)) %>%
    highcharter::hc_chart(borderColor  = config$border.color,
                          borderWidth  = config$border.width)
  
  if(!is.null(config$border.radius)){h %<>% highcharter::hc_xAxis(borderRadius = config$border.radius)}
  
  config$barMode %<>% verify('character', lengths = 1, domain = c('group', 'stack', 'relative'), varname = config$barMode, default = 'group')
  if(config$barMode == 'stack'){h %<>% hc_plotOptions(column = list(stacking = "normal"), bar = list(stacking = "normal"))}
  return(h)
}

highcharter.combo = function(obj, x = NULL, y = NULL, size = NULL, low = NULL, high = NULL, color = NULL, linkColor = NULL, shape = NULL, config = NULL){
  # Preparing Aesthetics:
  config = highcharter.combo.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'highcharter') %>% verifyConfigDimProperties(dims = 'color')
  
  if(is.null(shape)){shape = 'line'}
  
  a = prepareAesthetics(x = x, y = y, size = size, low = low, high = high, color = color, linkColor = linkColor, shape = shape, extend = c('shape', 'high', 'low'))
  
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  w = which(L$shape %in% names(highcharter.shape));
  if(length(w) > 0){L$shape[w] <- highcharter.shape[L$shape[w]] %>% unname}
  
  obj %<>% prepare4Plot(A, config)
  
  xnum    = is.numeric(obj[, L$x[1]]) 
  ynum    = chif(is.null(y), T, is.numeric(obj[, L$y[1]]))
  bars    = 'bar' %in% L$shape | 'column' %in% L$shape
  swapxy  = xnum & !ynum & bars
  special = length(c('pie', 'funnel', 'pyramid', 'treemap') %>% intersect(L$shape)) > 0
  
  if(swapxy){keep = L$y; L$y = L$x; L$x = keep; xnum = F; ynum = T}
  h    = highcharter::highchart()
  drll = F
  if(!xnum){
    w = which(L$shape == 'bar' | L$shape == 'column')
    if(length(w) > 0){L$shape[w] = chif(swapxy, 'bar','column')}
    if(inherits(obj[, L$x[1]], c('character', 'factor'))){
      if(length(L$x) > 1){
        drll = T
        h %<>% highcharter::hc_xAxis(type = 'category')
      } else {
        obj[, L$x] %<>% as.character
        ccn = obj[, L$x] %>% unique
        ccn.indx = (ccn %>% length %>% sequence) - 1; names(ccn.indx) <- ccn
        h %<>% highcharter::hc_xAxis(type = 'category', categories = ccn)
        if(!special){obj[, L$x] <- ccn.indx[obj[, L$x]]}
      }
    } else
      if(inherits(obj[, L$x[1]], c('Date', 'POSIXct'))){
        assert(length(L$x) == 1)
        h %<>% highcharter::hc_xAxis(type = "datetime")
        obj[, L$x] <- datetime_to_timestamp(obj[, L$x])
      } else {stop('Ino dige man nemishnasam!!!!!')}
  } else {
    h %<>% highcharter::hc_xAxis(type = 'linear')
  }
  
  if(!ynum){
    w = which(L$shape == 'bar' | L$shape == 'column')
    if(length(w) > 0){L$shape[w] = chif(swapxy, 'column','bar')}
    if(inherits(obj[, L$y[1]], c('character', 'factor'))){
      if(length(L$y) > 1){
        drll = T
        h %<>% highcharter::hc_yAxis(type = 'category')
      } else {
        obj[, L$y] %<>% as.character
        ccn = obj[, L$y] %>% unique
        ccn.indx = (ccn %>% length %>% sequence) - 1; names(ccn.indx) <- ccn
        h %<>% highcharter::hc_yAxis(type = 'category', categories = ccn)
        if(!special){obj[, L$y] <- ccn.indx[obj[, L$y]]}
      }
    } else
      if(inherits(obj[, L$y[1]], 'Date')){
        assert(length(L$y) == 1)
        h %<>% highcharter::hc_yAxis(type = "datetime")
        obj[, L$y] <- datetime_to_timestamp(obj[, L$y])
      } else {stop('Ino dige man nemishnasam!!!!!')}
  } else {
    h %<>% highcharter::hc_yAxis(type = 'linear')
  }  
  
  # todo: care for timedate class for x and y axis
  if(ynum & !xnum){
    dd   = list()
    if(!is.null(L$y)){
      sercount  = sequence(length(L$y))
      sernames  = L$y
    } else if (!is.null(L$size)){
      sercount = sequence(length(L$size))
      sernames  = L$size
    } else if (!is.null(L$low)){
      sercount = sequence(length(L$low))
      sernames  = L$low
    }
    seriescolor = NULL
    for (i in sercount){
      # Find series color:
      if(is.null(L$color[i])){
        if(!is.null(config$color)){
          seriescolor = config$color[[sernames[i]]]
        }
      } else {seriescolor = obj[1, L$color[i]]}
      if(!is.null(linkColor[i])){
        if(L$shape[i] == 'line'){L$shape[i] = 'coloredline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'coloredarea'}
      }
      if(config$link.smooth.enabled %>% verify('logical', lengths = 1, domain = c(T,F), default = F)){
        if(L$shape[i] == 'line'){L$shape[i] = 'spline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'areaspline'} else
            if(L$shape[i] == 'arearange'){L$shape[i] = 'areasplinerange'}
      }
      
      if(drll){
        tr = obj[, c(L$x, L$y[i])] %>% df2tree(L$x, L$y[i], func = config$aggregator %>% verify('character', lengths = 1, default = 'sum'))
        dt = tr %>% tree2RootSeries(value.var = L$y[i], idsuffix = i)
        dd %<>% c(tr %>% tree2DrilldownSeries(value.var = L$y[i], type = L$shape[i], series_name = L$y[i], idsuffix = i))
        h %<>% highcharter::hc_add_series(data = dt, name = L$y[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      } else {
        dt = obj %>% nameColumns(list(x = L$x, y = L$y[i], z = L$size[i], high = L$high[i], low = L$low[i], color = L$color[i], segmentColor = L$linkColor[i]))
        if(special){names(dt)[1] <- 'name'}
        if(L$shape[i] == 'treemap'){names(dt)[[which(names(dt) == 'z')]] <- 'value'}
        h %<>% highcharter::hc_add_series(data = dt, name = L$y[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      }
    }
  } else if (xnum & !ynum){
    for (i in sequence(length(L$x))){
      if(is.null(L$color[i])){
        if(!is.null(config$color)){
          seriescolor = config$color[[L$x[i]]]
        }
      } else {seriescolor = obj[1, L$color[i]]}
      if(!is.null(linkColor[i])){
        if(L$shape[i] == 'line'){L$shape[i] = 'coloredline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'coloredarea'}
      }
      if(config$link.smooth.enabled %>% verify('logical', lengths = 1, domain = c(T,F), default = F)){
        if(L$shape[i] == 'line'){L$shape[i] = 'spline'} else
          if(L$shape[i] == 'area'){L$shape[i] = 'areaspline'} else
            if(L$shape[i] == 'arearange'){L$shape[i] = 'areasplinerange'}
      }
      
      if(drll){
        tr = obj[, c(L$y, L$x[i])] %>% df2tree(L$y, L$x[i], func = config$aggregator %>% verify('character', lengths = 1, default = 'sum'))
        dt = tr %>% tree2RootSeries(value.var = L$x[i], idsuffix = i)
        dd %<>% c(tr %>% tree2DrilldownSeries(value.var = L$x[i], type = L$shape[i], series_name = L$x[i], idsuffix = i))
        h %<>% highcharter::hc_add_series(data = dt, name = L$x[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      } else {
        dt = obj %>% nameColumns(list(x = L$x[i], y = L$y, z = L$size[i], high = L$high[i], low = L$low[i], color = L$color[i], segmentColor = L$linkColor[i]))
        if(special){names(dt)[2] <- 'name'}
        if(L$shape[i] == 'treemap'){names(dt)[[which(names(dt) == 'z')]] <- 'value'}
        h %<>% highcharter::hc_add_series(data = dt, name = L$x[i], type = L$shape[i], showInLegend = T, color = seriescolor)
      }
    }
  } else {
    stop('either x or y must be categorical!')
  }
  if(drll){h %<>% hc_drilldown(allowPointDrilldown = TRUE, series = dd)}
  
  return(h %>% highcharter.applyConfig(config)) 
}

highcharter.line = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){
  
  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'spline', 'line'), x = x, y = y, color = color, config = config)
}

highcharter.area = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){
  
  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'areaspline', 'area'), x = x, y = y, color = color, config = config)
}

highcharter.bar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL){
  
  config = highcharter.line.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = 'column', x = x, y = y, color = color, config = config)
}

highcharter.bubble = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, config = NULL){
  
  config = highcharter.bubble.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    list.edit(dimclass = highcharter.bubble.defset$dimclass %>% list.edit(z = 'numeric', size = NULL))
  
  obj %>% highcharter.combo(shape = 'bubble', x = x, y = y, z = size, color = color, config = config)
}

highcharter.area.range = function(obj, x = NULL, low = NULL, high = NULL, color = NULL, config = NULL){
  
  config = highcharter.area.range.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    list.edit(dimclass = highcharter.area.range.defset$dimclass %>% list.edit(y = 'numeric'), multiples = c('y', 'low', 'high', 'color'))
  
  obj %>% highcharter.combo(shape = chif(config$link.smooth.enabled, 'areasplinerange', 'arearange'), x = x, low = low, high = high, color = color, config = config)
}

highcharter.funnel = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = 'funnel', x = label, y = size, color = color, config = config)
}

highcharter.pyramid = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = 'pyramid', x = label, y = size, color = color, config = config)
}

highcharter.pie = function(obj, label = NULL, theta = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = 'pie', x = label, y = theta, color = color, config = config)
}

highcharter.treemap = function(obj, label = NULL, size = NULL, color = NULL, config = NULL){
  config = highcharter.funnel.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  obj %>% highcharter.combo(shape = 'treemap', x = label, size = size, color = color, config = config)
}

highcharter.scatter = function(obj, x = NULL, y = NULL, size = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])
  
  config = highcharter.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color, extend = c('y', 'x', 'size', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape', 'color')
  
  obj %<>% prepare4Plot(A, config)
  
  if(is.null(L$shape)){L$shape = 'point'}
  L$shape = highcharter.shape[L$shape]
  
  names(L$shape) <- NULL
  
  hc <- highchart(...)
  for (i in seq(L$y)){
    if(is.na(L$shape[i])){L$shape[i] <- 'scatter'}
    if (L$shape[i] == 'bubble'){
      if (is.null(L$size[i])){
        dta = cbind(obj[, L$x[i]], obj[, L$y[i]], rep(3,nrow(obj)))
      } else {
        dta = cbind(obj[, L$x[i]], obj[, L$y[i]], obj[, L$size[i]])
      }
    } else {dta = cbind(obj[, L$x[i]], obj[, L$y[i]])}
    
    hc %<>% highcharter::hc_add_series(data    = dta, name = L$y[i],
                                       type    = L$shape[i],
                                       color   = L$color[i])
  }
  
  hc %>% highcharter.applyConfig(config)
  
}

# http://rstudio-pubs-static.s3.amazonaws.com/230276_25781805506b41478e93da2a641ad108.html
# hcaes

highcharter.scatter.molten = function(obj, x = NULL, y = NULL, size = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])
  
  config = highcharter.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, color = color, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  # Implementation:
  scr = paste0("hchart(obj, 'scatter', hcaes(x = ",
               L$x, ", y = ", L$y,
               chif(is.null(L$size), "", ", size = " %++% L$size),
               chif(is.null(L$group), "", ", group = " %++% L$group),
               chif(is.null(L$color) | inherits(obj[,L$color], "character"), "", ", color = " %++% L$color),"),",
               chif(inherits(obj[,L$color], 'character'), "color = '" %++% obj[1,L$color] %++% "',", ""),
               "...)"
  )
  
  
  eval(parse(text = scr)) %>% highcharter.applyConfig(config)
}

# This function is incomplete and does not work! We need to resolve host for quantmod::getSymbol() function to get data first
highcharter.tsline.stock = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications:
  assert(require(highcharter), "Package highcharter is not installed!", err_src = match.call()[[1]])
  
  config = highcharter.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  hc = highcharter::highchart(type = 'stock') %>% highcharter::hc_add_series(L$x)
  for (i in L$y){
    hc %<>% highcharter::hc_add_series(i)
  }
  return(hc)
}





# highcharts.R ----------------------------------------------------------------


# Header
# Filename:       highcharts.R
# Description:    Contains functions for plotting various charts from package highcharts plus and rCharts hPlots using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     12 May 2017
# Last Revision:  12 May 2017
# Version:        1.0.0
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R

highcharts.scatter.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric', 'character'),
    y     = c('numeric', 'character'),
    size  = 'numeric',
    group = 'factor',
    shape = 'character'),
  multiples  = c('group', 'x', 'y'),
  essentials = c('x', 'y'),
  horizontal = F
)


# Use with molten table:
# valid shapes are: square, circle, bar, line
# Note: size only affect circles
# Note: config$horizontal is affective only if there is at least one bar among shapes.
highcharts.scatter.molten = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = rCharts.highcharts.scatter.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, group, size will be columns of the table
  
  obj %<>% prepare4Plot(A, config = config)
  
  translateShape = c(square = 'scatter', bubble = 'bubble', point = 'scatter', circle = 'bubble', bar   = ifelse(config$horizontal, 'bar', 'column'), line  = 'line')
  # warn if bar is not among shapes, horizontal does not work!
  
  L$shape = translateShape[L$shape]
  if(is.empty(L$shape)){L$shape = 'scatter'}
  L$shape[is.na(L$shape)] == 'bubble'
  
  hPlot(x = L$x, y = L$y, data = obj,
        type = L$shape, group = L$group, size = L$size)
}



# jscripts.R ----------------------------------------------------------------


# Header
# Filename:       jscripts.R
# Description:    Contains javascript function code for all htmlWidgets used in niravis package.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com.au
# Start Date:     13 June 2017
# Last Revision:  23 February 2018
# Version:        0.0.3
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     13 June 2017       Initial issue
# 0.0.2     23 February 2018   Function DT.link.click.js() added
# 0.0.3     23 February 2018   Function DT.links.click.js() added and exported

#### dimple:
dimple.js = function(field_name = 'group'){
  S1 =
    '<script>
  myChart.axes.filter(function(ax){return ax.position == "x"})[0].titleShape.text(opts.xlab)
  myChart.axes.filter(function(ax){return ax.position == "y"})[0].titleShape.text(opts.ylab)
  myChart.legends = [];
  svg.selectAll("title_text")
  .data(["'
  S2 = ''
  S3 =
    '"])
  .enter()
  .append("text")
  .attr("x", 499)
  .attr("y", function (d, i) { return 90 + i * 14; })
  .style("font-family", "sans-serif")
  .style("font-size", "10px")
  .style("color", "Black")
  .text(function (d) { return d; });
  var filterValues = dimple.getUniqueValues(data, "'
  S5 = '");
  l.shapes.selectAll("rect")
  .on("click", function (e) {
  var hide = false;
  var newFilters = [];
  filterValues.forEach(function (f) {
  if (f === e.aggField.slice(-1)[0]) {
  hide = true;
  } else {
  newFilters.push(f);
  }
  });
  if (hide) {
  d3.select(this).style("opacity", 0.2);
  } else {
  newFilters.push(e.aggField.slice(-1)[0]);
  d3.select(this).style("opacity", 0.8);
  }
  filterValues = newFilters;
  myChart.data = dimple.filterData(data, "'
  
  S6 = '", filterValues);
  myChart.draw(800);
  myChart.axes.filter(function(ax){return ax.position == "x"})[0].titleShape.text(opts.xlab)
  myChart.axes.filter(function(ax){return ax.position == "y"})[0].titleShape.text(opts.ylab)
  });
  </script>'
  return(paste0(S1,S2, S3, field_name, S5, field_name, S6))
}


#### TFD3:
TFD3.color.single.js = function(col){
  JS('function colorScale(obj, i){
     return "' %++% col %++% '"}')
  }

TFD3.color.nominal.js = function(domain, range){
  range %<>% vect.extend(length(domain))
  dp = paste(domain, range) %>% duplicated
  domain = domain[!dp]
  range  = range[!dp]
  ss = 'function colorScale(obj,i){
  var color = d3.scale.ordinal().domain([' %++%
    paste('"' %++% domain %++% '"', collapse = ',') %++% ']).range([' %++%
    paste('"' %++% range  %++% '"', collapse = ',') %++% ']);
  return color(i);}'
  return(JS(ss))
}

TFD3.color.numeric.js = function(domain, range){
  N  = length(range)
  q  = domain %>% quantile(probs = (0:(N-1))/(N-1))
  ss = 'function colorScale(obj,i){
  var color = d3.scale.linear().domain([' %++%
    paste(q, collapse = ',') %++% ']).range([' %++%
    paste('"' %++% range  %++% '"', collapse = ',') %++% ']);
  return color(i);}'
  return(JS(ss))
  }


TFD3.shape.bar.js = function(format = '.1f'){
  JS(paste0('function makeGraph(selection){
            // find out wich table and column
            var regex = /(col_\\d+)/;
            var col = regex.exec(this[0][0].className)[0];
            var regex = /tbl_(\\S+)/;
            var tbl = regex.exec(this[0][0].className)[1];
            var innerWidth = 117;
            var innerHeight = 14;
            
            // create a scaling function
            var max = colMax(tbl, col);
            var min = colMin(tbl, col);
            var wScale = d3.scale.linear()
            .domain([0, max])
            .range([0, innerWidth]);
            
            // text formatting function
            var textformat = d3.format("', format, '");
            
            // column has been initialized before, update function
            if(tbl + "_" + col + "_init" in window) {
            var sel = selection.selectAll("svg")
            .selectAll("rect")
            .transition().duration(500)
            .attr("width", function(d) { return wScale(d.value)});
            var txt = selection
            .selectAll("text")
            .text(function(d) { return textformat(d.value); });
            return(null);
            }
            
            // can remove padding here, but still cant position text and box independently
            this.style("padding", "5px 5px 5px 5px");
            
            // remove text. will be added back later
            selection.text(null);
            
            var svg = selection.append("svg")
            .style("position",  "absolute")
            .attr("width", innerWidth)
            .attr("height", innerHeight);
            
            var box = svg.append("rect")
            .style("fill", "lightblue")
            .attr("stroke","none")
            .attr("height", innerHeight)
            .attr("width", min)
            .transition().duration(500)
            .attr("width", function(d) { return wScale(d.value); });
            
            // format number and add text back
            var textdiv = selection.append("div");
            textdiv.style("position",  "relative")
            .attr("align", "right");
            
            textdiv.append("text")
            .text(function(d) { return textformat(d.value); });
            window[tbl + "_" + col + "_init"] = true;
}'))
}

TFD3.shape.bubble.js = function(){
  
  JS(paste0('function makeGraph(selection){
            
            // find out wich table and column
            var regex = /(col_\\d+)/;
            var col = regex.exec(this[0][0].className)[0];
            var regex = /tbl_(\\S+)/;
            var tbl = regex.exec(this[0][0].className)[1];
            
            // create a scaling function
            var domain = colExtent(tbl, col);
            var rScale = d3.scale.sqrt()
            .domain(domain)
            .range([8, 14]);
            
            // column has been initialized before, update function
            if(tbl + "_" + col + "_init" in window) {
            var sel = selection.selectAll("svg")
            .selectAll("circle")
            .transition().duration(500)
            .attr("r", function(d) { return rScale(d.value)});
            return(null);
            }
            
            // remove text. will be added later within the svg
            selection.text(null)
            
            // create svg element
            var svg = selection.append("svg")
            .attr("width", 28)
            .attr("height", 28);
            
            // create a circle with a radius ("r") scaled to the
            // value of the cell ("d.value")
            var circle = svg.append("g")
            .append("circle").attr("class", "circle")
            .attr("cx", 14)
            .attr("cy", 14)
            .style("fill", "orange")
            .attr("stroke","none")
            .attr("r", domain[0])
            .transition().duration(400)
            .attr("r", function(d) { return rScale(d.value); });
            
            // place the text within the circle
            var text = svg.append("g")
            .append("text").attr("class", "text")
            .style("fill", "black")
            .attr("x", 14)
            .attr("y", 14)
            .attr("dy", ".35em")
            .attr("text-anchor", "middle")
            .text(function (d) { return d.value; });
            window[tbl + "_" + col + "_init"] = true;
            
}'))
}


TFD3.font.bold.js = JS('function makeGraph(selection){selection.style("font-weight", "bold")}')

TFD3.font.js = function(weight = 'bold', side = 'right', format = '.1f'){
  sidestr   = chif(is.null(side)  , '', paste0('.classed("text-', side, '", true)'))
  weightstr = chif(is.null(weight), '', paste0('.style("font-weight", "', weight ,'")'))
  formatstr2 = chif(is.null(format), '', paste0('.text(function(d) { return textformat(d.value); })'))
  formatstr1 = chif(is.null(format), '', paste0('var textformat = d3.format("', format, '");'))
  JS(paste0('function makeGraph(selection){', formatstr1, 'selection', sidestr , weightstr, formatstr2, ';}'))
}

### DT:
DT.link.click.js = function(link){
  nms = names(link$inputs)
  listOfInputs = paste(nms, ': $(this).data("', nms, '")') %>% paste(collapse = ', \n')
  paste0('$(document).on("click", ".', link$class.name, '", function(e) {
         e.preventDefault();
         Shiny.onInputChange("', link$shinyInputName, '", {
         ', listOfInputs, '  });
});')
}

DT.links.click.js = function(links){
  scr = ''
  for (item in links){scr %<>% paste(DT.link.click.js(item), sep = '\n \n')}
  return(scr)
}

# dygraphs:
dygraphs.shape.multibar.js = JS("
                                function multiColumnBarPlotter(e) {
                                // We need to handle all the series simultaneously.
                                function darkenColor(colorStr) {
                                // Defined in dygraph-utils.js
                                var color = Dygraph.toRGB_(colorStr);
                                color.r = Math.floor((255 + color.r) / 2);
                                color.g = Math.floor((255 + color.g) / 2);
                                color.b = Math.floor((255 + color.b) / 2);
                                return 'rgb(' + color.r + ',' + color.g + ',' + color.b + ')';
                                }
                                
                                if (e.seriesIndex !== 0) return;
                                var g = e.dygraph;
                                var ctx = e.drawingContext;
                                var sets = e.allSeriesPoints;
                                var y_bottom = e.dygraph.toDomYCoord(0);
                                // Find the minimum separation between x-values.
                                // This determines the bar width.
                                var min_sep = Infinity;
                                for (var j = 0; j < sets.length; j++) {
                                var points = sets[j];
                                for (var i = 1; i < points.length; i++) {
                                var sep = points[i].canvasx - points[i - 1].canvasx;
                                if (sep < min_sep) min_sep = sep;
                                }
                                }
                                var bar_width = Math.floor(2.0 / 3 * min_sep);
                                var fillColors = [];
                                var strokeColors = g.getColors();
                                for (var i = 0; i < strokeColors.length; i++) {
                                fillColors.push(darkenColor(strokeColors[i]));
                                }
                                for (var j = 0; j < sets.length; j++) {
                                ctx.fillStyle = fillColors[j];
                                ctx.strokeStyle = strokeColors[j];
                                for (var i = 0; i < sets[j].length; i++) {
                                var p = sets[j][i];
                                var center_x = p.canvasx;
                                var x_left = center_x - (bar_width / 2) * (1 - j/(sets.length-1));
                                ctx.fillRect(x_left, p.canvasy,
                                bar_width/sets.length, y_bottom - p.canvasy);
                                ctx.strokeRect(x_left, p.canvasy,
                                bar_width/sets.length, y_bottom - p.canvasy);
                                }}}
                                ")

dygraphs.shape.bar.js = JS("
                           // This function draws bars for a single series. See
                           // multiColumnBarPlotter below for a plotter which can draw multi-series
                           // bar charts.
                           function barChartPlotter(e) {
                           function darkenColor(colorStr) {
                           // Defined in dygraph-utils.js
                           var color = Dygraph.toRGB_(colorStr);
                           color.r = Math.floor((255 + color.r) / 2);
                           color.g = Math.floor((255 + color.g) / 2);
                           color.b = Math.floor((255 + color.b) / 2);
                           return 'rgb(' + color.r + ',' + color.g + ',' + color.b + ')';
                           }
                           var ctx = e.drawingContext;
                           var points = e.points;
                           var y_bottom = e.dygraph.toDomYCoord(0);
                           ctx.fillStyle = darkenColor(e.color);
                           // Find the minimum separation between x-values.
                           // This determines the bar width.
                           var min_sep = Infinity;
                           for (var i = 1; i < points.length; i++) {
                           var sep = points[i].canvasx - points[i - 1].canvasx;
                           if (sep < min_sep) min_sep = sep;
                           }
                           var bar_width = Math.floor(2.0 / 3 * min_sep);
                           // Do the actual plotting.
                           for (var i = 0; i < points.length; i++) {
                           var p = points[i];
                           var center_x = p.canvasx;
                           ctx.fillRect(center_x - bar_width / 2, p.canvasy,
                           bar_width, y_bottom - p.canvasy);
                           ctx.strokeRect(center_x - bar_width / 2, p.canvasy,
                           bar_width, y_bottom - p.canvasy);
                           }}
                           ")

dygraphs.shape.candle.js = JS("
                              function candlePlotter(e) {
                              // This is the officially endorsed way to plot all the series at once.
                              var BAR_WIDTH = 8;
                              if (e.seriesIndex !== 0) return;
                              var setCount = e.seriesCount;
                              if (setCount != 4) {
                              throw 'Exactly 4 prices each point must be provided for candle chart (open close high low)';
                              }
                              var prices = [];
                              var price;
                              var sets = e.allSeriesPoints;
                              for (var p = 0 ; p < sets[0].length; p++) {
                              price = {
                              open : sets[0][p].yval,
                              close : sets[1][p].yval,
                              high : sets[2][p].yval,
                              low : sets[3][p].yval,
                              openY : sets[0][p].y,
                              closeY : sets[1][p].y,
                              highY : sets[2][p].y,
                              lowY : sets[3][p].y
                              };
                              prices.push(price);
                              }
                              var area = e.plotArea;
                              var ctx = e.drawingContext;
                              ctx.strokeStyle = '#202020';
                              ctx.lineWidth = 0.6;
                              for (p = 0 ; p < prices.length; p++) {
                              ctx.beginPath();
                              price = prices[p];
                              var topY = area.h * price.highY + area.y;
                              var bottomY = area.h * price.lowY + area.y;
                              var centerX = area.x + sets[0][p].x * area.w;
                              ctx.moveTo(centerX, topY);
                              ctx.lineTo(centerX, bottomY);
                              ctx.closePath();
                              ctx.stroke();
                              var bodyY;
                              if (price.open > price.close) {
                              ctx.fillStyle ='rgba(244,44,44,1.0)';
                              bodyY = area.h * price.openY + area.y;
                              }
                              else {
                              ctx.fillStyle ='rgba(44,244,44,1.0)';
                              bodyY = area.h * price.closeY  + area.y;
                              }
                              var bodyHeight = area.h * Math.abs(price.openY - price.closeY);
                              ctx.fillRect(centerX - BAR_WIDTH / 2, bodyY, BAR_WIDTH,  bodyHeight);
                              }}
                              ")


dygraphs.click.js = function(input_id){
  "function(e, x, points){
  var row = points[0].idx + 1;" %++%
    "Shiny.onInputChange('" %++% input_id %++% "', row)}"
}


### billboarder:

billboarder.format.suffix.js = function(suffix){
  if(is.null(suffix)){return(NULL)}
  JS(paste0("function(x) {return x + '", suffix, "';}"))}


# leaflet.R ----------------------------------------------------------------


# Header
# Filename:       leaflet.R
# Description:    Contains functions for plotting various maps from package 'leaflet' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     29 December 2016
# Last Revision:  29 December 2016
# Version:        1.0.0
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     29 December 2016   Initial issue



#' @export
leaflet.map.plot = function(x, stores = rownames(sg$spec), tiles = F, icon = NULL){
  if (inherits(x, "STORE.GROUP")){
    spc  = sg$spec[stores,]
    atms = rownames(spc)
    map  <- leaflet(spc, height = "100px") %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
    if (is.null(icon)){
      map <- addCircleMarkers(map, lng = ~Longitude, lat = ~Latitude, layerId = atms, popup = atms)
    } else {map  <- addMarkers(map, lng = ~Longitude, lat = ~Latitude, layerId = atms, popup = atms, icon = icon)}
    if (tiles){map <- addTiles(map)}
  } else {cat("OTHER CLASSES ARE NOT SUPPORTED YET!")}
  return(map)
}


#' @export
leaflet.map.Zoom = function(x, lat, long, dist = 0.01){
  # Check out x is of class leaflet map
  x %>% clearPopups() %>% fitBounds(min(long) - dist, min(lat) - dist, max(long) + dist, max(lat) + dist)
}



# morrisjs.R ----------------------------------------------------------------



# Header
# Filename:       morrisjs.R
# Description:    Contains functions for plotting various charts from package 'morrisjs' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     23 April 2017
# Last Revision:  26 May 2017
# Version:        0.0.4
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     23 April 2017      Initial issue
# 0.0.2     26 May 2017        package is imported silently
# 0.0.3     26 May 2017        argument 'obj' can be ignored or given an empty data.frame()
# 0.0.4     26 May 2017        Function morrisjs.options() modified: corrects package bug for single color



# http://morrisjs.github.io/morris.js/lines.html

morrisjs.tsline.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'Date',
    y       = 'numeric',
    color   = valid.classes),
  multiples  = 'y',
  essentials = c('x', 'y'),
  smooth = F
)

morrisjs.pie.defset = defset %>% list.edit(
  dimclass   = list(
    theta    = 'numeric',
    color    = valid.classes,
    label    = 'character'),
  multiples  = c(),
  essentials = c('theta', 'label'),
  resize     = TRUE
)

morrisjs.options = function(config, color = NULL){
  # Should add more arguments
  opt = list(resize = config$resize)
  # if(is.null(color)){color = config$color}
  if(!is.null(color)){if(length(color) == 2){color = c(color, 'black')}}  # STUPID PACKAGE !!!!!!!!!!!!!!! When length of color is 1, it draws everything in black !!!!
  if (!color %>% is.empty){
    opt$colors = color
    opt$lineColors = color
    opt$barColors = color
  }
  if (!is.null(config$tooltip)){
    opt$formatter = JS(config$tooltip)
    opt$hoverCallback = JS(config$tooltip)
  }
  opt$smooth = config$smooth
  opt$ymin = config$yAxis.min
  opt$ymax = config$yAxis.max
  opt$preUnits = config$yAxis.tick.label.prefix
  opt$postUnits = config$yAxis.tick.label.suffix
  # opt$lineColors = config$color
  
  opt %<>% list.clean
  if(opt %>% is.empty){return (NULL)} else {return(opt)}
}

morrisjs.tsline = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])
  
  config = morrisjs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(18,60,171,143,110))
  
  obj %<>% prepare4Plot(A, config)
  
  mx = max(obj[, L$y], na.rm = T); mn = min(obj[, L$y], na.rm = T); rndmlt = 10^((mx - mn) %>% log(10) %>% as.integer)
  
  if(is.null(config$yAxis.min)){config$yAxis.min <- mn %>% roundto.multiple(rndmlt, adjust = 'bottom')}
  if(is.null(config$yAxis.max)){config$yAxis.max <- mx %>% roundto.multiple(rndmlt, adjust = 'top')}
  
  morrisjs(obj) %>% mjsLine(option = config %>% morrisjs.options(color = L$color))
}

morrisjs.tsbar = function(obj, x = NULL, y = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])
  
  config = morrisjs.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  obj %<>% prepare4Plot(A, config)
  
  morrisjs(obj) %>% mjsBar(option = config %>% morrisjs.options(color = L$color))
}


morrisjs.pie = function(obj = data.frame(), theta = NULL, label = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  assert(require(morrisjs, quietly = T, warn.conflicts = F), "Package morrisjs is not installed!", err_src = match.call()[[1]])
  if(is.empty(label) | is.empty(theta)){return(NULL)}
  config = morrisjs.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  if(is.empty(obj)){return(NULL)}
  
  list(obj[,L$label], obj[,L$theta]) %>%  morrisjs(...) %>% mjsDonut(options = morrisjs.options(config, obj[, L$color]))
  
}


# networkD3.R ----------------------------------------------------------------


# Header
# Filename:       networkD3.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using package 'networkD3'.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     28 November 2016
# Last Revision:  27 June 2018
# Version:        1.1.2
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 November 2016   Initial issue
# 1.1.0     13 May 2017        Fundamental changes, using standard aesthetics
# 1.1.1     27 June 2018       Dimension 'key' added
# 1.1.2     27 June 2018       More config properties added

networkD3.sankey.defset = defset %>% list.edit(
  dimclass = list(
    key          = c('character', 'factor', 'integer'),
    label        = c('character', 'factor'),
    group        = c('character', 'factor'),
    size         = 'numeric',
    color        = valid.classes,
    borderColor  = valid.classes,
    linkColor    = valid.classes,
    tooltip      = 'character',
    source       = c('character', 'factor', 'integer'),
    target       = c('character', 'factor', 'integer'),
    linkLength   = 'numeric',
    linkWidth    = 'numeric',
    linkLabel    = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize = 'numeric'
  ),
  multiples  = c(),
  essentials = c('key', 'source', 'target'),
  link.tooltip.suffix = ''
)

networkD3.sankey = function(obj, key = NULL, label = NULL, source = NULL, target = NULL, linkWidth = NULL, config = NULL){
  
  obj %>% verify('list', lengths = 2, names_identical = c('nodes', 'links'), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'networkD3.sankey')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'networkD3.sankey')
  
  assert(require(networkD3), "Package networkD3 is not installed!", err_src = match.call()[[1]])
  
  config = networkD3.sankey.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'networkD3')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, source = source, target = target, linkWidth = linkWidth)
  L = a$labels
  A = a$aesthetics
  
  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label'), config) %>% distinct_(L$key, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkWidth'), config)
  
  assert(obj$links[, L$source] %<% obj$nodes[, L$key], "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'network.sankey')
  assert(obj$links[, L$target] %<% obj$nodes[, L$key], "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'network.sankey')
  
  if(!inherits(obj$nodes[, L$key], 'integer')){
    nodemap = sequence(nrow(obj$nodes)) - 1
    names(nodemap) <- obj$nodes[, L$key]
    
    obj$links[, L$source] = nodemap[obj$links[, L$source]] %>% unname
    obj$links[, L$target] = nodemap[obj$links[, L$target]] %>% unname
    obj$nodes[, L$key]    = nodemap[obj$nodes[, L$key]] %>% unname
  }
  
  sankeyNetwork(Links  = obj$links, Nodes = obj$nodes, Source = L$source,
                Target = L$target, Value = L$linkWidth, NodeID = L$label,
                units  = config$link.tooltip.suffix, fontSize = 0.5*config$node.label.size, nodeWidth = config$node.width)
}



# niraplot.R ----------------------------------------------------------------


# Header
# Filename:       niraplot.R
# Description:    Contains the major plotting function for niravis: niraplot
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     13 July 2017
# Last Revision:  13 July 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ------------------------------------
# 0.0.1     13 July 2017       Initial issue


#' @export
niraPlot = function(type, plotter, ...){
  parse(text = paste0(plotter, '.', type, "(...)")) %>% eval
}

# niravis.R ----------------------------------------------------------------

#' niravis: This package is a tool-box for visualization in R and making word-class and elegant shiny dashboards.
#'
#' @section Class DASHBOARD:
#' niravis provides a Reference class named as DASHBOARD.
#' You can design your interactive dashboard layout and assign multiple R objects into your dashboard.
#' It supports various visualization engines like graphics, ggplot2, ggvis, googlevis, amCharts, rCharts, dygraphs and many other plotter engines.
#'
#' @docType package
#' @name niravis
#'
#' @import niragen
#' @import magrittr
#'
#' @include visgen.R
#' @include dashtools.R
#' @include dashboard.R
#' @include dialogs.R
#' @include jscripts.R
#' @include rscripts.R
#' @include bubbles.R
#' @include bpexploder.R
#' @include bubbleCloud.R
#' @include c3.R
#' @include coffeewheel.R
#' @include candela.R
#' @include d3plus.R
#' @include TFD3.R
#' @include dimple.R
#' @include dialogs.R
#' @include DT.R
#' @include dygraphs.R
#' @include echarts.R
#' @include ggvis.R
#' @include googleVis.R
#' @include highcharter.R
#' @include highcharts.R
#' @include leaflet.R
#' @include morrisjs.R
#' @include networkD3.R
#' @include nvd3.R
#' @include plotly.R
#' @include amCharts.R
#' @include rbokeh.R
#' @include rCharts.R
#' @include visNetwork.R
#' @include xCharts.R

#' @include niraplot.R

# Version History:
# 1.1.0 (04 July 2016)      - dashboard.R changed to version 1.6
# 1.2.0 (20 July 2016)      - nira_plotters.R added (Version 1.1.0)
# 1.2.1 (08 September 2016) - dashboard.R changed to version 1.6.2
# 1.2.3 (07 September 2016) - nira_plotters.R changed to version 1.1.3
# 1.2.7 (08 September 2016) - tools.R renamed to dashtools.R and changed to version 1.7
# 1.3.0 (29 September 2016) - deviset.R transferred from package nira.timser
# 1.3.1 (29 September 2016) - visgen.R transferred from package niragen
# 1.3.2 (29 September 2016) - nira_plotters.R changed to version 1.1.4
# 1.4.0 (06 October 2016)   - nira_plotters.R changed to version 1.2.1 and renamed to: niraPlotter.R
# 1.4.1 (12 October 2016)   - dashboard.R changed to version 1.6.3
# 1.5.0 (14 October 2016)   - highcharterPlotters.R (version 1.0.0) added to the package
# 1.6.0 (18 October 2016)   - bubblesPlotters.R (version 1.0.0) added to the package
# 1.7.0 (25 November 2016)  - rChartsPlotters.R (version 1.1.0) added to the package
# 1.8.0 (25 November 2016)  - plotlyPlotters.R (version 1.0.0) added to the package
# 2.0.0 (28 November 2016)  - networkD3Plotters.R (version 1.0.0) added to the package
# 2.1.0 (28 November 2016)  - rAmChartsPlotters.R (version 1.0.0) added to the package
# 2.2.0 (29 November 2016)  - googleVisPlotters.R (version 1.2.0) added to the package
# 2.2.1 (25 December 2016)  - DT.R added
# 2.2.2 (29 December 2016)  - All plotter files renamed: 'Plotter' removed from the name. For example 'googleVisPlotters.R' renamed to 'googleVis.R'.
# 2.2.3 (29 December 2016)  - dygraphs.R added
# 2.2.4 (29 December 2016)  - leaflet.R added
# 2.2.5 (29 December 2016)  - plotly.R added
# 2.2.6 (02 February 2017)  - dygraphs.R updated to version 1.1.0
# 2.3.0 (28 March 2017)     - DT.R updated to version 1.1.1
# 2.4.0 (31 March 2017)     - dygraphs.R updated to version 1.2.2
# 2.4.1 (31 March 2017)     - bubbles.R updated to version 1.1.2
# 2.5.0 (02 April 2017)     - googleVis.R updated to version 1.3.0
# 2.6.0 (13 April 2017)     - c3.R added (version 0.0.1)
# 2.6.1 (14 April 2017)     - coffeewheel.R added (version 0.0.1)
# 2.6.2 (14 April 2017)     - d3plus.R added and updated to version 1.1.2
# 2.6.3 (14 April 2017)     - ggvis.R added (version 0.0.1)
# 2.6.4 (14 April 2017)     - rbokeh.R added (version 0.0.1)
# 2.6.5 (19 April 2017)     - amCharts.R updated to version 1.1.2
# 2.6.6 (20 April 2017)     - visNetwork.R updated to version 1.1.1
# 2.7.0 (12 May 2017)       - rCharts.R added and updated to version 1.1.3
# 2.7.1 (12 May 2017)       - dimple.R added (version 1.0.0)
# 2.7.2 (12 May 2017)       - highcharts.R added (version 1.0.0)
# 2.7.3 (12 May 2017)       - nvd3.R added and updated to version 1.0.3
# 2.7.4 (12 May 2017)       - xCharts.R added (version 1.0.0)
# 2.7.6 (13 May 2017)       - networkD3.R updated to version 1.1.0
# 2.8.0 (23 May 2017)       - rscripts.R added (Version 0.0.1)
# 2.8.1 (25 May 2017)       - plotly.R updated to version 1.2.5
# 2.8.2 (26 May 2017)       - morrisjs.R added and updated to version 0.0.4
# 2.9.0 (01 June 2017)      - dashboard.R updated to version 1.9.3
# 2.9.1 (01 June 2017)      - highcharter.R updated to version 1.2.1
# 2.9.2 (13 June 2017)      - jscripts.R added (Version 0.0.1) (Small codes collected from various plotter engine files into one file plus more code added for D3TableFilter)
# 3.0.0 (14 June 2017)      - D3TableFilter.R added and updated to version 0.0.3
# 3.0.1 (13 July 2017)      - visgen.R updated to version 1.2.6
# 3.1.0 (13 July 2017)      - niraplot.R added and replaced niraPlotter
# 3.1.2 (20 July 2017)      - coffeewheel embedded to the package! coffeewheel.R and dahboard.R modified
# 3.2.5 (25 July 2017)      - D3TableFilter triggers to re-plot if the sync table changes structure from server side.
#                             Files modified: dashboard.R to version 1.9.5, rscripts.R to version 0.1.1
# 3.2.7 (03 August 2017)    - dashboard.R modified to version 1.9.7
# 3.2.8 (29 August 2017)    - rscripts.R modified to version 0.1.2
# 3.2.1 (29 August 2017)    - coffeewheel.R modified to version 0.0.3
# 3.2.9 (31 August 2017)    - dashboard.R modified to version 1.9.8
# 3.3.0 (31 August 2017)    - D3TableFilter.R renamed to TFD3.R and modified to version 0.0.4
# 3.3.1 (12 September 2017) - TFD3.R modified to version 0.0.5
# 3.4.2 (11 October 2017)   - dashboard.R modified to version 1.10.2
# 3.4.3 (13 October 2017)   - dashboard.R modified to version 1.10.3
# 3.4.4 (16 October 2017)   - dashboard.R modified to version 1.10.4
# 3.5.0 (17 October 2017)   - pivot added to plotters: pivot.R added (ver 0.0.1), dashboard.R modified to version 1.10.5
# 3.5.2 (23 October 2017)   - rscripts.R changed to version 0.1.5
# 3.5.4 (27 October 2017)   - rscripts.R changed to version 0.1.7
# 3.5.5 (01 November 2017)  - TFD3.R changed to version 0.0.6
# 3.6.0 (10 November 2017)  - dashboard.R changed to version 2.0.0
# 3.6.1 (22 November 2017)  - csscript.R added (version 0.0.1)
# 3.7.1 (22 November 2017)  - dashboard.R changed to version 2.1.0
# 3.7.4 (27 November 2017)  - dashboard.R changed to version 2.1.4
# 3.7.5 (27 November 2017)  - rscripts.R changed to version 0.1.8
# 3.7.6 (27 November 2017)  - dashboard.R changed to version 2.1.5
# 3.8.7 (29 November 2017)  - dashboard.R changed to version 2.2.5
# 3.8.8 (29 November 2017)  - csscript.R changed to version 0.0.2
# 3.8.9 (29 November 2017)  - dashboard.R changed to version 2.2.6
# 3.9.0 (06 December 2017)  - rscripts.R changed to version 0.1.9
# 3.9.1 (06 December 2017)  - dashboard.R changed to version 2.2.7
# 4.0.1 (08 December 2017)  - rscripts.R changed to version 0.2.1
# 4.0.2 (08 December 2017)  - dashboard.R changed to version 2.2.8
# 4.1.3 (15 December 2017)  - dashboard.R changed to version 2.3.1
# 4.1.4 (19 December 2017)  - dashboard.R changed to version 2.3.2
# 4.1.5 (15 January 2018)   - TFD3.R changed to version 0.0.7
# 4.1.6 (16 January 2018)   - dashboard.R changed to version 2.3.3
# 4.1.7 (29 January 2018)   - dashboard.R changed to version 2.3.4
# 4.1.8 (12 February 2018)  - dashboard.R changed to version 2.3.5
# 4.2.0 (12 February 2018)  - rscripts.R changed to version 0.2.3
# 4.2.1 (13 February 2018)  - candela.R added (Version 0.0.1)
# 4.2.2 (13 February 2018)  - visgen.R modified to (Version 1.2.7)
# 4.2.3 (14 February 2018)  - plotly.R modified to (Version 1.2.6)
# 4.2.4 (14 February 2018)  - highcharter.R modified to (Version 1.2.2)
# 4.2.7 (23 February 2018)  - DT.R modified to (Version 1.1.4)
# 4.2.9 (23 February 2018)  - jscripts.R modified to (Version 0.0.3)
# 4.3.2 (28 February 2018)  - TFD3.R modified to (Version 0.0.8) & DT.R to version (1.1.6)
# 4.3.3 (05 March 2018)     - c3.R modified to (Version 0.0.2)
# 4.3.4 (05 March 2018)     - billboarder.R added (Version 0.0.1)
# 4.4.4 (28 April 2018)     - grviz.R added and developed to version 0.1.0
# 4.4.7 (23 May 2018)       - bpexploder.R added and developed to version 0.0.1
# 4.4.8 (29 May 2018)       - dimple.R updated to version 1.0.2
# 4.4.0 (29 May 2018)       - visgen.R updated to version 1.2.9
# 4.4.1 (01 June 2018)      - dialogs.R initiated and updated to version 0.1.1
# 4.4.8 (04 June 2018)      - dashboard.R changed to version 2.4.2
# 4.7.0 (05 June 2018)      - highcharter.R changed to version 1.2.4
# 4.7.1 (05 June 2018)      - TFD3.R changed to version 0.0.9
# 4.7.9 (16 June 2018)      - dashboard.R changed to version 2.5.0
# 4.8.0 (19 June 2018)      - visgen.R changed to version 1.3.0
# 4.9.0 (19 June 2018)      - echarts.R added and developed to version 0.1.0
# 4.9.1 (19 June 2018)      - highcharter.R changed to version 1.2.5
# 4.9.5 (20 June 2018)      - googleVis.R changed to version 1.3.4
# 4.9.6 (20 June 2018)      - amCharts.R changed to version 1.1.4
# 5.0.0 (21 June 2018)      - bubbleCloud.R added and developed to version 0.1.0
# 5.0.1 (22 June 2018)      - dashboard.R changed to version 2.5.1
# 5.0.7 (25 June 2018)      - billboarder.R changed to version 0.0.7
# 5.0.9 (25 June 2018)      - plotly.R changed to version 1.3.1
# 5.1.1 (27 June 2018)      - visNetwork changed to version 1.1.5
# 5.1.2 (30 June 2018)      - calheatmap.R added (version 0.0.1)
# 5.1.5 (30 June 2018)      - d3plus.R changed to version 1.1.5
# 5.1.7 (30 June 2018)      - plotly.R changed to version 1.3.3
# 5.2.1 (30 June 2018)      - amCharts.R changed to version 1.1.8
# 5.2.2 (01 July 2018)      - d3plus.R changed to version 1.1.5 +1
# 5.2.3 (02 July 2018)      - plotly.R changed to version 1.3.4 +1
# 5.2.4 (18 July 2018)      - billboarder.R changed to version 0.0.8 
# 5.2.7 (19 July 2018)      - dygraphs.R changed to version 1.2.4, c3.R changed to version 0.0.5, candela.R changed to ver 0.0.2
# 5.3.2 (20 July 2018)      - highcharter.R changed to version 1.3.0 
# 5.3.7 (23 July 2018)      - nvd3.R changed to version 1.0.5, visgen.R changed to version 1.3.3
# 5.4.1 (24 July 2018)      - amCharts.R changed to version 1.1.9, billboarder.R changed to version 0.1.0, c3.R changed to version 0.0.6
# 5.5.2 (24 July 2018)      - dimple.R changed to ver 1.0.4, highcharter.R changed to version 1.3.9
# 5.5.3 (25 July 2018)      - d3plus.R changed to version 1.1.6
# 5.5.4 (27 July 2018)      - dimple.R changed to ver 1.0.5
# 5.5.5 (28 July 2018)      - dc.R added (ver 0.0.1)
# 5.5.6 (11 September 2018) - sankeyTree.R added (ver 0.0.1)
# 5.5.8 (17 September 2018) - billboarder.R changed to ver 0.1.1, c3.R changed to version 0.1.0
# 5.5.9 (18 September 2018) - dashboard.R changed to ver 2.5.2
# 5.6.0 (19 September 2018) - dashtools.R changed to ver 0.0.5
# 5.6.1 (25 September 2018) - streamgraph.R added (ver 0.0.1)
# 5.6.2 (16 October 2018)   - plotly.R changed to ver 1.3.5
# 5.6.3 (18 October 2018)   - plotly.R changed to ver 1.3.6
# 5.6.4 (18 October 2018)   - grviz.R changed to ver 0.1.1
# 5.6.5 (18 October 2018)   - visgen.R changed to ver 1.3.4
# 5.6.7 (29 October 2018)   - dashboard.R changed to ver 2.5.4
# 5.7.2 (05 November 2018)  - grviz.R changed to ver 0.1.7
# 5.7.3 (05 November 2018)  - dashboard.R changed to ver 2.5.5
# 5.7.4 (06 November 2018)  - grviz.R changed to ver 0.1.7
# 5.7.4 (06 November 2018)  - dashboard.R changed to ver 2.5.6
NULL
#> NULL


# nvd3.R ----------------------------------------------------------------

# Header
# Filename:       nvd3.R
# Description:    Contains functions for plotting various nvd3 charts from rCharts package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     12 May 2017
# Last Revision:  23 July 2018
# Version:        1.0.5
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 May 2017         Initial issue separated from rCharts.R
# 1.0.2     12 May 2017         Applies color$config, added horizontal plot for nvd3.bar.molten
# 1.0.3     12 May 2017         Added nvd3.pie.molten()
# 1.0.4     23 July 2018        nvd3.scatter.molten() embedded in new added function added nvd3.scatter()
# 1.0.5     23 July 2018        nvd3.bar.molten() embedded in new added function added nvd3.bar()

nvd3.bar.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor', 'numeric', 'integer'),
    y     = c('numeric','integer', 'character', 'factor'),
    group = 'factor'),
  multiples  = c('x','y', 'group'),
  essentials = c('x', 'y'),
  palette = list(color = NULL)
)

nvd3.pie.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    group = c('factor', 'character', 'integer')),
  multiples  = c(),
  essentials = 'group',
  palette = list(color = NULL)
)

nvd3.pie.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    theta = c('numeric', 'integer'),
    label = c('factor', 'character')),
  multiples  = c(),
  essentials = c('theta', 'label'),
  palette = list(color = NULL)
)

nvd3.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('numeric', 'integer'),
    y     = 'numeric',
    shape = 'character',
    group = 'factor'),
  multiples  = c('x', 'y'),
  essentials = c('x', 'y')
)

nvd3.type = function(shape, config){
  shape %<>% verify('character', lengths = 1, domain = c('line', 'point', 'bubble', 'circle', 'bar'), default = 'point', varname = 'shape', err_src = 'nvd2.type')
  config$zoomWindow %<>% verify('logical', lengths = 1, domain = c(T, F), default = F, varname = 'config$zoomWindow', err_src = 'nvd2.type')
  if(shape == 'line'){
    if(config$zoomWindow){return('lineWithFocusChart')} else {return('lineChart')}
  } else if (shape == 'bar'){
    return('multiBarChart')
  }
  else {
    return('scatterChart')
  }
}

nvd3.applyConfig = function(np, config){
  np$xAxis(axisLabel = config$xAxis.label)
  np$yAxis(axisLabel = config$yAxis.label)
  if(!is.null(config$palette$color)){np$chart(color = config$palette$color)}
  return(np)
}

nvd3.applyColors = function(np, clrvect){
  np$chart(color = clrvect)
  return(np)
}

nvd3.bar = function(obj, x = NULL, y = NULL, group = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('color')
  
  obj %<>% prepare4Plot(A, config = config)
  
  hor = isHorizontal(obj, L$x, L$y)
  Ly = chif(hor, L$x, L$y); Lx = chif(hor, L$y, L$x)
  
  if(!is.null(L$group)){
    clrvect = getColorVect(Ly = obj[, L$group] %>% as.character %>% unique, Lcolor = L$color, config = config)
    
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    nv = nPlot(data = obj, x = Lx, y = Ly, group = L$group, type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
  } else {
    if(length(Ly) > 1){
      
      nv = nPlot(data = obj %>% reshape2::melt(id.vars = Lx, measure.vars = Ly), 
                 x = Lx, y = 'value', group = 'variable', type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
    } else {
      nv = nPlot(data = obj, x = Lx, y = Ly, type = chif(hor, "multiBarHorizontalChart" ,"multiBarChart"))
    }
    clrvect = getColorVect(Ly, L$color, config)
  }
  nv %>% nvd3.applyConfig(config) %>% nvd3.applyColors(clrvect)
}

nvd3.pie.molten = function(obj, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.pie.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  scr = paste("nPlot(~", L$group, ", data = obj, type = 'pieChart')")
  np = parse(text = scr) %>% eval
  np$chart(donut = config$donut %>% verify('logical', lengths = 1, domain = c(T,F), default = F, varname = 'config$donut'))
  return(np %>% nvd3.applyConfig(config))
}

nvd3.scatter = function(obj, x = NULL, y = NULL, shape = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, shape = shape, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config = config)
  
  if(!is.null(L$group)){
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    nv = nPlot(data = obj, x = L$x, y = L$y, group = L$group, type = nvd3.type(shape, config))
  } else {
    if(length(L$y) > 1){
      nv = nPlot(data = obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y), 
                 x = L$x, y = 'value', group = 'variable', type = nvd3.type(shape, config))
    } else {
      nv = nPlot(data = obj, x = L$x, y = L$y, type = nvd3.type(shape, config))
    }
  }
  nv %>% nvd3.applyConfig(config) %>% nvd3.applyColors(clrvect)
}

nvd3.pie = function(obj, theta = NULL, label = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = nvd3.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(theta = theta, label = label)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  scr = paste("nPlot(", L$theta,"~", L$label, ", data = obj, type = 'pieChart')")
  np = parse(text = scr) %>% eval
  np$chart(donut = config$donut %>% verify('logical', lengths = 1, domain = c(T,F), default = F, varname = 'config$donut'))
  return(np %>% nvd3.applyConfig(config))
  
}


# pivot.R ----------------------------------------------------------------


# Header
# Filename:       pivot.R
# Description:    Contains functions for plotting pivot table charts from js package pivottable using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     17 October 2017
# Last Revision:  17 October 2017
# Version:        0.0.1
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     17 October 2017     Initial issue

# # Default settings for plotter pivot:
# pivot.defset = defset %>% list.edit(
#   # Valid classes for all dimensions
#   dimclass  = list(x = valid.classes, y = valid.classes),
#   multiples = c('x','y'),
#   withRowNames  = F
# )



# https://github.com/nicolaskruchten/pivottable/wiki/Parameters
# Check d3pie from this developer: https://github.com/smartinsightsfromdata/rd3pie

pivot <- function(
  data,
  rows = NULL,
  cols = NULL,
  aggregatorName = NULL,
  vals = NULL,
  rendererName = NULL,
  sorter = NULL,
  exclusions = NULL,
  inclusions = NULL,
  ...,
  width = NULL,
  height = NULL
) {
  # check for data.frame, data.table, or array
  if( length(intersect(class(data),c("data.frame", "data.table", "table","structable", "ftable" ))) == 0 ) {
    stop( "data should be a data.frame, data.table, or table", call.=F)
  }
  
  #convert table to data.frame
  if( length(intersect(c("table","structable", "ftable"), class(data))) > 0 ) data <- as.data.frame( data )
  
  params <- list(
    rows = rows,
    cols = cols,
    aggregatorName = aggregatorName,
    vals = vals,
    rendererName = rendererName,
    sorter = sorter,
    ...
  )
  
  #   auto_box vectors of length 1
  params <- Map( function(p){
    if(length(p) == 1 ){
      p = list(p)
    }
    return(p)
  }
  , params
  )
  # exlusions & inclusions need to be "excluded" from auto_boxing
  par <- list(
    exclusions = exclusions,
    inclusions = inclusions
  )
  
  params <- c(params, par)
  
  # remove NULL parameters
  params <- Filter(Negate(is.null), params)
  
  x <- list(
    data = data,
    params = params
  )
  
  htmlwidgets::createWidget(
    name = 'pivot',
    x,
    width = width,
    height = height,
    package = 'niravis'
  )
}

## Widget output function for use in Shiny
##
## @param outputId Shiny output ID
## @param width width default '100\%'
## @param height height default '500px'
##
## @examples
##
##   # A simple example - this goes in the ui part of a shiny application
##
##   # pivotOutput("pivot")
##
##
## @export
pivotOutput <- function(outputId, width = '100%', height = '500px'){
  shinyWidgetOutput(outputId, 'pivot', width, height, package = 'niravis')
}

## Widget render function for use in Shiny
##
## @param expr pivot expression
## @param env environment
## @param quoted logical, default = FALSE
##
## @examples
##
##   # A simple example - this goes in the server part of a shiny application
##
##   # output$pivot <- renderPivot({
##   #          pivot(data =   canadianElections   ,  rows = c( "Province"),cols="Party",
##   #          vals = "votes", aggregatorName = "Sum", rendererName = "Table",
##   #          width="100%", height="500px")
##   # })
##
##
##
## @export
renderPivot <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, pivotOutput, env, quoted = TRUE)
}


NULL


## Plots a pivot table chart
## @param data data.frame or data.table (R>=1.9.6 for safety) with data to use in the pivot table
## @param rows String name of the column in the data.frame to prepopulate
##              the \strong{rows} of the pivot table.
## @param cols String name of the column in the data.frame to prepopulate
##              the \strong{columns} of the pivot table.
## @param aggregatorName String name of the pivottable.js aggregator to prepopulate the pivot table.
## @param vals String name of the column in the data.frame to use with \code{aggregatorName}. Must be additive (i.e a number).
## @param rendererName List name of the renderer selected, e.g. Table, Heatmap, Treemap etc.
## @param sorter String name this allows to implement a javascript function to specify the ad hoc sorting of certain values. See vignette for an example.
##              It is especially useful with time divisions like days of the week or months of the year (where the alphabetical order does not work).
## @param inclusions List this optional parameter allows to filter the members of a particular dimension "by inclusion".
##              Using the 'Titanic' example, to display only the "Crew" member in the "Class" dimension, it is convenient to filter by inclusion using `inclusions=list(Class="Crew")`.
##              Please note that this only pre-selects the visible filter(s) on the pivot table: the other dimension members are still availabe for selection if needed.
## @param exclusions String this optional parameter allows to filter the members of a particular dimension "by exclusion".
##              Using the 'Titanic' example, to display only the "1st", "2nd" and "3rd" members in the "Class" dimension, it is convenient to filter by exclusion using `exclusions=list(Class="Crew")`.
##              Please note that this only pre-selects the visible filter(s) on the pivot table: the other dimension members are still availabe for selection if needed.
## @param width width parameter
## @param height height parameter
##
## @param ... list other \href{https://github.com/nicolaskruchten/pivottable/wiki/Parameters}{parameters} that
##            can be passed to \code{pivotUI}. See Nicolas's Wiki for more details.
##            A further example of parameter is onRefresh. This parameters (shiny-only) introduces a JS function that allows to get back server side the list of parameters selected by the user.
##            An example is: onRefresh=htmlwidgets::JS("function(config) { Shiny.onInputChange('myPivotData', config); }")
##            This setting makes available server-side a function input$myPivotData that gives back a list (of lists) with all the slice & dice parameters offered by pivottable.
##            See the example onRefresh-shiny.R for an example of how to use this feature.
##            Example of usage could be:
##            These parameters could be saved and re-sent to the user.
##            Alternative they could be used to subset the data item for saving as csv.
##
## @examples
##
##  # use Titanic dataset provided in base R - simple creation with just data
##
##  pivot.table( Titanic )
##
##  # prepopulate multiple columns and multiple rows
##
##  pivot.table( Titanic, rows = c("Class","Sex"), cols = c("Age","Survived" ) )
##
##
##  # A more complete example:
##
##  pivot.table(
##  Titanic,
##  rows = "Survived",
##  cols = c("Class","Sex"),
##  aggregatorName = "Sum as Fraction of Columns",
##  vals = "Freq",
##  rendererName = "Table Barchart"
##  )
##
## # An example with inclusions and exclusions filters:
##
## pivot.table(
## Titanic,
## rows = "Survived",
## cols = c("Class","Sex"),
## aggregatorName = "Sum as Fraction of Columns",
## inclusions = list( Survived = list("Yes")),
## exclusions= list( Class = list( "Crew")),
## vals = "Freq",
## rendererName = "Table Barchart"
## )
##
##
##
##
## @import htmlwidgets
##
## @export
pivot.table = function(obj, ...){
  
  pivot(obj, ...)
}

# plotly.R ----------------------------------------------------------------


# Header
# Filename:       plotly.R
# Description:    Contains functions for plotting various charts from package 'plotly' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     29 December 2016
# Last Revision:  18 October 2018
# Version:        1.3.6
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     29 December 2016   Initial issue
# 1.0.1     28 February 2017   Function plotly.barChart() renamed to plotly.bar() and modified: Calls verifyPlotInputs() to verify inputs
# 1.1.0     03 March 2017      Function plotly.pie() added
# 1.1.1     09 March 2017      Function plotly.multi() modified: With new version of plotly, add_trace is replaced by various add_x functions like add_lines, add_markers, ...
# 1.1.2     24 March 2017      Function plotly.multi() modified: Argument color added. Specifies color of each trace
# 1.1.3     24 March 2017      Function plotly.multi() renamed to plotly.combo()
# 1.1.5     31 March 2017      Function plotly.combo() modified. Calls prepareAesthetics() from visgen.R
# 1.1.6     31 March 2017      plotly.combo.defset added
# 1.2.0     04 April 2017      plotly.scatter.defset added, Function plotly.scatter() added
# 1.2.3     13 April 2017      plotly.combo updated. Determines if the plot is horizontal
# 1.2.4     25 May 2017        Function plotly.applyConfig() modified: Some legend properties added to config
# 1.2.5     25 May 2017        require() is called with arguments quietly = TRUE & warn.conflicts = FALSE
# 1.2.6     14 February 2018   Function plotly.addSeries() modified: arguments 'symbol' and 'symbols' are set appropriately based on the values of 'shape' argument
# 1.2.7     21 May 2018        Function plotly.applyConfig() modified: config properties tickangle and layout margins applied.
# 1.2.8     29 May 2018        Function plotly.applyConfig() and plotly.prepareConfig() modified: config properties showtickLabel, zeroline and showgrid applied.
# 1.2.9     29 May 2018        Function plotly.pie() added
# 1.3.0     25 June 2018       Function prepareConfig() removed: All config verifications transferred to visgen
# 1.3.1     25 June 2018       plotly.box.molten added
# 1.3.2     30 June 2018       plotly.combo() modified: accepts class Date and POSIXct for x and y
# 1.3.3     30 June 2018       plotly.tsline() and plotly.tsbar() added.
# 1.3.4     02 July 2018       plotly.box.molten() renamed to plotly.box().
# 1.3.5     16 October 2018    plotly.box() modified: factors converted to character for computing nchar in order to adjust margin
# 1.3.6     18 October 2018    plotly.combo() modified: tooltip added



# To generate shiny input for event handling, read this:
# https://plot.ly/r/shiny-coupled-events/

valid.plotly.shapes = c('bar', 'line', 'point', 'line.point')


# symbols': '0', 'circle', 100', 'circle-open', '200', 'circle-dot', '300', 'circle-open-dot', '1', 'square', '101', 'square-open', '201', 'square-dot', '301', 'square-open-dot', '2', 'diamond', '102', 'diamond-open', '202', 'diamond-dot', '302', 'diamond-open-dot', '3', 'cross', '103', 'cross-open', '203', 'cross-dot', '303', 'cross-open-dot', '4', 'x', '104', 'x-open', '204', 'x-dot', '304', 'x-open-dot', '5', 'triangle-up', '105', 'triangle-up-open', '205', 'triangle-up-dot', '305', 'triangle-up-open-dot', '6', 'triangle-down', '106', 'triangle-down-open', '206', 'triangle-down-dot', '306', 'triangle-down-open-dot', '7', 'triangle-left', '107', 'triangle-left-open', '207', 'triangle-left-dot', '307', 'triangle-left-open-dot', '8', 'triangle-right', '108', 'triangle-right-open', '208', 'triangle-right-dot', '308', 'triangle-right-open-dot', '9', 'triangle-ne', '109', 'triangle-ne-open', '209', 'triangle-ne-dot', '309'),

plotly.symbol = c(bubble = 20, point = 20, circle = 20, square.hollow = 0, o = 1, circle.hollow = 1, bubble.hollow = 1, triangle.hollow = 2, plus = 3, x = 4, multiply = 4, rhombus.hollow = 5, curl.hollow = 6, square = 7, star = 8, rhombus.x = 9)
# plotly.symbol = c(bubble = 'circle', point = 'circle', circle = 'circle', o = 'o', circle.open = 'o')
plotly.mode  = c(line = 'lines', point = 'markers')
plotly.mode[names(plotly.symbol)] = 'markers'
plotly.mode['line.' %++% names(plotly.symbol)] = 'lines+markers'
plotly.symbol['line.' %++% names(plotly.symbol)] = plotly.symbol
valid.plotly.shapes = names(plotly.mode)

plotly.combo.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("character", "factor", "numeric", "integer", 'Date', 'POSIXct'),
    y = c("character", "factor", "numeric", "integer", 'Date', 'POSIXct'),
    color    = valid.classes,
    tooltip  = 'character',
    shape    = 'character'),
  multiples  = c('x', 'y', 'color', 'shape'),
  essentials = c('x', 'y')
)

plotly.box.defset = defset %>% list.edit(
  dimclass   = list(
    x      = c("character", "factor", "numeric", "integer"),
    y      = c("character", "factor", "numeric", "integer"),
    group  = c('character', 'factor', 'integer', 'ordered')),
  multiples  = c(),
  essentials = c('x', 'y')
)

plotly.tsline.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x = c("Date", "POSIXct", "numeric", "integer"),
    y = c("numeric", "integer", "Date", "POSIXct")),
  multiples  = c(),
  essentials = c('x', 'y')
)


plotly.scatter.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    # x = c('numeric', 'character'),
    x = valid.classes,
    y = c('numeric', 'character'),
    z = 'numeric',
    size = 'numeric',
    label = 'character',
    tooltip = 'character',
    color    = valid.classes,
    shape    = c( 'factor', 'character', 'integer')),
  multiples  = c('y', 'color', 'shape', 'size'),
  essentials = c('x', 'y'),
  colorize   = F,
  minSize    = 10,
  maxSize    = 1000,
  palette.shape = c('circle', 'square.hollow', 'triangle.hollow', 'plus', 'x', 'rhombus.hollow') # todo: should include all shapes and extended to domain size of the categorical field
)

plotly.pie.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    # x = c('numeric', 'character'),
    label = c('character', 'factor'),
    theta = c('numeric', 'integer')),
  essentials = c('label', 'theta'),
  xAxis.enabled = F,
  yAxis.enabled = F,
  xAxis.grid.enabled = F,
  yAxis.grid.enabled = F,
  xAxis.tick.label.enabled = F,
  yAxis.tick.label.enabled = F
)

plotly.addSeries = function(p, z = NULL, size = NULL, shape = NULL, name = NULL, label = NULL, tooltip = NULL, color = NULL, config = NULL, ...){
  if (is.empty(shape)){shape = 'line'}
  if (inherits(shape,c('integer', 'factor'))){
    mode   = gndcd(43, 39,31,53,130,121,144)
    symbol = shape
  } else {
    shape %<>% as.character
    mode   = chif(shape[1] == gndcd(142,39,110), NULL, chif(is.na(plotly.mode[shape[1]]),'markers', plotly.mode[shape[1]]))
    symbol = plotly.symbol[shape]
    symbol[is.na(symbol)] <- 20
    names(symbol) <- NULL
  }
  if (!is.empty(label) & !is.null(mode)){mode %<>% paste0('+text')}
  type = chif(shape[1] == 'bar', 'bar', chif(is.empty(z), 'scatter', 'scatter3d'))
  flg  = type == 'bar'
  if (!flg){
    flg = flg | mode == 'lines' | mode == 'lines+text'
  }
  if (flg){
    marker = NULL
    symbol = NULL
  } else {
    marker = config %>% plotly.makeMarker(name)
  }
  shapeknown = sum(!shape %in% names(plotly.symbol)) == 0
  p %>% add_trace(
    z       = chif(is.empty(z), NULL, z),
    size    = chif(is.empty(size), NULL, size),
    sizes   = chif(is.null(config$point.size.min) | is.null(config$point.size.max), NULL, c(config$point.size.min, config$point.size.max)),
    mode    = mode,
    symbol  = chif(shapeknown, symbol, NULL),
    symbols = chif(shapeknown, unique(plotly.symbol[shape]), plotly.symbol[config$palette.shape]),
    type    = type,
    name    = name,
    text    = chif(is.empty(label) | is.null(mode), chif(is.empty(tooltip), NULL, tooltip), label),
    color   = chif(is.empty(color), config$point.color, chif(inherits(color, c('factor', 'numeric', 'integer')), color, color %>% I)),
    colors  = chif(is.empty(color),NULL, chif(inherits(color, c('factor', 'numeric', 'integer')), config$palette.color, NULL)),
    hoveron = chif(type == 'scatter', 'fills+points', NULL),
    marker  = marker, hoverinfo = chif(is.empty(tooltip), NULL, 'text'), ...)
}


plotly.makeMarker = function(config, name = NULL){
  mkr = list(line = list())
  mkr$size       = config$point.size
  mkr$color      = config$point.color
  mkr$line$color = config$point.border.color
  mkr$line$width = config$point.border.width
  return(mkr)
}

# plotly.prepareConfig = function(config){
#   config$xAxis.enabled %<>% verify('logical', domain = c(T,F), default = T)
#   config$yAxis.enabled %<>% verify('logical', domain = c(T,F), default = T)
#   config$xAxis.label %<>% verify('character', lengths = 1, varname = "config$xAxis.label")
#   config$yAxis.label %<>% verify('character', lengths = 1, varname = "config$yAxis.label")
#   config$barMode %<>% verify('character', domain = c('group', 'stack', 'relative'), lengths = 1, default = 'group', varname = "config$barMode")
#   config$xAxis.grid.enabled %<>% verify('logical', domain = c(T,F), default = config$xAxis.enabled)
#   config$yAxis.grid.enabled %<>% verify('logical', domain = c(T,F), default = config$yAxis.enabled)
#   config$xAxis.tick.label.enabled %<>% verify('logical', domain = c(T,F), default = config$xAxis.enabled)
#   config$yAxis.tick.label.enabled %<>% verify('logical', domain = c(T,F), default = config$yAxis.enabled)
#   
#   return(config)
# }

plotly.applyConfig = function(p, config){
  plotly::layout(p, title = config$title,
                 xaxis  = list(title = config$xAxis.label, tickangle = config$xAxis.tick.angle, showgrid = config$xAxis.grid.enabled, zeroline = config$xAxis.enabled, showticklabels = config$xAxis.tick.label.enabled), 
                 yaxis  = list(title = config$yAxis.label, tickangle = config$yAxis.tick.angle, showgrid = config$yAxis.grid.enabled, zeroline = config$yAxis.enabled, showticklabels = config$yAxis.tick.label.enabled),
                 margin = list(b = config$xAxis.margin.bottom, r = config$yAxis.margin.right, l = config$yAxis.margin.left),
                 showlegend = config$legend.enabled,
                 legend = list(
                   font = list(
                     family = config$legend.font,
                     color  = config$legend.color,
                     size   = config$legend.size
                   ) %>% list.clean,
                   bgcolor = config$legend.background,
                   bordercolor = config$legend.border.color,
                   borderwidth = config$legend.border.width,
                   orientation = config$legend.orientation
                 ),
                 barmode = config$barMode)
}

plotly.scatter = function(obj, x, y, z = NULL, size = NULL, shape = NULL, color = NULL, label = NULL, tooltip = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  if(is.null(shape)){shape = 'point'}
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.scatter.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, z = z, size = size, shape = shape, color = color, label = label, tooltip = tooltip, extend = c('y', 'shape', 'color'))
  L = a$labels
  A = a$aesthetics
  
  # Preparing Table:
  obj %<>% prepare4Plot(A, config = config)
  
  if(!is.null(L$color)){for (cl in L$color){obj[, cl] %<>% as.factor}}
  p <- plot_ly(x = obj[,L$x], ...)
  for (i in seq(L$y)){
    p %<>% plotly.addSeries(y = obj[,L$y[i]], z = obj[, L$z[i]], size = obj[ ,L$size[i]], shape = obj[, L$shape[i]], tooltip = obj[, L$tooltip[i]], name = L$y[i], label = obj[,L$label], color = obj[,L$color[i]], config = config)
  }
  
  if(is.null(config$xAxis.label)){config$xAxis.label = L$x}
  if(is.null(config$yAxis.label)){config$yAxis.label = L$y}
  # config %<>% plotly.prepareConfig
  
  p %>% plotly.applyConfig(config)
}

plotly.combo = function(obj, x = NULL, y = NULL, shape = NULL, color = NULL, config = NULL, tooltip = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.combo.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, color = color, shape = shape, tooltip = tooltip, extend = c('y', 'x', 'shape','color', 'tooltip'))
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor', 'Date', 'POSIXct'))}
  
  if (hor){
    if(inherits(obj[,L$y[1]], 'character')){obj[,L$y[1]] <- factor(obj[,L$y[1]], levels = obj[,L$y[1]])} 
    
    g <- plot_ly(y = obj[,L$y[1]], orientation = 'h', ...)
    
    for (i in seq(L$x)){
      g %<>% plotly.addSeries(x = obj[, L$x[i]], shape = L$shape[i], name = L$x[i], color = obj[, L$color[i]], tooltip = obj[, L$tooltip[i]], config = config)
    }
  } else {
    if(inherits(obj[,L$x[1]], 'character')){obj[,L$x[1]] <- factor(obj[,L$x[1]], levels = obj[,L$x[1]])} 
    g <- plot_ly(x = obj[,L$x[1]], ...)
    
    for (i in seq(L$y)){
      g %<>% plotly.addSeries(y = obj[, L$y[i]], shape = L$shape[i], name = L$y[i], color = obj[, L$color[i]], tooltip = obj[, L$tooltip[i]], config = config)
    }
  }
  
  g %>% plotly.applyConfig(config)
}

plotly.bar = function(..., config = NULL){
  plotly.combo(..., shape = 'bar', config = config)
}

plotly.pie = function(obj, label = NULL, theta = NULL, config = NULL){
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  config = plotly.pie.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% verifyConfig(plotter = 'plotly')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, theta = theta)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  plot_ly(labels = obj[, L$label], values = obj[, L$theta], type = 'pie') %>% plotly.applyConfig(config)
  
}

plotly.box = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.box.defset %<==>% verify(config, 'list', default = list())) %>% verifyConfig(plotter = 'plotly') -> config
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  hor = T
  for (i in L$x){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in L$y){hor = hor & inherits(obj[,i], c('character', 'factor', 'Date', 'POSIXct'))}
  
  if(!hor & is.null(config$xAxis.margin.bottom)){config$xAxis.margin.bottom = 5*(obj[, L$x] %>% as.character %>% nchar %>% max)}
  if( hor & is.null(config$yAxis.margin.left))  {config$yAxis.margin.left   = 10*(obj[, L$y] %>% as.character %>% nchar %>% max)}
  
  if(is.null(L$x)){xfrml = NULL} else {xfrml = as.formula('~`' %++% L$x %++% '`')}
  if(is.null(L$y)){yfrml = NULL} else {yfrml = as.formula('~`' %++% L$y %++% '`')}
  if(is.null(L$group)){clrfrml = NULL} else {clrfrml = as.formula('~`' %++% L$group %++% '`')}
  
  p  = plot_ly(obj, x = xfrml, y = yfrml, color = clrfrml, type = "box") %>% plotly.applyConfig(config)  
  if(!is.null(L$group)){p %<>% layout(boxmode = "group")}
  p
}


plotly.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.tsline.defset %<==>% verify(config, 'list', default = list())) -> config
  
  obj %>% plotly.combo(x = x, y = y, shape = 'line', config = config, ...)
}

plotly.tsbar = function(obj, x = NULL, y = NULL, config = NULL, ...){
  # Verifications
  assert(require(plotly, quietly = T, warn.conflicts = F), "Package plotly is not installed!", err_src = match.call()[[1]])
  (plotly.tsline.defset %<==>% verify(config, 'list', default = list())) -> config
  
  obj %>% plotly.combo(x = x, y = y, shape = 'bar', config = config, ...)
}

# rbokeh.R ----------------------------------------------------------------



# Header
# Filename:       rbokeh.R
# Description:    Contains functions for plotting various charts from package 'rbokeh' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     24 April 2017
# Last Revision:  24 April 2017
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# -----------------------------------
# 0.0.1     24 April 2017      Initial issue


rbokeh.scatter.defset = defset %>% list.edit(
  dimclass   = list(
    x       = 'numeric',
    y       = 'numeric',
    color   = valid.classes,
    shape   = 'character'),
  multiples  = c('y', 'shape', 'color'),
  essentials = c('x', 'y'),
  colorize = F
)

rbokeh.scatter = function(obj, x = NULL, y = NULL, color = NULL, shape = NULL, config = NULL, ...){
  # todo: add size, fix tooltip and other properties
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rbokeh), "Package rbokeh is not installed!", err_src = match.call()[[1]])
  
  config = rbokeh.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  if(is.null(shape)){shape = 'point'}
  # Preparing Aesthetics:
  # a = prepareAesthetics(x = x, y = y, color = color, shape = shape, extend = c('y', 'color', 'shape'))
  a = prepareAesthetics(x = x, y = y, color = color, shape = shape)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config)
  
  scr = gndcd(33,148,76,154,1,94) %++% "(data = obj, title = config$title, xlab = config$xAxis.label, ylab = config$yAxis.label, ...)"
  for (i in seq(L$y)){
    funstr = chif(L$shape[i] == 'line', "ly_lines(ly_points(")
    if (is.empty(funstr)){funstr = "ly_points("}
    if(is.empty(L$shape[i])){glph = NULL}
    else if ((L$shape[i] %in% names(obj)) & (obj[,L$shape[i]] %>% unique %>% length > 1))
    {glph = L$shape[i]} else {glph = NULL}
    scr %<>% paste("%>%", funstr, "x =", L$x, ",", "y =", L$y[i], ", data = obj",
                   chif(is.empty(L$color[i]), "", ", color = " %++% L$color[i]),
                   chif(is.empty(glph), "", ", glyph = " %++% glph),
                   # chif(is.empty(config$tooltip), "", ", hover = list(" %++% paste(config$tooltip, collapse = ',')  %++% ")"),
                   ")")
  }
  
  parse(text = scr) %>% eval
}

# rCharts.R ----------------------------------------------------------------


# Header
# Filename:       rCharts.R
# Description:    Contains functions for plotting various charts from rCharts package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     31 March 2017
# Last Revision:  12 May 2017
# Version:        1.1.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     31 March 2017       Initial issue
# 1.0.1     31 March 2017       rCharts.scatter.defset added
# 1.0.2     14 April 2017       rCharts.scatter modified
# 1.1.0     12 May 2017         dimple charts separated and transferred to dimple.R
# 1.1.1     12 May 2017         nvd3 charts separated and transferred to nvd3.R
# 1.1.2     12 May 2017         xCharts charts separated and transferred to xCharts.R
# 1.1.3     12 May 2017         highcharts charts separated and transferred to highcharts.R


# Incomplete:
rCharts.scatter = function(obj, x = NULL, y = NULL, size = NULL, shape = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = rCharts.scatter.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, size = size, shape = shape, color = color)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape') # Only x, y, color, size will be columns of the table
  
  obj %<>% prepare4Plot(A, config = config)
  
  rcharts.type = c(square = 'scatter', bubble = 'bubble', point = 'scatter', circle = 'bubble', bar   = ifelse(config$horizontal, 'bar', 'column'), line  = 'line')
  # warn if bar is not among shapes, horizontal does not work!
  
  L$shape = translateShape[L$shape]
  if(is.empty(L$shape)){L$shape = 'scatter'}
  L$shape[is.na(L$shape)] == 'bubble'
  
  hPlot(x = L$x, y = L$y, data = obj,
        type = L$shape, group = L$group, size = L$size)
  
}


# rscripts.R ----------------------------------------------------------------


# Header
# Filename:       rscripts.R
# Description:    Contains functions generating various R scripts.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     23 May 2017
# Last Revision:  12 February 2017
# Version:        0.2.3
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     23 May 2017         Initial issue for TFD3 syncing observers
# 0.1.0     25 July 2017        Function TFD3.observer.table.S2C.R() modified: supports triggering a replot if sync and report tables have different structures
# 0.1.1     25 July 2017        TFD3.service() modified: re-plots if sync$itemID_trigger changes
# 0.1.2     29 August 2017      TFD3.observer.table.S2C.R() modified: Small bug rectified: Changing cell value to/from NA did not trigger a change and setCellValue() was not called.
# 0.1.3     12 October 2017     Observer function TFD3.observer.column.footer.R() changed. Code protected by isolate to be called only when sync$<itemID>_column.footer changes
# 0.1.5     23 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Table redraw is trigerred if any cell is replaced to/from NA
# 0.1.6     26 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Bug fixed! Column number was shifted for tables with rownames
# 0.1.7     27 October 2017     Observer function TFD3.observer.table.S2C.R() modified: Calls function compareTable() from niragen to extract changes to be sent
# 0.1.8     27 November 2017    loginService and loginUIExtraService for loginInput added
# 0.1.9     06 December 2017    'login.srv' and 'logout.srv' modified: Saves/loads session data after logout/login"
# 0.2.0     08 December 2017    'login.srv' and 'logout.srv' modified: saves/loads local objects in userData
# 0.2.1     08 December 2017    'login.srv' and 'logout.srv' modified: user is saved in session$user as well as sync$user
# 0.2.2     15 December 2017    'login.srv' modified: Sends a different fail message for when username is not in the 'loginTable'. usernames are rownames of property 'loginTable'
# 0.2.3     12 February 2018    'login.srv' and 'logout.srv' modified: session data is saved in the path given by settings$savePath rather than the working directory (adding more flexibility to the package user)


# Only server to client!
TFD3.observer.column.footer.R = function(itemID){paste0("
                                                        if(is.null(sync$", itemID, "_column.footer)){sync$", itemID, "_column.footer <- items[['", itemID, "']]$config$column.footer}
                                                        isolate({
                                                        nms = c('rownames', colnames(sync$", itemID, "))
                                                        for (col in names(sync$", itemID, "_column.footer)){
                                                        wch = which(nms == col) - 1
                                                        if   (inherits(sync$", itemID, "_column.footer[[col]], 'function')){val = sapply(list(sync$", itemID, "[report$", itemID, "_filtered, col]), sync$", itemID, "_column.footer[[col]])}
                                                        else {val = sync$", itemID, "_column.footer[[col]] %>% as.character}
                                                        for (cn in wch){if(!is.empty(val)){setFootCellValue(session, tbl = '", itemID, "', row = 1, col = cn, value = val)}}
                                                        }
                                                        })
                                                        ")}

# server to client
TFD3.observer.column.editable.R = function(itemID){paste0("
                                                          
                                                          if(is.null(sync$", itemID, "_column.editable)){sync$", itemID, "_column.editable <- items[['", itemID, "']]$config$column.editable}
                                                          isolate({
                                                          wrnflag = items[['", itemID, "']]$config$withRowNames
                                                          if(wrnflag %>% is.empty){wrnflag = T}
                                                          enacols = sync$", itemID, "_column.editable %>% unlist %>% coerce('logical') %>% which %>% names %>% intersect(c('rownames', colnames(sync$", itemID, ")))
                                                          discols = c('rownames', colnames(sync$", itemID, ")) %-% enacols
                                                          for(col in enacols){
                                                          if (col == 'rownames'){
                                                          if(wrnflag){enableEdit(session, '", itemID, "', 'col_0')}
                                                          } else {
                                                          w = which(names(sync$", itemID, ") == col) - !wrnflag
                                                          enableEdit(session, '", itemID, "', 'col_' %++% w);
                                                          }
                                                          }
                                                          
                                                          for(col in discols){
                                                          if (col == 'rownames'){
                                                          if(wrnflag){disableEdit(session, '", itemID, "', 'col_0')}
                                                          } else {
                                                          w = which(names(sync$", itemID, ") == col) - !wrnflag
                                                          disableEdit(session, '", itemID, "', 'col_' %++% w);
                                                          }
                                                          }
                                                          #debug(check)
                                                          #check(x = 'editable', y = sync$", itemID, "_column.editable, z = enacols, t = discols, r = items[['", itemID, "']])
                                                          })
                                                          ")
}

# client to server:
TFD3.observer.edit.R = function(itemID) {paste0("
                                                if(is.null(input$", itemID, "_edit)) return(NULL);
                                                edit <- input$", itemID, "_edit;
                                                isolate({
                                                # need isolate, otherwise this observer would run twice
                                                # for each edit
                                                id  <- edit$id;
                                                row <- as.integer(edit$row);
                                                col <- as.integer(edit$col);
                                                val <- edit$val;
                                                nms <- colnames(sync$", itemID, ")
                                                
                                                if(col == 0) {
                                                oldval <- rownames(sync$", itemID, ")[row];
                                                cellClass = 'character'
                                                fltr = items[['", itemID, "']]$config$column.acceptor[['rownames']]}
                                                else {
                                                oldval <- sync$", itemID, "[row, col];
                                                fltr = items[['", itemID, "']]$config$column.acceptor[[nms[col]]]
                                                cellClass = class(sync$", itemID, "[, col])[1]
                                                }
                                                val0   = val
                                                if (cellClass == 'POSIXct') {val = try(as.POSIXct(val, format = '%Y-%m-%dT%H:%M:%S', tz = 'GMT'), silent = T)} else {val = try(coerce(val, cellClass), silent = T)}
                                                
                                                accept = inherits(val, cellClass) & !is.empty(val)
                                                
                                                if(accept & !is.empty(fltr)){
                                                if(      inherits(fltr, 'character')){txt = paste('val', fltr, collapse = ' & ')}
                                                else if (inherits(fltr, 'list'))     {txt = rScriptFilter('val', fltr)}
                                                else if (inherits(fltr, 'function')) {txt = 'val %>% sapply(fltr)'}
                                                else                                 {txt = 'TRUE'}
                                                accept = parse(text = txt) %>% eval
                                                if(!inherits(accept, 'logical')){accept = T}
                                                }
                                                
                                                if (accept){
                                                if(col == 0) {
                                                rownames(sync$", itemID, ")[row] <- val;
                                                rownames(report$", itemID, ")[row] <- val;
                                                } else {
                                                shp = items[['", itemID, "']]$config$column.shape[[nms[col]]]
                                                if (!is.null(shp)){
                                                if(shp == 'radioButtons'){
                                                sync$", itemID, "[, col] <- FALSE;
                                                report$", itemID, "[, col] <- FALSE;
                                                }
                                                }
                                                # debug(check); check(x = 'Angoolak kardi!', y = input$", itemID, "_edit, z = val, r = row, t = col, s = sync$", itemID, ")
                                                sync$", itemID, "[row, col] <- val;
                                                report$", itemID, "[row, col] <- val;
                                                
                                                }
                                                # confirm edits
                                                confirmEdit(session, tbl = '", itemID, "', row = row, col = col, id = id, value = val);
                                                report$", itemID, "_lastEdits['Success', 'Row'] <- row;
                                                report$", itemID, "_lastEdits['Success', 'Column'] <- col;
                                                report$", itemID, "_lastEdits['Success', 'Value'] <- val;
                                                } else {
                                                
                                                # debug(check)
                                                # check(x = accept, y = fltr, z = val, t = val0, r = oldval, s = list(row, col, id))
                                                
                                                rejectEdit(session, tbl = '", itemID, "', row = row, col = col,  id = id, value = oldval);
                                                report$", itemID, "_lastEdits['Fail', 'Row'] <- row;
                                                report$", itemID, "_lastEdits['Fail', 'Column'] <- col;
                                                report$", itemID, "_lastEdits['Fail', 'Value'] <- val0;
                                                }
                                                })
                                                ")}

# Use it later for creating the default footer:
# footer = list('Mean', object[[i]] %>% colMeans %>% as.matrix %>% t) %>% as.data.frame
# names(footer) = c('Rownames', colnames(object[[i]]))

# Client 2 Server: FOB1
TFD3.observer.filter.C2S.R = function(itemID){
  paste0("
         if(is.null(input$", itemID, "_filter)){return(NULL)}
         isolate({
         report$", itemID, "_filtered <- unlist(input$", itemID, "_filter$validRows);
         sync$", itemID, "_column.filter = list()
         nms = c('rownames', colnames(sync$", itemID, "))
         # lapply(input$", itemID, "_filter$filterSettings, function(x) )
         for(flt in input$", itemID, "_filter$filterSettings){
         colnumb = flt$column %>% substr(5, nchar(flt$column)) %>% as.integer
         colname = nms[colnumb]
         if(!is.na(colname)){sync$", itemID, "_column.filter[[colname]] = chif(is.empty(flt$value), NULL, flt$value)}
         # debug(check)
         # check('FOB1', colnumb, colname, input$", itemID, "_filter$filterSettings, flt, sync$", itemID, "_column.filter)
         }
         # report$", itemID, "_column.filter = sync$", itemID, "_column.filter
         })
         ")
}

#  Server 2 Client: FOB2
TFD3.observer.filter.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_column.filter)){sync$", itemID, "_column.filter = items[['", itemID, "']]$config$column.filter}
         isolate({
         for(flt in input$", itemID, "_filter$filterSettings){
         nms = c('rownames', colnames(sync$", itemID, "))
         
         colnumb = flt$column %>% substr(5, nchar(flt$column)) %>% as.integer
         colname = nms[colnumb]
         colnumb = colnumb - 1
         
         if (colname %in% names(sync$", itemID, "_column.filter)){
         if (!identical(flt$value, sync$", itemID, "_column.filter[[colname]])){
         # set filter
         setFilter(session, tbl = '", itemID, "', col = 'col_' %++% colnumb, filterString = sync$", itemID, "_column.filter[[colname]], doFilter = TRUE);
         }
         # else {do nothing}
         } else {
         setFilter(session, tbl = '", itemID, "', col = 'col_' %++% colnumb, filterString = '', doFilter = TRUE);
         }
         # debug(check)
         # check('FOB2', y = input$", itemID, "_filter$filterSettings, z = colnumb, t = colname, r = flt, s = sync$", itemID, "_column.filter)
         # report$", itemID, "_column.filter = sync$", itemID, "_column.filter
         }
         })
         ")
}

# client to server: sob1
TFD3.observer.selected.C2S.R = function(itemID){
  paste0("
         if(is.null(input$", itemID, "_select)){return(NULL)}
         isolate({
         sync$", itemID, "_selected = input$", itemID, "_select
         report$", itemID, "_selected = sync$", itemID, "_selected
         })
         ")
}


# server 2 client: sob2
TFD3.observer.selected.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_selected)){sync$", itemID, "_selected = items[['", itemID, "']]$config$selected}
         isolate({
         if(is.null(report$", itemID, "_selected)){report$", itemID, "_selected = items[['", itemID, "']]$config$selected}
         if(is.null(sync$", itemID, "_row.color)){sync$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         sel   = sync$", itemID, "_selected %-% report$", itemID, "_selected
         desel = report$", itemID, "_selected %-% sync$", itemID, "_selected
         for (i in sel){  setRowClass(session, tbl = '", itemID, "', row = i, class = items['", itemID, "']$config$selection.color)}
         for (i in desel){setRowClass(session, tbl = '", itemID, "', row = i, class = chif(sync$", itemID, "_row.color[i] == items['", itemID, "']$config$selection.color, '', items[['", itemID, "']]$config$row.color[i]))}
         report$", itemID, "_selected = sync$", itemID, "_selected
         })
         ")
}


# server 2 client: for row color: cob2
TFD3.observer.color.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, "_row.color)){sync$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         isolate({
         # debug(check)
         # check(x = 'cob2', y = sync$", itemID, "_row.color, z = report$", itemID, "_row.color, t = sync$", itemID, ")
         if(is.null(report$", itemID, "_row.color)){report$", itemID, "_row.color = items[['", itemID, "']]$config$row.color}
         w = which(sync$", itemID, "_row.color != report$", itemID, "_row.color)
         for (i in w){setRowClass(session, tbl = '", itemID, "', row = i, class = sync$", itemID, "_row.color[i])}
         report$", itemID, "_row.color = sync$", itemID, "_row.color
         })
         ")
}


# server to client: for table contents: tob2
TFD3.observer.table.S2C.R = function(itemID){
  paste0("
         if(is.null(sync$", itemID, ")){
         sync$", itemID, " = items[['", itemID, "']]$data
         }
         isolate({
         if(is.null(report$", itemID, ")){report$", itemID, " <- items[['", itemID, "']]$data}
         if(!identical(report$", itemID,", sync$", itemID, ")){
         cmp = compareTables(report$", itemID,", sync$", itemID, ")
         # if(length(cmp)>0){debug(check); check(x = 'I am editing', y = cmp, z = items[['", itemID, "']]$config, r = report$", itemID,", s = sync$", itemID, ", t = session)}
         if(!is.null(cmp)){
         for(df in cmp){
         for(i in sequence(nrow(df))){
         setCellValue(session, tbl = '", itemID, "', row = df$row[i], col = df$col[i], value = df$to[i], feedback = FALSE)
         }
         }
         }
         # debug(check); check(x = 'I am changing items data', y = cmp, z = items, r = report, s = sync, t = session)
         items[['", itemID, "']]$data <<- sync$", itemID, "
         report$", itemID, "          <- sync$", itemID, "
         
         if(is.null(cmp)){sync$", itemID, "_trigger = T}
         
         }
         })
         ")
}

# server to client: for table contents: tob2
TFD3.observer.table.S2C.R.old = function(itemID){
  paste0("
         if(is.null(sync$", itemID, ")){sync$", itemID, " = items[['", itemID, "']]$data}
         isolate({
         if(is.null(report$", itemID, ")){report$", itemID, " <- items[['", itemID, "']]$data}
         
         trig = ncol(sync$", itemID, ") != ncol(report$", itemID,")
         trig = trig | nrow(sync$", itemID, ") != nrow(report$", itemID,")
         # trig = trig | rownames(sync$", itemID, ") != rownames(report$", itemID,")
         # trig = trig | colnames(sync$", itemID, ") != colnames(report$", itemID,")
         if(!trig){trig = length(which(is.na(sync$", itemID, ") != is.na(report$", itemID, "))) > 0}
         
         if (trig){
         items[['", itemID, "']]$data <<- sync$", itemID, "
         report$", itemID, " <- sync$", itemID, "
         } else {
         
         for (i in sequence(ncol(sync$", itemID, "))){
         # Check for classes of columns 2b identical, otherwise trigger a re-plot
         # todo: If you the column type is factor with different levels, an error is caused here! Either use character columns or convert them to character here before comparing
         ss = sync$", itemID, "[,i]
         rr = report$", itemID, "[,i]
         trig = !inherits(ss, rr %>% class)
         if(is.factor(ss)){trig = trig | !(levels(ss) %==% levels(rr))}
         
         if(!trig){
         w = which(ss != rr)
         # w = c(w, which((is.na(ss) & !is.na(rr))|(is.na(rr) & !is.na(ss))))
         # w = w - chif(items[['", itemID, "']]$config$withRowNames,0,1)
         for (j in w) {
         setCellValue(session, tbl = '", itemID, "', row = j, col = i, value = sync$", itemID, "[j,i], feedback = TRUE)
         report$", itemID, "[j,i] <- sync$", itemID, "[j,i]
         }
         }
         }
         rnew = rownames(sync$", itemID, ")
         rold = rownames(report$", itemID, ")
         w    = which(rnew != rold)
         
         for (j in w) {
         setCellValue(session, tbl = '", itemID, "', row = j, col = 0, value = rnew[j], feedback = TRUE)
         }
         rownames(report$", itemID, ") <- rnew
         }
         if(trig){sync$", itemID, "_trigger = T}
         })
         ")
}

TFD3.service = function(itemID){
  paste0("
         if(sync[['", itemID, "_trigger']]){
         sync[['", itemID, "_trigger']] = F
         }
         isolate({
         sync[['", itemID, "']] %>% TFD3.table(config = items[['", itemID, "']]$config)})
         ")
}


loginBox = "div(
textInput('getUser', 'Username'),
passwordInput('getPass', 'Password'),
br(),
actionButton('login', 'Log in')
  )"

logoutBox =
  "div(
' Logged in as:', strong(session$user),
actionLink('logout', label = 'Log out', icon = shiny::icon('sign-out'))
  )
"
loginUIService = paste0("if(sync$user %>% is.null){", loginBox, "} else {", logoutBox, "}")

login.srv = "if(!is.empty(input$getUser) & !is.empty(input$getPass)){
res = loginTable %>% loginVerify(input$getUser, input$getPass, settings$passEncryption)
if(inherits(res, 'character')){sync$message = messages['loginSuccess']; sync$user = input$getUser; session$user = input$getUser; if(settings$saveSession){flnm = settings$savePath %++% session$user %++% '.rds'; if(file.exists(flnm)){sessiondata = readRDS(flnm);for(i in names(sessiondata$local)){session$userData[[i]] = sessiondata$local[[i]]}; for (i in names(sessiondata$sync)){sync[[i]] <- sessiondata$sync[[i]]}}}} else {session$user = NULL; if(res == 1){sync$message = messages['loginFail']} else if(res == 2){sync$message = messages['usernameNotFound']} else {sync$message = 'This should not happen!!'} }
}"

logout.srv = "if(settings$saveSession){lc = list(); for (i in names(session$userData)){lc[[i]] <- session$userData[[i]]}; ss = list(); for (i in names(sync)){ss[[i]] <- sync[[i]]}; sessiondata = list(local = lc, sync = ss); saveRDS(sessiondata, settings$savePath %++% session$user %++% '.rds')}; session$user = NULL; sync$user = NULL"

loginUIExtraService = paste("observeEvent(input$login, {", login.srv, "})", "observeEvent(input$logout, {", logout.srv, "})", sep = '\n')


# sankeyTree.R ----------------------------------------------------------------



# Header
# Filename:       sankeytree.R
# Description:    Contains functions for plotting interactive sankey tree charts in using package sankeyTreeR.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     11 September 2018
# Last Revision:  11 September 2018
# Version:        0.0.1
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 0.0.1     11 September 2018  Initial issue

sankeytree.tree.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    size   = 'numeric',
    label  = c('character', 'factor'),
    color   = valid.classes),
  multiples  = 'label',
  essentials = c('size', 'label'),
  aggregator.function.string = 'sum'
)

sankeytree.prepareConfig = function(config){
  # config$title  %<>% verify('character', default = '', varname = 'config$title')
  # config$width  %<>% verify(c('integer', 'numeric'), default = 1200, varname = 'config$width') %>% as.integer
  # config$height %<>% verify(c('integer', 'numeric'), default = 800, varname = 'config$height') %>% as.integer
  
  return(config)
}

sankeytree.tree = function(obj, label = NULL, size = NULL, color = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(sankeytreeR), "Package sankeytreeR is not installed!", err_src = match.call()[[1]])
  
  config = sankeytree.tree.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  if (is.null(color)){
    color = list(colour = size[[1]])
  } else {
    names(color) = 'colour'
    color %<>% as.list
  }
  
  
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, size = size, color = color)
  L = a$labels
  A = a$aesthetics
  
  obj       %<>% prepare4Plot(A, config)
  config    %<>% sankeytree.prepareConfig
  
  obj %>% treemap::treemap(index = L$label, vSize = c(L$size), vColor = L$color, fun.aggregate = config$aggregator, draw = F) %>% 
  {.$tm} %>% rename(size = vSize, color = vColor) %>%
  {.[, c(L$label, 'size', 'color')]} %>% d3r::d3_nest(value_cols=c("size","color"), root = "ROOT", json = F) %>%
    mutate(size = sum(obj[, L$size])) %>% 
    d3r::d3_json(strip = T) %>% 
    sankeytreeR::sankeytree(maxLabelLength = 10, nodeHeight = 90, height = 600, width = 1300, treeColors = T, tooltip = list("size"))
}

# streamgraph.R ----------------------------------------------------------------



# Header
# Filename:       streamgraph.R
# Description:    Contains functions for plotting various charts from js package 'streamgraph' using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@gmail.com
# Start Date:     25 September 2018
# Last Revision:  25 September 2018
# Version:        0.0.1
#

# Version History:

# Version   Date                 Action
# ----------------------------------
# 0.0.1     25 September 2018    Initial issue


# Does not work with POSIXct, don't try!
streamgraph.tsarea.defset = defset %>% list.edit(
  dimclass   = list(
    x       = c('Date', 'integer', 'numeric'),
    y       = 'numeric',
    group   = c('factor', 'character', 'integer')),
  multiples  = c('y'),
  essentials = c('x', 'y')
)

streamgraph.applyConfig = function(chart, config){
  return(chart)
}


# ' Create a new streamgraph
# ' 
# ' \code{streamgraph()} initializes the streamgraph html widget
# ' and takes a data frame in "long" format with columns for the
# ' category (by default, it looks for \code{key}) and its associated
# ' \code{date}  and \code{value}. You can supply the names for those
# ' columns if they aren't named as such in your data.\cr
# ' \cr
# ' By default, interactivity is on, but you can disable that by setting
# ' the \code{interactive} parameter to \code{FALSE}.
# ' 
# ' @param data data frame
# ' @param key bare or quoted name of the category column (defaults to \code{key})
# ' @param value bare or quoted name of the value column (defaults to \code{value})
# ' @param date bare or quoted name of the date column (defaults to \code{date})
# ' @param width Width in pixels (optional, defaults to automatic sizing)
# ' @param height Height in pixels (optional, defaults to automatic sizing)
# ' @param offset see d3's \href{https://github.com/mbostock/d3/wiki/Stack-Layout#offset}{offset layout} for more details.
# '        The default is probably fine for most uses but can be one of \code{silhouette} (default),
# '        \code{wiggle}, \code{expand} or \code{zero}
# ' @param interpolate see d3's \href{https://github.com/mbostock/d3/wiki/SVG-Shapes#area_interpolate}{area interpolation} for more details.
# '        The default is probably fine fore most uses, but can be one of \code{cardinal} (default),
# '        \code{linear}, \code{step}, \code{step-before}, \code{step-after}, \code{basis}, \code{basis-open},
# '        \code{cardinal-open}, \code{monotone}
# ' @param interactive set to \code{FALSE} if you do not want an interactive streamgraph
# ' @param scale axis scale (\code{date} [default] or \code{continuous})
# ' @param top top margin (default should be fine, this allows for fine-tuning plot space)
# ' @param right right margin (default should be fine, this allows for fine-tuning plot space)
# ' @param bottom bottom margin (default should be fine, this allows for fine-tuning plot space)
# ' @param left left margin (default should be fine, this allows for fine-tuning plot space)
# ' @import htmlwidgets htmltools
# ' @importFrom tidyr expand
# ' @return streamgraph object
# ' @export
# ' @examples \dontrun{
# ' library(dplyr)
# ' library(streamgraph)
# ' ggplot2movies::movies %>%
# ' select(year, Action, Animation, Comedy, Drama, Documentary, Romance, Short) %>%
# '   tidyr::gather(genre, value, -year) %>%
# '   group_by(year, genre) %>%
# '   tally(wt=value) %>%
# '   ungroup %>%
# '   mutate(year=as.Date(sprintf("%d-01-01", year))) -> dat
# ' 
# ' streamgraph(dat, "genre", "n", "year")
# ' }
stream <- function(data, key, value, date, width=NULL, height=NULL, 
                   offset = c("silhouette", "wiggle", "expand", "zero"), 
                   interpolate=c("cardinal", "linear", "step", "step-before", "step-after", "basis", "basis-open", "cardinal-open", "monotone"), 
                   interactive=TRUE, scale = c("date", "continuous"), 
                   top=20, right=40, bottom=30, left=50, config = config) {
  
  offset      = match.arg(offset)
  interpolate = match.arg(interpolate)
  scale       = match.arg(scale)
  
  data <- data.frame(data)
  data <- data[, c(key, value, date)]
  colnames(data) <- c("key", "value", "date")
  
  xti <- config$xAxis.tick.interval %>% verify(c('integer', 'numeric'), lengths = 1, default = 1)
  
  if (scale=="date") {
    
    xtu <- config$xAxis.tick.unit %>% verify('character', lengths = 1, default = "day")
    xtf <- config$xAxis.tick.format %>% verify('character', lengths = 1, default = "%Y-%m-%d")
    # date format
    # if (all(class(data$date) %in% c("numeric", "character", "integer"))) {
    #   if (all(nchar(as.character(data$date)) == 4)) {
    #     data %>%
    #       mutate(date=sprintf("%04d-01-01", as.numeric(date))) -> data
    #     xtu <- 'Year'
    #     xtf <- "%Y"
    #   }
    # }
  } else {
    xtu <- NULL
    xtf <- ",.0f"
  }
  
  config$aggregator %<>% verify('function', default = sum)
  if(!inherits(data$date, 'numeric')){data %<>% group_by(key, date) %>% summarise(value = do.call(config$aggregator, list(value, na.rm = T)))}
  # call config aggregator function
  
  params = list(
    data=data,
    markers=NULL,
    annotations=NULL,
    offset=offset,
    interactive=interactive,
    interpolate=interpolate,
    palette="Spectral",
    text="black",
    tooltip="black",
    x_tick_interval=xti,
    x_tick_units=xtu,
    x_tick_format=xtf,
    y_tick_count=5,
    y_tick_format=",g",
    top=top,
    right=right,
    bottom=bottom,
    left=left,
    legend=FALSE,
    legend_label="",
    fill="brewer",
    label_col="black",
    x_scale=scale
  )
  
  htmlwidgets::createWidget(
    name = 'streamgraph',
    x = params,
    width = width,
    height = height,
    package = 'streamgraph'
  )
  
}

# ' Add a title to the streamgraph
# '
# ' @param sg streamgraph object
# ' @param title title
# ' @return THIS DOES NOT RETURN AN \code{htmlwidget}!! It returns a \code{shiny.tag}
# '         class HTML (the widget is wrapped in a \code{<div>}). It should be the LAST
# '         call in a magrittr pipe chain or called to wrap a streamgraph object for
# '         output
# ' @export
sg_title <- function(sg, title="") {
  
  div(style="margin:auto;text-align:center", strong(title), br(), sg)
  
}

streamgraph_html <- function(id, style, class, width, height, ...) {
  list(tags$div(id = id, class = class, style = style),
       tags$div(id = sprintf("%s-legend", id), style=sprintf("width:%s", width), class = sprintf("%s-legend", class),
                HTML(sprintf("<center><label style='padding-right:5px' for='%s-select'></label><select id='%s-select' style='visibility:hidden;'></select></center>", id, id))))
}


streamgraph.tsarea = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  assert(require(streamgraph), "Package streamgraph is not installed!", err_src = match.call()[[1]])
  
  config = streamgraph.tsarea.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'streamgraph')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config)
  
  configscale = chif(inherits(obj[, L$x], 'Date'), 'date', 'continuous')
  
  if(is.null(L$group)){
    if(length(L$y) > 1){
      sg = obj %>% reshape2::melt(id.vars = L$x, measure.vars = L$y) %>% 
        stream(date = L$x, value = 'value', key = 'variable', scale = configscale, config = config) 
    } else {
      sg = obj %>% stream(value = L$y, date = L$x, scale = configscale, config = config)
    }
  } else {
    assert(length(L$x) == 1 & length(L$y) == 1, 'You can define series by either grouping the values or selecting multiple columns!')
    sg = stream(data = obj, date = L$x, value = L$y, key = L$group, scale = configscale)
  }
  
  return(sg)
  
  # if(obj[, L$x] %>% duplicated %>% sum > 0 & !is.null(config$aggregator)){
  #   # if(inherits(config$aggregator, 'function')){config$aggregator = as.character(substitute(config$aggregator))}
  #   config$aggregator %>% verify('character')
  #   obj %<>% dplyr::group_by_(L$x)
  #   scr = "obj %>% dplyr::summarise("
  #   N   = length(L$y)
  #   for (i in sequence(N)){
  #     scr %<>% paste0("`", L$y[i], "` = ", config$aggregator, "(`", L$y[i], "`)", chif(i < N, ", ", ")"))
  #   }
  #   obj = parse(text = scr) %>% eval %>% as.data.frame
  # } 
  # 
  # if (!is.null(L$x)){
  #   if (inherits(obj[,L$x], 'Date')){ct = obj %>% streamgraph(x = L$x)} else {
  #     ct = obj %>% streamgraph %>% xAxis(type = 'category', categories = obj[, L$x] %>% as.character)
  #   }
  # } else {ct = obj %>% streamgraph}
  # if(!is.null(L$shape)){names(L$shape) <- L$y}
  # if(is.null(L$shape)){
  #   if(!is.null(config$shape)){
  #     L$shape = config$shape  
  #   } else {L$shape = 'line'}
  # }
  # 
  # L$shape %<>% vect.extend(length(L$y))
  # if(is.empty(config$stack.groups)){config$stack.groups = L$y}
  # options(warn = -1)
  # ct %<>% streamgraph_mixedGeom(type = most.common(L$shape), types = L$shape %>% as.list, stacked = chif(config$stack.enabled, config$stack.groups, NULL))
  # options(warn = 1)
  # 
  # return(ct %>% streamgraph.applyConfig(config))
}

# streamgraph.tsline = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'line', config = config, ...)
# }
# 
# streamgraph.tsarea = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.tsline.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
#     verifyConfig(plotter = 'streamgraph')
#   
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'area', config = config, ...)
# }
# 
# streamgraph.area = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
#     verifyConfig(plotter = 'streamgraph')
# 
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'area', config = config, ...)
# }

# streamgraph.line = function(obj, x = NULL, y = NULL, config = NULL, ...){
#   config = streamgraph.bar.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
#     verifyConfig(plotter = 'streamgraph')
#   
#   obj %>% streamgraph.combo(x = x, y = y, shape = 'line', config = config, ...)
# }


# TFD3.R ----------------------------------------------------------------


# Header
# Filename:       TFD3.R
# Description:    Contains functions for plotting table charts from TFD3 package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     26 April 2017
# Last Revision:  05 June 2018
# Version:        0.0.9
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 0.0.1     26 April 2017       Initial issue
# 0.0.2     14 June 2017        bug in TFD3.bgColScales() rectified.
# 0.0.3     14 June 2017        TFD3.config.verify modified: If config$column.color.auto is missing, fills with default value (TRUE)
# 0.0.4     31 August 2017      Function TFD3.table() modified: Argument extensions applied to TFD3
# 0.0.5     12 September 2017   Function TFD3.tableprops() modified: col_10 extended to col_100
# 0.0.6     01 November 2017    Function TFD3.tableprops() modified: Arguments related to filter pop-up added
# 0.0.7     15 January 2017     Function TFD3.config.verify() modified: property 'sort' added to checks
# 0.0.8     28 February 2018    config property paging_length renamed to paging.length same in DT
# 0.0.9     05 June 2018        column labels unnamed before being passed to argument 'colNames'

#' @include visgen.R

# Default settings for package DT:
TFD3.table.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    label = valid.classes,
    color = valid.classes),
  multiples = c('label', 'color'),
  withRowNames  = T,
  column.filter.enabled = TRUE
)

TFD3.table.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass  = list(
    label = valid.classes,
    color = valid.classes,
    shape = 'character',
    group = valid.nominal.classes),
  multiples = c('label', 'color', 'shape'),
  withRowNames  = F,
  column.filter.enabled = FALSE
)

TFD3.addColumnTypes = function(config, obj){
  TFD3.column_type = c(numeric = 'Number', character = 'String', Date = 'Date')
  types = apply(obj, 2, class)
  names(types) = NULL
  types = TFD3.column_type['type']
  types[is.na(types)] <- 'None'
  config$sort_config %<>% add.list(sort_types = types)
  return(config)
}

# Check here for complete reference:
# http://tablefilter.free.fr/doc.php

TFD3.tableprops = function(config){
  tblprp = config %>% list.extract('fixed_headers', 'tbody_height', 'filters_cell_tag', 'col_width', 'popup_filters', 'popup_filters_image', 'popup_filters_image_active',
                                   'popup_filters_image_html', 'popup_div_css_class', 'on_before_popup_filter_open', 'on_after_popup_filter_open', 'on_before_popup_filter_close', 'on_after_popup_filter_close',
                                   'inf_div_css_class', 'left_div_css_class', 'right_div_css_class', 'middle_div_css_class', 'paging',
                                   'flts_row_css_class', 'flt_css_class', 'flt_small_css_class', 'flt_multi_css_class', 'single_flt_css_class',
                                   'highlight_css_class', 'paging_slc_css_class', 'even_row_css_class', 'odd_row_css_class', 'btn_css_class', 'btn_reset_css_class',
                                   'input_watermark_css_class', 'active_columns_css_class', 'nb_pages_css_class', 'paging_btn_css_class',
                                   'on_keyup', 'on_keyup_delay',
                                   'grid', 'search_type', 'refresh_filters', 'rows_always_visible',
                                   'col_operation', 'exact_match', 'custom_cell_data',
                                   'btn', 'btn_text','btn_reset', 'btn_reset_text', 'btn_reset_html', 'btn_reset_target_id', 'btn_next_page_text', 'btn_prev_page_text', 'btn_last_page_text', 'btn_first_page_text',
                                   'btn_next_page_html', 'btn_prev_page_html', 'btn_last_page_html', 'btn_first_page_html',
                                   'page_text', 'of_text', 'showHide_cols_at_start',
                                   'sort', 'sort_select', 'sort_num_asc', 'sort_num_desc',
                                   'slc_filling_method', 'multiple_slc_tooltip',
                                   'rows_counter', 'rows_counter_text', 'col_number_format', 'sort_config', 'rows_always_visible',
                                   paste('col', 0:100, sep = '_'), 'rows_always_visible',
                                   'sort_config', 'msg_sort', 'on_sort_loaded')
  tblprp$paging_length = config$paging.length %>% verify(c('integer', 'numeric'), lengths = 1, domain = c(1,Inf), default = 10)
  return(tblprp)
}

TFD3.config.verify = function(config){
  config$sort                  %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$sort")
  config$withRowNames          %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$withRowNames")
  config$column.filter.enabled %<>% verify('logical', domain = c(T,F), lengths = 1, default = T, varname = "config$column.filter.enabled")
  config$selection.mode        %<>% verify('character', domain = c('single', 'multi'), lengths = 1, varname = 'config$selection.mode')
  config$selection.color       %<>% verify('character', domain = c('active', 'success', 'info', 'warning', 'danger'), default = 'info', lengths = 1, varname = 'config$selection.color')
  #config$footer.font.weight    %<>% verify('character', domain = c('bold'), lengths = 1, varname = 'config$footer.font.weight')
  #config$footer.font.adjust    %<>% verify('character', domain = c('left', 'right', 'center'), lengths = 1, varname = 'config$footer.font.adjust')
  #config$footer.font.format    %<>% verify('character', domain = 1:9 %++% '.f', lengths = 1, varname = 'config$footer.font.format')
  if (!is.null(config$column.color)){
    if (is.null(config$column.color.auto)){config$column.color.auto = list()}
    nms = names(config$column.color.auto)
    for(i in names(config$column.color)){if(!(i %in% nms)){config$column.color.auto[[i]] = T}}
  }
  # and many more ...
  return(config)
}

# Converts config$column.footer list to a data.frame 2b passed as argument 'footData' to function 'TFD3()'
TFD3.footData = function(obj, config){
  out = data.frame()
  rws = obj %>% TFD3.filteredRows(config)
  for (col in names(config$column.footer)){
    nms = c(chif(config$withRowNames,'rownames',NULL), colnames(obj))
    if   (config$column.footer[[col]] %>% inherits('function')){val = obj[rws, col] %>% list %>% sapply(config$column.footer[[col]])}
    else {val = config$column.footer[[col]] %>% as.character}
    if(col == 'rownames'){col = 'Rownames'}
    if(!is.empty(val)){out[1, col] = val}
  }
  return(chif(out %>% is.empty, NULL, out))
}

TFD3.rowStyles = function(obj, config){
  if(is.null(config$row.color) & is.null(config$selection.mode)){return(NULL)}
  
  if(!is.null(config$row.color)){
    out = config$row.color %>% verify('character', domain = c('', 'active', 'success', 'info', 'warning', 'danger'), varname = 'config$row.color') %>% vect.extend(nrow(obj))
  } else {out = rep('', nrow(obj))}
  
  if(!is.null(config$selection.mode)){
    out[config$selected] = 'info'
  }
  return(out)
}

# Generates column.editable from config to be given to argument 'edit' when TFD3() is called
TFD3.edit = function(colnames, config){
  if(is.empty(config$column.editable)){return(FALSE)}
  enacols = config$column.editable %>% verify('list', default = list()) %>%
    unlist %>% coerce('logical') %>% which %>% names %>% intersect(c(chif(config$withRowNames,'rownames',NULL), colnames))
  
  nms = c(chif(config$withRowNames,'rownames',NULL), colnames %>% verify('character', default = character(), varname = 'colnames')) %>% unique
  out = character()
  for(i in enacols){
    w = which(nms == i) - 1
    for (j in w){out %<>% c('col_' %++% w)}
  }
  return(out)
}

TFD3.lastEdits.empty <- data.frame(Row = c("", ""), Column = (c("", "")), Value = (c("", "")), stringsAsFactors = FALSE);
rownames(TFD3.lastEdits.empty) <- c("Fail", "Success");


TFD3.initialFilters = function(colnames, config){
  nms = c(chif(config$withRowNames,'rownames',NULL), colnames %>% verify('character', default = character(), varname = 'colnames')) %>% unique
  out = list()
  for(i in names(config$column.filter) %>% verify('character', domain = nms, default = character(), varname = 'names(config$column.filter)')){
    w = which (nms == i)
    for (j in w){out[['col_' %++% (w - 1)]] = config$column.filter[[i]]}
  }
  return(out)
}

TFD3.applyFilterstr = function(v, fltstr){
  # todo: currently it can only work with four very simple filterstrs, "<, <=, >=, >" does not support "=" and combined conditions with and , or, not, ...
  if(v %>% inherits('character')){return(fltstr %>% tolower %>% grep(v %>% tolower))}
  parse(text = paste('v', fltstr)) %>% eval %>% which
}

TFD3.filteredRows = function(obj, config){
  ff = obj %>% nrow %>% sequence
  for(i in names(config$column.filter)){
    if (i == 'rownames'){
      ff = ff %^% (rownames(obj) %>% TFD3.applyFilterstr(config$column.filter[[i]]))
    } else {
      ff = ff %^% (obj[, i] %>% TFD3.applyFilterstr(config$column.filter[[i]]))
    }
  }
  return(ff)
}

TFD3.colNames = function(config){
  if(is.null(config$column.title)){return(NULL)}
  cn = character()
  for (cc in names(config$column.title)){
    if(cc == 'rownames'){cn['Rownames'] <- config$column.title[[cc]]} else {cn[cc] <- config$column.title[[cc]]}
  }
  return(cn)
}

TFD3.bgColScales = function(obj, config){
  bgcs = list()
  nms  = c(chif(config$withRowNames,'rownames',NULL), colnames(obj))
  for (cc in names(config$column.color)){
    w = which(nms == cc) - 1
    # config$column.color.auto[cc] %<>% verify('list', domain = c(T,F), lengths = 1, default = F, varname = "config$column.color.auto['" %++% cc %++% "']")
    if(config$column.color[[cc]] %>% unique %>% length == 1){
      scr = TFD3.color.single.js(config$column.color[[cc]] %>% unique)
    } else if(config$column.color.auto[[cc]]){
      scr = paste('auto', config$column.color[[cc]] %>% paste(collapse = ':'), sep = ':')
    } else if(inherits(obj[, cc], valid.numeric.classes)){
      scr = TFD3.color.numeric.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else if (inherits(obj[, cc], valid.nominal.classes)){
      scr = TFD3.color.nominal.js(domain = obj[, cc], range = config$column.color[[cc]])
    } else {scr = ''}
    if(!is.empty(scr)){for (i in w){bgcs[[paste0('col_', i)]] <- scr}}
  }
  return(bgcs)
}

## TFD3 Generate a HTML table widget with advanced filtering, sorting and
## colouring.
##
## R interface to Max Guglielmi's \href{http://tablefilter.free.fr/ }{HTML Table
## Filter Generator} JavaScript library. Provides advanced filtering and
## sorting. Columns can be formatted using D3 functions.
##
## @section Configuration: The TFD3 widget can be highly customized.
##   See the website of the JavaScript library
##   \href{http://tablefilter.free.fr/}{HTML Table Filter Generator} for
##   details. Configuration is passed as a list of key value pairs to the
##   JavaScript engine.
##
## @section Extensions: Some of the TableFilter functions are beeing provided as
##   extensions, in particular \itemize{ \item ColsVisibility: Visibility of
##   columns can be adjusted by configuration or interactively \item
##   ColumnsResizer: Interactive resizing of column width \item
##   FiltersRowVisibility: Interactively show or hide the filter row. } To
##   activate these extensions simply define them as a character vector in the
##   extensions parameter, e.g. \code{extensions = c("ColsVisibility",
##   "ColumnsResizer", "FiltersRowVisibility")}. This takes care of enabling and
##   basic configuration of the extensions. For further customization use the
##   tableProps parameter.
##
## @section Editing: The whole table (\code{edit = TRUE}) or selected columns
##   (\code{edit = c("col_1", "col_3")}) can set to be editable. An editable
##   table provides an input element named like the corresponding output element
##   + "_edit". Here each (debounced) edit event in a table cell is visible as a
##   list of row (\code{row}), column (\code{col}) and new value (\code{val}).
##   See examples/interaction for a Shiny app demonstrating this feature.
##
## @section Colouring: Table columns can be colored based on their cells value
##   using D3.js colour scales. Table background and foreground (text) can be
##   coloured independently. Colour definitions are passed to the JavaScript
##   engine as D3 scale functions. This allows for a variety of scales for
##   different purposes. See
##   \href{https://github.com/mbostock/d3/wiki/Scales}{D3 scale documentation}
##   and examples below for details. As a shortcut a linear scale over the full
##   value range of a column can be defined as \code{col_n =
##   "auto:startcolour:endcolour"} (n is the column number, starting with 0).
##   For better mapping from numeric values to perceived intensity a HCL colour
##   interpolation is used.
##
## @section Row selection: If \code{selectableRows} is set to \code{"single"} or
##   to \code{"multi"}, the widget provides a shiny input named outputid +
##   "_select". On (\code{ctrl-}) mouse click the input delivers an array of 1
##   based row coordinates. Selected rows are highligthed using the "info"
##   Bootstrap class. \code{setRowClass} can be used to set or to unset this
##   class from the server.
##
## @section Sparklines: Table columns containing a comma separated series of
##   numbers (\code{"1,3,5,7,11"}) can be turned into sparkline visualizations.
##   For example, \code{sparklines = list(col_0 = list(type = "line"))} will
##   turn the cells of the first column into a minature line chart.
##
## @param df Data frame, matrix or or \link[crosstalk]{SharedData} object to display as html table
## @param enableTf Enable the features for the "HTML table filter generator"
## @param tableProps A list object describing appearence and function of the
##   table
## @param showRowNames Add the R row names as first column to the table
## @param colNames Named character list to display as column names
## @param extensions List of table filter extensions to load.
## @param selectableRows Enable row selection on (\code{cltr-}) mouse click. If
##   \code{"multi"} multiple rows will be selectable using (\code{cltr click}),
##   if  \code{"single"}  only a single line will be selectable.
## @param selectableRowsClass CSS class of selected row. Could be "active",
##   "success", "info", "warning", or "danger" from Bootstrap3. Default: "info."
## @param tableStyle List css classes to apply to a table. Bootstrap3 provides
##   \code{table}, \code{table-striped}, \code{table-bordered},
##   \code{table-hover}, and \code{table-condensed}. The \code{table-hover}
##   class is applied automatically if \code{selectableRows} is active. If
##   \code{tableStyle} is not NULL, the normal CSS styling of TableFilter is
##   automatically cut down by appending \code{stylesheet =
##   "tablefilter-2.5/filtergridBS.css"} to the tableProps.
## @param rowStyles Character vector of Bootstrap classes to apply to rows.
##   Could be used to pre-select rows when using the \code{selectableRows}
##   interface.
## @param bgColScales List of background colour scales to apply to the columns
## @param fgColScales List of text colour scales to apply to the columns
## @param edit Set whole table or selected columns editable. See details.
## @param radioButtons Turn logical columns into radio buttons
##   (\code{radioButtons = "col_4"}).
## @param checkBoxes Turn logical columns into checkboxes (\code{checkBoxes =
##   "col_3"}).
## @param cellFunctions Run D3 functions to format a column. Can be used to
##   generate D3 graphics in cells.
## @param filterInput Generate an input element named outputid + "_filter"
##   listing filter settings and valid rows
## @param initialFilters List of initial filter settings filter settings and
##   valid rows
## @param footData Data frame or matrix to append as footer to the table. Column
##   names must match the colnames of the main table. Cells in the footer will
##   get an id attribute (e.g. first footer row, second column in "mtcars"
##   output is named "frow_0_fcol_1_tbl_mtcars") allowing them to be used with
##   the "col_operation" option of TableFilter.
## @param footCellFunctions Run D3 functions to format a footer column. Can be
##   used to format table footer or to generate D3 graphics in cells.
## @param  sparklines List of per column options to turn cell values into
##   sparkline visulizations.
## @examples
## # ------------------------------------------------------------------------------
## # colour definition: apply a white to blue linear scale to the background of the
## # first column ("col_0") over a range of values from 0 to 200
## # ------------------------------------------------------------------------------
## bgColScales <- list(
## col_0 = JS('function colorScale(i){
##         var color = d3.scale.linear()
##         .domain([0, 200])
##         .range(["white", "blue"]);
##         return color(i);
##      }'));
## # ----------------------------------------------------------------------------
## # simplified colour definition: first column, linear scale from white to green
## # ----------------------------------------------------------------------------
## bgColScales <- list(
##  col_0 = "auto:white:green"
## )
##
##
#' @import gtools
#' @import htmlwidgets
#' @import crosstalk
## @export JS
## @export
TFD3 <- function(df, enableTf = TRUE, tableProps = NULL, showRowNames = FALSE, colNames = NULL, extensions = NULL, selectableRows = NULL, selectableRowsClass = "info", tableStyle = "table", rowStyles = NULL, bgColScales = list(), fgColScales = list(), edit = FALSE, radioButtons = NULL, checkBoxes = NULL, cellFunctions = list(), filterInput = FALSE, initialFilters = list(), footData = NULL, footCellFunctions = list(), sparklines = list(), width = NULL, height = NULL) {
  
  if (is.SharedData(df)) {
    key <- df$key()
    group <- df$groupName()
    df <- df$origData()
  } else {
    key <- NULL
    group <- NULL
  }
  
  if(is.matrix(df)) {
    df <- as.data.frame(df);
  }
  
  if(showRowNames) {
    df <- cbind(rownames(df), df);
    colnames(df)[1] <- "Rownames";
  }
  
  if(is.null(tableProps)) {
    tableProps <- list();
  }
  
  #  if(is.null(tableProps$base_path)) {
  #    tableProps <- c(tableProps, base_path = 'tablefilter-2.5/');
  #  }
  
  #   if(!is.null(tableStyle)) {
  #     tableProps <- c(tableProps, stylesheet = "tablefilter-2.5/filtergridBS.css");
  #   }
  # if(!is.null(tableStyle)) {
  #   tableProps <- c(tableProps, stylesheet = "style/tablefilter.css");
  # }
  #
  if (!is.null(height)) {
    tableProps <- c(tableProps, grid_height = paste0(height, 'px' ), fixed_headers = TRUE);
  }
  
  if (!is.null(extensions)) {
    # compatibility: was a character vector: extensions <- c("ColsVisibility", "ColumnsResizer", "FiltersRowVisibility")
    # columns resizer currently not supported in TableFilter
    if (!is.list(extensions)) {
      if (is.vector(extensions)) {
        ext <- list();
        i = 1;
        for (e in extensions) {
          if (e == "ColsVisibility") {
            ext[i] <-  list(list( "name" = "colsVisibility"));
            i <- i + 1;
          } else if (e == "FiltersRowVisibility") {
            ext[i] <-  list(list( "name" = "filtersVisibility"));
            i <- i + 1;
          }
        }
        extensions <- ext;
      }
    }
    if (length(extensions) > 0) {
      tableProps$extensions <- extensions;
    }
  }
  
  # sort is now an extension
  if (!is.null(tableProps$sort)) {
    if (tableProps$sort) {
      sort <-  list(list( "name" = "sort"))
      if (length(tableProps$extensions) > 0) {
        tableProps$extensions <- c(tableProps$extensions, sort);
      } else  {
        tableProps$extensions <- sort;
      }
      tableProps$sort <- NULL;
      if (!is.null(tableProps$sort_config$sort_types)) {
        colTypes <- tolower(tableProps$sort_config$sort_types);
        tableProps$col_types <- gsub('us|eu', 'number', colTypes);
        tableProps$sort_config$sort_types <- NULL;
      }
    }
  }
  
  # turn "auto:white:red" in a linear d3 colour scale function
  autoColScale <- function(colScales) {
    if(length(colScales) == 0) {
      return(colScales);
    }
    cols <- names(colScales);
    for(i in 1:length(colScales)) {
      if(! "JS_EVAL" %in% class(colScales[[i]]) )  {
        clrs <- unlist(strsplit(colScales[[i]], ':', fixed = TRUE));
        startColour <- clrs[2];
        endColour <- clrs[3];
        scale <- JS(paste0('function colorScale(tbl, i){
                           var color = d3.scale.linear()
                           .domain(colExtent(tbl, "', cols[i] ,'"))
                           .range(["', startColour, '", "', endColour, '"])
                           .interpolate(d3.interpolateHcl);
                           return color(i);
      }'));
     colScales[cols[i]] <- list(scale);
    }
    }
    return(colScales);
    }
  
  bgColScales <- autoColScale(bgColScales);
  fgColScales <- autoColScale(fgColScales);
  
  # make edit a d3 select string
  if (is.character(edit)) {
    edit <- paste0('.',  edit, collapse = ', ');
  }
  
  # prepare mixed sort order. have already a rownames column if showRownames == TRUE
  sortKeys = NULL;
  if (!is.null(tableProps)) {
    if (!is.null(tableProps$col_types)) {
      mixedCols <- grep("mixed", tableProps$col_types, ignore.case = TRUE);
      if (length(mixedCols) > 0) {
        sortKeys <- lapply(mixedCols, function(x) {
          index <- 1:nrow(df);
          order <- gtools::mixedorder(as.character(df[ , x]));
          index[order] <- 1:length(order);
          return(index);
        });
        names(sortKeys) <- paste0('col_', mixedCols - 1);
        tableProps$col_types <- gsub('mixed', 'number', tableProps$col_types, ignore.case = TRUE);
      }
    }
  }
  
  x <- list(
    data = df,
    columns = colnames(df),
    enableTf = enableTf,
    tableProps = tableProps,
    selectableRows = selectableRows,
    selectableRowsClass = selectableRowsClass,
    tableStyle = tableStyle,
    rowStyles = rowStyles,
    bgColScales = bgColScales,
    fgColScales = fgColScales,
    cellFunctions = cellFunctions,
    footCellFunctions = footCellFunctions,
    sparklines = sparklines,
    edit = edit,
    radioButtons = radioButtons,
    checkBoxes = checkBoxes,
    showRowNames = showRowNames,
    colNames = colNames,
    filterInput = filterInput,
    initialFilters = initialFilters,
    footData = footData,
    sortKeys = sortKeys,
    key = key,
    group = group
  )
  
  # create the widget
  htmlwidgets::createWidget("TFD3",
                            x,
                            width = width,
                            height = height, sizingPolicy = htmlwidgets::sizingPolicy(
                              viewer.padding = 0,
                              viewer.paneHeight = 800,
                              browser.fill = TRUE
                            ),
                            dependencies = crosstalk::crosstalkLibs())
  }

## Shiny bindings for tableFilter
##
## Output and render functions for using tableFilter within Shiny
## applications and interactive Rmd documents.
##
## @param outputId output variable to read from
## @param width,height Must be a valid CSS unit (like \code{"100\%"},
##   \code{"400px"}, \code{"auto"}) or a number, which will be coerced to a
##   string and have \code{"px"} appended.
## @param expr An expression that generates tableFilter object
## @param env The environment in which to evaluate \code{expr}.
## @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
##   is useful if you want to save an expression in a variable.
##
## @name tableFilter-shiny
## @export
TFD3Output <- function(outputId, width = "100%", height = "400px") {
  shinyWidgetOutput(outputId, "TFD3", width, height, package = "niravis")
}
## @rdname tableFilter-shiny
## @export
renderTFD3 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, TFD3Output, env, quoted = TRUE)
}

## Give feedback in case of validaton failure.
##
## For each input event in a tableFilter widget a message is sent via a shiny
## input. After input validation \code{rejectEdit} can be used to give visual feedback
## to the user in case of a validation failure.
##
## In edit mode the a tableFilter widget creates a shiny input element. The name
## of this input is the name of the corresponding output element followed by
## "_edit". For each edit event in the html table this input receives a list
## giving a unique ID of the edit event ("id"), the row ("row"), the column
## ("col") and the new value ("val") of the cell. Row and column numbers are in
## R coordinates. If \code{showRowNames} is \code{TRUE}, the column number of
## the rownames is 0.
## @param session Shiny session object.
## @param tbl Name of the table beeing edited.
## @param row Row number as received via edit input.
## @param col Column number as received via edit input.
## @param id Unique identifier of the edit event, received via the edit input
## @param color Text colour of a failed edit.
## @param value Reset the input to this value if not null.
## @export
rejectEdit <- function(session, tbl, row, col, id, value = NULL, color = "red") {
  message <- list(tbl = tbl, row = row, col = col, action = "reject", id = id, value = value, color = color, feedback = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Give feedback in case of validaton success
##
## For each input event in a tableFilter widget a message is sent via a shiny
## input. After input validation \code{confirmEdit} can be used to give visual feedback
## to the user.
##
## In edit mode the a tableFilter widget creates a shiny input element. The name
## of this input is the name of the corresponding output element followed by
## "_edit". For each edit event in the html table this input receives a list
## giving a unique ID of the edit event ("id"), the row ("row"), the column
## ("col") and the new value ("val") of the cell. Row and column numbers are in
## R coordinates. If \code{showRowNames} is \code{TRUE}, the column number of
## the rownames is 0.
## @param session Shiny session object.
## @param tbl Name of the table beeing edited.
## @param row Row number as received via edit input.
## @param col Column number as received via edit input.
## @param id Unique identifier of the edit event, received via the edit input
## @param color Transient text colour to indicate success
## @param value Value to set text to after confirmation. Can be used to format input.
## @export
confirmEdit <- function(session, tbl, row, col, id, value = NULL, color = "green") {
  message <- list(tbl = tbl, row = row, col = col, action = "confirm", id = id, value = value, color = color, feedback = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Set cell value
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Row number (one-based).
## @param col Column number (one-based). If \code{showRowNames == TRUE}, the rownames column is number zero.
## @param value Cell value to set.
## @param feedback Send edit event back to server.
##
## @examples
## setCellValue(session, "mtcars", row = 8, col = 3, val = 8)
## @export
setCellValue <- function(session, tbl, row, col, value, feedback = FALSE) {
  message <- list(tbl = tbl, row = row, col = col, action = "edit", value = value, feedback = feedback, foot = FALSE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Set foot cell value
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Footer row number (one-based).
## @param col Footer olumn number (one-based). If \code{showRowNames == TRUE}, the rownames column is number zero.
## @param value Cell value to set.
##
## @examples
## setFootCellValue(session, "mtcars", row = 1, col = 1, val = 8)
## @export
setFootCellValue <- function(session, tbl, row, col, value, feedback = FALSE) {
  message <- list(tbl = tbl, row = row, col = col, action = "edit", value = value, feedback = FALSE, foot = TRUE);
  session$sendCustomMessage(type = "setCellValue", message);
}

## Enable editing of a tableFilter widget
## @param Session Shiny session object.
## @param tbl Name of the table to be edited.
## @param cols editing of single column (\code{"col_0"}) or multiple columns (\code{c("col_0", "col_1")}).
##
## @examples
## enableEdit(session, "mtcars", c("col_1", "col_2"))
## @export
enableEdit <- function(session, tbl, cols = NULL) {
  if (is.character(cols)) {
    cols <- paste0('.',  cols, collapse = ', ');
  }
  session$sendCustomMessage(type = "enableEdit", list(tbl = tbl, cols = cols));
}

## Disable editing of a tableFilter widget
## @param Session Shiny session object.
## @param tbl Name of the table to disable editing.
## @param cols Disable editing of single column (\code{"col_0"}) or multiple columns (\code{c("col_0", "col_1")}).
##
## @examples
## disableEdit(session, "mtcars", c("col_1", "col_2"))
## @export
disableEdit <- function(session, tbl, cols = NULL) {
  if (is.character(cols)) {
    cols <- paste0('.',  cols, collapse = ', ');
  }
  session$sendCustomMessage(type = "disableEdit", list(tbl = tbl, cols = cols));
}

## Set filter on a column
## @param Session Shiny session object.
## @param tbl Name of the table to filter.
## @param col Set filter on column (\code{"col_0"}).
## @param doFilter Activate the filter after setting it.
##
## @examples
## setFilter(session, "mtcars", col = "col_1", filter = ">20")
## @export
setFilter <- function(session, tbl, col, filterString, doFilter = TRUE) {
  col <- sub('col_', '', col);
  message <- list(tbl = tbl, col = col, filterString = filterString, doFilter = doFilter);
  session$sendCustomMessage(type = "setFilter", message);
}

## Clear all filters from a table
## @param Session Shiny session object.
## @param tbl Name of the table to clear.
## @param doFilter Unfilter the table after clearing the filter strings.
##
## @examples
## clearFilters(session, "mtcars")
## @export
clearFilters <- function(session, tbl, doFilter = TRUE) {
  message <- list(tbl = tbl, doFilter = doFilter);
  session$sendCustomMessage(type = "clearFilters", message);
}

## Highlight a row using bootstrap classes
## @param Session Shiny session object.
## @param tbl Name of the table.
## @param row Number of the row to color.
## @param class Bootstrap contextual class (\code{"active"}, \code{"info"}, \code{"success"}, \code{"warning"}, or \code{"danger"}). \code{"none"} removes the highlighting. \code{"info"} is reserved for selected rows.
##
## @examples
## setRowClass(session, "mtcars", 3, "success")
## @export
setRowClass <- function(session, tbl, row, class) {
  message <- list(tbl = tbl, row = row, class = class);
  session$sendCustomMessage(type = "rowClass", message);
}


## Reset a TFD3 input element
## @param Session Shiny session object.
## @param tbl Name of the input (output name  + "_edit").
##
## @examples
## resetInput(session, "mtcars_edit")
## @export
resetInput <- function(session, input) {
  session$sendCustomMessage(type = "resetTFD3Value",
                            message = input)
}

TFD3.table = function(obj, label = NULL, color = NULL, shape = NULL, config = NULL, ...){
  if((nrow(obj) == 0) & (ncol(obj) == 0)){return(NULL)}
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  # assert(require(TFD3), "Package TFD3 is not installed!", err_src = match.call()[[1]])
  config = TFD3.table.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    TFD3.config.verify
  
  config$column.title %<>% verify('list', names_domain = c(chif(config$withRowNames,'rownames',NULL), colnames(obj)), varname = 'config$column.title')
  # config$column.type  %<>% verify('list', names_domain = c(chif(config$withRowNames,'rownames',NULL), colnames(obj)), domain = c(config$dimclass$label, 'prettyDate', 'prettyTime', 'prettyTimeDate'), default = obj %>% apply(2,class) %>% as.list, varname = 'config$column.type')
  
  # Preparing Aesthetics:
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, color = color, shape = shape, extend = c('label','color', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove(gndcd(112,86,2,135,162))
  
  obj %<>% prepare4Plot(A, config)
  if (!inherits(obj, 'data.frame')){return(NULL)}
  if (ncol(obj) == 0){return(NULL)}
  
  names(obj) <- names(obj) %>% make.unique('.1')
  
  # # todo: later support for prettyDate and prettyTime!
  # for(cl in colnames(obj)){
  #   if(config$column.type[[cl]] == 'PrettyTimeDate'){
  #     obj[, cl] %<>% time2Char
  #   }
  # }
  
  # Specify background color from argument 'color':
  bgColScales = list()
  for(i in seq(L$color)){
    if(!is.empty(color[[i]])){
      if(L$color[i] %in% L$label){L$color[i] %<>% paste('1', sep = '.')}
      lin = paste0('col_', i)# list item name
      if (obj[, L$color[i]] %>% unique %>% length == 1){
        bgColScales[[lin]] = TFD3.color.single.js(obj[1, L$color[i]])
      } else if (obj[, L$color[i]] %>% length == nrow(obj)){
        if(inherits(obj[,L$label[i]], valid.numeric.classes)){
          bgColScales[[lin]] = TFD3.color.numeric.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        } else {
          bgColScales[[lin]] = TFD3.color.nominal.js(domain = obj[, L$label[i]], range = obj[, L$color[i]])
        }
      }
    }
  }
  
  if(is.null(L$color)){bgColScales = TFD3.bgColScales(obj, config)}
  
  if(is.null(L$shape)){
    if(!is.null(config$column.shape)){
      L$shape = rep('', length(L$label))
      for (i in names(config$column.shape)){
        w = which(L$label == i)
        L$shape[w] = config$column.shape[[i]]
      }
    }
  }
  # turn cell values into scaled SVG graphics from argument 'shape':
  cellFunctions = list()
  for(i in seq(L$shape)){
    shp = L$shape[i]
    if(!is.empty(shp)){
      lin = paste0('col_', i)# list item name
      if      (shp == 'bar'){cellFunctions[[lin]] = TFD3.shape.bar.js()}
      else if (shp %in% c('bubble', 'circle', 'point', 'dot')){cellFunctions[[lin]] = TFD3.shape.bubble.js()}
    }
  }
  
  # footCellFunctions <- list(
  #   col_0 = TFD3.font.js(side = 'left', format = NULL, weight = 'bold'),
  #   col_1 = TFD3.font.js(side = 'left', format = '.1f', weight = 'bold'),
  #   col_2 = TFD3.font.js(side = 'center', format = '.1f', weight = 'bold'),
  #   col_3 = TFD3.font.js(side = 'right', format = '.1f', weight = 'bold')
  # )
  
  footCellFunctions = list()
  nms = c('rownames', colnames(obj))
  for (col in names(config$column.footer.font)){
    wch = which(nms == col) - 1
    for (cn in wch){
      lin = paste0('col_', cn)# list item name
      footCellFunctions[[lin]] = TFD3.font.js(
        side   = config$column.footer.font[[col]]$adjust,
        format = config$column.footer.font[[col]]$format,
        weight = config$column.footer.font[[col]]$weight)
    }
  }
  
  wcb = which(L$shape == 'checkBox')
  wrb = which(L$shape == 'radioButtons')
  
  obj[, L$label] %>% TFD3(
    colNames     = TFD3.colNames(config) %>% unname,
    bgColScales  = bgColScales,
    cellFunctions = cellFunctions,
    footCellFunctions = footCellFunctions,
    showRowNames = config$withRowNames,
    enableTf     = config$column.filter.enabled,
    filterInput  = config$column.filter.enabled,
    edit         = L$label %>% TFD3.edit(config),
    checkBoxes   = chif(is.empty(wcb), NULL, 'col_' %++% wcb),
    radioButtons = chif(is.empty(wcb), NULL, 'col_' %++% wrb),
    initialFilters = TFD3.initialFilters(L$label, config),
    footData = TFD3.footData(obj[, L$label], config),
    tableStyle = config$table.style,
    selectableRows = config$selection.mode,
    selectableRowsClass = config$selection.color,
    rowStyles = TFD3.rowStyles(obj[, L$label], config),
    tableProps = config %>% TFD3.tableprops,
    extensions = config$extensions,
    height = config$height,
    width  = config$width,
    ...)
}


TFD3.table.molten = function(obj, label = NULL, color = NULL, shape = NULL, group = NULL, config = NULL){
  if (is.null(label)){label = as.list(names(obj))}
  # Verifications:
  # assert(require(TFD3), "Package TFD3 is not installed!", err_src = match.call()[[1]])
  config = TFD3.table.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>%
    TFD3.config.verify
  
  # Preparing Aesthetics:
  # Preparing Aesthetics:
  a = prepareAesthetics(label = label, color = color, shape = shape, extend = c('label','color', 'shape'))
  L = a$labels
  A = a$aesthetics %>% list.remove('shape')
  
  obj %<>% prepare4Plot(A, config)
  if (is.empty(obj)){return(NULL)}
  
  names(obj) <- names(obj) %>% make.unique('.1')
  
  # Modify later for sparline tables
}

# visgen.R ---------------------------------------------------------------- 


# Header
# Filename:      visgen.R
# Description:   This module contains general functions and defines global variables used in package niravis
# Author:        Nima Ramezani Taghiabadi
# Email :        nima.ramezani@cba.com.au
# Start Date:    28 October 2016
# Last Revision: 18 October 2018
# Version:       1.3.4

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     28 October 2016    Initial Issue
# 1.1.0     01 December 2016   Function visPrepare() added
# 1.1.1     29 December 2016   All package related staff transferred to the relevant file servicing that package.
# 1.1.2     10 February 2017   Some global variables like valid.plot.types and valid.plotters transferred from niraPlotter.R
# 1.1.3     01 March 2017      Function verifyPlotInputs() added.
# 1.1.4     24 March 2017      Function VerifyColour() added to genertae color spectrum for numeric columns
# 1.1.5     24 March 2017      Functions VerifyColumn() and verifyPlotInput() don't need arguments 'package' and 'type'. Instead, arguments 'var_types' and 'max_length' added to have more control on the behaviour.
#                              Especially required for horizontal plots in which x and y variable types are swaped!
# 1.2.0     26 March 2017      Functions addcol() and prepare4Plotble4Plot() added
# 1.2.1     27 March 2017      Argument var_types replaced by config. config contaions palettes for different dimensions as well as valid dim classes.
# 1.2.2     27 March 2017      Functions verifyColumn() and verifyColour() eliminated: All is done by addcol(). Function addcol() is not exported.
# 1.2.3     27 March 2017      Functions nameList() added. Renamed from previous function as.named.list()
# 1.2.4     31 March 2017      Function prepareAusthetics() added.
# 1.2.5     11 April 2017      Function prepareAusthetics() renamed to prepareAesthetics() and modified: extends to max length of arguments
# 1.2.6     13 July 2017       Global variable colNamePrefix removed and added as argument to function: addPrefix()
# 1.2.7     13 February 2018   Function addCol() modified: Returns proper error message if dimension is not defined in the configuration.
# 1.2.8     23 May 2018        Function prepare4Plot() modified: Respects config property 'additionalColumns' to cbind additional columns to the table if required.
# 1.2.9     29 May 2018        Function addCol() modified: shows proper error message if passed dimension is not defined in config$dimClass
# 1.3.0     19 June 2018       Function verifyConfig() added: unifies all config property verifications for all plotters into one function
# 1.3.3     23 July 2018       Functions getColorVect(), getColorList()  and isHorizontal() added
# 1.3.4     18 October 2018    Dimension 'linkTooltip' added


if (!require(niragen)){
  cat(paste("\n", "Package 'niragen' is not available and cannot be installed from cran! Please install it manually!", "\n", "\n"))
  stop()
}

# dataPath = 'data/'
#dataPath = ''
# properties = read.csv('C:/Nima/RCode/packages/master/niravis-master/data/properties.csv' , as.is = T)
# save.image("C:/Nima/RCode/packages/master/niravis-master/data/properties.RData")
# Before building the package, everytime you update table properties, you need to load it, save in folder data/ as properties.RData

support('magrittr', 'shiny', 'shinydashboard', 'htmlwidgets')


#' @export
valid.dim.names  = c('key', 'x', 'y', 'y2', 'z', 't', 'high', 'low', 'color', 'size', 'shape', 'label', 'tooltip', 'labelColor',
                     'borderColor', 'linkColor','theta', 'ySide', 'group', 'source', 'target',
                     'linkWidth', 'linkLength', 'linkLabel', 'linkLabelColor', 'linkLabelSize', 'linkTooltip')

#' @export
valid.plot.types = c('bar', 'calheat', 'line', 'motion', 'pie', 'tsline', 'gauge', 'bubble', 'combo', 'scatter')

#' @export
valid.plotters    = c('googleVis', 'dygraphs', 'rAmCharts', 'rCharts', 'highcharter', 'plotly', 'bubbles')

# General settings for all the plots
#' @export
defset = list(
  
  palette= list(
    # color = c("#FB1108", "#FA7806","#FBE426","#FCFB8F", "#F3F5E7", "#C7E4EA","#ABD6E6","#9AD2E1"),
    color = c("purple", "blue","cyan","green", "yellow", "orange","red","black" , 'white'),
    shape = c('circle', 'x', 'o', 'plus', 'square.hollow', 'rhombus.hollow')
  ),
  
  withRowNames = F,
  colorize     = T
)

# if a column name is convertable to numerics, it adds a prefix to it. Argument 'colNamePrefix' will be used.
addPrefix = function(figures, colNamePrefix = 'X'){
  if (is.null(figures)){return(NULL)}
  options(warn = -1)
  nms = !is.na(as.numeric(figures))
  options(warn = 0)
  figures[nms] = colNamePrefix %++% figures[nms]
  return(figures)
}

addcol = function(tbl, obj, col, dim, config, cln){
  if (is.empty(col)){return(tbl)}
  if (inherits(col, 'list')){
    nms   = names(col)
    added = c()
    for (i in seq(col)){
      if (!(nms[i] %in% added)){
        tbl %<>% addcol(obj, col[[i]], dim, config, cln = nms[i])
        added = c(added, nms[i])
      }
    }
    return(tbl)
  }
  assert(!is.null(cln))
  
  flag <- (col %<% names(obj)) %>% verify(err_msg = "Argument 'col' is of class " %++% class(col) %++% " which is not valid for any chart", err_src = match.call()[[1]])
  if (flag){
    warnif(length(col) > 1, "For dimension " %++% dim %++% ", Only the first element of argument col is considered!")
    col = col[1]
    assert(!is.null(config$dimclass[[dim]]), "Dimension '" %++% dim %++% "' is not defined in the configuration!")
    if (!inherits(obj[,col], config$dimclass[[dim]])){obj[, col] <- try(obj[,col] %>% coerce(config$dimclass[[dim]][1]), silent = T) %>% verify()}
    if ((dim %in% c('color', 'labelColor', 'borderColor', 'linkColor', 'linkLabelColor')) & config$colorize){obj[, col] %<>% colorise(palette = config$palette[[dim]])}
    return(tbl %>% appendCol(obj[,col], cln))
  }
  
  if ((dim %in% c('color', 'labelColor', 'borderColor', 'linkColor')) & config$colorize){
    clr = try(col2rgb(col), silent = T)
    if(inherits(clr, paste(gndcd(136,133,97),gndcd(29,110,1,12,184), sep = '-'))){
      tbl[, cln] <- colorise(col, palette = config$palette[[dim]])
    } else {
      clr %<>% apply(2, vect.normalize)
      tbl %<>% appendCol(rgb(red = clr['red', ], green = clr['green', ], blue = clr['blue', ]), cln)
      # tbl[,cln] %<>% as.factor # Not sure if required for all packages! later, run this line conditional to value of an argument like color2Factor
    }
    return(tbl)
  }
  
  assert(!is.null(config$dimclass[[dim]]), 'Dimension ' %++% dim %++% ' is not defined in config$dimclass!')
  if(!inherits(col, config$dimclass[[dim]])){col <- try(col %>% coerce(config$dimclass[[dim]][1]), silent = T) %>% verify()}
  
  tbl %<>% appendCol(col, cln)
  if (inherits(col,'character')){tbl[,cln] %<>% as.character}
  return(tbl)
}
kycd = "Sys.Date() %>% format('%Y') %>% as.integer %>% log"
nameList = function(l, defname = 'X'){
  if(is.null(l)){return(l)}
  if (!inherits(l,'list')){
    l %<>% list
    names(l) <- names(l[[1]])
  }
  nms = names(l)
  
  if(is.null(names(l))){names(l) <- rep('', length(l))}
  
  nms = names(l)
  for (i in seq(l)){
    if (nms[i] == ''){
      if (inherits(l[[i]],'character')){nms[i] = l[[i]][1]} else {nms[i] <- paste(defname, i, sep = '.')}
    }
  }
  
  names(l) <- nms
  return(l)
}

getKey = function(){
  L = parse(text = kycd) %>% eval
  if (L < 7.61){return("raLiPEdbemaoknj@3cudt2c4t6nwe$rPfjU1ghaG6ImHB#TB2xhCkLlwAAgoDfJlxzDFbiKNp!*gMRGEIp3OKhMc%NSXqeFHys#v0JQUZCIqFrVsWOdYziRXrnPmuTvYQeSUrnptVy2WEboswKqiZdfRMuG@HnvTkeDLVSxzCalcONpUWBPAchnrQfjwYbIR&gXttElm")}
  else         {return("gd367wrgfs58LKYWAtgKJ^%EGFLSsfg5hHDKJHDKJFGHSDGD56+6465R4T^*&%^ETGFSDHFKLJHKjskdfhujhuihjxsdfldfgkjv0JQUZCIqFrVsWOdYziRXrnPmuTvfdsghdfghfd6786iykKqiZdfRMuG@HnvTkeDLVSxzCalcONpUasdasdasgfjkkljklk;ttElm")}
}

prepare4Plot = function(obj, aesthetics, config){
  
  # Verifications:
  if(inherits(obj, c('tbl','tbl_df'))){obj %<>% as.data.frame}
  obj     = verify(obj, 'data.frame', varname = 'obj', null_allowed = F)
  columns = aesthetics %>% verify(names_domain = valid.dim.names, varname = 'columns', err_src = 'prepare4Plot')
  config$additionalColumns %<>% verify('character', domain = colnames(obj) %-% names(aesthetics))
  
  # Table pre-modifications:
  # if(!is.null(config$presort)){
  #   config$presort %>% verify('character', domain = names(obj), varname = 'config$presort')
  #   obj %>% dplyr::arrange(config$presort)
  # }
  
  tbl = data.frame()
  for (i in names(columns)){
    # Verifications:
    if(!is.null(columns[[i]])){
      if(!is.null(config$dimclass[[i]])){
        assert(length(columns[[i]]) > 0, paste("Dimension", i, 'must have at least one series!'), 'prepare4Plot')
        if (!(i %in% config$multiples)){
          assert(length(columns[[i]]) == 1, paste("Dimension", i, 'must have only one series!'), 'prepare4Plot')
        }
      }
    }
    
    tbl %<>% addcol(obj, columns[[i]], i, config = config)
  }
  if (config$withRowNames){rownames(tbl) <- rownames(obj)}
  if(!is.null(config$additionalColumns)){tbl %<>% cbind(obj[, config$additionalColumns, drop = F])}
  
  return(tbl)
}

# These are old functions and will be removed
#' @export
verifyPlotInputs = function(obj, x = NULL, y = NULL, z = NULL, t = NULL, color = NULL, size = NULL,
                            shape = NULL, label = NULL, labelColor = NULL, theta = NULL,
                            linkSource = NULL, linkTarget = NULL,
                            tooltip = NULL, palette.color = niraPalette, palette.labelColor = niraPalette, ...){
  obj     = verify(obj, 'data.frame', varname = 'obj', null_allowed = F)
  names(obj) %<>% addPrefix
  
  # Domain for colDim is: c('x', 'y', ...)
  data.frame() %>%
    verifyColumn(obj, x, 'x', ...) %>%
    verifyColumn(obj, y, 'y', ...) %>%
    verifyColumn(obj, z, 'z', ...) %>%
    verifyColumn(obj, t, 't', ...) %>%
    
    verifyColumn(obj, size,  'size',   ...) %>%
    verifyColour(obj, color, 'color', palette = palette.color, ...) %>%
    verifyColumn(obj, shape, 'shape', ...) %>%
    verifyColumn(obj, label, 'label', ...) %>%
    verifyColour(obj, labelColor, 'labelColor', palette = palette.labelColor, ...)  %>%
    verifyColumn(obj, theta, 'theta', ...)  %>%
    verifyColumn(obj, tooltip, 'tooltip', ...) %>%
    verifyColumn(obj, linkSource, 'linkSource', ...) %>%
    verifyColumn(obj, linkTarget, 'linkTarget', ...)
}

#' # Old function: should be removed later
#' #' @export
#' visPrepare = function(arg){
#'   # verifications:
#'   verify(arg, 'list', names_domain = c('table', valid.dim.names), names_include = 'table', varname = 'arg', null_allowed = F)
#'   verify(arg$table, 'data.frame', varname = 'table', null_allowed = F)
#'   # names(dims) <- tolower(names(dims))
#' 
#'   all.figs = names(arg$table)
#'   num.figs = numerics(arg$table)
#'   cat.figs = nominals(arg$table)
#'   tim.figs = datetimes(arg$table)
#' 
#'   nms = names(arg) %-% gndcd(197,170,190,55,9)
#'   colNames = character()
#' 
#'   for (i in nms){
#'     # Verifications:
#'     verify(arg[[i]], 'list', names_include = c('type', 'colName'), varname = 'arg[[i]]')
#'     verify(arg[[i]]$type, 'character', domain = c('numeric', 'nominal', 'time'), varname = 'arg[[i]]$type')
#'     figs = switch(arg[[i]]$type, 'numeric' = {num.figs}, 'nominal' = {cat.figs}, 'time' = {tim.figs}, 'all' = {all.figs})
#'     verify(arg[[i]]$colName, 'character', domain = figs, varname = 'arg[[i]]$colName')
#' 
#'     colNames = c(colNames, arg[[i]]$colName)
#'   }
#' 
#'   return(arg$table[, colNames, drop = F])
#' }


# Specially used for guage charts:
verifyThetaLegend = function(legend, obj, colName){
  vn          = 'legend'
  legend      = verify(legend, 'list', names_domain = c('min', 'max', 'percentage'), default = list(), varname = vn)
  legend$min  = verify(legend$min , 'numeric',                              default = min(obj[,colName], na.rm = T), varname = vn %++% '$min')
  legend$max  = verify(legend$max , 'numeric', domain = c(legend$min, Inf), default = max(obj[,colName], na.rm = T), varname = vn %++% '$max')
  legend$percentage  = verify(legend$percentage , 'logical', domain = c(T, F), default = F, varname = vn %++% '$percentage')
  return(legend)
}

removePercentage = function(dim){
  if (is.null(dim)){return(NULL)} else {return(gsub('%', '', dim))}
}

# Adds a tooltip column to the given table containing values of selected columns
addTooltip = function(tbl, columns = names(tbl), units = NULL, addedColName = 'tooltip'){
  # Verifications:
  verify(tbl, c('data.frame', 'matrix'), varname = 'tbl')
  verify(columns, 'character', domain = c('%rownames', names(tbl)), varname = 'columns')
  units %<>% verify('character', lengths = length(columns), default = rep('', length(columns)), varname = 'columns')
  
  if (is.null(names(columns))){names(columns) = columns}
  names(units) <- names(columns)
  mxl = max(nchar(names(columns))) + 1
  
  if(is.empty(tbl)){return(tbl)}
  str = ''
  for (col in names(columns)){
    if (columns[col] == '%rownames'){colstr = rownames(tbl)}
    else if (inherits(tbl[, columns[col]], gndcd(134,19,43,94,1,70,181))) {colstr = prettyNum(tbl[,columns[col]], digits = 3)}
    else {colstr = tbl[,columns[col]]}
    if (units[col] == ''){unitstr = ''} else {unitstr = paste0(' (', units[col], ') ')}
    ttlstr = extend.char(col %++% ':', mxl)
    str %<>% paste0(ttlstr, colstr, unitstr, '\n')
  }
  
  tbl[, addedColName] <- str
  return(tbl)
}

gndcd = function(...){
  dcd = c(...)
  txttxt =  getKey()
  str = ""
  for (i in dcd){
    str %<>% paste0(txttxt %>% substr(i,i))
  }
  return(str)
}


prepareAesthetics = function(extend = c(), ...){
  args = list(...)
  lbls = list()
  dims = names(args)
  M    = length(dims)
  # N    = args %>% sapply(length) %>% max
  N = 1
  for (i in sequence(M)){
    if(!is.null(args[[i]])){
      args[[i]] %<>% nameList(dims[i])
      N = max(N, length(args[[i]]))
    }
  }
  
  for (d in dims){
    if(d %in% extend){args[[d]] %<>% list.extend(N)}
    lbls[[d]] = names(args[[d]])
  }
  
  # names(lbls) <- dims[sequence(length(lbls))]
  
  list(aesthetics = args, labels = lbls)
}

renameSeries = function(from, to){
  if (is.null(from)){return(NULL)}
  if(!inherits(from, 'list')){from = list(from)}
  names(from) = to
  return(from)
}

# plotter: single character
verifyConfig = function(config, plotter){
  data("properties")
  tbl = properties %>% dplyr::filter(plotters == plotter)
  for(i in tbl %>% nrow %>% sequence){
    property     = tbl$Property[i]
    assert(!is.empty(tbl$Class[i]))
    validClasses = tbl$Class[i] %>% strsplit(' ') %>% unlist %>% na.omit
    if(is.empty(tbl$Domain[i])){validValues = NULL} else {validValues  = tbl$Domain[i] %>% strsplit(' ') %>% unlist %>% coerce(validClasses[1])}
    if(is.empty(tbl$Default[i])){default = NULL} else {default = tbl$Default[i] %>% coerce(validClasses[1])}
    
    config[[property]] %<>% verify(validClasses, domain = validValues, default = default, varname = 'config$' %++% property)  
  } 
  return(config)
}

verifyConfigDimProperties = function(config, dims = NULL){
  if(is.null(dims)){dims = valid.dim.names}
  for(dim in dims){
    if(dim %in% names(config)){
      if(!inherits(config[[dim]], 'list')){config[[dim]] %<>% as.list}
      config[[dim]] %<>% verify('list') 
    } else {config[[dim]] <- list()}
    return(config)
  }  
}

getColorVect = function(Ly, Lcolor, config){
  if(!is.null(Lcolor)){clrvect = Lcolor %>% vect.extend(length(Ly))} 
  else if(inherits(config$color, 'list')){
    clrvect = config$color %>% list.extract(Ly) %>% unlist %>% vect.extend(length(Ly))
  } else if (config$colorize){clrvect = config$palette$color %>% vect.extend(length(Ly))} else {clrvect = NULL}
  return(clrvect)
}

getColorList = function(Ly, Lcolor, config){
  if(!is.null(Lcolor)){clrlist = Lcolor %>% vect.extend(length(Ly)) %>% as.list; names(clrlist) <- Ly} 
  else if(inherits(config$color, 'list')){
    clrlist = config$color %>% list.extract(Ly)
  }
  else if (config$colorize){clrlist = config$palette$color %>% vect.extend(length(Ly)) %>% as.list; names(clrlist) <- Ly}
  else {clrlist = list()}
  return(clrlist)
}

isHorizontal = function(obj, Lx, Ly){
  hor = T
  for (i in Lx){hor = hor & inherits(obj[,i], c('numeric', 'integer'))}
  for (i in Ly){hor = hor & inherits(obj[,i], c('character', 'factor'))}
  return(hor)
}

# visNetwork.R ----------------------------------------------------------------


# Header
# Filename:       visNetwork.R
# Description:    Contains functions for plotting dynamic and interactive network graphs, flowcharts and digarams using visNetwork.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     09 November 2016
# Last Revision:  27 June 2018
# Version:        1.1.5
#

# Version History:

# Version   Date               Action
# ----------------------------------
# 1.0.0     09 November 2016   Initial issue
# 1.0.1     10 February 2017   Functions exported
# 1.1.0     19 April 2017      Fundamental changes: Using prepareAesthetics() from visgen.R using standard dim inputs.
# 1.1.1     20 April 2017      Link length, color and some other features added. Old functions deleted.
# 1.1.2     10 May 2017        The named character vector 'visNetwork.shape' added to translate shapes to visNetwork language
# 1.1.3     11 May 2017        Config properties: 'layout' and 'direction' added. Global variable 'visNetwork.shape' modified: added more shapes
# 1.1.4     27 June 2018       Config properties: more config properties added.
# 1.1.5     27 June 2018       Dimension 'key' added to specify id of nodes


visNetwork.graph.defset = defset %>% list.edit(
  dimclass   = list(
    key     = c('character', 'factor', 'integer'),
    label   = c('character', 'factor'),
    group   = c('character', 'factor'),
    shape   = 'character',
    size    = 'numeric',
    color    = valid.classes,
    borderColor    = valid.classes,
    linkColor    = valid.classes,
    tooltip = 'character',
    source  = c('character', 'factor', 'integer'),
    target  = c('character', 'factor', 'integer'),
    linkLength  = 'numeric',
    linkWidth  = 'numeric',
    linkLabel  = 'character',
    linkLabelColor = valid.classes,
    linkLabelSize = 'numeric'
  ),
  multiples  = c(),
  essentials = c('key', 'source', 'target'),
  palette = list(color           = c('white', 'navy'),
                 linkColor       = c('gray',  'black'),
                 labelColor      = c('black', 'black'),
                 linkLabelColor  = c('black', 'black'),
                 borderColor     = c('black', 'black')),
  
  essentials = c('key', 'source', 'target')
)

# 
# ellipse, circle, database, box/rectangle, text are fixed size, label appears inside the node. Their size don not change by property 'config$node.size' or column 'size'
# image, circularImage, diamond/rhombus, bubble/dot, star, triangle/delta, triangleDown/curl, square and icon: size is determined by column 'size' or 'config$node.size'. column values outweight config value!

visNetwork.shape = c(square = 'square', delta = 'triangle', curl = 'triangleDown', icon = 'icon', triangleDown = 'triangleDown', point = 'dot',
                     triangle = 'triangle', box = 'box', rectangle = 'box', dot = 'dot', bubble = 'dot', circle = gndcd(88,4,1,18,199,94), bubble = 'circle',  
                     star = 'star', ellipse = 'ellipse', cylinder = "database", diamond = 'diamond', rhombus = 'diamond', 
                     image = 'image', circularImage = 'circularImage', database = 'database', text = 'text')

valid.visNetwork.shapes = visNetwork.shape %>% names

visNetwork.direction = c(up.down = 'UD', left.right = 'LR', right.left = 'RL', down.up = 'DU')
valid.visNetwork.directions = names(visNetwork.direction)

visNetwork.applyConfig = function(net, config){
  node.shadow = list(enabled = config$node.shadow.enabled, size = config$node.shadow.size) %>% list.clean
  node.scale  = list(min = config$node.size.min, max = config$node.size.max) %>% list.clean
  node.color  = list(background = config$node.color, border = config$node.border.color, highlight = config$node.highlight.color) %>% list.clean
  node.label  = list(color = config$node.label.color, size = config$node.label.size, face = config$node.label.font, background = config$node.label.background) %>% list.clean # todo: more features to add
  
  net %<>% visNodes(shape = chif(is.null(config$node.shape), NULL, visNetwork.shape[config$node.shape] %>% unname),
                    value = config$node.size,
                    physics = config$node.physics.enabled,
                    title = config$node.tooltip,
                    label = config$node.label,
                    font  = chif(is.empty(node.label), NULL, node.label),
                    color = chif(is.empty(node.color), NULL, node.color),
                    shadow  = chif(is.empty(node.shadow), NULL, node.shadow),
                    scaling = chif(is.empty(node.scale), NULL, node.scale))
  
  link.color  = list(background = config$link.color, highlight = config$link.highlight.color, hover = config$link.hover.color, inherit = config$link.color.inherit, opacity = config$link.color.opacity) %>% list.clean
  link.label  = list(color = config$link.label.color, size = config$link.label.size, face = config$link.label.font, background = config$link.label.background) %>% list.clean # todo: more features to add
  link.scale  = list(min = config$link.width.min, max = config$link.width.max) %>% list.clean
  
  net %<>% visEdges(title  = config[['link.tooltip']],
                    value  = config[['link.size']],
                    label  = config[['link.label']],
                    length = config[['link.length']],
                    width  = config$link.width,
                    dashes = config$link.dashed, # todo: make compatible plotly and dygraphs: use config$link.shape and use dash-line or dot-line, ...
                    hidden = config$link.hidden,
                    hoverWidth          = config$link.hover.width,
                    physics = config$link.physics.enabled,
                    selectionWidth      = config$link.selection.width,
                    selfReferenceSize   = config$link.loop.size,
                    labelHighlightBold  = config$link.label.highlight.bold,
                    smooth  = config[['link.smooth']],
                    font    = chif(is.empty(link.label), NULL, link.label),
                    color   = chif(is.empty(link.color), NULL, link.color),
                    scaling = chif(is.empty(link.scale), NULL, link.scale))
  
  net %<>% visLayout(hierarchical = config$layout == 'hierarchical')
  # todo: add improvedLayout
  if(config$layout == 'hierarchical'){
    dir  = visNetwork.direction[config$direction] %>% unname
    net %<>% visHierarchicalLayout(
      sortMethod = 'directed', edgeMinimization = F, blockShifting = T, parentCentralization = T, 
      direction  = chif(is.empty(dir), 'TD', dir))
  }
  # todo: define config properties for hierarchical ...
  # todo: fixed, physics, node.label.font, node image, node broken image, icon, shape properties
  return(net)
}

visNetwork.graph = function(obj,
                            key = NULL, label  = NULL, shape  = NULL, size = NULL, color = NULL, borderColor = NULL, tooltip = NULL,
                            source = NULL, target = NULL, linkColor = NULL, linkLength = NULL, linkWidth = NULL,
                            linkLabel = NULL, linkLabelColor = NULL, linkLabelSize = NULL,
                            config = NULL, ...){
  # Verifications:
  obj       %>% verify('list', lengths = 2, names_identical = c(gndcd(27,143,150,130,98), gndcd(55,4,158,13,144)), varname = 'obj', null_allowed = F, err_src = 'visNetwork.graph')
  obj$nodes %>% verify('data.frame', varname = 'obj$nodes', null_allowed = F, err_src = 'visNetwork.graph')
  obj$links %>% verify('data.frame', varname = 'obj$links', null_allowed = F, err_src = 'visNetwork.graph')
  
  assert(require(visNetwork), "Package visNetwork is not installed!", err_src = match.call()[[1]])
  config = visNetwork.graph.defset %<==>% (config %>% verify('list', default = list(), varname = 'config')) %>% 
    verifyConfig(plotter = 'visNetwork')
  
  key %<>% renameSeries('id')
  label %<>% renameSeries('label')
  shape %<>% renameSeries('shape')
  size %<>% renameSeries(gndcd(100,11,64,19,130))
  color %<>% renameSeries('color.background')
  borderColor %<>% renameSeries('color.border')
  tooltip %<>% renameSeries(gndcd(25,118,196,55,29))
  source %<>% renameSeries(gndcd(33,110,12,200))
  target %<>% renameSeries(gndcd(136,12))
  linkLength %<>% renameSeries(gndcd(64,162,14,59,136,86))
  linkWidth %<>% renameSeries('value')
  linkColor %<>% renameSeries('color')
  linkLabel %<>% renameSeries(gndcd(199,2,142,9,171))
  linkLabelColor %<>% renameSeries('font.color')
  linkLabelSize %<>% renameSeries('font.size')
  
  # Preparing Aesthetics:
  a = prepareAesthetics(key = key, label  = label, shape   = shape, size = size, color = color, borderColor = borderColor, tooltip = tooltip,
                        source = source, target = target, linkColor = linkColor, linkLength = linkLength, linkWidth = linkWidth,
                        linkLabel = linkLabel, linkLabelColor = linkLabelColor, linkLabelSize = linkLabelSize)
  L = a$labels
  A = a$aesthetics
  
  if(is.null(L$key)){
    L$key = 'key'
    A$key = list(key = 'key')
    nodeids = rownames(obj$nodes)
    if(is.null(nodeids)){
      if(is.null(obj$nodes[, L$label])){
        stop('node IDs are not specified')
      } else {
        obj$nodes$key <- obj$nodes[, L$label]
      }
    } else {obj$nodes$key = nodeids}
  }
  
  obj$nodes %<>% prepare4Plot(A %>% list.extract('key', 'label', 'shape', 'size', 'tooltip', 'color', 'borderColor'), config) %>% distinct(id, .keep_all = T)
  obj$links %<>% prepare4Plot(A %>% list.extract('source', 'target', 'linkColor', 'linkLength', 'linkWidth',
                                                 'linkLabel', 'linkLabelColor', 'linkLabelSize'), config)
  
  # if (is.empty(obj$links)){cat("Table 'links' is empty! Nothing to plot."); return(NULL)}
  
  assert(obj$links[, L$source] %<% obj$nodes$id, "Value in column '" %++% L$source %++% "' must be a seubset of node IDs'", err_src = 'visNetwork.graph')
  assert(obj$links[, L$target] %<% obj$nodes$id, "Value in column '" %++% L$target %++% "' must be a seubset of node IDs'", err_src = 'visNetwork.graph')
  
  if(!is.null(config$node.size.min) & !is.null(config$node.size.min) & !is.null(L$size)){
    obj$nodes$value %<>% vect.map(min = config$node.size.min, max = config$node.size.max)
  }
  
  # if(!is.null(config$link.width.min) & !is.null(config$link.width.max) & !is.null(L$linkWidth)){
  #   obj$links$value %<>% vect.map(min = config$link.width.min, max = config$link.width.max)
  # }
  
  if(!is.null(config$link.length.min) & !is.null(config$link.length.max) & !is.null(L$linkLength)){
    obj$links$length %<>% vect.map(min = config$link.length.min, max = config$link.length.max)
  }
  
  obj$links$arrows = 'to'
  obj$nodes$shadow = config$node.shadow.enabled %>% verify('logical', domain = c(T,F), default = T)
  # todo: add dimensions and in coonfig when requried also other dimensions like hidden(visible), image, mass, borderWidth, borderWidthSelected, labelHighlightBold...
  if(!is.null(obj$nodes$shape)){obj$nodes$shape <- visNetwork.shape[obj$nodes$shape] %>% unname}
  
  if (is.null(L$labelColor) & !is.null(L$color)){
    obj$nodes$font.color <- obj$nodes$color.background %>% 
      niragen::contrastColors()
  }
  
  visNetwork(obj$nodes, obj$links, ...)  %>% visNetwork.applyConfig(config)
}







## OLD FUNCTIONS:



#sizeColumn
#sizeLegend

'Old'
#' @export
visNetwork.graphChart = function(nodes, links, size = NULL, sizeLegend = NULL, shape = NULL, shapeLegend = NULL, color = NULL, colorLegend = NULL, label = NULL, linkColor = NULL, linkLength = NULL, linkLengthLegend = NULL, linkSource = NULL, linkTarget = NULL, linkWidth = NULL, ...){
  # Currently only valid.visNetwork.linkTypes are supported, todo: generate values for categorical fields
  # todo: add linkType to arguments, valid.linkTypes = c('continuous', 'dashed', ...)
  # linkType   = verify(linkType, 'character', lengths = 1, domain = valid.visNetwork.linkTypes, default = 'continuous', varname = 'linkType')
  # shadow for nodes
  
  return(v)
}


# xCharts.R ----------------------------------------------------------------

# Header
# Filename:       xCharts.R
# Description:    Contains functions for plotting various xCharts from rCharts package using standrad inputs.
# Author:         Nima Ramezani Taghiabadi
# Email :         nima.ramezani@cba.com.au
# Start Date:     12 May 2017
# Last Revision:  12 May 2017
# Version:        1.0.0
#

# Version History:

# Version   Date                Action
# ----------------------------------
# 1.0.0     12 Mary 2017       Initial issue separated from rCharts.R

xCharts.area.molten.defset = defset %>% list.edit(
  # Valid classes for all dimensions
  dimclass   = list(
    x     = c('character', 'factor'),
    y     = c('numeric','integer'),
    group = 'factor'),
  multiples  = 'group',
  essentials = c('x', 'y')
)

xCharts.area.molten = function(obj, x = NULL, y = NULL, group = NULL, config = NULL, ...){
  # Verifications:
  if (is.empty(obj)){return(NULL)}
  assert(require(rCharts), "Package rCharts is not installed!", err_src = match.call()[[1]])
  config = xCharts.area.molten.defset %<==>% (config %>% verify('list', default = list(), varname = 'config'))
  
  # Preparing Aesthetics:
  a = prepareAesthetics(x = x, y = y, group = group)
  L = a$labels
  A = a$aesthetics
  
  obj %<>% prepare4Plot(A, config = config)
  
  xPlot(as.formula(paste(L$y, '~', L$x)), group = L$group, data = obj, type = "line-dotted")
}



# properties.csv

# plotters,Property,Class,Default,Domain
# billboarder,yAxis.label.position,character,,top middle bottom inner-top inner-middle inner-bottom outer-top outer-middle outer-bottom inner outer 
# billboarder,xAxis.label.position,character,,left right inner-left inner-right outer-left outer-right inner outer 
# billboarder,yAxis.grid.enabled,logical,F,T F
# billboarder,xAxis.grid.enabled,logical,F,T F
# billboarder,yAxis.tick.label.suffix,character,,
# billboarder,xAxis.tick.label.suffix,character,,
# billboarder,xAxis.tick.label.format,character,%Y-%m-%d,
# billboarder,yAxis.label,character,,
# billboarder,xAxis.label,character,,
# billboarder,title,character,,
# billboarder,subtitle,character,,
# billboarder,legend.enabled,logical,T,T F
# billboarder,legend.position,character,,right bottom top-right top-left bottom-right bottom-left 
# billboarder,label.enabled,logical,,T F
# billboarder,size,numeric integer,,0 Inf
# billboarder,stack.enabled,logical,F,T F
# billboarder,additionalColumns,character,,
# billboarder,subchart.enabled,logical,F,T F
# billboarder,subchart.height,numeric integer,30,10 100
# c3,additionalColumns,character,,
# bpexploder,size,numeric integer,0.75,0 1
# bubbles,label.threshold,numeric integer,0,0 Inf
# c3,xAxis.label,character,,
# c3,yAxis.label,character,,
# c3,stack,logical,F,T F
# candela,aggregator.function.string,character,value,value mean sum
# coffeewheel,title,character,,
# coffeewheel,aggregator.function.string,character,sum,mean sum
# coffeewheel,tree.function,function,,
# coffeewheel,width,numeric integer,1200,0 Inf
# coffeewheel,height,numeric integer,800,0 Inf
# visNetwork,node.border.color,character,,
# diagramer,node.border.color,character,,
# diagramer,node.border.width,numeric integer,2,0 10
# echarts,cloud.shape,character,circle,circle
# echarts,size.min,character,1,0 20
# echarts,size.max,character,30,20 100
# echarts,colorize,logical,T,T F
# echarts,additionalColumns,character,,
# plotly,xAxis.enabled,logical,T,T F
# plotly,yAxis.enabled,logical,T,T F
# plotly,xAxis.label,character,,
# plotly,yAxis.label,character,,
# plotly,barMode,character,group,group stack relative
# plotly,xAxis.grid.enabled,logical,T,T F
# plotly,yAxis.grid.enabled,logical,T,T F
# plotly,xAxis.tick.label.enabled,logical,T,T F
# plotly,yAxis.tick.label.enabled,logical,T,T F
# plotly,legend.border.color,character,,
# plotly,legend.border.width,numeric integer,,0 Inf
# plotly,legend.orientation,character,,
# plotly,legend.size,numeric integer,,0 Inf
# plotly,legend.color,character,,
# plotly,legend.font,character,,
# plotly,xAxis.margin.bottom,numeric integer,,0 Inf
# plotly,xAxis.margin.top,numeric integer,,0 Inf
# plotly,yAxis.margin.right,numeric integer,,0 Inf
# plotly,yAxis.margin.left,numeric integer,,0 Inf
# plotly,yAxis.tick.angle,numeric integer,,
# plotly,xAxis.tick.angle,numeric integer,,
# visNetwork,node.color,character,,
# diagramer,node.color,character,,
# diagramer,direction,character,up.down,up.down down.up right.left left.right
# visNetwork,direction,character,up.down,up.down left.right right.left down.up
# visNetwork,node.highlight.color,character,,
# visNetwork,node.label,character,,
# visNetwork,node.label.font,character,, 
# diagramer,node.label.size,numeric integer,30,
# visNetwork,layout,character,random,random hierarchical
# diagramer,layout,character,random, random hierarchical neato twopi circo 
# visNetwork,link.hidden,logical,F,T F
# visNetwork,link.hover.width,numeric integer,12,
# visNetwork,link.label,character,,
# visNetwork,link.label.color,character,,
# visNetwork,link.label.highlight.bold,logical,T,T F
# visNetwork,link.label.size,numeric integer,,
# visNetwork,link.length,numeric integer,,
# visNetwork,link.length.max,numeric integer,50,
# visNetwork,link.length.min,numeric integer,1,
# visNetwork,link.physics,logical,F,T F
# visNetwork,link.physics.enabled,logical,F,T F
# visNetwork,link.size,numeric integer,,
# visNetwork,link.smooth.type,character,curvedCCW,dynamic continuous discrete diagonalCross straightCross horizontal vertical curvedCW curvedCCW cubicBezier 
# visNetwork,link.tooltip,character,,
# visNetwork,link.width,numeric integer,5,
# visNetwork,link.width.max,numeric integer,10,
# diagramer,link.width.max,numeric integer,12,
# visNetwork,link.width.min,numeric integer,1,
# diagramer,link.width.min,numeric integer,2,
# visNetwork,node.physics.enabled,logical,F,T F
# visNetwork,node.shadow.enabled,logical,T,T F
# visNetwork,node.shape,character,rectangle,square delta curl icon triangleDown point triangle box rectangle dot bubble circle bubble star ellipse cylinder diamond rhombus image circularImage database text
# diagramer,node.shape,character,rectangle,square delta icon triangle rectangle box dot point egg oval text bubble circle star ellipse cylinder diamond rhombus
# diagramer,shinyInput.click,character,,
# diagramer,shinyInput.click,character,,
# visNetwork,node.size.max,numeric integer,40,
# diagramer,node.size.ratio,numeric integer,0.6,0 1
# diagramer,node.size,numeric integer,3,
# diagramer,node.size.max,numeric integer,4,
# visNetwork,node.size.min,numeric integer,15,
# diagramer,node.size.min,numeric integer,2,
# diagramer,title,character,,
# visNetwork,tooltip,character,, 
# diagramer,tooltip,character,,
# networkD3,link.tooltip.suffix,character,,
# networkD3,width,numeric integer,30,
# networkD3,label.size,numeric integer,30,
# plotly,size.min,numeric integer,10,
# plotly,size.max,numeric integer,500,
# amCharts,direction,character,up.down,up.down down.up
# amCharts,thetaAxis.tick.step,numeric integer,20,1 Inf
# amCharts,theta.min,numeric integer,0,
# amCharts,theta.max,numeric integer,100,
# amCharts,x.min,numeric integer,0,
# amCharts,x.max,numeric integer,100,
# amCharts,y.min,numeric integer,0,
# amCharts,y.max,numeric integer,100,
# amCharts,xAxis.tick.step,numeric integer,20,
# amCharts,yAxis.tick.step,numeric integer,20,
# amCharts,xAxis.step.enabled,logical,T,T F
# amCharts,yAxis.step.enabled,logical,T,T F
# d3plus,legend.enabled,logical,T,T F
# d3plus,legend.size,numeric integer,30,1 Inf
# d3plus,legend.tooltip.enabled,logical,T,T F
# d3plus,tooltip,character,,
# d3plus,additionalColumns,character,,
# d3plus,height,numeric integer character,,
# d3plus,width,numeric integer character,,
# d3plus,xAxis.grid.enabled,logical,T,T F
# d3plus,yAxis.grid.enabled,logical,T,T F
# d3plus,xAxis.min,numeric integer Date,,
# d3plus,xAxis.max,numeric integer Date,,
# d3plus,label.format,character,,
# d3plus,currency.symbol,character,US$,
# d3plus,shape,character,line,line bar
# morrisjs,yAxis.tick.suffix,character,,
# morrisjs,yAxis.tick.prefix,character,,
# dygraphs,subchart.enabled,logical,T,T F
# dygraphs,subchart.height,numeric,40,
# dygraphs,subchart.background,character,A7B1C4,
# dygraphs,subchart.color,character,808FAB,
# dygraphs,subchart.zoom.enabled,logical,T,T F
# highcharter,color.min,character,white,
# highcharter,color.max,character,blue,
# billboarder,title.position,character,left-top,left-top right-top
# billboarder,title.padding.top,integer,0,
# billboarder,title.padding.right,integer,0,
# billboarder,title.padding.left,integer,0,
# billboarder,title.padding.bottom,integer,20,
# amCharts,xAxis.label.rotation,numeric,0,-90 90
# amCharts,barMode,character,group,group stack
# amCharts,legend.enabled,logical,T,T F
# amCharts,theme,character,,light dark patterns chalk
# amCharts,legend.position,character,bottom,right left
# amCharts,title,character,,
# amCharts,title.color,character,black,
# amCharts,title.size,integer,15,1 100
# amCharts,xAxis.scrollbar.enabled,logical,F,T F
# amCharts,yAxis.scrollbar.enabled,logical,T,T F
# amCharts,xAxis.scrollbar.width,numeric,10,5 100
# amCharts,yAxis.scrollbar.width,numeric,10,5 100
# dimple,legend.enabled,logical,T,T F
# dc,yAxis.label,character,,
# dc,xAxis.label,character,,
# dc,xAxis.min,numeric,,
# dc,xAxis.max,numeric,,
# dc,legend.enabled,logical,T,T F
# dc,legend.position.x,numeric,20,
# dc,legend.position.y,numeric,20,
# dc,legend.width,numeric,140,
# dc,legend.height,numeric,140,
# diagramer,link.label.size,numeric integer,30,
# diagramer,link.arrow.size,numeric integer,1.5,


# DESCRIPTION -----------------------------------------------------------------------------

Package: niravis
Type: Package
Title: NIRASOFT R package for visualization 
Version: 5.7.4
Date: 06 November 2018
Author: Nima Ramezani
Maintainer: <nima.ramezani@gmail.com>
  Description: A toolbox for generating of more elegant visualizations and easier built of shiny dashboards
License: MIT
Collate: 
  'visgen.R'
'DT.R'
'TFD3.R'
'amCharts.R'
'billboarder.R'
'bpexploder.R'
'bubbleCloud.R'
'bubbles.R'
'c3.R'
'calheatmap.R'
'candela.R'
'coffeewheel.R'
'csscripts.R'
'd3plus.R'
'dashboard.R'
'dashtools.R'
'dc.R'
'dialogs.R'
'dimple.R'
'jscripts.R'
'dygraphs.R'
'echarts.R'
'ggvis.R'
'googleVis.R'
'grviz.R'
'highcharter.R'
'highcharts.R'
'leaflet.R'
'morrisjs.R'
'networkD3.R'
'niraplot.R'
'xCharts.R'
'visNetwork.R'
'rCharts.R'
'rbokeh.R'
'plotly.R'
'nvd3.R'
'rscripts.R'
'niravis.R'
'pivot.R'
'sankeyTree.R'
'streamgraph.R'
RoxygenNote: 6.0.1

# plotters.csv -----------------------------------------------------------------------------

# Plotter,bar,bar.molten,pie,pie.molten,scatter,scatter.molten,combo,combo.molten,box,box.molten,line,area,area.molten,graph,bubble,bubble.molten,tree,treemap,table,histogram,map,tsline,tsarea,tsbar,sankey,wordCloud,funnel,heatmap
# billboarder,x,x,x,,,x,,,,,,,,,,,,,,,,x,x,,,,,
# bpexploder,,,,,,,,,,x,,,,,,,,,,,,,,,,,,
# bubbles,,,,,,,,,,,,,,,x,,,,,,,,,,,,,
# c3,x,,x,,,x,x,,,,,,,,,,,,,,,x,,,,,,
# candela,x,x,,,x,,,,,,,,,,,,,,,,,,,,,,,
# coffeewheel,,,x,,,,,,,,,,,,,,,,,,,,,,,,,
# d3plus,,,,,x,,,,,,,,,,x,x,x,x,,,,,,,,,,
# DiagrammeR,,,,,,,,,,,,,,x,,,,,,,,,,,,,,
# dimple,,,,,,,x,x,,,,,,,,,,,,,,,,,,,,
# DT,,,,,,,,,,,,,,,,,,,x,,,,,,,,,
# dygraphs,,,,,,,x,,,,,,,,,,,,,,,,,,,,,
# echarts,x,,,,,,,,,,,,,,,,,,,,,,,,,x,x,
# ggvis,,,,,x,,,,,,,,,,,,,,,x,,,,,,,,
# googleVis,x,,,,,,,,,,x,x,,,,,,,,,,,,,,,,
# highcharter,,,x,,x,x,x,,,,,,,,,,,,,,,x,,,,,,
# highcharts,,,,,,x,,,,,,,,,,,,,,,,,,,,,,x
# leaflet,,,,,,,,,,,,,,,,,,,,,x,,,,,,,
# morrisjs,,,x,,,,,,,,,,,,,,,,,,,x,,x,,,,
# networkD3,,,,,,,,,,,,,,,,,,,,,,,,,x,,,
# nvd3,,x,x,x,,x,,,,,,,,,,,,,,,,,,,,,,
# pivot,,,,,,,,,,,,,,,,,,,x,,,,,,,,,
# plotly,x,,x,,x,,x,,,,,,,,,,,,,,,,,,,,,
# rAmCharts,x,,x,,,,,,,,,,,,,,,,,,,x,,,,,,
# rbokeh,,,,,x,,,,,,,,,,,,,,,,,,,,,,,
# rCharts,,,,,x,,,,,,,,,,,,,,,,,,,,,,,
# TFD3,,,,,,,,,,,,,,,,,,,x,,,,,,,,,
# visNetwork,,,,,,,,,,,,,,x,,,,,,,,,,,,,,
# xCharts,,,,,,,,,,,,,x,,,,,,,,,,,,,,,

# /htmlwidgets/bubbleforce.js -----------------------------------------------------------------------------
HTMLWidgets.widget({
  
  name: "bubbleForceInfobox",
  
  type: "output",
  
  initialize: function(el, width, height) {
    
    d3.select(el).append("svg")
    .attr("width", width)
    .attr("height", height);
    
    return d3.layout.force();
  },
  
  resize: function(el, width, height, force) {
    
    d3.select(el).select("svg")
    .attr("width", width)
    .attr("height", height);
    
    force.size([width, height]).resume();
  },
  
  renderValue: function(el, x, force) {
    
    // get the width and height
    var width = el.offsetWidth;
    var height = el.offsetHeight;
    
    
    //Get options
    //var initInfobox = x.settings.initInfobox;
    var initInfobox = "Click on the node to see details!";
    
    // alias options
    // var width = 560;
    // var height = 500;
    var padding = 1.5, // separation between same-color nodes
    clusterPadding = 6, // separation between different-color nodes
    maxRadius = 12;
    
    var n = x.n, // total number of nodes
    m = x.m; // number of distinct clusters
    
    var color = d3.scale.category10()
    .domain(d3.range(m));
    
    var clusters = HTMLWidgets.dataframeToD3(x.clusters);
    
    var nodes = HTMLWidgets.dataframeToD3(x.data);
    
    // todo: if force is not given:...
    var force = d3.layout.force()
    .nodes(nodes)
    .size([width, height])
    .gravity(0.02)
    .charge(0)
    .on("tick", tick)
    .start();
    
    // select the svg element and remove existing children
    var svg = d3.select(el).select("svg");
    svg.selectAll("*").remove();
    
    // draw nodes
    var node = svg.selectAll("circle")
    .data(nodes)
    .enter().append("circle")
    .style("fill", function(d) {
      return color(d.cluster);
    })
    .call(force.drag);
    
    // intro transition
    node.transition()
    .duration(750)
    .delay(function(d, i) {
      return i * 5;
    })
    .attrTween("r", function(d) {
      var i = d3.interpolate(0, d.radius);
      return function(t) {
        return d.radius = i(t);
      };
    });
    
    
    // Add click event
    // d3.selectAll(".node,.link")
    d3.selectAll("circle")
    .on("click", function(d, i) {
      // var d = this.__data__;
      // todo: This is where d.htmlinfo is returned, better to return label of the node as shiny input
      console.log("Node ID: ", i);
    });
    
    function tick(e) {
      node
      .each(cluster(10 * e.alpha * e.alpha))
      .each(collide(0.5))
      .attr("cx", function(d) {
        return d.x;
      })
      .attr("cy", function(d) {
        return d.y;
      });
    }
    
    // Move d to be adjacent to the cluster node.
    function cluster(alpha) {
      return function(d) {
        var cluster = clusters[d.cluster];
        if (cluster === d) return;
        var x = d.x - cluster.x,
        y = d.y - cluster.y,
        l = Math.sqrt(x * x + y * y),
        r = d.radius + cluster.radius;
        if (l != r) {
          l = (l - r) / l * alpha;
          d.x -= x *= l;
          d.y -= y *= l;
          cluster.x += x;
          cluster.y += y;
        }
      };
    }
    
    // Resolves collisions between d and all other circles.
    function collide(alpha) {
      var quadtree = d3.geom.quadtree(nodes);
      return function(d) {
        var r = d.radius + maxRadius + Math.max(padding, clusterPadding),
        nx1 = d.x - r,
        nx2 = d.x + r,
        ny1 = d.y - r,
        ny2 = d.y + r;
        quadtree.visit(function(quad, x1, y1, x2, y2) {
          if (quad.point && (quad.point !== d)) {
            var x = d.x - quad.point.x,
            y = d.y - quad.point.y,
            l = Math.sqrt(x * x + y * y),
            r = d.radius + quad.point.radius + (d.cluster === quad.point.cluster ? padding : clusterPadding);
            if (l < r) {
              l = (l - r) / l * alpha;
              d.x -= x *= l;
              d.y -= y *= l;
              quad.point.x += x;
              quad.point.y += y;
            }
          }
          return x1 > nx2 || x2 < nx1 || y1 > ny2 || y2 < ny1;
        });
      };
    }
    // Legend
    // http://jsfiddle.net/Rom2BE/H2PkT/
      
      var legendData = {clusterName: x.clusters.clusterName}
    legendData = HTMLWidgets.dataframeToD3(legendData)
    // console.log("color.domain(): ",color.domain())
    // console.log("Colors: ",color)
    // console.log("Color 3: ",color(3))
    // console.log(legendData)
    
    var legend = svg.selectAll(".legend")
    .data(legendData)
    .enter().append("g")
    .attr("class", "legend")
    .attr("transform", function(d, i) {
      return "translate(0," + i * 20 + ")";
    });
    
    legend.append("rect")
    .attr("x", width - 18)
    .attr("width", 18)
    .attr("height", 18)
    .style("fill", function(d,i) {
      return color(i);
    });
    
    legend.append("text")
    .attr("x", width - 24)
    .attr("y", 9)
    .attr("dy", ".35em")
    .style("text-anchor", "end")
    .text(function(d) {
      return d.clusterName;
    });
    
    // Tooltip
    
    $('svg circle').tipsy({
      gravity: 'w',
      html: true,
      title: function() {
        var d = this.__data__;
        var hover = d.hover
        return hover;
      }
    });
    
  },
});

# /htmlwidgets/bubbleforce.yaml -----------------------------------------------------------------------------
dependencies:
  - name: d3
version: 3.5.2
src: "lib/d3/d3-3.5.2"
script: d3.min.js
- name: jquery
version: 1.11.2
src: "/lib/jquery/v1.11.2"
script: jquery.min.js
- name: jquery.tipsy
version: 3.5.2
src: "lib/jquery.tipsy"
script: "/lib/jquery.tipsy/v3.5.2"
- name: tipsystyles
version: 0.2.1
src: style/bubbleforce/
  stylesheet: tipsy.css
- name: styles
version: 0.1.1
src: style/bubbleforce/
  stylesheet: style.css    



# /htmlwidgets/coffeewheel.js -----------------------------------------------------------------------------
HTMLWidgets.widget({
  
  name: 'coffeewheel',
  
  type: 'output',
  
  initialize: function(el, width, height) {
    return {
      el: el,
      width: width,
      height: height
    }
  },
  
  renderValue: function(el, x, instance) {
    initializeCoffeeWheel(x.treeData, el, instance.width, instance.height, x.partitionAttribute, x.main);
    instance["x"] = x;
  },
  
  resize: function(el, width, height, instance) {
    el.innerHTML = "";
    initializeCoffeeWheel(instance.x.treeData, el, width, height, instance.x.partitionAttribute, instance.x.main);
    instance.width = width;
    instance.height = height;
  }
  
});

# /htmlwidgets/coffeewheel.yaml -----------------------------------------------------------------------------
dependencies:
  - 
  name: d3
version: 3.4.4
src: htmlwidgets/
  script: d3.min.js
- 
  name: coffee-wheel
version: 0.1.1
src: htmlwidgets/
  script: d3-coffeewheel.js

# /htmlwidgets/d3-coffeewheel.js -----------------------------------------------------------------------------
var initializeCoffeeWheel = function(data, el, width, height, partitionAttribute, mainTitle) {
  var minSize = Math.min(width, height)-20;
  
  var div = d3.select(el);
  if(mainTitle.length > 0) {
    var mainTitleEl = div.append("h1")
    .attr("id", "main")
    .text(mainTitle);
    
    minSize -= 50;
  } 
  
  height = width = minSize;
  
  var radius = width / 2,
  x = d3.scale.linear().range([0, 2 * Math.PI]),
  y = d3.scale.pow().exponent(1.3).domain([0, 1]).range([0, radius]),
  padding = 0,
  duration = 1000;
  
  var vis = div.append("svg")
  .attr("width", width + padding * 2)
  .attr("height", height + padding * 2)
  .append("g")
  .attr("transform", "translate(" + [radius + padding, radius + padding] + ")");
  
  var partition = d3.layout.partition()
  .sort(null)
  .value(function(d) {
    return d[partitionAttribute];
  });
  
  var arc = d3.svg.arc()
  .startAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x))); })
  .endAngle(function(d) { return Math.max(0, Math.min(2 * Math.PI, x(d.x + d.dx))); })
  .innerRadius(function(d) { return Math.max(0, d.y ? y(d.y) : d.y); })
  .outerRadius(function(d) { return Math.max(0, y(d.y + d.dy)); });
  
  function isParentOf(p, c) {
    if (p === c) return true;
    if (p.children) {
      return p.children.some(function(d) {
        return isParentOf(d, c);
      });
    }
    return false;
  }
  
  function colour(d) {
    if (d.colour == undefined && d.children) {
      // There is a maximum of two children!
        var colours = d.children.map(colour);
        var sum = { r: 0, g: 0, b: 0 };
        for(var i=0; i < colours.length; i++) {
          var aColor = d3.rgb(colours[i]);
          sum.r = Math.max(aColor.r, sum.r);
          sum.g = Math.max(aColor.g, sum.g);
          sum.b = Math.max(aColor.b, sum.b);
        }
        return d3.rgb(sum.r, sum.g, sum.b);
    }
    
    return d.colour || "#fff";
  }
  
  // Interpolate the scales!
    function arcTween(d) {
      var my = maxY(d),
      xd = d3.interpolate(x.domain(), [d.x, d.x + d.dx]),
      yd = d3.interpolate(y.domain(), [d.y, my]),
      yr = d3.interpolate(y.range(), [d.y ? 20 : 0, radius]);
      return function(d) {
        return function(t) { x.domain(xd(t)); y.domain(yd(t)).range(yr(t)); return arc(d); };
      };
    }
  
  function maxY(d) {
    return d.children ? Math.max.apply(Math, d.children.map(maxY)) : d.y + d.dy;
  }
  
  // http://www.w3.org/WAI/ER/WD-AERT/#color-contrast
    function brightness(rgb) {
      return rgb.r * .299 + rgb.g * .587 + rgb.b * .114;
    }
  
  (function(json) {
    var assignSizes = function(node) {
      if(node.children == undefined || node.children.length < 1) {
        return;
      } else {
        var numOfChilds = node.children.length
        for(var ci=0; ci < numOfChilds; ci++) {
          var childNode = node.children[ci];
          childNode.value = 1.0/numOfChilds;
          assignSizes(childNode);
        }
      }
    };
    
    for(var i=0; i < json.length; i++) {
      assignSizes(json[i]);
    }
    
    var nodes = partition.nodes({children: json});
    
    var path = vis.selectAll("path").data(nodes);
    path.enter().append("path")
    .attr("id", function(d, i) { return "path-" + i; })
    .attr("d", arc)
    .attr("fill-rule", "evenodd")
    .style("fill", colour)
    .on("click", click);
    
    var text = vis.selectAll("text").data(nodes);
    var textEnter = text.enter().append("text")
    .style("fill-opacity", 1)
    .style("fill", function(d) {
      return brightness(d3.rgb(colour(d))) < 125 ? "#eee" : "#000";
    })
    .attr("text-anchor", function(d) {
      return x(d.x + d.dx / 2) > Math.PI ? "end" : "start";
    })
    .attr("dy", ".2em")
    .attr("transform", function(d) {
      var angle = x(d.x + d.dx / 2) * 180 / Math.PI - 90,
      rotate = angle;
      return "rotate(" + rotate + ")translate(" + (y(d.y) + padding) + ")rotate(" + (angle > 90 ? -180 : 0) + ")";
    })
    .on("click", click);
    textEnter.append("tspan")
    .attr("x", 0)
    .text(function(d) { return d.depth ? d.name.split(" ")[0] : ""; });
    
    function click(d) {
      path.transition()
      .duration(duration)
      .attrTween("d", arcTween(d));
      
      // Somewhat of a hack as we rely on arcTween updating the scales.
      text.style("visibility", function(e) {
        return isParentOf(d, e) ? null : d3.select(this).style("visibility");
      })
      .transition()
      .duration(duration)
      .attrTween("text-anchor", function(d) {
        return function() {
          return x(d.x + d.dx / 2) > Math.PI ? "end" : "start";
        };
      })
      .attrTween("transform", function(d) {
        var multiline = (d.name || "").split(" ").length > 1;
        return function() {
          var angle = x(d.x + d.dx / 2) * 180 / Math.PI - 90,
          rotate = angle + (multiline ? -.5 : 0);
          return "rotate(" + rotate + ")translate(" + (y(d.y) + padding) + ")rotate(" + (angle > 90 ? -180 : 0) + ")";
        };
      })
      .style("fill-opacity", function(e) { return isParentOf(d, e) ? 1 : 1e-6; })
      .each("end", function(e) {
        d3.select(this).style("visibility", isParentOf(d, e) ? null : "hidden");
      });
    }
  })(data);
  
};
# /htmlwidgets/d3.min.js -----------------------------------------------------------------------------

# /htmlwidgets/grviz.js -----------------------------------------------------------------------------
HTMLWidgets.widget({
  name: 'grviz',
  type: 'output',
  
  initialize: function(el, width, height) {
    return {
      // TODO: add instance fields as required
    };
  },
  
  renderValue: function(el, x, instance) {
    //  Use this to sort of make our diagram responsive
    //  or at a minimum fit within the bounds set by htmlwidgets
    //  for the parent container
    function makeResponsive(el){
      var svg = el.getElementsByTagName("svg")[0];
      if (svg) {
        if (svg.width) {svg.removeAttribute("width")}
        if (svg.height) {svg.removeAttribute("height")}
        svg.style.width = "100%";
        svg.style.height = "100%";
      }
    }
    
    if (x.diagram !== "") {
      if (typeof x.config === "undefined"){
        x.config = {};
        x.config.engine = "dot";
        x.config.options = {};
      }
      try {
        el.innerHTML = Viz(x.diagram, format="svg", engine=x.config.engine, options=x.config.options);
        makeResponsive(el);
        
        // set up a container for tasks to perform after completion
        //  one example would be add callbacks for event handling
        //  styling
        if (typeof x.tasks !== "undefined") {
          if ((typeof x.tasks.length === "undefined") ||
              (typeof x.tasks === "function")) {
            // handle a function not enclosed in array
            // should be able to remove once using jsonlite
            x.tasks = [x.tasks];
          }
          x.tasks.map(function(t){
            // for each tasks add it to the mermaid.tasks with el
            t.call(el);
          });
        }
      } catch(e){
        var p = document.createElement("pre");
        p.innerText = e;
        el.appendChild(p);
      }
    }
    
  },
  
  resize: function(el, width, height, instance) {
  }
});

# /htmlwidgets/grviz.yaml -----------------------------------------------------------------------------
dependencies:
  - name: viz
version: 1.8.2
src: lib/viz
script: viz.js

# /htmlwidgets/pivot.js -----------------------------------------------------------------------------
HTMLWidgets.widget({
  
  name: 'pivot',
  
  type: 'output',
  
  initialize: function(el, width, height) {
    
    return {};
    
  },
  
  renderValue: function(el, x, instance) {
    x.data = HTMLWidgets.dataframeToD3(x.data);
    
    var derivers = $.pivotUtilities.derivers;
    var tpl = $.pivotUtilities.aggregatorTemplates;
    
    // set locale to "en" which is the default for pivottable
    //  this eases code later
    if(typeof(x.locale) === "undefined") x.locale = "en";
    
    x.params.renderers = $.extend(
      $.pivotUtilities.locales[x.locale].renderers,
      $.pivotUtilities.d3_renderers,
      $.pivotUtilities.c3_renderers
    );
    
    // temporary hack to make Portuguese d3 and c3 renderers
    if(x.locale === "pt"){
      x.params.renderers["Mapa de Árvore"] = x.params.renderers["Treemap"];
      x.params.renderers["Gráfico de Linha"] = x.params.renderers["Line Chart"];
      x.params.renderers["Gráfico de Barras"] = x.params.renderers["Bar Chart"];
      x.params.renderers["Gráfico de Barras Empilhadas"] = x.params.renderers["Stacked Bar Chart"];
      x.params.renderers["Gráfico de Área"] = x.params.renderers["Area Chart"];
      x.params.renderers["Gráfico de Dispersão"] = x.params.renderers["Scatter Chart"];
      
      // delete the English
      delete(x.params.renderers["Treemap"]);
      delete(x.params.renderers["Line Chart"]);
      delete(x.params.renderers["Bar Chart"]);
      delete(x.params.renderers["Stacked Bar Chart"]);
      delete(x.params.renderers["Area Chart"]);
      delete(x.params.renderers["Scatter Chart"]);
      
    }
    
    if (typeof x.params.sorters != "undefined") {
      if (typeof x.params.sorters[0] == "string") {
        x.params.sorters = eval("("+x.params.sorters[0]+")")
      }
    }
    
    if (typeof x.params.onRefresh != "undefined") {
      x.params.onRefresh = x.params.onRefresh[0];
    }
    
    $('#'+el.id).pivotUI(
      x.data, x.params, true, x.locale
    );
    
  },
  
  resize: function(el, width, height, instance) {
    
  }
  
});

# /htmlwidgets/pivot.yaml -----------------------------------------------------------------------------
dependencies:
  - name: jquery
version: 1.11.2
src: lib/pivot/jquery
script: jquery.min.js
- name: jquery-ui
version: 1.11.4
src: lib/pivot/jqueryui
script:
  - jquery-ui.min.js
- jquery.ui.touch-punch.min.js
stylesheet: jquery-ui.min.css
- name: c3
version: 0.4.11
src: lib/general/c3
script: c3.min.js
stylesheet: c3.min.css
- name: d3
version: 3.5.5
src: lib/general/d3
script: d3.v3.min.js
- name: pivottable
version: 2.11.0
src: lib/pivot/pivottable
stylesheet: pivot.min.css
script: pivot.min.js
- name: pivottable_d3renderers
version: 2.11.0
src: lib/pivot/pivottable
script: d3_renderers.min.js
- name: pivottable_c3renderers
version: 2.11.0
src: lib/pivot/pivottable
script: c3_renderers.min.js
- name: polyfill
version: 0.1
src: lib/pivot/polyfill
script: polyfill.min.js
- name: pivot_style
version: 0.1
src: style/pivot
stylesheet: pivot.css
#


# /htmlwidgets/TFD3.js -----------------------------------------------------------------------------
HTMLWidgets.widget({
  
  name: "TFD3",
  
  type: "output",
  
  renderValue: function(el, data) {
    
    function log(message){
      if(typeof console == "object"){
        console.log(message);
      }
    }
    
    var $el = $(el);
    // name ouf the output widget
    var outputID = el.id;
    
    var celldata = HTMLWidgets.dataframeToD3(data.data);
    
    var footdata = HTMLWidgets.dataframeToD3(data.footData);
    
    var columns = data.columns;
    
    // set TF base path depending on mode
    var url = window.HTMLWidgets.getAttachmentUrl(id="tablefilter", key = 1);
    url = url.replace(/TF_Themes.*/, '');
    data.tableProps["base_path"] = url;
    
    // adjust path for tablefilter extensions
    if(data.tableProps.hasOwnProperty("extensions")) {
      if(data.tableProps.extensions.hasOwnProperty("src")) {
        for(var i = 0; i < data.tableProps.extensions.src.length; i++) {
          data.tableProps.extensions.src[i] = url + data.tableProps.extensions.src[i];
        }
      }
    }
    
    // need these global variables for server side edits
    window.TFD3 = window.TFD3 || {};
    window.TFD3["bgColScales_" + outputID] = data.bgColScales;
    window.TFD3["fgColScales_" + outputID] = data.fgColScales;
    window.TFD3["cellFunctions_" + outputID] = data.cellFunctions;
    window.TFD3["footCellFunctions_" + outputID] = data.footCellFunctions;
    window.TFD3["sparklines_" + outputID] = data.sparklines;
    
    var edit = data.edit;
    
    // have a unique id for each edit
    var editCounter = 0;
    
    var radioButtons = data.radioButtons;
    var checkBoxes = data.checkBoxes;
    
    var tableID = el.id + '_tbl';
    var tfName = 'tf_' + el.id;
    var inputID = outputID + '_edit';
    
    //  generate a filter input?
      //    var filterInput = data.filterInput;
    //    if(filterInput) {
      //      data.tableProps["on_after_filter"] = function(o) {updateFilterInput(o)};
      //    }
    var initialFilters = data.initialFilters;
    
    var sortKeys = data.sortKeys;
    
    // need to update colour after table sorting
    data.tableProps["on_after_sort"] = function(o) {colourCellsWrapper(o)};
    
    // remove existing table including table filter objects
    var table = d3.select(el).select("table").remove();
    var loader = d3.select(el).selectAll(".loader").remove();
    var inf = d3.select(el).selectAll(".inf").remove();
    
    // create new table
    var table = d3.select(el)
    .append("table")
    .attr("id", tableID)
    .classed({'table-condensed': true});
    
    var  thead = table.append("thead");
    var  tbody = table.append("tbody");
    
    thead.append("tr")
    .selectAll("th")
    .data(columns)
    .enter()
    .append("th")
    .text(function(column) { var name = (data.colNames != null && data.colNames.hasOwnProperty(column)) ? data.colNames[column] : column; return name; });
    
    // create a row for each object in the data
    var rows = tbody.selectAll("tr")
    .data(celldata)
    .enter()
    .append("tr")
    .attr('id', function(d, i) {return 'r' + i})
    .attr('class', 'tbl_' + outputID);
    if(data.key) {
      rows.attr("key", function(d,i) { return (data.key[i]) ;});
    }
    
    // create a cell in each row for each column
    var cells = rows.selectAll("td")
    .data(function(row) {
      return columns.map(function(column) {
        return {column: column, value: row[column]};
      });
    })
    .enter()
    .append("td")
    .html(function(d) { return d.value; })
    // address columns table filter style
    .attr('class', function(d, i, j){ return "col_" + i + ' ' + 'row_' + j + ' ' + 'tbl_' + outputID; });
    
    // create a table footer
    var  tfoot = table.append("tfoot");
    
    // create a row for each object in the data
    var footrows = tfoot.selectAll("tr")
    .data(footdata)
    .enter()
    .append("tr")
    .attr('id', function(d, i) {return 'fr' + i})
    .attr('class', 'tbl_' + outputID);
    
    // create a cell in each row for each column of the footer
    var footcells = footrows.selectAll("td")
    .data(function(row) {
      return columns.map(function(column) {
        return {column: column, value: row[column]};
      });
    })
    .enter()
    .append("td")
    .html(function(d) { return d.value; })
    // set an id to use for tablefilter "col_operations"
    .attr('id', function(d, i, j){ return 'frow_' + j + '_fcol_' + i + '_' +  'tbl_' + outputID; })
    .attr('class', function(d, i, j){ return "col_" + i + ' ' + 'row_' + j + ' ' + 'tbl_' + outputID; });
    
    // make table bootstrap styled
    if(data.tableStyle != null) {
      table.classed(data.tableStyle, true);
      tfoot.classed(data.tableStyle, true);
      thead.classed(data.tableStyle, true);
    }
    
    // apply row styles and add rows with style "info" to preselected
    var rowStyles = data.rowStyles;
    var selected = [];
    if(rowStyles != null) {
      rows.each(
        function(d, i, j) {
          
          var elt = d3.select(this);
          elt.classed(rowStyles[i], true);
          
          if(rowStyles[i] == data.selectableRowsClass){
            selected.push(i + 1)
          }
        }
      )
    }
    var inputID = outputID + "_select";
    if(window.HTMLWidgets.shinyMode) {
      Shiny.onInputChange(inputID, selected);
    }
    
    // debounce from Underscore.js
    // modified to allow rapid editing of multiple cells
    // if args are different between subsequent calls,
    // fire the previous call immediately.
    function debounce(func, wait, immediate) {
      var timeout, args, context, timestamp, result;
      return function() {
        // simply testing args != arguments doesnt work (timestamp in args[0]?)
        if(args != null && (args[1] != arguments[1] || args[2] != arguments[2])) {
          // called rapidly twice with different args.
          // execute previous call immediately
          func.apply(context, args);
        }
        context = this;
        args = arguments;
        timestamp = new Date();
        var later = function() {
          var last = (new Date()) - timestamp;
          if (last < wait) {
            timeout = setTimeout(later, wait - last);
          } else {
            timeout = null;
            if (!immediate) {
              result = func.apply(context, args);
              // Normal exit after timeout.
              // Set args null to have a clean start again.
              args = null;
            }
          }
        };
        var callNow = immediate && !timeout;
        if (!timeout) {
          timeout = setTimeout(later, wait);
        }
        if (callNow) result = func.apply(context, args);
        return result;
      };
    };
    
    
    // create a shiny input event, named as
    //  the corresponding output element + "_edit"
    function shinyInputEvent(d, i, j) {
      var sel = d3.select(this);
      var regex = /col_(\d+)/;
      var col = Number(regex.exec(this.className)[1]);
      var regex = /row_(\d+)/;
      var row = Number(regex.exec(this.className)[1]) + 1;
      var regex = /tbl_(\S+)/;
      var tbl = regex.exec(this.className)[1];
      var inputID = tbl + '_edit';
      var editID = "edit_" + tbl + '_' + editCounter++;
      sel.attr('id', editID);
      if(data.showRowNames) {
        col = col;
      } else {
        col = col + 1;
      }
      var val;
      if(this.type == "checkbox") {
        if(this.checked) {
          val = true;
        } else {
          val = false;
        }
      } else {
        var val = sel.text();
      }
      if(window.HTMLWidgets.shinyMode) {
        var edit = {id: editID, row: row, col: col, val: val};
        Shiny.onInputChange(inputID, edit);
      } else {
        row = 'row_' + (row - 1);
        col = 'col_' + col;
        var selector = '.' + row + '.' + col;
        var cell = d3.select('#' + tbl)
        .selectAll('tbody')
        .select(selector);
        setCellData(cell, val, tbl, col);
        runCellFunctions(tbl, col);
        runCellFunctions(tbl, col, foot = true);
        colourCol(tbl, col);
      }
    }
    
    
    // generate shiny input from radio buttons
    // get event from button group, need to find out which
    // button is selected
    function checkRadio (name) {
      var tbl = name.replace(/_.*/g, '');
      var col = name.replace(/.*_col/, 'col');
      var editID = "edit_" + editCounter++;;
      var inputID = tbl + '_edit';
      var radio = d3.selectAll('#' + tbl)
      .selectAll('td.' + col)
      .selectAll("input");
      var row;
      var states = radio.each(function(d, i, j)
      { if(this.checked) {
        var regex = /row_(\d+)/;
        row = Number(regex.exec(this.className)[1]);
        d.value = true;
      } else {
        d.value = false;
      }
      });
      
      var cell = d3.selectAll('#' + tbl)
      .selectAll('td.' + col + ' ' + '.row_' + row)
      .attr('id', editID);
      if(window.HTMLWidgets.shinyMode) {
        col = Number(col.replace(/col_/, ''));
        if(!data.showRowNames) {
          col = col + 1;
        }
        row = row + 1;
        var edit = {id: editID, row: row, col: col, val: true};
        Shiny.onInputChange(inputID, edit);
      }
    }
    
    // allow to reset an input value
    Shiny.addCustomMessageHandler('resetD3tfValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
    
    // update data for D3
    function setCellData(cell, val, tbl, col) {
      cell[0][0].__data__.value = val;
      if(cell[0][0].firstChild.type == "radio") {
        // uncheck other buttons in group
        var radio = d3.selectAll('#' + tbl)
        .selectAll('td.' + col)
        .selectAll("input")
        .property("checked", false);
      } else if (cell[0][0].firstChild.type == "checkbox" || cell[0][0].firstChild.type == "radio") {
        cell.selectAll("input").property("checked", val);
      } else if (cell.classed("sparklines")) {
        cell = cell.attr('value', val)
        .html(val);
        setSparkline(tbl, cell, col);
      } else {
        if(cell.selectAll("text").empty()) {
          // simple cell, update text directly
          cell = cell.attr('value', val)
          .html(val);
        } else {
          // cell styled using cellfunctions, look for text element within
          cell = cell.attr('value', val)
          .selectAll("text")
          .html(val);
        }
      }
    }
    
    
    // turn cell content into sparklines
    function setSparklines(tbl) {
      var sparklines = window.TFD3["sparklines_" + tbl];
      for (var key in sparklines) {
        if (sparklines.hasOwnProperty(key)) {
          table = tbl;
          var sparkCell = d3.selectAll('#' + tbl)
          .selectAll('tbody')
          .selectAll('td.' + key)
          .classed("sparklines", true);
          setSparkline(tbl, sparkCell, key);
        }
      }
    };
    
    function setSparkline(tbl, cell, key) {
      var sparklines = window.TFD3["sparklines_" + tbl];
      $(cell[0]).sparkline('html', sparklines[key])
    }
    
    // server side edit, confirm or reject
    Shiny.addCustomMessageHandler("setCellValue",
                                  function(message) {
                                    var row = 'row_' + (Number(message["row"]) - 1);
                                    if(data.showRowNames) {
                                      var col = 'col_' + message["col"];
                                    } else  {
                                      var col = 'col_' + (Number(message["col"]) - 1);
                                    }
                                    var tbl = message["tbl"];
                                    var selector = '.' + row + '.' + col;
                                    if(message["foot"]) {
                                      var cell = d3.select('#' + tbl)
                                      .selectAll('tfoot')
                                      .select(selector);
                                    } else {
                                      var cell = d3.select('#' + tbl)
                                      .selectAll('tbody')
                                      .select(selector);
                                    }
                                    
                                    if(message["action"] == "confirm" || message["action"] == "reject") {
                                      // only do something if cell id matches message
                                      cell = cell.filter('#' + message["id"]);
                                      if(cell.empty()) {
                                        return(null);
                                      }
                                      
                                      // signal reject by transient text colour change
                                      if(message["action"] == "reject") {
                                        // store color in attr so we can reset to it in subsequent edits
                                        cell.attr("oldcolor", function() {return cell.style("color")});
                                        var oldColor = cell.attr("oldcolor");
                                        cell.style("color", message["color"]);
                                        // if sever sends value, reset input to it and transition
                                        // color back to previous
                                        if(message["value"] !== null) {
                                          cell.transition("textcolor")
                                          .duration(1500)
                                          .style("color", oldColor)
                                          .attr('id', '')
                                          .attr('oldcolor', '');
                                        }
                                      } else if (message["action"] == "confirm") {
                                        // confirm edit by transient colour change
                                        if(cell.attr("oldcolor")) {
                                          // previous validation failed
                                          var oldColor = cell.attr("oldcolor");
                                        } else {
                                          var oldColor = cell.style("color");
                                        }
                                        if(message["color"]) {
                                          cell.style("color", message["color"])
                                          .transition("textcolor")
                                          .duration(1500)
                                          .style("color", oldColor)
                                          .attr('oldcolor', '');
                                        }
                                      }
                                      
                                    } // confirm or reject
                                    
                                    // no new value, no further action
                                    if(message["value"] === null) {
                                      return(null)
                                    }
                                    
                                    var val = message["value"];
                                    setCellData(cell, val, tbl, col);
                                    
                                    colourCol(tbl, col);
                                    if(message["foot"]) {
                                      runCellFunctions(tbl, col, foot = true);
                                    } else {
                                      runCellFunctions(tbl, col);
                                    }
                                    
                                    if(window.HTMLWidgets.shinyMode) {
                                      // send confirmation back to server
                                      // cell gets labeled with a unique edit id.
                                      // this way a confirmation or reject from the server will find
                                      // only the most recent edit
                                      if(message["feedback"]) {
                                        var editID = "edit_" + tbl + '_' + editCounter++;
                                        var inputID = tbl + '_edit';
                                        cell.attr('id', editID);
                                        var edit = {id: editID, row: message["row"], col:  message["col"], val: val};
                                        Shiny.onInputChange(inputID, edit);
                                      }
                                    }
                                  });
    
    // format cells or turn cell content in graphics
    function runCellFunctions(tbl, col, foot) {
      if(foot == true) {
        var selector = "tfoot";
        var cellFunctions =  window.TFD3["footCellFunctions_" + tbl];
      } else {
        var selector = "tbody"
        var cellFunctions = window.TFD3["cellFunctions_" + tbl] ;
      }
      if(col == null) {
        // check whole table
        for (var key in cellFunctions) {
          if (cellFunctions.hasOwnProperty(key)) {
            table = tbl; // strange. this makes it accessible inside of the select
            var cells = d3.selectAll('#' + table)
            .selectAll(selector)
            .selectAll('td.' + key);
            cells.call(cellFunctions[key]);
          };
        }
      } else {
        // only selected column
        if (cellFunctions.hasOwnProperty(col)) {
          var cells = d3.selectAll('#' + tbl)
          .selectAll(selector)
          .selectAll('td.' + col);
          cells.call(cellFunctions[col]);
        }
      }
    };
    
    
    // enable editing of a table
    Shiny.addCustomMessageHandler("enableEdit",
                                  function(message) {
                                    var cells = d3.selectAll('#' + message["tbl"])
                                    .selectAll('tbody');
                                    if(message["cols"] !== null) {
                                      cells = cells.selectAll(message["cols"]);
                                    } else {
                                      cells = cells.selectAll('td');
                                    }
                                    cells.attr({contenteditable: true})
                                    .on("input", debounce(shinyInputEvent, 800));
                                  });
    
    // disable editing of a table
    Shiny.addCustomMessageHandler("disableEdit",
                                  function(message) {
                                    var cells = d3.selectAll('#' + message["tbl"])
                                    .selectAll('tbody');
                                    if(message["cols"] !== null) {
                                      cells = cells.selectAll(message["cols"]);
                                    } else {
                                      cells = cells.selectAll('td');
                                    }
                                    cells.attr({contenteditable: false})
                                    .on("input", null);
                                  });
    
    // set filter
    Shiny.addCustomMessageHandler("setFilter",
                                  function(message) {
                                    var tfName = 'tf_' + message["tbl"];
                                    window[tfName].setFilterValue(message["col"], message["filterString"]);
                                    if(message["doFilter"]) {
                                      window[tfName].filter();
                                    }
                                    
                                  });
    
    // highlight a table row
    Shiny.addCustomMessageHandler("rowClass",
                                  function(message) {
                                    var clss = message["class"];
                                    var tbl = message["tbl"];
                                    
                                    var rows = d3.selectAll('#' + tbl)
                                    .selectAll('tbody')
                                    .selectAll('tr');
                                    
                                    // radio button behavior: clear selectableRowsClass from all rows
                                    // restore previous class
                                    if (data.selectTableRows == "single" && clss == data.selectTableRows ) {
                                      rows.classed(data.selectableRowsClass, false);
                                      rows.classed(data.rowStyles, true);
                                    }
                                    
                                    // current row
                                    var row = d3.selectAll('#' +  tbl)
                                    .select('#r' + (Number(message["row"]) - 1));
                                    row.classed({"active": false,  "success": false, "info": false, "warning": false, "danger": false});
                                    if(clss != "none") {
                                      row.classed(clss, true);
                                    }
                                    
                                    // now update selected rows input
                                    if(window.HTMLWidgets.shinyMode) {
                                      var selected = [];
                                      rows.each(function(d, i) {
                                        if($(this).hasClass(data.selectableRowsClass)) {
                                          selected.push(Number(this.id.replace('r', '')) + 1);
                                        }
                                      })
                                      var inputID = tbl + '_select';
                                      Shiny.onInputChange(inputID, selected);
                                    }
                                  });
    
    // handler for selectableRows
    // create a shiny input event, named like
    //  the corresponding output element + "_select"
    // also sends message to crosstalk
    if(typeof crosstalk != "undefined") {
      var ct_sel = new crosstalk.SelectionHandle(data.group);
      var ct_filter = new crosstalk.FilterHandle(data.group);
    }
    function shinyRowClickEvent(d, i, j) {
      var regex = /tbl_(\w+)/;
      
      var tbl = regex.exec(this.className)[1];
      var rows = d3.selectAll('#' + tbl)
      .selectAll('tbody')
      .selectAll('tr');
      
      var inputID = tbl + '_select';
      var sel = d3.select(this);
      if (!d3.event.ctrlKey || data.selectableRows == "single" ) {
        rows.classed(data.selectableRowsClass, false);
      }
      if($(this).hasClass(data.selectableRowsClass)) {
        sel.classed(data.selectableRowsClass, false);
      } else {
        sel.classed(data.selectableRowsClass, true);
      }
      
      var selected = [];
      var selectedKeys = [];
      rows.each(function(d, i) {
        if($(this).hasClass(data.selectableRowsClass)) {
          selected.push(Number(this.id.replace('r', '')) + 1);
          selectedKeys.push((String($(this).attr("key"))));
        }
      })
      if(typeof crosstalk != "undefined") {
        ct_sel.set(selectedKeys, {
          // Attach a sender property to the event
          sender: el
        });
      }
      if(window.HTMLWidgets.shinyMode) {
        Shiny.onInputChange(inputID, selected);
      }
    }
    
    // crosstalk selection handling
    if(typeof crosstalk != "undefined") {
      ct_sel.on("change", function(e) {
        if (e.sender === el) {
          return;
        }
        var rows = d3.select(el).selectAll('tbody')
        .selectAll('tr')
        .classed(data.selectableRowsClass, function(d) {
          if($.inArray(String($(this).attr("key")), e.value) == -1) {
            return false;
          } else {
            return true;
          }
        });
      });
    }
    
    // listen to crosstalk filter events
    if(typeof crosstalk != "undefined") {
      ct_filter.on("change", function(e) {
        if (e.sender === el) {
          return;
        };
        var rows = d3.select(el).selectAll('tbody')
        .selectAll('tr')
        .style("display", function(d) {
          if($.inArray(String($(this).attr("key")), e.value) == -1) {
            return "none";
          } else {
            return "table-row";
          }
        });
      });
    }
    
    // clear filters from table
    Shiny.addCustomMessageHandler("clearFilters",
                                  function(message) {
                                    var tfName = 'tf_' + message["tbl"];
                                    window[tfName].ClearFilters();
                                    if(message["doFilter"]) {
                                      window[tfName].filter();
                                    }
                                  });
    
    
    // calculate min / max / extent per column. Can be used from R for
    // dynamic colour scale range
    colExtent = function(tbl, col) {
      var colVals = d3.selectAll('#' + tbl)
      .selectAll("tbody")
      .selectAll('td.' + col)
      .data();
      var colExtent = d3.extent(colVals, function(d) { return d.value; });
      return(colExtent);
    }
    colMin = function (tbl, col){
      var colVals = d3.selectAll('#' + tbl)
      .selectAll("tbody")
      .selectAll('td.' + col)
      .data();
      var colMin = d3.min(colVals, function(d) { return d.value; })
      return(colMin);
    }
    colMax = function(tbl, col) {
      var colVals = d3.selectAll('#' + tbl)
      .selectAll("tbody")
      .selectAll('td.' + col)
      .data();
      var colMax = d3.max(colVals, function(d) { return d.value; })
      return(colMax);
    }
    
    // apply fg and bg colour scales to column
    function colourCol(tbl, col) {
      var bgColScales = window.TFD3["bgColScales_" + tbl];
      if (bgColScales.hasOwnProperty(col)) {
        table = tbl;
        var col2Color = d3.selectAll('#' + tbl)
        .selectAll("tbody")
        .selectAll('td.' + col)
        .transition("bgcolor")
        
        // run the d3 colour scale function defined in the bgColScales list on the R side
        col2Color.style("background-color", function(d, i){
          return bgColScales[col](tbl, d.value);
        });
      }
      var fgColScales = window.TFD3["fgColScales_" + tbl];
      if (fgColScales.hasOwnProperty(col)) {
        table = tbl;
        d3.selectAll('#' + tbl)
        .selectAll("tbody")
        .selectAll('td.' + col)
        //          .transition()
        
        // run the d3 colour scale function defined in the bgColScales list on the R side
        .style("background-color", function(d, i){
          return fgColScales[col](tbl, d.value);
        });
      }
    }
    
    // called from TableFilter. get table name from table filter object
    function colourCellsWrapper(o) {
      tbl = o['id'].replace(/_tbl/, '');
      colourCells(tbl);
    }
    
    // set background color for whole table
    // does nothing if length(bgColScales) == 0 and length(fgColScales) == 0
    function colourCells(tbl) {
      var bgColScales = window.TFD3["bgColScales_" + tbl];
      for (var key in bgColScales) {
        if (bgColScales.hasOwnProperty(key)) {
          table = tbl; // strange. this makes it accessible inside of the select
          d3.selectAll('#' + table)
          .selectAll('tbody')
          .selectAll('td.' + key)
          .style("background-color", function(d, i){
            // run the d3 colour scale function defined in the bgColScales list on the R side
            return bgColScales[key](tbl, d.value);
          });
        }
      };
      
      // set text color for whole table
      var fgColScales = window.TFD3["fgColScales_" + tbl];
      for (var key in fgColScales) {
        if (fgColScales.hasOwnProperty(key)) {
          table = tbl; // strange. this makes it accessible inside of the select
          d3.selectAll('#' + tbl)
          .selectAll('tbody')
          .selectAll('td.' + key)
          .style("color", function(d, i){
            // run the d3 colour scale function defined in the fgColScales list on the R side
            return fgColScales[key](tbl, d.value);
          });
        }
      };
    };
    
    // generate a shiny input listing the filter settings and
    // the displayed rows index
    // additinally set crosstalk filter
    function updateFilterInput(tbl) {
      
      // only in shiny mode
      if(!window.HTMLWidgets.shinyMode) {
        return(null);
      }
      
      // extract table id from tablefiler object
      tblID = tbl['id'].replace(/_tbl/, '');
      tfName = "tf_" + tblID;
      
      // get the row index. don't use tablefilter validRows because
      // it depends on sorting
      validRows = [];
      validKeys = [];
      d3.selectAll('#' + tbl['id'])
      .selectAll('tbody')
      .selectAll('tr')
      .each(function(d, i) {
        if(this.style["display"] !== "none") {
          // add 1 to match R row numbers
          validRows.push(Number(this.id.replace('r','')) + 1);
          validKeys.push($(this).attr("key"));
        }
      });
      
      if(typeof ct_filter != "undefined") {
        ct_filter.set(validKeys);
      }
      
      var filters = tbl.getFiltersValue();
      var filterSettings = [];
      var i = 1;
      filters.forEach(function(x) {
        var column = 'col_' + i;
        var value = x;
        filterSettings.push({column: column, value: value});
        i++;
      });
      
      var filterInputID = tblID + '_filter';
      
      filters = {filterSettings: filterSettings, validRows: validRows};
      Shiny.onInputChange(filterInputID, filters);
    }
    
    // make table rows selectable
    if(data.selectableRows == "single" || data.selectableRows == "multi") {
      table.classed({'table-hover': true})
      rows.attr({clickable: true})
      .on("click", shinyRowClickEvent);
    }
    
    // make cells editable
    if(edit === true) {
      cells.attr({contenteditable: true})
      .on("input", debounce(shinyInputEvent, 800));
    } else if (typeof(edit) == "string") {
      rows.selectAll(edit)
      .attr({contenteditable: true})
      .on("input", debounce(shinyInputEvent, 800));
    };
    
    // create radio buttons
    if (typeof(radioButtons) == "string") {
      radioButtons = [radioButtons];
    }
    if (typeof(radioButtons) == "object" && radioButtons != null) {
      radioButtons.forEach(function(col) {
        var btns = rows.selectAll('.' + col)
        .text("")
        .append("input")
        .attr("type", "radio")
        .attr("name", outputID + "_"  + col)
        .attr('class', function(d, i, j){ return col + ' ' + 'row_' + j + ' ' + 'tbl_' + outputID; })
        .property("checked", function(d, i) { return d.value; });
        // event for the radio button group
        $("input[name=" + outputID + "_"  + col + "]:radio")
        .change(function () {checkRadio(this.name)})
      })
    }
    
    // create checkboxes
    if (typeof(checkBoxes) == "string") {
      checkBoxes = [checkBoxes];
    }
    if (typeof(checkBoxes) == "object" && checkBoxes != null) {
      checkBoxes.forEach(function(col) {
        var btns = rows.selectAll('.' + col)
        .text("")
        .append("input")
        .attr("type", "checkbox")
        .attr('class', function(d, i, j){ return col + ' ' + 'row_' + j + ' ' + 'tbl_' + outputID; })
        .property("checked", function(d, i) { return d.value; })
        .on("change", shinyInputEvent);
      })
    }
    
    // run d3 functions to format cells
    runCellFunctions(outputID);
    runCellFunctions(outputID, col = null, foot = true);
    
    // set intial color. Has to run again after table sorting.
    colourCells(outputID);
    
    // apply sparkline
    setSparklines(outputID);
    
    
    //   $(function() {
      //        /** This code runs when everything has been loaded on the page */
        //        /* Inline sparklines take their values from the contents of the tag */
        //        $('.inlinesparkline').sparkline();
      //
        //    });
    
    
    // for mixed sorting give TableFilter a custom sort key generated by gtools::mixedsort
    for (var key in sortKeys) {
      if (sortKeys.hasOwnProperty(key)) {
        d3.selectAll('#' + outputID)
        .selectAll('tbody')
        .selectAll('td.' + key)
        .data(sortKeys[key])
        .attr("data-tf-sortKey", function(d) { return(d);})
      }};
    
    // initialize table filter generator
    if(data.enableTf == true) {
      //      window[tfName] = setFilterGrid(tableID, data.tableProps);
      window[tfName] = new TableFilter(tableID, data.tableProps);
      window[tfName].init();
      
      // initial filter settings
      for (var key in initialFilters) {
        if (initialFilters.hasOwnProperty(key)) {
          var col = Number(key.replace(/col_/, ''));
          window[tfName].setFilterValue(col, initialFilters[key]);
        };
      };
      window[tfName].filter();
      
      // crosstalk filter handling
      window[tfName].emitter.on(['after-filtering'], function(tf){
        console.log(tf.getValidRows());
        updateFilterInput(tf);
      });
      
    }
    
    // make thead and info row bootstrap styled
    // TODO: find a working solution for info row
    if(data.tableStyle != null) {
      thead.selectAll("tr").classed("active", true);
      var infDiv = d3.select(el)
      .selectAll("#inf_" + outputID + "_tbl")
      .classed({"active": true})
      .style("width", "auto");
    }
    
  } // end of renderValue !!
    
}); // end of HTMLWIDGET !!
  
# /htmlwidgets/TFD3.yaml -----------------------------------------------------------------------------

dependencies:
  - name: d3
version: 3.5.0
src: "lib/d3/dist"
script: d3.min.js
- name: colorbrewer
version: 1
src: "lib/colorbrewer/dist"
script: colorbrewer.js
- name: tablefilter
version: 0.4.15
src: "lib/tablefilter/dist"
script:
  - tablefilter.js
- tf-1.js
stylesheet: 
  - style/colsVisibility.css
- style/filtersVisibility.css
- style/tablefilter.css
attachment:
  - ''
- style/themes/blank.png
- style/themes/btn_clear_filters.png
- style/themes/btn_filter.png
- style/themes/btn_first_page.gif
- style/themes/btn_last_page.gif
- style/themes/btn_next_page.gif
- style/themes/btn_previous_page.gif
- style/themes/downsimple.png
- style/themes/upsimple.png
- style/themes/icn_clp.png
- style/themes/icn_exp.png
- style/themes/icn_filterActive.gif
- style/themes/icn_filter.gif
- style/themes/default/default.css
- style/themes/default/images/bg_infDiv.jpg
- style/themes/default/images/bg_th.jpg
- style/themes/default/images/btn_eraser.gif
- style/themes/default/images/btn_first_page.gif
- style/themes/default/images/btn_last_page.gif
- style/themes/default/images/btn_next_page.gif
- style/themes/default/images/btn_over_eraser.gif
- style/themes/default/images/btn_over_first_page.gif
- style/themes/default/images/btn_over_last_page.gif
- style/themes/default/images/btn_over_next_page.gif
- style/themes/default/images/btn_over_previous_page.gif
- style/themes/default/images/btn_previous_page.gif
- style/themes/default/images/img_loading.gif
- style/themes/mytheme/images/bg_headers.jpg
- style/themes/mytheme/images/bg_infDiv.jpg
- style/themes/mytheme/images/btn_filter.png
- style/themes/mytheme/images/btn_first_page.gif
- style/themes/mytheme/images/btn_last_page.gif
- style/themes/mytheme/images/btn_next_page.gif
- style/themes/mytheme/images/btn_previous_page.gif
- style/themes/mytheme/images/img_loading.gif
- style/themes/mytheme/mytheme.css
- style/themes/skyblue/images/bg_skyblue.gif
- style/themes/skyblue/images/btn_first_page.gif
- style/themes/skyblue/images/btn_last_page.gif
- style/themes/skyblue/images/btn_next_page.gif
- style/themes/skyblue/images/btn_prev_page.gif
- style/themes/skyblue/images/icn_clear_filters.png
- style/themes/skyblue/images/img_loading.gif
- style/themes/skyblue/skyblue.css
- name: jquery
version: 1.12.4
src: "lib/jquery/dist/"
script: jquery.min.js
- name: bootstrap
version: 3.3.7
src: "lib/bootstrap/dist/"
script:
  - js/bootstrap.min.js
#    - js/npm.js
stylesheet:
  #    - css/bootstrap.css.map
  - css/bootstrap.min.css
#    - css/bootstrap-theme.css.map
- css/bootstrap-theme.min.css
- name: sparklines
version: 2.1.2
src: "lib/sparklines/dist"
script: "jquery.sparkline.min.js"
stylesheet: "tooltip.css"


