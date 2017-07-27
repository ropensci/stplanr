stages = c("Trip generation (1)", "Distribution (2)", "Modal split (3)", "Assignment (4)")
grViz("
digraph nicegraph {

      # graph, node, and edge definitions
      graph [compound = true, nodesep = .5, ranksep = .25,
      color = crimson]

      node [fontname = Helvetica, fontcolor = darkslategray,
      shape = rectangle, fixedsize = true, width = 1,
      color = darkslategray]

      edge [color = grey, arrowhead = none, arrowtail = none]

      # subgraph for R information
      subgraph cluster0 {
      node [fixedsize = true, width = 3]
      '@@1-1' -> '@@1-2' -> '@@1-3' -> '@@1-4'
      '@@1-4'
      }


      'Raw data'             [width = 1.5, shape = circle]
      'Preprocess (0)' [width = 3]
      'Raw data' -> 'Preprocess (0)'
      'Preprocess (0)' -> '@@1-1'            [lhead = cluster0]
      'Communicate (6)' [width = 2]

      'Validate (5)' [width = 2]
      '@@1-1' -> 'Communicate (6)'
      '@@1-2' -> 'Communicate (6)'
      '@@1-3' -> 'Communicate (6)'
      '@@1-4' -> 'Communicate (6)'

      }

      [1]: stages

      ")
