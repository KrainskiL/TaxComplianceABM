;Global Variables and Breed Declarations
globals [
  debug-mode  ;; Add this to your list of globals
  b1 b2 b3 b4
  q1 q2 q3 q4
  num-bars
  total-tax-collected
  cumulative-tax-collected  ; Running total of tax collected over all rounds
  data-collection-list  ;; Store data for export
  average-tax-1st
  average-tax-2nd
  average-tax-3rd
  average-tax-4th
  average-tax-1st-q
  average-tax-2nd-q
  average-tax-3rd-q
  average-tax-4th-q
  log-mean  ; Mean of the logarithm of income
  log-sd    ; Standard deviation of the logarithm of income
  neighborhood-radius  ; Radius to define the neighborhood for peer-influence
  voting-allowed   ; Voting mechanism flag
  participation-interval  ;; Interval for citizens to participate in decision-making
  fraction-disadvantaged ;; Fraction of disadvantaged population to receive extra tax in capability world
  disadvantaged-tax-share-rate ; Share of taxes to be redistributed within disadvantaged - only in capability world-preference
  match-compliance-probability ;; Base compliance for citizens with preferences matching the world
  mismatch-compliance-probability ;; Base compliance for citizens with preferences not matching the world
  random-compliance-probability ;; Base compliance for citizens without knowledge
]

breed [citizens citizen]

citizens-own [
  income
  group       ;; Bucket
  quartile    ;; Quartile
  preferences ;; Utilitarian, capability
  knowledge   ;; True, false
  compliant
  tax         ;; Define tax here
  disadvantaged  ;; True or false
  disadvantage-severity  ;; Hard, middle, soft
  preference-match  ;; True or false
  peer-influence-modifier ;; Share of compliant or non-compliant neighbours affecting peer-influence
]

to move-citizens
  ask citizens [
    let old-x xcor
    let old-y ycor

    ;; Move four patches in each direction
    set xcor xcor + one-of [4 -4]
    set ycor ycor + one-of [4 -4]

    ;; Debug print to ensure movement is only changing positions
    if debug-mode [
      print (word "Citizen ID: " who " moved from (" old-x ", " old-y ") to (" xcor ", " ycor ")")
    ]
  ]
end

;Setup and Initialization Procedures
to setup
  clear-all
  set debug-mode false ;; Ensure this matches the switch state
  set num-bars 10  ; Set the number of income brackets globally

  set total-tax-collected 0
  set cumulative-tax-collected 0  ; Initialize cumulative tax collected
  set data-collection-list []

  set world-preference world-preference ; Initialize world-preference based on switch state
  ; Fixed parameters based on calibration and expert knowledge
  set log-mean ln 30000  ; Adjusted based on empirical data
  set log-sd 0.7     ; Adjusted based on empirical data
  set neighborhood-radius 2  ;; Define the neighborhood radius
  set fraction-disadvantaged 30 ; Set based on empirical data - estimated fraction of retired and other disadvantaged population in Poland
  set disadvantaged-tax-share-rate 40 ; Set based on empirical data - estimated redistribution based on ZUS data
  set match-compliance-probability 80 ;; Increased compliance with matching preferences of citizen and world
  set mismatch-compliance-probability 20 ;; Decreased compliance with not matching preferences of citizen and world
  set random-compliance-probability 50 ;; Random compliance with no knowledge

  create-citizens num-citizens [
    setup-attributes
    setxy random-xcor random-ycor
  ]

  assign-groups
  assign-quartiles
  calculate-buckets
  calculate-quartiles
  create-histogram

  reset-ticks
end


to setup-attributes
  ;; Set income using a log-normal distribution
  set income exp random-normal log-mean log-sd
  ;; Ensure the income falls within the desired range
  if income < 1000 [set income 1000]
  if income > 100000 [set income 100000]

  ;; Randomly assign knowledge
  set knowledge random-float 1.0 < (knowledge-level / 100)

  ;; Assign preferences based on the utilitarian-fraction slider
  ifelse random 100 < utilitarian-fraction [
    set preferences "utilitarian"
  ] [
    set preferences "capability"
  ]

  ;; Assign disadvantaged status
  set disadvantaged random-float 1.0 < (fraction-disadvantaged / 100)
  if disadvantaged [
    set disadvantage-severity one-of ["hard" "middle" "soft"]
  ]

  ;; Initialize compliant status randomly
  set compliant random-float 1.0 < 0.5  ;; 50% chance to start as compliant
  set peer-influence peer-influence
end

;Bucket Calculation and Group Assignment
to calculate-buckets
  let min-income 0
  let max-income 100000

  let income_range max-income - min-income  ; Total income range
  let income-step income_range / 4  ; Divide range by 4 for buckets steps

  ;; Set buckets thresholds based on fixed steps
  set b1 min-income + income-step  ; 1st bucket
  set b2 min-income + 2 * income-step  ; 2nd bucket
  set b3 min-income + 3 * income-step  ; 3rd bucket
  set b4 max-income ; 4th bucket includes everything up to max income
end

to-report upper-quartile [ xs ]
  let med median xs
  let upper filter [ x -> x > med ] xs
  report ifelse-value empty? upper [ med ] [ median upper ]
end

to-report lower-quartile [ xs ]
  let med median xs
  let lower filter [ x -> x < med ] xs
  report ifelse-value empty? lower [ med ] [ median lower ]
end

;Quartiles calculation
to calculate-quartiles
  let incomes [income] of citizens
  set q1 lower-quartile incomes
  set q2 median incomes
  set q3 upper-quartile incomes
end

to assign-groups
  ask citizens [
    set group calculate-new-group income
  ]
end

to assign-quartiles
  ask citizens [
    set group calculate-new-quartile income
  ]
end

to-report calculate-new-group [current_income]
  ifelse current_income > b4 [
    report "4th bucket"
  ] [
    ifelse current_income > b3 [
      report "3rd bucket"
    ] [
      ifelse current_income > b2 [
        report "2nd bucket"
      ] [
          report "1st bucket"
      ]
    ]
  ]
end

to-report calculate-new-quartile [current_income]
  ifelse current_income > q3 [
    report "4th quartile"
  ] [
    ifelse current_income > q2 [
      report "3rd quartile"
    ] [
      ifelse current_income > q1 [
        report "2nd quartile"
      ] [
          report "1st quartile"
      ]
    ]
  ]
end

;Histogram Creation and Updating
to create-histogram
  ; Clear the display
  ask patches [ set pcolor white ]

  let max-income max [income] of citizens

  let income-step max-income / num-bars

  ; Calculate maximum count for scaling the histogram height
  let max-count max (map [i -> count citizens with [income >= i * income-step and income < (i + 1) * income-step]] (range num-bars))

  foreach (range num-bars) [
    i ->
    let left-bound (- (world-width / 2) + (world-width / num-bars) * i)
    let right-bound left-bound + (world-width / num-bars) - 1
    let income-low i * income-step
    let income-high (i + 1) * income-step
    let income-count count citizens with [income >= income-low and income < income-high]

    ; Calculate the number of patches to color vertically based on the count
    let patches-to-color floor ((income-count / max-count) * world-height)

    ask patches with [pxcor >= left-bound and pxcor < right-bound] [
      if (pycor < patches-to-color - (world-height / 2)) [  ; Adjust pycor to start coloring from the bottom
        set pcolor red
      ]
    ]
  ]
end


to update-histogram
  ask patches [ set pcolor white ]  ; Clear previous display

  let max-income max [income] of citizens
  let income-step max-income / num-bars

  let max-count max (map [i -> count citizens with [income >= i * income-step and income < (i + 1) * income-step]] (range num-bars))

  foreach (range num-bars) [
    i ->
    let left-bound (- (world-width / 2) + (world-width / num-bars) * i)
    let right-bound left-bound + (world-width / num-bars) - 1
    let income-low i * income-step
    let income-high (i + 1) * income-step
    let income-count count citizens with [income >= income-low and income < income-high]

    let patches-to-color floor ((income-count / max-count) * world-height)

    ask patches with [pxcor >= left-bound and pxcor < right-bound] [
      if (pycor < patches-to-color - (world-height / 2)) [
        set pcolor red
      ]
    ]
  ]
end


to display-settings
  ask patches [
    ; Set a very narrow strip as the separator
    if (pxcor mod (world-width / num-bars) = 0) [set pcolor black]
  ]
end

;Main Simulation Loop
to go
  if ticks >= num-rounds [
    ;export-data-to-csv
    stop
  ]

  let current-gini calculate-gini
  if debug-mode [
    print (word "Current Gini Coefficient: " current-gini)
  ]

  ;; Move citizens once per tick
  move-citizens
  calculate-quartiles

  ask citizens [
    let new-group calculate-new-group income ; Determines new group based on model logic
    let new-quartile calculate-new-quartile income ; Determines new quartile based on model logic
    if new-group != group [
      if debug-mode [
        print (word "Citizen " who " moved from " group " to " new-group)
      ]
      set group new-group
      assign-colors  ; Update color based on the new group
    ]
    if new-quartile != quartile [
      if debug-mode [
        print (word "Citizen " who " moved from " quartile " to " new-quartile)
      ]
      set quartile new-quartile
    ]
  ]

  calculate-tax
  collect-taxes
  calculate-compliance
  update-income
  ;data collection and visualization
  collect-data
  update-tax-averages
  update-tax-averages-quartiles
  update-histogram
  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Tax Calculation and Compliance
to calculate-compliance
  let current-preference ifelse-value world-preference ["utilitarian"] ["capability"]
  ask citizens [
    let local-neighbors citizens in-radius neighborhood-radius
    let compliant-neighbors count local-neighbors with [compliant]
    let noncompliant-neighbors count local-neighbors with [not compliant]
    let total-neighbors count local-neighbors

    ;; Calculate the fraction of compliant and noncompliant neighbors
    let compliant-fraction ifelse-value (total-neighbors > 0) [compliant-neighbors / total-neighbors] [0]
    let noncompliant-fraction ifelse-value (total-neighbors > 0) [noncompliant-neighbors / total-neighbors] [0]

    ;; Debug prints for peer pressure calculations
    if debug-mode [
      print (word "Citizen ID: " who " Compliant Neighbors: " compliant-neighbors " Noncompliant Neighbors: " noncompliant-neighbors " Total Neighbors: " total-neighbors)
      print (word "Compliant Fraction: " compliant-fraction " Noncompliant Fraction: " noncompliant-fraction)
    ]

    ;; Initialize base compliance probability
    let base-compliance-probability 0
    ifelse knowledge [
      ifelse preferences = current-preference [
        set base-compliance-probability match-compliance-probability / 100
      ][
        set base-compliance-probability mismatch-compliance-probability / 100
      ]
    ][
      set base-compliance-probability random-compliance-probability / 100
    ]

    ;; Apply peer influence
    let peer-influence-compliance 0
    let peer-influence-noncompliance 0
    set peer-influence-modifier 0
    if compliant-neighbors > noncompliant-neighbors [
      set peer-influence-modifier compliant-fraction
      set peer-influence-compliance peer-influence / 100 * peer-influence-modifier
    ]
    if noncompliant-neighbors > compliant-neighbors [
      set peer-influence-modifier noncompliant-fraction
      set peer-influence-noncompliance peer-influence / 100 * peer-influence-modifier
    ]

    let final-compliance-probability base-compliance-probability + peer-influence-compliance - peer-influence-noncompliance
    set final-compliance-probability max list 0 (min list 1 final-compliance-probability)

    ;; Ensure compliance is a boolean
    ifelse random-float 1.0 < final-compliance-probability [
      set compliant true
    ][
      set compliant false
    ]

    ;; Final compliance debug print
    if debug-mode [
      print (word "Citizen ID: " who " Final Compliance: " compliant)
    ]
  ]
end


to-report compliance-rate
  if count citizens = 0 [report 0]  ;; Handle the case where there are no citizens to avoid division by zero
  report count citizens with [compliant] / count citizens
end

to calculate-tax
  ask citizens [
    set tax 0
    if compliant [
        set tax income * 0.1
    ]
  ]
end

to collect-taxes
  set total-tax-collected sum [tax] of citizens
  set cumulative-tax-collected cumulative-tax-collected + (total-tax-collected / 1000000)
end

to reset-taxes
  ask citizens [
    set tax 0  ;; Reset tax to zero initially every tick
  ]
end

;Income Redistribution
to update-income
  ;; Deduct tax from income first
  ask citizens [
    set income income - tax
  ]

  ;; Sum up the taxes collected
  set total-tax-collected sum [tax] of citizens

  ;; Redistribute the collected taxes based on world preference
  ifelse world-preference [
    redistribute-utilitarian
  ] [
    redistribute-capability
  ]
end


to redistribute-capability
  let total-weights sum (map [severity ->
    ifelse-value (severity = "hard") [3]
    [ifelse-value (severity = "middle") [2]
    [ifelse-value (severity = "soft") [1]
        [0]]]] [disadvantage-severity] of citizens)

  let base-share total-tax-collected * (1 - disadvantaged-tax-share-rate / 100) / count citizens
  let additional-tax-collected total-tax-collected * disadvantaged-tax-share-rate / 100  ;; disadvantaged-tax-share-rate of the collected tax goes into weighted distribution

  ifelse total-weights > 0 [
    ask citizens [
      let severity-weight ifelse-value (disadvantage-severity = "hard") [3]
                           [ifelse-value (disadvantage-severity = "middle") [2]
                              [ifelse-value (disadvantage-severity = "soft") [1]
                              [0]]]
      let tax-share (severity-weight / total-weights) * additional-tax-collected
      set income income + base-share + tax-share
    ]
  ][
    ask citizens [
      set income income + base-share + (additional-tax-collected / count citizens)
    ]
  ]
end

to redistribute-utilitarian
  let total-income sum [income] of citizens
  let average-income total-income / count citizens

  let total-taxes-collected sum [tax] of citizens
  let redistribution-share total-taxes-collected / count citizens

  ask citizens [
    if income < average-income [
      let deficit-ratio (average-income - income) / average-income
      let adjustment redistribution-share * (1 + deficit-ratio)
      set income income + adjustment
    ]
  ]

  let actual-redistributed sum [income] of citizens - total-income
  let correction (total-taxes-collected - actual-redistributed) / count citizens

  ask citizens [
    set income income + correction
  ]
end

;Data Collection and Export
to collect-data
  let current-preference ifelse-value world-preference ["utilitarian"] ["capability"]
  ask citizens [
    set preference-match (preferences = current-preference)
    let citizen-data (list ticks who group income tax compliant knowledge disadvantaged disadvantage-severity preference-match quartile)
    set data-collection-list lput citizen-data data-collection-list
  ]
end


;Additional Functions
to assign-colors
  ask citizens [
    ifelse group = "4th bucket" [
      set color green  ; Highest earners
    ] [ ifelse group = "3rd bucket" [
      set color blue
    ] [ ifelse group = "2nd bucket" [
      set color yellow
    ] [
       set color orange ; Lowest earners
    ]]]
  ]
end

to-report calculate-gini
  let incomes sort [income] of citizens
  let n length incomes
  let total-income sum incomes
  let cumulative-income 0
  let lorenz-sum 0

  foreach incomes [each-income ->
    set cumulative-income cumulative-income + each-income
    set lorenz-sum lorenz-sum + (cumulative-income / total-income)
  ]

  let normalized-lorenz-sum lorenz-sum / n
  report 1 - 2 * normalized-lorenz-sum
end

to update-tax-averages
  set average-tax-1st calculate-average-for-group "1st bucket"
  set average-tax-2nd calculate-average-for-group "2nd bucket"
  set average-tax-3rd calculate-average-for-group "3rd bucket"
  set average-tax-4th calculate-average-for-group "4th bucket"
end

to update-tax-averages-quartiles
  set average-tax-1st-q calculate-average-for-quartile "1st quartile"
  set average-tax-2nd-q calculate-average-for-quartile "2nd quartile"
  set average-tax-3rd-q calculate-average-for-quartile "3rd quartile"
  set average-tax-4th-q calculate-average-for-quartile "4th quartile"
end

to-report calculate-average-for-group [grp]
  let grp-taxes [tax] of citizens with [group = grp]
  ifelse not empty? grp-taxes [
    report mean grp-taxes
  ] [
    report 0
  ]
end

to-report calculate-average-for-quartile [q]
  let q-taxes [tax] of citizens with [quartile = q]
  ifelse not empty? q-taxes [
    report mean q-taxes
  ] [
    report 0
  ]
end

to update-plot-average-tax
  set-current-plot "Average Tax by Income Group"

  set-current-plot-pen "1st Bucket"
  plot average-tax-1st

  set-current-plot-pen "2nd Bucket"
  plot average-tax-2nd

  set-current-plot-pen "3rd Bucket"
  plot average-tax-3rd

  set-current-plot-pen "4th Bucket"
  plot average-tax-4th
end

to update-plot-average-tax-quartile
  set-current-plot "Average Tax by Income Quartile"

  set-current-plot-pen "1st Quartile"
  plot average-tax-1st-q

  set-current-plot-pen "2nd Quartile"
  plot average-tax-2nd-q

  set-current-plot-pen "3rd Quartile"
  plot average-tax-3rd-q

  set-current-plot-pen "4th Quartile"
  plot average-tax-4th-q
end
@#$#@#$#@
GRAPHICS-WINDOW
26
30
275
280
-1
-1
5.9
1
10
1
1
1
0
1
1
1
-20
20
-20
20
0
0
1
ticks
30.0

BUTTON
305
210
396
243
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
430
210
521
243
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
535
70
766
103
utilitarian-fraction
utilitarian-fraction
0
100
50.0
10
1
NIL
HORIZONTAL

SLIDER
305
120
519
153
num-rounds
num-rounds
0
1000
150.0
10
1
NIL
HORIZONTAL

SLIDER
535
115
766
148
knowledge-level
knowledge-level
0
100
50.0
10
1
NIL
HORIZONTAL

PLOT
25
305
395
507
Compliant Taxpayers
Ticks
Citizens
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -16777216 true "" "plot count citizens with [compliant = true]"

PLOT
790
27
1270
290
Average Tax by Income Group
Ticks
Taxes
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"1st Bucket" 1.0 0 -2674135 true "" "plotxy ticks  average-tax-1st"
"2nd Bucket" 1.0 0 -955883 true "" "plotxy ticks average-tax-2nd"
"3rd Bucket" 1.0 0 -1184463 true "" "plotxy ticks average-tax-3rd"
"4th Bucket" 1.0 0 -13345367 true "" "plotxy ticks average-tax-4th"

PLOT
405
305
765
506
Taxes Collected
Ticks
Taxes
0.0
100.0
0.0
100.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-tax-collected"

SLIDER
305
165
520
198
num-citizens
num-citizens
0
100
100.0
1
1
NIL
HORIZONTAL

MONITOR
1275
30
1480
75
Cumulative Taxes Collected (in millions)
cumulative-tax-collected
6
1
11

MONITOR
790
305
840
350
B1
count citizens with [group = \"1st bucket\"]
17
1
11

MONITOR
910
305
960
350
B2
count citizens with [group = \"2nd bucket\"]
17
1
11

MONITOR
1030
305
1080
350
B3
count citizens with [group = \"3rd bucket\"]
17
1
11

MONITOR
685
255
765
300
Hard Severity
count citizens with [disadvantage-severity = \"hard\"]
17
1
11

MONITOR
595
255
685
300
Middle Severity
count citizens with [disadvantage-severity = \"middle\"]
17
1
11

MONITOR
520
255
595
300
Soft Severity
count citizens with [disadvantage-severity = \"soft\"]
17
1
11

SWITCH
305
70
520
103
world-preference
world-preference
1
1
-1000

MONITOR
1275
145
1357
190
Current GINI
calculate-gini
2
1
11

TEXTBOX
335
50
507
78
ON: Utilitarian | OFF: Capability
11
0.0
1

MONITOR
1150
305
1200
350
B4
count citizens with [group = \"4th bucket\"]
17
1
11

MONITOR
1275
200
1357
245
Compliance %
compliance-rate
2
1
11

SLIDER
535
165
765
198
peer-influence
peer-influence
0
100
50.0
1
1
NIL
HORIZONTAL

PLOT
790
365
1270
625
Average Tax by Income Quartile
NIL
Taxes
0.0
100.0
0.0
100.0
true
true
"" ""
PENS
"1st Quartile" 1.0 0 -2674135 true "" "plotxy ticks  average-tax-1st-q"
"2nd Quartile" 1.0 0 -955883 true "" "plotxy ticks  average-tax-2nd-q"
"3rd Quartile" 1.0 0 -1184463 true "" "plotxy ticks  average-tax-3rd-q"
"4th Quartile" 1.0 0 -13345367 true "" "plotxy ticks  average-tax-4th-q"

MONITOR
790
640
840
685
Q1
count citizens with [quartile = \"1st quartile\"]
17
1
11

MONITOR
910
640
960
685
Q2
count citizens with [quartile = \"2nd quartile\"]
17
1
11

MONITOR
1030
640
1080
685
Q3
count citizens with [quartile = \"3rd quartile\"]
17
1
11

MONITOR
1150
640
1200
685
Q4
count citizens with [quartile = \"4th quartile\"]
17
1
11

MONITOR
865
690
935
735
Q1 Val
q1
0
1
11

MONITOR
1000
690
1070
735
Q2 Val
q2
0
1
11

MONITOR
1130
690
1200
735
Q3 Val
q3
0
1
11

MONITOR
1275
85
1355
130
Sum of income
sum [income] of citizens
0
1
11

MONITOR
840
305
910
350
B1 Comp %
count citizens with [group = \"1st bucket\" and compliant] / count citizens with [group = \"1st bucket\"]
2
1
11

MONITOR
960
305
1030
350
B2 Comp %
count citizens with [group = \"2nd bucket\" and compliant] / count citizens with [group = \"2nd bucket\"]
2
1
11

MONITOR
1080
305
1150
350
B3 Comp %
count citizens with [group = \"3rd bucket\" and compliant] / count citizens with [group = \"3rd bucket\"]
2
1
11

MONITOR
1200
305
1270
350
B4 Comp %
count citizens with [group = \"4th bucket\" and compliant] / count citizens with [group = \"4th bucket\"]
2
1
11

MONITOR
840
640
910
685
Q1 Comp %
count citizens with [quartile = \"1st quartile\" and compliant] / count citizens with [quartile = \"1st quartile\"]
2
1
11

MONITOR
960
640
1030
685
Q2 Comp %
count citizens with [quartile = \"2nd quartile\" and compliant] / count citizens with [quartile = \"2nd quartile\"]
2
1
11

MONITOR
1080
640
1150
685
Q3 Comp %
count citizens with [quartile = \"3rd quartile\" and compliant] / count citizens with [quartile = \"3rd quartile\"]
2
1
11

MONITOR
1200
640
1270
685
Q4 Comp %
count citizens with [quartile = \"4th quartile\" and compliant] / count citizens with [quartile = \"4th quartile\"]
2
1
11

PLOT
25
515
395
725
Gini Index
Ticks
Gini index
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot calculate-gini"

PLOT
405
515
765
725
Average ratio of peer influence modifier
Ticks
Ratio
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [peer-influence-modifier] of citizens"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="20" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>compliance-rate</metric>
    <metric>calculate-gini</metric>
    <metric>cumulative-tax-collected</metric>
    <metric>count citizens with [preferences = "utilitarian"]</metric>
    <runMetricsCondition>ticks mod 150 = 0</runMetricsCondition>
    <enumeratedValueSet variable="participation-rate">
      <value value="0.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="world-preference">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="utilitarian-fraction" first="0" step="20" last="100"/>
    <steppedValueSet variable="peer-influence" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="num-citizens">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax-regime">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="knowledge-level" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="num-rounds">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="participation-compliance-boost">
      <value value="0.15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment_2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>compliance-rate</metric>
    <metric>calculate-gini</metric>
    <metric>cumulative-tax-collected</metric>
    <runMetricsCondition>ticks mod 150 = 0</runMetricsCondition>
    <enumeratedValueSet variable="world-preference">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="utilitarian-fraction">
      <value value="0"/>
      <value value="100"/>
    </enumeratedValueSet>
    <steppedValueSet variable="peer-influence" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="num-citizens">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tax-regime">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="knowledge-level" first="0" step="10" last="100"/>
    <enumeratedValueSet variable="num-rounds">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
