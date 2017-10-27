module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (angle, _, _) 
  = angle

-- |Returns the base string for the given system.
base :: System -> String
base (_, string, _)
  = string

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, rules)
  = rules

-- |Look up a character in the given set of rules.
-- could be done using higher order functions. chose to use list 
-- comprehension in this case for some diversity.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar chr rules 
  = concat [x | (chr', x)  <- rules, chr' == chr ]

-- |Expand a command once using the given set of rules.
-- can use flip lookupChar isntead of ``
expandOne :: Rules -> String -> String
expandOne rules str = concat (map (flip lookupChar rules) str)   

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand rules str n 
  | n == 0    = str
  | otherwise = (expandOne rules (expand rules str (n - 1)))

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move chr turtlestate theta'
  | chr == 'L' = ((x, y), theta + theta')
  | chr == 'R' = ((x, y), theta - theta')
  | chr == 'F' = (((x + cos (thetarad), (y + sin (thetarad))), theta))
  where
    ((x, y), theta)  = turtlestate
    thetarad         = theta * (pi / 180)
  

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 commands theta colour
  = fst (trace1' commands initialTS)
  where
    initialTS = ((0, 0), 90)
    trace1' :: String -> TurtleState -> ([ColouredLine], String)
    trace1' "" _
      = ([], "")
    trace1' (chr : chrs) turtlestate@(vector, _) 
      | chr == '[' = (cLine ++ cLine', str')
      | chr == ']' = ([], chrs)
      | chr == 'F' = ((vector, vector', colour) : cLine'', str'')
      | otherwise  = trace1' chrs newTS
      where
        newTS@(vector', theta')    = move chr turtlestate theta
        (cLine, str)               = trace1' chrs turtlestate
        (cLine', str')             = trace1' str turtlestate
        (cLine'', str'')           = trace1' chrs newTS

        
trace1cust:: String -> Float -> Colour -> [ColouredLine]
trace1cust commands theta colour
  = fst (trace1' commands initialTS colour)
  where
    initialTS = ((0, 0), 90)
    trace1' :: String -> TurtleState -> Colour -> ([ColouredLine], String)
    trace1' "" _  _
      = ([], "")
    trace1' (chr : chrs) turtlestate@(vector, _) colour@(r, g, b)
      | chr == '[' = (cLine ++ cLine', str')
      | chr == ']' = ([], chrs)
      | chr == 'F' = ((vector, vector', colour) : cLine'', str'')
      | otherwise  = trace1' chrs newTS colour
      where
        newTS@(vector', theta')    = move chr turtlestate theta
        (cLine, str)               = trace1' chrs turtlestate (b, r, g)
        (cLine', str')             = trace1' str turtlestate (b, r, g)
        (cLine'', str'')           = trace1' chrs newTS colour
 
-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 commands theta colour
  = trace2' commands initialTS
  where
    initialTS@(top : stack) = ((0, 0), 90) : []
    trace2' :: String -> Stack -> [ColouredLine]
    trace2' "" _
      = []  
    trace2' (chr : chrs) tss@(top : stack)  
      | chr == '[' = trace2' chrs (top : tss)
      | chr == ']' = trace2' chrs stack
      | chr == 'F' = ((x, y), (x', y'), colour) : trace2' chrs (newTS : stack)
      | otherwise  = trace2' chrs (newTS : stack)
        where
          newTS@((x', y'), _) = move chr top theta
          ((x, y), _)         = top
      
--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1cust (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
