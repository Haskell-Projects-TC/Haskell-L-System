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
  | chr == 'F' = (((x + cos (thetarad), (y + sin (thetarad))), theta)
  where
    ((x, y), theta)  = turtlestate
    theta * pi / 180 = thetarad
  

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1
trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 (chr : chrs) theta colour
  = error "TODO: implement trace1"
    where
      trace1'
        |  
        |

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 (chr : chrs) theta colour
  = [((x,y), (x',y'), colour)]
  where
    (top : stack) = ((0, 0), 90) : []
    trace2' :: String -> Stack -> 
    trace2' (chr : chrs) (top : stack)  
      | chr == '[' = newTS : (top : stack) 
      | chr == ']' = stack
      | otherwise  = 
        where
          newTS@((x', y'), theta') = move chr top theta  
      
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
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
