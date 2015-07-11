-- |
-- Module      : Palette
-- Description : Colours for Cairo
-- Copyright   : (c) Jonatan H Sundqvist, 2015
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   : experimental
-- Portability : POSIX (not sure)
-- 
-- Jonatan H Sundqvist
-- June 11 2015
--

-- TODO | - Polymorphic colours (?)
--        -

-- SPEC | -
--        -



---------------------------------------------------------------------------------------------------
-- Pragmas
---------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}



---------------------------------------------------------------------------------------------------
-- API definition
---------------------------------------------------------------------------------------------------
module Southpaw.Picasso.Palette where



---------------------------------------------------------------------------------------------------
-- We'll need these
---------------------------------------------------------------------------------------------------
import qualified Graphics.Rendering.Cairo as Cairo



---------------------------------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------------------------------
type Colour = (Double, Double, Double, Double)



---------------------------------------------------------------------------------------------------
-- Colours
---------------------------------------------------------------------------------------------------
black  = (0, 0, 0, 1) :: Colour
white  = (1, 1, 1, 1) :: Colour

red    = (1, 0, 0, 1) :: Colour
green  = (0, 1, 0, 1) :: Colour
blue   = (0, 0, 1, 1) :: Colour

-- From http://www.avatar.se/molscript/doc/colour_names.html (June 11 2015)
aliceblue            = (0.94117600, 0.97254900, 1.00000000, 1) :: Colour
antiquewhite         = (0.98039200, 0.92156900, 0.84313700, 1) :: Colour
aquamarine           = (0.49803900, 1.00000000, 0.83137300, 1) :: Colour
azure                = (0.94117600, 1.00000000, 1.00000000, 1) :: Colour
beige                = (0.96078400, 0.96078400, 0.86274500, 1) :: Colour
bisque               = (1.00000000, 0.89411800, 0.76862700, 1) :: Colour
blanchedalmond       = (1.00000000, 0.92156900, 0.80392200, 1) :: Colour
blueviolet           = (0.54117600, 0.16862700, 0.88627500, 1) :: Colour
brown                = (0.64705900, 0.16470600, 0.16470600, 1) :: Colour
burlywood            = (0.87058800, 0.72156900, 0.52941200, 1) :: Colour
cadetblue            = (0.37254900, 0.61960800, 0.62745100, 1) :: Colour
chartreuse           = (0.49803900, 1.00000000, 0.00000000, 1) :: Colour
chocolate            = (0.82352900, 0.41176500, 0.11764700, 1) :: Colour
coral                = (1.00000000, 0.49803900, 0.31372500, 1) :: Colour
cornflowerblue       = (0.39215700, 0.58431400, 0.92941200, 1) :: Colour
cornsilk             = (1.00000000, 0.97254900, 0.86274500, 1) :: Colour
crimson              = (0.86274500, 0.07843140, 0.23529400, 1) :: Colour
cyan                 = (0.00000000, 1.00000000, 1.00000000, 1) :: Colour
darkblue             = (0.00000000, 0.00000000, 0.54509800, 1) :: Colour
darkcyan             = (0.00000000, 0.54509800, 0.54509800, 1) :: Colour
darkgoldenrod        = (0.72156900, 0.52549000, 0.04313730, 1) :: Colour
darkgray             = (0.66274500, 0.66274500, 0.66274500, 1) :: Colour
darkgreen            = (0.00000000, 0.39215700, 0.00000000, 1) :: Colour
darkgrey             = (0.66274500, 0.66274500, 0.66274500, 1) :: Colour
darkkhaki            = (0.74117600, 0.71764700, 0.41960800, 1) :: Colour
darkmagenta          = (0.54509800, 0.00000000, 0.54509800, 1) :: Colour
darkolivegreen       = (0.33333300, 0.41960800, 0.18431400, 1) :: Colour
darkorange           = (1.00000000, 0.54902000, 0.00000000, 1) :: Colour
darkorchid           = (0.60000000, 0.19607800, 0.80000000, 1) :: Colour
darkred              = (0.54509800, 0.00000000, 0.00000000, 1) :: Colour
darksalmon           = (0.91372500, 0.58823500, 0.47843100, 1) :: Colour
darkseagreen         = (0.56078400, 0.73725500, 0.56078400, 1) :: Colour
darkslateblue        = (0.28235300, 0.23921600, 0.54509800, 1) :: Colour
darkslategray        = (0.18431400, 0.30980400, 0.30980400, 1) :: Colour
darkslategrey        = (0.18431400, 0.30980400, 0.30980400, 1) :: Colour
darkturquoise        = (0.00000000, 0.80784300, 0.81960800, 1) :: Colour
darkviolet           = (0.58039200, 0.00000000, 0.82745100, 1) :: Colour
deeppink             = (1.00000000, 0.07843140, 0.57647100, 1) :: Colour
deepskyblue          = (0.00000000, 0.74902000, 1.00000000, 1) :: Colour
dimgray              = (0.41176500, 0.41176500, 0.41176500, 1) :: Colour
dimgrey              = (0.41176500, 0.41176500, 0.41176500, 1) :: Colour
dodgerblue           = (0.11764700, 0.56470600, 1.00000000, 1) :: Colour
firebrick            = (0.69803900, 0.13333300, 0.13333300, 1) :: Colour
floralwhite          = (1.00000000, 0.98039200, 0.94117600, 1) :: Colour
forestgreen          = (0.13333300, 0.54509800, 0.13333300, 1) :: Colour
gainsboro            = (0.86274500, 0.86274500, 0.86274500, 1) :: Colour
ghostwhite           = (0.97254900, 0.97254900, 1.00000000, 1) :: Colour
gold                 = (1.00000000, 0.84313700, 0.00000000, 1) :: Colour
goldenrod            = (0.85490200, 0.64705900, 0.12549000, 1) :: Colour
gray                 = (0.74509800, 0.74509800, 0.74509800, 1) :: Colour
greenyellow          = (0.67843100, 1.00000000, 0.18431400, 1) :: Colour
grey                 = (0.74509800, 0.74509800, 0.74509800, 1) :: Colour
honeydew             = (0.94117600, 1.00000000, 0.94117600, 1) :: Colour
hotpink              = (1.00000000, 0.41176500, 0.70588200, 1) :: Colour
indianred            = (0.80392200, 0.36078400, 0.36078400, 1) :: Colour
indigo               = (0.29411800, 0.00000000, 0.50980400, 1) :: Colour
ivory                = (1.00000000, 1.00000000, 0.94117600, 1) :: Colour
khaki                = (0.94117600, 0.90196100, 0.54902000, 1) :: Colour
lavender             = (0.90196100, 0.90196100, 0.98039200, 1) :: Colour
lavenderblush        = (1.00000000, 0.94117600, 0.96078400, 1) :: Colour
lawngreen            = (0.48627500, 0.98823500, 0.00000000, 1) :: Colour
lemonchiffon         = (1.00000000, 0.98039200, 0.80392200, 1) :: Colour
lightblue            = (0.67843100, 0.84705900, 0.90196100, 1) :: Colour
lightcoral           = (0.94117600, 0.50196100, 0.50196100, 1) :: Colour
lightcyan            = (0.87843100, 1.00000000, 1.00000000, 1) :: Colour
lightgoldenrod       = (0.93333300, 0.86666700, 0.50980400, 1) :: Colour
lightgoldenrodyellow = (0.98039200, 0.98039200, 0.82352900, 1) :: Colour
lightgray            = (0.82745100, 0.82745100, 0.82745100, 1) :: Colour
lightgreen           = (0.56470600, 0.93333300, 0.56470600, 1) :: Colour
lightgrey            = (0.82745100, 0.82745100, 0.82745100, 1) :: Colour
lightpink            = (1.00000000, 0.71372500, 0.75686300, 1) :: Colour
lightsalmon          = (1.00000000, 0.62745100, 0.47843100, 1) :: Colour
lightseagreen        = (0.12549000, 0.69803900, 0.66666700, 1) :: Colour
lightskyblue         = (0.52941200, 0.80784300, 0.98039200, 1) :: Colour
lightslateblue       = (0.51764700, 0.43921600, 1.00000000, 1) :: Colour
lightslategray       = (0.46666700, 0.53333300, 0.60000000, 1) :: Colour
lightslategrey       = (0.46666700, 0.53333300, 0.60000000, 1) :: Colour
lightsteelblue       = (0.69019600, 0.76862700, 0.87058800, 1) :: Colour
lightyellow          = (1.00000000, 1.00000000, 0.87843100, 1) :: Colour
limegreen            = (0.19607800, 0.80392200, 0.19607800, 1) :: Colour
linen                = (0.98039200, 0.94117600, 0.90196100, 1) :: Colour
magenta              = (1.00000000, 0.00000000, 1.00000000, 1) :: Colour
maroon               = (0.69019600, 0.18823500, 0.37647100, 1) :: Colour
mediumaquamarine     = (0.40000000, 0.80392200, 0.66666700, 1) :: Colour
mediumblue           = (0.00000000, 0.00000000, 0.80392200, 1) :: Colour
mediumorchid         = (0.72941200, 0.33333300, 0.82745100, 1) :: Colour
mediumpurple         = (0.57647100, 0.43921600, 0.85882400, 1) :: Colour
mediumseagreen       = (0.23529400, 0.70196100, 0.44313700, 1) :: Colour
mediumslateblue      = (0.48235300, 0.40784300, 0.93333300, 1) :: Colour
mediumspringgreen    = (0.00000000, 0.98039200, 0.60392200, 1) :: Colour
mediumturquoise      = (0.28235300, 0.81960800, 0.80000000, 1) :: Colour
mediumvioletred      = (0.78039200, 0.08235290, 0.52156900, 1) :: Colour
midnightblue         = (0.09803920, 0.09803920, 0.43921600, 1) :: Colour
mintcream            = (0.96078400, 1.00000000, 0.98039200, 1) :: Colour
mistyrose            = (1.00000000, 0.89411800, 0.88235300, 1) :: Colour
moccasin             = (1.00000000, 0.89411800, 0.70980400, 1) :: Colour
navajowhite          = (1.00000000, 0.87058800, 0.67843100, 1) :: Colour
navy                 = (0.00000000, 0.00000000, 0.50196100, 1) :: Colour
navyblue             = (0.00000000, 0.00000000, 0.50196100, 1) :: Colour
oldlace              = (0.99215700, 0.96078400, 0.90196100, 1) :: Colour
olivedrab            = (0.41960800, 0.55686300, 0.13725500, 1) :: Colour
orange               = (1.00000000, 0.64705900, 0.00000000, 1) :: Colour
orangered            = (1.00000000, 0.27058800, 0.00000000, 1) :: Colour
orchid               = (0.85490200, 0.43921600, 0.83921600, 1) :: Colour
palegoldenrod        = (0.93333300, 0.90980400, 0.66666700, 1) :: Colour
palegreen            = (0.59607800, 0.98431400, 0.59607800, 1) :: Colour
paleturquoise        = (0.68627500, 0.93333300, 0.93333300, 1) :: Colour
palevioletred        = (0.85882400, 0.43921600, 0.57647100, 1) :: Colour
papayawhip           = (1.00000000, 0.93725500, 0.83529400, 1) :: Colour
peachpuff            = (1.00000000, 0.85490200, 0.72549000, 1) :: Colour
peru                 = (0.80392200, 0.52156900, 0.24705900, 1) :: Colour
pink                 = (1.00000000, 0.75294100, 0.79607800, 1) :: Colour
plum                 = (0.86666700, 0.62745100, 0.86666700, 1) :: Colour
powderblue           = (0.69019600, 0.87843100, 0.90196100, 1) :: Colour
purple               = (0.62745100, 0.12549000, 0.94117600, 1) :: Colour
rosybrown            = (0.73725500, 0.56078400, 0.56078400, 1) :: Colour
royalblue            = (0.25490200, 0.41176500, 0.88235300, 1) :: Colour
saddlebrown          = (0.54509800, 0.27058800, 0.07450980, 1) :: Colour
salmon               = (0.98039200, 0.50196100, 0.44705900, 1) :: Colour
sandybrown           = (0.95686300, 0.64313700, 0.37647100, 1) :: Colour
seagreen             = (0.18039200, 0.54509800, 0.34117600, 1) :: Colour
seashell             = (1.00000000, 0.96078400, 0.93333300, 1) :: Colour
sgibeet              = (0.55686300, 0.21960800, 0.55686300, 1) :: Colour
sgibrightgray        = (0.77254900, 0.75686300, 0.66666700, 1) :: Colour
sgibrightgrey        = (0.77254900, 0.75686300, 0.66666700, 1) :: Colour
sgichartreuse        = (0.44313700, 0.77647100, 0.44313700, 1) :: Colour
sgidarkgray          = (0.33333300, 0.33333300, 0.33333300, 1) :: Colour
sgidarkgrey          = (0.33333300, 0.33333300, 0.33333300, 1) :: Colour
sgilightblue         = (0.49019600, 0.61960800, 0.75294100, 1) :: Colour
sgilightgray         = (0.66666700, 0.66666700, 0.66666700, 1) :: Colour
sgilightgrey         = (0.66666700, 0.66666700, 0.66666700, 1) :: Colour
sgimediumgray        = (0.51764700, 0.51764700, 0.51764700, 1) :: Colour
sgimediumgrey        = (0.51764700, 0.51764700, 0.51764700, 1) :: Colour
sgiolivedrab         = (0.55686300, 0.55686300, 0.21960800, 1) :: Colour
sgisalmon            = (0.77647100, 0.44313700, 0.44313700, 1) :: Colour
sgislateblue         = (0.44313700, 0.44313700, 0.77647100, 1) :: Colour
sgiteal              = (0.21960800, 0.55686300, 0.55686300, 1) :: Colour
sgiverydarkgray      = (0.15686300, 0.15686300, 0.15686300, 1) :: Colour
sgiverydarkgrey      = (0.15686300, 0.15686300, 0.15686300, 1) :: Colour
sgiverylightgray     = (0.83921600, 0.83921600, 0.83921600, 1) :: Colour
sgiverylightgrey     = (0.83921600, 0.83921600, 0.83921600, 1) :: Colour
sienna               = (0.62745100, 0.32156900, 0.17647100, 1) :: Colour
skyblue              = (0.52941200, 0.80784300, 0.92156900, 1) :: Colour
slateblue            = (0.41568600, 0.35294100, 0.80392200, 1) :: Colour
slategray            = (0.43921600, 0.50196100, 0.56470600, 1) :: Colour
slategrey            = (0.43921600, 0.50196100, 0.56470600, 1) :: Colour
snow                 = (1.00000000, 0.98039200, 0.98039200, 1) :: Colour
springgreen          = (0.00000000, 1.00000000, 0.49803900, 1) :: Colour
steelblue            = (0.27451000, 0.50980400, 0.70588200, 1) :: Colour
-- tan                  = (0.82352900, 0.70588200, 0.54902000, 1) :: Colour
thistle              = (0.84705900, 0.74902000, 0.84705900, 1) :: Colour
tomato               = (1.00000000, 0.38823500, 0.27843100, 1) :: Colour
turquoise            = (0.25098000, 0.87843100, 0.81568600, 1) :: Colour
violet               = (0.93333300, 0.50980400, 0.93333300, 1) :: Colour
violetred            = (0.81568600, 0.12549000, 0.56470600, 1) :: Colour
wheat                = (0.96078400, 0.87058800, 0.70196100, 1) :: Colour
whitesmoke           = (0.96078400, 0.96078400, 0.96078400, 1) :: Colour
yellow               = (1.00000000, 1.00000000, 0.00000000, 1) :: Colour
yellowgreen          = (0.60392200, 0.80392200, 0.19607800, 1) :: Colour



---------------------------------------------------------------------------------------------------
-- Functions
---------------------------------------------------------------------------------------------------
-- |
choose :: Colour -> Cairo.Render ()
choose (r, g, b, a) = Cairo.setSourceRGBA r g b a


-- |
blend :: Colour -> Colour -> Colour
blend (r, g, b, a) (r', g', b', a') = ((r+r')/2, (g+g')/2, (b+b')/2, (a+a')/2)


-- Lenses -----------------------------------------------------------------------------------------
-- |
-- TODO: Clamp (?)
-- TODO: Use proper lenses instead (?)
withRed   :: Colour -> (Double -> Double) -> Colour
withRed (r, g, b, a) f = (f r, g, b, a)


withGreen :: Colour -> (Double -> Double) -> Colour
withGreen (r, g, b, a) f = (r, f g, b, a)


withBlue  :: Colour -> (Double -> Double) -> Colour
withBlue  (r, g, b, a) f = (r, g, f b, a)


withAlpha :: Colour -> (Double -> Double) -> Colour
withAlpha (r, g, b, a) f = (r, g, b, f a)