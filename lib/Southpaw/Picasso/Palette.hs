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
--        - Remove Cairo dependency (separate 'back-end' modules, eg. Palette.Cairo, Palette.OpenGL) (?)
--        - Use lenses to generate 'collections' (?)

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- Pragmas
--------------------------------------------------------------------------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances               #-} -- I have no idea why I need this or what it means...
{-# LANGUAGE FlexibleContexts                #-} -- Ditto



--------------------------------------------------------------------------------------------------------------------------------------------
-- API definition
--------------------------------------------------------------------------------------------------------------------------------------------
module Southpaw.Picasso.Palette where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
-- import qualified Graphics.Rendering.Cairo as Cairo
import Control.Lens



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
type Colour f = (f, f, f, f)


-- | Lens aliases
red' :: (Field1 s t a b, Functor f) => (a -> f b) -> s -> f t
red' = _1

green' :: (Field2 s t a b, Functor f) => (a -> f b) -> s -> f t
green' = _2

blue' :: (Field3 s t a b, Functor f) => (a -> f b) -> s -> f t
blue' = _3

alpha' :: (Field4 s t a b, Functor f) => (a -> f b) -> s -> f t
alpha' = _4



--------------------------------------------------------------------------------------------------------------------------------------------
-- Colours
--------------------------------------------------------------------------------------------------------------------------------------------
black  = (0.0, 0.0, 0.0, 1.0)
white  = (1.0, 1.0, 1.0, 1.0)

red    = (1.0, 0.0, 0.0, 1.0)
green  = (0.0, 1.0, 0.0, 1.0)
blue   = (0.0, 0.0, 1.0, 1.0)

-- From http://www.avatar.se/molscript/doc/Num f => Colour f_names.html (June 11 2015)
aliceblue            = (0.94117600, 0.97254900, 1.00000000, 1.0)
antiquewhite         = (0.98039200, 0.92156900, 0.84313700, 1.0)
aquamarine           = (0.49803900, 1.00000000, 0.83137300, 1.0)
azure                = (0.94117600, 1.00000000, 1.00000000, 1.0)
beige                = (0.96078400, 0.96078400, 0.86274500, 1.0)
bisque               = (1.00000000, 0.89411800, 0.76862700, 1.0)
blanchedalmond       = (1.00000000, 0.92156900, 0.80392200, 1.0)
blueviolet           = (0.54117600, 0.16862700, 0.88627500, 1.0)
brown                = (0.64705900, 0.16470600, 0.16470600, 1.0)
burlywood            = (0.87058800, 0.72156900, 0.52941200, 1.0)
cadetblue            = (0.37254900, 0.61960800, 0.62745100, 1.0)
chartreuse           = (0.49803900, 1.00000000, 0.00000000, 1.0)
chocolate            = (0.82352900, 0.41176500, 0.11764700, 1.0)
coral                = (1.00000000, 0.49803900, 0.31372500, 1.0)
cornflowerblue       = (0.39215700, 0.58431400, 0.92941200, 1.0)
cornsilk             = (1.00000000, 0.97254900, 0.86274500, 1.0)
crimson              = (0.86274500, 0.07843140, 0.23529400, 1.0)
cyan                 = (0.00000000, 1.00000000, 1.00000000, 1.0)
darkblue             = (0.00000000, 0.00000000, 0.54509800, 1.0)
darkcyan             = (0.00000000, 0.54509800, 0.54509800, 1.0)
darkgoldenrod        = (0.72156900, 0.52549000, 0.04313730, 1.0)
darkgray             = (0.66274500, 0.66274500, 0.66274500, 1.0)
darkgreen            = (0.00000000, 0.39215700, 0.00000000, 1.0)
darkgrey             = (0.66274500, 0.66274500, 0.66274500, 1.0)
darkkhaki            = (0.74117600, 0.71764700, 0.41960800, 1.0)
darkmagenta          = (0.54509800, 0.00000000, 0.54509800, 1.0)
darkolivegreen       = (0.33333300, 0.41960800, 0.18431400, 1.0)
darkorange           = (1.00000000, 0.54902000, 0.00000000, 1.0)
darkorchid           = (0.60000000, 0.19607800, 0.80000000, 1.0)
darkred              = (0.54509800, 0.00000000, 0.00000000, 1.0)
darksalmon           = (0.91372500, 0.58823500, 0.47843100, 1.0)
darkseagreen         = (0.56078400, 0.73725500, 0.56078400, 1.0)
darkslateblue        = (0.28235300, 0.23921600, 0.54509800, 1.0)
darkslategray        = (0.18431400, 0.30980400, 0.30980400, 1.0)
darkslategrey        = (0.18431400, 0.30980400, 0.30980400, 1.0)
darkturquoise        = (0.00000000, 0.80784300, 0.81960800, 1.0)
darkviolet           = (0.58039200, 0.00000000, 0.82745100, 1.0)
deeppink             = (1.00000000, 0.07843140, 0.57647100, 1.0)
deepskyblue          = (0.00000000, 0.74902000, 1.00000000, 1.0)
dimgray              = (0.41176500, 0.41176500, 0.41176500, 1.0)
dimgrey              = (0.41176500, 0.41176500, 0.41176500, 1.0)
dodgerblue           = (0.11764700, 0.56470600, 1.00000000, 1.0)
firebrick            = (0.69803900, 0.13333300, 0.13333300, 1.0)
floralwhite          = (1.00000000, 0.98039200, 0.94117600, 1.0)
forestgreen          = (0.13333300, 0.54509800, 0.13333300, 1.0)
gainsboro            = (0.86274500, 0.86274500, 0.86274500, 1.0)
ghostwhite           = (0.97254900, 0.97254900, 1.00000000, 1.0)
gold                 = (1.00000000, 0.84313700, 0.00000000, 1.0)
goldenrod            = (0.85490200, 0.64705900, 0.12549000, 1.0)
gray                 = (0.74509800, 0.74509800, 0.74509800, 1.0)
greenyellow          = (0.67843100, 1.00000000, 0.18431400, 1.0)
grey                 = (0.74509800, 0.74509800, 0.74509800, 1.0)
honeydew             = (0.94117600, 1.00000000, 0.94117600, 1.0)
hotpink              = (1.00000000, 0.41176500, 0.70588200, 1.0)
indianred            = (0.80392200, 0.36078400, 0.36078400, 1.0)
indigo               = (0.29411800, 0.00000000, 0.50980400, 1.0)
ivory                = (1.00000000, 1.00000000, 0.94117600, 1.0)
khaki                = (0.94117600, 0.90196100, 0.54902000, 1.0)
lavender             = (0.90196100, 0.90196100, 0.98039200, 1.0)
lavenderblush        = (1.00000000, 0.94117600, 0.96078400, 1.0)
lawngreen            = (0.48627500, 0.98823500, 0.00000000, 1.0)
lemonchiffon         = (1.00000000, 0.98039200, 0.80392200, 1.0)
lightblue            = (0.67843100, 0.84705900, 0.90196100, 1.0)
lightcoral           = (0.94117600, 0.50196100, 0.50196100, 1.0)
lightcyan            = (0.87843100, 1.00000000, 1.00000000, 1.0)
lightgoldenrod       = (0.93333300, 0.86666700, 0.50980400, 1.0)
lightgoldenrodyellow = (0.98039200, 0.98039200, 0.82352900, 1.0)
lightgray            = (0.82745100, 0.82745100, 0.82745100, 1.0)
lightgreen           = (0.56470600, 0.93333300, 0.56470600, 1.0)
lightgrey            = (0.82745100, 0.82745100, 0.82745100, 1.0)
lightpink            = (1.00000000, 0.71372500, 0.75686300, 1.0)
lightsalmon          = (1.00000000, 0.62745100, 0.47843100, 1.0)
lightseagreen        = (0.12549000, 0.69803900, 0.66666700, 1.0)
lightskyblue         = (0.52941200, 0.80784300, 0.98039200, 1.0)
lightslateblue       = (0.51764700, 0.43921600, 1.00000000, 1.0)
lightslategray       = (0.46666700, 0.53333300, 0.60000000, 1.0)
lightslategrey       = (0.46666700, 0.53333300, 0.60000000, 1.0)
lightsteelblue       = (0.69019600, 0.76862700, 0.87058800, 1.0)
lightyellow          = (1.00000000, 1.00000000, 0.87843100, 1.0)
limegreen            = (0.19607800, 0.80392200, 0.19607800, 1.0)
linen                = (0.98039200, 0.94117600, 0.90196100, 1.0)
magenta              = (1.00000000, 0.00000000, 1.00000000, 1.0)
maroon               = (0.69019600, 0.18823500, 0.37647100, 1.0)
mediumaquamarine     = (0.40000000, 0.80392200, 0.66666700, 1.0)
mediumblue           = (0.00000000, 0.00000000, 0.80392200, 1.0)
mediumorchid         = (0.72941200, 0.33333300, 0.82745100, 1.0)
mediumpurple         = (0.57647100, 0.43921600, 0.85882400, 1.0)
mediumseagreen       = (0.23529400, 0.70196100, 0.44313700, 1.0)
mediumslateblue      = (0.48235300, 0.40784300, 0.93333300, 1.0)
mediumspringgreen    = (0.00000000, 0.98039200, 0.60392200, 1.0)
mediumturquoise      = (0.28235300, 0.81960800, 0.80000000, 1.0)
mediumvioletred      = (0.78039200, 0.08235290, 0.52156900, 1.0)
midnightblue         = (0.09803920, 0.09803920, 0.43921600, 1.0)
mintcream            = (0.96078400, 1.00000000, 0.98039200, 1.0)
mistyrose            = (1.00000000, 0.89411800, 0.88235300, 1.0)
moccasin             = (1.00000000, 0.89411800, 0.70980400, 1.0)
navajowhite          = (1.00000000, 0.87058800, 0.67843100, 1.0)
navy                 = (0.00000000, 0.00000000, 0.50196100, 1.0)
navyblue             = (0.00000000, 0.00000000, 0.50196100, 1.0)
oldlace              = (0.99215700, 0.96078400, 0.90196100, 1.0)
olivedrab            = (0.41960800, 0.55686300, 0.13725500, 1.0)
orange               = (1.00000000, 0.64705900, 0.00000000, 1.0)
orangered            = (1.00000000, 0.27058800, 0.00000000, 1.0)
orchid               = (0.85490200, 0.43921600, 0.83921600, 1.0)
palegoldenrod        = (0.93333300, 0.90980400, 0.66666700, 1.0)
palegreen            = (0.59607800, 0.98431400, 0.59607800, 1.0)
paleturquoise        = (0.68627500, 0.93333300, 0.93333300, 1.0)
palevioletred        = (0.85882400, 0.43921600, 0.57647100, 1.0)
papayawhip           = (1.00000000, 0.93725500, 0.83529400, 1.0)
peachpuff            = (1.00000000, 0.85490200, 0.72549000, 1.0)
peru                 = (0.80392200, 0.52156900, 0.24705900, 1.0)
pink                 = (1.00000000, 0.75294100, 0.79607800, 1.0)
plum                 = (0.86666700, 0.62745100, 0.86666700, 1.0)
powderblue           = (0.69019600, 0.87843100, 0.90196100, 1.0)
purple               = (0.62745100, 0.12549000, 0.94117600, 1.0)
rosybrown            = (0.73725500, 0.56078400, 0.56078400, 1.0)
royalblue            = (0.25490200, 0.41176500, 0.88235300, 1.0)
saddlebrown          = (0.54509800, 0.27058800, 0.07450980, 1.0)
salmon               = (0.98039200, 0.50196100, 0.44705900, 1.0)
sandybrown           = (0.95686300, 0.64313700, 0.37647100, 1.0)
seagreen             = (0.18039200, 0.54509800, 0.34117600, 1.0)
seashell             = (1.00000000, 0.96078400, 0.93333300, 1.0)
sgibeet              = (0.55686300, 0.21960800, 0.55686300, 1.0)
sgibrightgray        = (0.77254900, 0.75686300, 0.66666700, 1.0)
sgibrightgrey        = (0.77254900, 0.75686300, 0.66666700, 1.0)
sgichartreuse        = (0.44313700, 0.77647100, 0.44313700, 1.0)
sgidarkgray          = (0.33333300, 0.33333300, 0.33333300, 1.0)
sgidarkgrey          = (0.33333300, 0.33333300, 0.33333300, 1.0)
sgilightblue         = (0.49019600, 0.61960800, 0.75294100, 1.0)
sgilightgray         = (0.66666700, 0.66666700, 0.66666700, 1.0)
sgilightgrey         = (0.66666700, 0.66666700, 0.66666700, 1.0)
sgimediumgray        = (0.51764700, 0.51764700, 0.51764700, 1.0)
sgimediumgrey        = (0.51764700, 0.51764700, 0.51764700, 1.0)
sgiolivedrab         = (0.55686300, 0.55686300, 0.21960800, 1.0)
sgisalmon            = (0.77647100, 0.44313700, 0.44313700, 1.0)
sgislateblue         = (0.44313700, 0.44313700, 0.77647100, 1.0)
sgiteal              = (0.21960800, 0.55686300, 0.55686300, 1.0)
sgiverydarkgray      = (0.15686300, 0.15686300, 0.15686300, 1.0)
sgiverydarkgrey      = (0.15686300, 0.15686300, 0.15686300, 1.0)
sgiverylightgray     = (0.83921600, 0.83921600, 0.83921600, 1.0)
sgiverylightgrey     = (0.83921600, 0.83921600, 0.83921600, 1.0)
sienna               = (0.62745100, 0.32156900, 0.17647100, 1.0)
skyblue              = (0.52941200, 0.80784300, 0.92156900, 1.0)
slateblue            = (0.41568600, 0.35294100, 0.80392200, 1.0)
slategray            = (0.43921600, 0.50196100, 0.56470600, 1.0)
slategrey            = (0.43921600, 0.50196100, 0.56470600, 1.0)
snow                 = (1.00000000, 0.98039200, 0.98039200, 1.0)
springgreen          = (0.00000000, 1.00000000, 0.49803900, 1.0)
steelblue            = (0.27451000, 0.50980400, 0.70588200, 1.0)
-- tan                  = (0.82352900, 0.70588200, 0.54902000, 1.0)
thistle              = (0.84705900, 0.74902000, 0.84705900, 1.0)
tomato               = (1.00000000, 0.38823500, 0.27843100, 1.0)
turquoise            = (0.25098000, 0.87843100, 0.81568600, 1.0)
violet               = (0.93333300, 0.50980400, 0.93333300, 1.0)
violetred            = (0.81568600, 0.12549000, 0.56470600, 1.0)
wheat                = (0.96078400, 0.87058800, 0.70196100, 1.0)
whitesmoke           = (0.96078400, 0.96078400, 0.96078400, 1.0)
yellow               = (1.00000000, 1.00000000, 0.00000000, 1.0)
yellowgreen          = (0.60392200, 0.80392200, 0.19607800, 1.0)


--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- TODO: Use map (?)
every = [aliceblue,
         antiquewhite,
         aquamarine,
         azure,
         beige,
         bisque,
         blanchedalmond,
         blueviolet,
         brown,
         burlywood,
         cadetblue,
         chartreuse,
         chocolate,
         coral,
         cornflowerblue,
         cornsilk,
         crimson,
         cyan,
         darkblue,
         darkcyan,
         darkgoldenrod,
         darkgray,
         darkgreen,
         darkgrey,
         darkkhaki,
         darkmagenta,
         darkolivegreen,
         darkorange,
         darkorchid,
         darkred,
         darksalmon,
         darkseagreen,
         darkslateblue,
         darkslategray,
         darkslategrey,
         darkturquoise,
         darkviolet,
         deeppink,
         deepskyblue,
         dimgray,
         dimgrey,
         dodgerblue,
         firebrick,
         floralwhite,
         forestgreen,
         gainsboro,
         ghostwhite,
         gold,
         goldenrod,
         gray,
         greenyellow,
         grey,
         honeydew,
         hotpink,
         indianred,
         indigo,
         ivory,
         khaki,
         lavender,
         lavenderblush,
         lawngreen,
         lemonchiffon,
         lightblue,
         lightcoral,
         lightcyan,
         lightgoldenrod,
         lightgoldenrodyellow,
         lightgray,
         lightgreen,
         lightgrey,
         lightpink,
         lightsalmon,
         lightseagreen,
         lightskyblue,
         lightslateblue,
         lightslategray,
         lightslategrey,
         lightsteelblue,
         lightyellow,
         limegreen,
         linen,
         magenta,
         maroon,
         mediumaquamarine,
         mediumblue,
         mediumorchid,
         mediumpurple,
         mediumseagreen,
         mediumslateblue,
         mediumspringgreen,
         mediumturquoise,
         mediumvioletred,
         midnightblue,
         mintcream,
         mistyrose,
         moccasin,
         navajowhite,
         navy,
         navyblue,
         oldlace,
         olivedrab,
         orange,
         orangered,
         orchid,
         palegoldenrod,
         palegreen,
         paleturquoise,
         palevioletred,
         papayawhip,
         peachpuff,
         peru,
         pink,
         plum,
         powderblue,
         purple,
         rosybrown,
         royalblue,
         saddlebrown,
         salmon,
         sandybrown,
         seagreen,
         seashell,
         sgibeet,
         sgibrightgray,
         sgibrightgrey,
         sgichartreuse,
         sgidarkgray,
         sgidarkgrey,
         sgilightblue,
         sgilightgray,
         sgilightgrey,
         sgimediumgray,
         sgimediumgrey,
         sgiolivedrab,
         sgisalmon,
         sgislateblue,
         sgiteal,
         sgiverydarkgray,
         sgiverydarkgrey,
         sgiverylightgray,
         sgiverylightgrey,
         sienna,
         skyblue,
         slateblue,
         slategray,
         slategrey,
         snow,
         springgreen,
         steelblue,
         thistle,
         tomato,
         turquoise,
         violet,
         violetred,
         wheat,
         whitesmoke,
         yellow,
         yellowgreen ]



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------
-- |
-- choose :: Colour -> Cairo.Render ()
-- choose (r, g, b, a) = Cairo.setSourceRGBA r g b a


-- |
blend :: Fractional f => Colour f -> Colour f -> Colour f
blend (r, g, b, a) (r', g', b', a') = ((r+r')/2, (g+g')/2, (b+b')/2, (a+a')/2)


-- Lenses -----------------------------------------------------------------------------------------
-- |
--         (Field1  s          t         a b, Functor f) => (n -> f n) ->  s         -> f  t
redpart :: (Field1 (Colour n) (Colour n) n n, Functor f) => (n -> f n) -> Colour n -> f (Colour n)
redpart = _1


greenpart :: (Field2 (Colour n) (Colour n) n n, Functor f) => (n -> f n) -> Colour n -> f (Colour n)
-- |
greenpart = _2

-- |
bluepart :: (Field3 (Colour n) (Colour n) n n, Functor f) => (n -> f n) -> Colour n -> f (Colour n)
bluepart = _3


-- |
alphapart :: (Field4 (Colour n) (Colour n) n n, Functor f) => (n -> f n) -> Colour n -> f (Colour n)
alphapart = _4
