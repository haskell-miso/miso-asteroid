----------------------------------------------------------------------------
-- Asteroids in Miso
-- https://github.com/haskell-miso/miso-asteroid
-- Controls: ← → rotate  ↑ thrust  SPACE shoot  R restart
----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE RecordWildCards   #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad      (forever)
import           Data.IntSet        (IntSet)
import qualified Data.IntSet        as IS
import           Data.List          (foldl')
----------------------------------------------------------------------------
import           Miso
import qualified Miso.Html          as H
import qualified Miso.Html.Property as HP
import qualified Miso.CSS           as CSS
import qualified Miso.Svg.Element   as SE
import qualified Miso.Svg.Property  as SP
import           Miso.Lens
import           Miso.Reload
import           Miso.String        (MisoString, ms)
----------------------------------------------------------------------------
-- Screen / constants
----------------------------------------------------------------------------
sw, sh :: Double
sw = 800
sh = 600

shipSize  :: Double; shipSize  = 14
bulletSpd :: Double; bulletSpd = 9
bulletMax :: Int;    bulletMax = 50

-- Key codes
kLeft, kRight, kUp, kSpace, kR :: Int
kLeft  = 37; kRight = 39; kUp = 38; kSpace = 32; kR = 82
----------------------------------------------------------------------------
-- Domain types
----------------------------------------------------------------------------
data GameState = Playing | Dead | Victory deriving (Show, Eq)

data Ship = Ship
  { _sx    :: !Double   -- x position
  , _sy    :: !Double   -- y position
  , _svx   :: !Double   -- x velocity
  , _svy   :: !Double   -- y velocity
  , _sang  :: !Double   -- angle in radians (0 = pointing up)
  } deriving (Show, Eq)

data Asteroid = Asteroid
  { _ax  :: !Double
  , _ay  :: !Double
  , _avx :: !Double
  , _avy :: !Double
  , _asz :: !Int      -- size: 3 large, 2 medium, 1 small
  , _aid :: !Int
  } deriving (Show, Eq)

data Bullet = Bullet
  { _bx   :: !Double
  , _by   :: !Double
  , _bvx  :: !Double
  , _bvy  :: !Double
  , _bttl :: !Int     -- time to live (frames)
  } deriving (Show, Eq)

data Model = Model
  { _ship       :: Ship
  , _asts       :: [Asteroid]
  , _buls       :: [Bullet]
  , _keys       :: IntSet
  , _score      :: Int
  , _lives      :: Int
  , _gs         :: GameState
  , _nextId     :: Int
  , _cooldown   :: Int    -- shoot cooldown
  , _invincible :: Int    -- invincibility frames after being hit
  } deriving (Show, Eq)
----------------------------------------------------------------------------
-- Lenses
----------------------------------------------------------------------------
ship_      :: Lens Model Ship;       ship_      = lens _ship       (\r v -> r { _ship       = v })
asts_      :: Lens Model [Asteroid]; asts_      = lens _asts       (\r v -> r { _asts       = v })
buls_      :: Lens Model [Bullet];   buls_      = lens _buls       (\r v -> r { _buls       = v })
keys_      :: Lens Model IntSet;     keys_      = lens _keys       (\r v -> r { _keys       = v })
score_     :: Lens Model Int;        score_     = lens _score      (\r v -> r { _score      = v })
lives_     :: Lens Model Int;        lives_     = lens _lives      (\r v -> r { _lives      = v })
gs_        :: Lens Model GameState;  gs_        = lens _gs         (\r v -> r { _gs         = v })
nextId_    :: Lens Model Int;        nextId_    = lens _nextId     (\r v -> r { _nextId     = v })
cooldown_  :: Lens Model Int;        cooldown_  = lens _cooldown   (\r v -> r { _cooldown   = v })
invincible_:: Lens Model Int;        invincible_= lens _invincible (\r v -> r { _invincible = v })
----------------------------------------------------------------------------
-- Initial state
----------------------------------------------------------------------------
initShip :: Ship
initShip = Ship (sw/2) (sh/2) 0 0 0

initAsts :: [Asteroid]
initAsts =
  [ Asteroid  100  120  1.4   0.9  3 0
  , Asteroid  680   90 (-1.2) 1.1  3 1
  , Asteroid  200  480  1.0  (-1.3) 3 2
  , Asteroid  600  440 (-1.7) 0.6  3 3
  , Asteroid  380  150  1.3  (-1.0) 3 4
  ]

initModel :: Model
initModel = Model
  { _ship       = initShip
  , _asts       = initAsts
  , _buls       = []
  , _keys       = IS.empty
  , _score      = 0
  , _lives      = 3
  , _gs         = Playing
  , _nextId     = 10
  , _cooldown   = 0
  , _invincible = 0
  }
----------------------------------------------------------------------------
-- Entry point
----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = live defaultEvents app
#else
main = startApp defaultEvents app
#endif

-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

app :: App Model Action
app = (component initModel updateModel viewModel)
        { subs = [ keyboardSub KeyChange, tickSub ] }

tickSub :: Sub Action
tickSub sink = forever $ do
  threadDelay 16667   -- ~60 fps
  sink Tick
----------------------------------------------------------------------------
-- Actions
----------------------------------------------------------------------------
data Action
  = Tick
  | KeyChange IntSet
  deriving (Show, Eq)
----------------------------------------------------------------------------
-- Update
----------------------------------------------------------------------------
updateModel :: Action -> Effect parent Model Action
updateModel = \case
  KeyChange ks -> keys_ .= ks
  Tick         -> modify step

step :: Model -> Model
step m@Model{..} =
  case _gs of
    Dead    | IS.member kR _keys -> initModel
    Victory | IS.member kR _keys -> initModel
    Dead    -> m
    Victory -> m
    Playing ->
      let
        -- Rotate
        rotSpd = 0.065
        ang' = _sang _ship
               + (if IS.member kLeft  _keys then -rotSpd else 0)
               + (if IS.member kRight _keys then  rotSpd else 0)

        -- Thrust
        thrust = IS.member kUp _keys
        ax = if thrust then  sin ang' * 0.22 else 0
        ay = if thrust then -cos ang' * 0.22 else 0
        drag = 0.985
        vx' = (_svx _ship + ax) * drag
        vy' = (_svy _ship + ay) * drag

        -- Move ship (wrap at edges)
        sx' = wrapD sw (_sx _ship + vx')
        sy' = wrapD sh (_sy _ship + vy')
        ship' = Ship sx' sy' vx' vy' ang'

        -- Shoot
        fire = IS.member kSpace _keys && _cooldown == 0
        nbul = Bullet
                 { _bx  = sx' + sin ang' * shipSize
                 , _by  = sy' - cos ang' * shipSize
                 , _bvx = vx' + sin ang' * bulletSpd
                 , _bvy = vy' - cos ang' * bulletSpd
                 , _bttl = bulletMax
                 }
        buls' = (if fire then (nbul :) else id)
                  [ b { _bx  = wrapD sw (_bx b + _bvx b)
                      , _by  = wrapD sh (_by b + _bvy b)
                      , _bttl = _bttl b - 1 }
                  | b <- _buls, _bttl b > 1 ]
        cd' = if fire then 12 else max 0 (_cooldown - 1)

        -- Move asteroids (wrap at edges)
        asts' = [ a { _ax = wrapD sw (_ax a + _avx a)
                    , _ay = wrapD sh (_ay a + _avy a) }
                | a <- _asts ]

        -- Bullet-asteroid collisions
        (asts'', buls'', scoreGain, nid') =
          bulletCollisions asts' buls' _nextId 0

        -- Ship-asteroid collision
        shipHit = _invincible == 0 &&
                  any (\a -> dist2 (sx', sy') (_ax a, _ay a)
                             < (astRadius (_asz a) + 8) ^ (2 :: Int))
                      asts''

        lives'  = if shipHit then _lives - 1 else _lives
        gs'     | shipHit && lives' <= 0 = Dead
                | null asts''            = Victory
                | otherwise              = Playing
        ship''  = if shipHit then initShip else ship'
        inv'    = if shipHit then 120 else max 0 (_invincible - 1)

      in m { _ship       = ship''
           , _asts       = asts''
           , _buls       = buls''
           , _score      = _score + scoreGain
           , _cooldown   = cd'
           , _nextId     = nid'
           , _lives      = lives'
           , _gs         = gs'
           , _invincible = inv'
           }

-- | Fold bullets through all asteroids, collecting hits.
bulletCollisions
  :: [Asteroid] -> [Bullet] -> Int -> Int
  -> ([Asteroid], [Bullet], Int, Int)
bulletCollisions asts0 buls0 nid0 sc0 =
  foldl' go (asts0, [], sc0, nid0) buls0
  where
    go (as, bs, sc, nid) b =
      case hitAsteroid as (_bx b, _by b) nid of
        Nothing           -> (as, b:bs, sc, nid)
        Just (as', p, n') -> (as', bs, sc+p, n')

-- | Destroy one asteroid overlapping the given position, spawning children.
hitAsteroid
  :: [Asteroid] -> (Double, Double) -> Int
  -> Maybe ([Asteroid], Int, Int)
hitAsteroid []     _   _   = Nothing
hitAsteroid (a:as) pos nid
  | dist2 pos (_ax a, _ay a) < astRadius (_asz a) ^ (2 :: Int) =
      let pts      = case _asz a of { 3 -> 20; 2 -> 50; _ -> 100 }
          children = if _asz a > 1
            then [ Asteroid (_ax a) (_ay a)
                     (_avx a *  1.4 + 1.0) (_avy a * 0.6 - 0.6)
                     (_asz a - 1) nid
                 , Asteroid (_ax a) (_ay a)
                     (_avx a * (-0.8) - 0.9) (_avy a * 1.4 + 0.5)
                     (_asz a - 1) (nid+1) ]
            else []
      in Just (children ++ as, pts, nid + 2)
  | otherwise =
      fmap (\(as', p, n) -> (a:as', p, n)) (hitAsteroid as pos nid)
----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
wrapD :: Double -> Double -> Double
wrapD lim x
  | x < 0    = x + lim
  | x >= lim = x - lim
  | otherwise = x

-- | Squared distance (avoids sqrt for collision checks)
dist2 :: (Double, Double) -> (Double, Double) -> Double
dist2 (x1,y1) (x2,y2) = (x2-x1)*(x2-x1) + (y2-y1)*(y2-y1)

astRadius :: Int -> Double
astRadius 3 = 38
astRadius 2 = 21
astRadius _ = 11

-- | Double coordinate rounded to integer MisoString
mc :: Double -> MisoString
mc = ms . (round :: Double -> Int)

-- | "x,y" pair for SVG points attribute
pt :: Double -> Double -> MisoString
pt x y = mc x <> "," <> mc y
----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------
viewModel :: Model -> View Model Action
viewModel m@Model{..} =
  H.div_
    [ CSS.style_
        [ CSS.margin "0"
        , CSS.padding "0"
        , CSS.background "#000"
        , CSS.display "flex"
        , CSS.flexDirection "column"
        , CSS.alignItems "center"
        , CSS.fontFamily "monospace"
        , CSS.color (CSS.hex "0f0")
        , ("user-select", "none")
        ] ]
    [ repoBar
    , hudBar _score _lives _gs
    , gameCanvas m
    , helpBar
    ]

repoBar :: View Model Action
repoBar =
  H.div_
    [ CSS.style_
        [ CSS.width "800px"
        , CSS.padding "4px 0"
        , CSS.fontSize "12px"
        , CSS.color (CSS.hex "0a0")
        , CSS.textAlign "right"
        ] ]
    [ H.a_
        [ HP.href_ "https://github.com/haskell-miso/miso-asteroid"
        , HP.target_ "_blank"
        , CSS.style_ [ CSS.color (CSS.hex "0a0") ]
        ]
        [ text "github.com/haskell-miso/miso-asteroid" ]
    ]

hudBar :: Int -> Int -> GameState -> View Model Action
hudBar sc lv st =
  H.div_
    [ CSS.style_
        [ CSS.width "800px"
        , CSS.padding "6px 0"
        , CSS.fontSize "16px"
        , CSS.display "flex"
        , CSS.justifyContent "space-between"
        ] ]
    [ H.span_ [] [ text $ "Score: " <> ms sc ]
    , H.span_ [] [ text (statusMsg st) ]
    , H.span_ [] [ text $ "Lives: " <> ms lv ]
    ]
  where
    statusMsg Playing = "ASTEROIDS"
    statusMsg Dead    = "GAME OVER — press R"
    statusMsg Victory = "YOU WIN! — press R"

helpBar :: View Model Action
helpBar =
  H.div_
    [ CSS.style_
        [ CSS.width "800px"
        , CSS.padding "4px 0"
        , CSS.fontSize "12px"
        , CSS.color (CSS.hex "0a0")
        , CSS.textAlign "center"
        ] ]
    [ text "\x2190\x2192 Rotate   \x2191 Thrust   SPACE Shoot   R Restart" ]

gameCanvas :: Model -> View Model Action
gameCanvas Model{..} =
  SE.svg_
    [ HP.width_   (mc sw)
    , HP.height_  (mc sh)
    , CSS.style_
        [ CSS.background "#000"
        , CSS.border "1px solid #1a1"
        , CSS.display "block"
        ]
    ]
    $  map renderAst    _asts
    ++ map renderBullet _buls
    ++ [ renderShip _ship _invincible ]
    ++ gameOverlay _gs

renderShip :: Ship -> Int -> View Model Action
renderShip Ship{..} inv =
  let a   = _sang
      -- Nose
      nx = _sx + sin a * shipSize;       ny = _sy - cos a * shipSize
      -- Back-left wing
      lx = _sx + sin (a+2.4) * (shipSize*0.85)
      ly = _sy - cos (a+2.4) * (shipSize*0.85)
      -- Back-right wing
      rx = _sx + sin (a-2.4) * (shipSize*0.85)
      ry = _sy - cos (a-2.4) * (shipSize*0.85)
      -- Rear notch
      cx = _sx + sin (a + pi) * (shipSize*0.35)
      cy' = _sy - cos (a + pi) * (shipSize*0.35)
      pts = pt nx ny <> " " <> pt lx ly <> " "
         <> pt cx cy' <> " " <> pt rx ry
      op  = if inv > 0 && (inv `mod` 8) > 3 then "0.15" else "1"
  in SE.polygon_
       [ SP.points_      pts
       , SP.fill_        "none"
       , SP.stroke_      "#00ff88"
       , SP.strokeWidth_ "2"
       , SP.opacity_     op
       ]

renderAst :: Asteroid -> View Model Action
renderAst Asteroid{..} =
  SE.circle_
    [ SP.cx_          (mc _ax)
    , SP.cy_          (mc _ay)
    , SP.r_           (mc (astRadius _asz))
    , SP.fill_        "none"
    , SP.stroke_      "#0f0"
    , SP.strokeWidth_ "1.5"
    ]

renderBullet :: Bullet -> View Model Action
renderBullet Bullet{..} =
  SE.circle_
    [ SP.cx_   (mc _bx)
    , SP.cy_   (mc _by)
    , SP.r_    "3"
    , SP.fill_ "#ff0"
    ]

gameOverlay :: GameState -> [View Model Action]
gameOverlay Playing = []
gameOverlay st =
  let (msg, col) = case st of
        Dead    -> ("GAME OVER", "#f00")
        Victory -> ("YOU WIN!", "#0f0")
        Playing -> ("", "#fff")
  in [ SE.text_
         [ SP.x_ "400", SP.y_ "270"
         , SP.textAnchor_ "middle"
         , SP.fill_     col
         , SP.fontSize_ "52"
         ] [ text msg ]
     , SE.text_
         [ SP.x_ "400", SP.y_ "330"
         , SP.textAnchor_ "middle"
         , SP.fill_     "#888"
         , SP.fontSize_ "20"
         ] [ text "Press R to play again" ]
     ]
----------------------------------------------------------------------------
