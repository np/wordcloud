{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.WordCloud.Types where

import Graphics.GD
import Data.Default
import Control.Monad.State.Lazy
import Data.Typeable
import Data.Generics

data Word = Word {
      wordString :: String
    , wordSize   :: Double
    , wordPoint  :: Point }
type Rect = ((Int,Int),(Int,Int))
type Region = (Point, Point, Point, Point)
type Histogram = [WordEntry]
type WordEntry = (String,Int)

-- | Which font to use.
data Font = FontPath FilePath -- ^ Specify by filepath.
          | FontName String   -- ^ Specify by FontConfig name.
            deriving (Data,Typeable)

-- | Configuration for a word cloud.
--
-- Default configuration:
--
--   * confFontFamily = FontName \"URW Bookman L\"
--
--   * confFontSize   = 72
--
--   * confMaxWords   = 100
--
--   * confCanvasSize = (800,600)
--
--   * confDefaultPos = (400,400)
--
--   * confColor      = (200,100,20)
--
--   * confCloudAlgo  = Original
data Config = Config { 
      confFontFamily :: Font          -- ^ The font to use.
    , confFontSize   :: Double        -- ^ Largest font size on which to base the other sizes.
    , confFontSizeMin :: Double
    , confMaxWords   :: Int           -- ^ Maximum words to draw in the cloud.
    , confCanvasSize :: (Int,Int)     -- ^ Canvas (image) size.
    , confDefaultPos :: (Int,Int)     -- ^ Specify a default position for the largest word.
    , confColor      :: (Int,Int,Int) -- ^ The colour to use for the cloud words.
    , confBGColor    :: (Int,Int,Int) -- ^ The background colour.
    , confCloudAlgo  :: Algorithm     -- ^ The algorithm to use for rendering the cloud.
    }-- deriving (Data,Typeable)
            
instance Default Config where
    def = Config {
            confFontFamily = FontName "URW Bookman L"
          , confFontSize   = 60
          , confMaxWords   = 70
          , confFontSizeMin = 10
          , confCanvasSize = (800,600)
          , confDefaultPos = (400,400)
          , confColor      = (200,100,20)
          , confBGColor    = (0,0,0)
          , confCloudAlgo  = Original }

-- | The algorithm to use for the cloud generation.
data Algorithm
  = Original  -- ^ Generates a mostly horizontal cloud.
  | Circular  -- ^ Generates a mostly circular cloud.
--    deriving (Data,Typeable)

-- Apply a function to the config; handy function.
config :: (Config -> a) -> Cloud a
config f = gets (f . cloudConf)

--deriving instance Data Image

-- Cloud creation monad
data CloudSt = CloudSt { cloudImg   :: Image
                       , cloudMax   :: Int
                       , cloudConf  :: Config }
  --             deriving (Data,Typeable)
type Cloud a = StateT CloudSt IO a

-- Default state
defCloudSt :: CloudSt
defCloudSt = CloudSt { cloudConf = undefined
                     , cloudMax  = undefined
                     , cloudImg  = undefined }
