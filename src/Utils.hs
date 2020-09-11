{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Data.Text (pack)
import Data.Bool (bool)
import Numeric (showFFloat)
import Data.Ord (Down(..))
import Data.List.Extra
import qualified Data.Monoid (getSum, Sum(..))

import Graphics.Vega.VegaLite hiding (filter, lookup, repeat)
import qualified Graphics.Vega.VegaLite as VL (filter, lookup, repeat)

import Tyche.Dist
import Tyche.Model
import Tyche.Prob
import Tyche.Prelude
import Tyche.Inference.Enumerate


unif :: Ord a => [a] -> Model a
unif xs = belief . Discrete $ map (, lp) xs
  where lp = 1 / (fromIntegral (length xs))

categorical :: [(a, Prob)] -> Model a
categorical = belief . Discrete

delta :: a -> Model a
delta = belief . Delta

barFromDist :: (Ord a, Show a) => Dist a -> VegaLite
barFromDist m =
  let (vs,ps) = unzip (toList m)
      dvals = dataFromColumns []
              . dataColumn "outcome" (Strings $ fmap (pack . show) vs)
              . dataColumn "probability" (Numbers $ fmap exp ps)

      enc = encoding
            . position X [PName "outcome", PmType Ordinal, PAxis [AxLabelAngle (-55), AxNoTitle], PScale [SRangeStep (Just 30)], PSort []]
            . position Y [PName "probability", PmType Quantitative, PAxis [AxTitle "Prob"]]

  in toVegaLite [description "", dvals [], mark Bar [], enc []]

sdBarFromDist :: (Ord a, Show a, Integral a) => [Int] -> String -> Dist a -> VegaLite
sdBarFromDist sds t m =
  let (vs,ps) = unzip (toList m)
      dvals = dataFromColumns []
              . dataColumn "sigma" (Numbers $ fmap fromIntegral vs)
              . dataColumn "prob" (Numbers $ fmap exp ps)
              . dataColumn "exp" (Numbers $ zipWith (\p v -> exp p * fromIntegral v) ps vs)

      enc = encoding
            . position X [ PName "sigma", PmType Quantitative
                         , PAxis [{- AxLabelAngle (-55),-} AxTitle (pack $ "Expected " ++ t)], PSort []
                         -- , PScale [SDomain (DNumbers $ fmap fromIntegral sds)]
                         ]
            . position Y [PName "prob", PmType Quantitative, PAxis [AxTitle "Prob"]]
            
      barplot = [description "", dvals [], width 150, mark Bar [MColor "firebrick", MOpacity 0.65], enc []]
      barline = [description "", dvals [], mark Rule [], encoding . position X [PName "exp", PAggregate Sum] . size [MNumber 5] $ []]
      bartext = [ description "", dvals [], mark Text [MFontSize 14, MdX 60, MdY (-80)]
                , encoding . text [TName "exp", TAggregate Sum, TmType Quantitative, TFormat ".2f"] $ []]

  in toVegaLite [layer [asSpec barplot, asSpec barline, asSpec bartext]]

sidewaysDists :: [VegaLite] -> VegaLite
sidewaysDists ms =
  toVegaLite [ spacing 50
             , resolve (resolution (RScale [(ChColor, Independent)]) [])
             , hConcat (fromVL <$> ms)
             , configure . configuration (BarStyle [MContinuousBandSize 13.5]) $ []
             ]

titled :: String -> VegaLite -> VegaLite
titled s v = toVegaLite [title (pack s) [], layer [fromVL v]]

heatFromDist :: (Ord a, Ord b, Show a, Show b) => Dist (a,b) -> VegaLite
heatFromDist m =
  let (vs,ps) = unzip (toList m)
      dvals = dataFromColumns []
              . dataColumn "outcome_A" (Strings $ fmap (pack . show . fst) vs)
              . dataColumn "outcome_B" (Strings $ fmap (pack . show . snd) vs)
              . dataColumn "probability" (Numbers $ fmap exp ps)

      conf = configure
             . configuration (View [ViewStrokeWidth 0])
             . configuration (Scale [SCRangeStep (Just 30)])
             . configuration (Axis [Domain False])

      enc = encoding
            . position X [PName "outcome_A", PmType Ordinal, PAxis [AxLabelAngle (-55)], PScale [SRangeStep (Just 30)], PSort []]
            . position Y [PName "outcome_B", PmType Ordinal, PAxis [], PScale [SRangeStep (Just 30)], PSort []]
            . color [MName "probability", MAggregate Sum, MmType Quantitative , MLegend [LNoTitle], MScale [SScheme "blues" []]]

  in toVegaLite [dvals [], mark Rect [], conf [], enc []]

margHeatFromDist :: (Ord a, Ord b, Show a, Show b) => Dist (a,b) -> VegaLite
margHeatFromDist m =
  let (vs,ps) = unzip (toList m)
      scl = nub . fmap (pack . show . fst) $ vs
      dvals = dataFromColumns []
              . dataColumn "ht" (Strings $ fmap (pack . show . fst) vs)
              . dataColumn "mu" (Strings $ fmap (pack . show . snd) vs)
              . dataColumn "prob" (Numbers $ fmap (read . trunc . exp) ps)

      diag = dataFromColumns [] . dataColumn "vals" (Strings scl)

      conf = configure
             . configuration (Scale [SCRectBandPaddingInner 0])

      enc = encoding
            . position X [ PName "mu", PmType Ordinal, PAxis [AxLabelAngle 0, AxTitle "CC Center"], PSort []
                         , PScale [SRangeStep (Just 30), SPadding 0.05, SDomain (DStrings scl)]
                         ]
            . position Y [ PName "ht", PmType Ordinal, PAxis [AxLabelAngle 0, AxTitle "Subj Height"], PSort []
                         , PScale [SRangeStep (Just 30), SPadding 0.05, SDomain (DStrings $ reverse scl)]
                         ]

      heatmap = [ mark Rect []
                , encoding
                  . color [MName "prob", MAggregate Sum, MmType Quantitative , MLegend [], MScale [SScheme "blues" [0.2,0.9]]]
                  $ []
                ]

      heattxt = [ mark Text [MFontSize 9]
                , encoding
                  . text [TName "prob", TAggregate Sum, TmType Quantitative]
                  . color [MString "white"]
                  $ []
                ]

      heatlin = [ mark Line [MStroke "firebrick", MStrokeWidth 2, MOpacity 0.5]
                , diag []
                , encoding
                  . position X [ PName "vals", PmType Ordinal, PSort [] ]
                  . position Y [ PName "vals", PmType Ordinal, PSort [] ]
                  $ []
                ]

      heat = [ enc []
             , layer [asSpec heatmap, asSpec heattxt, asSpec heatlin]
             ]

      abar = [ mark Bar []
             , height 60
             , encoding
               . position X [PName "mu", PmType Ordinal, PAxis [], PSort []
                            , PScale [SRangeStep (Just 30), SDomain (DStrings $ scl)]
                            ]
               . position Y [PName "prob", PAggregate Sum, PmType Quantitative, PAxis [AxTitle "Prob"]]
               $ []
             ]

      bbar = [ mark Bar []
             , width 60
             , encoding
               . position Y [ PName "ht", PmType Ordinal, PAxis [], PSort []
                            , PScale [SRangeStep (Just 30), SDomain (DStrings $ reverse scl)]
                            ]
               . position X [PName "prob", PAggregate Sum, PmType Quantitative, PAxis [AxTitle "Prob"]]
               $ []
             ]

      hplots = map asSpec [heat, bbar]
      plots = map asSpec [abar, [spacing 15, bounds Flush, hConcat hplots]]

  in toVegaLite [dvals [], conf [], spacing 15, bounds Flush, vConcat plots]

choose :: Prob -> a -> a -> Model a
choose p t f = bool f t <$> toss p

-- truncate probabilities to readable length
trunc :: Double -> String
trunc n = showFFloat (Just 1) (100 * n) ""

-- display a discrete distribution
showEnums :: Show b => [(b, Double)] -> IO ()
showEnums = mapM_ (\(b,d) -> putStrLn $ show b ++ ": " ++ trunc d ++ "%") . sortOn (Down . snd)
