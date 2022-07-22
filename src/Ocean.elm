module Ocean exposing (Tide, Wave, update, newTide, newWave, highTide, tidalValue, viewFrontTide, viewBackTide, viewComingWave, viewCrashingWave, waveValue, new, Ocean, waveSurfaceDangerZone, isWaveCrashing, waveBridgeDangerZone, tidalDangerZone, oceanWave, oceanTide)

import Basics.Extra exposing (flip, uncurry)
import GFXAsset
import Misc

type Ocean = Ocean Tide Wave

new = Ocean newTide newWave
oceanTide (Ocean t _) = t
oceanWave (Ocean _ w) = w
updateOcean tideUpdater waveUpdater ocean =
  (ocean, ocean)
  |> Tuple.mapBoth (oceanTide >> updateTide tideUpdater) (oceanWave >> updateWave waveUpdater)
  |> uncurry Ocean
resetOceanWave = updateOcean identity (\wd -> if wd <= (0 - waveCrashDuration) then newWave |> waveValue else wd)
waveSurfaceDangerZone ocean =
  if ocean |> oceanTide |> tidalValue |> Misc.within 0 33 then
    (116, 524)
  else if ocean |> oceanTide |> tidalValue |> Misc.within 0 66 then
    (61, 560)
  else 
    (15, 560)
waveBridgeDangerZone ocean =
  if ocean |> oceanTide |> tidalValue |> Misc.within 0 33 then
    (116, 524)
  else if ocean |> oceanTide |> tidalValue |> Misc.within 0 66 then
    (61, 560)
  else 
    (15, 560)
tidalDangerZone ocean =
  if ocean |> oceanTide |> tidalValue |> Misc.within 0 33 then
    (10000, 100001)
  else if ocean |> oceanTide |> tidalValue |> Misc.within 0 66 then
    (10000, 100001)
  else 
    (249, 416)
isWaveCrashing = oceanWave >> waveValue >> ((>=) 0.0)

    
type Tide = Tide Float

updateTide updater (Tide wl) = setTide (updater wl)
tidalValue (Tide wl) = wl
newTide = Tide 0.0
setTide = clamp 0.0 100.0 >> Tide
highTide = oceanTide >> tidalValue >> (<) 100


type Wave = Wave Float
newWave = Wave 30.0
updateWave updater (Wave distance) =  updater distance |> Wave
waveValue (Wave distance) = distance
setWave = clamp -1.0 30.0 >> Wave
waveCrashDuration = 1.0

update : Float -> Ocean -> Ocean
update delta ocean =
  let dts = delta / 1000
  in 
  ocean
  |> updateOcean ((+) dts) (flip (-) dts)
  |> resetOceanWave

viewFrontTide =
  oceanTide >> tidalValue >> GFXAsset.frontTide 

viewBackTide =
  oceanTide >> tidalValue >> GFXAsset.backTide

viewComingWave ocean =
  GFXAsset.waveSprite (ocean |> oceanWave |> waveValue |> (+) 1 |> (/) 1) (isWaveCrashing ocean)


viewCrashingWave ocean =
  if isWaveCrashing ocean then
    GFXAsset.crashingWaveSprite (ocean |> oceanWave |> waveValue)
  else
    []
