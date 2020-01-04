# boopadoop
![Hackage](https://img.shields.io/hackage/v/boopadoop?style=flat-square)

Mathematically sound sound synthesis.

A music theory library for just intonation and other mathematically pure ideas.
Documentation and additional exposition on [Hackage](https://hackage.haskell.org/package/boopadoop)!
## Pitches and Chords

We can build chords:
```haskell
majorAChord :: DWave
majorAChord = balanceChord
  [sinWave concertA
  ,sinWave $ intervalOf majorThird concertA
  ,sinWave $ intervalOf perfectFifth concertA
  ]
```
and pretty print the resulting sound wave:
```
>>> show majorAChord
........................................................................................................
..xxx.......................................................xxx.........................................
.x...............................................xxx....................................................
.....x.......xxx...............xx...............x..........x...x.......xxx...............xxx............
x...........x...xx..xxxx......x..xx....xx......x....x.................x...xx..xxxx......x...xx...xx.....
......x....x......xx....xx..xx.....xxxx..xx...x...........x.....x...........xx....xx..xx......xxx..xxx..
.......x..................xx...............xxx.......x...........x...x..............xx................xx
........xxx...........................................x..x........xxx...................................
.......................................................xx...............................................
```
or write it to a `.wav` file by specifying sampling parameters with
```haskell
testWave "aChord" (tickTable 32000 . discretize $ majorAChord)
```

The intervals can be given as ratios
```haskell
perfectFifth :: PitchFactorDiagram
perfectFifth = countPFD (3/2)
```
or using "[Pitch Factor Diagrams](https://hackage.haskell.org/package/boopadoop/docs/Boopadoop-Diagram.html)":
```haskell
octave       = normalizePFD $ Factors [1]
perfectFifth = normalizePFD $ Factors [0,1]
majorThird   = normalizePFD $ Factors [0,0,1]
minorThird   = normalizePFD $ Factors [0,1,-1] -- Minor third is up a p5, down a M3
```
Also supports equal temperament semitones: (a perfect fifth is approximately seven semitones)
```haskell
>>> diagramToSemi perfectFifth
7.019550008653875

>>> equalTempPerfectFifth = countPFDFuzzy $ semi ** 7
Factors {getFactors = [1,-2,0,0,1,0,0,1,0,0,-1]}
```

## Filtering
We have a Waveform that is made of a sine wave concert A (440 Hz) and a tritone above it (D# 609 Hz), and we want to filter out the tritone and end up with just the A.
```haskell
unfiltered :: Wavetable
unfiltered = modulate (+) 
  (setVolume 0.5 $ fastSin concertA                      stdtr) 
  (setVolume 0.5 $ fastSin (intervalOf tritone concertA) stdtr) 

tritone = countPFD (18/13)
```
The original signal looks something like this:
```
>>> show unfiltered
...xxxxx..............................................xxxx.................xx...........................
..x.....x............................................x....x..............xx..xx.........................
.x.......x.............xxxx..........x..............x......x............x......x.......................x
..........x..........xx....xxxx.xxxxx.xxx..........x........x..........x........x..........xxx.......xx.
x.......................................................................................................
...........x.......xx..........x.........x........x..........x........x..........xx.....xxx...xxxxxxx...
............x.....x.......................xx....xx............x......x.............xxxxx................
.............xxxxx..........................xxxx...............x....x...................................
................................................................xxxx....................................
```
Its (Discrete) Fourier Transform looks like this, with two peaks (one for the A, one for the D#):
```
>>> show (realDFT 200 stdtr unfiltered)
........................................................................................................
...........................................xxx...............xxxx.......................................
........................................xxx...xxx.........xxx....xx.....................................
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.........xxxxxxxxx.........xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
........................................................................................................
........................................................................................................
........................................................................................................
........................................................................................................
........................................................................................................
```
Now, we filter it using a bandpass filter centered at concert A:
```haskell
filtered :: Wavetable
filtered = tickConvolution 160 10 (sampleFrom $ const theFilter) $ unfiltered
  where
    !theFilter = optimizeFilter 160 $ tickTable stdtr $ bandpassFilter concertA 100
```
The filtered signal looks more like a single sine wave now:
```
>>> show filtered
........................................................................................................
........................................................................................................
.....xxxx...................xxxxx...................xxxxx....................xxxx...................xxxx
.xxxx....xxxx............xxx.....xxxx............xxx.....xxxx............xxxx....xxxx............xxx....
........................................................................................................
x............xxxx....xxxx............xxx.....xxxx............xxxx....xxxx............xxx.....xxxx.......
.................xxxx...................xxxxx....................xxxx...................xxxxx...........
........................................................................................................
........................................................................................................
```

If we take the Fourier Transform of the filtered signal, we get only one peak, at concert A:
```
>>> show (realDFT 200 stdtr filteredTicks)
........................................................................................................
........................................................................................................
...........................................xxxxx........................................................
xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx.....xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
........................................................................................................
........................................................................................................
........................................................................................................
........................................................................................................
........................................................................................................
```

If you write `filtered` and `unfiltered` to `.wav` files using `testWave`, you can hear the difference!
## Usage

Simply run `stack ghci` using [Haskell Stack Tool](https://www.haskellstack.org) to interact with boopadoop.
