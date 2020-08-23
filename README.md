# boopadoop
![Hackage](https://img.shields.io/hackage/v/boopadoop?style=flat-square)

Mathematically sound sound synthesis.

A music theory library for just intonation and other mathematically pure ideas.
Documentation and additional exposition on [Hackage](https://hackage.haskell.org/package/boopadoop)!
## Usage

Simply run `stack ghci` using [Haskell Stack Tool](https://www.haskellstack.org) to interact with boopadoop.

## SolFeck guide
SolFeck is a shorthand language for quickly writing down twelve tone note sequences

| SolFeck Symbol | What it does |
| --- | --- |
| Digit in `{0,1,...,9,a,b}` | Play the nearest pitch to the previous note from the corresponding pitch class |
| `^` | Go an octave up for the next note |
| `v` | Go an octave down for the next note |
| `-` | Extend the previous note |
| `=` followed by number | `=5` is the same as `-----` |
| `~` followed by pitch symbol | `~4` modulates the key into `k=4` |
| ` ` | Plays a rest (silence) |
| `x` | Repeat previous note, e.g. `024xxxxx` is `02444444` |
| `'` | Go up by one scale step, e.g. `0'` is `02` |
| `.` | Go down by one scale step, e.g. `0.` is `0b` |
| ``` | Go up by two scale steps, e.g. `0\`` is `04` |
| `,` | Go down by two scale steps, e.g. `0,` is `09` |
| `i` | Go up by three scale steps, e.g. `0i` is `05` |
| `!` | Go down by three scale steps, e.g. `0!` is `07` |

## Output

Boopadoop will play your input or the generated music, and also output it to `out/listen.wav` as an audio file, and to `out/lilyout.ly` as a LilyPond music typesetting file.
If you get the Lilypond program, you can double click this file and it will output a `lilyout.pdf` rendering of it as sheet music.

## Older features (may not still work)
### Pitches and Chords

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

### Filtering
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
