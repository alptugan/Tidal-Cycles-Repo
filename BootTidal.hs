
:set -XOverloadedStrings
:set prompt ""

module PatternFunctions where
import Sound.Tidal.Params


import Sound.Tidal.Context

import System.IO (hSetEncoding, stdout, utf8)

hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
--tidal <- startTidal (superdirtTarget {oLatency = 0.5, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20})

tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cEnableLink = True, cQuantum = 4, cBeatsPerCycle = 4})


:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13 . (|< orbit 12)
    d14 = p 14 . (|< orbit 13)
    d15 = p 15 . (|< orbit 14)
    d16 = p 16 . (|< orbit 15)
:}

:{
let setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}


-- custom function
-- let bpm x = setcps(x/120)
let bpm x = setcps(x/60/4)

-- fadeout function
let xfadeout i t o = xfadeIn i t $ s "bit1" # gain "0" # orbit o



-- custom pattern
let koko i fname f o = p i $ every 2 (|* speed "0.5") $ sound fname # speed "3" # n (irand 89) # gain f # orbit o

let pat1 = "{0*2 ~ [0 ~] ~ 0*4 [~ 0] 0}%4"

let pat2 = "{[~ 0] 0 ~ 0 [~ 0] ~ 0}%4"

let pat3 = "{0 0*4 [~ 0] [~ 0] 0*2  0 ~ 0*4 ~ 0 -12 0 ~ [~ 0] [~ 0] ~ 0*2 -12}%8"

let pat4 = "{0 0*2 [~ 0] [~ 0] 0 0 -12 0*4 ~ 0 ~ 0 -12 0*2 ~ [~ 0] [~ 0] ~ 0*2 -12}%8"

let pat5 = "{[~ 0] 0 0 ~ 0 -12 0*2 ~ [~ 0] [~ 0] ~ 0 -12 0*4 ~ [~ 0] [~ 0] ~ 0*2 -12}%4"

let pat6 = "{~ 0 ~ 0 -12 0*4 0 0*2 [~ 0] -12 0*4 ~ 0 ~ 0 -12 0*2 ~ [~ 0] [~ 0] ~ 0*2 -12}%4"

let pat7 = "{2 0 4 5}"

let pat8 = "{0*2 ~ [4 ~] ~ 3*4 [~ 1] 8}%2"

let pat9 = "{2 0 4 5}%8"


-- Equalizer
let hiPassFreq = pF "hiPassFreq"
    hiPassRq   = pF "hiPassRq"
    hiPassBypass   = pF "hiPassBypass"
    loShelfFreq = pF "loShelfFreq"
    loShelfGain = pF "loShelfGain"
    loShelfRs   = pF "loShelfRs"
    loShelfBypass   = pF "loShelfBypass"
    loPeakFreq  = pF "loPeakFreq"
    loPeakGain  = pF "loPeakGain"
    loPeakRq    = pF "loPeakRq"
    loPeakBypass    = pF "loPeakBypass"
    midPeakFreq = pF "midPeakFreq"
    midPeakGain = pF "midPeakGain"
    midPeakRq   = pF "midPeakRq"
    midPeakBypass   = pF "midPeakBypass"
    hiPeakFreq  = pF "hiPeakFreq"
    hiPeakGain  = pF "hiPeakGain"
    hiPeakRq    = pF "hiPeakRq"
    hiPeakBypass    = pF "hiPeakBypass"
    hiShelfFreq = pF "hiShelfFreq"
    hiShelfGain = pF "hiShelfGain"
    hiShelfRs   = pF "hiShelfRs"
    hiShelfBypass   = pF "hiShelfBypass"
    loPassFreq = pF "loPassFreq"
    loPassRq   = pF "loPassRq"
    loPassBypass   = pF "loPassBypass"

-- Compressor
let cpAttack = pF "cpAttack"
    cpRelease = pF "cpRelease"
    cpThresh = pF "cpThresh"
    cpTrim = pF "cpTrim"
    cpGain = pF "cpGain"
    cpRatio = pF "cpRatio"
    cpLookahead = pF "cpLookahead"
    cpSaturate = pF "cpSaturate"
    cpHpf = pF "cpHpf"
    cpKnee = pF "cpKnee"
    cpBias = pF "cpBias"

-- Mixer
let masterGain = pF "masterGain"

:set prompt "tidal> "
:set prompt-cont ""
