// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 16; // increase this if you need to load more samples
s.options.memSize = 8192 * 32; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 64; // increase this if you are getting drop outs and the message "too many nodes"

s.options.numOutputBusChannels = 32; // set this to your hardware output channel size, if necessary
//s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// boot the server and start SuperDirt

s.waitForBoot {
	"We code and compose honey".postln;
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles("/Users/alptugan/Google Drive/Dirt-Samples/*");   // load samples (path containing a wildcard can be passed in)
	s.sync;

	//~dirt --no-dirty-compressor
	~dirt.start(57120, 0 ! 16);   // start listening on port 57120, create two busses each sending audio to channel 0
	 //~dirt.start(57120, [0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30]);

	s.meter;
	s.latency = 0.5; // increase this if you get "late" messages
};

(
	// foward OSC from TidalCycles

~addr = NetAddr.new("127.0.0.1", 3333);

OSCFunc({ |msg, time, tidalAddr|
var latency = time - Main.elapsedTime;
msg = msg ++ ["time", time, "latency", latency];
msg.postln;
~addr.sendBundle(latency, msg)}, "/play2").fix;

);









/*
var server = server ? Server.default;
var numChannels = ~dirt.numChannels;
var synthbuf = Buffer.alloc(server, server.sampleRate * 4, 1);
var recbuf = Array.fill(9, {|x| Buffer.alloc(server, server.sampleRate*4,1)});
var notebus = Bus.control(s,1);
var ringfunc = {|a,b| a*a+b;};

SynthDef(\noop, {}).add;

SynthDef(\superfff, {|out, pan, freq, sustain|
	var sound;
	var aenv = EnvGen.ar(Env.perc(0.01, 0.99, 1, -1), timeScale:sustain, doneAction:2);
	sound = SinOsc.ar(freq);
	sound = ringfunc.value(sound, SinOsc.ar(freq-40));
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv));
}).add;

// FUTURE???
SynthDef(\superpsaw, {|out, pan, freq, sustain|
	var sound, mod, aenv, fenv;
	aenv = EnvGen.ar(Env.linen(0.05, 0.9, 0.05, 1, 'lin'), timeScale:sustain, doneAction:2);
	fenv = EnvGen.ar(Env.perc(0.1, 0.9, 1, -1), timeScale:sustain);
	sound = Splay.arFill(4, {|n| VarSaw.ar(freq*(0.01*n+0.985), Rand(0,1), 0.01)}, 0.4);
	sound = RLPF.ar(sound, freq*5*fenv, 1.5);
	sound = sin(sound*2)/1.4;
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv));
}).add;

// trying some phase sync stuff
// kind of brassy?
SynthDef(\supersync, {|out, pan, freq, sustain|
	var sound, mod, aenv, fenv;
	aenv = EnvGen.ar(Env.linen(0.1, 0.8, 0.1, 1, 'lin'), timeScale:sustain, doneAction:2);
	fenv = EnvGen.ar(Env.perc(0.2, 0.7, 1, -1), timeScale:sustain);
	sound = Splay.ar(SyncSaw.ar(freq, [1800, 803, 500, 499, 800, 1810]), 0.5);
	sound = sound + ([0.3,-0.3] * DelayC.ar(sound, 0.1, Line.ar(1,0.5,sustain)*0.005));
	sound = RLPF.ar(sound, clip(freq*7*fenv, 20, 15000), 1.5);
	sound = sin(sound*2)/2;
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv));
}).add;

// build in some distortion?
SynthDef(\superdrop, {|out, sustain=1, pan, freq, accelerate=0, pitch1=4, voice=0,lfo=0|
	var sound, basefreq, aenv, fenv, menv, ind, m1, m2, filtrange;
	basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	aenv = EnvGen.ar(Env.linen(0.05, 0.9, 0.05, 1, 'lin'), timeScale:sustain, doneAction:2);
	menv = XLine.ar(4, 1, sustain/3);
	sound = Saw.ar(basefreq * menv * [0.99, 1, 1.01]);
	sound = Splay.ar(sound);
	sound = sound * Pulse.ar(4*basefreq*menv, 0.3);
	sound = tanh(1.5*sin(1.5*sound));
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv))
}).add;

// maybe turn into a nice pad and rename
SynthDef(\superdub5, {|out, sustain=1, pan, freq, accelerate=0, pitch1=4, voice=0,lfo=0|
	var sound, basefreq, aenv, fenv, menv, ind, m1, m2, filtrange;
	aenv = EnvGen.ar(Env.linen(0.05, 0.9, 0.05, 1, 'lin'), timeScale:sustain, doneAction:2);
	menv = EnvGen.ar(Env.linen(0.3, 0.2, 0.5), timeScale:sustain);
	basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	m1 = SinOsc.ar(basefreq*2) * basefreq/8;
	m2 = SinOsc.ar(basefreq + m1*4) * basefreq*2 * menv;
	sound = Splay.ar(VarSaw.ar(basefreq+m2 * [0.99,1,1.01], 0, 0.2*menv+0.1));
	sound = (sound*(50*voice+5)).clip(-1,1)*0.5;
	sound = HPF.ar(LeakDC.ar(sound), 60);
	sound = 0.5*LFTri.ar(basefreq) + sound;
	sound = sin(2*sound);
	filtrange = clip(menv*basefreq*12+1200, 2, 20000);
	sound = LPF.ar(sound, LFTri.kr(pitch1).range(filtrange*(1-lfo), filtrange));
	OffsetOut.ar(out, DirtPan.ar(sound*aenv, ~dirt.numChannels, pan, 1))
}).add;

// pops a bit, could be nice if fixed
SynthDef(\superdub6, {|out, sustain=1, pan, freq, accelerate=0, pitch1=4, voice=0,lfo=0,detune=0.008|
	var sound, basefreq, aenv, fenv, menv, ind, m1, m2, filtrange, detarr;
	aenv = EnvGen.ar(Env.linen(0.05, 0.9, 0.05, 1, 'lin'), timeScale:sustain, doneAction:2);
	menv = EnvGen.ar(Env.linen(0.4, 0.6, 0.0, 1, 1), timeScale:sustain);
	basefreq = 2**(Lag.kr(In.kr(notebus,1),sustain));
	ReplaceOut.kr(notebus, log2(freq));
	basefreq = basefreq * Line.kr(1, 1+accelerate, sustain);
	detarr = Array.interpolation(3,1-detune,1+detune);
	sound = Splay.ar(Pulse.ar(basefreq * detarr, detarr-0.5)) + Splay.ar(VarSaw.ar(basefreq, 0, [0.1,0.2,0.3])* 0.5);
	sound = sound - CombN.ar(sound, 0.2, 0.010*Line.kr(1,1.5,sqrt(sustain)), 0.1);
	sound = LPF.ar(sound, menv*3000);
	sound = sin(sound*20/sqrt(freq));
	sound = HPF.ar(LeakDC.ar(sound), 60);
	sound = BPeakEQ.ar(sound, XLine.kr(30, 3000, 1), 0.1, -20);
	sound = BPeakEQ.ar(sound, XLine.kr(5000, 60, 1), 0.15, -20);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv))
}).add;

// not what the name says
SynthDef(\superacid, {|out, speed=1, sustain=1, decay=0, pan, freq, accelerate=0, voice=0.0, slide=0,
	pitch1=500, pitch2=1500|
	var sound, basefreq, aenv, fenv, glide;
	aenv = EnvGen.ar(Env.perc(0.01, 1, 1, 'lin'), timeScale:sustain, doneAction:2);
	fenv = EnvGen.ar(Env.perc(0.01, 1+decay, 1, 'lin'), timeScale:sustain);
	glide = EnvGen.ar(Env.perc(0.05, 0.5, 1, \lin), timeScale:sustain) * (-1*slide/4) + 1;
	basefreq = freq * glide * Line.kr(1, 1+accelerate, sustain);
	sound = VarSaw.ar(basefreq, 0) - VarSaw.ar(basefreq, voice/2+0.5);
	sound = 2.0*RLPFD.ar(sound, pitch2*fenv+pitch1, 0.5, 1);
	sound = tanh(10.0*sound);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, aenv))
}).add;

SynthDef(\superwobble, {|out, sustain=1, pan, speed=1, accelerate, freq, decay=0,
	  pitch1=4000, lfo=0.5, pitch2=4|
	var env = EnvGen.ar(Env.pairs([[0,0],[0.07,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain) * [0.99, 1.01];
	var sound = LPF.ar(Pulse.ar(basefreq), basefreq*4) + Saw.ar(1.99*basefreq) + Saw.ar(3.98*basefreq);
	sound = LPF.ar(sound, pitch1*SinOsc.kr(pitch2,3.5).exprange(1,10.0**(-2.0*lfo)));
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

// pulse?
SynthDef(\superpulse, {|out, sustain=1, pan, speed=1, accelerate, freq, decay=0, slide=1 |
	var env = EnvGen.ar(Env.pairs([[0,0],[0.07,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain) * [0.99, 1.01];
	var sound = Blip.ar(basefreq, 64) + Impulse.ar(basefreq) + Saw.ar(basefreq);
	sound = BHiShelf.ar(sound, 1200, 10, -8);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

// original idea - http://www.attackmagazine.com/technique/synth-secrets/detuned-pad/
SynthDef(\superzzz, {|out, sustain=1, pan, freq, detune=1  |
	var sound, env, freqs, osc1, osc2;
	env = EnvGen.ar(Env.linen(0.05, 0.9, 0.05), timeScale:sustain, doneAction:2);
	freqs = freq * ([0, 0.01*Rand(-1,1), 0.011*Rand(-1,1), 0.015*Rand(-1,1)]*detune + 1.0);
	osc1 = DelayC.ar( LFSaw.ar(freqs, Rand(0,2pi)) , 0.2, freqs.reciprocal * SinOsc.ar(Rand(3,4), Rand(0,2pi)).range(0.9,1));
	osc2 = DelayC.ar( LFSaw.ar(freqs/2*Rand(0.99,1.01), Rand(0,2pi)), 0.2, freqs.reciprocal * SinOsc.ar(Rand(2,3), Rand(0,2pi)).range(0.9,1));
	sound = BLowPass4.ar(osc2, SinOsc.ar(Rand(2,4)).range(800,900), 1) + osc1;
	OffsetOut.ar(out, DirtPan.ar(Mix.ar(0.3*sound), ~dirt.numChannels, pan, env));
}).add;

// trying out some synth voice stuff, right now starts with 'a' and glides to vowel with formants at pitch1,pitch2 (default 'i')
SynthDef(\supervoice, {|out, sustain=1, pan, freq, pitch1=240, pitch2=2400|
	var sound, env, p1, p2;
	env = EnvGen.ar(Env.linen(0.1, 0.8, 0.1), timeScale:sustain, doneAction:2);
	sound = Impulse.ar(freq);
	p1 = XLine.kr(850, pitch1, sustain*0.5);
	p2 = XLine.kr(1610, pitch2, sustain*0.5);
	sound = DynKlank.ar(`[[p1, p2], nil, nil], sound, decayscale:0.01);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

// write a signal into a delay, tap it at multiple times
SynthDef(\superbuf, {|out, sustain=1, pan, freq|
	var sound, env, tapPhase, tap1, tap2, tap3, sl, sr;
	// env = EnvGen.ar(Env.perc(0.005, 1, 1, 1), timeScale:sustain, doneAction:2);
	env = EnvGen.ar(Env.linen(0.005, 0.8, 0.195, 1, 1), timeScale:sustain, doneAction:2);
	sound = Mix.ar(VarSaw.ar(freq, [0, 0.5, 1], Line.kr(0,1,sustain)));
    tapPhase = DelTapWr.ar(synthbuf, sound);
    #tap1, tap2, tap3 = DelTapRd.ar(synthbuf, tapPhase,
		[0.1, 0.27, 0.303] * Line.kr(0.48,0.52,sustain) * 0.3 * sustain,      // tap times
        1,                      // no interp
        [1.0, 0.6, 0.4]         // muls for each tap
        );
	sl = sound+tap2;
	sr = tap1 + tap3;
	OffsetOut.ar(out, DirtPan.ar(0.5*[sl, sr], ~dirt.numChannels, pan, env));
}).add;

SynthDef(\supercross, {|out, speed=1, sustain=1, decay=0, pan, freq, accelerate=0, voice=0.0, detune=0.0,
	  pitch1=1, pitch2=2, vcoegint=0|
	var env = EnvGen.ar(Env.pairs([[0,0],[0.03,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq1 = pitch1 * freq * Line.kr(1, 1+accelerate, sustain);
	var basefreq2 = pitch2 * freq * Line.kr(1, 1+accelerate, sustain);
	var sound, sa, sb, loop, rlo, rhi;
	rlo = 1.0 - (0.9 * vcoegint);
	rhi = 1.0 + (0.9 * vcoegint);
	loop = LocalIn.ar(1);
	basefreq1 = clip(basefreq1 * loop.range(rlo,rhi), 2, 20000); // FM mod from osc 2
	sa = SelectX.ar(voice, [
		VarSaw.ar(basefreq1, 0, 0.5) - VarSaw.ar(basefreq1+detune, 0, 0.2),
		SinOsc.ar(basefreq1+detune,0.1) * loop.range(rlo,1), // ring mod for sine,
		LFTri.ar(basefreq1, 0) + LFTri.ar(basefreq1+detune, 0.1) * 0.5,
		Pulse.ar(basefreq1+detune, LPF.ar(loop.range(rlo/2,rhi/2), 50)) * 0.5 // mod pulse width for square
	]);
	basefreq2 = clip(basefreq2 * sa.range(rlo,rhi), 2, 20000); // FM mod from osc 1
	sb = SelectX.ar(voice, [
		VarSaw.ar(basefreq2-detune, 0.2, 0.2) - VarSaw.ar(basefreq2-detune, 0.2, 0.3),
		SinOsc.ar(basefreq2-detune,0.3) * sa.range(rlo,1), // ring mod for sine
		LFTri.ar(basefreq2-detune, 0.3) + LFTri.ar(basefreq2-detune, 0.2) * 0.5,
		Pulse.ar(basefreq2-detune, LPF.ar(sa.range(rlo/2,rhi/2), 50) ) * 0.5
	]);
	LocalOut.ar(DelayN.ar(sb,0.2, Line.ar(0,0.5,sustain)/basefreq1));
    sound = sa+sb;
	sound = LPF.ar(sound, XLine.ar(1,0.01,sustain, 12000));
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env))
}).add;

// probably way too complicated, this synth has it's own set of filters, controllable with new parameters
// voice: 0 - saw, 1 - square (can go past 1 for noisy overdrive)
// slide: initial pitch glide, can be used to fake portamento
// decay: 0 is minimal decay, 1 will quickly decay to silence (you can also use the global release param for a different shape)
// sfcutoff, sfresonance, sfenv, sfattack, sfrelease: filter params for this synth only
//    the envelope shape for this synth is different from the global filter envelope
// vcfegint: advanced param for tweaking Q of bandpass (vcf for short)
// pitch1, pitch2: advanced params for tweaking lowpass and bandpass frequencies

// something I still don't like - CLEAN UP AND FIX
SynthDef(\super302, {|out, speed=1, sustain=1, decay=0, pan, freq, accelerate=0, voice=0.0, slide=0,
	pitch1=1,sfattack=0, sfrelease=0, sfenv=0, sfcutoff=1000, sfresonance=0|
	var sound, glide, basefreq, env, env2, resq, loresq;
	env = EnvGen.ar(Env.new([1e-6,1,(1-decay)*0.5,1e-6], [0.003, 0.9, 0.007], 'exp'), timeScale:sustain, doneAction:2);
	// env2= EnvGen.ar(Env.perc(sfattack, sfrelease, 1, [-4,4]));
	env2 = EnvGen.ar(Env.linen(sfattack, 0, sfrelease));
	glide = EnvGen.ar(Env.perc(0.05, 0.5, 1, \lin), timeScale:sustain) * (-1*slide/4) + 1;
	basefreq = freq * glide * Line.kr(1, 1+accelerate, sustain);
	sound = tanh(VarSaw.ar(basefreq, 0, 0.98)+PinkNoise.ar(0.01*exp(voice))*(voice*50+1));
	resq = 1.0 + (26 * sfresonance);
	// loresq = 1.0 + (26*4.67*sfresonance);
	loresq = 1.0 + (26*sfresonance);
	// sound = BPF.ar(sound, clip(sfcutoff/10*pitch2+(sfenv/10*env2*pitch2), 20,11000),
	// rq:2.9-(2.8*vcfegint));
	// sound = sound * (1.2 + (1.2*vcfegint));
	sound = BMoog.ar(sound, clip(sfcutoff+(sfenv*env2)/2, 20, 11000), q:sfresonance, mode:2, saturation:0.99);
	sound = RHPF.ar(sound, clip(sfcutoff/100*pitch1+(sfenv/10000*env2*pitch1), 8,8000), rq:1/loresq);
	sound = RLPF.ar(sound, clip(sfcutoff+(sfenv*env2), 20, 11000), rq:1/clip(resq*1000/sfcutoff,1,52));
	//sound = ((sfresonance * sound)**3 + sound) / (max(1,sfresonance)**3);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env))
}).add;

SynthDef(\superlagcomp, {|out, speed=1, sustain=1, decay=0, pan, freq, accelerate=0, voice=0.2|
	var env = EnvGen.ar(Env.pairs([[0,0],[0.03,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	var sound, sa,sb, sl, sr;
	sa = Saw.ar(basefreq) + Saw.ar(basefreq*1.011);
	sb = Saw.ar(clip(basefreq*4.0+(freq/5*sa),5,20000)) + Saw.ar(clip(basefreq*4.021*(1+voice)+(freq/5*sa), 5, 20000));
	sound = LPF.ar(sa > sb, clip(freq*(voice+1),5,20000));
	sa = sa * sb.range(0.5,1);
	sound = (sound * sa) + ((1-sound) * sb) + BlitB3Tri.ar(basefreq, 0.99, 0.99, 0.2);
	sl = 0.5 * sound;
	sr = 0.5 * DelayN.ar(sound, 0.2, Line.ar(0.01,0.02,sustain));
	OffsetOut.ar(out, DirtPan.ar([sl,sr], ~dirt.numChannels, pan, env));
}).add;

SynthDef(\superphase, {|out, speed=1, sustain=1, decay=0, pan, freq, accelerate=0, voice=0 |
	var env = EnvGen.ar(Env.pairs([[0,0],[0.07,1],[0.2,1-decay],[0.95,1-decay],[1,0]], -1), timeScale:sustain, doneAction:2);
	var basefreq = freq * Line.kr(1, 1+accelerate, sustain);
	var fadj = [0.005, 0.007] * (voice+1) + 1.0;
	var fadj2 = 1.0 - ([0.005, 0.007] * (voice+1));
	var sound = Saw.ar(basefreq*fadj[0]) - DelayC.ar(Saw.ar(basefreq*fadj[1]), 0.2, SinOsc.kr(0.5).range(0.2,0.4)/basefreq);
	var fenv = EnvGen.ar(Env.perc(0.01, 2, 1, 2), timeScale:sustain);
	sound = sound + Saw.ar(basefreq*fadj2[0]) - DelayC.ar(Saw.ar(basefreq*fadj2[1]), 0.2, SinOsc.kr(0.5).range(0.2,0.4)/basefreq);
	sound = sound + PMOsc.ar(basefreq, basefreq*1.01, Line.kr(6.28,0));
	sound = sound - BAllPass.ar(sound, XLine.kr(4400,44,sustain), 2);
	sound = MoogFF.ar(sound/2, fenv*XLine.kr(10000, 500/speed+(basefreq/2), sustain), 1);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env))
}).add;

SynthDef(\superblip, {|out, sustain=1, pan, freq|
	var env = EnvGen.ar(Env.perc(0.050, 0.900, 1, -2), timeScale:sustain, doneAction:2);
	var sound = Pulse.ar(freq, Line.kr(0,1,sustain), 0.5);
	OffsetOut.ar(out, DirtPan.ar(sound, ~dirt.numChannels, pan, env));
}).add;

///////////////////////////////////////////////////////////////////////////////////////////////////
//// custom effects ////////////////////////////////////////////////////////////////////////////////

~dirt.addModule('lpf2', {|dirtEvent| dirtEvent.sendSynth("dirt_lpf2" ++ ~dirt.numChannels,
	[cutoff2: ~cutoff2, freq:~freq, resonance:~resonance, out: ~out])}, {~cutoff2.notNil});
SynthDef("dirt_lpf2"++~dirt.numChannels, {|out, cutoff2=0, resonance, freq|
	var sig;
	sig = In.ar(out, ~dirt.numChannels);
	sig = BLowPass4.ar(sig, freq*(1+cutoff2), resonance.linexp(0, 1, 1, 0.001));
	ReplaceOut.ar(out, sig);
}).add;

~dirt.addModule('amdist', {|dirtEvent| dirtEvent.sendSynth("dirt_amdist" ++ ~dirt.numChannels,
	[amdist: ~amdist, out: ~out])}, {~amdist.notNil});
SynthDef("dirt_amdist"++~dirt.numChannels, {|out, amdist|
	var sig, ampmod;
	sig = In.ar(out, ~dirt.numChannels);
	ampmod = (1.0 + PinkNoise.kr()) * 0.05 * amdist;
	sig = sig * SinOsc.ar([9800,9810]+PinkNoise.kr(10*amdist)); // modulate
	sig = sig + PinkNoise.ar(ampmod); // add noise
	sig = sig * SinOsc.ar([9800,9810]+PinkNoise.kr(10*amdist)); // demodulate
	ReplaceOut.ar(out, sig);
}).add;

// actually a stereo field distortion now
~dirt.addModule('fmdist', {|dirtEvent| dirtEvent.sendSynth("dirt_fmdist" ++ ~dirt.numChannels,
[fmdist: ~fmdist, fmdistf: ~fmdistf, out: ~out])}, {~fmdist.notNil});
SynthDef("dirt_fmdist"++~dirt.numChannels, {|out, fmdist, fmdistf|
	var dry, wet, lpr, lmr;
	dry = In.ar(out, ~dirt.numChannels);
	lpr = dry[0]+dry[1];
	lmr = dry[0]-dry[1];
	lmr = RLPF.ar(lmr, 800, 0.1);
	lmr = lmr + WhiteNoise.ar(fmdist*0.01);
	lmr = tanh(fmdist*lmr);
	wet = [lpr+lmr, lpr-lmr];
	ReplaceOut.ar(out, SelectX.ar(fmdist, [dry, 0.5*wet]));
}).add;

~dirt.addModule('allpass', {|dirtEvent| dirtEvent.sendSynth("dirt_allpass" ++ ~dirt.numChannels,
[allpass: ~allpass, out: ~out])}, {~allpass.notNil});
SynthDef("dirt_allpass"++~dirt.numChannels, {|out, allpass|
	var dry, wet, rez, imz, zsq, th, mag;
	th = SinOsc.ar(1).range(0,3);
	mag = 0.99;
	rez = mag*cos(th);
	imz = mag*sin(th);
	zsq = (rez*rez) + (imz*imz);
	dry = In.ar(out, ~dirt.numChannels);
	wet = SOS.ar(dry, zsq, -2.0*rez, 1.0, 2.0*rez, -1.0*zsq);
	ReplaceOut.ar(out, SelectX.ar(allpass, [dry, wet]));
}).add;

// general EQ?
~dirt.addModule('sparkle', { |dirtEvent| dirtEvent.sendSynth("dirt_sparkle" ++ ~dirt.numChannels,
			[sparkle: ~sparkle, sparklef: ~sparklef, out: ~out])}, { ~sparkle.notNil});
SynthDef("dirt_sparkle" ++ ~dirt.numChannels, {|out, sparkle, sparklef|
	var signal, freq;
	signal = In.ar(out, ~dirt.numChannels);
	freq = Select.kr(sparklef > 0, [DC.kr(2000), sparklef]);
	signal = BHiShelf.ar(signal, freq, 0.9, sparkle*10);
	// signal = Limiter.ar(signal);
	ReplaceOut.ar(out, signal);
}).add;

// Sonic Pi's krush
// moified a bit so krush "0" is the same as dry signal
~dirt.addModule('krush', { |dirtEvent| dirtEvent.sendSynth("dirt_krush" ++ ~dirt.numChannels,
			[krush: ~krush, kcutoff: ~kcutoff, out: ~out])}, { ~krush.notNil});
SynthDef("dirt_krush" ++ ~dirt.numChannels, {|out, krush, kcutoff|
	var orig, signal, freq;
	freq = Select.kr(kcutoff > 0, [DC.kr(4000), kcutoff]);
	orig = In.ar(out, ~dirt.numChannels);
	signal = (orig.squared + (krush*orig)) / (orig.squared + (orig.abs * (krush-1.0)) + 1.0);
	signal = RLPF.ar(signal, clip(freq, 20, 10000), 1);
	signal = SelectX.ar(krush*2.0, [orig, signal]);
	ReplaceOut.ar(out, signal);
}).add;

// ringmod
~dirt.addModule('ring', { |dirtEvent| dirtEvent.sendSynth("dirt_ring" ++ ~dirt.numChannels,
			[ring: ~ring, ringf: ~ringf, ringdf: ~ringdf, out: ~out])}, { ~ring.notNil});
SynthDef("dirt_ring" ++ ~dirt.numChannels, {|out, ring=0, ringf=0, ringdf|
	var signal, mod;
	signal = In.ar(out, ~dirt.numChannels);
	mod = ring * SinOsc.ar(Clip.kr(XLine.kr(ringf, ringf+ringdf), 20, 20000));
	signal = ring1(signal, mod);
	ReplaceOut.ar(out, signal);
}).add;

// Sonic Pi's octaver
~dirt.addModule('octer', { |dirtEvent| dirtEvent.sendSynth("dirt_octer" ++ ~dirt.numChannels,
	[octer: ~octer, octersub: ~octersub, octersubsub: ~octersubsub, out: ~out])},
    { ~octer.notNil or: {~octersub.notNil } or: {~octersubsub.notNil}});
SynthDef("dirt_octer" ++ ~dirt.numChannels, {|out, octer, octersub, octersubsub|
	var signal, oct1, oct2, oct3, sub;
	signal = In.ar(out, ~dirt.numChannels);
	oct1 = 2.0 * LeakDC.ar( abs(signal) );
	sub = LPF.ar(signal, 440);
	oct2 = ToggleFF.ar(sub);
	oct3 = ToggleFF.ar(oct2);
	signal = SelectX.ar(octer, [signal, octer*oct1, DC.ar(0)]);
	signal = signal + (octersub * oct2 * sub) + (octersubsub * oct3 * sub);
	ReplaceOut.ar(out, signal);
}).add;

// trying a limiter/depopper/shaper
~dirt.addModule('wall', { |dirtEvent| dirtEvent.sendSynth("dirt_wall" ++ ~dirt.numChannels,
			[wall: ~wall, walllev: ~walllev, out: ~out])}, { ~wall.notNil});
SynthDef("dirt_wall" ++ ~dirt.numChannels, {|out, wall=0, walllev=0.5|
	var signal, mod;
	signal = In.ar(out, ~dirt.numChannels);
	mod = signal - signal.clip2(walllev/2+0.5);
	signal = signal.clip2(walllev/2+0.5) + ((1.0-wall)*mod);
	ReplaceOut.ar(out, signal);
}).add;

// add envelope to lpf
~dirt.addModule('lpf',{ |dirtEvent| dirtEvent.sendSynth("dirt_lpf" ++ ~dirt.numChannels,
			[cutoff: ~cutoff, resonance: ~resonance, out: ~out, fattack:~fattack, fhold:~fhold, frelease:~frelease,
		    fenv:~fenv, flfo:~flfo, flfof:~flfof])}, { ~cutoff.notNil });
SynthDef("dirt_lpf" ++ numChannels, { |out, cutoff = 440, resonance = 0, fattack=0, fhold=1, frelease=0, fenv=0, flfo,flfof,sustain|
	var signal = In.ar(out, numChannels);
	var env = Lag.ar(EnvGen.ar(Env.linen(fattack, fhold, frelease, 1, 0)), 0.01);
	var lfo = flfo * SinOsc.kr(flfof,0) + 1;
	// signal = RLPF.ar(signal, clip(cutoff+(env*fenv),2,20000), resonance.linexp(0, 1, 1, 0.001));
	signal = BLowPass4.ar(signal, clip(lfo*(cutoff + (env*fenv)), 2, 20000), resonance.linexp(0,1,1,0.001));
	ReplaceOut.ar(out, signal)
}, [\ir, \ir, \ir]).add;

// delightfully crunchy
~dirt.addModule('distort', { |dirtEvent| dirtEvent.sendSynth("dirt_distort" ++ ~dirt.numChannels,
			[distort: ~distort, out: ~out])}, { ~distort.notNil});
SynthDef("dirt_distort" ++ ~dirt.numChannels, {|out, distort=0|
	var signal, mod ;
	signal = In.ar(out, ~dirt.numChannels);
	mod = CrossoverDistortion.ar(signal, amp:0.2, smooth:0.01);
	mod = mod + (0.1 * distort * DynKlank.ar(`[[60,61,240,3000+SinOsc.ar(62,mul:100)],nil,[0.1, 0.1, 0.05, 0.01]], signal));
	mod = (mod.cubed * 8).softclip * 0.5;
	mod = SelectX.ar(distort, [signal, mod]);
	ReplaceOut.ar(out, mod);
}).add;

// this compressor is per-voice, so no memory from one sample to the next, would need a global effect for that
~dirt.addModule('comp', { |dirtEvent|
		dirtEvent.sendSynth("dirt_comp" ++ ~dirt.numChannels,
			[comp: ~comp, compa: ~compa, compr: ~compr, out: ~out])},
{ ~comp.notNil});
SynthDef("dirt_comp" ++ ~dirt.numChannels, {|out, comp=0, compa=0, compr=0|
	var signal, mod, inlev, cgain;
	signal = In.ar(out, ~dirt.numChannels);
	inlev = Amplitude.ar(HPF.ar(signal, 600, 1.5), 0.01+compa, 0.02+compr);
	mod = signal / max( inlev, 0.1) * 0.3;
	mod = SelectX.ar(comp, [signal, mod]);
	ReplaceOut.ar(out, mod);
}).add;

// fm with a sine wave
~dirt.addModule('fm', { |dirtEvent|
	dirtEvent.sendSynth('dirt_fm' ++ ~dirt.numChannels,
		[fm: ~fm,	fmf: ~fmf,  out: ~out	]
	)
}, { ~fm.notNil or: { ~fmf.notNil } }); // play synth only if at least one of the two was given
SynthDef("dirt_fm" ++ numChannels, { |out, fm, fmf |
	var signal, mod;
	signal = In.ar(out, numChannels);
	mod = SinOsc.ar(fmf * (1.0 + fm * signal), mul:fm);
	ReplaceOut.ar(out, mod + (1.0-fm)*signal);
}).add;

~dirt.addModule('rectify', { |dirtEvent|
	dirtEvent.sendSynth('dirt_rectify' ++ ~dirt.numChannels,
		[rect: ~rect, rectoff: ~rectoff, out: ~out])}, { ~rect.notNil});
SynthDef("dirt_rectify" ++ numChannels, { |out, rect, rectoff=0 |
	var signal;
	signal = In.ar(out, numChannels);
	ReplaceOut.ar(out, signal.fold(rectoff-rect, rect+rectoff) / rect);
}).add;

~dirt.addModule('wrap', { |dirtEvent|
	dirtEvent.sendSynth('dirt_wrap' ++ ~dirt.numChannels,
		[wrap: ~wrap, wrapoff: ~wrapoff, out: ~out])}, { ~wrap.notNil});
SynthDef("dirt_wrap" ++ numChannels, { |out, wrap, wrapoff=0 |
	var signal;
	signal = In.ar(out, numChannels);
	ReplaceOut.ar(out, signal.wrap(wrapoff-wrap, wrap+wrapoff) / wrap);
}).add;

// not actually a saw anymore, just a "chopper"
~dirt.addModule('envsaw', { |dirtEvent|
	dirtEvent.sendSynth('dirt_envsaw' ++ ~dirt.numChannels,
		[envsaw: ~envsaw, envsawf: ~envsawf, out: ~out])}, { ~envsaw.notNil;});
SynthDef("dirt_envsaw" ++ numChannels, { |out, envsaw, envsawf=10 |
	var signal, mod;
	signal = In.ar(out, numChannels);
	mod = Lag.ar(LFPulse.ar(envsawf, iphase:0, width:0.5), 0.050) * envsaw + 1.0 - envsaw;
	// mod = ((Saw.ar(envsawf) * (-1)) + 1.0) * 0.5 * envsaw + 1.0 - envsaw;
	ReplaceOut.ar(out, signal * mod);
}).add;

// reverb, tails will be cut off
// this is really rough on the CPU if you stack it up, cut things off!
~dirt.addModule('boom', { |dirtEvent| dirtEvent.sendSynth("dirt_boom" ++ ~dirt.numChannels,
			[boom: ~boom, out: ~out])}, { ~boom.notNil});
SynthDef("dirt_boom" ++ ~dirt.numChannels, {|out, boom=0|
	var signal, wet;
	signal = In.ar(out, ~dirt.numChannels);
	wet = JPverb.ar(signal, 0.1, damp:0.5, size:0.5, earlyDiff:1, modDepth:0.7, modFreq:2, low:1, mid:0.1, high:0.1);
	ReplaceOut.ar(out, SelectX.ar(boom, [signal, wet]));
}).add;

// an alternative to boom, might be less CPU-hungry
~dirt.addModule('gboom', { |dirtEvent| dirtEvent.sendSynth("dirt_gboom" ++ ~dirt.numChannels,
			[gboom: ~gboom, out: ~out])}, { ~gboom.notNil});
SynthDef("dirt_gboom" ++ ~dirt.numChannels, {|out, gboom=0|
	var signal, wet;
	signal = In.ar(out, ~dirt.numChannels);
	//wet = (abs(Amplitude.ar(signal, 0.01, 0.1)) > 0.2) * signal;
	wet = (abs(signal) > 0.2) * signal;
	wet = FreeVerb.ar(signal, 1, 0.1, 1);
	ReplaceOut.ar(out, SelectX.ar(gboom, [signal, wet]));
}).add;
*/


// See built-in modules
//~dirt.modules;


