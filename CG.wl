(* ::Package:: *)

BeginPackage["CG`"];


Mix::usage = 
"Mix[a, b, t] return linear interpolation of a and b based on weight t.
Mix[a, b] represents operator form of Mix";

Lerp::usage = "Alias for Mix";


InvMix::usage = 
"InvMix[a, b, v] returns a weight of linear interpolation for value v
InvMix[a, b] represents operator form of InvMix";

InvLerp::usage = "Alias for InvMix";


Frac::usage = "Frac[x] returns the fractional portion of value";


Remap::usage = 
"Remap[mini, maxi, mino, maxo, v] returns a value in scale mino-maxo \
keeping the same relative position as v have in scale mini-maxi";


FragImage::usage = 
"FragImage[f, ...] create an Image rendered by function f[x, y], ";


AnimatedFragImage::usage = "AnimatedFragImage[frag, t] render AnimatedImage based on fragment iterating over t from 0 to 1 with step 0.05
AnimatedFragImage[frag, {t, tmin, tmax}] render AnimatedImage over specific range of t
AnimatedFragImage[frag, {t, tmin, tmax, tstep}] render AnimatedImage over specific range and spet";


Begin["`Private`"];


Mix[a_, b_, t_] := (1 - t) a + b t;
Mix[a_, b_] :=  Mix[a, b, #]&;
Lerp = Mix;


InvMix[a_, b_, v_] :=(v - a)/(b - a);
InvMix[a_, b_] := InvMix[a, b, #]&;
InvLerp = InvMix;


Remap[mini_, maxi_, mino_, maxo_, v_] := Mix[mino, maxo] @* InvMix[mini, maxi] @ v;
Remap[mini_, maxi_, mino_, maxo_] := Remap[mini, maxi, mino, maxo, #]&;


Frac[x_]:= x - Floor[x];


FragImage[
	frag_Function, 
	OptionsPattern[{
		"Size" -> {200, 200},
		"Range" -> {-1, 1},
		"ColorSpace" -> "Automatic"
	}]
]:= Module[
	{min, max, sizex, sizey},

	{min, max} = OptionValue["Range"];
	{sizex, sizey} = OptionValue["Size"];

	Image[
		Table[
			frag[x, y], 
			{x, min, max, (max - min) / sizex}, 
			{y, min, max, (max - min) / sizey}
		],
		ColorSpace -> OptionValue["ColorSpace"]
	]
];


AnimatedFragImage[frag_, {t_, tmin_, tmax_, tstep_:0.05}, args___] := AnimatedImage[Table[FragImage[frag, args], {t, tmin, tmax, tstep}]];
AnimatedFragImage[frag_, t_, args___] := AnimatedFragImage[frag, {t, 0, 1, 0.05}, args];


End[];


EndPackage[];
