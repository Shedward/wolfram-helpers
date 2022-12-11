(* ::Package:: *)

BeginPackage["CG`"];


Mix::usage = "
Mix[a, b, t] returns the linear interpolation of a and b based on weight w. \\
a and b are either both scalars or both vectors of the same length. \
The weight w may be a scalar or a vector of the same length as a and b. \
w can be any value (so is not restricted to be between zero and one); \ 
If w has values outside the [0,1] range, it actually extrapolates. \
";
Lerp::usage = "
Alias for Mix
";


InvMix::usage = "
InvMix[a, b, v] returns a measure of value between a and b \
where 0.0 means value is equal to a and 1.0 means values is equal to b
";
InvLerp::usage = "
Alias for InvMix
";


Frac::usage = "
Frac[x] returns the fractional portion of value
";


Remap::usage = "
Remap[mini, maxi, mino, maxo, v] returns a value in scale mino-maxo \
keeping the same relative position as v have in scale mini-maxi
";


Begin["`Private`"];


Mix[a_, b_, t_] := (1 - t) a + b t;
Mix[a_, b_] :=  Mix[a, b, #]&;
Lerp = Mix;


InvMix[a_, b_, v_] :=(v - a)/(b - a);
InvMix[a_, b_] := InvMix[a, b, #]&;
InvLerp = InvMix;


Remap[mini_, maxi_, mino_, maxo_, v_] := Mix[mino, maxo] @* InvMix[mini, maxi] @ v;
Remap[mini_, maxi_, mino_, maxo_] := Remap[mini, maxi, mino, maxo];


Frac[x_]:= x - Floor[x];


End[];


EndPackage[];
