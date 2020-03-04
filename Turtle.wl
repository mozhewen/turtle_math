(* ::Package:: *)

BeginPackage["Turtle`"];


(* ::Section:: *)
(*ParseDirectives[]*)


(* ::Text:: *)
(*All directives are (Drawing directives with the first letter uppercase, non-drawing directives with the first letter lowercase)*)
(*1. Normal movements*)
(* 	"Forward"[distance]			Move forward by a certain distance and draw a line.*)
(*	"forward"[distance]			Move forward without drawing a line.*)
(*	"turn"[[angle]]					angle > 0 ( < 0) turn left (right).*)
(**)
(*	"Polar"[distance, [angle]]		Abbreviation for "turn"[angle], "Forward"[distance], "turn"[-angle]. *)
(*	"polar"[distance, [angle]]		Similar to "Polar"[] without drawing a line. *)
(*	"Cartesian"[dx, dy]				Use  local Cartesian Coordinates to guide movement, where the x-axis points to the front  and y-axis to the right.*)
(*	"cartesian"[dx, dy]				Similar to "Cartesian"[] without drawing a line.*)
(*	( You can use "P(p)olar"[distance] which means "P(p)olar"[distance,180\[Degree]] or  to achieve something like "B(b)ackward"[distance]. )*)
(*	(The "P(p)olar"[] and "C(c)artesian"[] directives do not change the orientation of the turtle. )*)
(*	*)
(*	"Bend"[angle, radius]			angle > 0 ( < 0): Make a round bend to the left (right) with a certain radius.*)
(**)
(*2. \:98db\:96f7\:795e\:306e\:8853*)
(*	"mark"[mark]					Mark the current position.*)
(*	"unmark"[mark]				Unmark a mark.*)
(*	"Goto"[mark, [proportion]]		Move to the position of mask and draw a  line. The turtle's orientation is left unchanged.*)
(*	"goto"[mark, [proportion]]		Move to the position of mask without drawing a line.*)
(*	"lookat"[mark, [proportion]]		 Set turtle's orientation to the mark.*)
(**)
(*3. Extras*)
(*	"Arrowhead"[]*)
(*	"Text"["text", offset]*)
(**)
(*4. Stylization*)
(*	"on"[style, parameters...]*)
(*	"off"[style]*)
(**)
(*5. Raw*)
(*	"echo"["raw_graphics_directives_string"]*)
(**)
(*(* TODO: *)*)
(*	Add measurement functions to measure distances and angles between marks.*)
(*	Add B-spline curves.*)
(*	Optimize using "SubstitutionSystem[]" and "AnglePath[]"?*)
(*	...*)


ParseDirectives::usage = "Parse the turtle's directives to Mathematica compatible graphics primitives. ";
ClearAll[ParseDirectives];
Options[ParseDirectives] = {
	InitialState -> {{0, 0}, 0} (* {pos, orient} *),
	ArrowStyle -> {{Automatic, 1/2}}, (* Arrowhead in the middle of line *)
	TextStyle -> Large, (* Large text *)
	CoiledStyle -> {0.1, 0.1, 3/4}, (* {pitch, width, ratio} *)
	WavyStyle -> {0.1, 0.1} (* {pitch, width} *)
};


Begin["`Private`"];


Mod2\[Pi][x_] := Mod[FullSimplify@x, 2\[Pi]];


ClearAll[Outer1];
Outer1[func_, a_, B_List] := Outer[func, {a}, B, 1][[1]];


(* Coiled *)
ClearAll[CoiledLine];
CoiledLine[{p1_List, p2_List}, pitchRef_, width_, ratio_] := Module[
	{vec = p2-p1, num, pitch, pts},
	num = Max[Round[2 Norm[vec]/Abs@pitchRef-4ratio], 0]; If[EvenQ[num], num++];
	pitch = 2Norm[vec]/(num+4ratio);
	pts = Outer1[Plus, p1+(1/2-(num pitch)/(4Norm[vec]))vec, Outer1[Times, (num pitch)/(2Norm[vec]) vec, Subdivide[2num]]];
	pts[[1;; ;;4]] = Outer1[Plus, -AngleVector[ArcTan@@vec]ratio pitch, pts[[1;; ;;4]]];
	pts[[2;; ;;4]] = Outer1[Plus, AngleVector[ArcTan@@vec+\[Pi]/2]width, pts[[2;; ;;4]]];
	pts[[3;; ;;4]] = Outer1[Plus, AngleVector[ArcTan@@vec] ratio pitch, pts[[3;; ;;4]]];
	If[num>1, pts[[4;; ;;4]] = Outer1[Plus, AngleVector[ArcTan@@vec-\[Pi]/2]width, pts[[4;; ;;4]]]];
	BSplineCurve[pts]
]

ClearAll[CoiledCircle];
CoiledCircle[center_List, radius_, {\[Theta]1_,\[Theta]2_}, pitchRef_, width_, ratio_] := Module[
	{\[Delta] = \[Theta]2-\[Theta]1, num, \[Alpha], \[Theta]s, pts},
	num = Max[Round[Abs[2 \[Delta] radius/pitchRef]-4ratio], 0]; If[EvenQ[num], num++];
	\[Alpha] = Abs[2\[Delta]]/(num+4ratio);
	\[Theta]s = \[Theta]1+1/2 \[Delta]-(num \[Alpha])/4+Subdivide[num/2, 2num]\[Alpha];
	\[Theta]s[[1;; ;;4]] -= ratio \[Alpha];
	\[Theta]s[[3;; ;;4]] += ratio \[Alpha];
	pts = Outer1[Plus, center, (AngleVector/@\[Theta]s)radius];
	pts[[2;; ;;4]] += (AngleVector/@(\[Theta]s[[2;; ;;4]] + UnitStep[\[Delta]]\[Pi]))width;
	If[num>1, pts[[4;; ;;4]] += (AngleVector/@(\[Theta]s[[4;; ;;4]] + UnitStep[-\[Delta]]\[Pi]))width];
	BSplineCurve[pts]
]


(* Wavy *)
ClearAll[WavyLine];
WavyLine[{p1_List, p2_List}, pitchRef_, width_] := Module[
	{vec = p2-p1, num, pts},
	num = Abs@Round[2 Norm[vec]/pitchRef];
	pts = Outer1[Plus, p1, Outer1[Times, vec, Subdivide[If[num == 0, 1, 2num]]]];
	If[num > 0, pts[[2;; ;;4]] = Outer1[Plus, AngleVector[ArcTan@@vec+\[Pi]/2]width, pts[[2;; ;;4]]]];
    If[num > 1, pts[[4;; ;;4]] = Outer1[Plus, -AngleVector[ArcTan@@vec+\[Pi]/2]width, pts[[4;; ;;4]]]];
    BSplineCurve[pts]
]

ClearAll[WavyCircle];
WavyCircle[center_List, radius_, {\[Theta]1_, \[Theta]2_}, pitchRef_, width_] := Module[
	{\[Delta] = \[Theta]2-\[Theta]1, num, \[Theta]s, pts},
	num = Abs@Round[2 \[Delta] radius/pitchRef];
	\[Theta]s = \[Theta]1 + Subdivide[If[num==0, 1, 2num]]\[Delta];
	pts = Outer1[Plus, center, (AngleVector/@\[Theta]s)radius];
	If[num > 0, pts[[2;; ;;4]] += (AngleVector/@(\[Theta]s[[2;; ;;4]] + UnitStep[\[Delta]]\[Pi]))width];
	If[num > 1, pts[[4;; ;;4]] += (AngleVector/@(\[Theta]s[[4;; ;;4]] + UnitStep[-\[Delta]]\[Pi]))width];
	BSplineCurve[pts]
]


ParseDirectives[directiveList_List, OptionsPattern[]] := Module[
	{
		stateStack = {},
		(* Stacked properties *)
		pos, orient,
		(* Unstacked properties *)
		marks = <||>,
		(*   Arrow *)
		ArrowFunc = Identity,
		(*   Coiled & Wavy *)
		LineFunc = Line, CircleFunc = Circle,

		(* Output. Initialized with global directives *)
		output = {CapForm["Round"]},

		ParseDirectivesRecur
	},
	(* Define recursion function *)
	ParseDirectivesRecur[directive_] :=
		If[Head@directive === List,
			AppendTo[stateStack, {pos, orient}]; (* Push *)
			ParseDirectivesRecur/@directive; (* Parse each item of the list *)
			{{{pos, orient}}, stateStack} = TakeDrop[stateStack, -1]; (* Pop *)
		,(*Else*)
			Switch[Head@directive,
			(* 1. *)
			"Forward" | "Fd",
				AppendTo[output,
					ArrowFunc@LineFunc[{pos, pos += AngleVector[orient]directive[[1]]}]
				];
			,"forward" | "fd",
				pos += AngleVector[orient]directive[[1]];
			,"turn" | "tn",
				If[Length[directive] >= 1,
					orient = Mod2\[Pi][orient + directive[[1]] ];,
					orient = Mod2\[Pi][orient + \[Pi]];
				]
			,"Polar" | "Pol",
				AppendTo[output,
					If[Length[directive] >= 1,
						ArrowFunc@LineFunc[{pos, pos += AngleVector[orient + directive[[2]] ]directive[[1]]}],
						ArrowFunc@LineFunc[{pos, pos -= AngleVector[orient]directive[[1]]}]
					]
				];
			,"polar" | "pol",
				pos += AngleVector[orient + directive[[2]] ]directive[[1]];
			,"Cartesian" | "Cart",
				AppendTo[output,
					ArrowFunc@LineFunc[{pos, pos += AngleVector[orient]directive[[1]] + AngleVector[orient+\[Pi]/2]directive[[2]]}]
				];
			,"cartesian" | "cart",
				pos += AngleVector[orient]directive[[1]] + AngleVector[orient+\[Pi]/2]directive[[2]];

			,"Bend",
				Block[{center, \[Theta]i},
					Which[
					directive[[1]] > 0, 
						center = pos + AngleVector[orient + \[Pi]/2] directive[[2]];
						\[Theta]i = orient - \[Pi]/2;,
					directive[[1]] < 0,
						center = pos + AngleVector[orient - \[Pi]/2] directive[[2]];
						\[Theta]i = orient + \[Pi]/2;
					];
					AppendTo[output, 
						CircleFunc[center, directive[[2]], \[Theta]i + {0, directive[[1]]}]
					];
					pos = center + AngleVector[\[Theta]i + directive[[1]]]directive[[2]];
					orient = Mod2\[Pi][orient + directive[[1]]];
				];

			(* 2. *)
			,"mark",
				AppendTo[marks, directive[[1]] -> pos];
			,"unmark",
				Delete[marks, Key[ directive[[1]] ]];
			,"Goto",
				AppendTo[output,
					If[Length[directive] >= 2,
						ArrowFunc@LineFunc[{pos, pos = (1 - directive[[2]])pos + directive[[2]]marks[ directive[[1]] ]}],
						ArrowFunc@LineFunc[{pos, pos = marks[ directive[[1]] ]}]
					]
				];
			,"goto",
				If[Length[directive] >= 2,
					pos = (1 - directive[[2]])pos + directive[[2]]marks[ directive[[1]] ];,
					pos = marks[ directive[[1]] ];
				]
			,"lookat",
				If[Length[directive] >= 2,
					orient = orient + directive[[2]]Mod2\[Pi][ArcTan@@(marks[ directive[[1]] ] - pos) - orient];,
					orient = ArcTan@@(marks[ directive[[1]] ] - pos);
				]

			(* 3. *)
			,"Arrowhead",
				AppendTo[output, Arrow[{pos, Scaled[AngleVector[orient]*0.01, pos]}]];
			,"Text",
				AppendTo[output,
					Text[Style[ directive[[1]] , OptionValue[TextStyle]], pos, directive[[2]]]
				];

			(* 4. Stylization *)
			,"on",
				Switch[directive[[1]],
				"Arrow",
					ArrowFunc = Arrow;
					AppendTo[output,
						If[Length[directive] >= 2,
							Arrowheads[ directive[[2]] ],
							Arrowheads[OptionValue[ArrowStyle]]
						]
					];
				,"Dashed",
					AppendTo[output, Dashed];
				,"Coiled",
					LineFunc = CoiledLine[#, Sequence@@OptionValue[CoiledStyle]]&;
					CircleFunc = CoiledCircle[#1, #2, #3, Sequence@@OptionValue[CoiledStyle]]&;
				,"Wavy",
					LineFunc = WavyLine[#, Sequence@@OptionValue[WavyStyle]]&;
					CircleFunc = WavyCircle[#1, #2, #3, Sequence@@OptionValue[WavyStyle]]&;
				];
			,"off",
				Switch[directive[[1]],
				"Arrow",
					ArrowFunc = Identity;
					AppendTo[output,
						Arrowheads[OptionValue[ArrowStyle]]
					];
				,"Dashed",
					AppendTo[output, Dashing[None]];
				,"Coiled" | "Wavy",
					LineFunc = Line;
					CircleFunc = Circle;
				];

			(* Raw *)
			,"echo",
				AppendTo[output, ToExpression[ directive[[1]] ]];
			];
		];

	(* Start recursion *)
	pos = OptionValue[InitialState][[1]];
	orient = OptionValue[InitialState][[2]];
	(*   Parse each item of the list *)
	ParseDirectivesRecur/@directiveList;
	Return[output];
];


End[]; (*`Private`*)


EndPackage[];
