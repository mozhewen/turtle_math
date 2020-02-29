(* ::Package:: *)

BeginPackage["Turtle`"];


(* ::Section:: *)
(*ParseDirectives[]*)


(* ::Text:: *)
(*All directives are (Drawing directives with the first letter uppercase, non-drawing directives with the first letter lowercase)*)
(*1. Normal movements*)
(* 	"Forward"[distance]			Move forward by a certain distance and draw a line.*)
(*	"forward"[distance]			Move forward without drawing a line.*)
(*	"Bend"[angle, radius]			angle > 0 ( < 0) bend to the left (right) with a certain radius.*)
(*	"turn"[angle]					angle > 0 ( < 0) turn left (right).*)
(**)
(*2. \:98db\:96f7\:795e\:306e\:8853*)
(*	"mark"[mark]					Mark the current position.*)
(*	"unmark"[mark]				Unmark a mark.*)
(*	"Goto"[mark, [proportion]]		Move to the position of mask and draw a  line. The turtle's orientation is left unchanged.*)
(*	"goto"[mark, [proportion]]		Move to the position of mask without drawing a line.*)
(*	"lookat"[mark, [proportion]]		 Set turtle's orientation to the mark.*)
(**)
(*3. Extras*)
(*	"Arrow"[]*)
(*	"Text"["text", offset]*)
(*	"echo"["graphics_directives_string"]*)
(*	"on"[context]*)
(*	"off"[context]*)
(*(* TODO: *)*)
(*	Optimize using "SubstitutionSystem[]" and "AnglePath[]"?*)
(*	Simplify usage of echo[]*)
(*	...*)


ParseDirectives::usage = "Parse the turtle's directives to Mathematica compatible graphics primitives. ";
ClearAll[ParseDirectives];
Options[ParseDirectives] = {
	InitialState -> {{0, 0}, 0} (* {pos, orient} *),
	WavyStyle -> {0.1, 0.1} (* {pitch, width} *)
};


Begin["`Private`"];

Mod2\[Pi][x_] := Mod[x, 2\[Pi]];

ClearAll[WavyLine];
WavyLine[{p1_List, p2_List}, pitch_, width_] := Module[
	{vec = p2-p1, pts, newHead, num},
	num = Round[2 Norm[vec]/pitch];
	pts = newHead@@p1 + Subdivide[If[num==0, 1, 2 num]]newHead@@vec;
	If[num>0, pts[[2;;;;4]] += (newHead@@AngleVector[ArcTan@@vec+\[Pi]/2])width];
    If[num>1, pts[[4;;;;4]] -= (newHead@@AngleVector[ArcTan@@vec+\[Pi]/2])width];
    BSplineCurve[pts]/.newHead -> List
]

ClearAll[WavyCircle];
WavyCircle[center_List, radius_, {\[Theta]1_, \[Theta]2_}, pitch_, width_] := Module[
	{\[Delta] = \[Theta]2-\[Theta]1, pts, newHead, num},
	num=Round[2 \[Delta] radius/pitch];
	pts=(AngleVector/@(\[Theta]1+Subdivide[If[num==0,1,2num]]*\[Delta]));
	If[num>0,pts[[2;;;;4]]*=(radius-width)/radius];
	If[num>1,pts[[4;;;;4]]*=(radius+width)/radius];
	BSplineCurve[newHead@@center+pts]/.newHead -> List
]


ParseDirectives[directiveList_List, OptionsPattern[]] := Module[
	{
		stateStack = {},
		(* Stacked properties *)
		pos, orient,
		(* Unstacked properties *)
		marks = <||>,
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
			"Forward",
				AppendTo[output,
					LineFunc[{pos, pos += AngleVector[orient]directive[[1]]}]
				];
			,"forward",
				pos += AngleVector[orient]directive[[1]];
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
			,"turn",
				orient = Mod2\[Pi][orient + directive[[1]]];

			(* 2. *)
			,"mark",
				AppendTo[marks, directive[[1]] -> pos];
			,"unmark",
				Delete[marks, Key[ directive[[1]] ]];
			,"Goto",
				AppendTo[output,
					LineFunc[{pos, pos = (1 - directive[[2]])pos + directive[[2]]marks[ directive[[1]] ]}]
				];
			,"goto",
				pos = (1-directive[[2]])pos + directive[[2]]marks[ directive[[1]] ];
			,"lookat",
				orient = orient + directive[[2]]Mod2\[Pi][ArcTan@@(marks[ directive[[1]] ] - pos) - orient];

			(* 3. *)
			,"Arrow",
				AppendTo[output, Arrow[{pos, Scaled[AngleVector[orient]*0.01, pos]}]];
			,"Text",
				AppendTo[output, Text[ directive[[1]] , pos, directive[[2]]]];
			,"echo",
				AppendTo[output, ToExpression[ directive[[1]] ]];
			,"on",
				Switch[directive[[1]],
				"Wavy",
					LineFunc = WavyLine[#, Sequence@@OptionValue[WavyStyle]]&;
					CircleFunc = WavyCircle[#1, #2, #3, Sequence@@OptionValue[WavyStyle]]&;
				];
			,"off",
				Switch[directive[[1]],
				"Wavy",
					LineFunc = Line;
					CircleFunc = Circle;
				];
			]
		];
	(* Start recursion *)
	pos = OptionValue[InitialState][[1]];
	orient = OptionValue[InitialState][[2]];
	(*   Parse each item of the list *)
	ParseDirectivesRecur/@directiveList;
	Return[output];
];

End[];


EndPackage[];
