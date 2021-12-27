(* ::Package:: *)

BeginPackage["Turtle`"];


(* ::Section:: *)
(*ParseDirectives[]*)


(* ::Text:: *)
(*All directives are listed below (Drawing directives with the first letter uppercase, non-drawing directives with the first letter lowercase)*)
(*1. Normal movements*)
(* 	"Forward"[distance]					Move forward by a certain distance and draw a line. If distance < 0, the orientation of the turtle will be reversed. *)
(* 	"Forward"[distance, mark]				... and mark the destination as mark. *)
(* 	"Forward"[distance, prop1->mark1, ...]		... and mark the intermediate points at proportions prop1,... as mark1,... *)
(*	"forward"[...]							Move forward without drawing a line. *)
(**)
(*	"turn"[[angle]]							angle > 0 ( < 0) turn left (right). If angle is omitted, turn back. *)
(**)
(*	"Polar"[distance, angle]					Abbreviation for "turn"[angle], "Forward"[distance], "turn"[-angle]. *)
(*	"Polar"[distance, angle, mark]*)
(*	"Polar"[distance, angle, prop1->mark1, ...]*)
(*	"polar"[...]							Similar to "Polar"[] without drawing a line. *)
(**)
(*	"Cartesian"[dx, dy]						Use  local Cartesian Coordinates to guide movement, where the x-axis points to the front  and y-axis to the left. *)
(*	"Cartesian"[dx, dy, mark]*)
(*	"Cartesian"[dx, dy, prop1->mark1, ...]*)
(*	"cartesian"[...]							Similar to "Cartesian"[] without drawing a line. *)
(**)
(*	(NOTE: The "P(p)olar"[] and "C(c)artesian"[] directives do not change the orientation of the turtle. )*)
(**)
(*	"Bend"[angle, radius]					angle > 0 ( < 0): Make a round bend to the left (right) with a certain radius. *)
(*	"Bend"[angle, radius, mark]*)
(*	"Bend"[angle, radius, prop1->mark1, ...]*)
(**)
(*2. \:98db\:96f7\:795e\:306e\:8853*)
(*	"mark"[mark]							Mark the current position and orientation. *)
(*	"unmark"[mark]						Unmark a mark. *)
(*	"Goto"[mark[, prop1->mark1, ...]]			Move to the position of mark and draw a  line. The turtle's orientation is set to the marked orientation. *)
(*	"goto"[mark[, prop1->mark1, ...]]			Move to the position of mark without drawing a line. *)
(*	"lookat"[mark[, proportion]]		 		Set turtle's orientation to look at the mark. The turtle's position is left unchanged. *)
(**)
(*3. Extras*)
(*	"Arrowhead"[[mark]]					Add an arrowhead at the current position or at the position of mark. The turtle will not go to the marked position. *)
(*	"Text"["text"[, offset, mark]]				Add text at the current position with offset or at the position of mark. The turtle will not go to the marked position. *)
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


Begin["`Private`"];


Mod2\[Pi][x_] := FullSimplify@Mod[FullSimplify[x], 2\[Pi]];


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


offsetTable = <|
	"left" -> {1, 0},
	"right" -> {-1, 0},
	"above" -> {0, -1},
	"below" -> {0, 1},
	"above left" -> {1, -1},
	"above right" -> {-1, -1},
	"below left" -> {1, 1},
	"below right" -> {-1, 1}
|>;


ParseDirectives::invalid="Invalid directive ``. ";
Options[ParseDirectives] = {
	InitialState -> {{0, 0}, 0} (* {pos, orient} *),
	ArrowStyle -> {{Automatic, 1/2}}, (* Arrowhead in the middle of line *)
	TextStyle -> Sequence[], (* Large text *)
	CoiledStyle -> {0.1, 0.1, 3/4}, (* {pitch, width, ratio} *)
	WavyStyle -> {0.1, 0.1} (* {pitch, width} *)
};
ParseDirectives[directiveList_List, OptionsPattern[]] := Module[
	{
		stateStack = {},
		(* Stacked properties *)
		pos, orient, newPos, newOrient, 
		(* Unstacked properties *)
		marks = <||>,
		(*   Arrow *)
		ArrowFunc = Identity,
		(*   Coiled & Wavy *)
		LineFunc = Line, CircleFunc = Circle,

		(* Output. Initialized with global directives *)
		output = {CapForm["Round"], Arrowheads[OptionValue[ArrowStyle]]},

		(* Local functions *)
		AddMarks,
		ParseDirectivesRecur
	},
	AddMarks[newMarks___] :=
		Do[Switch[m,
				_->_, AppendTo[marks, m[[2]] -> {(pos+m[[1]](newPos - pos)), orient}],
				_, AppendTo[marks, m -> {newPos, orient}]
			], {m, {newMarks}}];
	(* Define recursion function *)
	ParseDirectivesRecur[directive_] :=
		If[Head@directive === List,
			AppendTo[stateStack, {pos, orient}]; (* Push *)
			ParseDirectivesRecur/@directive; (* Parse each item of the list *)
			{{{pos, orient}}, stateStack} = TakeDrop[stateStack, -1]; (* Pop *)
		,(*Else*)
			Switch[directive,
			(* 1. *)
			("Forward" | "F")[__],
				newPos = pos + AngleVector[orient]directive[[1]];
				AddMarks@@directive[[2;;]];
				AppendTo[output, ArrowFunc@LineFunc[{pos, pos = newPos}]];
				If[directive[[1]] < 0, orient = Mod2\[Pi][orient + \[Pi]]];
			,("forward" | "f")[__],
				newPos = pos + AngleVector[orient]directive[[1]];
				AddMarks@@directive[[2;;]];
				pos = newPos;

			,("turn" | "t")[_:\[Pi]],
					orient = Mod2\[Pi][orient + First[directive, \[Pi]]];

			,("Polar" | "Pol")[_, _, ___],
				newPos = pos + AngleVector[orient + directive[[2]] ]directive[[1]];
				AddMarks@@directive[[3;;]];
				AppendTo[output, ArrowFunc@LineFunc[{pos, pos = newPos}]];
			,("polar" | "pol")[_, _, ___],
				newPos = pos + AngleVector[orient + directive[[2]] ]directive[[1]];
				AddMarks@@directive[[3;;]];
				pos = newPos;

			,("Cartesian" | "Cart")[_, _, ___],
				newPos = pos + AngleVector[orient]directive[[1]] + AngleVector[orient+\[Pi]/2]directive[[2]];
				AddMarks@@directive[[3;;]];
				AppendTo[output, ArrowFunc@LineFunc[{pos, pos = newPos}]];
			,("cartesian" | "cart")[_, _, ___],
				newPos = pos + AngleVector[orient]directive[[1]] + AngleVector[orient+\[Pi]/2]directive[[2]];
				AddMarks@@directive[[3;;]];
				pos = newPos;

			,("Bend" | "B")[_, _, ___],
				Block[{center, \[Theta]i},
					Which[
					directive[[1]] > 0, 
						center = pos + AngleVector[orient + \[Pi]/2] directive[[2]];
						\[Theta]i = orient - \[Pi]/2;,
					directive[[1]] < 0,
						center = pos + AngleVector[orient - \[Pi]/2] directive[[2]];
						\[Theta]i = orient + \[Pi]/2;
					];
					(* Marks *)
					newPos = center + AngleVector[\[Theta]i + directive[[1]]]directive[[2]];
					newOrient = Mod2\[Pi][orient + directive[[1]]];
					Do[Switch[m,
						_->_, AppendTo[marks, m[[2]] -> {(center + AngleVector[\[Theta]i + m[[1]] directive[[1]]]directive[[2]]), Mod2\[Pi][orient + m[[1]] directive[[1]]]}],
						_, AppendTo[marks, m -> {newPos, newOrient}]
					], {m, List@@directive[[3;;]]}];
					(* Output *)
					AppendTo[output, 
						CircleFunc[center, directive[[2]], \[Theta]i + {0, directive[[1]]}]
					];
					{pos, orient} = {newPos, newOrient};
				];


			(* 2. *)
			,"mark"[_],
				AppendTo[marks, directive[[1]] -> {pos, orient}];
			,"unmark"[_],
				Delete[marks, Key[ directive[[1]] ]];
			,"Goto"[__],
				{newPos, newOrient} = marks[directive[[1]]];
				AddMarks@@directive[[2;;]];
				AppendTo[output, ArrowFunc@LineFunc[{pos, pos = newPos}]];
				orient = newOrient;
			,"goto"[__],
				{newPos, newOrient} = marks[directive[[1]]];
				AddMarks@@directive[[2;;]];
				{pos, orient}= {newPos, newOrient};
			,"lookat"[_, _:1],
				If[Length[directive] >= 2,
					orient = orient + directive[[2]]Mod2\[Pi][ArcTan@@(marks[directive[[1]]][[1]] - pos) - orient];,
					orient = ArcTan@@(marks[directive[[1]]][[1]] - pos);
				]


			(* 3. *)
			,"Arrowhead"[_:""],
				{newPos, newOrient} = If[Length[directive]>=1, marks[directive[[1]]], {pos, orient}];
				AppendTo[output, Arrow[{newPos, Scaled[AngleVector[newOrient]*0.01, newPos]}]];
			,"Text"[__],
				newPos = If[Length[directive]>=3, marks[directive[[3]]][[1]], pos];
				AppendTo[output,
					Text[Style[ directive[[1]] , Sequence@@OptionValue[TextStyle]], 
						newPos, If[Length[directive]>=2, directive[[2]]/.offsetTable, Unevaluated@Sequence[]]
					]
				];


			(* 4. Stylization *)
			,"on"[__],
				Switch[directive[[1]],
				"Arrow",
					ArrowFunc = Arrow;
					AppendTo[output,
						If[Length[directive] >= 2,
							Arrowheads[ directive[[2]] ],
							Arrowheads[OptionValue[ArrowStyle]]
						]
					];
				,Dashed | Dotted | DotDashed | _Dashing | _AbsoluteDashing,
					AppendTo[output, directive[[1]]];
				,Thick | Thin | _Thickness | _AbsoluteThickness,
					AppendTo[output, directive[[1]]];
				,"Coiled",
					LineFunc = CoiledLine[#, Sequence@@OptionValue[CoiledStyle]]&;
					CircleFunc = CoiledCircle[#1, #2, #3, Sequence@@OptionValue[CoiledStyle]]&;
				,"Wavy",
					LineFunc = WavyLine[#, Sequence@@OptionValue[WavyStyle]]&;
					CircleFunc = WavyCircle[#1, #2, #3, Sequence@@OptionValue[WavyStyle]]&;
				];
			,"off"[_],
				Switch[directive[[1]],
				"Arrow",
					ArrowFunc = Identity;
					AppendTo[output,
						Arrowheads[OptionValue[ArrowStyle]]
					];
				,Dashing,
					AppendTo[output, Dashing[None]];
				,Thickness,
					AppendTo[output, Thickness[Medium]];
				,"Coiled" | "Wavy",
					LineFunc = Line;
					CircleFunc = Circle;
				];


			(* 5. Raw *)
			,"echo"[_],
				AppendTo[output, directive[[1]]];


			,_,
				Message[ParseDirectives::invalid, directive];
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
