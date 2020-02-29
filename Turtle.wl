(* ::Package:: *)

BeginPackage["Turtle`"];


(* ::Section:: *)
(*Parse turtle directives to graphics primitives*)


ClearAll[ParseDirective];
Options[ParseDirective] = {
	InitialState -> {{0, 0}, 0}, (* {pos, orient} *)
	"Forward" -> {1},
	"forward" -> {1},
	"Bend" -> {\[Pi]/2, 1},
	"Left" -> {\[Pi]/2},
	"Right" -> {\[Pi]/2}
};
ParseDirective[directiveList_List, OptionsPattern[]] := Module[
	{
		stateStack={},
		pos,orient,(* Stacked properties *)
		style,(* Unstacked properties *)
		historyParam=<|
			"Forward" -> OptionValue["Forward"],
			"forward" -> OptionValue["forward"],
			"Bend" -> OptionValue["Bend"],
			"Left" -> OptionValue["Left"],
			"Right" -> OptionValue["Right"]
		|>,
		output = {},
		ParseDirectiveRecur
	},
	(* Define recursion function *)
	ParseDirectiveRecur[directive_] := Module[{head = Head[directive], param},
		If[head === List,
			AppendTo[stateStack,{pos,orient}]; (* Push *)
			ParseDirectiveRecur/@directive; (* Parse each item of the list *)
			{{{pos,orient}},stateStack}=TakeDrop[stateStack,-1]; (* Pop *)
		,(*Else*)
			If[Length[directive] < 1,
				param = historyParam[head];,
				param = directive;
			];
			Switch[head,
			"Forward",
				AppendTo[output,
					Line[{pos, pos += AngleVector[orient]*param[[1]]}]
				];,
			"forward",
				pos += ReIm@Exp[I orient]*param[[1]];,
			"Bend",
				Block[{center, \[Theta]i},
					Which[
					param[[1]] > 0, 
						center = pos + AngleVector[orient+\[Pi]/2]*param[[2]];
						\[Theta]i = orient-\[Pi]/2;,
					param[[1]] < 0,
						center = pos + AngleVector[orient-\[Pi]/2]*param[[2]];
						\[Theta]i = orient+\[Pi]/2;
					];
					AppendTo[output, 
						Circle[center, param[[2]], \[Theta]i + {0, param[[1]]}]
					];
					pos = center + AngleVector[\[Theta]i+param[[1]]]*param[[2]]; orient += param[[1]];
				];,
			"Left",
				orient += param[[1]];,
			"Right",
				orient -= param[[1]];
			]
		]
	];
	(* Start iteration *)
	pos = OptionValue[InitialState][[1]]; orient = OptionValue[InitialState][[2]];
	ParseDirectiveRecur/@directiveList; (* Parse each item of the list *)
	Return[output];
]


EndPackage[];
