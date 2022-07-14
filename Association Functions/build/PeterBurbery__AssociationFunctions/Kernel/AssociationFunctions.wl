(* ::Package:: *)

BeginPackage["PeterBurbery`AssociationFunctions`"];

(* Declare your packages public symbols here. *)

KeyValueMapGeneralized;
AssociationThrough;
AssociationPartition;
AssociationNormalize;
ConstantAssociation;
PropertiesSummary;
LinearOptimizationInformation;
QuadraticOptimizationInformation;
Begin["`Private`"];

(* Define your public and private symbols here. *)

KeyValueMapGeneralized[f_,association_?AssociationQ,keykey_,valuekey_]:=Association@KeyValueMap[#1-><|keykey->f[#1],valuekey->#2|>&,association]

ClearAll[AssociationThrough];
AssociationThrough[a_Association, x_] := 
  AssociationThrough[Normal[a], x];
AssociationThrough[f_List, x_] := 
  AssociationThrough[
   Replace[f, op : Except[_Rule] :> (op -> op), {1}], x];
AssociationThrough[functionRules : {(_Rule | _RuleDelayed) ..}, x_] :=
   AssociationThread[
   functionRules[[All, 2]],
   #[x] & /@ functionRules[[All, 1]]
   ];
AssociationThrough[{}, x_] := <||>;
AssociationThrough[f_][x_] := AssociationThrough[f, x];
AssociationPartition[assoc_Association,args__]:=Module[{res=Association@@@Partition[Normal@assoc,args]},
res/;ListQ[res]
]
AssociationNormalize[a_Association,f_:Norm]:=AssociationThread[Keys[a],Normalize[Values[a],f]]


ConstantAssociation[keys_,constantValue_]:=AssociationThread[keys->ConstantArray[constantValue,Length[keys]]]
PropertiesSummary[data_]:=Block[{properties},properties=data["Properties"];AssociationMap[data[#]&,properties]]

ClearAll[LinearOptimizationInformation]

LinearOptimizationInformation[f_,cons_,vars_]:=AssociationMap[LinearOptimization[f,cons,vars ,#]&,{"PrimalMinimizer","PrimalMinimizerRules","PrimalMinimizerVector","PrimalMinimumValue","DualMaximizer","DualMaximumValue","DualityGap","Slack","ConstraintSensitivity","ObjectiveVector","LinearInequalityConstraints","LinearEqualityConstraints"}]

ClearAll[QuadraticOptimizationInformation]

QuadraticOptimizationInformation[f_,cons_,vars_]:=AssociationMap[QuadraticOptimization[f,cons,vars ,#]&,{"PrimalMinimizer","PrimalMinimizerRules","PrimalMinimizerVector","PrimalMinimumValue","DualMaximizer","DualMaximumValue","DualityGap","Slack","ConstraintSensitivity","ObjectiveMatrix","ObjectiveVector","FactoredObjectiveMatrix","FactoredObjectiveVector","LinearInequalityConstraints","LinearEqualityConstraints"}]

End[]; (* End `Private` *)

EndPackage[];
