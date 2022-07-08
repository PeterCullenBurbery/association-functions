(* ::Package:: *)

BeginPackage["PeterBurbery`AssociationFunctions`"];

(* Declare your packages public symbols here. *)

PeterBurbery`AssociationFunctions`KeyValueMapGeneralized;
PeterBurbery`AssociationFunctions`AssociationThrough;
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
End[]; (* End `Private` *)

EndPackage[];
