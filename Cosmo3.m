(* ::Package:: *)

(* ::Subtitle:: *)
(*Christoffel Symbols and Geodesic  Equation*)


(* ::Text:: *)
(*This is a Mathematica program to compute the Christoffel  and the geodesic equations, starting from a given metric  Subscript[g, \[Alpha]\[Beta]]. The Christoffel symbols are calculated from the formula*)


(* ::Text:: *)
(*Subscript[\[CapitalGamma]^\[Lambda], \[Mu]\[Nu]]=1/2 g^\[Lambda]\[Sigma](\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\ *)
(*\*SubscriptBox[\(g\), \(\[Sigma]\[Nu]\)]\)+\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Nu]\)]\ *)
(*\*SubscriptBox[\(g\), \(\[Sigma]\[Mu]\)]\)-\!\( *)
(*\*SubscriptBox[\(\[PartialD]\), \(\[Sigma]\)]\ *)
(*\*SubscriptBox[\(g\), \(\[Mu]\[Nu]\)]\))*)
(**)
(*where g^\[Lambda]\[Sigma] is the matrix inverse of Subscript[g, \[Lambda]\[Sigma] ]called the inverse metric. This is the  solution of the relation (8.19) and the notation for the inverse metric is standard [cf (20.17)]. The components of the geodesic equation are *)
(**)
(*du^\[Alpha]/d\[Tau] = - Subscript[\[CapitalGamma]^\[Alpha], \[Beta]\[Gamma]] u^\[Beta]u^\[Gamma] .*)


(* ::Text:: *)
(*You must input the covariant components of the metric tensor Subscript[g, \[Mu]\[Nu]] by editing the relevant input line in this Mathematica notebook. You may also wish to change the names of the coordinates. The nonzero components of the above quantities are displayed as the output. *)


(* ::Subsubsection:: *)
(*Clearing the values of symbols:*)


(* ::Text:: *)
(*First clear any values that may already have been assigned to the names of the various objects to be calculated. The names of the coordinates that you will use are also cleared.*)


(* ::Input:: *)
(*Clear[coord, metric,inversemetric, affine,  r, \[Theta], \[Phi], t]*)


(* ::Subsubsection:: *)
(*Setting The Dimension *)


(* ::Text:: *)
(*The dimension n of the spacetime (or space) must be set:*)


(* ::Input:: *)
(*n=2*)


(* ::Subsubsection:: *)
(*Defining a list of coordinates:*)


(* ::Text:: *)
(*The example given here is the wormhole metric (7.40). Note that for convenience t is denoted by x^4 rather than x^0 and summations run from 1 to 4 rather than 0 to 3. *)


(* ::Input:: *)
(*coord = {x,y}*)


(* ::Text:: *)
(*You can change the names of the coordinates by simply editing the definition of coord, for example, to coord = {x, y, z, t}, when another set of coordinate names is more appropriate.*)


(* ::Subsubsection:: *)
(*Defining the metric:*)


(* ::Text:: *)
(*Input the metric as a list of lists, i.e., as a matrix. You can input the components of any metric here, but you must specify them as explicit functions of the coordinates.*)


(* ::Input:: *)
(*metric={{a^2,0},{0,a^2*Sin[x]^2}}*)


(* ::Text:: *)
(*You can also display this in matrix form:*)


(* ::Input:: *)
(*metric//MatrixForm*)


(* ::Subsubsection:: *)
(*Note:*)


(* ::Text:: *)
(*It is important not to use the symbols, i, j, k, l, n, or s  as constants or coordinates in the metric that you specify above. The reason is that  the first four of those symbols are used as summation or table indices in the calculations done below. The last is the dimension of the space. *)


(* ::Subsubsection:: *)
(*Calculating the inverse metric:*)


(* ::Text:: *)
(*The inverse metric is obtained through matrix inversion.*)


(* ::Input:: *)
(*inversemetric=Simplify[Inverse[metric]]*)


(* ::Text:: *)
(*The inverse metric can also be displayed in matrix form:*)


(* ::Input:: *)
(*inversemetric//MatrixForm*)


(* ::Subsubsection:: *)
(*Calculating the affine connection:*)


(* ::Text:: *)
(*The calculation of the components of the affine connection is done by transcribing the definition given earlier into the notation of Mathematica and using the Mathematica functions D for taking partial derivatives, Sum for summing over repeated indices, Table for forming a list of components, and Simplify for simplifying the result.*)


(* ::Input:: *)
(*affineFirstKind:= affineFirstKind = Simplify[Table[(1/2)**)
(*(D[metric[[i,k]],coord[[j]] ]+*)
(*D[metric[[j,k]],coord[[i]] ]-D[metric[[i,j]],coord[[k]] ]),*)
(*{i,1,n},{j,1,n},{k,1,n}]]*)


(* ::Input:: *)
(*affine:=affine=Simplify[Table[(1/2)*Sum[(inversemetric[[i,s]])**)
(*(D[metric[[s,j]],coord[[k]] ]+*)
(*D[metric[[s,k]],coord[[j]] ]-D[metric[[j,k]],coord[[s]] ]),{s,1,n}],*)
(*{i,1,n},{j,1,n},{k,1,n}] ]*)


(* ::Subsubsection:: *)
(*Displaying the affine connection:*)


(* ::Text:: *)
(*The nonzero components of the affine connections are displayed below. You need not follow the details of constructing the functions that we use for that purpose. Because the affine connection is symmetric under interchange of the last two indices, only the independent components are displayed.*)


(* ::Input:: *)
(*listaffine:=Table[If[UnsameQ[affine[[i,j,k]],0],{ToString[\[CapitalGamma][i,j,k]],affine[[i,j,k]]}] ,{i,1,n},{j,1,n},{k,1,j}]*)


(* ::Input:: *)
(*TableForm[Partition[DeleteCases[Flatten[listaffine],Null],2],TableSpacing->{2,2}]*)


(* ::Input:: *)
(*listaffineFirstKind:=Table[If[UnsameQ[affineFirstKind[[i,j,k]],0],{ToString[\[Gamma][i,j,k]],affineFirstKind[[i,j,k]]}] ,{i,1,n},{j,1,i},{k,1,n}]*)


(* ::Input:: *)
(*TableForm[Partition[DeleteCases[Flatten[listaffineFirstKind],Null],2],TableSpacing->{2,2}]*)


(* ::Subsubsection:: *)
(*Calculating the geodesic equations:*)


(* ::Text:: *)
(*The geodesic equations are calculated by asking  Mathematica to carry out the sum -Subscript[\[CapitalGamma]^\[Alpha], \[Beta]\[Gamma]]u^\[Beta]u^\[Gamma],  where u^\[Alpha] are the components of the four-velocity.  (This gives the derivitive of u^\[Alpha] with respect to proper time  \[Tau].  (This is replaced by  s  if the geodesics are spacelike.)*)


(* ::Input:: *)
(*geodesic:=geodesic=Simplify[Table[-Sum[affine[[i,j,k]]u[j]u[k],{j,1,n},*)
(*{k,1,n}],{i,1,n}]]*)


(* ::Subsubsection:: *)
(*Displaying the geodesic equations:*)


(* ::Input:: *)
(*listgeodesic:=Table[{"d/d\[Tau]"ToString[u[i]],"=",geodesic[[i]]},{i,1,n}]*)


(* ::Input:: *)
(*TableForm[listgeodesic,TableSpacing->{2}]*)


(* ::Subsubsection:: *)
(* Acknowledgment*)


(* ::Text:: *)
(*This program was adapted from the notebook Curvature and the Einstein equation kindly written by Leonard Parker  especially for this text. *)
