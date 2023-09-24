# funk
Interpreter for a subset of the Haskell language. Note that this project is in an early experimental phase

## Function definition and application
Functions can be defined by lambda abstraction over the arguments.
```
> successor = \n -> n+1
lexed:
[LexVarName "successor",LexEquals,LexLambda,LexVarName "n",LexArrow,LexVarName "n",LexOperator "+",LexLitInt 1]
parsed:
Def "successor" (FeAbs "n" (FeOp "+" (FeVar "n") (FeLitInt 1)))
desugared:
FeLet "successor" (FeAbs "n" (FeApp (FeApp (FeVar "+") (FeVar "n")) (FeLitInt 1))) (FeVar "successor")
corified:
let successor = \n -> ((+) (n)) (1) in successor
[("b",(Int )),("c",((Int ) -> (Int ))),("d",(Int ))]
((Int ) -> (Int ))
[(((Int ) -> ((Int ) -> (Int ))),(b -> c)),(c,((Int ) -> d)),(((Int ) -> (Int )),((Int ) -> (Int )))]
forall  . ((Int ) -> (Int ))
> successor 1
lexed:
[LexVarName "successor",LexLitInt 1]
parsed:
Expr (FeApp (FeVar "successor") (FeLitInt 1))
desugared:
FeApp (FeVar "successor") (FeLitInt 1)
corified:
(successor) (1)
[("a",(Int ))]
a
[(((Int ) -> (Int )),((Int ) -> a))]
forall  . (Int )
2
```
## Case expressions
To branch between different values in an expression, pattern match against their constructors.
```
> factorial = \n -> case n of {0 -> 1; any -> n * factorial (n-1);}
lexed:
[LexVarName "factorial",LexEquals,LexLambda,LexVarName "n",LexArrow,LexCase,LexVarName "n",LexOf,LexCurlyL,LexLitInt 0,LexArrow,LexLitInt 1,LexSemicolon,LexVarName "any",LexArrow,LexVarName "n",LexOperator "*",LexVarName "factorial",LexParensL,LexVarName "n",LexOperator "-",LexLitInt 1,LexParensR,LexSemicolon,LexCurlyR]
parsed:
Def "factorial" (FeAbs "n" (FeCases (FeVar "n") [(FPaLitInt 0,FeLitInt 1),(FPaVar "any",FeOp "*" (FeVar "n") (FeApp (FeVar "factorial") (FeOp "-" (FeVar "n") (FeLitInt 1))))]))
desugared:
FeLet "factorial" (FeAbs "n" (FeCases (FeVar "n") [(FPaLitInt 0,FeLitInt 1),(FPaVar "any",FeApp (FeApp (FeVar "*") (FeVar "n")) (FeApp (FeVar "factorial") (FeApp (FeApp (FeVar "-") (FeVar "n")) (FeLitInt 1))))])) (FeVar "factorial")
corified:
let factorial = \n -> case n of {(CPaLitInt 0,1);(CPaVar "any",((*) (n)) ((factorial) (((-) (n)) (1))));} in factorial
[("a",((Int ) -> (Int ))),("b",(Int )),("c",(Int )),("d",(Int )),("e",((Int ) -> (Int ))),("f",((Int ) -> (Int ))),("g",(Int )),("h",(Int )),("i",(Int ))]
((Int ) -> (Int ))
[(b,(Int )),(c,(Int )),(b,d),(((Int ) -> ((Int ) -> (Int ))),(b -> e)),(((Int ) -> ((Int ) -> (Int ))),(b -> f)),(f,((Int ) -> g)),(a,(g -> h)),(e,(h -> i)),(c,i),(((Int ) -> (Int )),((Int ) -> (Int )))]
forall  . ((Int ) -> (Int ))
> factorial 10
lexed:
[LexVarName "factorial",LexLitInt 10]
parsed:
Expr (FeApp (FeVar "factorial") (FeLitInt 10))
desugared:
FeApp (FeVar "factorial") (FeLitInt 10)       
corified:
(factorial) (10)
[("a",(Int ))]
a
[(((Int ) -> (Int )),((Int ) -> a))]
forall  . (Int )
3628800
```
## Custom datatypes
Custom datatypes can be defined using GADT-Syntax.
```
> type List where {Nil :: forall . List; Cons :: forall . Int -> (List -> List);} 
lexed:
[LexType,LexTypeName "List",LexWhere,LexCurlyL,LexTypeName "Nil",LexHasType,LexForall,LexPeriod,LexTypeName "List",LexSemicolon,LexTypeName "Cons",LexHasType,LexForall,LexPeriod,LexTypeName "Int",LexArrow,LexParensL,LexTypeName "List",LexArrow,LexTypeName "List",LexParensR,LexSemicolon,LexCurlyR]
parsed:
TypeDef "List" [] [("Nil",forall  . (List )),("Cons",forall  . ((Int ) -> ((List ) -> (List ))))]
desugared:
> length = \list -> case list of {Nil -> 0; Cons a b -> 1 + length b;}
lexed:
[LexVarName "length",LexEquals,LexLambda,LexVarName "list",LexArrow,LexCase,LexVarName "list",LexOf,LexCurlyL,LexTypeName "Nil",LexArrow,LexLitInt 0,LexSemicolon,LexTypeName "Cons",LexVarName "a",LexVarName "b",LexArrow,LexLitInt 1,LexOperator "+",LexVarName "length",LexVarName "b",LexSemicolon,LexCurlyR]
parsed:
Def "length" (FeAbs "list" (FeCases (FeVar "list") [(FPaCons "Nil" [],FeLitInt 0),(FPaCons "Cons" [FPaVar "a",FPaVar "b"],FeOp "+" (FeLitInt 1) (FeApp (FeVar "length") (FeVar "b")))]))
desugared:
FeLet "length" (FeAbs "list" (FeCases (FeVar "list") [(FPaCons "Nil" [],FeLitInt 0),(FPaCons "Cons" [FPaVar "a",FPaVar "b"],FeApp (FeApp (FeVar "+") (FeLitInt 1)) (FeApp (FeVar "length") (FeVar "b")))])) (FeVar "length")     
corified:
let length = \list -> case list of {(CPaCons "Nil" [],0);(CPaCons "Cons" ["a","b"],((+) (1)) ((length) (b)));} in length
[("a",((List ) -> (Int ))),("b",(List )),("c",(Int )),("d",((Int ) -> (Int ))),("e",(Int )),("f",(Int ))]
((List ) -> (Int ))
[(b,(List )),(c,(Int )),(b,(List )),(((Int ) -> ((Int ) -> (Int ))),((Int ) -> d)),(a,((List ) -> e)),(d,(e -> f)),(c,f),(((List ) -> (Int )),((List ) -> (Int )))]
forall  . ((List ) -> (Int ))
> length (Cons 1 (Cons 2 (Cons 3 Nil)))
lexed:
[LexVarName "length",LexParensL,LexTypeName "Cons",LexLitInt 1,LexParensL,LexTypeName "Cons",LexLitInt 2,LexParensL,LexTypeName "Cons",LexLitInt 3,LexTypeName "Nil",LexParensR,LexParensR,LexParensR]
parsed:
Expr (FeApp (FeVar "length") (FeApp (FeApp (FeCons "Cons") (FeLitInt 1)) (FeApp (FeApp (FeCons "Cons") (FeLitInt 2)) (FeApp (FeApp (FeCons "Cons") (FeLitInt 3)) (FeCons "Nil")))))
desugared:
FeApp (FeVar "length") (FeApp (FeApp (FeCons "Cons") (FeLitInt 1)) (FeApp (FeApp (FeCons "Cons") (FeLitInt 2)) (FeApp (FeApp (FeCons "Cons") (FeLitInt 3)) (FeCons "Nil"))))
corified:
(length) (((Cons) (1)) (((Cons) (2)) (((Cons) (3)) (Nil))))
[("a",((List ) -> (List ))),("b",((List ) -> (List ))),("c",((List ) -> (List ))),("d",(List )),("e",(List )),("f",(List )),("g",(Int ))]
g
[(((Int ) -> ((List ) -> (List ))),((Int ) -> a)),(((Int ) -> ((List ) -> (List ))),((Int ) -> b)),(((Int ) -> ((List ) -> (List ))),((Int ) -> c)),(c,((List ) -> d)),(b,(d -> e)),(a,(e -> f)),(((List ) -> (Int )),(f -> g))] 
forall  . (Int )
3
```
