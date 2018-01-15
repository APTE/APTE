# TESTS

## Formats
2sides+n: processus p1 à droite et p2 à gauche. p1 et p2 sont égaux à un process p sauf sur les noms (choisis différemment à droite et à gauche).
2sides: processus p à droite et p à gauche.
1sie: processus p à gauche et 0 à droite.

La compilation vers Porridge au sein de Apte respecte le format 2sides+n (comme attendu). D'ailleurs dans les tableaux ci-dessous, c'est le seul format pour lequel
on retrouve exactement le même nombre de traces et d'états "à explorer".

## BENCHS:
Dans la branche "bench-lucca" sur Porridge et POR2 dans Apte:
 - Porridge: time ./test.byte --sleep --size --bench toy3
 - Porridge+APTE: time ../apte -with_por_gen ../bench/protocols/toy/Simple_3_par.txt


## Porridge:
+----------+----------+----------+----------+
|          | toy3     | toy4     | toy5     |
+----------+----------+----------+----------+
| 2sides+n |  0.01    | 0.52     | 18.28    |
+----------+----------+----------+----------+
| 2sides   |  0.00    | 0.05     | 0.93     |
+----------+----------+----------+----------+
| 1side    |  0.00    | 0.04     | 0.80     |
+----------+----------+----------+----------+

## Porridge appelé en tant que LIB par APTE (sur une version=2sides+n):
+----------+----------+----------+----------+
|          | toy3     | toy4     | toy5     |
+----------+----------+----------+----------+
| 2sides+n |  0.01    | 0.51     | 18.14    |
+----------+----------+----------+----------+

Pour les 3 exemples, le nombre d'états et de traces calculés par Porridge est le même que dans la ligne 2sides+n dans le premier tableau.



# TEST MEMOS

## AUCUNE MEMO
Voir diff [1] ci-dessous.

2sides:
	./test --sleep --size --bench toy5  164.66s user

2sides+n:
	./test --sleep --size --bench toy5  167.65s user



## En gardant le  MEMO pour frame seulement:
Voir diff [2] ci-dessous.
2sides (taille 6):
	./test --sleep --size --bench toy6  116.47s user 0.20s system 99% cpu 1:56.70 total

2sides:
	./test --sleep --size --bench toy5  5.77s user 0.01s system 99% cpu 5.789 total

2sides+n:
	./test --sleep --size --bench toy5  96.78s user 0.03s system 99% cpu 1:36.83 total


[1]
--------------------------------------------------
diff --git a/ml/LTS.ml b/ml/LTS.ml
index 95ed2c1..70005fa 100644
--- a/ml/LTS.ml
+++ b/ml/LTS.ml
@@ -139,7 +139,7 @@ module Make (T:Simple) = struct
   open T
 
   module StateSet = Set.Make(State)
-  module SMemo = Memo.Make(State)
+  module SMemo = Memo.Fake(State)
 
   (** Set of reachable states from a given state. *)
   let reachable_states = SMemo.make_rec (fun reachable_states s ->
diff --git a/ml/POR.ml b/ml/POR.ml
index db781ed..8a2d0e3 100644
--- a/ml/POR.ml
+++ b/ml/POR.ml
@@ -11,8 +11,8 @@ module Make (T:S) = struct
 
   open T
 
-  module SMemo = Memo.Make(State)
-  module AMemo = Memo.Make(SemanticAction)
+  module SMemo = Memo.Fake(State)
+  module AMemo = Memo.Fake(SemanticAction)
 
   (** Return the set of first conflicting actions wrt a given initial state
     * and enabled action. More precisely, we are looking for actions b such that:
diff --git a/ml/frame.ml b/ml/frame.ml
index a4ec74d..233a7e2 100644
--- a/ml/frame.ml
+++ b/ml/frame.ml
@@ -89,7 +89,7 @@ end = struct
       Hashtbl.hash (Term.hash t,l.id)
   end
 
-  module M = Memo.Make(PT)
+  module M = Memo.Fake(PT)
   let count = ref 0
   let cons = M.make (fun (t,l) ->
                        incr count ;
 
-module SMemo = Memo.Make(State)
+module SMemo = Memo.Fake(State)
 
 module SuccFail = struct
 

[2]
--------------------------------------------------
diff --git a/ml/LTS.ml b/ml/LTS.ml
index 95ed2c1..70005fa 100644
--- a/ml/LTS.ml
+++ b/ml/LTS.ml
@@ -139,7 +139,7 @@ module Make (T:Simple) = struct
   open T
 
   module StateSet = Set.Make(State)
-  module SMemo = Memo.Make(State)
+  module SMemo = Memo.Fake(State)
 
   (** Set of reachable states from a given state. *)
   let reachable_states = SMemo.make_rec (fun reachable_states s ->
diff --git a/ml/POR.ml b/ml/POR.ml
index db781ed..8a2d0e3 100644
--- a/ml/POR.ml
+++ b/ml/POR.ml
@@ -11,8 +11,8 @@ module Make (T:S) = struct
 
   open T
 
-  module SMemo = Memo.Make(State)
-  module AMemo = Memo.Make(SemanticAction)
+  module SMemo = Memo.Fake(State)
+  module AMemo = Memo.Fake(SemanticAction)
 
   (** Return the set of first conflicting actions wrt a given initial state
     * and enabled action. More precisely, we are looking for actions b such that:
