Exercice 1 :
------------

fonction int -> int

let rec somme = fun

0 ->
	0

| n ->
	if n < 0
	then failwith "Définie que pour les entiers naturels"
	else n + somme (n - 1)
;;

somme (4) ;; -> 10
somme (0) ;; -> 0
somme (-1) ;; -> Erreur


Exercice 2 :
------------

fonction float * int -> int

let rec puissance = fun

(_, 0) -> 1.

| (reel, exposant) ->
	if exposant < 0
	then failwith "Définie que pour les exposants naturels"
	else reel *. puissance (reel, exposant - 1)
;;

puissance (2.5, 2) ;; -> 6.25
puissance (0., 0) ;; -> 1.0
puissance (25., -2) ;; -> Erreur


Exercice 3 :
------------

1)
fonction int * int -> int

let rec repet = fun

(_, 0) -> 0

| (chiffre, repetition) ->
	if repetition < 0
	then failwith "Définie que pour les répétitions naturelles"
	else if chiffre < 0 or chiffre > 9
		then failwith "Définie que pour les chiffres"
		else chiffre * int_of_float (puissance (10., repetition - 1)) + repet (chiffre, repetition - 1)
;;

repet (5, 4) ;; -> 5555
repet (0, 0) ;; -> 0
repet (15643, 1) ;; -> Erreur
repet (8, -1) ;; -> Erreur

2)
fonction int * int -> bool

let rec nombrePositif = fun

(0, _) -> true

| (nombre, chiffre) ->
	nombre mod 10 = chiffre & nombrePositif (nombre / 10, chiffre)
;;

fonction int * int -> bool

let rec unChiffre = fun

(0, chiffre) ->
	chiffre = 0

| (nombre, chiffre) ->
	if chiffre < 0 or chiffre > 9
	then failwith "Définie que pour les chiffres"
	else nombrePositif (abs (nombre), chiffre)
;;

unChiffre (555755, 5) ;; -> false
unChiffre (-555755, 5) ;; -> false
unChiffre (55555, 5) ;; -> true
unChiffre (-55555, 5) ;; -> true
unChiffre (0, 0) ;; -> true
unChiffre (4512, -2) ;; -> Erreur

3)
fonction int * int -> int

let rec pgd = fun

0 ->
	failwith "Caml light a un modulo qui fait une division par zéro ici."

| (nombre, diviseur) ->
	if nombre mod diviseur = 0
	then diviseur
	else pgd (nombre, diviseur - 1)
;;

pgd (18, 12) ;; -> 9
pgd (7, 6) ;; -> 1
pgd (0, 1) ;; -> 1

4)
4.1)
fonction int -> bool

let rec nbPairChif1 = fun

0 -> false

| entier ->
	let enleveUnChif = entier / 10 in
		not (enleveUnChif = 0 or nbPairChif1 (enleveUnChif))
;;

nbPairChif1 (456789) ;; -> true
nbPairChif1 (4567899) ;; -> false
nbPairChif1 (0) ;; -> false

4.2)
fonction int -> bool

let rec nbPairChif2 = fun

0 -> false

| entier ->
	let enleveUnChif = entier / 10
	and enleveDeuxChif = entier / 100 in
		enleveUnChif <> 0 & (enleveDeuxChif = 0 or nbPairChif2 (enleveDeuxChif))
;;

nbPairChif2 (456789) ;; -> true
nbPairChif2 (4567899) ;; -> false
nbPairChif2 (0) ;; -> false


Exercice 4 :
------------

1)
fonction int * int -> int

let rec mult_egypt = fun

(0, p) -> 0

| (n, p) ->
	if n mod 2 = 0
	then mult_egypt (n / 2, p + p)
	else mult_egypt (n - 1, p) + p
;;

mult_egypt (59, 17) ;; -> 1003
mult_egypt (0, 10) ;; -> 0
mult_egypt (42, 0) ;; -> 0

2)
mult_egypt (59, 17) ;;
-> 59 impair
donc mult_egypt (59 - 1, 17) + 17
-> 58 pair
donc mult_egypt (58 / 2, 17 + 17) + 17
-> 29 impair
donc mult_egypt (29 - 1, 34) + 34 + 17
-> 28 pair
donc mult_egypt (28 / 2, 34 + 34) + 34 + 17
-> 14 pair
donc mult_egypt (14 / 2, 68 + 68) + 34 + 17
-> 7 impair
donc mult_egypt (7 - 1, 136) + 136 + 34 + 17
-> 6 pair
donc mult_egypt (6 / 2, 136 + 136) + 136 + 34 + 17
-> 3 impair
donc mult_egypt (3 - 1, 272) + 272 + 136 + 34 + 17
-> 2 pair
donc mult_egypt (2 / 2, 272 + 272) + 272 + 136 + 34 + 17
-> 1 impair
donc mult_egypt (1 - 1, 544) + 544 + 272 + 136 + 34 + 17
-> 0 donne 0
donc 0 + 544 + 272 + 136 + 34 + 17
= 1003

3)
trace ("mult_egypt") ;;

3.1)
mult_egypt (59, 17) ;; ->
	mult_egypt <-- 59, 17
	mult_egypt <-- 58, 17
	mult_egypt <-- 29, 34
	mult_egypt <-- 28, 34
	mult_egypt <-- 14, 68
	mult_egypt <-- 7, 136
	mult_egypt <-- 6, 136
	mult_egypt <-- 3, 272
	mult_egypt <-- 2, 272
	mult_egypt <-- 1, 544
	mult_egypt <-- 0, 544
	mult_egypt --> 0
	mult_egypt --> 544
	mult_egypt --> 544
	mult_egypt --> 816
	mult_egypt --> 816
	mult_egypt --> 952
	mult_egypt --> 952
	mult_egypt --> 952
	mult_egypt --> 986
	mult_egypt --> 986
	mult_egypt --> 1003
1003

3.2)
mult_egypt (0, 10) ;; ->
	mult_egypt --> 0
0

3.3)
mult_egypt (42, 0) ;; ->
	mult_egypt <-- 21, 0
	mult_egypt <-- 20, 0
	mult_egypt <-- 10, 0
	mult_egypt <-- 5, 0
	mult_egypt <-- 4, 0
	mult_egypt <-- 2, 0
	mult_egypt <-- 1, 0
	mult_egypt <-- 0, 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
	mult_egypt --> 0
0


Exercice 5 :
------------

1)
fonction int -> int

let rec s_chif = fun

0 -> 0

| entier ->
	entier mod 10 + s_chif (entier / 10)
;;

s_chif (302091) ;; -> 15
s_chif (0) ;; -> 0

2)
fonction int -> int

let rec som_chif = fun

entier ->
	if entier / 10 = 0
	then entier
	else som_chif (s_chif (entier))
;;

som_chif (302091) ;; -> 6
som_chif (0) ;; -> 0


Exercice 6 :
------------

1)
1.1)
fonction char -> bool

let majuscule = fun

caractere ->
	`A` <= caractere & caractere <= `Z`
;;

1.2)
fonction char -> bool

let minuscule = fun

caractere ->
	`a` <= caractere & caractere <= `z`
;;

1.3)
fonction char -> bool

let lettre = fun

caractere ->
	majuscule (caractere) or minuscule (caractere)
;;

lettre (`d`) ;; -> true
lettre (`E`) ;; -> true
lettre (` `) ;; -> false
lettre (`_`) ;; -> false
lettre (`3`) ;; -> false

2)
2.1)
fonction string -> char

let tete = fun

chaine ->
	nth_char (sub_string chaine 0 1) 0
;;

2.2)
fonction string -> string

let queue = fun

chaine ->
	let taille = string_length chaine in
		sub_string chaine 1 (taille - 1)
;;

2.3)
fonction char * string -> bool

let rec appartient = fun

(_, "") -> false

| (caractere, chaine) ->
	caractere = tete (chaine) or appartient (caractere, queue (chaine))
;;

appartient (`e`, "antecedent") ;; -> true
appartient (`e`, "avant") ;; -> false
appartient (`a`, "") ;; -> false

3)
3.1)
fonction string * string -> bool

let rec debutAsym = fun

("", _) -> true

| (_, "") -> false

| (chaineA, chaineB) ->
    tete (chaineA) = tete (chaineB)
    & debutAsym (queue (chaineA), queue (chaineB))
;;

debutAsym ("", "") ;; -> true
debutAsym ("bonjour", "") ;; -> false
debutAsym ("", "bonsoir") ;; -> true
debutAsym ("oui", "oui oui") ;; -> true
debutAsym ("non non", "non") ;; -> false
debutAsym ("x abc", "abc x") ;; -> false

3.2)
fonction string * string -> bool

let rec debut = fun

("", _) -> true

| (_, "") -> true

| (chaineA, chaineB) ->
    tete (chaineA) = tete (chaineB)
    & debut (queue (chaineA), queue (chaineB))
;;

debut ("", "") ;; -> true
debut ("bonjour", "") ;; -> true
debut ("", "bonsoir") ;; -> true
debut ("oui", "oui oui") ;; -> true
debut ("non non", "non") ;; -> true
debut ("x abc", "abc x") ;; -> false

4)
4.1)
fonction string -> int

let rec taille = fun

"" -> 0

| chaine -> 1 + taille (queue (chaine))
;;

taille ("") ;; -> 0
taille ("Hello World") ;; -> 11

4.2)
fonction string * string -> bool

let rec incluseAsym = fun

("", "") -> true

| (_, "") -> false

| (petite, grande) ->
    debutAsym (petite, grande)
    or incluseAsym (petite, queue (grande))
;;

incluseAsym ("", "") ;; -> true
incluseAsym ("au revoir", "") ;; -> false
incluseAsym ("roc", "Un roc si biscornu") ;; -> true
incluseAsym ("Un roc si biscornu", "roc") ;; -> false
incluseAsym ("Un roc si biscornu", "rock") ;; -> false
incluseAsym ("rock", "Un roc si biscornu") ;; -> false
incluseAsym ("abc", "ab") ;; -> false
incluseAsym ("ab", "abc") ;; -> true

4.3)
fonction string * string -> bool

let incluse = fun

(chaineA, chaineB) ->
    if taille (chaineA) < taille (chaineB)
    then incluseAsym (chaineA, chaineB)
    else incluseAsym (chaineB, chaineA)
;;

incluse ("", "") ;; -> true
incluse ("au revoir", "") ;; -> true
incluse ("roc", "Un roc si biscornu") ;; -> true
incluse ("Un roc si biscornu", "roc") ;; -> true
incluse ("Un roc si biscornu", "rock") ;; -> false
incluse ("rock", "Un roc si biscornu") ;; -> false
incluse ("abc", "ab") ;; -> true
incluse ("ab", "abc") ;; -> true

5)
fonction char * string -> int

let rec frequence = fun

(_, "") -> 0

| (caractere, chaine) ->
    let recursion = frequence (caractere, queue (chaine)) in
        if caractere = tete (chaine)
        then recursion + 1
        else recursion
;;

frequence (`i`, "invisibilite") ;; -> 5
frequence (`a`, "invisibilite") ;; -> 0
frequence (`1`, "") ;; -> 0

6)
fonction char * string -> string

let rec elimine = fun

(_, "") -> ""

| (caractere, chaine) ->
    let recursion = elimine (caractere, queue (chaine)) in
        let premier = tete (chaine) in
            if caractere = premier
            then recursion
            else string_of_char (premier) ^ recursion
;;

elimine (`i`, "invisibilite") ;; -> "nvsblte"
elimine (`a`, "invisibilite") ;; -> "invisibilite"
elimine (`1`, "") ;; -> ""

7)
fonction string -> string

let rec renverse = fun

"" -> ""

| chaine ->
    renverse (queue (chaine)) ^ string_of_char (tete (chaine))
;;

renverse ("invisibilite") ;; -> "etilibisivni"
renverse ("unrocsibiscornu") ;; -> "unrocsibiscornu"
renverse ("") ;; -> ""

8)
fonction string -> bool

let palindrome = fun

chaine -> chaine = renverse (chaine)
;;

palindrome ("invisibilite") ;; -> false
palindrome ("unrocsibiscornu") ;; -> true
palindrome ("kayak") ;; -> true
palindrome ("kaxyak") ;; -> false
palindrome ("XxxX") ;; -> true
palindrome ("cc") ;; -> true
palindrome ("") ;; -> true
