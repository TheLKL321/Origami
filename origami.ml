(*  Autor: Łukasz Zarębski
    Code review: Szymon Haponiuk *)

open List;;

(*  Punkt na płaszczyźnie: (x, y) *)
type point = float * float

(*  Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(*  Równanie kierunkowe prostej y = ax + b: (a, b) *)
type prosta = float * float

(*  Zał: x1 <= x2, y1 <= y2
    Zwraca kartkę w kształcie domkniętego prostokąta o bokach równoległych do
    osi układu współrzędnych i lewym dolnym rogu (x1, y1) a prawym górnym
    (x2, y2). Po wbiciu szpilki wewnątrz (lub na krawędziach), kartka zostanie 
    przebita 1 raz, w pozostałych przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) =
  fun (x, y) ->
    if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
    else 0

(*  Zał: r >= 0
    Zwraca kartkę w kształcie koła domkniętego o środku w punkcie (xs, ys) i
    promieniu r *)
let kolko (xs, ys) r =
  fun (x, y) ->
    if (x -. xs) *. (x -. xs) +. (y -. ys) *. (y -. ys) <= r *. r then 1
    else 0

(*  Zał: a <> 0, a rzeczywisty
    Zwraca prostą przechodzącą przez punkt p, będącą prostą prostopadłą do
    prostej o współczynniku kierunkowym a *)
let prostopadla p a =
  let kierunek = -1. /. a
  in (kierunek, snd p -. (kierunek *. fst p))

(*  Zał: p1 <> p2, fst p2 <> fst p1
    Zwraca prostą zawierającą punkty p1 i p2  *)
let prosta p1 p2 = 
  let kierunek = (snd p2 -. snd p1) /. (fst p2 -. fst p1)
  in (kierunek, snd p1 -. (kierunek *. fst p1))

(*  Zał: fst prost1 <> fst prost2
    Zwraca punkt leżący na przecięciu prostych prost1 i prost2 *)
let przeciecie prost1 prost2 = 
  let x = (snd prost2 -. snd prost1) /. (fst prost1 -. fst prost2)
  in 
    let y = fst prost1 *. x +. snd prost1
    in (x, y) 

(*  Zwraca symetryczne odbicie punktu p względem punktu s  *)
let symetria p s = (2. *. fst s -. fst p, 2. *. snd s -. snd p)

(*  Zał: l nie jest prostopadła ani do osi ox ani do oy
    Zwraca punkt będący odbiciem punktu p względem prostej l *)
let odbij p l = symetria p (przeciecie l (prostopadla p (fst l)))

(*  Zwraca x/y punktu będącego odbiciem punktu o x/y'owej xy1 względem prostej o
    równaniu [x/y] = xy2. Argument cmp określa z której strony prostej odbijamy
    *)
let odbijXY xy1 cmp xy2 = 
  let wieksze =  xy2 +. abs_float (xy2 -. xy1)
  and mniejsze = xy2 -. abs_float (xy2 -. xy1) 
  in if cmp wieksze mniejsze then mniejsze else wieksze

(*  Zał: p1 <> p2
    Składa kartkę k wzdłuż prostej przechodzącej przez punkty (x1, y1) i 
    (x2, y2). Z prawej strony prostej (patrząc od (x1, y1) do (x2, y2)) papier 
    jest przekładany na lewą. Przebicie dokładnie na prostej powinno zwrócić 
    tyle, co przebicie przed złożeniem. *)
let zloz p1 p2 k =
  if fst p1 = fst p2 then
  (*  Prosta pozioma *)
    (*  Określamy na którą stronę składamy *)
    let cmp = if snd p1 < snd p2 then ( < ) else ( > )
    in
      fun (x, y) -> 
        if cmp x (fst p1) then 
          k (x, y) + k (odbijXY x cmp (fst p1), y)
        else if x = fst p1 then 
          k (x, y)
        else 0
  else if snd p1 = snd p2 then
  (*  Prosta pionowa *)
    let cmp = if fst p1 < fst p2 then ( > ) else ( < )
    in
      fun (x, y) -> 
        if cmp y (snd p1) then
          k (x, y) + k (x, odbijXY y cmp (snd p1))
        else if y = snd p1 then 
          k (x, y)
        else 0
  else 
  (*  Pozostałe przypadki *)
    let cmp = 
      if fst p1 < fst p2 then ( > ) else ( < )
    and prost = prosta p1 p2
    in
      fun (x, y) -> 
        (*  Obliczamy wartość funkcji prostej dla danego x *)
        let zgiecie = fst prost *. x +. snd prost
        in
          if cmp y zgiecie then
            k (x, y) + k (odbij (x, y) prost)
          else if y = zgiecie then
            k (x, y)
          else 0 

(*  Składa kartkę k kolejno wzdłuż wszystkich prostych z listy l (proste w 
    liście są w postaci pary punktów przez nie przechodzących) *)
let skladaj l k = 
  let pom k (p1, p2) = zloz p1 p2 k
  in fold_left pom k l
;;

(*  
let test a b msg = if a=b then print_endline "ok"
else (print_int a; print_string "<>"; print_int b; print_string " test: "; print_endline msg);;


let p1 = prostokat (0., 0.) (10., 10.)
let k1 = kolko (5., 5.) 5.
let l1 = [((0., 0.), (10., 10.));
    ((5., 0.), (10., 5.));
    ((10., 0.), (0., 10.));
    ((2.5, 0.), (2.5, 10.))];;
let l2 = [((8., 0.), (10., 2.));
    ((6., 0.), (10., 4.));
    ((4., 0.), (10., 6.));
    ((2., 0.), (10., 8.));
    ((0., 0.), (10., 10.));
    ((0., 2.), (8., 10.));
    ((0., 4.), (6., 10.));
    ((0., 6.), (4., 10.));
    ((0., 8.), (2., 10.))];;

let p2 = skladaj l1 p1
let p3 = skladaj l2 p1
let k2 = skladaj l1 k1;;

test (p2 (7., 3.)) 0 "0.1: p2";;
test (p2 (5., 8.)) 0 "0.2: p2";;
test (p2 (3., 5.)) 0 "0.3: p2";;
test (p2 (5., 5.)) 0 "0.4: p2";;
test (p2 (0., 0.)) 2 "1: p2";;
test (p2 (0., 10.)) 2  "2: p2";;
test (p2 (2.5, 2.5)) 2 "3: p2";;
test (p2 (2.5, 7.5)) 2 "4: p2";;
test (p2 (2.5, 5.)) 4 "5: p2";;
test (p2 (0., 5.)) 5 "6: p2";;
test (p2 (1., 2.)) 4 "7: p2";;
test (p2 (1., 5.)) 8 "8: p2";;
test (p2 (1., 8.)) 4 "9: p2";;

test (k2 (7., 3.)) 0 "0.1: k2";;
test (k2 (5., 8.)) 0 "0.2: k2";;
test (k2 (3., 5.)) 0 "0.3: k2";;
test (k2 (5., 5.)) 0 "0.4: k2";;
test (k2 (2.5, 2.5)) 2 "1: k2";;
test (k2 (2.5, 7.5)) 2 "2: k2";;
test (k2 (2.5, 5.)) 4 "3: k2";;
test (k2 (0., 5.)) 5 "4: k2";;
test (k2 (1., 3.)) 4 "5: k2";;
test (k2 (1., 5.)) 8 "6: k2";;
test (k2 (1., 7.)) 4 "7: k2";;

test (p3 ((-4.), 6.)) 2 "1: p3";;
test (p3 ((-3.), 5.)) 1 "2: p3";;
test (p3 ((-3.), 7.)) 2 "3: p3";;
test (p3 ((-2.), 6.)) 3 "4: p3";;
test (p3 ((-2.5), 6.5)) 4 "5: p3";;
test (p3 ((-2.), 8.)) 4 "6: p3";;
test (p3 ((-1.), 7.)) 3 "7: p3";;
test (p3 ((-1.5), 7.5)) 6 "8: p3";;
test (p3 (0., 8.)) 5 "9: p3";;
test (p3 ((-1.), 9.)) 4 "10: p3";;
test (p3 ((-0.5), 8.5)) 8 "11: p3";;
test (p3 (0., 10.)) 6 "12: p3";;
test (p3 (1., 9.)) 5 "13: p3";;
test (p3 (0.5, 9.5)) 10 "14: p3";;

let kolo = kolko (0.,0.) 10. in
assert (kolo (1000., 0.) = 0);
let poziomo = zloz (0.,0.) (1.,0.) kolo in
assert (poziomo (0.,0.) = 1);
assert (poziomo (0.,1.) = 2);
assert (poziomo (0.,-1.) = 0);
let pionowo = zloz (0.,0.) (0.,1.) kolo in
assert (pionowo (0.,0.) = 1);
assert (pionowo (-1.,0.) = 2);
assert (pionowo (1.,0.) = 0);
let cwiartka = zloz (0.,0.) (0.,1.) poziomo in
assert (cwiartka (0.,0.) = 1);
assert (cwiartka (-1.,1.) = 4);
assert (cwiartka (-1.,0.) = 2);
print_endline "ok!"
 *)