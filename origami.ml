open List;;

(*  @codeReviewer 
    Czy mam precyzować że a <> infinity itp?
    Czy jak napiszę fst p gdzie p to punkt to czy wiadomo że chodzi o x'ową? *)
(*  Punkt na płaszczyźnie: (x, y) *)
type point = float * float

(*  Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(*  Równanie kierunkowe prostej y = ax + b: (a, b) *)
type prosta = float * float

(*  Zał: x1 <= x2, y1 <= y2
    Zwraca kartkę, reprezentującą domknięty prostokąt o bokach równoległych do
    osi układu współrzędnych i lewym dolnym rogu (x1, y1) a prawym górnym
    (x2, y2). Gdy w kartkę tę wbije się szpilkę wewnątrz
    (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
    w pozostałych przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) =
  fun (x, y) ->
    if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
    else 0

(*  Zwraca kwadrat danej liczby  *)
let sq x = x *. x

(*  Zał: r >= 0
    Zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie (xs, ys) i
    promieniu r *)
let kolko (xs, ys) r =
  fun (x, y) ->
    if (sq (x -. xs)) +. (sq (y -. ys)) <= sq r then 1
    else 0

(*  Zał: a <> 0
    Zwraca prostą przechodzącą przez punkt p, będącą prostą prostopadłą do
    prostej o współczynniku kierunkowym a *)
let prostopadla p a =
  let kierunek = -1. /. a
  in (kierunek, -.(kierunek *. fst p) +. snd p)

(*  Zał: p1 <> p2, fst p2 <> fst p1
    Zwraca prostą zawierającą punkty p1 i p2  *)
let prosta p1 p2 = 
  if fst p2 = fst p1 then (*  TODO: what now *)
  let kierunek = (snd p2 -. snd p1)/.(fst p2 -. fst p1)
  in (kierunek, -.(kierunek *. fst p1) +. snd p1)

(*  Zał: fst prost1 <> fst prost2
    Zwraca punkt leżący na przecięciu prostych prost1 i prost2 *)
let przeciecie prost1 prost2 = 
  let x = (snd prost2 -. snd prost1) /. (fst prost1 -. fst prost2)
  in 
    let y = fst prost1 *. x +. snd prost1
    in (x, y) 

(*  Zwraca symetryczne odbicie punktu p względem punktu s  *)
let symetria p s = (2. *. fst s -. fst p, 2. *. snd s -. snd p)

(*  Zwraca punkt będący odbiciem punktu p względem prostej l *)
let odbij p l = symetria p (przeciecie l (prostopadla p (fst l)))

(*  Zał: (x1, y1) <> (x2, y2)
    Składa kartkę k wzdłuż prostej przechodzącej przez punkty (x1, y1) i 
    (x2, y2). Papier jest składany w ten sposób, że z prawej strony prostej
    (patrząc w kierunku od (x1, y1) do (x2, y2) jest przekładany na lewą.
    Wynikiem funkcji jest złożona kartka. Jej przebicie po prawej stronie
    prostej powinno więc zwrócić 0. Przebicie dokładnie na prostej powinno 
    zwrócić tyle samo, co przebicie kartki przed złożeniem. Po stronie lewej -
    tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie, który
    nałożył się na punkt przebicia. *)
let zlozPara k ((x1, y1), (x2, y2)) =
  if x1 = x2 then
    (* pion *) 42
    (* TODO: CZY DZIAŁA Z MOIMI FUNKCJAMI *)
  else if y1 = y2 then
    (*  poziom *) 42
  else if x1 < x2 then
    (* mniejsze od funkcji *) 42
  else
    (* większe od funkcji *) 42

(*  Zał: p1 <> p2
    Wywołuje zlozPara odpowiednio dopasowując argumenty *)
let zloz p1 p2 k = zlozPara k (p1, p2)

(*  Składa kartkę k kolejno wzdłuż wszystkich prostych z listy l *)
let skladaj l k = fold_left zlozPara k l
;;
