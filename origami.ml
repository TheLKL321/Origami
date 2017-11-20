open List;;

(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(** Zwraca kartkę, reprezentującą domknięty prostokąt
    o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
    a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
    od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
    (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
    w pozostałych przypadkach 0 razy *)
let prostokat (x1, y1) (x2, y2) =
  let kartka (x, y) =
    if x >= x1 && x <= x2 && y >= y1 && y <= y2 then 1
    else 0

(** Zwraca kwadrat danej liczby  *)
let sq x = x * x

(** Zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] *)
let kolko (xs, ys) r =
  let kartka (x, y) =
    if (x -. xs)

(** Składa kartkę [k] wzdłuż prostej przechodzącej prze
    punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
    w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
    jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
    przebicie po prawej stronie prostej powinno więc zwrócić 0.
    Przebicie dokładnie na prostej powinno zwrócić tyle samo,
    co przebicie kartki przed złożeniem. Po stronie lewej -
    tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
    który nałożył się na punkt przebicia. *)
let zlozPara k (p1, p2) =


(** Wywołuje zlozPara odpowiednio dopasowując argumenty *)
let zloz p1 p2 k =
  zlozPara k (p1, p2)

(** Wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy l *)
let skladaj l k =
  fold_left zlozPara k l
;;
