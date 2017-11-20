open List;;

(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
    o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
    a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
    od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
    (lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
    w pozostałych przypadkach 0 razy *)
let prostokat p1 p2 = 42


(** [kolko p r] zwraca kartkę, reprezentującą kółko domknięte o środku w punkcie [p] i promieniu [r] *)
let kolko p r = 42

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
    punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
przebicie po prawej stronie prostej powinno więc zwrócić 0.
Przebicie dokładnie na prostej powinno zwrócić tyle samo,
co przebicie kartki przed złożeniem. Po stronie lewej -
tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
który nałożył się na punkt przebicia. *)
let zloz p1 p2 k = 42

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
    czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
z listy *)
let skladaj l k =
  fold_left zloz k l
;;
