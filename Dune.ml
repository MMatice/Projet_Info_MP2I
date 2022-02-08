open Graphics
(* Documentation de graphics ici :

    http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html
*)
(* ATTENTION J'AI L'IMPRESSION QU'IL Y A UN BUG D'AFFICHAGE  *)
type case = Air | Sable | Balle;;
let sky_color = rgb 52 146 235 (*COULEUR CIEL  *)
let _ =
    open_graph " 1000x1000"; (*1000x1000 car c'est beau  *)
    let screen = Array.make_matrix 1000 1000 Air in 
    
    moveto 0 200; (*point de depart  *)
    set_color sky_color;
    fill_rect 0 0 1000 1000; (*on fill un rectangle pour le ciel (background)  *)
    Random.self_init(); (*seed random  *)

    for i = 0 to 4 do 
    (*l'idée est d'uilisé la fonction curveto qui prend trois points + le point de départ voir la doc
    pour plus de détails suivant le modéle de Bézier jcrois https://en.wikipedia.org/wiki/B%C3%A9zier_curve   *)
    (*on joue alors sur les parametre pour ne pas avoir des choses trop bizarre  *)
    (*ça nous donne donc une line noir qui à l'allure d'une courbe  *)
        set_color 0;
        let a = Random.int 200 + 50 + i in (*le i est pour décoré il sert à rien  *)
        curveto (current_x(),Random.int 200 + 200) 
        (current_x() + a, Random.int 200 + 300) 
        (current_x() + a ,current_y());
    (*on en fait deux d'affiler avec l'idée davoir concave convexe  *)

        curveto (current_x(),Random.int 200 - current_y()) 
        (current_x() + a, current_y() - Random.int 100 - 100) 
        (current_x() + a ,current_y());
    done;
    set_color yellow;
    (*ici c'est la partie qui fait mal, car elle est couteuse
    on va chercher tout les points noirs puis on va tracer des lingnes jaune comme le ricard  *)
    for i = 0 to Array.length screen - 1 do
        for j = 0 to Array.length screen.(0) - 1 do
            moveto i j;
            if point_color i j = black then lineto i 0;
        done;
    done;
    while true do
        set_color green;
        fill_circle 50 300 10;
    done

(* **PREMIERE VERSION**
#L'idée est de faire des courbes concave / convexe à l'aide d'un polynome d'odre 2
#Ça permet d'avoir un côté aléatoire avec des coef a b c différent
#Pour la continuité des courbes j'ai fait des fonctions qui permettent de trouver le 
#X pour un Y donné et inversement
#Cette version est totalement cassé mais si ça peut vous inspirez

let findY a b c x = 
    int_of_float(a*.x*.x +. b*.x +. c)

let findX a b d y = 
    let c = d -. y in
    let plus = (sqrt(b*.b -. 4.*.a*.c) -. b) /. (2. *. a) in
    let moins = (-.sqrt(b*.b -. 4.*.a*.c) -. b) /. (2. *. a) in
    if plus >= 0.0 then int_of_float plus
    else int_of_float moins

let sky_color = rgb 52 146 235 
let set_coul t =
    match t with
        | Air -> set_color sky_color
        | Sable -> set_color yellow
        | Balle -> set_color black 

let ini a b c =
     a := (Random.float 0.1) +. 0.001;
     b := ((Random.float 1.0) +. 0.);
     c := ((Random.float 10.))
# ici c'est un peu abstrait mais ce que j'essaie de faire c'est de trouver
# un a et b tel que pour un x donné on soit à un point critque d'ou le bool conv
# qui permet de donner l'info si on veut ensuite concave / convexe avec a < 0 ou non

let ini2 a b c x conv =
    if !conv then a := 0. -. (Random.float 0.1) -. 0.001
    else a := ((Random.float 0.1) +. 0.001);
    b := (float_of_int(x))*.(!a) /. (0. -. 2.);
    c := ((Random.float 10.))
let _ =
    open_graph " 1000x1000";
    Random.self_init ();
    let screen = Array.make_matrix 1000 1000 Air in
    
    ** ANCIEN CODE DE CLEM 1**
    (* let currentlh = ref 200 in
    let lhmax = ref ((Random.int 150) + 50) in
    let xl = ref (Random.int 150) in
    let reset a b c rindex index x =
    ini a b c;
    rindex := ((Random.int 150) + 50);
    index := 0;
    x := findX !a !b !c (float_of_int(!y))
    *)

    let index = ref 0 in
    let rindex = ref 200 in
    let a,b,c= ref 0., ref 0., ref 0. in
    let x = ref 0 in
    let conv = ref false in
    ini2 a b c !x conv;
    let y = ref 200 in (*Position de depart*)

    for i = 0 to Array.length screen - 1 do
        (*if !y > 990 then begin
            y := (findY !a !b !c (float_of_int(!x))) - (findY !a !b !c (float_of_int(!x - 1))) ;
            x := !x - 1 ;
            incr index
        end
        else begin
            y := (findY !a !b !c (float_of_int(!x))) +1 ;
            incr x;
            incr index
        end;*)

        if !y > 990 then y := 990
        else if !y < 10 then y := 10;
        for j = !y downto 0 do
            screen.(i).(j) <- Sable;
        done;
        
        y := (findY !a !b !c (float_of_int(!x))) +1 ;
        incr x;
        incr index;


        if !index = !rindex 
        then begin
            conv := true;
            ini2 a b c !x conv;
            rindex := ((Random.int 150) + 50);
            index := 0;
            x := findX !a !b !c (float_of_int(!y));
        end;
    done;
    
    ** ANCIEN CODE DE CLEMENT 2 **
    (*for i = 0 to Array.length screen-1 do
        if !currentlh >= !lhmax 
        then currentlh := !currentlh -1
        else currentlh := !currentlh +1;
        

        for j = !currentlh downto 0 do
            screen.(i).(j) <- Sable;
        done;
        
        if i = !xl
        then begin
            lhmax := (Random.int 150) + 50;
            xl := i + (Random.int ((Array.length screen)-1 -i))
        end

    done;*)

    for i = 0 to Array.length screen -1 do
        for j = 0 to Array.length screen.(0)-1 do
            begin
            set_coul screen.(i).(j);
            plot i j;
            end
        done
    done;

    while true do
        set_color green;
        fill_circle 50 300 10;
    done

*)