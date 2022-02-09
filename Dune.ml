(* Documentation de graphics ici : http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html *)

open Graphics
(* ATTENTION J'AI L'IMPRESSION QU'IL Y A UN BUG D'AFFICHAGE  *)


type case = Air | Sable | Balle;;
let sky_color = rgb 52 146 235 (*COULEUR CIEL  *)
let contact_check x y a =
    not (a.(x).(y - 5) = Sable || a.(x - 5).(y) = Sable || a.(x + 5).(y) = Sable || a.(x).(y) = Sable) (* version primaire de la gravité *)
let go_down x y =  (* Prend les coord actual et decr de 1 en y  *)
    set_color sky_color;         (* Permet d'effacer *)
    fill_circle !x !y 5;   (* l'ancienne balle *)
    set_color green;             (* Dessine la nouvelle *)
    fill_circle !x (!y - 1) 5;
    decr y
let capture x y = (* prend une capture sans la boule  *)
    set_color sky_color;         (* Permet d'effacer *)
    fill_circle x y 5;   (* l'ancienne balle *)
    let a = get_image 1 0 500 500 in
    draw_image a 0 0;
    set_color green;             (* Dessine la nouvelle *)
    fill_circle x y 5
let fill i k abs screen = (* remplit la ligne pour les points noirs *)
    for j = k to !abs + 20 do
            moveto i j;
            if point_color i j = black 
            then begin 
                lineto i 0;
                abs := j;
                screen.(i).(j) <- Sable
                end;
        done
let _ =
    open_graph " 500x500"; (*500x500  *)
    let screen = Array.make_matrix 500 500 Air in 
    let y = ref 250 in (* Hauteur de départ de la balle et gerera la hauteur de la balle
                               dans le temps*)
    let x = ref 50 in (* x correspdant à la balle, on ne le change pas mais c'est 
        plus pratique *)
    
    moveto 0 100; (*point de depart  *)
    set_color sky_color;
    fill_rect 0 0 500 500; (*on fill un rectangle pour le ciel (background)  *)
    Random.self_init(); (*seed random  *)

    for i = 0 to 4 do 
    (*l'idée est d'uilisé la fonction curveto qui prend trois points + le point de départ voir la doc
    pour plus de détails suivant le modéle de Bézier jcrois https://en.wikipedia.org/wiki/B%C3%A9zier_curve   *)
    (*on joue alors sur les parametre pour ne pas avoir des choses trop bizarre  *)
    (*ça nous donne donc une line noir qui à l'allure d'une courbe  *)
        set_color 0;
        let a = Random.int 100 + 25 + i in (*le i est pour décoré il sert à rien  *)
        curveto (current_x(),Random.int 100 + 100) 
        (current_x() + a, Random.int 100 + 200) 
        (current_x() + a +10,current_y());
    (*on en fait deux d'affiler avec l'idée davoir concave convexe  *)

        curveto (current_x(),Random.int 200 - current_y()) 
        (current_x() + a, current_y() - Random.int 50 - 50) 
        (current_x() + a +10,current_y());
    done;
    set_color yellow;
    (*
        ici c'est la partie qui fait mal, car elle est couteuse
    on va chercher tout les points noirs puis on va tracer des lingnes jaune comme le ricard  *)
    let abs = ref 100 in
    moveto 0 !abs;
    for i = 0 to Array.length screen - 1 do (* opti un peu l'affiche en évitant de se taper les 500 pixels de chaque ligne*)
        if !abs < 30 then fill i 0 abs screen
        else fill i (!abs - 30) abs screen;
    done;
    while true do
        set_color green;
        fill_circle !x !y 5;
        (*let touche_sable = ref false in      sera utile plus tard normalement *)
        let gravite = ref (contact_check !x !y screen) in
        (* on regarde si la balle est en contact avec le sol, plus tard on pourra imaginer
        un facteur vitesse qui permet déplacer les dunes  *)
        while !gravite do
            go_down x y;
            gravite := (contact_check !x !y screen);
        done;

        
        let action = wait_next_event [Key_pressed] in
        if (action.keypressed)
        then capture !x !y; (* on prend en capture l'écran et on copie à un pixel prés ou plus ça dépendera
        de la vitesse  *)

        (* Regarde si une touche du CLAVIER (d'autres fonction de la lib ne regarde que la souris)
           est presser et si c'est le cas la balle descend si elle touche du sable elle descend pas*)
    done;