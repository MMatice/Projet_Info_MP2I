(* Documentation de graphics ici : http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html *)

open Graphics
open Unix
(* ATTENTION J'AI L'IMPRESSION QU'IL Y A UN BUG D'AFFICHAGE  *)


type case = Air | Sable | Balle;;
let sky_color = rgb 52 146 235 (*COULEUR CIEL  *)
let velocity = ref 0 (* Pour gérer la vitesse *)
let ylastballe = ref 0 (* position du dernier balle*)
let xpos = ref 0 
let contact_check x y a =
    not (a.(x).(y - 5) = Sable || a.(x - 5).(y) = Sable || a.(x + 5).(y) = Sable || a.(x).(y) = Sable) (* version primaire de la gravité *)
let go_down y =  (* Prend les coord actual et decr de velocity qui peut etre négatif en y  *)
    ylastballe := !y;
    y := !y + !velocity

    
let capture x y = (* prend une capture sans la boule  *)
    set_color sky_color;         (* Permet d'effacer *)
    fill_circle x !ylastballe 5;   (* l'ancienne balle *)
    let a = get_image 1 0 1000 500 in
    draw_image a 0 0;
    set_color green;             (* Dessine la nouvelle *)
    fill_circle x y 5;
    xpos := !xpos + 1 
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
    open_graph " 1000x500"; (*1000x500  pour que la génération procédurale soit bien en amont du gameplay et donc que le joueur ne soit pas impacté par la génération des dunes *)
    let screen = Array.make_matrix 500 500 Air in 
    let y = ref 250 in (* Hauteur de départ de la balle et gerera la hauteur de la balle
                               dans le temps*)
    let x = ref 50 in (* x correspdant à la balle, on ne le change pas mais c'est 
        plus pratique *)
    
    moveto 0 100; (*point de depart  *)
    set_color sky_color;
    fill_rect 0 0 1000 500; (*on fill un rectangle pour le ciel (background)  *)
    Random.self_init(); (*seed random  *)

    let trace_curve pos = (* generation procedurale des dunes => plus ou moins un copié collé de la fonction pour créer les premieres dunes*)
        Printf.printf "curve";
        set_color 0;
        moveto 680 (current_y());
        let a = 50+ 25 in (*le i est pour décoré il sert à rien  *)
        curveto (current_x(),Random.int 100 + 100) 
        (current_x() + a, Random.int 100 + 200 + pos) (* pos ne sert à rien c'est juste pour pas que trace_curve soit appelle directement mais considérer une fonction*)
        (current_x() + a +10,current_y());
        (*on en fait deux d'affiler avec l'idée davoir concave convexe  *)

        curveto (current_x(),Random.int 200 - current_y()) 
        (current_x() + a, current_y() - Random.int 50 - 50) 
        (current_x() + a +10,current_y());
    in
    

    for i = 0 to 4 do 
    (*l'idée est d'uilisé la fonction curveto qui prend trois points + le point de départ voir la doc
    pour plus de détails suivant le modéle de Bézier jcrois https://en.wikipedia.org/wiki/B%C3%A9zier_curve   *)
    (*on joue alors sur les parametre pour ne pas avoir des choses trop bizarre  *)
    (*ça nous donne donc une line noir qui à l'allure d'une courbe  *)
        set_color 0;
        let a = 50+ 25 + i - i in (*le i est pour décoré il sert à rien  *)
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
        (*let touche_sable = ref false in      sera utile plus tard normalement *)
        let gravite = ref (contact_check !x !y screen) in
        (* on regarde si la balle est en contact avec le sol, plus tard on pourra imaginer
        un facteur vitesse qui permet déplacer les dunes  *)
        if !gravite then
            begin
            gravite := (contact_check !x !y screen);
            if !velocity > -1 then velocity := !velocity - 1;    
                                                                    (* en chute libre naturelle la vélocité est diminué de -1 et est de -1 au max*)
            if button_down () then velocity := !velocity - 1; (* si on veut forcer la velocité de la balle*)
            end
        else Printf.printf "contact";
        if !velocity < -5 then (* velocité max de -5*)
            velocity := - 5;
        go_down y; (* ajuste juste la position avec la velocité*)
        sleepf 0.05; (* pour que le jeu se termine pas en 0.001s xd*)
        capture !x !y; (* met a jour visuellement la position en y de la balle*)
        if !xpos mod 170 = 0 then begin (* en gros cette partie gère la génération des nouvelles dunes PS les printfs ne servent a rien*)
            trace_curve 15;
            Printf.printf "curve"
            end

        
         (* on prend en capture l'écran et on copie à un pixel prés ou plus ça dépendera
        de la vitesse  *)

        (* Regarde si une touche du CLAVIER (d'autres fonction de la lib ne regarde que la souris)
           est presser et si c'est le cas la balle descend si elle touche du sable elle descend pas*)
    done;