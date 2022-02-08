(* Documentation de graphics ici : http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html *)

open Graphics
(* ATTENTION J'AI L'IMPRESSION QU'IL Y A UN BUG D'AFFICHAGE  *)


type case = Air | Sable | Balle;;
let sky_color = rgb 52 146 235 (*COULEUR CIEL  *)


let _ =
    open_graph "1000x1000"; (*1000x1000 car c'est beau  *)
    let screen = Array.make_matrix 1000 1000 Air in 
    let hballe = ref 500 in (* Hauteur de départ de la balle et gerera la hauteur de la balle
							   dans le temps*)
	
	
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
            if point_color i j = black 
			then begin 
				lineto i 0;
				screen.(i).(j) <- Sable;
				end;
        done;
    done;

    while true do
        set_color green;
        fill_circle 50 !hballe 10;
		
		let touche_sable = ref false in		(* sera utile plus tard normalement *)
		
		
		(* Regarde si une touche du CLAVIER (d'autres fonction de la lib ne regarde que la souris)
		   est presser et si c'est le cas la balle descend si elle touche du sable elle descend pas*)
		let action = wait_next_event [Key_pressed] in
		if (action.keypressed && screen.(50).(!hballe - 5) <> Sable)
		then begin
			touche_sable := false;								
			set_color sky_color;         (* Permet d'effacer *)
			fill_circle 50 !hballe 10;   (* l'ancienne balle *)
			set_color green;			 (* Dessine la nouvelle *)
			fill_circle 50 (!hballe - 2) 10;
			hballe := !hballe - 2;
			end;
    done