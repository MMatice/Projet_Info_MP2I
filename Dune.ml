open Graphics
(* Documentation de graphics ici :

    http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html
*)
type case = Air | Sable | Balle;;


let _ =
    open_graph " 500x500";
    Random.self_init ();
    let screen = Array.make_matrix 500 500 Air in
    let sky_color = rgb 52 146 235 in
    

    let currentlh = ref 200 in
    let lhmax = ref ((Random.int 150) + 50) in
    let xl = ref (Random.int 150) in 

    let set_coul t =
    match t with
    | Air -> set_color sky_color
    | Sable -> set_color yellow
    | Balle -> set_color black
    in

    for i = 0 to Array.length screen-1 do
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
    done;
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

