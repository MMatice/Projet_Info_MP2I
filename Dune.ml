open Graphics
(* Documentation de graphics ici :

    http://pauillac.inria.fr/~remy/poly/ocaml/htmlman/libref/Graphics.html
*)
type case = Air | Sable | Balle;;


let _ =
    open_graph " 500x500";
    let screen = Array.make_matrix 500 500 Air in
    
    plot 50 50;
    lineto 20 20;
    lineto 20 20;

    let set_coul t =
    match t with
    | Air -> set_color blue
    | Sable -> set_color yellow
    | Balle -> set_color green
    in


    while true do
        for i = 0 to Array.length screen -1 do
            for j = 0 to Array.length screen.(0)-1 do
                begin
                set_coul screen.(i).(j);
                plot i j;
                end
            done
        done
    done

