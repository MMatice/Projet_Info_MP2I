let affiche_des_trucs t =
	for i = 0 to Array.length t - 1 do
		Printf.printf "%d" t.(i)
	done

let _ =
	let t = [|13; 15; 6; 6|] in
	affiche_des_trucs t