(*Crée des arpèges et renvoie leur codes MIDI ou leur fréquences*)
(*Usage :
- Créer une note fondamentale : avec let _nomNote_ = {n=_Tonalité_; a=_Diese/Becarre_; o=_NumArpege_};;
- Créer un arpège : let _nomArpege_ noteFondamentale intervalle1 intervalle2 nbOctave typeArpege
- typeArpege peut etre Montant,Descendant ou MonDes (Pour Montant puis Descendant)
- L'arpège sera donné sous forme de liste de notes en MIDI
- Si on veut en liste de fréquences, appeler : notesJouables arpege
- Des exemples sont données en fin de code, et on peut jouer les notes sur http://guilhem.jaber.fr/midi-player/ par exemple (Site de mon professeur au moment du tp)
*)

type nom =
	| Do
	| Re
	| Mi
	| Fa
	| Sol
	| La
	| Si
;;

type alteration =
	| Becarre
	| Diese
;;

type hauteur = {n : nom; a : alteration; o : int};;

let nom_vers_demitons x =
	match x with
	| Do -> 36
	| Re -> 38
	| Mi -> 40
	| Fa -> 41
	| Sol -> 43
	| La -> 45
	| _ -> 47
;;

let alteration_vers_demitons x =
	match x with
	| Becarre -> 0
	| _ -> 1
;;

let octave_vers_base_midi x = (x-1)*12;;

let note2midi h = nom_vers_demitons h.n + alteration_vers_demitons h.a + octave_vers_base_midi h.o;;

let midi2freq m = 440.*.2.**((float_of_int(m)-.69.)/.12.);;

type typeArpege =
|Montant
|Descendant
|MonDes

let rec arpegeAux basseMidi itv1 itv2 nbOctave listNotes =
	if (nbOctave >1) then(
		arpegeAux (basseMidi+12) itv1 itv2 (nbOctave-1) (listNotes@[basseMidi ; (basseMidi)+itv1 ; (basseMidi)+itv2]);
	)else(
		listNotes@[basseMidi ; (basseMidi)+itv1 ; (basseMidi)+itv2 ; basseMidi+12];
	)
	;;

let arpege hauteur itv1 itv2 nbOctave typeArpege = 
	match typeArpege with
	|Montant -> arpegeAux (note2midi hauteur) itv1 itv2 nbOctave []
	|Descendant -> List.rev (arpegeAux (note2midi hauteur) itv1 itv2 nbOctave [])
	|MonDes -> let res = arpegeAux (note2midi hauteur) itv1 itv2 nbOctave [] in res@(List.rev res)
;;

let join l sep = 
	let rec aux l sep = 
	match l with
	|[] -> ""
	|[x] -> string_of_int x
	|x::reste -> (string_of_int x)^sep^(aux reste sep)
	in aux l sep
	;;

(*soucis d'inversion, a regler plus tard*)
let notesJouables l = 
	let rec aux l = 
		match l with
		|[] -> failwith "Aucune note a convertir en fréquence !"
		|[x]-> [(int_of_float(midi2freq x))]
		|x::reste -> ([(int_of_float(midi2freq x))]@(aux reste))
		in join (aux l) ","
	;;
	
let do2 = {n=Do; a=Becarre; o=2};; (*Déclaration d'un do2*)
let do4 = {n=Do; a=Diese; o=4};; (*Déclaration d'un do4 diese*)
let arpege1 = arpege do2 4 7 3 MonDes;; (*Do2 majeur*)
let arpege2 = arpege do4 4 7 1 Montant;;(*Do4 majeur*)
let exemple1 = notesJouables arpege1;;
let exemple2 = notesJouables arpege2;;

