type couleur = Noir |Blanc ;;
type piece = Roi|Reine|Fou|Cavalier|Tour|Pion;;

type case = Vide | Piece of ( piece * couleur ) ;;

type echiquier =           { ech : case array array}
									  ;;


let verif_deplacement_pion echiquier  (idep,jdep) (iarr,jarr) =
(*  VÈrifie si le pion peut effectuer le dÈplacement donnÈ sans prendre en compte les Èchecs à la dÈcouverte*)
let n = Array.length(echiquier) in 

        let cased = echiquier.(idep).(jdep) in 
        let casea = echiquier.(iarr).(jarr) in

        match casea with 
        |Vide-> ( match cased with |Piece (Pion,Noir) -> if iarr = idep-1 && jarr=jdep  then true
                                                        else if iarr = idep -2 && jarr=jdep && idep = n-2 && echiquier.(idep-1).(jdep)= Vide then true 
                                                        else false
                                   |Piece (Pion,Blanc) -> if iarr = idep +1 && jarr=jdep then true
                                                         else if iarr= idep +2 && jarr=jdep  && idep = 1 && echiquier.(idep+1).(jdep) = Vide then true 
                                                         else false 
                                   |_ -> failwith "Aucun pion sur cette case"  ) 
        |Piece(_,coul)->( match cased with 

                |Piece (Pion,Noir) ->
                                 if iarr=idep -1 && ( jdep =1+jarr || jdep=jarr-1) && coul = Blanc  && iarr= 0 then failwith " Promotion"
                                 else if iarr=idep -1 && ( jdep =1+jarr || jdep=jarr-1) && coul = Blanc then true
                                 else false 
                                
                                
                |Piece(Pion,Blanc)->
                                if iarr=idep +1 && ( jdep =1+jarr || jdep=jarr-1) &&  coul =Noir  && iarr = n-1 then failwith "Promotion"
                                else if iarr=idep +1 && ( jdep =1+jarr || jdep=jarr-1) &&  coul =Noir then true
                                else false
                |_ -> failwith "Aucun pion sur cette case" )
                                ;;

let verif_deplacement_fou echiquier (idep,jdep) (iarr,jarr) = 
    (* VÈrifie qu'un dÈplacement de fou est valide c à d vÈrifie les obstruction par les autres pièces et que la case d'arrivÈe fait partie des cases où ce fou peut aller 
    EntrÈe : echiquier, coordonnÈe d , coordonnÈe a . 
    Sortie : Erreur si cases d'arrivÈe est dans la meme colonne ou meme ligne que la case de dÈpart, sinon bool indiquant que le fou respecte ses regles de dÈplacements    *)  
    let n =Array.length echiquier in 
    if iarr<0 || jarr<0 || iarr>n-1 || jarr>n-1 then false else 
    
    let distance_d_a = abs (idep - iarr) in
    if distance_d_a = 0 || jarr-jdep=0 then false 
    else ( 
    let direction_i,direction_j = (iarr - idep) / distance_d_a , ( jarr - jdep)/ distance_d_a in
    
    let res = ref true in   
    res :=  (idep + distance_d_a * direction_i = iarr) && (jdep + distance_d_a * direction_j=jarr); 
    for k =1 to distance_d_a -1 do 
        res := !res && (echiquier.(idep + k*direction_i).(jdep+k*direction_j)=Vide)
    done ;
    
    !res ) ;;
let verif_deplacement_cavalier echiquier (idep,jdep) (iarr,jarr) =
    let n =Array.length echiquier in 
    if iarr<0 || jarr<0 || iarr>n-1 || jarr>n-1 then false
    else 
    (abs (iarr-idep) + abs (jarr-jdep) =3) && idep <> iarr && jdep <> jarr
;;

let verif_deplacement_tour echiquier (idep,jdep) (iarr,jarr) =
    let res=ref true in
    if idep=iarr then begin
      if jdep<jarr then begin
         for k=jdep+1 to jarr-1 do
             res:= (echiquier.(idep).(k)=Vide) && !res
             done;
             !res end
      else begin
         for k=jdep -1 downto jarr+1 do
         res:= (echiquier.(idep).(k)=Vide) && !res
         done;
             !res end end
    else if jdep=jarr then  begin
      if idep<iarr then begin
          for l=idep+1 to iarr-1 do
          res:= (echiquier.(l).(jdep)=Vide) && !res
          done;
          !res end
      else begin
          for l=idep -1 downto iarr+1 do
          res:= (echiquier.(l).(jdep)=Vide) && !res
          done;
          !res end end
    else false
;;

let verif_deplacement_reine echiquier (idep,jdep) (iarr,jarr) =
 verif_deplacement_tour echiquier (idep,jdep) (iarr,jarr) || verif_deplacement_fou echiquier (idep,jdep) (iarr,jarr)
;;

let verif_deplacement_roi echiquier (idep,jdep) (iarr,jarr) =   let n = Array.length echiquier in
        if iarr<0 || jarr<0 || iarr> n-1 || jarr>n-1 then false
        else
        let cased = echiquier.(idep).(jdep) in
        let  casea = echiquier.(iarr).(jarr) in 
        match cased with
        |Piece(Roi,coul)-> (match casea with |Piece(_,coula) when coula = coul-> false
                                            |_ -> if abs(idep-iarr)<2 && abs(jdep-jarr)<2 then true else false)
        |_-> failwith "Aucun roi sur cette case"
;;

let verifie_deplacement echiquier (idep,jdep) (iarr,jarr) =
    
    let n = Array.length echiquier in
    if iarr<0 || jarr<0 || iarr> n-1 || jarr>n-1 then false
    else let cased = echiquier.(idep).(jdep) in
            let casea = echiquier.(iarr).(jarr) in
        match cased with
        |Vide-> false
        |Piece(valeur, couleura) -> begin  match casea with
                                                        |Piece(_,couleurd) when couleurd=couleura-> false
                                                        |_-> begin match valeur with
                                                                    |Roi-> verif_deplacement_roi echiquier (idep,jdep) (iarr,jarr)
                                                                    |Fou-> verif_deplacement_fou echiquier (idep,jdep) (iarr,jarr)
                                                                    |Pion->verif_deplacement_pion echiquier (idep,jdep) (iarr,jarr)
                                                                    |Reine->verif_deplacement_reine echiquier(idep,jdep) (iarr,jarr)
                                                                    |Cavalier->verif_deplacement_cavalier echiquier(idep,jdep) (iarr,jarr)
                                                                    |Tour->verif_deplacement_tour echiquier(idep,jdep) (iarr,jarr) end end ;;


let rec rajoute_cases_possibles echiquier (idep,jdep) pile =
(*Rajoute les cases accessibles ‡ partir de la case (idep,jdep) ‡ la pile*)
for i=0 to Array.length(echiquier) do
      for j=0 to Array.length(echiquier) do
        if (verifie_deplacement echiquier (idep,jdep) (i,j))
        then Stack.push ((idep,jdep),(i,j)) pile
      done;
     done;
;;

let ex_petit_echiquier =             [|[|Piece(Cavalier,Blanc);Piece(Pion,Blanc);Vide;Vide|]
                                    ;[|Vide;Piece(Roi,Blanc);Piece(Pion,Blanc) ;Vide|]
                                    ;[|Vide;Piece(Pion,Noir);Vide ;Vide |] 
                                    ;[|Vide; Piece(Pion,Noir); Vide; Vide |] |];;

let coups_possibles_couleur echiquier couleur =
    (*Renvoie la pile des coups possibles pour une couleur sans prendre en compte les Èchecs *)
    let pile=Stack.create () in
    let n=Array.length echiquier in
    for idep=0 to n-1 do
    for jdep=0 to n-1 do
        match echiquier.(idep).(jdep) with
        |Piece (v,c) when c=couleur -> rajoute_cases_possibles echiquier (idep,jdep) pile
        |_ -> ()
done; done;
  pile
;;

let copie_echiquier echiquier=
    (*renvoie la copie d'un echiquier *)
    let n =Array.length echiquier in
    let copie = Array.make_matrix n n Vide in
    for i=0 to (n-1) do
        copie.(i) <- Array.copy echiquier.(i)
    done;
    copie;;

let couleur_opposee couleur=
if couleur=Noir then Blanc
else Noir
;;

let effectue_un_coup echiquier ((idep,jdep), (iarr,jarr)) =
(* Modifie l'echiquier en effectuant un coup. Si le coup mËne ‡ une position illÈgale
 (ou le roi serait en Èchec), remet l'Èchiquier dans sa position d'avant le coup *)

 let cased = echiquier.(idep).(jdep) in 
 
 match cased with 
             |Vide->failwith"Pas de pieces sur cette case"
             |Piece(_,couleur)->begin
 
 let casea = echiquier.(iarr).(jarr) in
             if verifie_deplacement echiquier (idep,jdep) (iarr,jarr) then
                  begin echiquier.(iarr).(jarr) <- cased  ; echiquier.(idep).(jdep)<-Vide ;
                                  
                  if not (verifie_position echiquier couleur)
                   then (echiquier.(idep).(jdep)<-cased ; echiquier.(iarr).(jarr)<- casea ); end
                   else failwith "dÈplacement non valide" end ;;

let coups_possibles_sans_echecs echiquier couleur =
    (*renvoie la liste des coups possibles en tenant compte des Èchecs *)
      List.filter (fun coup -> let copie = copie_echiquier echiquier in
                               effectue_un_coup copie coup;
                               not (echiquier = copie ) )
                  (list_of_stack (coups_possibles_couleur echiquier couleur))
;;

let position_Roi_Couleur echiquier couleur =
    (*Renvoie la position du Roi de la couleur donnÈe en entrÈe *)
        let n = Array.length echiquier in
        let posRoi =ref  (-1,-1) in
        for i = 0 to n-1 do
            for j = 0 to n-1 do
                if  echiquier.(i).(j)= Piece(Roi,couleur) then
                     posRoi :=(i,j)  
                    
                 done; done; !posRoi ;;
    
let verifie_echec_couleur echiquier couleur=
(*Prend en entrÈe un echiquier et une couleur et renvoie un 
boolÈen indiquant si le roi de la couleur selectionnÈe est en echec*)

    let couleur_adverse =  couleur_opposee couleur in
    let iRoi,jRoi= position_Roi_Couleur echiquier couleur in
    
    let pile=coups_possibles_couleur echiquier couleur_adverse in
    let res = ref false in
    while not (Stack.is_empty pile) do
        let dep,arr= Stack.pop pile in 
            res := !res || ((iRoi,jRoi) = arr)
            done;
            !res
   ;;

let verifie_position echiquier couleur = 
        (* prend en entrÈe un echiquier et la couleur du joueur qui vient de jouer et 
        renvoie un  boolÈen determinant si le joueur en entrÈe n'a pas son roi en echec *) 

      not (verifie_echec_couleur echiquier couleur) 

 ;;
 
let rec list_of_stack pile=
   (*renvoie une liste contenant les ÈlÈments de la pile *)
    if (not (Stack.is_empty pile)) then
    let n=(Stack.pop pile) in
    n::list_of_stack pile
    else []
;;

let resolution echiquier couleur n =
   let rec resolutionjoueur echiquier ncoups =
      (*renvoie true si le problËme a une solution en ncoups coups et false sinon*)
     if ncoups = 0 then false
     else let coups_possibles = coups_possibles_sans_echecs echiquier couleur in
          match coups_possibles with
           | t :: q -> let nouvelle_position = copie_echiquier echiquier in
                       effectue_un_coup nouvelle_position t;
                       if resolutionadversaire nouvelle_position ncoups then true
                       else resolutionjoueur_suite echiquier ncoups q
           | [] -> false

and

   resolutionjoueur_suite echiquier ncoups = function
   (*renvoie true si le problËme a une solution en ncoups coups commenÁant par un coup
   de la liste en entrÈe et false sinon*)
            | [] -> false
            | t :: q -> let nouvelle_position = copie_echiquier echiquier in
                        effectue_un_coup nouvelle_position t;
                        if resolutionadversaire nouvelle_position ncoups then true
                        else resolutionjoueur_suite echiquier ncoups q

and

   resolutionadversaire echiquier ncoups =
    (*renvoie true si le problËme a une solution pour tous coups de l'adversaire et false sinon *)
         let c = couleur_opposee couleur in
         let coups_possibles = coups_possibles_sans_echecs echiquier c in
            match coups_possibles with
             | [] -> (verifie_echec_couleur echiquier c)
             | t :: q -> let nouvelle_position = copie_echiquier echiquier in
                  effectue_un_coup nouvelle_position t;
                  let a_une_solution = resolutionjoueur (nouvelle_position) (ncoups - 1) in
                  a_une_solution && resolutionadversaire_suite echiquier ncoups q

and

   resolutionadversaire_suite echiquier ncoups = function
   (*renvoie true si le problËme a une solution pour tous coups de la liste en entrÈe et false sinon *)
           | [] -> true
           | t :: q -> let nouvelle_position = copie_echiquier echiquier in
                    effectue_un_coup nouvelle_position t;
                    let a_une_solution = resolutionjoueur (nouvelle_position) (ncoups - 1) in
                    a_une_solution && resolutionadversaire_suite echiquier  ncoups q

   in
   resolutionjoueur echiquier n
;;

resolution [|[|Vide;Vide;Vide;Piece (Tour, Noir)|]
        ;[|Vide;Piece(Roi,Blanc);Vide ;Piece(Fou,Noir)|]
        ;[|Vide;Piece(Pion,Noir);Vide ;Vide |] 
        ;[|Vide;Piece(Roi,Noir); Vide; Vide |] |] Noir 1 (*Cela renvoie bien "true" *);;

resolution [|[|Vide;Vide;Vide;Vide|]
        ;[|Vide;Piece(Roi,Blanc);Vide ;Piece(Fou,Noir)|]
        ;[|Vide;Piece(Pion,Noir);Vide ;Vide |] 
        ;[|Vide;Piece(Roi,Noir); Vide; Vide |] |] Noir 1 (*Cela renvoie bien "false" *);;