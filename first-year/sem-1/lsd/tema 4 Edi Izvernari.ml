(*problema 1*)
 let numar_judete lst = List.fold_left (fun acc (nume,populatie) -> acc + 1 ) 0 lst ;;
  numar_judete [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168); ("Bihor", 575398)] ;;
  numar_judete [("Alba", 342376); ("Arad", 430629); ("Arges", 612431)] ;;
  numar_judete [("Alba", 342376)] ;;
  numar_judete [("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714)] ;;
  numar_judete [("Botosani", 412626); ("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069); ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106); ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745);] ;;


(*problema 2*)
let modifica_populatie nume_jud x lst = List.map (fun (nume,populatie) -> if nume_jud = nume then (nume,populatie + x) else (nume,populatie)) lst;;
  modifica_populatie "Alba" 100000 [("Alba", 342376); ("Arad", 430629); ("Arges", 612431)] ;;
  modifica_populatie "Arad" (-200000) [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Bacau", 616168); ("Bihor", 575398)] ;;
  modifica_populatie "Timis" 16460 [("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714)] ;;
  modifica_populatie "Alba" 1 [("Alba", 342376)] ;;


(*problema 3*)
let media_populatie lst = List.fold_left (fun acc (nume,pop) -> acc + (pop / numar_judete lst)) 0 lst ;;
  media_populatie [("Alba", 342376); ("Arad", 430629); ("Arges", 612431)] ;;  
  media_populatie [("Timis", 683540)] ;;
  media_populatie [("Cluj", 691106); ("Constanta", 684082); ("Covasna", 210177); ("Dambovita", 518745); ("Dolj", 660544); ("Galati", 536167); ("Giurgiu", 281422)] ;;
  media_populatie [("Cluj", 691106); ("Constanta", 684082)] ;;


(*problema 4*)
let statistica x lst =
  let populatie x lst = List.fold_left (fun acc (nume,pop) -> if nume = x then acc + pop else acc) 0 lst in
  List.map (fun (nume,pop) -> nume) (List.filter (fun (nume,pop) -> pop - populatie x lst >0 ) lst )
  ;; 
  statistica "Arad" [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Caras-Severin", 295579)] ;;
  statistica "Caras-Severin" [("Alba", 342376); ("Arad", 430629); ("Arges", 612431); ("Caras-Severin", 295579)] ;;
  statistica "Brasov" [("Brasov", 549217); ("Braila", 321212); ("Buzau", 451069); ("Caras-Severin", 295579); ("Calarasi", 306691); ("Cluj", 691106); ("Constanta", 684082)] ;;
  statistica "Vaslui" [("Timis", 683540); ("Tulcea", 213083); ("Vaslui", 395499); ("Valcea", 371714); ("Vrancea", 340310)] ;;  
