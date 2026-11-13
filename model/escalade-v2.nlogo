;; ============================================================
;;  ESCALADE v2 : genetique (morphologie) VS technique (entrainement)
;;
;;  VOIES
;;   - chaque prise est un agent : type (jug, pinch, crimp, sloper,
;;     mono, jib), qualite d'accroche 1-5, booleen sommet?,
;;     devers du support (patch)
;;   - mode aleatoire (cotation, type de voie, hauteur) ou
;;     mode existant (chargement d'un fichier CSV)
;;  GRIMPEUR = 5 agents : 2 mains, 2 pieds, 1 centre de gravite (CdG)
;;   - chaque membre a force et endurance (1-5)
;;   - les membres restent a distance max du CdG
;;   - un membre verrouille signale aux autres
;;   - le CdG (= poids) tire les membres vers lui et vers le bas
;;   - niveau de prise vs niveau du membre => tient ou lache
;; ============================================================

extensions [ csv ]

globals [
  y-sommet-p          ;; hauteur du sommet (en patches)
  hauteur-reelle-m    ;; hauteur effective de la voie (en m)
  liste-cotations
  ordre-membres
  index-membre
  mouvement-ok?
  blocages
  nb-chutes
  nb-mouvements
  nb-lachers
  reussite?
  termine?
  hauteur-max-m
  fatigue-globale
  coef-prise          ;; constante de calibration : kg tenus par unite de (force x qualite)
  coef-pieds          ;; les jambes sont plus puissantes que les bras
  cout-fatigue
]

breed [ prises prise ]
breed [ membres membre ]
breed [ cdgs cdg ]
undirected-link-breed [ attaches attache ]   ;; lien CdG <-> membre

patches-own [ devers ]                       ;; 0 = vertical, 1 = tres desaxe
prises-own  [ type-prise qualite sommet? devers-support ]
membres-own [ type-membre main? force endurance energie verrouille? au-sol?
              prise-actuelle nb-signaux charge ]
cdgs-own    [ masse ]

;; ------------------------------------------------------------
;;  INITIALISATION
;; ------------------------------------------------------------

to setup
  clear-all
  set liste-cotations [ "2" "3" "4" "5a" "5b" "5c" "6a" "6b" "6c" "7a" "7b" "7c" "8a" "8b" "8c" "9a" ]
  set ordre-membres [ "main-d" "pied-g" "main-g" "pied-d" ]
  set coef-prise 4
  set coef-pieds 2
  set cout-fatigue 1.5
  reinitialiser-compteurs
  creer-voie
  creer-mur
  creer-grimpeur
  reset-ticks
end

;; nouvelle voie (et nouveau mur) sans tout reinitialiser
to nouvelle-voie
  ask prises [ die ]
  ask membres [ die ]
  ask cdgs [ die ]
  reinitialiser-compteurs
  creer-voie
  creer-mur
  creer-grimpeur
  clear-all-plots
  reset-ticks
end

to reinitialiser-compteurs
  set index-membre 0
  set mouvement-ok? false
  set blocages 0
  set nb-chutes 0
  set nb-mouvements 0
  set nb-lachers 0
  set reussite? false
  set termine? false
  set hauteur-max-m 0
  set fatigue-globale 0
end

;; ------------------------------------------------------------
;;  LES VOIES (prises = agents)
;; ------------------------------------------------------------

to creer-voie
  ifelse mode-voie = "aleatoire" [ creer-voie-aleatoire ] [ creer-voie-existante ]
  set y-sommet-p max [ycor] of prises
  set hauteur-reelle-m precision ((y-sommet-p - 1) * cm-par-patch / 100) 1
end

to-report difficulte-voie
  ;; 0 (cotation la plus facile) -> 1 (la plus dure)
  report (position cotation liste-cotations) / (length liste-cotations - 1)
end

;; Mode aleatoire : les prises sont reparties couche par couche entre
;; le depart et le sommet. Plus la cotation est haute, plus les prises
;; sont espacees, mauvaises, et de types difficiles.
to creer-voie-aleatoire
  let d difficulte-voie
  let coef-esp 1
  if type-voie = "bloc" [ set coef-esp 1.15 ]      ;; blocs : mouvements plus gros
  if type-voie = "falaise" [ set coef-esp 0.9 ]    ;; falaise : prises plus nombreuses
  let h-cap ifelse-value (type-voie = "bloc") [ 5 ] [ 200 ]
  let h-m min (list hauteur-voie h-cap ((max-pycor - 4) * cm-par-patch / 100))
  let y-top 1 + h-m * 100 / cm-par-patch
  let esp (30 + 40 * d) * coef-esp / cm-par-patch  ;; espacement vertical (patches)
  let x-cour (max-pxcor / 2) + random-float 6 - 3
  let y 1
  while [ y < y-top - esp * 0.5 ] [
    set x-cour max (list 4 (min (list (max-pxcor - 4) (x-cour + random-float 3 - 1.5))))
    creer-prise x-cour y (tirer-type d) (tirer-qualite d) false
    ;; prise secondaire (plus rare quand la voie est dure)
    if random-float 1 < (1 - 0.6 * d) [
      let dx (1 + random-float 2) * one-of [ -1 1 ]
      let y2 max (list 1 (min (list (y-top - 0.5) (y + random-float esp - esp / 2))))
      creer-prise (x-cour + dx) y2 (tirer-type d) (tirer-qualite d) false
    ]
    set y y + esp
  ]
  ;; le sommet : un bon jug
  creer-prise x-cour y-top "jug" 5 true
end

;; Mode existant : lecture d'un CSV (x_cm, y_cm, type, qualite, sommet)
to creer-voie-existante
  ifelse file-exists? fichier-voie [
    let lignes csv:from-file fichier-voie
    foreach but-first lignes [ ligne ->
      if length ligne >= 5 [
        let xc item 0 ligne
        let yc item 1 ligne
        let ty item 2 ligne
        let q item 3 ligne
        let s (item 4 ligne = 1)
        creer-prise (xc / cm-par-patch) (1 + yc / cm-par-patch) ty q s
      ]
    ]
    ;; si aucun sommet n'est indique, la prise la plus haute devient le sommet
    if not any? prises with [ sommet? ] [
      ask max-one-of prises [ ycor ] [ set sommet? true set shape "star" set size 4 set color yellow ]
    ]
  ] [
    user-message (word "Fichier introuvable : " fichier-voie ". Une voie aleatoire est creee a la place.")
    creer-voie-aleatoire
  ]
end

to creer-prise [ px py t q s ]
  create-prises 1 [
    setxy (max (list 1 (min (list (max-pxcor - 1) px)))) (max (list 1 (min (list (max-pycor - 1) py))))
    set type-prise t
    set qualite q
    set sommet? s
    set shape "circle"
    set color couleur-type t
    set size 0.8 + 0.4 * q          ;; plus la prise est rugueuse, plus elle est grosse
    if s [ set shape "star" set size 4 set color yellow ]
  ]
end

to-report tirer-type [ d ]
  let noms [ "jug" "pinch" "crimp" "sloper" "mono" "jib" ]
  let facile [ 40 20 20 10 5 5 ]
  let dur [ 5 15 35 20 15 10 ]
  let ps (map [ [ a b ] -> (1 - d) * a + d * b ] facile dur)
  let tirage random-float sum ps
  let choix last noms
  let cumul 0
  let trouve? false
  (foreach noms ps [ [ n p ] ->
    set cumul cumul + p
    if (not trouve?) and tirage <= cumul [ set choix n set trouve? true ]
  ])
  report choix
end

to-report tirer-qualite [ d ]
  report max (list 1 (min (list 5 round random-normal (4 - 2.5 * d) 0.9)))
end

;; coefficient de "tenue" selon le type de prise (hypothese a calibrer)
to-report coef-type [ t ]
  if t = "jug" [ report 1.0 ]
  if t = "pinch" [ report 0.7 ]
  if t = "crimp" [ report 0.6 ]
  if t = "sloper" [ report 0.5 ]
  if t = "mono" [ report 0.5 ]
  if t = "jib" [ report 0.4 ]
  report 0.5
end

to-report couleur-type [ t ]
  if t = "jug" [ report green ]
  if t = "pinch" [ report cyan ]
  if t = "crimp" [ report orange ]
  if t = "sloper" [ report violet ]
  if t = "mono" [ report red + 2 ]
  report gray + 3
end

;; ------------------------------------------------------------
;;  LE MUR (patches = devers)
;; ------------------------------------------------------------

to creer-mur
  ask patches [ set devers calcul-devers ]
  if profil-mur = "aleatoire" [
    ask patches [ set devers random-float devers-max ]
    repeat 6 [ diffuse devers 0.6 ]
  ]
  ;; plus c'est sombre, plus c'est desaxe
  ask patches [ set pcolor scale-color brown devers 1.6 -0.4 ]
  ask patches with [ pycor = 0 ] [ set pcolor green - 2 ]
  ;; chaque prise garde le devers de son support
  ask prises [ set devers-support [devers] of patch-here ]
end

to-report calcul-devers
  let h min (list 1 (pycor / y-sommet-p))
  if profil-mur = "devers-progressif" [ report devers-max * h ]
  if profil-mur = "toit" [ report ifelse-value (h > 0.4 and h < 0.6) [ devers-max ] [ 0 ] ]
  report 0
end

to-report dv-local
  report [devers] of patch-here
end

;; ------------------------------------------------------------
;;  LE GRIMPEUR : 4 membres + 1 centre de gravite
;; ------------------------------------------------------------

to creer-grimpeur
  foreach [ "main-g" "main-d" "pied-g" "pied-d" ] [ t ->
    create-membres 1 [
      set type-membre t
      set main? (substring t 0 4 = "main")
      set force ifelse-value main? [ force-mains ] [ force-pieds ]
      set endurance ifelse-value main? [ endurance-mains ] [ endurance-pieds ]
      set shape "square"
      set size 1.6
      set color ifelse-value main? [ blue ] [ lime ]
    ]
  ]
  create-cdgs 1 [
    set masse poids
    set shape "circle"
    set size 3
    set color red
    set label (word poids " kg")
    ask membres [ create-attache-with myself [ set color white ] ]
  ]
  placer-au-depart
end

;; depart : pieds au sol (verrouilles), mains libres
to placer-au-depart
  let x0 [xcor] of min-one-of prises [ ycor ]
  set blocages 0
  ask membres [
    set energie 100
    set verrouille? false
    set au-sol? false
    set prise-actuelle nobody
    set nb-signaux 0
    set charge 0
    let dx ifelse-value (last type-membre = "g") [ -1 ] [ 1 ]
    ifelse main? [ setxy (x0 + dx) 2 ] [ setxy (x0 + dx) 0 ]
  ]
  ask membres with [ not main? ] [ set verrouille? true set au-sol? true ]
  ;; chaque membre verrouille previent les autres
  ask membres with [ verrouille? ] [
    ask other membres [ set nb-signaux nb-signaux + 1 ]
  ]
  mettre-a-jour-cdg
end

;; poids relatif porte par un membre (selon le devers local) :
;; en vertical les pieds portent beaucoup, en devers ce sont les mains
to-report poids-rel [ est-main dv ]
  report ifelse-value est-main [ 1 + 2 * dv ] [ 2 * (1 - dv) + 0.2 ]
end

;; distance maximale membre <-> CdG, en patches
to-report portee-patch [ est-main ]
  report ifelse-value est-main [ dist-cdg-mains / cm-par-patch ] [ dist-cdg-pieds / cm-par-patch ]
end

;; Le CdG se situe sous les mains et au-dessus des pieds (distance = 60 %
;; de la portee) ; le devers l'abaisse davantage ("tension de gravite").
;; pts = liste de [x y main? devers] des membres verrouilles.
to-report cdg-depuis [ pts ]
  let sx 0
  let sy 0
  let sw 0
  foreach pts [ p ->
    let px item 0 p
    let py item 1 p
    let est-main item 2 p
    let dv item 3 p
    let w poids-rel est-main dv
    let rp portee-patch est-main
    let decalage ifelse-value est-main [ 0 - 0.6 * rp * (1 + gravite * dv) ] [ 0.6 * rp ]
    set sx sx + w * px
    set sy sy + w * (py + decalage)
    set sw sw + w
  ]
  let cx sx / sw
  let cy sy / sw
  report (list cx (max (list min-pycor (min (list max-pycor cy)))))
end

;; CdG si le membre m se verrouillait sur la prise h
to-report cdg-si [ m h ]
  let pts [ (list xcor ycor main? dv-local) ] of membres with [ verrouille? and self != m ]
  let est-main [main?] of m
  set pts lput (list ([xcor] of h) ([ycor] of h) est-main ([devers-support] of h)) pts
  report cdg-depuis pts
end

to mettre-a-jour-cdg
  let pts [ (list xcor ycor main? dv-local) ] of membres with [ verrouille? ]
  if not empty? pts [
    let c cdg-depuis pts
    ask cdgs [ setxy (item 0 c) (item 1 c) ]
  ]
end

;; ------------------------------------------------------------
;;  CALCUL "NIVEAU DE LA PRISE" vs "NIVEAU DU MEMBRE"
;; ------------------------------------------------------------

;; capacite (en kg) d'un membre m sur la prise h :
;; force x qualite x type de prise x fraicheur (endurance restante)
to-report capacite-sur [ m h ]
  let q [qualite] of h
  let tp [type-prise] of h
  let c coef-type tp
  let f [force] of m
  let e [energie] of m
  let facteur-fatigue 0.4 + 0.6 * e / 100
  let facteur-membre ifelse-value [main?] of m [ 1 ] [ coef-pieds ]
  report coef-prise * f * q * c * facteur-fatigue * facteur-membre
end

;; charge (en kg) que porterait m sur la prise h, selon la repartition du poids
to-report charge-estimee [ m h ]
  let dv [devers-support] of h
  let est-main [main?] of m
  let w-m poids-rel est-main dv
  let autres sum [ poids-rel main? dv-local ] of membres with [ verrouille? and self != m ]
  report poids * w-m / (w-m + autres)
end

to-report ratio-charge
  ;; contexte : un membre verrouille sur une prise
  report charge / capacite-sur self prise-actuelle
end

;; ------------------------------------------------------------
;;  BOUCLE PRINCIPALE (1 tick = 1 seconde)
;; ------------------------------------------------------------

to go
  if termine? [ stop ]

  ;; 1. un membre tente de se deplacer (alternance mains / pieds)
  let fait? false
  let essai 0
  while [ not fait? and essai < 4 ] [
    let t item ((index-membre + essai) mod 4) ordre-membres
    tenter-deplacement one-of membres with [ type-membre = t ]
    set fait? mouvement-ok?
    set essai essai + 1
  ]
  ifelse fait? [
    set index-membre (index-membre + essai) mod 4
    set nb-mouvements nb-mouvements + 1
    set blocages 0
  ] [
    set blocages blocages + 1
  ]

  ;; 2. physique : CdG, charges, tenue des prises, fatigue
  mettre-a-jour-cdg
  calculer-charges
  verifier-prises
  fatigue-membres
  tirer-membres-libres
  mettre-a-jour-indicateurs

  ;; 3. fin de simulation ?
  if any? membres with [ main? and verrouille? and au-sommet? ] [
    set reussite? true
    set termine? true
    tick
    stop
  ]
  if count membres with [ verrouille? ] < 2 or blocages > 20 [ chute ]
  if nb-chutes >= nb-essais-max or ticks >= duree-max [ set termine? true ]
  tick
end

;; Le membre m cherche la meilleure prise accessible.
to tenter-deplacement [ m ]
  set mouvement-ok? false
  let ns [nb-signaux] of m
  ;; il faut qu'au moins 2 autres membres aient signale qu'ils sont verrouilles
  if ns < 2 [ stop ]
  let occupees [prise-actuelle] of membres
  let est-main [main?] of m
  let ym [ycor] of m
  let cx0 [xcor] of one-of cdgs
  let cy0 [ycor] of one-of cdgs
  let rayon (portee-patch est-main) + 2
  let cands prises with [
    (not member? self occupees)
    and (ycor > ym - 1.5)
    and (distancexy cx0 cy0 <= rayon)
    and (geometrie-ok? m self)
    and (charge-estimee m self <= 0.95 * capacite-sur m self)   ;; "peut tenir ?"
  ]
  if any? cands [
    let cible max-one-of cands [ score-prise ]
    verrouiller m cible
    ask m [ set energie min (list 100 (energie + 2)) ]
    set mouvement-ok? true
  ]
end

to-report score-prise
  ;; contexte : une prise. Le sommet est prioritaire, sinon on monte
  ;; en preferant les bonnes prises.
  if sommet? [ report 1000 ]
  report ycor + 0.5 * qualite
end

;; Contraintes geometriques : tous les membres verrouilles (y compris m)
;; doivent rester a portee du nouveau CdG.
to-report geometrie-ok? [ m h ]
  let hx [xcor] of h
  let hy [ycor] of h
  let est-main [main?] of m
  let c cdg-si m h
  let cx item 0 c
  let cy item 1 c
  if sqrt ((hx - cx) ^ 2 + (hy - cy) ^ 2) > portee-patch est-main [ report false ]
  ;; variante : les pieds ne passent pas au-dessus du CdG
  if (not est-main) and pieds-sous-cdg? and hy > cy + 0.5 [ report false ]
  let rp-m portee-patch true
  let rp-p portee-patch false
  report not any? membres with [
    verrouille? and self != m
    and ((distancexy cx cy) > (ifelse-value main? [ rp-m ] [ rp-p ]))
  ]
end

;; Un membre se verrouille : il signale aux autres.
to verrouiller [ m h ]
  ask m [
    move-to h
    set prise-actuelle h
    set au-sol? false
    if not verrouille? [
      set verrouille? true
      ask other membres [ set nb-signaux nb-signaux + 1 ]
    ]
  ]
end

;; contexte : un membre. Il lache : les autres sont prevenus.
to lacher
  if verrouille? [
    set verrouille? false
    set au-sol? false
    set prise-actuelle nobody
    ask other membres [ set nb-signaux nb-signaux - 1 ]
    set nb-lachers nb-lachers + 1
  ]
end

;; Repartition du poids du grimpeur entre les membres verrouilles.
to calculer-charges
  let lies membres with [ verrouille? ]
  ifelse any? lies [
    let total sum [ poids-rel main? dv-local ] of lies
    ask membres [
      ifelse verrouille?
        [ set charge poids * (poids-rel main? dv-local) / total ]
        [ set charge 0 ]
    ]
  ] [
    ask membres [ set charge 0 ]
  ]
end

;; Le membre le plus surcharge lache si la prise ne tient plus.
to verifier-prises
  let tenants membres with [ verrouille? and not au-sol? ]
  if any? tenants [
    let pire max-one-of tenants [ ratio-charge ]
    if [ratio-charge] of pire > 1 [ ask pire [ lacher ] ]
  ]
end

;; Fatigue : un membre verrouille s'epuise d'autant plus vite que sa charge
;; est proche de sa capacite et que son endurance est faible.
;; Un membre libre ou au sol recupere.
to fatigue-membres
  ask membres [
    ifelse (verrouille? and not au-sol?) [
      set energie max (list 0 (energie - cout-fatigue * ratio-charge * (6 - endurance) / 2))
    ] [
      set energie min (list 100 (energie + 1.5 * endurance / 3))
    ]
  ]
end

;; Le CdG tire vers lui les membres non verrouilles.
to tirer-membres-libres
  let c one-of cdgs
  ask membres with [ not verrouille? ] [
    let d distance c
    if d > 0.5 [
      face c
      fd 0.35 * d
    ]
  ]
end

to-report au-sommet?
  ;; contexte : un membre
  if prise-actuelle = nobody [ report false ]
  report [sommet?] of prise-actuelle
end

to chute
  set nb-chutes nb-chutes + 1
  placer-au-depart
end

;; ------------------------------------------------------------
;;  INDICATEURS
;; ------------------------------------------------------------

to mettre-a-jour-indicateurs
  set fatigue-globale precision (100 - mean [energie] of membres) 1
  let mains membres with [ main? and verrouille? ]
  if any? mains [
    let h-m (max [ycor] of mains) * cm-par-patch / 100
    set hauteur-max-m max (list hauteur-max-m (precision h-m 2))
  ]
end

to-report reussite-num
  report ifelse-value reussite? [ 1 ] [ 0 ]
end
@#$#@#$#@
GRAPHICS-WINDOW
360
10
600
610
-1
-1
6.0
1
10
1
1
1
0
1
1
1
0
39
0
99
0
0
1
ticks
30.0

BUTTON
10
10
90
43
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
95
10
175
43
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
10
48
90
81
1 pas
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
95
48
175
81
nouvelle-voie
nouvelle-voie
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
10
88
175
133
mode-voie
mode-voie
"aleatoire" "existante"
0

CHOOSER
10
138
175
183
type-voie
type-voie
"bloc" "voie" "falaise"
0

CHOOSER
10
188
175
233
cotation
cotation
"2" "3" "4" "5a" "5b" "5c" "6a" "6b" "6c" "7a" "7b" "7c" "8a" "8b" "8c" "9a"
6

SLIDER
10
238
175
271
hauteur-voie
hauteur-voie
1
40
5.0
0.5
1
m
HORIZONTAL

SLIDER
10
274
175
307
cm-par-patch
cm-par-patch
10
50
20.0
10
1
cm
HORIZONTAL

CHOOSER
10
312
175
357
profil-mur
profil-mur
"vertical" "devers-progressif" "toit" "aleatoire"
1

SLIDER
10
362
175
395
devers-max
devers-max
0
1
0.5
0.05
1
NIL
HORIZONTAL

INPUTBOX
10
400
175
460
fichier-voie
voie-exemple.csv
1
0
String

SLIDER
185
10
350
43
poids
poids
40
120
70.0
1
1
kg
HORIZONTAL

SLIDER
185
46
350
79
dist-cdg-mains
dist-cdg-mains
40
120
70.0
5
1
cm
HORIZONTAL

SLIDER
185
82
350
115
dist-cdg-pieds
dist-cdg-pieds
40
140
80.0
5
1
cm
HORIZONTAL

SLIDER
185
118
350
151
force-mains
force-mains
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
185
154
350
187
force-pieds
force-pieds
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
185
190
350
223
endurance-mains
endurance-mains
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
185
226
350
259
endurance-pieds
endurance-pieds
1
5
3.0
1
1
NIL
HORIZONTAL

SLIDER
185
262
350
295
gravite
gravite
0
1
0.3
0.05
1
NIL
HORIZONTAL

SLIDER
185
298
350
331
nb-essais-max
nb-essais-max
1
10
1.0
1
1
NIL
HORIZONTAL

SLIDER
185
334
350
367
duree-max
duree-max
100
2000
600.0
50
1
s
HORIZONTAL

SWITCH
185
372
350
405
pieds-sous-cdg?
pieds-sous-cdg?
0
1
-1000

MONITOR
610
10
705
55
prises
count prises
0
1
11

MONITOR
710
10
805
55
hauteur voie (m)
hauteur-reelle-m
1
1
11

MONITOR
610
60
705
105
hauteur max (m)
hauteur-max-m
2
1
11

MONITOR
710
60
805
105
fatigue (%)
fatigue-globale
1
1
11

MONITOR
610
110
705
155
chutes
nb-chutes
0
1
11

MONITOR
710
110
805
155
mouvements
nb-mouvements
0
1
11

MONITOR
610
160
705
205
reussite?
reussite?
0
1
11

MONITOR
710
160
805
205
prises lachees
nb-lachers
0
1
11

PLOT
610
215
810
365
Hauteur atteinte
temps (s)
hauteur (m)
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"hauteur" 1.0 0 -13345367 true "" "plot hauteur-max-m"

PLOT
610
370
810
520
Fatigue globale
temps (s)
fatigue (%)
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"fatigue" 1.0 0 -2674135 true "" "plot fatigue-globale"

@#$#@#$#@
## QUELLE QUESTION ?

**Quels parametres influencent le plus la reussite d'un grimpeur : la genetique ou la technique ?**

Dans ce modele, on oppose :

- la **morphologie (genetique)** : `poids`, `dist-cdg-mains`, `dist-cdg-pieds` (allonge) ;
- l'**entrainement (technique)** : `force-mains`, `force-pieds`, `endurance-mains`, `endurance-pieds` (niveaux de 1 a 5).

Indicateurs de sortie : **reussite** (atteindre la prise sommet), **hauteur max atteinte**, **fatigue globale**, nombre de chutes.

## AGENTS

**Prises** : type (jug, pinch, crimp, sloper, mono, jib), qualite d'accroche (1 = lisse, 5 = rugueux, represente par la taille), booleen `sommet?` (etoile jaune), devers du support (patch sous la prise).

**Grimpeur (5 agents)** : 2 mains (bleues), 2 pieds (vertes), 1 centre de gravite (rond rouge, = parametre `poids`). Les 4 membres sont relies au CdG par un lien et ne peuvent pas depasser `dist-cdg-mains` / `dist-cdg-pieds`.

**Environnement** : le mur (patches), plus ou moins desaxe. Plus la case est sombre, plus le devers est fort. La ligne verte en bas est le sol.

## COMPORTEMENTS

1. **Signal de verrouillage** : un membre qui se verrouille sur une prise (ou le sol) previent les 3 autres. Un membre ne peut bouger que si au moins 2 autres ont signale qu'ils sont verrouilles.
2. **Centre de gravite** : place sous les mains et au-dessus des pieds, il est tire vers le bas par la gravite (davantage en devers, reglage `gravite`). Il tire aussi vers lui les membres libres.
3. **Repartition du poids** : le poids est reparti entre les membres verrouilles. En vertical les pieds portent plus ; en devers ce sont les mains.
4. **Tient / ne tient pas** : capacite = force du membre x qualite de la prise x coefficient du type x fraicheur (endurance restante). Si la charge depasse la capacite, le membre lache.
5. **Fatigue** : un membre verrouille s'epuise d'autant plus vite que sa charge est proche de sa capacite et que son endurance est faible.
6. **Deplacement** : a chaque tick, un membre (alternance mains / pieds) vise la meilleure prise accessible, de preference le sommet.
7. **Chute** : si moins de 2 membres restent verrouilles (ou si le grimpeur est bloque trop longtemps), il tombe et recommence en bas, dans la limite de `nb-essais-max`.

## ECHELLES

1 patch = `cm-par-patch` cm (10 a 50, a regler **avant** `setup`). 1 tick = 1 seconde.

## PARAMETRES

- **Mode** : `mode-voie` = aleatoire ou existante.
- **Aleatoire** : `cotation` (2 a 9a, notation FFME), `type-voie` (bloc ≤ 5 m, voie, falaise), `hauteur-voie` (m). Plus la cotation est haute, plus les prises sont espacees, de mauvaise qualite et de types durs.
- **Existante** : `fichier-voie`, un CSV place dans le meme dossier que le modele. Colonnes : `x_cm,y_cm,type,qualite,sommet` (sommet = 1 pour la prise sommet). Un exemple est fourni (`voie-exemple.csv`, voie fictive).
- **Variante** : `pieds-sous-cdg?` interdit aux pieds de passer au-dessus du centre de gravite.

## PLAN D'EXPERIENCES

Deux experiences BehaviorSpace sont preparees (Outils > BehaviorSpace) :

- `genetique-morphologie` : fait varier poids et allonge, force/endurance fixes ;
- `technique-entrainement` : fait varier force et endurance, morphologie fixe.

Chaque combinaison est repetee 20 fois sur des voies aleatoires differentes. On compare ensuite le taux de reussite entre les deux familles de parametres.

## HYPOTHESES ET LIMITES

- Pas de prise en compte de l'orientation des prises.
- Le grimpeur est decrit comme 5 elements independants.
- Les coefficients (type de prise, ponderation des mains et des pieds, cout de fatigue) sont des hypotheses a calibrer sur des donnees reelles.
- Modele 2D : le devers modifie la repartition du poids et la position du CdG, mais le mur reste un plan.
- Le sommet des voies aleatoires est un bon jug (qualite 5).
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
false
0
Circle -7500403 true true 0 0 300

square
false
0
Rectangle -7500403 true true 30 30 270 270

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="genetique-morphologie" repetitions="20" sequentialRunOrder="true" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>reussite-num</metric>
    <metric>hauteur-max-m</metric>
    <metric>fatigue-globale</metric>
    <metric>nb-chutes</metric>
    <enumeratedValueSet variable="mode-voie">
      <value value="&quot;aleatoire&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poids">
      <value value="55"/>
      <value value="70"/>
      <value value="85"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dist-cdg-mains">
      <value value="60"/>
      <value value="70"/>
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="force-mains">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="force-pieds">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endurance-mains">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endurance-pieds">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="technique-entrainement" repetitions="20" sequentialRunOrder="true" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>reussite-num</metric>
    <metric>hauteur-max-m</metric>
    <metric>fatigue-globale</metric>
    <metric>nb-chutes</metric>
    <enumeratedValueSet variable="mode-voie">
      <value value="&quot;aleatoire&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="poids">
      <value value="70"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dist-cdg-mains">
      <value value="70"/>
    </enumeratedValueSet>
    <steppedValueSet variable="force-mains" first="1" step="1" last="5"/>
    <steppedValueSet variable="endurance-mains" first="1" step="1" last="5"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
