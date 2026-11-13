;; ============================================================
;;  ESCALADE : ouverture de voie et grimpeur
;;  - Les PRISES sont des agents qui forment un chemin du bas
;;    vers le haut du mur (distance plus ou moins proche).
;;  - Les PATCHES simulent le mur et son DEVERS.
;;  - Le GRIMPEUR = 4 agents (2 mains, 2 pieds) lies a un
;;    CENTRE DE GRAVITE : ils ne peuvent pas s'en eloigner.
;; ============================================================

globals [
  y-sommet          ;; hauteur a atteindre pour reussir la voie
  nb-chutes
  nb-reussites
  nb-mouvements
  index-membre      ;; position dans l'ordre de deplacement des membres
  mouvement-ok?     ;; le dernier essai de mouvement a-t-il reussi ?
  ordre-membres     ;; ordre d'alternance mains / pieds
]

breed [ prises prise ]
breed [ membres membre ]
breed [ centres centre ]
undirected-link-breed [ parcours un-parcours ]   ;; trait entre prises successives
undirected-link-breed [ attaches attache ]       ;; lien membre <-> centre de gravite

patches-own [ devers ]                  ;; 0 = vertical, 1 = tres desaxe
prises-own  [ rang ]                    ;; numero de la prise dans la voie
membres-own [ type-membre main? prise-actuelle ]
centres-own [ energie ]

;; ------------------------------------------------------------
;;  INITIALISATION
;; ------------------------------------------------------------

to setup
  clear-all
  set nb-chutes 0
  set nb-reussites 0
  set nb-mouvements 0
  set index-membre 0
  set y-sommet max-pycor - 3
  set ordre-membres [ "main-d" "pied-g" "main-g" "pied-d" ]
  creer-mur
  creer-voie
  creer-grimpeur
  reset-ticks
end

;; garde le meme mur, genere une nouvelle voie et un nouveau grimpeur
to nouvelle-voie
  ask prises [ die ]
  ask membres [ die ]
  ask centres [ die ]
  set nb-mouvements 0
  set index-membre 0
  creer-voie
  creer-grimpeur
  reset-ticks
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
end

to-report calcul-devers
  let h pycor / max-pycor
  if profil-mur = "vertical" [ report 0 ]
  if profil-mur = "devers-progressif" [ report devers-max * h ]
  if profil-mur = "toit" [
    report ifelse-value (h > 0.4 and h < 0.6) [ devers-max ] [ 0 ]
  ]
  report 0
end

;; ------------------------------------------------------------
;;  OUVERTURE DE LA VOIE (prises)
;; ------------------------------------------------------------

to creer-voie
  let x0 (min-pxcor + 6) + random (world-width - 12)
  let precedente nobody
  create-prises 1 [
    setxy x0 1
    initialiser-prise 0
    set precedente self
  ]
  let n 1
  let continuer? true
  ;; on ajoute des prises jusqu'a atteindre le sommet :
  ;; le nombre de prises n'est donc pas fixe a l'avance
  while [ continuer? and [ycor] of precedente < y-sommet ] [
    let pos [ position-suivante ] of precedente
    ifelse empty? pos [
      set continuer? false
    ] [
      let nouvelle nobody
      create-prises 1 [
        setxy item 0 pos item 1 pos
        initialiser-prise n
        set nouvelle self
        ;; create-parcours-with attend un set mais recoit un agent
        let precedente-groupe (turtle-set precedente)
        create-parcours-with precedente-groupe [ set color gray - 2 ]
      ]
      set precedente nouvelle
      set n n + 1
    ]
  ]
end

to initialiser-prise [ numero ]
  set rang numero
  set shape "circle"
  set size 1.6
  set color yellow
end

;; contexte : une prise. Renvoie [x y] de la prise suivante, ou [] si echec.
;; La prise suivante est placee entre dist-min et dist-max de celle-ci,
;; dans un cone oriente vers le haut.
to-report position-suivante
  let essais 0
  while [ essais < 200 ] [
    let d dist-min + random-float (max (list 0 (dist-max - dist-min)))
    let a (random-float (2 * angle-max)) - angle-max
    let nx xcor + d * sin a
    let ny ycor + d * cos a
    if nx > min-pxcor + 2 and nx < max-pxcor - 2 and ny < max-pycor - 0.5 [
      if not any? prises with [ distancexy nx ny < 1.2 ] [
        report (list nx ny)
      ]
    ]
    set essais essais + 1
  ]
  report []
end

;; ------------------------------------------------------------
;;  LE GRIMPEUR (4 membres + centre de gravite)
;; ------------------------------------------------------------

to creer-grimpeur
  let x0 [xcor] of one-of prises with [ rang = 0 ]
  let types [ "main-g" "main-d" "pied-g" "pied-d" ]
  let decalages [ -1 1 -1 1 ]
  (foreach types decalages [ [ t d ] ->
    create-membres 1 [
      set type-membre t
      set main? (substring t 0 4 = "main")
      set prise-actuelle nobody
      setxy (x0 + d) 0
      set shape "square"
      set size 1.2
      set color ifelse-value main? [ blue ] [ green ]
    ]
  ])
  create-centres 1 [
    set energie 100
    set shape "circle"
    set size 1.5
    set color red
    let c centre-actuel
    setxy item 0 c item 1 c
    ;; les membres sont attaches au centre et ne peuvent pas s'en eloigner
    ask membres [ create-attache-with myself [ set color white ] ]
  ]
end

;; Centre de gravite = moyenne des 4 membres, abaisse par le devers
;; (plus le mur est desaxe, plus le corps "pend" loin de la paroi).
to-report calcule-centre [ xs ys ]
  let moy-x mean xs
  let moy-y mean ys
  let dv [devers] of patch (round moy-x) (round moy-y)
  let cx max (list min-pxcor (min (list max-pxcor moy-x)))
  let cy max (list min-pycor (min (list max-pycor (moy-y - gravite * dv))))
  report (list cx cy)
end

to-report centre-actuel
  report calcule-centre [xcor] of membres [ycor] of membres
end

;; centre de gravite si le membre m etait deplace en (px, py)
to-report centre-si [ m px py ]
  let autres membres with [ self != m ]
  report calcule-centre (fput px [xcor] of autres) (fput py [ycor] of autres)
end

to-report portee-de [ t ]
  report ifelse-value [main?] of t [ portee-mains ] [ portee-pieds ]
end

;; Un deplacement est valide si, apres deplacement, TOUS les membres
;; restent a portee du centre de gravite.
to-report valide? [ m px py ]
  let c centre-si m px py
  let cx item 0 c
  let cy item 1 c
  if sqrt ((px - cx) ^ 2 + (py - cy) ^ 2) > portee-de m [ report false ]
  report not any? membres with [
    self != m and distancexy cx cy > portee-de self
  ]
end

;; ------------------------------------------------------------
;;  BOUCLE PRINCIPALE
;; ------------------------------------------------------------

to go
  if not any? centres [ stop ]

  ;; on essaie de bouger un membre (alternance mains / pieds)
  let fait? false
  let essai 0
  while [ not fait? and essai < 4 ] [
    let t item ((index-membre + essai) mod 4) ordre-membres
    deplacer-membre one-of membres with [ type-membre = t ]
    set fait? mouvement-ok?
    set essai essai + 1
  ]
  ifelse fait? [
    set index-membre (index-membre + essai) mod 4
    set nb-mouvements nb-mouvements + 1
  ] [
    ;; bloque : on perd de l'energie en cherchant la suite
    ask centres [ set energie energie - 2 ]
  ]

  mettre-a-jour-centre

  ;; fatigue : plus on est en devers, plus on se fatigue
  ask centres [
    set energie energie - fatigue * (1 + 3 * [devers] of patch-here)
  ]

  ;; voie reussie ?
  if any? membres with [ main? and ycor >= y-sommet ] [
    set nb-reussites nb-reussites + 1
    tick
    stop
  ]

  ;; chute ?
  if [energie] of one-of centres <= 0 [ chute ]

  tick
end

;; Deplace le membre m vers la meilleure prise accessible.
to deplacer-membre [ m ]
  set mouvement-ok? false
  let occupees [prise-actuelle] of membres
  let hc item 1 centre-actuel
  let est-main [main?] of m
  let cands prises with [
    (not member? self occupees)
    and (est-main or ycor <= hc + 1)      ;; les pieds restent sous le centre
    and (valide? m xcor ycor)
  ]
  if any? cands [
    ;; on prefere la prise la plus haute, sans trop se deplacer
    let cible max-one-of cands [ ycor - 0.2 * distance m ]
    ask m [
      move-to cible
      set prise-actuelle cible
    ]
    set mouvement-ok? true
  ]
end

to mettre-a-jour-centre
  let c centre-actuel
  ask centres [ setxy item 0 c item 1 c ]
end

to chute
  set nb-chutes nb-chutes + 1
  let x0 [xcor] of one-of prises with [ rang = 0 ]
  ask membres [
    set prise-actuelle nobody
    setxy (x0 + ifelse-value (last type-membre = "g") [ -1 ] [ 1 ]) 0
  ]
  ask centres [ set energie 100 ]
  mettre-a-jour-centre
end

;; ------------------------------------------------------------
;;  MONITEURS
;; ------------------------------------------------------------

to-report energie-actuelle
  report ifelse-value any? centres [ precision [energie] of one-of centres 0 ] [ 0 ]
end

to-report hauteur-max
  report ifelse-value any? membres [ precision max [ycor] of membres 1 ] [ 0 ]
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
458
458
-1
-1
8.0
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
30
0
55
0
0
1
ticks
30.0

BUTTON
10
10
100
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
105
10
195
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
195
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

SLIDER
10
90
195
123
dist-min
dist-min
1
10
2.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
126
195
159
dist-max
dist-max
1
12
5.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
162
195
195
angle-max
angle-max
10
90
70.0
5
1
NIL
HORIZONTAL

SLIDER
10
198
195
231
portee-mains
portee-mains
2
12
6.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
234
195
267
portee-pieds
portee-pieds
2
12
5.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
270
195
303
gravite
gravite
0
5
2.0
0.5
1
NIL
HORIZONTAL

SLIDER
10
306
195
339
devers-max
devers-max
0
1
0.6
0.05
1
NIL
HORIZONTAL

SLIDER
10
342
195
375
fatigue
fatigue
0
3
0.5
0.1
1
NIL
HORIZONTAL

CHOOSER
10
380
195
425
profil-mur
profil-mur
"vertical" "devers-progressif" "toit" "aleatoire"
1

MONITOR
470
10
570
55
prises
count prises
0
1
11

MONITOR
470
60
570
105
mouvements
nb-mouvements
0
1
11

MONITOR
470
110
570
155
chutes
nb-chutes
0
1
11

MONITOR
470
160
570
205
reussites
nb-reussites
0
1
11

MONITOR
470
210
570
255
energie
energie-actuelle
0
1
11

MONITOR
470
260
570
305
hauteur max
hauteur-max
1
1
11

@#$#@#$#@
## QUE FAIT CE MODELE ?

Ce modele simule l'**ouverture d'une voie d'escalade** puis son **ascension** par un grimpeur.

- **Les prises** (ronds jaunes) sont des agents. Un "ouvreur" les place du bas vers le haut du mur : chaque prise est placee entre `dist-min` et `dist-max` de la precedente, dans un cone dirige vers le haut (`angle-max`). On s'arrete quand on atteint le sommet : le **nombre de prises n'est pas fixe**, il resulte des parametres.
- **Le mur** est represente par les *patches*. Chaque patch a une valeur de **devers** (0 = mur vertical, 1 = tres desaxe). Plus la case est sombre, plus le devers est fort.
- **Le grimpeur** est un groupe de **4 agents** (2 mains bleues, 2 pieds verts) relies par des liens a un **centre de gravite** (rond rouge). Aucun membre ne peut s'eloigner du centre au-dela de sa portee.

## HYPOTHESES DU MODELE

1. Le mur est un plan 2D ; le devers est une propriete locale du mur (patch).
2. Le centre de gravite est la moyenne des 4 membres, **abaissee proportionnellement au devers et a la gravite** : en devers, le corps "pend".
3. Chaque membre doit rester a une distance maximale du centre (`portee-mains`, `portee-pieds`).
4. Le grimpeur ne bouge qu'un membre a la fois, en alternant mains et pieds.
5. Une main choisit la prise accessible la plus haute ; un pied ne peut pas depasser le centre de gravite de plus d'une unite.
6. L'energie baisse a chaque tour, d'autant plus vite que le devers est fort (`fatigue`). A 0, le grimpeur chute et recommence en bas.
7. La voie est reussie quand une main atteint le sommet.

## COMMENT L'UTILISER

1. Choisir un `profil-mur` (vertical, devers-progressif, toit, aleatoire).
2. Cliquer sur **setup** puis **go**.
3. **nouvelle-voie** genere une autre voie sur le meme mur.

## A EXPERIMENTER

- Augmenter `dist-max` : la voie devient plus "ecartee", donc plus dure (voire impossible).
- Augmenter `devers-max` ou `gravite` : les chutes augmentent.
- Comparer le profil "toit" et le profil "vertical" pour une meme voie.
- Chercher a partir de quelle valeur de `dist-max` le grimpeur ne reussit plus jamais.
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

@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
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
