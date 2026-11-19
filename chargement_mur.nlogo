extensions [gis]

breed [prises prise]

globals [
  mur              ;; jeu de donnees GIS (data/mur.shp)
  noms-champs      ;; noms des champs de la table du shapefile
  formes-prises    ;; types de prises ayant une forme dediee
  cm-par-patch     ;; resolution : 1 patch = cm-par-patch cm de mur
  rayon-fond-cm    ;; rayon (cm) du fond noir autour d'une prise en devers
  liaison-max-cm   ;; distance max (cm) pour relier deux prises en devers (meme volume)
  taille-prise     ;; taille de base des prises (en patches)
]

prises-own [ couleur-nom cotation diff type-prise devers panneau voie ]

;; zoom (slider de l'interface) = nombre de pixels d'ecran par cm de mur

to setup
  clear-all
  set cm-par-patch 2
  set rayon-fond-cm 40
  set liaison-max-cm 120
  set taille-prise 14
  set formes-prises ["jug" "grimp" "sloper" "pinch" "mono" "jib"]
  charger-mur
  creer-prises
  colorier-fond
  afficher-diagnostic
  reset-ticks
end

;; change la taille d'affichage sans tout recharger
to appliquer-zoom
  set-patch-size zoom * cm-par-patch
end

;; ---------------------------------------------------------------
;; Charge le shapefile et adapte le monde NetLogo a la taille du mur
;; ---------------------------------------------------------------
to charger-mur
  set mur gis:load-dataset "data/mur.shp"
  set noms-champs gis:property-names mur
  let env gis:envelope-of mur                ;; [xmin xmax ymin ymax] en cm
  let marge 40
  let xmin (item 0 env) - marge
  let xmax (item 1 env) + marge
  let ymin (item 2 env) - marge
  let ymax (item 3 env) + marge
  let nb-x ceiling ((xmax - xmin) / cm-par-patch)
  let nb-y ceiling ((ymax - ymin) / cm-par-patch)
  resize-world 0 (nb-x - 1) 0 (nb-y - 1)
  set-patch-size zoom * cm-par-patch
  gis:set-world-envelope (list xmin (xmin + nb-x * cm-par-patch) ymin (ymin + nb-y * cm-par-patch))
end

;; ---------------------------------------------------------------
;; Cree une tortue par prise (forme = type, couleur = couleur saisie)
;; ---------------------------------------------------------------
to creer-prises
  foreach gis:feature-list-of mur [ f ->
    let sommets gis:vertex-lists-of f
    if not empty? sommets [
      let loc gis:location-of (first (first sommets))
      if not empty? loc [
        create-prises 1 [
          setxy (item 0 loc) (item 1 loc)
          set couleur-nom lire f "couleur" "COULEUR"
          set cotation    lire f "cotation" "COTATION"
          set diff        lire f "diff" "DIFF"
          set devers      lire f "devers" "DEVERS"
          set panneau     lire f "panneau" "PANNEAU"
          set voie        lire f "voie" "VOIE"
          let t0 lire f "type" "TYPE"
          set type-prise minuscules (word t0)
          ifelse member? type-prise formes-prises
            [ set shape type-prise ]
            [ set shape "circle" ]
          set color couleur-netlogo couleur-nom
          set size taille-prise * facteur-taille shape
        ]
      ]
    ]
  ]
end

;; ---------------------------------------------------------------
;; Fond : gris partout (devers 0 par defaut), noir autour des prises
;; en devers, et noir entre deux prises en devers proches (meme volume)
;; ---------------------------------------------------------------
to colorier-fond
  ask patches [ set pcolor gray ]
  let r rayon-fond-cm / cm-par-patch
  let pentes prises with [ en-devers? ]
  ask pentes [
    ask patches in-radius r [ set pcolor black ]
  ]
  ask pentes [
    let a self
    ask other pentes with [ who > [who] of a and panneau = [panneau] of a and distance a < liaison-max-cm / cm-par-patch ] [
      relier-fond ([xcor] of a) ([ycor] of a) xcor ycor r
    ]
  ]
end

;; noircit tous les patches situes a moins de r patches du segment (x1,y1)-(x2,y2)
to relier-fond [x1 y1 x2 y2 r]
  let vx x2 - x1
  let vy y2 - y1
  let l2 vx * vx + vy * vy
  ask patches with [ pxcor >= (min (list x1 x2)) - r and pxcor <= (max (list x1 x2)) + r and
                     pycor >= (min (list y1 y2)) - r and pycor <= (max (list y1 y2)) + r ] [
    let t 0
    if l2 > 0 [ set t max (list 0 (min (list 1 (((pxcor - x1) * vx + (pycor - y1) * vy) / l2)))) ]
    let qx x1 + t * vx
    let qy y1 + t * vy
    if sqrt ((pxcor - qx) ^ 2 + (pycor - qy) ^ 2) <= r [ set pcolor black ]
  ]
end

to-report en-devers?
  report (is-number? devers) and (devers != 0)
end

;; ---------------------------------------------------------------
;; Informations dans le Centre de commandes
;; ---------------------------------------------------------------
to afficher-diagnostic
  print (word count prises " prises chargees")
  print (word "couleurs lues : " remove-duplicates [couleur-nom] of prises)
  print (word "types lus : " remove-duplicates [type-prise] of prises)
  print (word "devers lus : " remove-duplicates [devers] of prises)
end

;; ---------------------------------------------------------------
;; Utilitaires
;; ---------------------------------------------------------------
;; lit un champ de la table (accepte minuscules ou majuscules)
to-report lire [f nom-min nom-maj]
  ifelse member? nom-min noms-champs
    [ report gis:property-value f nom-min ]
    [ ifelse member? nom-maj noms-champs
        [ report gis:property-value f nom-maj ]
        [ report "" ] ]
end

;; texte en minuscules, sans espaces
to-report minuscules [s]
  let haut "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  let bas "abcdefghijklmnopqrstuvwxyz"
  let res ""
  foreach n-values length s [ i -> item i s ] [ c ->
    let p position c haut
    set res (word res (ifelse-value (p = false) [ c ] [ item p bas ]))
  ]
  report remove " " res
end

;; accepte jaune/Jaune/JAUNE, "rouge " etc. (on compare les 4 premieres lettres)
to-report couleur-netlogo [nom]
  let n minuscules (word nom)
  let d substring n 0 (min (list 4 length n))
  if d = "jaun" [ report yellow ]
  if d = "oran" [ report orange ]
  if d = "vert" [ report green ]
  if d = "bleu" [ report blue ]
  if d = "roug" [ report red ]
  if d = "noir" [ report black ]
  if d = "viol" [ report violet ]
  if d = "blan" [ report white ]
  report brown                                ;; couleur inconnue
end

;; les petites prises sont dessinees plus petites, les grosses plus grandes
to-report facteur-taille [forme]
  if forme = "jug"    [ report 1.3 ]
  if forme = "sloper" [ report 1.3 ]
  if forme = "pinch"  [ report 1.0 ]
  if forme = "grimp"  [ report 1.0 ]
  if forme = "mono"   [ report 0.8 ]
  if forme = "jib"    [ report 0.6 ]
  report 1.0
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
647
448
-1
-1
13.0
1
10
1
1
1
0
1
1
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
10
10
110
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

SLIDER
10
55
190
88
zoom
zoom
1
10
3.0
0.5
1
NIL
HORIZONTAL

BUTTON
10
100
190
133
appliquer le zoom
appliquer-zoom
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1
@#$#@#$#@
## EscalAgent - chargement du mur

Clique sur **setup** : le shapefile `data/mur.shp` est charge.

- forme de la prise = type (jug, grimp, sloper, pinch, mono, jib)
- couleur de la prise = couleur saisie dans QGIS (jaune, orange, vert, bleu, rouge, noir, violet, blanc)
- fond gris = devers 0 (par defaut), fond noir = devers different de 0
- deux prises en devers proches et du meme panneau sont reliees par une bande noire (meme volume)
- 1 patch = 2 cm de mur
- slider **zoom** = pixels par cm de mur ; "appliquer le zoom" change la taille sans recharger

Ce fichier doit se trouver a la racine du projet (a cote du dossier `data`).
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

circle
false
0
Circle -7500403 true true 0 0 300

jug
false
0
Polygon -1 true false 300 158 286 209 263 260 205 278 150 298 93 282 36 261 16 208 0 158 12 106 39 57 94 35 150 14 207 34 259 59 284 108
Polygon -7500403 true true 288 158 271 203 250 249 199 265 150 283 99 269 48 250 30 203 12 158 26 112 50 68 100 48 150 29 201 47 248 70 270 113
Polygon -7500403 true false 232 118 224 135 201 149 168 157 132 157 99 149 76 135 68 118 76 101 99 87 132 79 168 79 201 87 224 101
Polygon -16777216 true false 201 118 196 129 182 137 161 142 139 142 118 137 104 129 99 118 104 107 118 99 139 94 161 94 182 99 196 107

grimp
false
0
Polygon -1 true false 0 122 30 93 270 93 300 122 300 178 270 207 30 207 0 178
Polygon -7500403 true true 15 125 45 100 255 100 285 125 285 175 255 200 45 200 15 175
Line -7500403 true 45 122 255 122
Line -7500403 true 60 175 240 175

sloper
false
0
Polygon -1 true false 300 255 299 205 283 159 259 119 227 88 190 69 150 63 110 69 73 88 41 119 17 159 1 205 0 255
Polygon -7500403 true true 290 245 285 200 271 158 249 121 220 93 186 76 150 70 114 76 80 93 51 121 29 158 15 200 10 245
Line -7500403 true 245 190 229 159
Line -7500403 true 229 159 206 135
Line -7500403 true 206 135 179 120
Line -7500403 true 179 120 150 115
Line -7500403 true 150 115 121 120
Line -7500403 true 121 120 94 135
Line -7500403 true 94 135 71 159
Line -7500403 true 71 159 55 190
Line -7500403 true 206 205 196 188
Line -7500403 true 196 188 182 176
Line -7500403 true 182 176 167 168
Line -7500403 true 167 168 150 165
Line -7500403 true 150 165 133 168
Line -7500403 true 133 168 118 176
Line -7500403 true 118 176 104 188
Line -7500403 true 104 188 94 205

pinch
false
0
Polygon -1 true false 111 0 189 0 223 32 223 268 189 300 111 300 77 268 77 32
Polygon -7500403 true true 115 15 185 15 215 45 215 255 185 285 115 285 85 255 85 45
Line -7500403 true 125 55 125 245
Line -7500403 true 175 55 175 245
Line -7500403 true 100 50 200 50

mono
false
0
Polygon -1 true false 32 77 77 32 223 32 268 77 268 223 223 268 77 268 32 223
Polygon -7500403 true true 45 85 85 45 215 45 255 85 255 215 215 255 85 255 45 215
Circle -7500403 true false 108 108 84
Circle -16777216 true false 118 118 64

jib
false
0
Polygon -1 true false 150 41 213 145 236 220 208 258 92 258 64 220 87 145
Polygon -7500403 true true 150 60 205 150 225 215 200 248 100 248 75 215 95 150
Circle -7500403 true false 130 170 40
Circle -16777216 true false 144 184 12
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

