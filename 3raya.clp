;;;;;;; JUGADOR DE 3 en RAYA ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Version de 3 en raya clásico: fichas que se pueden poner libremente en cualquier posicion libre (i,j) con 0 < i,j < 4
;;;;;;;;;;;;;;;;;;;;;;; y cuando se han puesto las 3 fichas las jugadas consisten en desplazar una ficha propia
;;;;;;;;;;;;;;;;;;;;;;; de la posición en que se encuentra (i,j) a una contigua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; Hechos para representar un estado del juego

;;;;;;; (Turno X|O)   representa a quien corresponde el turno (X maquina, O jugador)
;;;;;;; (Posicion ?i ?j " "|X|O) representa que la posicion i,j del tablero esta vacia, o tiene una ficha de Clisp o tiene una ficha del contrincante

;;;;;;;;;;;;;;;; Hechos para representar una jugadas

;;;;;;; (Juega X|O ?origen_i ?origen_j ?destino_i ?destino_j) representa que la jugada consiste en desplazar la ficha de la posicion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (?origen_i,?origen_j) a la posición (?destino_i,?destino_j)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; las fichas que se ponen inicialmente se supondrá que están en el posición (0,0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INICIALIZAR ESTADO

(deffacts Tablero
  (Conectado 1 a horizontal 1 b)
  (Conectado 1 b horizontal 1 c)
  (Conectado 2 a horizontal 2 b)
  (Conectado 2 b horizontal 2 c)
  (Conectado 3 a horizontal 3 b)
  (Conectado 3 b horizontal 3 c)
  (Conectado 1 a vertical 2 a)
  (Conectado 2 a vertical 3 a)
  (Conectado 1 b vertical 2 b)
  (Conectado 2 b vertical 3 b)
  (Conectado 1 c vertical 2 c)
  (Conectado 2 c vertical 3 c)
  (Conectado 1 a diagonal 2 b)
  (Conectado 2 b diagonal 3 c)
  (Conectado 1 c diagonal_inversa 2 b)
  (Conectado 2 b diagonal_inversa 3 a)
)

(deffacts Estado_inicial
  (Posicion 1 a " ")
  (Posicion 1 b " ")
  (Posicion 1 c " ")
  (Posicion 2 a " ")
  (Posicion 2 b " ")
  (Posicion 2 c " ")
  (Posicion 3 a " ")
  (Posicion 3 b " ")
  (Posicion 3 c " ")
  (Fichas_sin_colocar O 3)
  (Fichas_sin_colocar X 3)
)

(defrule Conectado_es_simetrica
  (declare (salience 1))
  (Conectado ?i ?j ?forma ?i1 ?j1)
=>
  (assert (Conectado ?i1 ?j1 ?forma ?i ?j))
)

(defrule Elige_quien_comienza
=>
  (printout t "Quien quieres que empieze: (escribre X para la maquina u O para empezar tu) ")
  (assert (Turno (read)))
)

;;;;;;;;;;;;;;;;;;;;;;; RECOGER JUGADA DEL CONTRARIO ;;;;;;;;;;;;;;;;;;;;;;;
(defrule muestra_posicion
  (declare (salience 1))
  (muestra_posicion)
  (Posicion 1 a ?p11)
  (Posicion 1 b ?p12)
  (Posicion 1 c ?p13)
  (Posicion 2 a ?p21)
  (Posicion 2 b ?p22)
  (Posicion 2 c ?p23)
  (Posicion 3 a ?p31)
  (Posicion 3 b ?p32)
  (Posicion 3 c ?p33)
=>
  (printout t crlf)
  (printout t "   a      b      c" crlf)
  (printout t "   -      -      -" crlf)
  (printout t "1 |" ?p11 "| -- |" ?p12 "| -- |" ?p13 "|" crlf)
  (printout t "   -      -      -" crlf)
  (printout t "   |  \\   |   /  |" crlf)
  (printout t "   -      -      -" crlf)
  (printout t "2 |" ?p21 "| -- |" ?p22 "| -- |" ?p23 "|" crlf)
  (printout t "   -      -      -" crlf)
  (printout t "   |   /  |  \\   |" crlf)
  (printout t "   -      -      -" crlf)
  (printout t "3 |" ?p31 "| -- |" ?p32 "| -- |" ?p33 "|"crlf)
  (printout t "   -      -      -" crlf)
)

(defrule muestra_posicion_turno_jugador
  (declare (salience 10))
  (Turno O)
=>
  (assert (muestra_posicion))
)

(defrule jugada_contrario_fichas_sin_colocar
  ?f <- (Turno O)
  (Fichas_sin_colocar O ?n)
=>
  (printout t "en que posicion colocas la siguiente ficha" crlf)
  (printout t "escribe la fila (1,2 o 3): ")
  (bind ?fila (read))
  (printout t "escribe la columna (a,b o c): ")
  (bind ?columna (read))
  (assert (Juega O 0 0 ?fila ?columna))
  (retract ?f)
)

(defrule juega_contrario_fichas_sin_colocar_check
  (declare (salience 1))
  ?f <- (Juega O 0 0 ?i ?j)
  (not (Posicion ?i ?j " "))
=>
  (printout t "No puedes jugar en " ?i ?j " porque no esta vacio" crlf)
  (retract ?f)
  (assert (Turno O))
)

(defrule juega_contrario_fichas_sin_colocar_actualiza_estado
  ?f <- (Juega O 0 0 ?i ?j)
  ?g <- (Posicion ?i ?j " ")
=>
  (retract ?f ?g)
  (assert (Turno X) (Posicion ?i ?j O) (reducir_fichas_sin_colocar O))
)

(defrule reducir_fichas_sin_colocar
  (declare (salience 1))
  ?f <- (reducir_fichas_sin_colocar ?jugador)
  ?g <- (Fichas_sin_colocar ?jugador ?n)
=>
  (retract ?f ?g)
  (assert (Fichas_sin_colocar ?jugador (- ?n 1)))
)

(defrule todas_las_fichas_en_tablero
  (declare (salience 1))
  ?f <- (Fichas_sin_colocar ?jugador 0)
=>
  (retract ?f)
  (assert (Todas_fichas_en_tablero ?jugador))
)

(defrule juega_contrario
  ?f <- (Turno O)
  (Todas_fichas_en_tablero O)
=>
  (printout t "en que posicion esta la ficha que quieres mover?" crlf)
  (printout t "escribe la fila (1,2,o 3): ")
  (bind ?origen_i (read))
  (printout t "escribe la columna (a,b o c): ")
  (bind ?origen_j (read))
  (printout t "a que posicion la quieres mover?" crlf)
  (printout t "escribe la fila (1,2,o 3): ")
  (bind ?destino_i (read))
  (printout t "escribe la columna (a,b o c): ")
  (bind ?destino_j (read))
  (assert (Juega O ?origen_i ?origen_j ?destino_i ?destino_j))
  (printout t "Juegas mover la ficha de "  ?origen_i ?origen_j " a " ?destino_i ?destino_j crlf)
  (retract ?f)
)

(defrule juega_contrario_check_mueve_ficha_propia
  (declare (salience 1))
  ?f <- (Juega O ?origen_i ?origen_j ?destino_i ?destino_j)
  (Posicion ?origen_i ?origen_j ?X)
  (test (neq O ?X))
=>
  (printout t "No es jugada valida porque en " ?origen_i ?origen_j " no hay una ficha tuya" crlf)
  (retract ?f)
  (assert (Turno O))
)

(defrule juega_contrario_check_mueve_a_posicion_libre
  (declare (salience 1))
  ?f <- (Juega O ?origen_i ?origen_j ?destino_i ?destino_j)
  (Posicion ?destino_i ?destino_j ?X)
  (test (neq " " ?X))
=>
  (printout t "No es jugada valida porque " ?destino_i ?destino_j " no esta libre" crlf)
  (retract ?f)
  (assert (Turno O))
)

(defrule juega_contrario_check_conectado
  (declare (salience 1))
  (Todas_fichas_en_tablero O)
  ?f <- (Juega O ?origen_i ?origen_j ?destino_i ?destino_j)
  (not (Conectado ?origen_i ?origen_j ? ?destino_i ?destino_j))
=>
  (printout t "No es jugada valida porque "  ?origen_i ?origen_j " no esta conectado con " ?destino_i ?destino_j crlf)
  (retract ?f)
  (assert (Turno O))
)

(defrule juega_contrario_actualiza_estado
  ?f <- (Juega O ?origen_i ?origen_j ?destino_i ?destino_j)
  ?h <- (Posicion ?origen_i ?origen_j O)
  ?g <- (Posicion ?destino_i ?destino_j " ")
=>
  (retract ?f ?g ?h)
  (assert (Turno X) (Posicion ?destino_i ?destino_j O) (Posicion ?origen_i ?origen_j " ") )
)



;;;;;;;;;;; ACTUALIZAR  ESTADO TRAS JUGADA DE CLISP ;;;;;;;;;;;;;;;;;;

(defrule juega_clisp_actualiza_estado
  ?f <- (Juega X ?origen_i ?origen_j ?destino_i ?destino_j)
  ?h <- (Posicion ?origen_i ?origen_j X)
  ?g <- (Posicion ?destino_i ?destino_j " ")
=>
  (retract ?f ?g ?h)
  (assert (Turno O) (Posicion ?destino_i ?destino_j X) (Posicion ?origen_i ?origen_j " ") )
)


;;;;;;;;;;; CLISP JUEGA SIN CRITERIO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule clisp_juega_sin_criterio_fichas_sin_colocar
  (declare (salience -9999))
  ?f<- (Turno X)
  (Fichas_sin_colocar X ?n)
  ?g<- (Posicion ?i ?j " ")
=>
  (printout t "Juego poner ficha en " ?i ?j crlf)
  (retract ?f ?g)
  (assert (Posicion ?i ?j X) (Turno O) (reducir_fichas_sin_colocar X))
)

(defrule clisp_juega_sin_criterio
  (declare (salience -9999))
  ?f<- (Turno X)
  (Todas_fichas_en_tablero X)
  (Posicion ?origen_i ?origen_j X)
  (Posicion ?destino_i ?destino_j " ")
  (Conectado ?origen_i ?origen_j ? ?destino_i ?destino_j)
=>
  (assert (Juega X ?origen_i ?origen_j ?destino_i ?destino_j))
  (printout t "Juego mover la ficha de "  ?origen_i ?origen_j " a " ?destino_i ?destino_j crlf)
  (retract ?f)
)

(defrule tres_en_raya
  (declare (salience 9999))
  ?f <- (Turno ?X)
  (Posicion ?i1 ?j1 ?jugador)
  (Posicion ?i2 ?j2 ?jugador)
  (Posicion ?i3 ?j3 ?jugador)
  (Conectado ?i1 ?j1 ?forma ?i2 ?j2)
  (Conectado ?i2 ?j2 ?forma ?i3 ?j3)
  (test (neq ?jugador " "))
  (test (or (neq ?i1 ?i3) (neq ?j1 ?j3)))
=>
  (printout t ?jugador " ha ganado pues tiene tres en raya " ?i1 ?j1 " " ?i2 ?j2 " " ?i3 ?j3 crlf)
  (retract ?f)
  (assert (muestra_posicion))
)

;;; B1) Crear reglas para que el sistema deduzca que dos posiciones están en línea. ;;;

(defrule Enlinea
  (Conectado ?i1 ?j1 ?forma ?i2 ?j2)            ; posiciones conectadas de la
  (Conectado ?i2 ?j2 ?forma ?i3 ?j3)            ; misma forma a distancia 1 y 2
  (test (or (neq ?i1 ?i3) (neq ?j1 ?j3)))       ; tienen que ser distintas
=>
  (assert (Enlinea ?forma ?i1 ?j1 ?i2 ?j2))
  (assert (Enlinea ?forma ?i2 ?j2 ?i1 ?j1))
  (assert (Enlinea ?forma ?i1 ?j1 ?i3 ?j3))
  (assert (Enlinea ?forma ?i3 ?j3 ?i1 ?j1))
)

;;; B2) Crear reglas para que el sistema deduzca y mantenga que un jugador tiene dos fichas en la misma en línea. ;;;

(defrule Dos_en_linea
  (declare (salience 9999))
  (logical                                       ; si cualquier predicado deja de ser cierto desaparece
    (Posicion ?i ?j ?jugador)                    ; ficha ocupada por jugador
    (Posicion ?i1 ?j1 ?jugador)                  ; otra ficha ocupada por jugador
    (test (neq ?jugador " ")))                   ; las fichas son distintas
  (Enlinea ?forma ?i ?j ?i1 ?j1)                 ; las fichas estan en linea
=>
  (assert (Dos_en_linea ?forma ?i ?j ?i1 ?j1 ?jugador))
)

;;; B3) Crear reglas para deducir y mantener si un jugador puede hacer un movimiento ganador. ;;;

(defrule Puede_ganar_colocando
	(declare (salience 9999))
	(logical
		(Fichas_sin_colocar ?jugador 1)			          ; queda solo una ficha sin colocar
		(Dos_en_linea ?forma ?f1 ?c1 ?f2 ?c2 ?jugador); tenemos previamente 2 en linea
		(Posicion ?f3 ?c3 " ")                        ; hay una posicion vacia
		(Enlinea ?forma ?f1 ?c1 ?f3 ?c3))             ; que esta en linea
	=>
	(assert (Puede_ganar_colocando ?f3 ?c3 ?jugador))
)

(defrule Puede_ganar_moviendo
	(declare (salience 9999))
	(logical
		(Todas_fichas_en_tablero ?jugador)		        ; todas colocadas
		(Dos_en_linea ?forma ?f1 ?c1 ?f2 ?c2 ?jugador); hay 2 en linea
		(Enlinea ?forma ?f1 ?c1 ?f3 ?c3)              ; buscamos la que esta en linea
		(Posicion ?f3 ?c3 " ")                        ; esta libre
		(Posicion ?i ?j ?jugador)                     ; posicion de la tercera ficha
		(Conectado ?f3 ?c3 ?otraforma ?i ?j)          ; conectada al objetivo
		(test (neq ?forma ?otraforma)))               ; Vemos que no sea ninguna de las 2 fichas que estan ya en linea
	=>
	(assert (Puede_ganar_moviendo ?i ?j ?f3 ?c3 ?jugador))
)

;;; B4) Añadir reglas para que el sistema incluya las estrategia “Si la maquina puede ganar haciendo una jugada, entonces hace esa jugada”. ;;;

(defrule Ganar_colocando
	(declare (salience 9999))
	?f<- (Turno X)                                   ; turno de clisp
	(Puede_ganar_colocando ?i ?j X)                  ; condicion fundamental
	?g<- (Posicion ?i ?j " ")                        ; hay que borrar
=>
	(printout t "Juego poner ficha en " ?i ?j crlf)  ; movemos a la posicion objetivo
	(retract ?f ?g)
	(assert (Posicion ?i ?j X) (Turno O) (reducir_fichas_sin_colocar X))
)

(defrule Ganar_moviendo
	(declare (salience 9999))
	?f<- (Turno X)
	(Puede_ganar_moviendo ?i1 ?j1 ?i2 ?j2 X)         ; si puedo ganar
=>
	(assert (Juega X ?i1 ?j1 ?i2 ?j2))               ; juego ganar
	(printout t "Juego mover la ficha de "  ?i1 ?j1 " a " ?i2 ?j2 crlf)
	(retract ?f)
)

;;; B5) Añadir reglas para que el sistema incluya las estrategia “Si el jugador puede ganar haciendo una jugada y la máquina puede evitarlo, hace la jugada que lo evita”. ;;;

(defrule Evita_ganar_colocando
	(declare (salience 9998))                        ; salience bajado
	?f<- (Turno X)
	(Fichas_sin_colocar X ?)                         ; a clisp le quedan fichas sin colocar
	(Puede_ganar_colocando ?i ?j O)                  ; el usuario puede ganar colocando
	?g<- (Posicion ?i ?j " ")
=>
	(printout t "Juego poner ficha en " ?i ?j crlf)
	(retract ?f ?g)
	(assert (Posicion ?i ?j X) (Turno O) (reducir_fichas_sin_colocar X))
)

(defrule Evita_ganar_colocando_2
	(declare (salience 9998))                        ; salience bajado
	?f<- (Turno X)
	(Fichas_sin_colocar X ?)                         ; a clisp le quedan fichas sin colocar
	(Puede_ganar_moviendo ?i ?j O)                   ; el usuario puede ganar colocando
	?g<- (Posicion ?i ?j " ")
=>
	(printout t "Juego poner ficha en " ?i ?j crlf)
	(retract ?f ?g)
	(assert (Posicion ?i ?j X) (Turno O) (reducir_fichas_sin_colocar X))
)

(defrule Evita_ganar_moviendo
	(declare (salience 9998))                        ; salience bajado
	?f<- (Turno X)
	(Todas_fichas_en_tablero X)                      ; todas las fichas colocadas
	(Puede_ganar_colocando ?i1 ?j1 ?i2 ?j2 O)        ; usuario puede ganar moviendo
	(Conectado ?i2 ?j2 ?forma ?f ?c)                 ; la posicion donde gana esta conectada con otra
	(Posicion ?f ?c X)                               ; donde clisp tiene una ficha
=>
	(assert (Juega X ?f ?c ?i2 ?j2))
	(printout t "Juego mover la ficha de "  ?f ?c " a " ?i2 ?j2 crlf)
	(retract ?f)
)

(defrule Evita_ganar_moviendo_2
	(declare (salience 9998))                        ; salience bajado
	?f<- (Turno X)
	(Todas_fichas_en_tablero X)                      ; todas las fichas colocadas
	(Puede_ganar_moviendo ?i1 ?j1 ?i2 ?j2 O)         ; usuario puede ganar moviendo
	(Conectado ?i2 ?j2 ?forma ?f ?c)                 ; la posicion donde gana esta conectada con otra
	(Posicion ?f ?c X)                               ; donde clisp tiene una ficha
=>
	(assert (Juega X ?f ?c ?i2 ?j2))
	(printout t "Juego mover la ficha de "  ?f ?c " a " ?i2 ?j2 crlf)
	(retract ?f)
)

;;; Una posible mejora que he detectado es que clisp no mueva fichas que están evitando ganar ;;;

;(defrule Evita_quitar_ficha_clave
;	(declare (salience 9997))                        ; salience bajado más
;	?f<- (Turno X)
;	(Todas_fichas_en_tablero X)                      ; todas las fichas colocadas
;	(Dos_en_linea ?forma ?i1 ?j1 ?i2 ?j2 O)          ; usuario puede ganar moviendo
;	(Conectado ?i2 ?j2 ?forma ?f1 ?c1)               ; la posicion donde gana esta conectada con otra
;	(Posicion ?f1 ?c1 X)                             ; donde clisp tiene una ficha
; (Posicion ?f2 ?c2 X)                             ; mover otra ficha
; (Posicion ?f3 ?c3 " ")                           ; posicion vacia
; (Conectado ?f2 ?c2 ? ?f3 ?c3)                    ; y conectada
; (test (or (neq ?f1 ?f2) (neq ?c1 ?c2)))
;=>
;	(assert (Juega X ?f2 ?c2 ?f3 ?c3))
;	(printout t "Juego mover la ficha de "  ?f2 ?c2 " a " ?f3 ?c3 crlf)
;	(retract ?f)
;)
