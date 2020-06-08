;;;;;;;                                            PRACTICA DE IC                                             ;;;;;;;
;;;;;;;                                     Carlos Santiago Sánchez Muñoz                                     ;;;;;;;
;;;;;;; Asesorar a un estudiante de ingeniería informática como lo haría un compañero, sobre:                 ;;;;;;;
;;;;;;; - qué rama elegir.                                                                                    ;;;;;;;
;;;;;;; - qué asignaturas matricular                                                                          ;;;;;;;

;;;;;;;                                                RAMAS                                                  ;;;;;;;
;; 1. Le pregunte al usuario que pide asesoramiento lo que le preguntaría el compañero que hace de experto.
;; 2. Realice los razonamientos que haría el compañero que hace de experto
;; 3. Le aconseje la rama o las ramas que le aconsejaría el compañero junto con los motivos por los que se lo aconsejaría.
;;; El sistema realizará preguntas e irá tomando un Consejo y los motivos por los cuales decide ese Consejo.
;;; En cualquier momento se puede responder "Fin" y el usuario será aconsejado. También se da la posibilidad de responder
;;; "No se" a algunas preguntas.

;;;;;;;                                             ASIGNATURAS                                               ;;;;;;;
;;;;;;; Dadas un conjunto de asignaturas posibles y unos créditos a cumplimentar, aconsejar en cuáles de esas ;;;;;;;
;;;;;;; asignaturas matricularse.                                                                             ;;;;;;;
;;;;;;; Incluir al menos un tipo de razonamiento con incertidumbre (razonamiento por defecto, factores de     ;;;;;;;
;;;;;;; certeza, lógica difusa o razonamiento probabilístico) y el sistema debe de justificar claramente al   ;;;;;;;
;;;;;;; usuario el razonamiento que se ha seguido.                                                            ;;;;;;;

(defmodule MAIN (export ?ALL))

(deffacts MAIN::Main
  (Elige 0)
)

(defmodule RAMAS (import MAIN ?ALL))

(defmodule ASIGNATURAS (import MAIN ?ALL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MENSAJE DE BIENVENIDA AL SISTEMA ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Motramos las diferentes Ramas y explicamos el funcionamiento ;;;

(defrule MAIN::Da_bienvenida
  (declare (salience 10))
  ?f <- (Elige 0)
=>
  (printout t "Bienvenido al sistema de asesoramiento de ramas y asignaturas del Grado en I. Informatica." crlf)
  (printout t "Indique si desea asesoramiento acerca de Ramas (Ramas) o Asignaturas (Asignaturas)." crlf)
  (bind ?res (read))
  (while (and (neq ?res Ramas) (neq ?res Asignaturas))
    (printout t "Respuesta incorrecta. Indique si desea asesoramiento acerca de Ramas (Ramas) o Asignaturas (Asignaturas)." crlf)
    (bind ?res (read)))
  (retract ?f)
  (if (eq ?res Ramas) then (assert (Elige 1)) (focus RAMAS)
  else (assert (Elige 2)) (focus ASIGNATURAS))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   PREGUNTAS DEL SISTEMA (CON CHECKEOS)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Representar las ramas ;;;

(deffacts RAMAS::Ramas
  (Rama Computacion_y_Sistemas_Inteligentes)
  (Rama Ingenieria_del_Software)
  (Rama Ingenieria_de_Computadores)
  (Rama Sistemas_de_Informacion)
  (Rama Tecnologias_de_la_Informacion)
)

;; Bienvenida al sistema de asesoración de Ramas.

(defrule RAMAS::Da_bienvenida_Ramas
  (declare (salience 10))
=>
  (printout t "Bienvenido al sistema de asesoramiento de ramas del Grado en I. Informatica. Las ramas son:" crlf)
  (printout t "- Computacion y Sistemas Inteligentes." crlf)
  (printout t "- Ingenieria del Software." crlf)
  (printout t "- Ingenieria de Computadores." crlf)
  (printout t "- Sistemas de Informacion." crlf)
  (printout t "- Tecnologias de la Informacion." crlf)
  (printout t "En cualquier momento puede responder Fin para obtener el consejo sin responder el resto de preguntas." crlf)
)

;;; Si le gustan las matematicas ;;;

(defrule RAMAS::Gusta_mat
  (declare (salience 9))
=>
  (printout t "Te gustan las matematicas? (Si/No)" crlf)
  (assert (Gusta_mat (read)))
)

(defrule RAMAS::Gusta_mat_check
  (declare (salience 100))
  ?f <- (Gusta_mat ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gustan las matematicas? (Si/No)" crlf)
  (retract ?f)
  (assert (Gusta_mat (read)))
)

;;; Cómo de trabajador se considera ;;;

(defrule RAMAS::Es_trabajador
  (declare (salience 9))
  (Gusta_mat Si)
=>
  (printout t "Eres trabajador? (Mucho/Normal/Poco)" crlf)
  (assert (Es_trabajador (read)))
)

(defrule RAMAS::Es_trabajador_check
  (declare (salience 100))
  ?f <- (Es_trabajador ?r)
  (test (and (neq ?r Mucho) (neq ?r Normal) (neq ?r Poco) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Eres trabajador? (Mucho/Normal/Poco)" crlf)
  (retract ?f)
  (assert (Es_trabajador (read)))
)

;;; Si le gusta programar ;;;

(defrule RAMAS::Gusta_prog
  (declare (salience 9))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
=>
  (printout t "Te gusta programar? (Si/No/No se)" crlf)
  (assert (Gusta_prog (explode$ (readline))))
)

(defrule RAMAS::Gusta_prog_check
  (declare (salience 100))
  ?f <- (Gusta_prog ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gusta programar? (Si/No/No se)" crlf)
  (retract ?f)
  (assert (Gusta_prog (explode$ (readline))))
)

;;; Si le gusta el hardware ;;;

(defrule RAMAS::Gusta_hardw
  (declare (salience 9))
  (Gusta_mat No)
=>
  (printout t "Te gusta el hardware? (Si/No/No se)" crlf)
  (assert (Gusta_hardw (explode$ (readline))))
)

(defrule RAMAS::Gusta_hardw_check
  (declare (salience 100))
  ?f <- (Gusta_hardw ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gustan el hardware? (Si/No/No se)" crlf)
  (retract ?f)
  (assert (Gusta_hardw (explode$ (readline))))
)

;;; En qué quiere trabajar ;;;

(defrule RAMAS::Trabajar
  (declare (salience 9))
  (Gusta_mat No)
  (Gusta_hardw Si)
=>
  (printout t "En que te gustaria trabajar? (Publica/Privada/Docencia/No se)" crlf)
  (assert (Trabajar (explode$ (readline))))
)

(defrule RAMAS::Trabajar_check
  (declare (salience 100))
  ?f <- (Trabajar ?r)
  (test (and (neq ?r Publica) (neq ?r Privada) (neq ?r Docencia) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta incorrecta. En que te gustaria trabajar? (Publica/Privada/Docencia/No se)" crlf)
  (retract ?f)
  (assert (Trabajar (explode$ (readline))))
)

;;; La nota media que ha obtenido en las asignaturas que ha cursado hasta ahora ;;;

(defrule RAMAS::Nota
  (declare (salience 9))
  (Gusta_mat No)
  (Gusta_hardw No)
=>
  (printout t "Introduce tu calificacion media de expediente" crlf)
  (bind ?num (read))
  (while (and (not (numberp ?num)) (not (eq ?num Fin)))
    (printout t "Respuesta incorrecta (debe de ser flotante o entero). Introduce tu calificacion media de expediente" crlf)
    (bind ?num (read)))
  (if (eq ?num Fin) then (assert (Consejo Sistemas_de_Informacion "no te gustan las matematicas ni el hardware" "CLISP"))
  else (assert (Calificacion_media ?num)))
)

(defrule RAMAS::Nota_check_interval
  (declare (salience 99))
  ?f <- (Calificacion_media ?r)
  (test (or (> ?r 10) (< ?r 5)))
=>
  (printout t "Respuesta incorrecta (fuera de [5,10]). Introduce tu calificacion media de expediente" crlf)
  (retract ?f)
  (bind ?num (read))
  (while (and (not (numberp ?num)) (not (eq ?num Fin)))
    (printout t "Respuesta incorrecta (debe de ser flotante o entero). Introduce tu calificacion media de expediente" crlf)
    (bind ?num (read)))
  (if (eq ?num Fin) then (assert (Consejo Sistemas_de_Informacion "no te gustan las matematicas ni el hardware" "CLISP"))
  else (assert (Calificacion_media ?num)))
)

;;; Convertir la Calificacion_media a Nota ;;;

(defrule RAMAS::Nota_conversion_alta
  (declare (salience 20))
  (Calificacion_media ?r)
  (test (>= ?r 8))
=>
  (assert (Nota Alta))
)

(defrule RAMAS::Nota_conversion_media
  (declare (salience 20))
  (Calificacion_media ?r)
  (test (and (>= ?r 6.5) (< ?r 8)))
=>
  (assert (Nota Media))
)

(defrule RAMAS::Nota_conversion_baja
  (declare (salience 20))
  (Calificacion_media ?r)
  (test (and (>= ?r 5) (< ?r 6.5)))
=>
  (assert (Nota Baja))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   CONSEJOS DEL SISTEMA   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Método para imprimir un consejo ;;;

(defrule RAMAS::imprime_consejo
  (Elige ?e)
  (Rama ?rama)          ; sirve para asegurarse de que la rama existe
  (Consejo ?rama ?texto ?experto)
=>
  (printout t crlf)
  (printout t ?experto " te aconseja la Rama de " ?rama "." crlf)
  (printout t "Motivo: " ?texto "." crlf)
  (printout t crlf)
  (if (neq ?e 2) then (printout t "Quiere consejo acerca de asignaturas del Grado en I. Informatica? (Si/No)" crlf)
  (bind ?res (read))
  (while (and (neq ?res Si) (neq ?res No))
    (printout t "Respuesta incorrecta. Repita (Si/No)." crlf)
    (bind ?res (read)))
  (if (eq ?res Si) then (focus ASIGNATURAS)))
)

;;; Rama de Computación y Sistemas Inteligentes ;;;

(defrule RAMAS::Rama_CSI
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog Si)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas y programar y ademas eres trabajador" "CLISP"))
)

;;; Rama de Ingeniería del Software ;;;

(defrule RAMAS::Rama_IS
  (declare (salience 10))
  (Gusta_mat Si)
  (Es_trabajador Poco)
=>
  (assert (Consejo Ingenieria_del_Software "te gustan las matematicas pero no quieres tener mucha carga de trabajo" "CLISP"))
)

(defrule RAMAS::Rama_IS2
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw No)
  (Nota Baja)
=>
  (assert (Consejo Ingenieria_del_Software "no te gustan ni las matematicas ni el hardware y no tiene calificaciones altas por lo que te aconsejo una Rama mas general" "CLISP"))
)

;;; Rama de Ingeniería de Computadores ;;;

(defrule RAMAS::Rama_IC
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog No)
=>
  (assert (Consejo Ingenieria_de_Computadores "te gustan las matematicas y ser trabajador pero no te gusta programar" "CLISP"))
)

(defrule RAMAS::Rama_IC2
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar Publica)
=>
  (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware y la empresa publica" "CLISP"))
)

;;; Rama de Sistemas de Información ;;;

(defrule RAMAS::Rama_SI
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw No)
  (or (Nota Alta) (Nota Media))
=>
  (assert (Consejo Sistemas_de_Informacion "no te gustan las matematicas ni el hardware pero tienes buen exdiente" "CLISP"))
)

;;; Rama de Tecnologías de la Información ;;;

(defrule RAMAS::Rama_TI
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (or (Trabajar Privada) (Trabajar Docencia))
=>
  (assert (Consejo Tecnologias_de_la_Informacion "te gustaria trabajar en la publica o docencia y te gusta el hardware pero no las mates" "CLISP"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   POSIBILIDAD DE RESPONDER "No se"   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; No sabe a qué dedicarse ;;;

(defrule RAMAS::Rama_TI_No_se
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar No se)
=>
  (assert (Consejo Tecnologias_de_la_Informacion "no sabes donde trabajar pero te gusta el hardware y no las mates" "CLISP"))
  (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware por lo que esta es una buena opcion si descubres que te gustaria trabajar en la Empresa Publica" "CLISP"))
)

;;; No sabe si le gusta programar ;;;

(defrule RAMAS::Rama_CSI_No_se
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog No se)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "aunque no sabes si te gusta programar te gustan las matematicas y eres trabajador" "CLISP"))
  (assert (Consejo Ingenieria_de_Computadores "si descubres que no te gusta programar, esta puede ser una buena eleccion" "CLISP"))
)

;;; No sabe si le gusta el hardware ;;;

(defrule RAMAS::Gusta_hardw_No_se
  (declare (salience 10))
  ?f <- (Gusta_hardw No se)
=>
  (printout t "Te gustaron asignaturas como FFT, TOC y EC? (Si/No)" crlf)
  (retract ?f)
  (bind ?a (read))
  (while (and (neq ?a Si) (neq ?a No) (neq ?a Fin))
    (printout t "Respuesta incorrecta. Te gustaron asignaturas como FFT, TOC y EC? (Si/No)" crlf)
    (bind ?a (read)))
  (assert (Gusta_hardw ?a))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   POSIBILIDAD DE RESPONDER "Fin"   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule RAMAS::Fin_1
  (declare (salience 100))
  (Gusta_mat Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "no me has dado ninguna informacion, esta es la especialidad mas escogida" "CLISP"))
)

(defrule RAMAS::Fin_2
  (declare (salience 100))
  (Gusta_mat Si)
  (Es_trabajador Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas" "CLISP"))
)

(defrule RAMAS::Fin_3
  (declare (salience 100))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas y eres trabajador" "CLISP"))
)

(defrule RAMAS::Fin_4
  (declare (salience 100))
  (Gusta_mat No)
  (Gusta_hardw Fin)
=>
  (assert (Consejo Ingenieria_de_Computadores "no te gustan las matematicas" "CLISP"))
)

(defrule RAMAS::Fin_5
  (declare (salience 100))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar Fin)
=>
  (assert (Consejo Ingenieria_de_Computadores "no te gustan las matematicas pero te gusta el hardware" "CLISP"))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deffacts ASIGNATURAS::Asignaturas
  ;(Asingatura Nombre Codigo Curso Cuatri Teorica/Practica Dificil/Facil Mucho/Poco)

  (Asignatura Estructura_de_Datos ED 2 1 Practica Dificil Mucho)
  (Asignatura Estructura_de_Computadores EC 2 1 Teorica Facil Poco)
  (Asignatura Sistemas_Concurrentes_y_Distribuidos SCD 2 1 Practica Facil Poco)
  (Asignatura Sistemas_Operativos SO 2 1 Teorica Dificil Mucho)
  (Asignatura Programacion_y_Disenio_Orientado_a_Objetos PDOO 2 1 Practica Dificil Mucho)

  (Asignatura Algoritmica ALG 2 2 Practica Facil Poco)
  (Asignatura Arquitectura_de_Computadores AC 2 2 Teorica Facil Mucho)
  (Asignatura Fundamentos_de_Bases_de_Datos FBD 2 2 Practica Facil Poco)
  (Asignatura Fundamentos_de_Ingenieria_del_Software FIS 2 2 Teorica Facil Poco)
  (Asignatura Inteligencia_Artificial IA 2 2 Practica Dificil Mucho)

  (Asignatura Disenio_y_Desarrollo_de_Sistemas_de_la_Informacion DDSI 3 1 Practica Facil Poco)
  (Asignatura Fundamentos_de_Redes FR 3 1 Teorica Dificil Mucho)
  (Asignatura Informatica_Grafica IG 3 1 Practica Dificil Mucho)
  (Asignatura Ingenieria_de_Servidores ISE 3 1 Teorica Dificil Mucho)
  (Asignatura Modelos_de_Computacion MC 3 1 Teorica Facil Mucho)

  (SoloConsideradas FALSE)
  (ListoParaAconsejar FALSE)

  (AsigDeseadas 2 -1)
  (AsigDeseadas 1 -1)

  (ContarAsignaturas 2)
  (ContarAsignaturas 1)

  (ContadorAconsejadas 2 0)
  (ContadorAconsejadas 1 0)

  (ContadorConsejosSeguros 2 0)
  (ContadorConsejosSeguros 1 0)
)

;;; Método para imprimir un consejo ;;;

(defrule ASIGNATURAS::imprime_consejo_seguro
  (declare (salience 10))
  (ListoParaAconsejar TRUE)
  ?g <- (Consejo ?i ?r ?experto)
  (test (neq ?r por_defecto))
  (Asignatura ?a ?i ?c1 ?c2 ? ? ?)

  (AsigDeseadas ?c2 ?d)
  ?f <- (ContadorAconsejadas ?c2 ?n)
  (test (< ?n ?d))
=>
  (printout t crlf)
  (printout t ?experto " te aconseja la Asignatura " ?a " (" ?i ") del curso " ?c1 " y cuatrimestre " ?c2 "." crlf)
  (printout t "Razonamiento: por ser " ?r "." crlf)
  (retract ?f)
  (retract ?g)
  (assert (ContadorAconsejadas ?c2 (+ 1 ?n)))
)

(defrule ASIGNATURAS::imprime_consejo_por_defecto
  (ListoParaAconsejar TRUE)
  ?g <- (Consejo ?i por_defecto ?experto)
  (Asignatura ?a ?i ?c1 ?c2 ? ? ?)

  (AsigDeseadas ?c2 ?d)
  ?f <- (ContadorAconsejadas ?c2 ?n)
  (test (< ?n ?d))
=>
  (printout t crlf)
  (printout t ?experto " te aconseja la Asignatura " ?a " (" ?i ") del curso " ?c1 " y cuatrimestre " ?c2 "." crlf)
  (printout t "Razonamiento: por defecto." crlf)
  (retract ?f)
  (retract ?g)
  (assert (ContadorAconsejadas ?c2 (+ 1 ?n)))
)

;; Inicializamos el contador
(defrule ASIGNATURAS::contar
  (declare (salience 9999))
	(ContarAsignaturas ?c)
=>
	(assert (AsigExistentes ?c 0))
)

;; Conteo
(defrule ASIGNATURAS::contar_hecho
  (declare (salience 9998))
	(Asignatura ?a ? ? ?c ? ? ?)
	(ContarAsignaturas ?c)
  (not (Contado ?a))
	?f <- (AsigExistentes ?c ?n)
=>
  (assert (Contado ?a))
	(retract ?f)
	(assert (AsigExistentes ?c (+ 1 ?n)))
)

;; Borrar
(defrule ASIGNATURAS::borrar1
  (declare (salience 9996))
	?f <- (Contado ?a)
=>
	(retract ?f)
)

(defrule ASIGNATURAS::borrar2
  (declare (salience 9997))
	?f <- (ContarAsignaturas ?c)
=>
	(retract ?f)
)

(defrule ASIGNATURAS::Da_bienvenida_Asignaturas
  (declare (salience 10))
=>
  (printout t "Bienvenido al sistema de asesoramiento de asignaturas OBLIGATORIAS del Grado en I. Informatica. Las asignaturas son:" crlf)
  (assert (ImprimeAsig))
)

(defrule ASIGNATURAS::Imprime_Asignaturas_2
  (declare (salience 10))
  (ImprimeAsig)
  (Asignatura ?a ?i 2 ?c ? ? ?)
=>
  (if (eq ?c 1) then (printout t "- " ?a ", 2do curso, 1er cuatrimestre (" ?i ")." crlf)
  else (printout t "- " ?a ", 2do curso, 2do cuatrimestre (" ?i ")." crlf))
)

(defrule ASIGNATURAS::Imprime_Asignaturas_3
  (declare (salience 9))
  (ImprimeAsig)
  (Asignatura ?a ?i 3 ?c ? ? ?)
=>
  (if (eq ?c 1) then (printout t "- " ?a ", 3er curso, 1er cuatrimestre (" ?i ")." crlf)
  else (printout t "- " ?a ", 3er curso, 2do cuatrimestre (" ?i ")." crlf))
)

(defrule ASIGNATURAS::Indique_Asignaturas
  (declare (salience 8))
  ?f <- (ImprimeAsig)
=>
  (retract ?f)
  (printout t "Indique, usando los codigos, las asignaturas que quiere que se consideren (separelas por espacios)." crlf)
  (assert (OpcionesElegidas (explode$ (readline))))
)

(defrule ASIGNATURAS::Opciones_Elegidas
  ?f <- (OpcionesElegidas ?part1 $?part2)
=>
  (assert (OpcionElegida ?part1))
  (retract ?f)
  (assert (OpcionesElegidas ?part2))
)

(defrule ASIGNATURAS::Check_Opcion_Elegida
  (declare (salience 3))
  ?f <- (OpcionElegida ?o)
  (not (Asignatura ? ?o ? ? ? ? ?))
=>
  (printout t "La opcion elegida " ?o " no es una asinatura valida. Repita:" crlf)
  (assert (OpcionesElegidas (explode$ (readline))))
  (retract ?f)
)

(defrule ASIGNATURAS::borrar_op
  ?f <- (OpcionesElegidas)
=>
  (retract ?f)
)

(defrule ASIGNATURAS::Borrar_Asig_No_Consideradas
  (declare (salience 2))
  (SoloConsideradas FALSE)
  ?f <- (Asignatura ? ?o ? ?c ? ? ?)
  ?g <- (AsigExistentes ?c ?)
  (not (OpcionElegida ?o))
  (not (OpcionesElegidas $?))
=>
  (printout t "Borrando " ?o " ya que no es una opcion considerada." crlf)
  (assert (ContarAsignaturas ?c))
  (retract ?f)
  (retract ?g)
)

(defrule ASIGNATURAS::Pedir_Asig
  ?g <- (AsigDeseadas ?c -1)
=>
  (printout t "Indique el numero de asignaturas a matricular del " ?c " cuatrimestre:" crlf)
  (bind ?num (read))
  (while (not (integerp ?num))
  (printout t "Respuesta '" ?num "' no valida (debe de ser entero). Repita la opcion" crlf)
  (bind ?num (read)))
  (retract ?g)
  (assert (AsigDeseadas ?c ?num))
)

(defrule ASIGNATURAS::Check_Asig
  ?g <- (AsigDeseadas ?c ?n)
  (test (or (< ?n 0) (> ?n 5)))
=>
  (printout t "Numero de asignaturas a matricular en " ?c " cuatrimestre fuera de rango. Repita:" crlf)
  (bind ?num (read))
  (while (not (integerp ?num))
  (printout t "Respuesta '" ?num "' no valida (debe de ser entero). Repita la opcion" crlf)
  (bind ?num (read)))
  (retract ?g)
  (assert (AsigDeseadas ?c ?num))
)

(defrule ASIGNATURAS::Cambiar_Solo_Consideradas
  (not (and (AsigDeseadas ? ?n) (test (eq ?n -1))))
  ?f <- (SoloConsideradas FALSE)
=>
  (assert (SoloConsideradas TRUE))
  (retract ?f)
)

(defrule ASIGNATURAS::Borrar_Opcion_Elegida
  (declare (salience 20))
  (SoloConsideradas TRUE)
  ?f <- (OpcionElegida ?)
=>
  (retract ?f)
)

(defrule ASIGNATURAS::Introduce_Consejo
  (declare (salience 10))
  (not (and (AsigDeseadas ? ?n) (test (eq ?n -1))))
  (SoloConsideradas TRUE)
  (Asignatura ? ?i ? ? ? ? ?)
=>
  (assert (Consejo ?i por_defecto "CLIPS"))
)

(defrule ASIGNATURAS::Fin_cuatri
  (declare (salience 999))
  (ListoParaAconsejar FALSE)
  (AsigExistentes ?c ?ne)
  (AsigDeseadas ?c ?d)
  (ContadorConsejosSeguros ?c ?ns)
  (not (FinCuatri ?c))
  (test (or (<= ?ne ?d) (and (<= ?d ?ns) (neq ?ns 0))))
=>
  (assert (FinCuatri ?c))
)

(defrule ASIGNATURAS::Fin_Consejo
  (declare (salience 999))
  ?f <- (ListoParaAconsejar FALSE)
  (FinCuatri 1)
  (FinCuatri 2)
=>
  (retract ?f)
  (assert (ListoParaAconsejar TRUE))
)

;; Carácter Teórico/Práctico

(defrule ASIGNATURAS::Siguiente1
  ?f <- (ListoParaAconsejar FALSE)
  (not (and (AsigDeseadas ? ?n) (test (eq ?n -1))))
  (SoloConsideradas TRUE)
=>
  (printout t "Prefiere asignaturas con mas teoricas o mas practicas (Teorica/Practica/Fin):" crlf)
  (bind ?res (read))
  (while (and (neq ?res Teorica) (neq ?res Practica) (neq ?res Fin))
    (printout t "Respuesta incorrecta. Repita (Teorica/Practica/Fin)." crlf)
    (bind ?res (read)))
  (if (eq ?res Fin) then (retract ?f) (assert (ListoParaAconsejar TRUE))
  else (assert (Caracter ?res)))
)

(defrule ASIGNATURAS::Teorica_Practica
  (declare (salience 10))
  (Caracter ?car)
  (Asignatura ?a ?i ? ?c ?car ? ?)
  ?f <- (Consejo ?i por_defecto ?e)
  ?g <- (ContadorConsejosSeguros ?c ?n)
=>
  (retract ?f)
  (retract ?g)
  (assert (Consejo ?i ?car ?e))
  (assert (ContadorConsejosSeguros ?c (+ 1 ?n)))
)

;; Dificultad Fácil/Difícil

(defrule ASIGNATURAS::Siguiente_2
  ?f <- (ListoParaAconsejar FALSE)
  (not (and (AsigDeseadas ? ?n) (test (eq ?n -1))))
  (SoloConsideradas TRUE)
  (Caracter ?)
=>
  (printout t "Prefiere realizar asignaturas faciles o dificiles (Facil/Dificil/Fin):" crlf)
  (bind ?res (read))
  (while (and (neq ?res Facil) (neq ?res Dificil) (neq ?res Fin))
    (printout t "Respuesta incorrecta. Repita (Facil/Dificil/Fin)." crlf)
    (bind ?res (read)))
  (if (eq ?res Fin) then (retract ?f) (assert (ListoParaAconsejar TRUE))
  else (assert (Dificultad ?res)))
)

(defrule ASIGNATURAS::Facil_Dificil
  (declare (salience 10))
  (Dificultad ?dif)
  (Asignatura ?a ?i ? ?c ? ?dif ?)
  ?f <- (Consejo ?i por_defecto ?e)
  ?g <- (ContadorConsejosSeguros ?c ?n)
=>
  (retract ?f)
  (retract ?g)
  (assert (Consejo ?i ?dif ?e))
  (assert (ContadorConsejosSeguros ?c (+ 1 ?n)))
)

;; Trabajo Mucho/Poco

(defrule ASIGNATURAS::Siguiente3
  ?f <- (ListoParaAconsejar FALSE)
  (not (and (AsigDeseadas ? ?n) (test (eq ?n -1))))
  (SoloConsideradas TRUE)
  (Caracter ?)
  (Dificultad ?)
=>
  (printout t "Prefiere realizar asignaturas con mucho o poco trabajo (Mucho/Poco/Fin):" crlf)
  (bind ?res (read))
  (while (and (neq ?res Mucho) (neq ?res Poco) (neq ?res Fin))
    (printout t "Respuesta incorrecta. Repita (Mucho/Poco/Fin)." crlf)
    (bind ?res (read)))
  (if (eq ?res Fin) then (retract ?f) (assert (ListoParaAconsejar TRUE))
  else (assert (Trabajo ?res)))
)

(defrule ASIGNATURAS::Mucho_Poco
  (declare (salience 10))
  (Trabajo ?tra)
  (Asignatura ?a ?i ? ?c ? ? ?tra)
  ?f <- (Consejo ?i por_defecto ?e)
  ?g <- (ContadorConsejosSeguros ?c ?n)
=>
  (retract ?f)
  (retract ?g)
  (assert (Consejo ?i ?tra ?e))
  (assert (ContadorConsejosSeguros ?c (+ 1 ?n)))
)

(defrule ASIGNATURAS::Ir_Ramas
  (declare (salience -9999))
  (Elige ?e)
  (ListoParaAconsejar TRUE)
=>
  (printout t crlf)
  (if (neq ?e 1) then (printout t "Quiere consejo acerca de la Rama del Grado en I. Informatica? (Si/No)" crlf)
  (bind ?res (read))
  (while (and (neq ?res Si) (neq ?res No))
    (printout t "Respuesta incorrecta. Repita (Si/No)." crlf)
    (bind ?res (read)))
  (if (eq ?res Si) then (focus RAMAS)))
)
