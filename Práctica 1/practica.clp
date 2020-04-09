;;;;;;;                                            PRACTICA DE IC                                             ;;;;;;;
;;;;;;;                                     Carlos Santiago Sánchez Muñoz                                     ;;;;;;;
;;;;;;; Asesorar a un estudiante de ingeniería informática como lo haría un compañero, sobre qué rama elegir. ;;;;;;;

;; 1. Le pregunte al usuario que pide asesoramiento lo que le preguntaría el compañero que hace de experto.
;; 2. Realice los razonamientos que haría el compañero que hace de experto
;; 3. Le aconseje la rama o las ramas que le aconsejaría el compañero junto con los motivos por los que se lo aconsejaría.

;;; El sistema realizará preguntas e irá tomando un Consejo y los motivos por los cuales decide ese Consejo.
;;; En cualquier momento se puede responder "Fin" y el usuario será aconsejado. También se da la posibilidad de responder
;;; "No se" a algunas preguntas.

;;; Representar las ramas ;;;

(deffacts Ramas
  (Rama Computacion_y_Sistemas_Inteligentes)
  (Rama Ingenieria_del_Software)
  (Rama Ingenieria_de_Computadores)
  (Rama Sistemas_de_Informacion)
  (Rama Tecnologias_de_la_Informacion)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MENSAJE DE BIENVENIDA AL SISTEMA ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Motramos las diferentes Ramas y explicamos el funcionamiento ;;;

(defrule Da_bienvenida
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   PREGUNTAS DEL SISTEMA (CON CHECKEOS)   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Si le gustan las matematicas ;;;

(defrule Gusta_mat
  (declare (salience 9))
=>
  (printout t "Te gustan las matematicas? (Si/No)" crlf)
  (assert (Gusta_mat (read)))
)

(defrule Gusta_mat_check
  (declare (salience 100))
  ?f <- (Gusta_mat ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gustan las matematicas? (Si/No)" crlf)
  (retract ?f)
  (assert (Gusta_mat (read)))
)

;;; Cómo de trabajador se considera ;;;

(defrule Es_trabajador
  (declare (salience 9))
  (Gusta_mat Si)
=>
  (printout t "Eres trabajador? (Mucho/Normal/Poco)" crlf)
  (assert (Es_trabajador (read)))
)

(defrule Es_trabajador_check
  (declare (salience 100))
  ?f <- (Es_trabajador ?r)
  (test (and (neq ?r Mucho) (neq ?r Normal) (neq ?r Poco) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Eres trabajador? (Mucho/Normal/Poco)" crlf)
  (retract ?f)
  (assert (Es_trabajador (read)))
)

;;; Si le gusta programar ;;;

(defrule Gusta_prog
  (declare (salience 9))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
=>
  (printout t "Te gusta programar? (Si/No/No se)" crlf)
  (assert (Gusta_prog (explode$ (readline))))
)

(defrule Gusta_prog_check
  (declare (salience 100))
  ?f <- (Gusta_prog ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gusta programar? (Si/No/No se)" crlf)
  (retract ?f)
  (assert (Gusta_prog (explode$ (readline))))
)

;;; Si le gusta el hardware ;;;

(defrule Gusta_hardw
  (declare (salience 9))
  (Gusta_mat No)
=>
  (printout t "Te gusta el hardware? (Si/No/No se)" crlf)
  (assert (Gusta_hardw (explode$ (readline))))
)

(defrule Gusta_hardw_check
  (declare (salience 100))
  ?f <- (Gusta_hardw ?r)
  (test (and (neq ?r Si) (neq ?r No) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta no valida. Te gustan el hardware? (Si/No/No se)" crlf)
  (retract ?f)
  (assert (Gusta_hardw (explode$ (readline))))
)

;;; En qué quiere trabajar ;;;

(defrule Trabajar
  (declare (salience 9))
  (Gusta_mat No)
  (Gusta_hardw Si)
=>
  (printout t "En que te gustaria trabajar? (Publica/Privada/Docencia/No se)" crlf)
  (assert (Trabajar (explode$ (readline))))
)

(defrule Trabajar_check
  (declare (salience 100))
  ?f <- (Trabajar ?r)
  (test (and (neq ?r Publica) (neq ?r Privada) (neq ?r Docencia) (neq ?r No se) (neq ?r Fin)))
=>
  (printout t "Respuesta incorrecta. En que te gustaria trabajar? (Publica/Privada/Docencia/No se)" crlf)
  (retract ?f)
  (assert (Trabajar (explode$ (readline))))
)

;;; La nota media que ha obtenido en las asignaturas que ha cursado hasta ahora ;;;

(defrule Nota
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

(defrule Nota_check_interval
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

(defrule Nota_conversion_alta
  (declare (salience 20))
  (Calificacion_media ?r)
  (test (>= ?r 8))
=>
  (assert (Nota Alta))
)

(defrule Nota_conversion_media
  (declare (salience 20))
  (Calificacion_media ?r)
  (test (and (>= ?r 6.5) (< ?r 8)))
=>
  (assert (Nota Media))
)

(defrule Nota_conversion_baja
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

(defrule imprime_consejo
  (Rama ?rama)          ; sirve para asegurarse de que la rama existe
  (Consejo ?rama ?texto ?experto)
=>
  (printout t ?experto " te aconseja la Rama de " ?rama "." crlf)
  (printout t "Motivo: " ?texto "." crlf)
)

;;; Rama de Computación y Sistemas Inteligentes ;;;

(defrule Rama_CSI
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog Si)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas y programar y ademas eres trabajador" "CLISP"))
)

;;; Rama de Ingeniería del Software ;;;

(defrule Rama_IS
  (declare (salience 10))
  (Gusta_mat Si)
  (Es_trabajador Poco)
=>
  (assert (Consejo Ingenieria_del_Software "te gustan las matematicas pero no quieres tener mucha carga de trabajo" "CLISP"))
)

(defrule Rama_IS2
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw No)
  (Nota Baja)
=>
  (assert (Consejo Ingenieria_del_Software "no te gustan ni las matematicas ni el hardware y no tiene calificaciones altas por lo que te aconsejo una Rama mas general" "CLISP"))
)

;;; Rama de Ingeniería de Computadores ;;;

(defrule Rama_IC
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog No)
=>
  (assert (Consejo Ingenieria_de_Computadores "te gustan las matematicas y ser trabajador pero no te gusta programar" "CLISP"))
)

(defrule Rama_IC2
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar Publica)
=>
  (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware y la empresa publica" "CLISP"))
)

;;; Rama de Sistemas de Información ;;;

(defrule Rama_SI
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw No)
  (or (Nota Alta) (Nota Media))
=>
  (assert (Consejo Sistemas_de_Informacion "no te gustan las matematicas ni el hardware pero tienes buen exdiente" "CLISP"))
)

;;; Rama de Tecnologías de la Información ;;;

(defrule Rama_TI
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

(defrule Rama_TI_No_se
  (declare (salience 10))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar No se)
=>
  (assert (Consejo Tecnologias_de_la_Informacion "no sabes donde trabajar pero te gusta el hardware y no las mates" "CLISP"))
  (assert (Consejo Ingenieria_de_Computadores "te gusta el hardware por lo que esta es una buena opcion si descubres que te gustaria trabajar en la Empresa Publica" "CLISP"))
)

;;; No sabe si le gusta programar ;;;

(defrule Rama_CSI_No_se
  (declare (salience 10))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog No se)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "aunque no sabes si te gusta programar te gustan las matematicas y eres trabajador" "CLISP"))
  (assert (Consejo Ingenieria_de_Computadores "si descubres que no te gusta programar, esta puede ser una buena eleccion" "CLISP"))
)

;;; No sabe si le gusta el hardware ;;;

(defrule Gusta_hardw_No_se
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

(defrule Fin_1
  (declare (salience 100))
  (Gusta_mat Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "no me has dado ninguna informacion, esta es la especialidad mas escogida" "CLISP"))
)

(defrule Fin_2
  (declare (salience 100))
  (Gusta_mat Si)
  (Es_trabajador Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas" "CLISP"))
)

(defrule Fin_3
  (declare (salience 100))
  (Gusta_mat Si)
  (or (Es_trabajador Mucho) (Es_trabajador Normal))
  (Gusta_prog Fin)
=>
  (assert (Consejo Computacion_y_Sistemas_Inteligentes "te gustan las matematicas y eres trabajador" "CLISP"))
)

(defrule Fin_4
  (declare (salience 100))
  (Gusta_mat No)
  (Gusta_hardw Fin)
=>
  (assert (Consejo Ingenieria_de_Computadores "no te gustan las matematicas" "CLISP"))
)

(defrule Fin_5
  (declare (salience 100))
  (Gusta_mat No)
  (Gusta_hardw Si)
  (Trabajar Fin)
=>
  (assert (Consejo Ingenieria_de_Computadores "no te gustan las matematicas pero te gusta el hardware" "CLISP"))
)
