;;; Declaracion de templates --------------------------

;;; Template para los medicamentos tomados por el paciente
(deftemplate MAIN::Medicamentos
	(slot thiopental (type SYMBOL) (default no))
	(slot last_adrenaline_shot (type INTEGER) (default -1))
	(slot 0_negative_blood_units (type INTEGER) (default 0))
	(slot tranaxemid_acid (type SYMBOL) (default no))
	(slot fibrinogen (type SYMBOL) (default no))
)

;;; Template para los tratamientos recibidos por el paciente
(deftemplate MAIN::Tratamientos
	(slot tourniquet (type SYMBOL) (default no))
	(slot REBOA (type SYMBOL) (default no))
	(slot thoracotomy (type SYMBOL) (default no))
	(slot last_hypotension_check (type INTEGER) (default -1))
	(slot MTP_applied (type SYMBOL) (default no))
	(slot ALS_start_time (type INTEGER) (default -1))
	(slot ALS_stop_time (type INTEGER) (default -1))
)

;;; Template para la ficha de paciente
(deftemplate Paciente
        (slot id (type INTEGER))
        (slot Medicamentos)
        (slot Tratamientos)
)

;;; Declaracion de funciones --------------------------

;;; Funcion para hacer una pregunta con respuesta numerica unica
(deffunction MAIN::pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
	(bind ?respuesta (read))
	(while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
		(format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
		(bind ?respuesta (read))
	)
	?respuesta
)

;;; Declaracion de modulos ----------------------------

;;;-------------------------------------------------
;;;--------------------MAIN-------------------------
;;;-------------------------------------------------
(defmodule MAIN (export ?ALL))

;;; Modulo de inicializacion de pacientes
(defmodule Pacientes
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Modulo de tratado de pacientes
(defmodule Tratamientos
	(import MAIN ?ALL)
	(import Pacientes deftemplate ?ALL)
	(export ?ALL)
)

;;; Declaracion de reglas y hechos ---------------------

(defrule MAIN::iniciar_sistema "Saludo del sistema"
	(declare (salience 10))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                  Knowledge-Based Emergency Room                  =" crlf)
	(printout t "====================================================================" crlf)
  	(printout t crlf)  	
	(printout t "Welcome to the Emergency Room. The status of the current patients is:" crlf)
	(printout t crlf)
	(focus Pacientes)
)

;;;-------------------------------------------------
;;;------------------PACIENTES----------------------
;;;-------------------------------------------------

(defrule Pacientes::pasar-a-tratamientos "Pasamos a elegir tratamientos"
	(declare (salience -10))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                         Patient list:                          =" crlf)
  	(printout t crlf)
	(printout t "Bla bla bla ......" crlf)
	(printout t crlf)
	(focus Tratamientos)
)

;;;-------------------------------------------------
;;;-----------------TRATAMIENTOS--------------------
;;;-------------------------------------------------

(defrule Tratamientos::elegir_tratamiento "Elegimos el siguiente tratamiento a ejecutar"
	(declare (salience 10))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                         Treatment list:                          =" crlf)
  	(printout t crlf)  	
	(printout t " 1 - Administer thiopental" crlf)
	(printout t " 2 - Administer 1mg of adrenaline" crlf)
	(printout t " 3 - Administer 0 negative blood unit" crlf)
	(printout t " 4 - Administer tranaxemic acid" crlf)
	(printout t " 5 - Administer fibrinogen" crlf)
	(printout t " 6 - Apply tourniquet" crlf)
	(printout t " 7 - Apply REBOA" crlf)
	(printout t " 8 - Apply thoracotomy technique" crlf)
	(printout t " 9 - Apply MTP" crlf)
	(printout t "10 - Start ALS" crlf)
	(printout t "11 - Stop ALS" crlf)
	(printout t "12 - Check patient hypotension" crlf)
	(printout t "13 - Wait 1 minute" crlf)
	(printout t crlf)
	(bind ?siguiente_tratamiento (pregunta-numerica "Next treatment?" 1 13))
	(assert (tratamiento ?siguiente_tratamiento))
	(if (< ?siguiente_tratamiento 13) 
    	 	then (assert (aplicar_tratamiento))
	)

)

(defrule Tratamientos::elegir_paciente "Elegimos el siguiente paciente a tratar"
	(declare (salience 9))
	?g <- (aplicar_tratamiento)
	=>
	(bind ?siguiente_paciente (pregunta-numerica "To which patient should we apply the selected treatment?" 1 50))
	(assert (paciente ?siguiente_paciente))
)

(defrule Tratamientos::K1 "bla"
	(Paciente (id ?p)(Medicamentos (thiopental ?t)) (Tratamientos (> last_hypotension_check 2)))
	=>
	(printout t "K1 Activa - Revisa la hipotensión de " ?p "!" crlf)
	(printout t crlf)
)





















