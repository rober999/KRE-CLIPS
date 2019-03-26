(defglobal ?*patient-id* = 1) ; // counter to generate patients IDs

;;; Declaracion de templates --------------------------

;;; Template para los medicamentos tomados por el paciente
(deftemplate Medicamentos
	(slot thiopental (type SYMBOL) (allowed-values yes no) (default no))
	(slot last_adrenaline_shot (type INTEGER) (default -1))
	(slot 0_negative_blood_units (type INTEGER) (default 0))
	(slot tranaxemid_acid (type SYMBOL) (allowed-values yes no) (default no))
	(slot fibrinogen (type SYMBOL) (allowed-values yes no) (default no))
)

;;; Template para los tratamientos recibidos por el paciente
(deftemplate Tratamientos
	(slot tourniquet (type SYMBOL) (allowed-values yes no) (default no))
	(slot REBOA (type SYMBOL) (allowed-values yes no) (default no))
	(slot thoracotomy (type SYMBOL) (allowed-values yes no) (default no))
	(slot last_hypotension_check (type INTEGER) (default -1))
	(slot MTP_applied (type SYMBOL) (allowed-values yes no) (default no))
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

(defrule Pacientes::inicializar-historiales "Inicializamos las listas de Medicamentos y Tratamientos"
	(declare (salience 11))
	=>
	(assert (Medicamentos (thiopental no) (last_adrenaline_shot -1) (0_negative_blood_units 0) (tranaxemid_acid no) (fibrinogen no)))
	(assert (Medicamentos (thiopental yes) (last_adrenaline_shot -1) (0_negative_blood_units 0) (tranaxemid_acid no) (fibrinogen no)))
	(assert (Medicamentos (thiopental no) (last_adrenaline_shot -1) (0_negative_blood_units 3) (tranaxemid_acid no) (fibrinogen no)))
	(assert (Medicamentos (thiopental no) (last_adrenaline_shot 5) (0_negative_blood_units 0) (tranaxemid_acid no) (fibrinogen no)))
	(assert (Medicamentos (thiopental no) (last_adrenaline_shot -1) (0_negative_blood_units 0) (tranaxemid_acid yes) (fibrinogen no)))
	
	(assert (Tratamientos (tourniquet no) (REBOA no) (thoracotomy no) (last_hypotension_check -1) (MTP_applied no) (ALS_start_time -1) (ALS_stop_time -1)))
	(assert (Tratamientos (tourniquet yes) (REBOA no) (thoracotomy no) (last_hypotension_check -1) (MTP_applied no) (ALS_start_time -1) (ALS_stop_time -1)))
	(assert (Tratamientos (tourniquet no) (REBOA yes) (thoracotomy no) (last_hypotension_check -1) (MTP_applied no) (ALS_start_time -1) (ALS_stop_time -1)))
	(assert (Tratamientos (tourniquet no) (REBOA no) (thoracotomy no) (last_hypotension_check 3) (MTP_applied yes) (ALS_start_time -1) (ALS_stop_time -1)))
	(assert (Tratamientos (tourniquet no) (REBOA no) (thoracotomy no) (last_hypotension_check -1) (MTP_applied no) (ALS_start_time 5) (ALS_stop_time -1)))
)

(defrule Pacientes::inicializar-pacientes "Inicializamos pacientes en el sistema"
	(declare (salience 10))
	?med <- (Medicamentos)
	?tra <- (Tratamientos)
	=>
	(assert (Paciente (id ?*patient-id*) (Medicamentos ?med) (Tratamientos ?tra)))
	(bind ?*patient-id* (+ 1 ?*patient-id*))
	(retract ?med)
	(retract ?tra)
)

(defrule Pacientes::encabezado-pacientes "Mostrar el encabezado de la lista de pacientes"
	(declare (salience 5))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                         Patient list:                          =" crlf)
  	(printout t crlf)
)

(defrule Pacientes::listar-pacientes "Mostramos la lista de pacientes"
	(Paciente (id ?id) (Medicamentos ?med) (Tratamientos ?tra))
	
	;(Medicamentos (thiopental ?med1) (last_adrenaline_shot ?med2) (0_negative_blood_units ?med3) ;(tranaxemid_acid ?med4) (fibrinogen ?med5) ?med) 
	;(Tratamientos (tourniquet ?tra1) (REBOA ?tra2) (thoracotomy ?tra3) (last_hypotension_check ?tra4) (MTP_applied ?tra5) (ALS_start_time ?tra6) (ALS_stop_time ?tra7) ?tra)
	=>
	(printout t "Paciente: " ?id crlf)
	;(printout t "Test: " ?med crlf)
	(printout t "Medicamentos: " ?med crlf)
	;(bind ?test (send ?med get-thiopental))
	;(printout t "Test: " ?med:thiopental crlf)
	;(send ?med print-all-slots)
	
	;(printout t "  - Thiopental: " (nth$ 1 ?med) crlf)
	;(printout t "  - Time since last adrenaline shot: " ?med2 crlf)
	;(printout t "  - 0 negative blood units administered: " ?med3 crlf)
	;(printout t "  - Tranaxemid Acid: " ?med4 crlf)
	;(printout t "  - Fibrinogen: " ?med5 crlf)
	(printout t "Tratamientos: " ?tra crlf)
	;(printout t "  - Tourniquet: " ?tra1 crlf)
	;(printout t "  - REBOA: " ?tra2 crlf)
	;(printout t "  - Thoracotomy: " ?tra3 crlf)
	;(printout t "  - Time since sast Hypotension check: " ?tra4 crlf)
	;(printout t "  - MTP Applied: " ?tra5 crlf)
	;(printout t "  - Time since ALS started: " ?tra6 crlf)
	;(printout t "  - Time since ALS stopped: " ?tra7 crlf)
	(printout t crlf)
)

(defrule Pacientes::pasar-a-tratamientos "Pasamos a elegir tratamientos"
	(declare (salience -10))
	=>
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






















