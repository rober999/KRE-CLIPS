(defglobal ?*patient-id* = 1) ; // counter to generate patients IDs

;;; Declaracion de templates --------------------------

;;; Template para los medicamentos tomados por el paciente
(deftemplate Drugs
	(slot patient-id (type INTEGER))
	(slot thiopental (type SYMBOL) (allowed-values yes no) (default no))
	(slot last_adrenaline_shot (type INTEGER) (default -1))
	(slot 0_negative_blood_units (type INTEGER) (default 0))
	(slot tranexamic_acid (type SYMBOL) (allowed-values yes no) (default no))
	(slot fibrinogen (type SYMBOL) (allowed-values yes no) (default no))
	(slot first_0_negative_blood_unit (type INTEGER) (default -1))
)

;;; Template para los tratamientos recibidos por el paciente
(deftemplate Treatments
	(slot patient-id (type INTEGER))
	(slot last_hypotension_check (type INTEGER) (default -1))
	(slot ALS_started (type SYMBOL) (allowed-values yes no) (default no))
	(slot MTP_started (type SYMBOL) (allowed-values yes no) (default no))
	(slot thoracotomy_applied (type INTEGER) (default -1))
)

;;; Template para la ficha de paciente
(deftemplate Patient
	(slot id (type INTEGER))
	(slot name (type SYMBOL))
)

;;; Declaracion de funciones --------------------------

;;; Funcion para hacer una pregunta con respuesta cualquiera
(deffunction pregunta-general (?pregunta)
    (format t "%s " ?pregunta)
	(bind ?respuesta (read))
	(while (not (lexemep ?respuesta)) do
		(format t "%s " ?pregunta)
		(bind ?respuesta (read))
    )
	?respuesta
)

;;; Funcion para hacer una pregunta con respuesta numerica unica
(deffunction pregunta-numerica (?pregunta ?rangini ?rangfi)
	(format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
	(bind ?respuesta (read))
	(while (not(and(>= ?respuesta ?rangini)(<= ?respuesta ?rangfi))) do
		(format t "%s [%d, %d] " ?pregunta ?rangini ?rangfi)
		(bind ?respuesta (read))
	)
	?respuesta
)


;;; Funcion para hacer una pregunta general con una serie de respuestas admitidas
(deffunction MAIN::pregunta-opciones (?question $?allowed-values)
   (format t "%s "?question)
   (progn$ (?curr-value $?allowed-values)
		(format t "[%s]" ?curr-value)
	)
   (printout t ": ")
   (bind ?answer (read))
   (if (lexemep ?answer) 
       then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed-values)) do
      (format t "%s "?question)
	  (progn$ (?curr-value $?allowed-values)
		(format t "[%s]" ?curr-value)
	  )
	  (printout t ": ")
      (bind ?answer (read))
      (if (lexemep ?answer) 
          then (bind ?answer (lowcase ?answer))))
   ?answer
)
   
;;; Funcion para hacer una pregunta de tipo si/no
(deffunction pregunta-si-no (?question)
   (bind ?response (pregunta-opciones ?question y n))
   (if (or (eq ?response yes) (eq ?response y))
       then yes 
       else no)
)

;;; Declaracion de modulos ----------------------------

;;;-------------------------------------------------
;;;--------------------MAIN-------------------------
;;;-------------------------------------------------
(defmodule MAIN (export ?ALL))

;;; Modulo de inicializacion de pacientes
(defmodule Patients
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Modulo del menu de tratamientos
(defmodule Menu
	(import Patients ?ALL)
	(export ?ALL)
)

;;; Modulo de tratado de pacientes
(defmodule Treatments
	(import Menu ?ALL)
	(export ?ALL)
)

;;; Modulo de mostrado de avisos
(defmodule Alerts
	(import MAIN ?ALL)
	(import Patients ?ALL)
	(import Treatments ?ALL)
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
	(focus Patients)
	(assert (mas-pacientes))
)

;;;-------------------------------------------------
;;;------------------PACIENTES----------------------
;;;-------------------------------------------------

(defrule Patients::insertar-paciente "Pregunta al usuario la informacion sobre un paciente"
	(declare (salience 10))
	?hecho <- (mas-pacientes)
	=>
	(bind ?nombre (pregunta-general "What is the patient's name?"))
	(bind ?thiopental (pregunta-si-no "Have you administered Thiopental to the patient?"))
	(bind ?adrenaline (pregunta-numerica "How long ago did you last administer adrenaline to the patient (in minutes) (-1 if not administered)?" -1 100))
	(bind ?blood_units (pregunta-numerica "How many zero negative blood units did you administer to the patient?" 0 100))
	(if (> ?blood_units 0) then
		(bind ?first_blood (pregunta-numerica "How long ago did you first administer a zero negative blood unit to the patient (in minutes)?" 0 100))
		else
		(bind ?first_blood -1)
	)
	(bind ?tranexamic (pregunta-si-no "Have you administered Tranexamic Acid to the patient?"))
	(bind ?fibrinogen (pregunta-si-no "Have you administered Fibrinogen to the patient?"))
	
	(bind ?hypotension (pregunta-numerica "How long ago did you last check the patient's hypotension (in minutes) (-1 if not checked)?" -1 100))
	(bind ?als (pregunta-si-no "Have you started ALS on the patient?"))
	(bind ?mtp (pregunta-si-no "Have you started MTP on the patient?"))
	(bind ?thoracotomy (pregunta-numerica "How long ago did you applied a tourniquet, REBOA or thoracotomy technique to the patient (in minutes) (-1 if not applied)?" -1 100))
	
	(bind ?next (pregunta-si-no "Thank you for your patience. Do you want to keep introducing patients?"))
	
	;;;;;;;;;;; Fin preguntas
	
	(assert (Patient (id ?*patient-id*) (name ?nombre)))
	(assert (Drugs (patient-id ?*patient-id*) (thiopental ?thiopental) (last_adrenaline_shot ?adrenaline) (0_negative_blood_units ?blood_units) (tranexamic_acid ?tranexamic) (fibrinogen ?fibrinogen) (first_0_negative_blood_unit ?first_blood)))
	(assert (Treatments (patient-id ?*patient-id*) (last_hypotension_check ?hypotension) (ALS_started ?als) (MTP_started ?mtp) (thoracotomy_applied ?thoracotomy)))
	(bind ?*patient-id* (+ 1 ?*patient-id*))
	(retract ?hecho)
	(if (eq ?next yes) then
		(assert (mas-pacientes))
	)
)

(defrule Patients::encabezado-pacientes "Mostrar el encabezado de la lista de pacientes"
	(declare (salience 5))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                          Patient list:                           =" crlf)
  	(printout t crlf)
)

(defrule Patients::listar-pacientes "Mostramos la lista de pacientes"
	(Patient (id ?id) (name ?name))
	(Drugs (patient-id ?id) (thiopental ?med1) (last_adrenaline_shot ?med2) (0_negative_blood_units ?med3) (tranexamic_acid ?med4) (fibrinogen ?med5) (first_0_negative_blood_unit ?med6)) 
	(Treatments (patient-id ?id) (last_hypotension_check ?tra1) (ALS_started ?tra2) (MTP_started ?tra3)  (thoracotomy_applied ?tra4))
	=>
	(printout t "Patient: " ?name crlf)
	(printout t "Patient ID: " ?id crlf)
	(printout t "Drugs: " crlf)
	(printout t "  - Thiopental administered: " ?med1 crlf)
	(printout t "  - Time since last adrenaline shot: " ?med2 crlf)
	(printout t "  - Zero negative blood units administered: " ?med3 crlf)
	(printout t "  - Tranexamic Acid administered: " ?med4 crlf)
	(printout t "  - Fibrinogen administered: " ?med5 crlf)
	(printout t "  - Time since first zero negative blood unit administered: " ?med6 crlf)
	(printout t "Treatments: " crlf)
	(printout t "  - Time since sast Hypotension check: " ?tra1 crlf)
	(printout t "  - ALS started: " ?tra2 crlf)
	(printout t "  - MTP started: " ?tra3 crlf)
	(printout t "  - Time since tourniquet, REBOA or thoracotomy technique was applied: " ?tra4 crlf)
	(printout t crlf)
)

(defrule Patients::pasar-a-tratamientos "Pasamos a elegir tratamientos"
	(declare (salience -10))
	=>
	(focus Menu)
)

;;;-------------------------------------------------
;;;---------------------MENU------------------------
;;;-------------------------------------------------

(defrule Menu::elegir_tratamiento "Elegimos el siguiente tratamiento a ejecutar"
	(not (aplicar_tratamiento))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=                         Treatment list:                          =" crlf)
  	(printout t crlf)  	
	(printout t " 1 - Administer thiopental" crlf)
	(printout t " 2 - Administer 1mg of adrenaline" crlf)
	(printout t " 3 - Administer 0 negative blood unit" crlf)
	(printout t " 4 - Administer tranexamic acid" crlf)
	(printout t " 5 - Administer fibrinogen" crlf)
	(printout t " 6 - Apply tourniquet" crlf)
	(printout t " 7 - Apply REBOA" crlf)
	(printout t " 8 - Apply thoracotomy technique" crlf)
	(printout t " 9 - Apply MTP" crlf)
	(printout t "10 - Start ALS" crlf)
	(printout t "11 - Stop ALS" crlf)
	(printout t "12 - Check patient hypotension" crlf)
	(printout t "13 - Wait 1 minute" crlf)
	(printout t "14 - (TBD)Show current system alerts" crlf)
	(printout t "15 - (TBD)Show list of patients" crlf)
	(printout t "16 - Stop system" crlf)
	(printout t crlf)
	(bind ?siguiente_tratamiento (pregunta-numerica "Next treatment?" 1 16))
	(if (< ?siguiente_tratamiento 13) 
		then 
		(assert (tratamiento ?siguiente_tratamiento))
		(assert (aplicar_tratamiento))
		else
		(focus Treatments)
		(assert (actualizar_pacientes (- ?*patient-id* 1)))
		(assert (tratamiento ?siguiente_tratamiento))
	)
)

(defrule Menu::elegir_paciente "Elegimos el siguiente paciente a tratar"
	?g <- (aplicar_tratamiento)
	?t <- (tratamiento ?siguiente_tratamiento)
	=>
	(bind ?siguiente_paciente (pregunta-numerica "To which patient should we apply the selected treatment?" 1 (- ?*patient-id* 1)))
	(retract ?g)
	(retract ?t)
	(focus Treatments)
	(assert (tratamiento ?siguiente_tratamiento))
	(assert (paciente ?siguiente_paciente))
)

(defrule Menu::show_patients "Mostramos la lista de pacientes"
	?g <- (tratamiento 14)
	=>
	(retract ?g)
	(focus Alerts)
)

(defrule Menu::show_patients "Mostramos la lista de pacientes"
	?g <- (tratamiento 15)
	=>
	(retract ?g)
	(focus Patients)
)

(defrule Menu::stop_execution "Paramos el sistema"
	?g <- (tratamiento 16)
	=>
	(retract ?g)
	(halt)
)


;;;-------------------------------------------------
;;;-----------------TRATAMIENTOS--------------------
;;;-------------------------------------------------

(defrule Treatments::tratamiento1 "Administer thiopental"
	?g <- (tratamiento 1)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?m <- (Drugs (patient-id ?id))
	=>
	(printout t "Thiopental administered to " ?name " (Patient ID: " ?id ")" crlf)
	(modify ?m (thiopental yes))
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?m (thiopental yes))
	(focus Alerts)
)

(defrule Treatments::tratamiento2 "Administer 1mg of adrenaline"
	?g <- (tratamiento 2)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?m <- (Drugs (patient-id ?id))
	=>
	(printout t "1mg of adrenaline administered to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?m (last_adrenaline_shot 0))
	(focus Alerts)
)

(defrule Treatments::tratamiento3 "Administer 0 negative blood unit"
	?g <- (tratamiento 3)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?m <- (Drugs (patient-id ?id)(0_negative_blood_units ?b)(first_0_negative_blood_unit ?f))
	=>
	(printout t "0 negative blood unit administered to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(if (< ?f 0) 
		then 
		(focus Menu)
		(modify ?m (first_0_negative_blood_unit 0))
	)
	(focus Alerts)
)

(defrule Treatments::tratamiento4 "Administer tranexamic acid"
	?g <- (tratamiento 4)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?m <- (Drugs (patient-id ?id))
	=>
	(printout t "Tranaxemic acid administered to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?m (tranexamic_acid yes))
	(focus Alerts)
)

(defrule Treatments::tratamiento5 "Administer fibrinogen"
	?g <- (tratamiento 5)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?m <- (Drugs (patient-id ?id))
	=>
	(printout t "Fibrinogen administered to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?m (fibrinogen yes))
	(focus Alerts)
)

(defrule Treatments::tratamiento6 "Apply tourniquet"
	?g <- (tratamiento 6)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "Tourniquet applied to " ?name " (Patient ID: " ?id ")" crlf)	
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (thoracotomy_applied 0))
	(focus Alerts)
)

(defrule Treatments::tratamiento7 "Apply REBOA"
	?g <- (tratamiento 7)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "REBOA applied to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (thoracotomy_applied 0))
	(focus Alerts)
)

(defrule Treatments::tratamiento8 "Apply thoracotomy technique"
	?g <- (tratamiento 8)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "Thoracotomy technique applied to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (thoracotomy_applied 0))
	(focus Alerts)
)

(defrule Treatments::tratamiento9 "Apply MTP"
	?g <- (tratamiento 9)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "MTP applied to " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (MTP_started yes))
	(focus Alerts)
)

(defrule Treatments::tratamiento10 "Start ALS"
	?g <- (tratamiento 10)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "ALS started for " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (ALS_started yes))
	(focus Alerts)
)

(defrule Treatments::tratamiento11 "Stop ALS"
	?g <- (tratamiento 11)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "ALS stopped for patient " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (ALS_started no))
	(focus Alerts)
)

(defrule Treatments::tratamiento12 "Check patient hypotension"
	?g <- (tratamiento 12)
	?p <- (paciente ?id)
	?pa <- (Patient (id ?id) (name ?name))
	?t <- (Treatments (patient-id ?id))
	=>
	(printout t "Hypotension checked for patient " ?name " (Patient ID: " ?id ")" crlf)
	(retract ?g)
	(retract ?p)
	(focus Menu)
	(modify ?t (last_hypotension_check 0))
	(focus Alerts)
)

(defrule Treatments::tratamiento13 "Wait 1 minute"
	?g <- (tratamiento 13)
	?act <- (actualizar_pacientes ?id)
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(last_adrenaline_shot ?a)(first_0_negative_blood_unit ?b))
	?t <- (Treatments (patient-id ?id)(thoracotomy_applied ?th))
	=>
	(printout t ?name " (Patient ID: " ?id ") has waited for 1 minute." crlf)
	(focus Menu)
	(if (> ?a -1) 
    	then (modify ?m (last_adrenaline_shot (+ ?a 1)))
	)
	(if (> ?b -1) 
    	then (modify ?m (first_0_negative_blood_unit (+ ?b 1)))
	)
	(if (> ?th -1) 
    	then (modify ?t (thoracotomy_applied (+ ?th 1)))
	)
	(focus Treatments)
	(retract ?act)
	(if (> ?id 1) 
    	then (assert (actualizar_pacientes (- ?id 1)))
		else (assert (fin_espera))
	)
)

(defrule Treatments::fin_esperar_minuto "Finish Waiting"
	?g <- (tratamiento 13)
	?f <- (fin_espera)
	=>
	(retract ?g)
	(retract ?f)
	(focus Alerts)
)

;;;-------------------------------------------------
;;;--------------------ALERTA-----------------------
;;;-------------------------------------------------

;;; K1: Whenever thiopental is administered, the clinician must pay attention to patient's hypotension.
(defrule Alerts::K1 "Rule K1"
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(thiopental yes))
	=>
	(printout t "ALERT K1. Apply Treatment number 12: Check hypotension for " ?name " (Patient ID: " ?id ")" crlf)
)	

;;; K2: When advanced life support (ALS) is started, the clinician has to administer 1 mg of adrenaline every 3 minutes.
(defrule Alerts::K2 "Rule K2"
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(last_adrenaline_shot ?adr))
	?t <- (Treatments (patient-id ?id)(ALS_started yes))
	(test (>= ?adr 3))
	=>
	(printout t "ALERT K2. Apply Treatment number 2: Administer 1 mg of adrenaline every 3 minutes to " ?name " (Patient ID: " ?id ")" crlf)
)

;;; K3: When a zero negative blood unit is administered three times and the patient has received tranexamic acid and fibrinogen, the clinician has to start a massive transfusion protocol (MTP).
(defrule Alerts::K3 "Rule K3"
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(0_negative_blood_units ?znegative)(tranexamic_acid yes)(fibrinogen yes))
	(test (>= ?znegative 3))
	=>
	(printout t "ALERT K3. Apply Treatment number 9: Apply a Massive Transfusion Protocol to " ?name " (Patient ID: " ?id ")" crlf)
)

;;; K4: If zero negative blood is administered at time T but at time T + 5 minutes fibrinogen or tranexamic acid are not yet administered, then the missing drugs have to be administered.
(defrule Alerts::K4_1 "Rule K4 tranexamic acid"
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(first_0_negative_blood_unit ?fznegative)(tranexamic_acid no))
	(test (>= ?fznegative 5))
	=>
	(printout t "ALERT K4. Apply Treatment number 4: Administer tranexamic acid to " ?name " (Patient ID: " ?id ")" crlf)
)	

;;; K4: If zero negative blood is administered at time T but at time T + 5 minutes fibrinogen or tranexamic acid are not yet administered, then the missing drugs have to be administered.
(defrule Alerts::K4_2 "Rule K4 fibrinogen"
	?p <- (Patient (id ?id)(name ?name))
	?m <- (Drugs (patient-id ?id)(first_0_negative_blood_unit ?fznegative)(fibrinogen no))
	(test (>= ?fznegative 5))
	=>
	(printout t "ALERT K4. Apply Treatment number 5: Administer fibrinogen to " ?name " (Patient ID: " ?id ")" crlf)
)			

;;; K5: When a tourniquet, REBOA, or thoracotomy technique is applied, 
;;; the clinician must be notified every 15 minutes about the time passed since the application.
(defrule Alerts::K5 "Rule K5"
	?p <- (Patient (id ?id)(name ?name))
	?t <- (Treatments (patient-id ?id)(thoracotomy_applied ?th))
	(test (eq ?th 15))
	=>
	(printout t "ALERT K5. 15 minutes have passed since the application of the thoracotomy technique to " ?name " (Patient ID: " ?id ")" crlf)
)


; RULES
; xx K1: Whenever thiopental is administered, the clinician must pay attention to patient's hypotension.
; xx K2: When advanced life support (ALS) is started, the clinician has to administer 1 mg of adrenaline every 3 minutes.
; xx K3: When a zero negative blood unit is administered three times and the patient has received tranexamic acid and fibrinogen, the clinician has to start a massive transfusion protocol (MTP).
; xx K4: If zero negative blood is administered at time T but at time T + 5 minutes fibrinogen or tranexamic acid are not yet administered, then the missing drugs have to be administered1.
; xx K5: When a tourniquet, REBOA, or thoracotomy technique is applied, the clinician must be notified every 15 minutes about the time passed since the application.

