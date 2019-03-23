;;; Declaracion de templates --------------------------

;;; Template para los datos socio-demograficos del usuario
(deftemplate MAIN::Grupo
	(slot numero_asistentes (type INTEGER) (default -1))
	(slot tipo_evento (type SYMBOL) (default desconocido))
	(slot min_precio (type INTEGER) (default -1))
	(slot max_precio (type INTEGER) (default -1))
	(slot epoca_estacion (type SYMBOL) (default desconocido))
)

;;; Template para las preferencias del grupo
(deftemplate MAIN::preferencias
	(multislot alimentos_prohibidos (type INSTANCE))
	(slot estilo_regional (type SYMBOL) (default desconocido))
	(slot estilo_favorito (type SYMBOL) (default desconocido))
	(slot quiere_vino (type SYMBOL) (default desconocido))
	(slot vino_por_plato (type SYMBOL) (default desconocido))
)

;;; Template para las preferencias del grupo
(deftemplate MAIN::lista_platos
	(multislot platos (type INSTANCE))
)

(deftemplate recomendacion
        (slot Platos)
        (slot prioridad)
)

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
   (while (not (member ?answer ?allowed-values)) do
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
(deffunction MAIN::pregunta-si-no (?question)
   (bind ?response (pregunta-opciones ?question si no))
   (if (or (eq ?response si) (eq ?response s))
       then TRUE 
       else FALSE)
)

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

;;; Funcion para hacer pregunta con indice de respuestas posibles
(deffunction MAIN::pregunta-indice (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (bind ?respuesta (pregunta-numerica "Escoge una opci鏮:" 1 (length$ ?valores-posibles)))
	?respuesta
)

;;; Funcion para hacer una pregunta multi-respuesta con indices
(deffunction pregunta-multi (?pregunta $?valores-posibles)
    (bind ?linea (format nil "%s" ?pregunta))
    (printout t ?linea crlf)
    (progn$ (?var ?valores-posibles) 
            (bind ?linea (format nil "  %d. %s" ?var-index ?var))
            (printout t ?linea crlf)
    )
    (format t "%s" "Indica los neros separados por un espacio: ")
    (bind ?resp (readline))
    (bind ?numeros (str-explode ?resp))
    (bind $?lista (create$ ))
    (progn$ (?var ?numeros) 
        (if (and (integerp ?var) (and (>= ?var 1) (<= ?var (length$ ?valores-posibles))))
            then 
                (if (not (member$ ?var ?lista))
                    then (bind ?lista (insert$ ?lista (+ (length$ ?lista) 1) ?var))
                )
        ) 
    )
    ?lista
)


(deffunction incrementa-prioridad-recomendacion (?recomendacion ?inc)
(modify ?recomendacion (prioridad (+ (fact-slot-value ?recomendacion prioridad) ?inc))))


(deffunction incrementa-prioridad-platos (?slot ?valor ?inc)
  (progn$ (?f (get-fact-list)) 
    (if (eq (fact-relation ?f) recomendacion) then 
      (bind ?Plato (fact-slot-value ?f Platos))
      (if (eq (send ?Plato (sym-cat get- ?slot)) ?valor) then 
(incrementa-prioridad-recomendacion ?f ?inc)))))

;;; Fin declaracion funciones--------------------------
;;; Declaracion de modulos ----------------------------

;;;--------------------MAIN-------------------------
;;;-------- 				------------
;;;-------------------------------------------------
(defmodule MAIN (export ?ALL))

;;; Modulo de recopilacion de los datos del grupo
(defmodule recopilacion-grupo
	(import MAIN ?ALL)
	(export ?ALL)
)

;;; Modulo de filtrado y procesado del contenido adequado al usuario
(defmodule procesado
	(import MAIN ?ALL)
	(import recopilacion-grupo deftemplate ?ALL)
	(export ?ALL)
)

;;; Declaracion de reglas y hechos ---------------------

(defrule MAIN::init-recomendaciones
 	(declare (salience 15))
	 ?Platos <- (object (is-a Platos))
	 =>
	(assert (recomendacion (Platos ?Platos) (prioridad 0)))
)

(defrule MAIN::initialRule "Regla inicial"
	(declare (salience 10))
	=>
	(printout t "====================================================================" crlf)
  	(printout t "=         Sistema de recomendacion de platos Rico Rico          =" crlf)
	(printout t "====================================================================" crlf)
  	(printout t crlf)  	
	(printout t "、ienvenido al sistema Rico Rico! A continuaci鏮 se le formular嫕 una serie de preguntas para poder recomendarle sus platos favoritos." crlf)
	(printout t crlf)
	(focus recopilacion-grupo)
)

;;; Modulo recopilacion

(defrule recopilacion-grupo::establecer-nombre "Establece el numero de asistentes, es la primera pregunta"
	(not (Grupo))
	=>
	(bind ?numero_asistentes (pregunta-numerica "燒umero de asistentes?" 1 500))
	(assert (Grupo (numero_asistentes ?numero_asistentes)))
)

(defrule recopilacion-grupo::establecer-evento "Establece el que tipo de evento es congreso o familiar"
	?g <- (Grupo (tipo_evento desconocido))
	=>
	(bind ?s (pregunta-opciones "澧omida de congreso o familiar?" congreso familiar))
	(modify ?g (tipo_evento ?s)) 
)

(defrule recopilacion-grupo::establecer-epoca "Establece en que epoca del a隳 se realiza el pedido"
	?g <- (Grupo (epoca_estacion desconocido))
	=>
	(bind ?s (pregunta-opciones "激n que epoca de anyo desearia usted acudir a nuestro establecimiento?" invierno primavera verano otonyo))
	(modify ?g (epoca_estacion ?s))
)

(defrule recopilacion-grupo::establecer-minprecio "激stablece el presupuesto minimo del grupo?"
	?g <- (Grupo (min_precio ?min_precio))
	(test (< ?min_precio 0))
	=>
	(bind ?e (pregunta-numerica "澠ntroduce el precio minimo que esta dispuesto a gastar por menu?" 0 10000))
	(modify ?g (min_precio ?e))
	
)

(defrule recopilacion-grupo::establecer-maxprecio "激stablece el presupuesto maximo del grupo?"
	?g <- (Grupo (max_precio ?max_precio))
	(test (< ?max_precio 0))
	=>
	(bind ?e (pregunta-numerica "澠ntroduce el precio maximo que esta dispuesto a gastar por menu?" 0 10000))
	(modify ?g (max_precio ?e))
	
)

(defrule recopilacion-grupo::preguntar-preferencias "Pasa a la recopilacion de preferencias"
	(declare (salience 10))
	?g <- (Grupo (numero_asistentes ?n) (min_precio ?min) (max_precio ?max) (tipo_evento ~desconocido) (epoca_estacion ~desconocido))
	(test (> ?n 0))
	(test (> ?min 0))
	(test (> ?max 0))
	=>
	;(assert (alimentos_prohibidos ask))
	(assert (estilo_regional ask))
	(assert (estilo_favorito ask))
	(assert (quiere_vino ask))
	(assert (preferencias))
)




(defrule recopilacion-grupo::establecer-estilo-regional "Establece la region preferida del grupo"
	?hecho <- (estilo_regional ask)
	?g <- (preferencias)
	=>
	(bind ?r (pregunta-si-no "熹uiere comida de una region en concreto?"))
	(if (eq ?r TRUE) then
		(bind ?s (pregunta-opciones "熹ue region prefiere?" japonesa mejicana china india tropical mediterrania usa turca))
		(modify ?g (estilo_regional ?s)) 
	)	
	(retract ?hecho)
)

(defrule recopilacion-grupo::establecer-estilo-favorito "Establece el estilo favorito del grupo"
	?g <- (preferencias (estilo_favorito desconocido))
	?hecho <- (estilo_favorito ask)
	=>
	(bind ?response (pregunta-opciones "熹u� estilo se ajusta mas a los gustos del grupo?" clasico moderno sibarita))

	(if (eq ?response clasico) 
    	 	then (incrementa-prioridad-platos estilo clasico 100))
  	(if (eq ?response moderno) 
    	 	then (incrementa-prioridad-platos estilo moderno 100))
  	(if (eq ?response sibarita) 
    		then (incrementa-prioridad-platos estilo sibarita 100))
	(modify ?g (estilo_favorito ?response)) 
	(retract ?hecho)
)

(defrule recopilacion-grupo::establecer-quiere-vino "Establece si el grupo quiere tomar vino"
	?g <- (preferencias (quiere_vino desconocido))
	?hecho <- (quiere_vino ask)
	=>
	(bind ?r (pregunta-si-no "熹uieren tomar vino con su comida?"))
	(if (eq ?r TRUE) then
		(assert (vino_por_plato ask))
	)
	(modify ?g (quiere_vino ?r))
	(retract ?hecho)
)

(defrule recopilacion-grupo::establecer-vino-por-plato "Establece si el grupo quiere tomar una copa de vino por plato"
	?g <- (preferencias (vino_por_plato desconocido))
	?hecho <- (vino_por_plato ask)
	=>
	(bind ?r (pregunta-si-no "熹uieren tomar una copa de vino con cada plato?"))
	(modify ?g (vino_por_plato ?r))
	(retract ?hecho)
)

(defrule recopilacion-grupo::pasar-a-procesado "Empezamos a procesar la respuesta"
	(declare (salience -10))
	=>
	(focus procesado)
)


;;; Modulo procesado de contenido ---------------------------------------------------


(defrule procesado::anadir-platos "Se a鎙den todos los platos, luego se filtran"
	(declare (salience 10))
	=>
	(bind $?allPlatos (find-all-instances((?inst Platos)) TRUE))
	(printout t crlf)
	;(format t ", esta es nuestra recomendaci鏮 para usted. ‧speramos que la disfrute!")
	;(format t "%d" (length$ ?allPlatos))
	(printout t crlf)
	(progn$ (?curr-con ?allPlatos)
		(make-instance (gensym) of Platos (nombrePlato ?curr-con) (estilo (send ?curr-con get-estilo)))
		;(format t "Nombre: %s " (send ?curr-con get-nombrePlato))
	)
		
)

(defrule procesado::soy-inutil "ee"
   (declare (salience 1))
   =>
   (printout t crlf)  
   (format t "Le recomendamos el siguiente menu compuesto por :")
   (printout t crlf)     
        )

(defrule procesado::mostrar-prioridad-cien "Prueba"
   (declare (salience 0))
   (recomendacion (Platos ?Platos) (prioridad ?prioridad))
   (test (>= ?prioridad 100))
    =>
    (printout t crlf)
    (format t "Nombre: %s " (send ?Platos get-nombrePlato))
        
    (printout t crlf)
     

        )


(defrule procesado::filtra-estilo "Se quitan los platos que no cumplan con el estilo elegido"
	(preferencias (estilo_favorito ?e))
	?p <- (object (is-a Platos) (estilo ?est))
	(test (not (eq ?est ?e)))
	=>
	(send ?p delete)
)
