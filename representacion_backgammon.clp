(deftemplate tablero
    (multislot mapeo (type INTEGER))
    (slot id (type INTEGER))
)

(deftemplate turno
    (slot jugador (type INTEGER))
)

(deftemplate movimientoObligatorio
	(slot mov(type INTEGER))
)

(deffunction tirarDado ()
    (bind ?d1 (random 1 6))
    (return ?d1)
)

(deffunction askPosicion(?IA ?j ?dados $?tab)
    (printout t crlf "Introduzca la posicion de la ficha a mover: " crlf)
    (if (or (!= 1 ?IA) (!= 1 ?j)) then
        (bind ?pos (read))
        (bind $?list)
        (if (= ?pos 30) then
            (foreach ?pos $?list
                (bind $?list)
            )
        )
    else
        (bind ?pos (nth$ 1 (getBestMove 3 $?dados $?tab)))
        (bind $?list)
        (if (= ?pos 30) then
            (foreach ?pos $?list
                (bind $?list)
            )
        )
        (printout t crlf "La IA elige la posicion: " ?pos crlf)
    )
    (return ?pos)
)

(deffunction askMovimientos(?IA ?j ?dados $?tab)
    (printout t crlf "Introduzca los movimientos a realizar (0 para volver): " crlf)
    (if (or (!= 1 ?IA) (!= 1 ?j)) then
        (bind ?movs (read))
    else
        (bind ?movs (nth$ 2 (getBestMove 3 $?dados $?tab)))
        (printout t crlf "La IA elige los movimientos: " ?movs crlf)
    )
    (return ?movs)
)

(deffunction askMovimientosComida(?IA ?pos ?j ?dados $?tab)
    (printout t crlf "Necesita mover la ficha comida. Por favor, introduzca el movimiento a realizar: " crlf)
    (if (or (!= 1 ?IA) (!= 1 ?j)) then
        (bind ?movs (read))
    else
        (bind ?movs (getBestMoveComidas ?pos 3 ?dados $?tab))
        (printout t crlf "La IA elige los movimientos: " ?movs crlf)
    )
    (return ?movs)
)

(deffunction askPosicionCorrecta(?IA ?j ?dados $?tab)
    (printout t crlf "La posicion debe ser valida: " crlf)
    (if (or (!= 1 ?IA) (!= 1 ?j)) then
        (bind ?pos (read))
    else
        (bind ?pos (nth$ 1 (getBestMove 3 ?dados $?tab)))
        (printout t crlf "La IA elige la posicion: " ?pos crlf)
    )
    (return ?pos)
)

(deffunction askDadosCorrecta()
    (printout t crlf "No puedes mover a esa posicion, vuelve a intentarlo" crlf)
    (bind ?mov (read))
    (return ?mov)
)

(deffunction hayComidas(?j $?tab)
	(if (= 1 ?j) then
		(if (= 0 (nth$ 25 $?tab)) then
			(return FALSE)
		else
			(return TRUE)
		)
	else
		(if (= 0 (nth$ 27 $?tab)) then
			(return FALSE)
		else
			(return TRUE)
		)
	) 
)

(deffunction checkPosicion(?pos ?j $?tab)
	(if(and (> ?pos 0) (< ?pos 25)) then
		(if (= ?j 1) then
			(if (> (nth ?pos $?tab) 0) then
				(return TRUE)
			else
				(return FALSE)
			)
		else
			(if (< (nth ?pos $?tab) 0) then
				(return TRUE)
			else
				(return FALSE)
			)
		)
	)
	(return FALSE)
)

(deffunction checkDadosNoDobles(?mov $?dados)
	(if (= ?mov 0) then
		(return TRUE)
	)
	(if (= (length$ $?dados) 1) then
		(bind ?d1 (nth 1 $?dados))
		(if (= ?mov ?d1) then 
			(return TRUE)
		else 
			(return FALSE)
		)
	else 
		(bind ?d1 (nth 1 $?dados))
		(bind ?d2 (nth 2 $?dados))
		(if ( or (= ?mov ?d1) (= ?mov ?d2) (= ?mov (+ ?d1 ?d2))) then 
			(return TRUE)
		else 
			(return FALSE)
		)
	) 
)

(deffunction checkDadosDobles(?mov $?dados)
	(if (= ?mov 0) then
		(return TRUE)
	)
	(bind ?length (length$ $?dados))
	(bind ?d (nth 1 $?dados))
	(if (and (= (mod ?mov ?d) 0) (<= ?mov (* ?length ?d))) then
			(return TRUE)
		else 
			(return FALSE)
	)
)

(deffunction buscarMovimientosPosiblesSegunDados ($?dados)
	(bind ?d1 (nth$ 1 $?dados))
	(if(= (length$ $?dados) 1) then
		(bind $?list (create$ ?d1))
	else 
		(bind ?d2 (nth$ 2 $?dados))
		(if(=(length$ $?dados) 2) then
			(bind $?list (create$ ?d1 ?d2 (+ ?d1 ?d2)))
		else 
            (if(=(length$ $?dados) 3) then
                (bind $?list (create$ ?d1 (* ?d1 2) (* ?d1 3)))
            else 
                (bind $?list (create$ ?d1 (* ?d1 2) (* ?d1 3) (* ?d1 4)))
            )
			
		)
	)
	(return $?list)
)

(deffunction buscarPosiciones (?j $?tab)
	(bind $?list (create$))
	(if (= 1 ?j) then 
		(loop-for-count (?pos 1 24)
			(if (< 0 (nth$ ?pos $?tab)) then
				(bind $?list (create$ $?list ?pos))
			)
		)
	else
		(loop-for-count (?pos 1 24)
			(if (> 0 (nth$ ?pos $?tab)) then
				(bind $?list (create$ $?list ?pos))
			)
		)
	)
	(return $?list)
)

(deffunction checkAllHome (?j $?tab)
	(bind $?posiciones (buscarPosiciones ?j $?tab))
	(if (= ?j 1) then
		(foreach ?pos $?posiciones
			(if (< ?pos 19) then
				(return FALSE)
			)
		)
	else
		(foreach ?pos $?posiciones
			(if (> ?pos 6) then
				(return FALSE)
			)
		)
	)
	(return TRUE)
)

(deffunction checkMoveSinComidas (?pos ?movs ?j $?tab)
	(if (= ?j 1) then
		(bind ?newPos (+ ?pos ?movs))
		(if (> ?newPos 24) then
			(if (checkAllHome ?j $?tab) then
				(return TRUE)
			)
			(return FALSE)
		else
			(if (>= (nth ?newPos $?tab) -1) then
				(return TRUE)
			else
				(return FALSE)
			)
		)		
	else
		(if (< (- ?pos ?movs) 1) then
			(if (checkAllHome ?j $?tab) then
				(return TRUE)
			)
			(return FALSE)
		else
			(if (< (nth (- ?pos ?movs) $?tab) 2) then
				(return TRUE)
			else
				(return FALSE)
			)
		)
	)
)

(deffunction hayMovimientosEnPosicion (?j ?pos ?dados $?tab)
	(bind $?movimientos (buscarMovimientosPosiblesSegunDados $?dados))
	(foreach ?mov $?movimientos
		(if (checkMoveSinComidas ?pos ?mov ?j $?tab) then
			(return TRUE)
		)
	)
	(return FALSE)
)

(deffunction hayMovimientos (?j ?dados $?tab)
	(bind $?posiciones (buscarPosiciones ?j $?tab))
	(if (and (hayComidas ?j $?tab) (= ?j 1)) then
		(bind $?posiciones (create$ $?posiciones 0))
	)
	(if (and (hayComidas ?j $?tab) (= ?j 2)) then
		(bind $?posiciones (create$ $?posiciones 25))
	)
	(foreach ?pos $?posiciones
		(if (hayMovimientosEnPosicion ?j ?pos ?dados $?tab) then
			(return TRUE)
		)
	)
	(return FALSE)
)

(deffunction actualizarTablero (?pos ?mov ?j $?tab) ;25: comidas blanca, 26:casa blanca, 27:comidas negra, 28: casa negra
	(printout t crlf "Actualizando tablero..." crlf)
	(bind $?newTab $?tab)

	(if (= ?j 1) then
		(bind ?newpos (+ ?pos ?mov))
		(if (>= ?newpos 25) then ;Se saca del tablero
			(bind $?newTab (replace$ $?tab ?pos ?pos (- (nth$ ?pos $?tab) 1)))
			(bind $?newTab (replace$ $?newTab 26 26 (+ (nth$ 26 $?tab) 1)))
		else 
			(if (= (nth$ ?newpos $?tab) -1) then ;comida
				(if (!= ?pos 0) then ;miramos que no salga de casa, para no restar en pos 0
					(bind $?newTab (replace$ $?tab ?pos ?pos (- (nth$ ?pos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos 1))
					(bind $?newTab (replace$ $?newTab 27 27 (+ (nth$ 27 $?tab) 1)))
				else 
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos 1))
					(bind $?newTab (replace$ $?newTab 27 27 (+ (nth$ 27 $?tab) 1)))
					(bind $?newTab (replace$ $?newTab 25 25 (- (nth$ 25 $?tab) 1)))
				)
			else 
				(if (= ?pos 0) then ;miramos si sale de casa, para no restar en pos 0
					(bind $?newTab (replace$ $?tab ?newpos ?newpos (+ (nth$ ?newpos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab 25 25 (- (nth$ 25 $?tab) 1)))
				else ;movimiento
					(bind $?newTab (replace$ $?tab ?pos ?pos (- (nth$ ?pos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos (+ (nth$ ?newpos $?tab) 1)))
				)
			)
		)
	else 
		(bind ?newpos (- ?pos ?mov))
		(if (<= ?newpos 0) then ;Se saca del tablero
			(bind $?newTab (replace$ $?tab ?pos ?pos (+ (nth$ ?pos $?tab) 1)))
			(bind $?newTab (replace$ $?newTab 28 28 (+ (nth$ 28 $?tab) 1)))
		else 
			(if (= (nth$ ?newpos $?tab) 1) then ;comida
				(if (!= ?pos 25) then ;miramos que no salga de casa, para no restar en pos 25
					(bind $?newTab (replace$ $?tab ?pos ?pos (+ (nth$ ?pos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos -1))
					(bind $?newTab (replace$ $?newTab 25 25 (+ (nth$ 25 $?tab) 1)))
				else 
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos -1))
					(bind $?newTab (replace$ $?newTab 25 25 (+ (nth$ 25 $?tab) 1)))
					(bind $?newTab (replace$ $?newTab 27 27 (- (nth$ 27 $?tab) 1)))
				)
			else 
				(if (= ?pos 25) then ;miramos si sale de casa, para no restar en pos 25
					(bind $?newTab (replace$ $?tab ?newpos ?newpos (- (nth$ ?newpos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab 27 27 (- (nth$ 27 $?tab) 1)))
				else ;movimiento
					(bind $?newTab (replace$ $?tab ?pos ?pos (+ (nth$ ?pos $?tab) 1)))
					(bind $?newTab (replace$ $?newTab ?newpos ?newpos (- (nth$ ?newpos $?tab) 1)))
				)
			)
		)
	)
	(return $?newTab)
)


(deffunction imprimir-mapeo ($?mapeo)
    (if (eq (length$ $?mapeo) 0) then 
        (do-for-fact ((?tablero tablero)) (eq ?tablero:id 0)
            (bind $?mapeo ?tablero:mapeo)
        )
    )
    (printout t crlf "   ")
    (loop-for-count (?i 13 18)
        (printout t "   " ?i)
    )
    (printout t "     ")
	(loop-for-count (?i 19 24)
        (printout t "   " ?i)
    )
	(printout t crlf)

	(printout t "     ")
    (loop-for-count 13
        (printout t "__ ")
	)
	
    (printout t crlf)
   
	(printout t "    ")
	(loop-for-count 6
		(printout t "|    ")
	)
	(printout t "||||")
	(printout t "|")
	(loop-for-count 6
		(printout t "|    ")
	)
	(printout t "|")
	(printout t crlf)
	(printout t "    | ")
	(loop-for-count (?j 13 18)
		(if (eq (nth$ ?j $?mapeo) 0)
			then (printout t "  ")
		else 
			(if (< (nth$ ?j $?mapeo) 0) then
				(printout t (str-cat N (abs(nth$ ?j $?mapeo))))
			else
				(if (> (nth$ ?j $?mapeo) 0) then
					(printout t (str-cat B (abs(nth$ ?j $?mapeo))))
				)
			)
		)
		(if (neq ?j 18) then
			(printout t " | ")
		)
	)
	(printout t " |||||")
	(printout t "| ")
	(loop-for-count (?j 19 24)
		(if (eq (nth$ ?j $?mapeo) 0)
				then (printout t "  ")
		else 
			(if (< (nth$ ?j $?mapeo) 0) then
				(printout t (str-cat N (abs(nth$ ?j $?mapeo))))
			else
				(if (> (nth$ ?j $?mapeo) 0) then
					(printout t (str-cat B (abs(nth$ ?j $?mapeo))))
				)
			)
		)
		(printout t " | ")
	)
	(printout t crlf)
	(printout t "    ")
	(loop-for-count 6
		(printout t "|__")
	)
	(printout t "||||")
	(printout t "|")
	(loop-for-count 6
		(printout t "|__")
	)
	(printout t "|")
	(printout t crlf)

	
	(printout t "     ")
	(loop-for-count 13
		(printout t "__ ")
	)
	(printout t crlf)
	(printout t "     ")
	(loop-for-count 13
		(printout t "__ ")
	)
	(printout t crlf)
	(printout t "    ")
	(loop-for-count 6
		(printout t "|    ")
	)
	(printout t "||||")
	(printout t "|")
	(loop-for-count 6
		(printout t "|    ")
	)
	(printout t "|")
	(printout t crlf)

	(printout t "    | ")
	(bind ?j 12)
	(while (neq ?j 6)
		(if (eq (nth$ ?j $?mapeo) 0)
			then (printout t "  ")
		else 
			(if (< (nth$ ?j $?mapeo) 0) then
				(printout t (str-cat N (abs(nth$ ?j $?mapeo))))
			else
				(if (> (nth$ ?j $?mapeo) 0) then
					(printout t (str-cat B (abs(nth$ ?j $?mapeo))))
				)
			)
		)
		(bind ?j (- ?j 1))
		(if (neq ?j 6) then
			(printout t " | ")
		)
		
	)
	(printout t " |||||")
	(printout t "| ")
	(bind ?j 6)
	(while (neq ?j 0)
		(if (eq (nth$ ?j $?mapeo) 0)
				then (printout t "  ")
		else 
			(if (< (nth$ ?j $?mapeo) 0) then
				(printout t (str-cat N (abs(nth$ ?j $?mapeo))))
			else
				(if (> (nth$ ?j $?mapeo) 0) then
					(printout t (str-cat B (abs(nth$ ?j $?mapeo))))
				)
			)
		)
		(printout t " | ")
		(bind ?j (- ?j 1))
	)
	
	(printout t crlf)
	(printout t "    ")
	(loop-for-count 6
		(printout t "|__")
	)
	(printout t "||||")
	(printout t "|")
	(loop-for-count 6
		(printout t "|__")
	)
	(printout t "|")
	(printout t crlf)
	(printout t crlf "   ")
	
	(bind ?i 12)
	(while (neq ?i 6)
		(if (< ?i 10) then
			(printout t "    " ?i)
		else
			(printout t "   " ?i)
		)
		(bind ?i (- ?i 1))
	)
	(printout t "     ")
	(bind ?i 6)
	(while (neq ?i 0)
		(printout t "    " ?i)
		(bind ?i (- ?i 1))
	)
	(printout t crlf)
	(printout t crlf)
	(printout t crlf)

	(printout t "Comidas blancas: " (nth$ 25 $?mapeo) crlf)
	(printout t "En casa blancas: " (nth$ 26 $?mapeo) crlf)
	(printout t "Comidas negras:  " (nth$ 27 $?mapeo) crlf)
	(printout t "En casa negras:  " (nth$ 28 $?mapeo) crlf)

	(printout t crlf)
	(printout t crlf)
	(printout t crlf)
)


(deffacts inicializacion "Estado inicial del tablero"
    (tablero (mapeo (create$ 2 0 0 0 0 -5 0 -3 0 0 0 5 -5 0 0 0 3 0 5 0 0 0 0 -2 0 0 0 0)) (id 0))
    (turno (jugador 1))
)

(defrule elegirContrincante
    (declare (salience 10000))
    =>
    (printout t "BIENVENIDO AL JUEGO BACKGAMMON. ¿CONTRA QUIEN QUIERES JUGAR?" crlf "1. Inteligencia Artificial" crlf "2. Otro jugador" crlf "3. IA vs IA (dados no aleatorios)" crlf)
    (bind ?jugador (read))
    (if (= ?jugador 1) then
        (assert (IA 1))
		(assert (IAvsIA 0))
    else
		(if (= ?jugador 3) then
			(assert (IA 1))
			(assert (IAvsIA 1))
		else 
			(assert (IA 0))
			(assert (IAvsIA 0))
		)
    )
)

(defrule infoInicioRonda 
    (tablero (mapeo $?mapeo) (id ?id))
	(turno (jugador ?j))
	(IAvsIA ?IAvsIA)
    =>
    (bind ?tirada1 (tirarDado))
    (bind ?tirada2 (tirarDado))
    (assert (dados (create$ ?tirada1 ?tirada2)))
    (imprimir-mapeo $?mapeo)
	(if (= ?j 1) then
		(printout t "-----JUEGAN BLANCAS-----" crlf)
	else 
		(printout t "-----JUEGAN NEGRAS-----" crlf)
	)
	(if (or (!= 1 ?IAvsIA) (!= ?j 2)) then
		(printout t "La tirada de dados ha salido: " ?tirada1 " y "?tirada2 crlf)
	)
)

(defrule jugada1
    (turno (jugador ?j))
    ?dadosFact <- (dados $?dados)
    ?tablero <- (tablero (mapeo $?tab))
    (IA ?IA)
	(IAvsIA ?IAvsIA)
    =>
	(if (and (= 1 ?IAvsIA) (= ?j 2)) then
		(printout t "Introduce los dos dados (el primero <enter> el segundo): " crlf)
		(bind ?d1 (read))
		(bind ?d2 (read))
		(bind $?dados (create$ ?d1 ?d2))
	)
	(if (= (nth 1 $?dados) (nth 2 $?dados)) then
		(bind ?d (nth 1 $?dados))
		(assert (dadosDobles (create$ ?d ?d ?d ?d)))
        (retract ?dadosFact)
	else 
		(if (hayMovimientos ?j $?dados $?tab) then
			(bind ?mov 0)
			(while (= ?mov 0)
				(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 0 $?dados $?tab) (= ?j 1)) then 
					(bind ?pos 0)
					(printout t "Debes mover una ficha comida." crlf)
                    (bind ?mov (askMovimientosComida ?IA ?pos ?j $?dados $?tab))
				else 
					(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 25 $?dados $?tab) (= ?j 2)) then
						(bind ?pos 25)
						(printout t "Debes mover una ficha comida." crlf)
					else 
						(bind ?pos (askPosicion ?IA ?j $?dados $?tab))
						(while (not (checkPosicion ?pos ?j $?tab))
							(bind ?pos (askPosicionCorrecta ?IA ?j $?dados $?tab))
						)
					)
                    (bind ?mov (askMovimientos ?IA ?j ?dados $?tab))
				)
				
				(while (or (not (checkDadosNoDobles ?mov $?dados)) (not (checkMoveSinComidas ?pos ?mov ?j $?tab)))
					(bind ?mov (askDadosCorrecta))
				) 
			)

			(bind $?tabNew (actualizarTablero ?pos ?mov ?j $?tab))
			(retract ?tablero)
			(assert (tablero (mapeo $?tabNew)))
			(if (= ?mov (+ (nth 1 $?dados) (nth 2 $?dados))) then
				(retract ?dadosFact)
				(assert (cambio si))
			else
				(imprimir-mapeo $?tabNew)
				(if (= ?mov (nth 1 $?dados)) then
					(printout t "Dado restante: " (nth 2 $?dados) crlf)
					(assert (movimientoObligatorio (mov (nth 2 $?dados))))
					(retract ?dadosFact)
				else
					(printout t "Dado restante: " (nth 1 $?dados) crlf)
					(assert (movimientoObligatorio (mov (nth 1 $?dados))))
					(retract ?dadosFact)
				)
			)
		else 
			(printout t "No hay movimientos posibles. Se pasa el turno." crlf)
            (assert (cambio si))
		)
	)
)

(defrule jugada2
	(turno (jugador ?j))
	?movObl <- (movimientoObligatorio(mov ?mov))
	?tablero <- (tablero (mapeo $?tab))
    (IA ?IA)
	=>
	(if (hayMovimientos ?j ?mov $?tab) then
		(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 0 ?mov $?tab) (= ?j 1)) then 
				(bind ?pos 0)
				(printout t "Debes mover una ficha comida." crlf)
		else 
			(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 25 ?mov $?tab) (= ?j 2)) then
				(bind ?pos 25)
				(printout t "Debes mover una ficha comida." crlf)
			else 
				(bind ?pos (askPosicion ?IA ?j ?mov $?tab))
				(while (or (not (checkPosicion ?pos ?j $?tab)) (not (checkMoveSinComidas ?pos ?mov ?j $?tab)))
					(printout t "No puedes mover a esa posición." crlf)
					(bind ?pos (askPosicionCorrecta ?IA ?j ?mov $?tab))
				)
			)
		)

		(retract ?movObl)

		(bind $?tabNew (actualizarTablero ?pos ?mov ?j $?tab))
		(retract ?tablero)
		(assert (tablero (mapeo $?tabNew)))
		(assert (cambio si))
	else
		(printout t "No hay movimientos posibles. Se pasa el turno." crlf)
        (assert (cambio si))
	)
)

(defrule jugadaDobles
	(turno (jugador ?j))
	?dd <- (dadosDobles $?dados)
	?tablero <- (tablero (mapeo $?tab))
    (IA ?IA)

	=>
	(if (hayMovimientos ?j $?dados $?tab) then
		(bind ?mov 0)
		(while (= ?mov 0)
			(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 0 $?dados $?tab) (= ?j 1)) then 
				(bind ?pos 0)
				(printout t "Debes mover una ficha comida." crlf)
                (bind ?mov (askMovimientosComida ?IA ?pos ?j $?dados $?tab))
			else 
				(if (and (hayComidas ?j $?tab) (hayMovimientosEnPosicion ?j 25 $?dados $?tab) (= ?j 2)) then
					(bind ?pos 25)
					(printout t "Debes mover una ficha comida." crlf)
				else 
					(bind ?pos (askPosicion ?IA ?j $?dados $?tab))
					(while (not (checkPosicion ?pos ?j $?tab))
						(bind ?pos (askPosicionCorrecta ?IA ?j $?dados $?tab))
					)
				)
                (bind ?mov (askMovimientos ?IA ?j $?dados $?tab))
			)
			
			(while (or (not (checkDadosDobles ?mov $?dados)) (not (checkMoveSinComidas ?pos ?mov ?j $?tab)))
				(bind ?mov (askDadosCorrecta))
			) 
		)
		(bind ?d (nth 1 $?dados))
		(bind $?tabNew (actualizarTablero ?pos ?mov ?j $?tab))
		(retract ?tablero)
		(assert (tablero (mapeo $?tabNew)))
		(bind ?dadosUsados (/ ?mov ?d))
		(bind ?inicio (+ 1 (- (length$ $?dados) ?dadosUsados)))
		(bind ?final (length$ $?dados))
		(bind $?newDados (delete$ $?dados ?inicio ?final))
		(retract ?dd)
		(if(!= 0 (length$ $?newDados)) then
			(imprimir-mapeo $?tabNew)
			(assert (dadosDobles $?newDados))
			(printout t "Dados restantes: " $?newDados crlf)
		else
			(retract ?dd)
			(assert (cambio si))
		)
    else
        (printout t "No hay movimientos posibles. Se pasa el turno." crlf)
        (assert (cambio si))
	)
)

(defrule cambioTurno
	?jug <- (turno (jugador ?j))
	?cambio <- (cambio si)
	(tablero (mapeo $?tab))
	=>
	(if (= ?j 1) then
		(retract ?jug)
		(retract ?cambio)
		(assert (turno (jugador 2)))
	else
		(retract ?jug)
		(retract ?cambio)
		(assert (turno (jugador 1)))
	)
)

(defrule ganado
	(declare (salience 1000))
	?tablero <- (tablero (mapeo $?tab))
	=>
	(if (= (nth$ 26 $?tab) 15) then
		(printout t "HAN GANADO BLANCAS, ¡ENHORABUENA!" crlf)
		(assert (ganado blancas))
        (retract ?tablero)
	)
	(if (= (nth$ 28 $?tab) 15) then
		(printout t "HAN GANADO NEGRAS, ¡ENHORABUENA!" crlf)
		(assert (ganado negras))
        (retract ?tablero)
	)
)

(defrule fin
    (ganado ?j)
    =>
    (printout t "Fin del juego." crlf)
    (printout t "¿Quieres volver a jugar? (si/no)" crlf)
    (bind ?respuesta (read))
    (if (eq ?respuesta si) then
        (reset)
        (run)
    else
        (halt)
    )
)