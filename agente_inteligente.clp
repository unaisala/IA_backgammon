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
			(if (> ?pos 4) then
				(return FALSE)
			)
		)
	)
	(return TRUE)
)

(deffunction fichas_casa_IA ($?tab)
	(bind ?fichas 0)
	(loop-for-count (?pos 1 24)
		(if (>= (nth$ ?pos $?tab) 1) then
			(if (> ?pos 19) then
				(bind ?fichas (+ ?fichas (nth$ ?pos $?tab)))
			)
		)
	)
	(return ?fichas)
)


(deffunction checkMoveSinComidas (?pos ?movs ?j $?tab)
	(if (= ?j 1) then
		(bind ?newPos (+ ?pos ?movs))
        (if (< ?newPos 0) then
            (return FALSE)
        )
		(if (> ?newPos 24) then
			(if (checkAllHome ?j $?tab) then
				(return TRUE)
			)
			(return FALSE)
		else
			(if (>= (nth$ ?newPos $?tab) -1) then
				(return TRUE)
			else
				(return FALSE)
			)
		)		
	else
		(if (< (- ?pos ?movs) 1) then
			(if (and (checkAllHome ?j $?tab) (= (- ?pos ?movs) 0)) then
				(return TRUE)
			)
			(return FALSE)
		else
			(if (< (nth$ (- ?pos ?movs) $?tab) 2) then
				(return TRUE)
			else
				(return FALSE)
			)
		)
	)
)

(deffunction actualizarTableroIA (?pos ?mov ?j $?tab) ;25: comidas blanca, 26:casa blanca, 27:comidas negra, 28: casa negra
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


;; Metiendo la IA


(deffunction evaluacion ($?tab)
   (bind ?ia_puntuacion 0)
   (bind ?otro_puntuacion 0)
   (bind ?recompensa_comer 1000) ; Recompensa por comer una ficha rival
   (bind ?penalizacion_ficha_suelta 1100) ; Penalización por dejar una ficha suelta
   (bind ?distancia_maxima 18) ; Distancia máxima para premiar acercar fichas a casa
   (bind ?recompensa_acercar 600) ; Recompensa por acercar una ficha a casa
   (bind ?recompensa_mover_casa 2000) ; Recompensa por mover fichas a casa

   ; Calcular puntuaciones de cada jugador en base a las fichas en el tablero
   (loop-for-count (?pos 1 24)
      (bind ?fichas (nth$ ?pos $?tab))
      (if (> ?fichas 0) then
         (bind ?ia_puntuacion (+ ?ia_puntuacion (* ?fichas ?pos)))
      else
         (bind ?otro_puntuacion (+ ?otro_puntuacion (* (- 0 ?fichas) (- 25 ?pos))))
      )
   )

   ; Calcular recompensa por comer una ficha rival
   (bind ?fichas-comidas-ia (nth$ 25 $?tab))
   (bind ?fichas-comidas-otro (nth$ 27 $?tab))
   (bind ?recompensa-comer-otro (* ?fichas-comidas-otro ?recompensa_comer))
   (bind ?recompensa-comer-ia (* ?fichas-comidas-ia ?recompensa_comer))
   (bind ?ia_puntuacion (+ ?ia_puntuacion ?recompensa-comer-otro))
   (bind ?otro_puntuacion (+ ?otro_puntuacion ?recompensa-comer-ia))

   ; Penalizar dejar fichas sueltas más cerca de casa
   (loop-for-count (?pos 1 24)
      (bind ?fichas-sueltas (nth$ ?pos $?tab))
      (if (= ?fichas-sueltas 1) then
         (bind ?penalizacion-posicion (* ?penalizacion_ficha_suelta ?pos))
         (bind ?ia_puntuacion (- ?ia_puntuacion ?penalizacion-posicion))
      )
   )

   ; Premiar acercar fichas a casa
   (bind ?ia_puntuacion (+ ?ia_puntuacion (* (fichas_casa_IA $?tab) ?recompensa_acercar)))

   ; Premiar mover fichas a casa
   (if (checkAllHome 1 $?tab) then
		(bind ?fichas-casa-ia (nth$ 26 $?tab))
		(bind ?recompensa-mover-casa-ia (* ?fichas-casa-ia ?recompensa_mover_casa))
		(bind ?ia_puntuacion (+ ?ia_puntuacion ?recompensa-mover-casa-ia))
   )

   (return (- ?ia_puntuacion ?otro_puntuacion))
)



(deffunction minimax (?prof ?j ?dados $?tab)
    (if (= ?prof 0) then
        (return (evaluacion $?tab))    
    )

    (if (= ?j 1) then ;Maximizando --> IA
        (bind ?max_eval -1000000)

        (foreach ?pos (buscarPosiciones ?j $?tab)
            (foreach ?move (buscarMovimientosPosiblesSegunDados $?dados)
                (bind $?tabAux (actualizarTableroIA ?pos ?move ?j $?tab))
                (bind ?eval (minimax (- ?prof 1) 2 $?dados $?tabAux))
                (bind ?max_eval (max ?max_eval ?eval))
            )
        )

        (return ?max_eval)

    else
        (bind ?min_eval 1000000)

        (foreach ?pos (buscarPosiciones ?j $?tab)
            (foreach ?move (buscarMovimientosPosiblesSegunDados $?dados)
                (bind $?tabAux (actualizarTableroIA ?pos ?move ?j $?tab))
                (bind ?eval (minimax (- ?prof 1) 1 $?dados $?tabAux))
                (bind ?min_eval (min ?min_eval ?eval))
            )
        )

        (return ?min_eval)
    )
)

(deffunction getBestMove (?prof ?dados $?tab)
    (bind ?best_eval -1000000)
    (bind ?best_move 0)
    (bind ?best_pos 0)
    (bind ?j 1)

    (foreach ?pos (buscarPosiciones ?j $?tab)
        (foreach ?move (buscarMovimientosPosiblesSegunDados $?dados)
            (bind $?tabAux (actualizarTableroIA ?pos ?move ?j $?tab))
            (bind ?eval (minimax (- ?prof 1) 2 $?dados $?tabAux))
            
            (if (and (> ?eval ?best_eval) (checkMoveSinComidas ?pos ?move ?j $?tab)) then
                (bind ?best_eval ?eval)
                (bind ?best_move ?move)
                (bind ?best_pos ?pos)
            )
        )
    )

    (return (create$ ?best_pos ?best_move))
)

(deffunction getBestMoveComidas (?pos ?prof ?dados $?tab)
    (bind ?best_eval -1000000)
    (bind ?best_move 0)
    (bind ?j 1)

    (foreach ?move (buscarMovimientosPosiblesSegunDados $?dados)
            (bind $?tabAux (actualizarTableroIA ?pos ?move ?j $?tab))
            (bind ?eval (minimax (- ?prof 1) 2 $?dados $?tabAux))
            
            (if (and (> ?eval ?best_eval) (checkMoveSinComidas ?pos ?move ?j $?tab)) then
                (bind ?best_eval ?eval)
                (bind ?best_move ?move)
            )
    )

    (return ?best_move)
)