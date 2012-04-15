"パーサが複文に対応していないため式の終わりのセミコロンふたつ必須。しょぼいのう。あとコメントは行頭のセミコロンのみ" ;;
; 下手に空行を入れるとばぐりますよ ;;
; いくつかの関数はプリミティブ関数化してある。waveはそのままだと大変遅い ;;
; Data constructor/selector ;;
"data structure: vector" ;;
(define make-vect cons) ;;
(define xcor-vect car) ;;
(define ycor-vect cdr) ;;
;(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))) ;;
;(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))) ;;
;(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v)))) ;;
"data structure: frame" ;;
(define make-frame list) ;;
(define origin-frame car) ;;
(define edge1-frame cadr) ;;
(define edge2-frame caddr) ;;
"data structure: segment" ;;
(define make-segment cons) ;;
(define start-segment car) ;;
(define end-segment cdr) ;;
;painter/frame operator ;;
;(define (frame-coord-map frame)
  ;(lambda (v) (add-vect
				;(origin-frame frame)
				;(add-vect
				  ;(scale-vect (xcor-vect v) (edge1-frame frame))
				  ;(scale-vect (ycor-vect v) (edge2-frame frame)))))) ;;
(define (segments->painter segment-list)
  (lambda (frame)
	(let ((fcm (frame-coord-map frame)))
	  (for-each
		(lambda (segment)
		  (draw-line
			(fcm (start-segment segment))
			(fcm (end-segment segment))))
		segment-list)))) ;;
; painter filter ;;
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
	(let ((m (frame-coord-map frame)))
	  (let ((new-origin (m origin)))
		(painter
		  (make-frame new-origin
					  (sub-vect (m corner1) new-origin)
					  (sub-vect (m corner2) new-origin))))))) ;;
(define flip-vert (lambda (painter)
  (transform-painter painter
					 (make-vect 0.0 1.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0)))) ;;
(define flip-horiz (lambda (painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 1.0)))) ;;
(define (scale-painter painter scale)
  (transform-painter painter
					 (make-vect 0.0 0.0)
					 (make-vect scale 0.0)
					 (make-vect 0.0 scale))) ;;
(define (identity painter) painter) ;;
(define (rotate90 painter)
  (transform-painter painter
					 (make-vect 1.0 0.0)
					 (make-vect 1.0 1.0)
					 (make-vect 0.0 0.0))) ;;
(define (rotate180 painter)
  (transform-painter painter
					 (make-vect 0.0 1.0)
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 1.0))) ;;
(define (squash-inwards painter)
  (transform-painter painter
					 (make-vect 0.0 0.0)
					 (make-vect 0.65 0.35)
					 (make-vect 0.35 0.65))) ;;
(define (red-painter painter)
  (colored-painter painter (rgb 255 0 0))) ;;
(define (green-painter painter)
  (colored-painter painter (rgb 0 255 0))) ;;
(define (blue-painter painter)
  (colored-painter painter (rgb 0 0 255))) ;;
(define (multi-colored-painter painter)
  (let ((r (red-painter painter))
        (g (green-painter painter))
        (b (blue-painter painter))
		(counter 0))
	(lambda (frame)
	  (if (= counter 0)
		(begin
		  (r frame)
		  (set! counter 1))
		(if (= counter 1)
		  (begin
			(g frame)
			(set! counter 2))
		  (if (= counter 2)
			(begin
			  (b frame)
			  (set! counter 0))
			0)))))) ;;
; painter mapper ;;
(define (beside l r)
  (lambda (frame)
	((transform-painter l
						(make-vect 0.0 0.0)
						(make-vect 0.5 0.0)
						(make-vect 0.0 1.0))
	 frame)
	((transform-painter r
						(make-vect 0.5 0.0)
						(make-vect 1.0 0.0)
						(make-vect 0.5 1.0))
	 frame))) ;;
(define (below bottom top)
  (lambda (frame)
	((transform-painter top
						(make-vect 0.0 0.5)
						(make-vect 1.0 0.5)
						(make-vect 0.0 1.0))
	 frame)
	((transform-painter bottom
						(make-vect 0.0 0.0)
						(make-vect 1.0 0.0)
						(make-vect 0.0 0.5))
	 frame))) ;;
(define (square-of-four tl tr bl br)
  (lambda (painter)
	(let ((top (beside (tl painter) (tr painter)))
		  (bottom (beside (bl painter) (br painter))))
	  (below bottom top)))) ;;
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
	(below painter2 painter2))) ;;
(define (split a b)
  (let ((f 0))
	(set! f (lambda (painter n)
			 (if (= n 0)
			   painter
			   (let ((smaller (f painter (- n 1))))
				 (a painter (b smaller smaller))))))
	f)) ;;
(define right-split (split beside below)) ;;
(define up-split (split below beside)) ;;
(define (corner-split painter n)
  (if (= n 0)
	painter
	(let ((up (up-split painter (- n 1)))
		  (right (right-split  painter (- n 1))))
	  (let ((top-left (beside up up))
			(bottom-right (below right right))
			(corner (corner-split painter (- n 1))))
		(beside (below painter top-left)
				(below bottom-right corner)))))) ;;
; basic painter ;;
;(define wave
  ;(let ((p01 (make-vect 0.40 1.00))
        ;(p02 (make-vect 0.60 1.00))
        ;(p03 (make-vect 0.00 0.80))
        ;(p04 (make-vect 0.35 0.80))
        ;(p05 (make-vect 0.65 0.80))
        ;(p06 (make-vect 0.00 0.60))
        ;(p07 (make-vect 0.30 0.60))
        ;(p08 (make-vect 0.40 0.60))
        ;(p09 (make-vect 0.60 0.60))
        ;(p10 (make-vect 0.70 0.60))
        ;(p11 (make-vect 0.20 0.55))
        ;(p12 (make-vect 0.30 0.55))
        ;(p13 (make-vect 0.35 0.50))
        ;(p14 (make-vect 0.65 0.50))
        ;(p15 (make-vect 0.20 0.45))
        ;(p16 (make-vect 1.00 0.40))
        ;(p17 (make-vect 0.50 0.20))
        ;(p18 (make-vect 1.00 0.20))
        ;(p19 (make-vect 0.25 0.00))
        ;(p20 (make-vect 0.40 0.00))
        ;(p21 (make-vect 0.60 0.00))
        ;(p22 (make-vect 0.75 0.00))
        ;(p23 (make-vect 0.40 0.75))
        ;(p24 (make-vect 0.60 0.75))
        ;(p25 (make-vect 0.50 0.70)))
    ;(segments->painter
     ;(list (make-segment p01 p04)
           ;(make-segment p04 p08)
           ;(make-segment p08 p07)
           ;(make-segment p07 p11)
           ;(make-segment p11 p03)
           ;(make-segment p06 p15)
           ;(make-segment p15 p12)
           ;(make-segment p12 p13)
           ;(make-segment p13 p19)
           ;(make-segment p20 p17)
           ;(make-segment p17 p21)
           ;(make-segment p22 p14)
           ;(make-segment p14 p18)
           ;(make-segment p16 p10)
           ;(make-segment p10 p09)
           ;(make-segment p09 p05)
           ;(make-segment p05 p02)
           ;(make-segment p25 p23)
           ;(make-segment p25 p24))))) ;;
(define (wave frame) (wave-impl (current-canvas-context) frame)) ;;
(define triangle
  (segments->painter
	(list (make-segment (make-vect 0.1 0.1) (make-vect 0.9 0.1))
		  (make-segment (make-vect 0.9 0.1) (make-vect 0.5 0.9))
		  (make-segment (make-vect 0.5 0.9) (make-vect 0.1 0.1))))) ;;
; composed painters ;;
(define wave2 (beside wave (flip-vert wave))) ;;
(define wave4 (below wave2 wave2)) ;;
; primitives(device dependent) ;;
(define (draw painter) (painter (base-frame))) ;;
(define base-frame 
  (lambda ()
	(make-frame
	  (make-vect 0 (canvas-height))
	  (make-vect (canvas-width) 0)
	  (make-vect 0 (- (canvas-height)))))) ;;
(define draw-line
  (lambda (start end)
	(begin-path)
	(move-to (xcor-vect start) (ycor-vect start))
	(line-to (xcor-vect end) (ycor-vect end))
	(stroke))) ;;
(define (colored-painter painter color)
  (lambda (frame)
	(push-canvas-state)
	(stroke-style color)
	(fill-style color)
	(painter frame)
	(pop-canvas-state))) ;;
(define bind-canvas-method (lambda (name) (lambda () (js-apply (current-canvas-context) name ())))) ;;
(define bind-canvas-prop-setter (lambda (name) (lambda (value) (js-set-prop (current-canvas-context) name value)))) ;;
(define bind-canvas-prop-getter (lambda (name) (lambda () (js-get-prop (current-canvas-context) name)))) ;;
(define apply-canvas-context (lambda (method args) (js-apply (current-canvas-context) method args))) ;;
(define (clear-canvas) (js-apply (current-canvas-context) "clearRect" (list 0 0 (canvas-width) (canvas-height)))) ;;
(define clr clear-canvas) ;;
(define canvas-width (lambda () (js-get-prop (current-canvas) "width"))) ;;
(define canvas-height (lambda () (js-get-prop (current-canvas) "height"))) ;;
(define fill-style (bind-canvas-prop-setter "fillStyle")) ;;
(define get-fill-style (bind-canvas-prop-getter "fillStyle")) ;;
(define stroke-style (bind-canvas-prop-setter "strokeStyle")) ;;
(define get-fill-style (bind-canvas-prop-getter "strokeStyle")) ;;
(define rgb (lambda (r g b) (append-string "rgb(" r "," g "," b ")"))) ;;
(define rgba (lambda (r g b a) (append-string "rgb(" r "," g "," b "," a ")"))) ;;
(define fill-rect (lambda (x y w h) (apply-canvas-context "fillRect" (list x y w h)))) ;;
(define move-to (lambda (x y) (apply-canvas-context "moveTo" (list x y)))) ;;
(define line-to (lambda (x y) (apply-canvas-context "lineTo" (list x y)))) ;;
(define arc (lambda (x y radius start-angle end-angle anticlockwise) (apply-canvas-context "arc" (list x y radius start-angle end-angle anticlockwise)))) ;;
(define begin-path (bind-canvas-method "beginPath")) ;;
(define fill (bind-canvas-method "fill")) ;;
(define stroke (bind-canvas-method "stroke")) ;;
(define push-canvas-state (bind-canvas-method "save")) ;;
(define pop-canvas-state (bind-canvas-method "restore")) ;;
"dummy(だせえ……。Scheme.parse('')に失敗するのだ)"
