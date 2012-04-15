//picture language in SICP 2.2.4 on browser
PictLang={}
PictLang.bindProcs=function(env) {
//(define (add-vect v1 v2) (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)))) ;;
	env.bind('add-vect',function(v1,v2) { return new Cons(Number(v1.car)+Number(v2.car),Number(v1.cdr)+Number(v2.cdr)) })
//(define (sub-vect v1 v2) (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)))) ;;
	env.bind('sub-vect',function(v1,v2) { return new Cons(Number(v1.car)-Number(v2.car),Number(v1.cdr)-Number(v2.cdr)) })
//(define (scale-vect s v) (make-vect (* s (xcor-vect v)) (* s (ycor-vect v)))) ;;
	env.bind('scale-vect',function(s,v) { s=Number(s); return new Cons(Number(v.car)*s,Number(v.cdr)*s) })
//(define (frame-coord-map frame)
  //(lambda (v) (add-vect
				//(origin-frame frame)
				//(add-vect
				  //(scale-vect (xcor-vect v) (edge1-frame frame))
				  //(scale-vect (ycor-vect v) (edge2-frame frame)))))) ;;
	var frame_coord_map=function(frame) {
		var origin_x=Number(frame.car.car)
		var origin_y=Number(frame.car.cdr)
		var e1_x=Number(frame.cdr.car.car)
		var e1_y=Number(frame.cdr.car.cdr)
		var e2_x=Number(frame.cdr.cdr.car.car)
		var e2_y=Number(frame.cdr.cdr.car.cdr)
		return function(v) {
			var x=Number(v.car)
			var y=Number(v.cdr)
			return new Cons(
				origin_x+x*e1_x+y*e2_x ,
				origin_y+x*e1_y+y*e2_y )
		}
	}
	env.bind('frame-coord-map',frame_coord_map);
//(define wave
  //(let ((p01 (make-vect 0.40 1.00))
        //(p02 (make-vect 0.60 1.00))
        //(p03 (make-vect 0.00 0.80))
        //(p04 (make-vect 0.35 0.80))
        //(p05 (make-vect 0.65 0.80))
        //(p06 (make-vect 0.00 0.60))
        //(p07 (make-vect 0.30 0.60))
        //(p08 (make-vect 0.40 0.60))
        //(p09 (make-vect 0.60 0.60))
        //(p10 (make-vect 0.70 0.60))
        //(p11 (make-vect 0.20 0.55))
        //(p12 (make-vect 0.30 0.55))
        //(p13 (make-vect 0.35 0.50))
        //(p14 (make-vect 0.65 0.50))
        //(p15 (make-vect 0.20 0.45))
        //(p16 (make-vect 1.00 0.40))
        //(p17 (make-vect 0.50 0.20))
        //(p18 (make-vect 1.00 0.20))
        //(p19 (make-vect 0.25 0.00))
        //(p20 (make-vect 0.40 0.00))
        //(p21 (make-vect 0.60 0.00))
        //(p22 (make-vect 0.75 0.00))
        //(p23 (make-vect 0.40 0.75))
        //(p24 (make-vect 0.60 0.75))
        //(p25 (make-vect 0.50 0.70)))
    //(segments->painter
     //(list (make-segment p01 p04)
           //(make-segment p04 p08)
           //(make-segment p08 p07)
           //(make-segment p07 p11)
           //(make-segment p11 p03)
           //(make-segment p06 p15)
           //(make-segment p15 p12)
           //(make-segment p12 p13)
           //(make-segment p13 p19)
           //(make-segment p20 p17)
           //(make-segment p17 p21)
           //(make-segment p22 p14)
           //(make-segment p14 p18)
           //(make-segment p16 p10)
           //(make-segment p10 p09)
           //(make-segment p09 p05)
           //(make-segment p05 p02)
           //(make-segment p25 p23)
           //(make-segment p25 p24))))) ;;
	var line=function(canvas,p1,p2) {
		canvas.moveTo(p1[0],p1[1])
		canvas.lineTo(p2[0],p2[1])
	}
	env.bind('wave-impl',function(canvas,frame) {
		var origin_x=Number(frame.car.car)
		var origin_y=Number(frame.car.cdr)
		var e1_x=Number(frame.cdr.car.car)
		var e1_y=Number(frame.cdr.car.cdr)
		var e2_x=Number(frame.cdr.cdr.car.car)
		var e2_y=Number(frame.cdr.cdr.car.cdr)
		var fcm=function(c) {
			return [
				origin_x+c[0]*e1_x+c[1]*e2_x ,
				origin_y+c[0]*e1_y+c[1]*e2_y ]
		}
		var p=[
		 [0.00,0.00], //padding
		 [0.40,1.00],
		 [0.60,1.00],
		 [0.00,0.80],
		 [0.35,0.80],
		 [0.65,0.80],
		 [0.00,0.60],
		 [0.30,0.60],
		 [0.40,0.60],
		 [0.60,0.60],
		 [0.70,0.60],
		 [0.20,0.55],
		 [0.30,0.55],
		 [0.35,0.50],
		 [0.65,0.50],
		 [0.20,0.45],
		 [1.00,0.40],
		 [0.50,0.20],
		 [1.00,0.20],
		 [0.25,0.00],
		 [0.40,0.00],
		 [0.60,0.00],
		 [0.75,0.00],
		 [0.40,0.75],
		 [0.60,0.75],
		 [0.50,0.70]].map(fcm);
		canvas.beginPath()
		line(canvas,p[01],p[04])
		line(canvas,p[04],p[08])
		line(canvas,p[08],p[07])
		line(canvas,p[07],p[11])
		line(canvas,p[11],p[03])
		line(canvas,p[06],p[15])
		line(canvas,p[15],p[12])
		line(canvas,p[12],p[13])
		line(canvas,p[13],p[19])
		line(canvas,p[20],p[17])
		line(canvas,p[17],p[21])
		line(canvas,p[22],p[14])
		line(canvas,p[14],p[18])
		line(canvas,p[16],p[10])
		line(canvas,p[10],p[09])
		line(canvas,p[09],p[05])
		line(canvas,p[05],p[02])
		//line(canvas,p[25],p[23])
		//line(canvas,p[25],p[24])
		canvas.stroke()
	})
}
