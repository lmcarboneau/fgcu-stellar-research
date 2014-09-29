; Over plot the region of Pamyatnykh 1999
; Adaped from figure in Peter deCat Ph.D. dissertation

PRO pamyat, x, blue=blue, spb=spb, betacep=betacep, txtout=txtout, cz=cz

default9, blue, 1B
default9, spb, 1B
default9, betacep, 1B
default9, txtout, 0B
default9, cz, 1.0

x_blue = [3.95, 3.90, 3.85, 3.75]
y_blue = [1.1,  1.7 , 2.6,  5.05]
if blue ge .5 then oplot,x_blue,y_blue, thick=blue

x_spb = [4.27, 4.07, 4.03, 4.02, 4.30]
y_spb = [2.90, 1.80, 2.00, 2.25, 3.95]
if spb ge 0.5 then begin
 oplot,x_spb(0:2),y_spb(0:2), line=2, thick=spb
 oplot,x_spb(2:4),y_spb(2:4), line=2, thick=spb
 oplot,[x_spb(0),x_spb(4)],[y_spb(0),y_spb(4)], line=2, thick=spb
endif


x_beta = [4.68, 4.62, 4.57,4.495, 4.45, 4.41,  4.35,4.32, 4.27,4.24, 4.25,4.28,4.282,4.30,4.33,4.32]
y_beta = [6.02, 5.72, 5.4, 5.0,   4.6 , 4.20,  3.42,3.32, 3.23, 3.40, 3.64,3.85,4.2,  4.6, 5.1, 5.63]

if betacep ge 0.5 then begin
 nb = n_elements(x_beta)
 oplot,x_beta(0:8),y_beta(0:8), thick=beta
 oplot,x_beta(8:nb-1),y_beta(8:nb-1), thick=beta
 oplot,[x_beta(0),x_beta(nb-1)],[y_beta(0),y_beta(nb-1)], thick=betacep
endif


if txtout then begin
 xyouts, (x_blue(3)+x_blue(2))*0.5-0.055, (y_blue(3)+y_blue(2))*0.57, 'Classical blue edge, l=0', $
  orientation=78.5,charsi=cz,charthick=cz*.8,alignment=0.5

dy = 0.04 ; offset txt in y-direction
 xyouts, 4.53, 0.5-dy, 'SPB: !17l!3=0 - 2',charsi=cz,charthick=2 
   oplot,[4.62,4.55],[.5,.5],line=2,thick=3
 xyouts, 4.53, 1.0-dy, 'SPB: !17l!3=1 - 2',charsi=cz,charthick=2 
   oplot,[4.62,4.55],[1.,1.],thick=3

 

endif

END
