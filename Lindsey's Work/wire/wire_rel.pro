!P.multi = [0,1,5]
data_rel_to_main_target = 1

for i=0,4 do begin

; wx = where(wire4.mag(i) gt 0. and wire4.mag(i) lt 20.,c)

  wx = where(wire4.mag(0) gt 0. and wire4.mag(0) lt 20. and $
             wire4.mag(i) gt 0. and wire4.mag(i) lt 20. ,c) 

 noise = robust_sigma(wire4(wx).mag(i))

   dat   = wire4(wx).mag(i)
   dat0  = wire4(wx).mag(0)

   if (data_rel_to_main_target eq 1) and (i ne 0) then $
      dat = dat / wire4(wx).mag(0)

   tt   = wire4(wx).hjd - t0

   resistant_mean,dat,3,me,sd,nr
   dat = dat - me

; -------------------------------------------------------------------
   per1 = 15.009 ; 009 ; wire_per(i) / fac ; period for wire craft
   per2 = per1
   per2 = per1*0.8
; -------------------------------------------------------------------

   pha1 =  (tt mod (1./per1)) * per1
   pha2 =  (tt mod (1./per2)) * per2

   wx = where( (pha1 ge -.510 and pha1 lt -0.22) or $
               (pha1 ge  .495 and pha1 lt  0.79),cx)

     plot,pha2,dat,psym=3,yr=[-6.*noise,6*noise]*0.1,ysty=1,xr=[-.55,1.05],xsty=1
     oplot,pha2(wx),dat(wx),psym=3,col=col.red
   
   dat = dat(wx)
   tt = tt(wx)

endfor

end
