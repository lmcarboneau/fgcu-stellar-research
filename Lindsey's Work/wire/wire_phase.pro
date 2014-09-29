!P.multi = [0,1,5]

for i=0,4 do begin

wx = where(wire4.mag(i) gt 0. and wire4.mag(i) lt 20.,c)
 mmag = median(wire4(wx).mag(i))
 noise = robust_sigma(wire4(wx).mag(i))

   dat  = wire4(wx).mag(i)
   tt   = wire4(wx).hjd - t0

   resistant_mean,dat,3,me,sd,nr
   dat = dat - me

; -------------------------------------------------------------------
   per1 = 15.009 ; wire_per(i) / fac ; period for wire craft
; -------------------------------------------------------------------

   pha = (tt mod (1./per1)) * per1
   wx = where( (pha ge -.51 and pha lt -0.22) or $
               (pha ge  .495 and pha lt  0.79),cx)

     plot,pha,dat,psym=3,yr=[-6.*noise,6*noise],ysty=1,xr=[-.55,1.05],xsty=1
     oplot,pha(wx),dat(wx),psym=3,col=col.red
   
   dat = dat(wx)
   tt = tt(wx)

endfor

end
