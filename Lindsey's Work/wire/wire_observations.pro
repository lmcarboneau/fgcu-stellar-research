; RA / DEC plot for observed wire targets: mark procyon

dops = 1

if dops then begin
   dir = '~/wire/'
   startFilename = 'WIRE_Observing.ps'
   startFilename = strcompress(startFilename,/remove_all)

   x = 20. & y = 10. & xo = (21.5-x) * 0.5 & yo = (30.5-y)*.5

   keywords = PSConfig(Cancel=cancelled, Filename=startFilename, $
    /European, directory=dir, $
    xsize=x, ysize=y, xoffset=xo, yoffset=yo)

   IF cancelled THEN RETURN
      thisDevice = !D.Name
      thisFont   = !P.Font
      !P.Font = 0
    Set_Plot, 'PS'
    Device, _Extra=keywords
endif



restore,'~/wire/wire_essential/wire_sec_info.idl' ; 09 NOV 2004 wireobj structure

; w = where(strmatch(wireobj.hdnam,'*61421*') eq 1)
; print,wireobj(w).v, wireobj(w).bv
; 0.340000     0.400000


plotsym,0,/fill
plot,wireobj.ra,wireobj.dec,psym=8,xr=[0,360],xsty=1,symsi=.7,$
 xthick=2,ythick=2, charsi=1.2, charthick=2, $
 xtit='R.A.', ytit='DECL.',tit='WIRE Targets < October 2004'
w = where(strmatch(wireobj.hdnam,'*61421*') eq 1)
plotsym,0
plots,wireobj(w).ra,wireobj(w).dec,psym=8,symsi=4

;xyouts, wireobj(w).ra+10,wireobj(w).dec, alignment=0.5, $
xyouts,110,30, alignment=0.5, $
 charsi=1.1,'Procyon: September; RA=' + $
  strcompress(string(wireobj(w).ra,format='(F9.1)'),/remove_all),$
 orientation = 0.






if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  ggv ' + keywords.filename + '  & '
endif


end
