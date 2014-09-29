; Get expected count rates / SN levels for WIRE targets
; Countrates can be offset to the observed levels by setting true=1
; Noise is when binning data to one data point very 15 seconds.

dops = 1

true = 1B ; offset noise to OBSERVED noise levels?

m4_get_basedir, base

if dops then begin
   dir = base + 'wire/wire_eps/'

   if true then startFilename = 'WIRE_Countrates_True.ps' else $
     startFilename = 'WIRE_Countrates.ps'
    
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


restore,base + 'wire/wire_essential/wire_sec_info.idl' ; 09 NOV 2004 wireobj structure

; w = where(strmatch(wireobj.hdnam,'*61421*') eq 1)
; print,wireobj(w).v, wireobj(w).bv
; 0.340000     0.400000

plot_io,[0,1],/nodata, xr=[-1,8], yr=[100,3e4]/1000,ysty=1,xsty=1,$
 xtit='V',ytit='Noise level in mmag', $
 xthick=2,ythick=2, charsi=1.2, charthick=2
plotsym,0,/fill


n = n_elements(wireobj)
cnt = fltarr(2,n)


for i=0,n-1 do begin

bv = wireobj(i).bv
v  = wireobj(i).v
b  = bv + v

cnt(0,i) = v

wire_counts,b, v, counts,/out

gain = 15.
if true then sn = 1e3/sqrt(counts * gain)/2.  else $
 sn = 1e3/sqrt(counts * gain)  
 
print,v,sn

plots, v, sn, psym=8,symsi=.7

cnt(1,i) = sn

endfor

; TBD: Must use LOG:
;x = robust_poly_fit(cnt(0,*),cnt(1,*),1,myfit)
;vall = -2.0 + (findgen(100) / 99.) * 10.
;snall = x(0) + x(1) * vall
; oplot,vall, snall, line=5,thick=2

; Mark procyon
w = where(strmatch(wireobj.hdnam,'*61421*') eq 1)
bv = wireobj(w).bv
v  = wireobj(w).v
b  = bv + v
wire_counts,b, v, counts,/out
sn = 1e6/sqrt(counts * gain)
plotsym,0
plots, v, sn, psym=8,symsi=2

oplot,[.1,5.0]*v, 285. * sqrt(5.) * [1.,1.],line=2
xyouts, 1, 700., 'Procyon observed noise level', $
 charthick=2, charsi=1.2


if dops then begin
      Device, /Close_File
      Set_Plot, thisDevice
      !P.Font = thisFont
      set_plot,'x'
   print,' $  ggv ' + keywords.filename + '  & '
endif


end
