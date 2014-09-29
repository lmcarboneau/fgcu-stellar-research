dops = 1

if dops then begin
 out = '/ai40/bruntt/wire/wire_eps/procyon/wire_linearity2_cen.ps
 a4ps, filename=out, /square
endif

if n_elements(wire) eq 0 then begin
 ; restore,'/ai40/bruntt/wire/ProcyonF5IV-V/data_wire_005.idl'
; n1= 48500 + 1. * 9500. & n2 = 50500 + 1. * 9500. ; procyon

 restore,'/ai40/bruntt/wire/alphaCen/data_wire__035.idl'
 n1= 4000. & n2 = 20000.


 proc = reform(wire.d(0,*,*))
 wire_ccd_flip_sat,proc,proc2,lim=10000.
endif

!P.multi= 0 ; [0,8,8]

vwa_getpos,4,4,0,0,0,pos & v = 0.01
; vwa_getpos,8,8,0,0,0,pos & v = 0.065

; nx,ny,spx,spy,order,pos

cnt = 0L
nme = [' ',' ',' ',' ',' ',' ',' ', ' ']


; for i=0,7 do begin
;  for j=0,7 do begin

plot,[0,1],/nodata,xsty=6,ysty=6

for i=2,5 do begin
 for j=2,5 do begin
 
p2 = reform(proc2(i,j,*))
p = p2(n1:n2)

; Make artificial light curve with the same mean value + RMS
 np = n_elements(p) & val = fltarr(np)
 rr = robust_sigma(p) & me = median(p)
 for k=0L,np-1 do val(k) = me + rr * randomn(seed)
 val = long(val)

; plot,val,psym=3,ysty=1 & oplot,p,psym=4,col=col.red

; Calculate histogram:
bz = 8. ; binsize 

bz = ceil(0.5 * me / 200.) * 2.
if bz lt 2 then bz = 2.

bz = 2.
if me gt 4096. and me lt 8192 then bz = 4.
if me gt 8192. then bz = 4.

mval  = me - 20000. & if mval lt 0. then mval = 0.
mxval = me + 20000.
orr = 0.

h = histogram(p,bin=bz,min=mval,max=mxval)
h = h / float(np)
xh = mval + bz * findgen(n_elements(h))
g = gaussfit(xh,h,gf)


h_syn  = histogram(val,bin=bz,min=mval,max=mxval)
h_syn  = h_syn / float(np)
xh_syn = mval + bz * findgen(n_elements(h_syn))
g_syn  = gaussfit(xh_syn,h_syn,gf)


xx = me+[-1,1]*gf(2)*2.35*3.

plot,xh,h,xr=xx,xsty=1,ysty=3,position=pos(*,cnt), $
 xtickname=nme, ytickname=nme,/noerase,xthick=2,ythick=2,/nodata
oplot,gf(1)+[-1,1]*gf(2)*2.35*0.5,[1,1]*gf(0)*0.5,thick=3 ; FWHM

; HISTOGRAMS:
; oplot,xh,g,thick=6,col=150. & oplot,xh_syn,g_syn,thick=2,col=0. ; gaussians
oplot,xh,h,col=150.,thick=3 & oplot,xh_syn, h_syn,col=0. ; raw histograms


xyouts,pos(0,cnt)+v,pos(1,cnt)+0.09,/normal, $
 'm='+strcompress(string(me,format='(I6)'),/remove_all), orientation=orr,$
 charsi=0.5,charthick=1

xyouts,pos(0,cnt)+v,pos(1,cnt)+0.08,/normal, $
 '!4r!3='+strcompress(string(gf(2),format='(F6.1)'),/remove_all), orientation=orr,$
 charsi=0.5,charthick=1

xyouts,pos(0,cnt)+v,pos(1,cnt)+0.07,/normal, $
 '!4r!3!Ie!N='+strcompress(string(sqrt(gf(1)*15.)/15.,format='(F6.1)'),/remove_all), $
 orientation=orr,charsi=0.5,charthick=1

xyouts,pos(0,cnt)+v,pos(1,cnt)+0.06,/normal, $
 'r='+strcompress(string(gf(2) / (sqrt(gf(1)*15.)/15.),format='(F6.2)'),/remove_all), $
 orientation=orr,charsi=0.5,charthick=1


xyouts,pos(0,cnt)+0.01,pos(1,cnt)+0.02,/normal, $
 '('+strcompress(string(i+1,format='(I1)'),/remove_all) + ',' + $
     strcompress(string(j+1,format='(I1)'),/remove_all) + ')', $
     charsi=0.6,charthick=1

 for b=1,6 do begin
  fx = 2^14. + 1024. + 512. + 256.
  bitval = fx  + 2.^(float(b)) 
  if bitval gt xx(0) and bitval lt xx(1) then $
  plots,bitval,!y.crange,line=2,col=150,thick=2
 endfor



; for b=1,16 do begin
;  bitval = 2.^(float(b)) 
;  if bitval gt xx(0) and bitval lt xx(1) then $
;  plots,bitval,!y.crange,line=2,col=150
; endfor



cnt = cnt + 1

endfor
endfor

!P.multi=0

if dops then begin
 a4ps,/close
 print,' gv ' + out + ' &'
endif

END
