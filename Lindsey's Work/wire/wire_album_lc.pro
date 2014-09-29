PRO wire_album_lc, files,colx=colx, t0all=t0all,t0range=t0range, dops=dops

; Simply plot the light curves
; Example:
; m4_get_basedir,base & base = base + 'wire_process/' 
;  files = base + ['reduced_super.idl','reduced_giants1.idl'
; wire_album_lc,files

nf = n_elements(files)
default9, yeach, 0.07
default9, t0all, 51400D ; 51400 = okt. 1999
default9, pz, 1
default9, sz, 0.1
default9, nslot, 12
default9, dops, 1B
default9, t0_limit, 14 ; same set of data?

m4_get_basedir,base
col=getcolor(/load)



easyps, keywords, kk, dops=dops, /prepare, dim = [300,15,-1,1], $
 fil = 'wirelc.ps', dir = base + 'wire/wire_eps/'

col=getcolor(/load)
colx=[col.charcoal, col.black, $
      col.cyan, col.magenta, $
      col.red, col.red]


if n_elements(t0range) eq 0 then t0range = 53700 - t0all ; 53700 = Nov 2005

plot,[0,1],/nodata,yr=[-.25,float(nslot)-0.25]*yeach,xr=[0,t0range],xsty=1,ysty=5,$
 xtit='HJD - 24'+string(t0all,format='(I5)') + ' [d]'

; stop

; plot,[0,1],/nodata,yr=[-.25,float(nslot)-0.25]*yeach,xr=[-5,t0range+5],ysty=5
for i=0,nslot-1 do plots,!x.crange,0+yeach*i,line=5,thick=2,col=100
for i=0,nslot-1 do xyouts,-0.5,yeach*float(i)-0.01,string(i*yeach,format='(F4.2)'),alignment=1

t0use = fltarr(500)
cnt = 0

for f=0,nf-1 do begin

print,' %%% Reading file: ' + files(f), f+1, nf+1
restore,files(f)
nlc = n_elements(wire)



for n=0,nlc-1 do begin

lc  = wire(n).lc(0:wire(n).np-1)     ; MASSAGED LIGHT CURVE
lc1 = wire(n).lcsub2(0:wire(n).np-1) ; ORBITAL FREQ. WERE SUBTRACTED
lc2 = wire(n).lcsub(0:wire(n).np-1)  ; ALL FREQ REMOVED
tt  = wire(n).hjd(0:wire(n).np-1)    ; HJD TIMES (minus t0)
wei = wire(n).wei(0:wire(n).np-1)    ; WEIGHTS

; X range for light curve
x1 = floor(min(tt) * 10.) / 10.
x2 =  ceil(max(tt) * 10.) / 10.

yfac = 1e6 / 1.086 ; Remember: 1ppm = 1.086microMag: (ppm/microMag) * (1/1.086)
w = where(wei/max(wei) gt .1,c)
ptp_robust_fin,lc(w),noise,1
rr = robust_sigma(lc(w)) 


if noise gt .06 or rr gt .06 then begin
  print,' %%% Large scatter: ' + wire(n).comp + string(wire(n).entry,format='(I2)')
  goto,skip_star
endif

; Offset LC in y-direction if another star was observed already:
wt0 = where(abs(wire(n).t0-t0use) lt t0_limit,ct0) ; number of LC from this WIRE run?
offy = float(ct0) * yeach
t0use(cnt) = wire(n).t0
cnt = cnt + 1

neach = 8 ; only plot every 8th data point
x = findgen(floor(float(c)/neach)) * neach

oplot,tt(w(x))+wire(n).t0 - t0all,-lc(w(x))+offy,psym=pz,symsi=sz,col=colx(f)
; plot,lc2,wei/max(wei),psym=1

print,min(tt(w))+wire(n).t0 - t0all

; Print the spectral type, V + B-V colour
;xyouts,min(tt(w))+wire(n).t0-t0all,offy+yeach*.2,wire(n).spec + $
;                                ' ' + string(wire(n).v,format='(F3.1)') + $
;                                ' ' + string(wire(n).bv,format='(F4.2)'),charsi=1.2,charthick=2

txtout = strmid(wire(n).spec,0,5) + '/' + strmid(strcompress(wire(n).nam,/remove_all),0,10)
xyouts,min(tt(w))+wire(n).t0-t0all,offy+yeach*.2,txtout,charsi=1.2,charthick=2

; stop
; help,offy,tt,lc,w

skip_star:

endfor
endfor; next file!

easyps, keywords, kk, dops=dops, /close


END
