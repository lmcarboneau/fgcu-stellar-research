PRO wire_freq_mirror, perfile, fmirror, flim=flim, $
 specfile=specfile, fwire=fwire, x1=x1,x2=x2

; Example:
; wire_freq_mirror,'~/wire/EpsilonCep/care7/w12_NO_komb.per',fmirror,flim=0.10,fwire=15.348,$
; specfile='~/wire/EpsilonCep/care7/fouwire_original.fou'

default9, flim, 0.05 ; default limit in c/day
default9, specfile,''
default9, fwire, 15.4

readcol,perfile, aa,f,a,p, format='a,d,d,d' ; read PERIOD04/98 file
nf = n_elements(f) ; number of modes 

if n_elements(x1) eq 0 then x1 = floor(min(f)-1.0)
if n_elements(x2) eq 0 then x2 = ceil(max(f)-1.0)

if specfile ne '' then begin
 readcol,specfile,freq,ampl ; for debugging
 plot,freq,ampl,xr=[x1,x2]
 col=getcolor(/load)
 plots,fwire,!y.crange,col=col.sky,thick=3,line=5
endif

yy = !y.crange
yy = max(yy) * .25

comment = ['2 Fwire - Fi', 'Fi - Fwire', 'Fi + Fwire','2 * Fi - Fw']

for i=0,nf-1 do begin

df = f(i) - fwire 
; if df gt 0 then mirror = fwire - (f(i) - fwire) 
; if df lt 0 then 
mirror = fwire + (fwire - f(i)) 
add1 = f(i) - fwire
add2 = f(i) + fwire
add3 = 2*f(i) - fwire

; Look for MIRROR frequencies:
 fcheck = [mirror, add1, add2, add3] 
for c=0,3 do begin ; check the three combination frequencies
 for k=0,nf-1 do begin
  if k eq i then goto,skip_f ; the mode itself

 dist = abs(fcheck(c)- f(k))
 if dist lt flim then begin
   print,' %%% Mirror freq. for mode Fi=F'+strcompress(string(i+1,format='(I3)'),/remove_all) + $
     ' =' + string(f(i),format='(F9.2)') + $
     ' which is ' + comment(c) + ' == F' + strcompress(string(k+1,format='(I3)'),/remove_all) + $
     '=' + string(f(k),format='(F9.2)')

 endif
 skip_f:
endfor
endfor


if specfile ne '' then begin
 plot,freq,ampl,xr=[x1,x2]
 plots,fwire,!y.crange,col=col.sky,thick=3,line=5

;  print,f(i), ' c/day'

 plots,f(i),!y.crange,col=col.red,thick=2
 plots,mirror,!y.crange,col=col.red,line=2,thick=2
 plots,add1,!y.crange,col=col.yellow,line=2,thick=2
 plots,add2,!y.crange,col=col.green,line=2,thick=2
 plots,add3,!y.crange,col=col.magenta,line=2,thick=2


 for k=0,nf-1 do oplot,f(k)*[1.,1],[0,yy],col=col.charcoal,thick=6

 hitme,s1,/silent & if s1 eq 'x' then stop 
endif

endfor

END
