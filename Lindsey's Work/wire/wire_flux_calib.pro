; Measure flux in all main targets

; spawnrob,'ls -

; a1 = findfile('/data2/bruntt/wire/dat/*/*allslot*.idl',Count=cnt1)
; a2 = findfile('/data2/bruntt/wire/dat/*/*/*allslot*.idl',Count=cnt2)

 a1 = findfile('/ai40/bruntt/wire/collection/*allslot*.idl',Count=cnt1)
 a2 = findfile('/data2/bruntt/wire/dat/*/*/*allslot*.idl',Count=cnt2)


target = strarr(cnt1 + cnt2)
file = target
jj = 0

for i=0,cnt1-1 do begin
 a = strsplit(a1(i),'/',/extract)
; w = where(a eq 'dat',c)
 w = where(a eq 'collection',c)
 target(jj) = a(w+1)
 file(jj) = a1(i)
 jj = jj + 1
endfor

for i=0,cnt2-1 do begin
 a = strsplit(a2(i),'/',/extract)
; w = where(a eq 'dat',c)
 w = where(a eq 'collection',c)
 target(jj) = a(w+1)
 file(jj) = a2(i)
 jj = jj + 1
endfor

target = target(0:jj-1)
file   = file(0:jj-1)

a = sort(target)  & target = target(a)  & file = file(a)
uu = uniq(target) & target = target(uu) & file = file(uu)
jj = n_elements(uu)

for i=0,jj-1 do $
 print,target(i), ' -- ' , file(i)



; wire_target_info, '~/wire/wire_process/targets_wire.txt', info
wire_target_info, '/ai40/bruntt/wire/wire_process/targets_wire.txt', info


; Measure flux & positions ...

d = replicate({file:'', target:'', object:'', flux:0., x:0., y:0., mag:0., $
 b:0., v:0., bv:0., spec:'', str:fltarr(4)}, 1500)
dcnt = 0L
d.v = 99 & d.bv = 99 & d.b = 150 & d.flux = -1 & d.mag = -99 & d.object = 'NA'

for i=0,jj-1 do begin

wireult = 0B
restore,file(i)

np = n_elements(wireult.mag(0))
ns = n_elements(wireult(0).x) ; number of stars

for star = 0,ns-1 do begin

if n_elements(wireult) lt 10 then stop

 w = where(wireult.x(star) gt 1. and wireult.y(star) gt 1. and $
           wireult.x(star) lt 510. and wireult.y(star) lt 510. and $
           wireult.mag(star) gt 5. and wireult.mag(star) lt 25. and $
           wireult.hjd gt 5e4 and wireult.hjd lt 6e4,c)


 if c ge 50. then begin

  d(dcnt).file = file(i)
  d(dcnt).target = target(i)
  mag = median(wireult(w).mag(star))
  d(dcnt).flux = 10.^ ( (25. - mag) / 2.5 )
  d(dcnt).mag = mag
  d(dcnt).x = median(wireult(w).x(star))
  d(dcnt).y = median(wireult(w).y(star))

  if abs(d(dcnt).x-260) lt 15. and $ 
     abs(d(dcnt).y-260) lt 15. then begin ; main target

    wt = where(strmatch(info.object,'*'+target(i)+'*',/fold_case) eq 1,ct)
    if ct ne 1 then stop

    if ct eq 1 then begin 
       d(dcnt).b  = info(wt).b
       d(dcnt).v  = info(wt).v
       d(dcnt).bv = info(wt).bv
       d(dcnt).spec = info(wt).spec
       d(dcnt).str = [info(wt).by, info(wt).m1, info(wt).c1, info(wt).hb]
       print,' Wire target: '+target(i) + ' ---> ' + info(wt).object
    endif

  endif


  dcnt = dcnt + 1

endif else print,' No good data for : '+file(i),star
; stop ; at least 50 points for this star?

 

endfor

endfor

d = d(0:dcnt-1)

help,d

wcen = where(strmatch(d.file,'*alphaCen_merged_allslots_101*') eq 1,ccen)
if ccen eq 1 then d(wcen).flux = d(wcen).flux * 5. ; int. time = 0.1 sec

w = where(d.v gt -5 and d.v lt 15 and d.bv gt -5 and d.bv lt 5 and d.flux gt 50,c)

plot,d(w).flux,d(w).v,psym=2


;	COEFF = ROBUST_REGRESS(X,Y, YFIT,SIG, [ NUMIT = , FLOOR = ] )
;	X = Matrix of independent variable vectors, dimensioned 
;		(NUM_VAR,NUM_PTS), as in REGRESS
;	Y = Dependent variable vector. All Y<MINVAL are ignored.

w = where(d.v gt -5 and d.v lt 15 and d.bv gt -5 and d.bv lt 5 and d.flux gt 50,c)
x = fltarr(3,c)
x(0,*) = d(w).v
x(1,*) = d(w).bv
x(2,*) = d(w).bv^2.
y = d(w).mag
coeff = ROBUST_REGRESS(x,y,yfit,sig)

print,'Johnson calibr:'
print, coeff(0) , ' + ' , coeff(1) ,' * V + ', $
 coeff(2) ,' * (B-V) + ', coeff(3) ,' * (B-V)^2 '
print,' Sigma Johnson ', sig





plot,x(0,*),y,psym=2,yr=[-5,20],xtit='(B - V)',ytit='V and (V - Vfit)'
oplot,x(0,*),y - yfit,psym=4


w = where(d.v gt -5 and d.v lt 15 and d.str(0) gt -5 and d.str(0) lt 5 $
          and d.flux gt 50 and d.str(2) gt -5 and d.str(2) lt 5,c ) ;  and $
;                               d.str(1) gt -5 and d.str(1) lt 5,c)
x = fltarr(5,c)
x(0,*) = d(w).v
x(1,*) = d(w).str(0)
x(2,*) = d(w).str(0)^2.
x(3,*) = d(w).str(2)    ; c1 ---> evolution stage !!
x(4,*) = d(w).str(2)^2.
  ; x(3,*) = d(w).str(1)
y = d(w).mag

coeff = ROBUST_REGRESS(x,y,yfit,sig)

print,'Stromgren calibr:'
print, coeff(0) , ' + ' , coeff(1) ,' * V + ', $
 coeff(2) ,' * (b-y) + ', coeff(3) ,' * (b-y)^2 + ' 
print,  '          ' , coeff(4) ,' * c_1 + ', coeff(5) ,' * c_1^2'
print,' Sigma stromgren ', sig


x = fltarr(3,c)
x(0,*) = d(w).v
x(1,*) = d(w).str(0)
x(2,*) = d(w).str(2)    ; c1 ---> evolution stage !!
y = d(w).mag

coeff = ROBUST_REGRESS(x,y,yfit,sig)

print,'Stromgren calibr:'
print, coeff(0) , ' + ' , coeff(1) ,' * V + ', $
 coeff(2) ,' * (b-y) + ', coeff(3) ,' * c_1 + '
print,' Sigma stromgren ', sig



x = fltarr(1,c)
x(0,*) = d(w).v
y = d(w).mag
coeff = ROBUST_REGRESS(x,y,yfit,sig)
print,'Johnson calibr. simple 101: '
print, coeff(0) , ' + ' , coeff(1) ,' * V '
print,' Sigma Johnson, simgle 101: ', sig

;x = fltarr(2,c)
;x(0,*) = d(w).v
;x(1,*) = d(w).v^2.
;y = d(w).mag
;coeff = ROBUST_REGRESS(x,y,yfit,sig)
;print,'Johnson calibr. simple 101: '
;print, coeff(0) , ' + ' , coeff(1) ,' * V ', ' + ' , coeff(1) ,' * V^2. '
;print,' Sigma Johnson, simgle 101: ', sig


x = fltarr(1,c)
x(0,*) = d(w).b
y = d(w).mag
coeff = ROBUST_REGRESS(x,y,yfit,sig)
print,'Johnson calibr. simple 201: '
print, coeff(0) , ' + ' , coeff(1) ,' * B '
print,' Sigma Johnson, simgle 201: ', sig


;x = fltarr(2,c)
;x(0,*) = d(w).b
;x(1,*) = d(w).b^2.0
;y = d(w).mag
;coeff = ROBUST_REGRESS(x,y,yfit,sig)
;print,'Johnson calibr. simple 201: '
;print, coeff(0) , ' + ' , coeff(1) ,' * B ', ' + ' , coeff(1) ,' * B^2 '
;print,' Sigma Johnson, simgle 201: ', sig




END
