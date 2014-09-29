PRO wire_rem_ecl, lc1, lc2, te

necl = n_elements(te(0,*))
if necl eq 0 then RETURN

; Example
; wire_rem_ecl, $
;  'wire_lc_LambdaSco_Mar2004_s0_HD158926_B2IV+....dat',$
;  'wire_lc_LambdaSco_Mar2004_s0_HD158926_B2IV+.noecl.dat',$
; [ [-8.8,-8.3],[-2.8,-2.2], [3.1,3.7], [9.,9.7] ]

readcol, lc1, t, d, w, format='D,D,D'

help,t

for i=0,necl-1 do begin
; if i eq 0 then $
;  g = where(t lt te(0,i) or t ge te(1,i),cg)

; if i eq (necl-1) then $

  g = where(t lt te(0,i) or t ge te(1,i),cg)

  if cg ge 1 then begin
   t = t(g) & d = d(g) & w = w(g) 
  endif

endfor

w = w / total(w) ; renormalize weights 

openw,1,lc2
n = n_elements(t)
for j=0L,n-1 do $
 printf,1,$
   t(j),d(j),w(j), $
   format='(D15.7, D15.6, D15.8)'
close,1

help,t

print,' %%% Exported lc: ' + lc2

END
