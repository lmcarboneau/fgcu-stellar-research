PRO wire_tidy_spec, p, p2, f, forb=forb, inc=inc, df=df

nf = n_elements(f(0,*))

p2 = p ; result structure
p3 = p ; temporary structure

p3.d(*) = -1.0
p3.d2(*) = -1.0

; Remove unwanted frequencies before smoothing
for i=0,nf-1 do begin
 w = where(p.f ge f(0,i) and p.f le f(1,i),c)
 if c ge 3 then begin
  p3(w).f = p(w).f
  p3(w).d = p(w).d
 endif
endfor

; ====================================================================================
; Computed gaussian-smoothed spectrum: gaussian has width (df) at freq
; (fref). At other frequencies that width scales with the number of
; points in each logarithmic frequency interval.
; ====================================================================================
default9, df,   35.      ; parameter for excluding oribital power
default9, forb, 173.6    ; parameter for excluding oribital power
default9, inc, 0.001

wire_exclude_orbital,p3.f, p3.d,fny,dny, forb=forb,df=df,inc=inc

w = where((dny gt 1e-6),c)
f1 = p3.f & d1 = p3.d & f1 = f1(w) & d1 = d1(w)
wire_power_smooth, f1, d1, ds, df=150.,fref=1000.,res=0.1 ; ,/debug

; Keep the original spectrum: do not remove orbital freqs! 18 feb 2005
; p2(0:c-1).f1 = f1 ; The freq + power that you are fitting the data to!
; p2(0:c-1).d1 = d1 

np = n_elements(p3.f2)
p2.n2 = np-1       ; number of points
p2.f2 = p3.f
p2.d2 = interpol(ds, f1, p2.f)

wb = where((dny lt 1e-6),c)
if c ge 1 then p2(wb).d2 = -1. ; MARK BAD DATA!



END
