PRO wire_exclude_orbital, f,d, fnew, dnew, df=df, $
 forb=forb, inc=inc, fskip=fskip,skip2=skip2,skip3=skip3,unit=unit,debug=debug

; ================================================================
default9, unit, 'microHz'
case unit of 
 'microHz': fact = 1.0
 'milliHz': fact = 1e3
 'c/day':   fact = 1e6/86400.
endcase
; ================================================================

; ================================================================
default9, forb, 173.6/fact
default9, df,   20./fact   ; width of freq. window to exclude
default9, inc,   0.2  ; how fast windows grow
default9, fskip, 0B
default9, skip2, 0B
default9, skip3, 0B
default9, debug, 0B
; ================================================================

; print,' %%% df = ',df

fc = forb
mx = max(f)
i = 1

np = n_elements(f)
flag = bytarr(np) & flag(*) = 1B

if debug then begin
 col=getcolor(/load)
 ; plot,f,d,min_value=1e-6
 plot_oo,f,d,min_value=1e-6
endif


while fc lt mx do begin

 fc = i * forb

 df_use = df * (1. + i * inc)

 w = where(abs(f - fc) lt df_use,c)
 if c ge 1 then flag(w) = 0B

 if debug then begin
  if c ge 2 then oplot, f(w), d(w),col=col.red
 endif

 i = i + 1

endwhile


wg = where(flag eq 1,cg)
wb = where(flag eq 0,cb) 

fnew = f      & dnew = d
if cb ge 1 then begin ; any flagged data?
 fnew(wb) = -1 & dnew(wb) = -1
endif

if n_elements(fskip) eq 2 then begin
 wbad = where(fnew gt fskip(0) and fnew lt fskip(1),cbad)
 if cbad ge 1 then begin
  fnew(wbad) = -2. & dnew(wbad) = -2.
 endif
endif

if n_elements(skip2) eq 2 then begin
 wbad = where(fnew gt skip2(0) and fnew lt skip2(1),cbad)
 if cbad ge 1 then begin
  fnew(wbad) = -2. & dnew(wbad) = -2.
 endif
endif

if n_elements(skip3) eq 2 then begin
 wbad = where(fnew gt skip3(0) and fnew lt skip3(1),cbad)
 if cbad ge 1 then begin
  fnew(wbad) = -2. & dnew(wbad) = -2.
 endif
endif




END
