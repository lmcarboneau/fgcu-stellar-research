PRO wire_hjd, time1, time2, ra_deg, dec_deg, jd, hjd

; ra_deg and dec_deg is the R.A. and DECL. in degrees!

 t1 = strcompress(string(time1),/remove_all)

 year = 1900 + float(strmid(t1,0,2))
 check_100 = strmid(t1,0,1)
 if check_100 eq '0' then year = year + 100

 month = float(strmid(t1,2,2))
 day   = float(strmid(t1,4,2))
 
 t2  = strcompress(string(time2),/remove_all)
 t2a = strcompress(string(floor(double(t2))),/remove_all)

 nt = n_elements(t2) ; number of data points
 hjd = dblarr(nt)
 jd = hjd

 time2a = double(time2)

; Debugging:
; w = where(strlen(t2a) eq 3)
; w = where(strlen(t2a) eq 6)
; i = w(50)
;  ll = strlen(t2a(i))
; print,t2a(i),' ',t2(i)
; print,hr, min, sec

; ==============
; Get HJD times!
; ==============
for t=0L,nt-1 do begin ; for each data point

 sec_frac = ( time2a(t)-floor(time2a(t)) ) ; fraction of a second!

  ll = strlen(t2a(t)) ; length of string ... 
  if ll eq 1 or ll eq 2 then begin
   sec = time2a
   min = 0.
   hr  = 0.
  endif

  if ll eq 3 or ll eq 4 then begin
   sec = float( strmid(t2a(t),1+ll-3,2) ) + sec_frac
   min = float( strmid(t2a(t),0,1+ll-3) )
   hr  = 0.
  endif

  if ll eq 5 or ll eq 6 then begin
   sec = float( strmid(t2a(t),3+ll-5,2) ) + sec_frac
   min = float( strmid(t2a(t),1+ll-5,2) )
   hr  = float( strmid(t2a(t),0,ll-5+1) )
  endif

  if ll ge 7 or ll eq 0 then stop ; corrupt time!

   min_x = min + sec / 60. ; Time is UT, right?
   juldate, [year(t), month(t), day(t), hr, min_x], jd_temp ; Julian Date
   hjd_temp = helio_jd( jd_temp, ra_deg, dec_deg) ; Heliocentric Julian Date
   ; print, (hjd_temp-jd_temp)*86400. ; difference in seconds ...
   jd(t) = jd_temp
   hjd(t) = hjd_temp

stop

   ; Sanity checks:
   ; caldat,2400000D + jd(t), m1, m2, m3, m4, m5, m6
   ; caldat,2400000D + hjd(t), hm1, hm2, hm3, hm4, hm5, hm6
   ; print,'JD Date: ',t1,' Time: ',t2(t)
   ; print,'JD Date: ',m2, ' Month: ',m1,' Year: ',m3
   ; print,'JD Hr:   ',m4, ' Min: ', m5, ' Sec: ',m6
   ; print,'HJD Hr: ',hm4, ' Min: ', hm5, ' Sec: ',hm6
    
   if hjd(t) lt 51000. or hjd(t) gt 53000 then stop ; some thing is wrong with the times!
   ; This is Before 5th of July 1998: WIRE WAS NOT FLYING AT THAT TIME!

endfor


END
