PRO wire_smooth_spec, freq, ampl, lowfreq, ampl2, stiff1=stiff1, stiff2=stiff2

default9, stiff1,  5.0
default9, stiff2, 20.0

ampl2 = ampl & ampl2(*) = 0.0

wl1 = where(freq lt lowfreq,c_wl1) ; below 2 c/day
if c_wl1 ge 10 then begin
 sm1 = ddsmooth(ampl(wl1),stiff=stiff1)
 ampl2(wl1) = sm1
endif

wl2 = where(freq ge lowfreq,c_wl2) ; above 2.0 c/day
if c_wl2 ge 10 then begin
 sm2 = ddsmooth(ampl(wl2),stiff=stiff2)
 ampl2(wl2) = sm2
endif

; Finally: smooth lightly to take care of overlapping region
ampl2 = smooth(ampl2,3,/edge)

END
