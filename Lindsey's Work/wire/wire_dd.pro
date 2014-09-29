; Identify the neighbours to Altair in the WIRE data

spawn,'cat /ai38/bruntt/wire/data/altair_neigh.dat',dd

nn  = n_elements(dd)
ra  = fltarr(nn)
dec = fltarr(nn)
vv  = fltarr(nn)
bb  = fltarr(nn)

for i=0,nn-1 do begin
 a = strsplit(dd(i),' ',/extract)
 ra(i)  = float(a(0)) + float(a(1))/60. + float(a(2)) / (3600.) ; hours
 ra(i)  = ra(i) * 360. / 24. ; degrees
 dec(i) = float(a(3)) + float(a(4))/60. + float(a(5)) / (3600.) ; degrees
 bb(i) = float(a(6))
 vv(i) = float(a(7))
endfor

bv = bb-vv
w = where(abs(bv) gt 50 or bb lt -50. or vv lt -50.,c)
w2 = where(abs(bv) lt 50 and bb gt -50. and vv gt -50.,c)

if c ge 1 then begin
 bv(w) = 0.5
 vv(w) = 13.0
 bb(w) = 13.5
endif


; Count rate for a star with V=0.0 in one second: (534400 + 246230*bv+ 976300*bv^2)
; For stars B-V > 0.72 the count rate is 1.22e6

cr = (534400 + 246230*bv+ 976300*bv^2) * 0.5 * (10.^(-0.4*vv))

; Mean counts in 0.5 secs for the five stars in the altair field
ct =[ 57816.8    ,  3269.00  ,    15210.0,      3591.97  ,    173176]

plot,bv(w2),vv(w2),psym=4,xr=[-3,3],yr=[0,13]

plot,bv(w2),cr(w2),psym=4,yr=[0,3e5],xr=[-3,3]

end