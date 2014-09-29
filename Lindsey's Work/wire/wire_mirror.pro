pro wire_mirror,f,fcor ; ,cc

fmax = 600

; f = 0.3 * 11.574
; Example: m4_mirror,11.574 * 0.3,fcor ; , 50

; sep = 0.3
; f = (3.5 - 0.*sep )* 1e6 / (86400.) ; micro hertz

cn = 9 ; possible aliases for the "true" frequency (ODD NUMBER!!!!!!!)
cn2 = ( cn - 1 ) * 0.5

cn_day_alias = 3 ; only +- 1/day alias (ODD NUMBER!!!!!!!)
cn2_day_alias = (cn_day_alias - 1) * 0.5

freq_wire = 15.009 ; cycles pr. day
freq_hz   =  freq_wire / 86400.
freq_micro_hz = freq_hz * 1e6

fcor = fltarr(3,cn * cn)
cnt = 0

for i=0,cn_day_alias-1 do begin
 for j=0,cn-1 do begin
  fcor(0,cnt) = float(i-cn2_day_alias) * freq_micro_hz + float(j-cn2) * f
  fcor(1,cnt) = (j-cn2)
  fcor(2,cnt) = (i-cn2_day_alias)
  cnt = cnt + 1
 endfor
endfor

fcor(0,*) = abs(fcor(0,*)) ; all freqs are positive
a = sort(fcor(0,*))
fcor = fcor(*,a)
c2 = n_elements(fcor(0,*))

;print,fcor

b = !y.crange

a = uniq(fcor(0,*))
fcor2 = fcor(*,a)

; Lines
for i=0,n_elements(fcor(0,*))-1 do $
 if (fcor(0,i)-0.3) ge .1 and (fcor(0,i)+0.3) lt fmax then $
  plots,fcor(0,i),!y.crange,line=1 ; ,color=cc

; Names
for i=0,n_elements(fcor2(0,*))-1 do begin
 addf = 0
 if (fcor2(0,i)-0.3) ge .1 and (fcor2(0,i)+0.3) lt fmax then $ ; make sure TEXT is inside plot!
 xyouts,fcor2(0,i)-0.2,avg(b)*1.0+addf,$
  '!6h!L'+strcompress(string(fcor2(1,i),format='(I2)'),/remove_all)+$
  '/'+strcompress(string(fcor2(2,i),format='(I2)'),/remove_all)+'!N!3',$
  charthick=2,charsi=0.8,orientation=90,/data
endfor

; Use 1.18 for star200 and 1.15 for clean500 (in first xyouts cmmand for x offset)
 plots,f,!y.crange,thick=3,line=2

;xyouts,f*1.18,avg(b)*1.15,/data,alignment=0.0,'!6Sep=!7Dm!3',charthick=2,charsi=1.2,orientation=90
 plots,freq_micro_hz,!y.crange,thick=3,line=2
;xyouts,freq_micro_hz*1.04,avg(b)*1.15,/data,alignment=0.0,'!61/day!3',charthick=2,charsi=1.2,orientation=90

end

