; Plot ampl. spektre + target identifikation for WIRE targets:
; (c) July 2004 H. Bruntt

spawn,'ls -1 /ai40/bruntt/wire/wire_lc/wire_lc_*imp_clean.idl',ll

wire_red_setup, position_file, target_file
restore,position_file
; Fil indeholder wireinfo og xyinfo! Fra programmet wire_auto_field.pro

outfile = '/ai40/bruntt/wire/wire_summary.txt'
close,1
openw,1,outfile

nl = n_elements(ll)

for i=0,nl-1 do begin

a = strsplit(ll(i),'_',/extract)
w = where(a eq 'star',c)
base = ''
for j=0,w(0)+1 do base = base + a(j) + '_'

improve_file = base + 'imp_clean.idl'
clean_file   = base + 'clean.idl'

aa = findfile(improve_file,Count=cnt1)
bb = findfile(clean_file,  Count=cnt2)


main_target_name = a(w(0)-1)
slot_num = a(w(0)+1)

wmain = where(strmatch(wireinfo.dir,'*'+main_target_name+'*') eq 1,cmain)
wmain = wmain(0)

if cmain ne 1 then begin
 print,' %%% Main target not found in IDENTIFICATION structure: '+main_target_name
 object_info = 'Missing Identification'
 stop
endif else begin
 object_info = xyinfo(wmain).object(slot_num)
endelse

print,' %%% Object identified as: ' + object_info

;n = wireinfo(wmain).nstars ; number of slots
;print,' %%% Identified stars: '
;for i=0,n-1 do print,i,xyinfo(wmain).object(i),format='(I3,X,A20)'


; Do the req. files exist?
if cnt1 eq 1 and cnt2 eq 1 then begin
 restore, improve_file ; fc2 structure --> significant peaks
 restore, clean_file   ; fc
 wire_clean_plot,fc,auto=1,fc2=fc2,name=base,id=object_info

 printf,1,object_info + '  (' + ll(i) + ')
 n2 = n_elements(fc2)

 nf = n_elements(fc) & f1 = min(fc(nf-1).freq) & f2 = max(fc(nf-1).freq) 
 wire_calc_ampl_noise, fc(nf-1).freq, fc(nf-1).amp, f1,f2, 50, noise
 n_at_f = interpol(noise(1,*), noise(0,*), fc2.f)
 sn_at_f = fc2.a / n_at_f

 ; n_at_f_org = interpol(noise(1,*), noise(0,*), fc.f)
 ; sn_at_f_org = fc.a / n_at_f

; Guess the frequency unit:
 ff = max(fc(0).freq)
 if ff gt 40. and ff lt 80 then f_conv =  1. ; c/day
 if ff lt 10.  then f_conv =  11.574 / 1e3 ; milliHz
 if ff gt 500. then f_conv =  11.574  ; microHz
 f_orbit = 15.345 * f_conv ; 15.345 = orbital freq. in spring 2004.

; Pick modes that are not multiples of the orbit frequency (+- 0.15 c/day):
 nm = n_elements(fc2)

 orb = bytarr(nm)
 for k=0,nm-1 do begin
  if abs(f_orbit      - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
  if abs(f_orbit * 2. - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
  if abs(f_orbit * 3. - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
  if abs(f_orbit * 4. - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
  if abs(f_orbit * 5. - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
  if abs(f_orbit * 6. - fc2(k).f) lt 0.15 * f_conv then orb(k) = 1 ; obital freq
 endfor

 w_not_orb = where(orb eq 0,c_not_orb)

 nmode = c_not_orb
 nmode_use = nmode
 if nmode gt 5 then nmode_use = 5 ; export < 5 modes

 for mode=0,nmode_use-1 do begin
  printf,1,fc2(w_not_orb(mode)).f, fc2(w_not_orb(mode)).a, sn_at_f(w_not_orb(mode)), $
   format='(F9.3, X, I8, X, F7.1)'
  print,fc2(w_not_orb(mode)).f, fc2(w_not_orb(mode)).a, sn_at_f(w_not_orb(mode)), $
   format='(F9.3, X, I8, X, F7.1)'
 endfor

; print,' Hit ... ' & s = get_kbrd(1) & if s eq 'x' then stop

 printf,1,' ---------------------------- '


endif



endfor

close,1

print, ' %%% Output file: ' + outfile

END
