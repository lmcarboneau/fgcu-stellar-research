FUNCTION wire_comb_usno_gsc, usno, gsc, comb

; Combine information from USNO and GSC 
; (routines: queryusno_large_field and querygsc_large_field)
; Determine expected WIRE count rates from 0.5 second integrations
; Determine sizes of plotting symbols!

; Example of the use of this program:
;   querysimbad,'Altair',ra,dec
;   magran = [-5,9] ; get stars in this magnitude range
;   position = dblarr(2) & position(0) = ra & position(1) = dec
;   usno = QueryUSNO_large_field(position, 350., Magrange=magran )
;   gsc = querygsc_large_field(position, 350., Magrange=magran)
;   comb = wire_comb_usno_gsc(usno, gsc)
;   plot,comb.ra,comb.dec,xsty=3,ysty=3,/nodata
;   w = where(comb.wr gt 10.,c) & plotsym,0 ; ,/fill
;   for i=0,c-1 do plots,comb(w(i)).ra,comb(w(i)).dec,psym=8,symsi=comb(w(i)).pz


nn_usno = n_elements(usno)
nn_gsc = n_elements(gsc)

smax = nn_usno + nn_gsc
comb = replicate({id:'', id_usno:'',id_gsc:'', $
                  ra:0D, dec:0D, $
                  obj_type: 0B, v_mag:0.0, v_mag_err:0.0, $
                  r_mag:0., b_mag:0., $
                  wr: 0., $
                  pz: 0.},smax)

comb.v_mag = 99.9
comb.b_mag = 89.9
comb.r_mag = 79.9
comb.pz    = 0.0
comb.v_mag_err = 9.9
comb.id_usno = 'N.A.'
comb.id_gsc = 'N.A.'

; wr = count rate expected from WIRE satellite
; pz = recommended size of plotting symbol (based on count rate)

comb(0:nn_usno-1).id_usno = usno.id
comb(0:nn_usno-1).ra      = usno.ra
comb(0:nn_usno-1).dec     = usno.dec
comb(0:nn_usno-1).r_mag   = usno.r_mag
comb(0:nn_usno-1).b_mag   = usno.b_mag

cnt = 0L
rlim = 0.1 ; must with within 0.1 degrees

for i=0L,nn_gsc-1 do begin
 dist = sqrt( (comb(0:nn_usno-1).ra - gsc(i).ra)^2.0 + $
              (comb(0:nn_usno-1).dec - gsc(i).dec)^2.0 )
 w = where( dist lt rlim, c)

; More than one object found?
 if c ge 2 then begin
  w2 = where(dist(w) eq min(dist(w)),c2)
  w = w(w2) ; select the nearest star
  c = 1
 endif

 if c eq 1 then begin
  comb(w).v_mag     = gsc(i).v_mag
  comb(w).v_mag_err = gsc(i).v_mag_err
  comb(w).obj_type  = gsc(i).obj_type
  comb(w).id_gsc    = gsc(i).id
 endif

 if c eq 0 then begin
  comb(nn_usno+cnt).ra        = gsc(i).ra
  comb(nn_usno+cnt).dec       = gsc(i).dec
  comb(nn_usno+cnt).v_mag     = gsc(i).v_mag
  comb(nn_usno+cnt).v_mag_err = gsc(i).v_mag_err
  comb(nn_usno+cnt).obj_type  = gsc(i).obj_type
  comb(nn_usno+cnt).id_gsc    = gsc(i).id
  cnt = cnt + 1 ; object not in USNO, but in GSC
 endif

endfor

; Remove unused entries
comb = comb(0:nn_usno+cnt-1)

; Write report to screen
 print,' %%% Report: '
 print,strcompress(string(nn_usno),/remove_all) + ' US Naval Obs. objects'
 print,strcompress(string(nn_gsc),/remove_all) + ' Guide Star Cat. objects'
 print,strcompress(string(nn_usno+cnt),/remove_all) + ' objects in combined structure!'

; Add the bright main targets if avail.
ra  = [ 297.69583   ]
dec = [   8.8683224 ]
obj = [ 'Altair'    ]
v_m = [ 0.77 ]
b_m = [ 0.99 ] ; from simbad

nee = n_elements(ra)

   dis_lim = 0.1 ; degrees

; Run though manually entered stars!
for i=0,nee-1 do begin
   dis = (ra(i) - comb.ra)^2. + (dec(i) - comb.dec) ^2.
   dis = sqrt(dis)
   wd = where(dis lt dis_lim,c_d)
   if c_d ge 2 then begin
    wd2 = where(dis(wd) eq min(dis(wd)),c_d2)
    wd = wd(wd2)
    c_d = 1
   endif
   
   if c_d eq 1 then begin
    comb(wd).b_mag = b_m(i)
    comb(wd).v_mag = v_m(i)
    comb(wd).id = obj(i)
    print,' % Found manually entered object: ' + obj(i)

   endif
endfor ; next manually entered star

; Calculate WIRE count rates
 wc = where(comb.v_mag gt -5 and comb.v_mag lt 15. and $
            comb.b_mag gt -5 and comb.b_mag lt 15.,cok)
 comb.wr = -1.0 ; no data available

 if cok ge 1 then begin
  nbv = comb(wc).b_mag - comb(wc).v_mag
   vv = comb(wc).v_mag

  comb(wc).wr = $
   (534400. + 246230.*(nbv)+ 976300*(nbv^2)) * 0.5 * (10.^(-0.4*vv)) ; in 0.5 seconds

 endif

; get symbol plotting sizes
 mag = -2.5 * alog10(comb(wc).wr) + 25.0
 symmag,mag,0.5,1.5,pz 
 comb(wc).pz = pz

return, comb

END
