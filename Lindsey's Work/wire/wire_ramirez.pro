PRO wire_ramirez, wireobj, wireobj2,  $
 slack_giant_bv=slack_giant_bv, $
 slack_giant_by=slack_giant_by, $
 debug=debug

default9, slack_giant_bv, 0.0 ; allow slightly redder giant stars in B-V calibr.
                              ; should not exceed 0.10
default9, slack_giant_by, 0.0 ; allow slightly redder giant stars in B-V calibr.
                              ; should not exceed 0.10

default9, debug, 0B

; ============================================
; Alpha Cir = HD 128898 has no spectral type: I artikel af Balona
; finder jeg:  Sokolov (1998) obtained T eff= 8440 ± 290 K for alpha
; Cir. We used the spectrum code (Gray & Corbally 1994) to calculate a 
; Kurucz solar-abundance model atmosphere with T eff= 8500 K and logg= 5.00. 

w = where(wireobj.hd eq 128898,c)
wireobj(w).spec1 = 'A5V'
; ============================================

eho_mode = 'replace all'
; eho_mode = 'simbad' ; prefer SIMBAD to EHO, except V !!

wireobj2 = wireobj

; wireobj2.by = -10.;      ; one new value
; wireobj2.v = -90.
; wireobj2.hbeta = -90.    ; Six new values
; wireobj2.c1 = -90.       ; one new value out of 175
; wireobj2.m1 = -90.       ; one new value out of 175

; Get the EHOlsen catalog of Stromgren data
m4_get_basedir, base
restore,base + 'wire_analysis/eho/EHO.IDL'

; Insert the EHO data:
n = n_elements(wireobj)
for i=0,n-1 do begin
 hd = wireobj(i).hd
  w = where(eho.hiphd eq hd,c)

 if c eq 1 then begin

; Only use EHO value if value from SIMBAD does not exist
 if eho_mode eq 'simbad' then begin
  if abs(eho(w).m1)   lt 2  and abs(wireobj(i).m1) gt 2 then wireobj2(i).m1 = eho(w).m1
  if abs(eho(w).c1)   lt 4  and abs(wireobj(i).c1) gt 4 then wireobj2(i).c1 = eho(w).c1
  if abs(eho(w).by)   lt 4  and abs(wireobj(i).by) gt 4 then wireobj2(i).by = eho(w).by
  if abs(eho(w).beta) lt 5  and abs(wireobj(i).hbeta) gt 5 then wireobj2(i).hbeta = eho(w).beta
  if abs(eho(w).v)    lt 15 and abs(wireobj(i).v)  gt 15 then wireobj2(i).v  = eho(w).v 
 endif else begin

; Replace all by EHO:
  if abs(eho(w).m1)   lt 2  then wireobj2(i).m1 = eho(w).m1
  if abs(eho(w).c1)   lt 4  then wireobj2(i).c1 = eho(w).c1
  if abs(eho(w).by)   lt 4  then wireobj2(i).by = eho(w).by
  if abs(eho(w).beta) lt 5  then wireobj2(i).hbeta = eho(w).beta
;;;  if abs(eho(w).v)    lt 15 then wireobj2(i).v  = eho(w).v 
 endelse

 endif

endfor


; Use Stromgren indices to get metallicity: Ramirez et al. 2004, paper I
; Valid for stars with log g > 3.4, ie. lumn class V and IV

w1 = where(abs(wireobj.m1) lt 3. and abs(wireobj.c1) lt 2. and $
            wireobj.by ge 0.15 and wireobj.by lt 0.35 and $
          wireobj.lumclass ge 4 and wireobj.lumclass le 5,c1)
w2 = where(abs(wireobj.m1) lt 3. and abs(wireobj.c1) lt 2. and $
            wireobj.by ge 0.35 and wireobj.by lt 0.50 and $
          wireobj.lumclass ge 4 and wireobj.lumclass le 5,c2)
w3 = where(abs(wireobj.m1) lt 3. and abs(wireobj.c1) lt 2. and $
            wireobj.by ge 0.50 and wireobj.by le 0.80 and $
          wireobj.lumclass ge 4 and wireobj.lumclass le 5,c3)

eta = wireobj.m1 - (0.4 - 3.0 * wireobj.by + 5.6 * (wireobj.by)^2.0 )

; Fix one star with a bad eta value: HD 210839 == O6I super giant!
;wbad = where(eta lt 0. and $
; abs(wireobj.m1) lt 3. and abs(wireobj.c1) lt 2. and $
;            wireobj.by ge 0.19 and wireobj.by lt 0.35,c1bad)
;if c1bad ge 1 then eta(wbad) = median(eta(w1))

if c1 ge 1 then $
 wireobj2(w1).feh = -4.29 - 66.0 * wireobj(w1).m1 + 444.2*wireobj(w1).m1*wireobj(w1).by - $
                    782.4 * wireobj(w1).m1*(wireobj(w1).by)^2.0 + $
                    (0.966-37.8*wireobj(w1).m1 - 1.707*wireobj(w1).c1) * alog10(eta(w1))

if c2 ge 1 then $
 wireobj2(w2).feh = -3.864 + 48.6 * wireobj(w2).m1 - 108.5*(wireobj(w2).m1)^2.0 - $
                    85.2 * wireobj(w2).m1*(wireobj(w2).by) + $
                    190.6 * ((wireobj(w2).m1)^2.0)*(wireobj(w2).by) + $
                   (15.7*wireobj(w2).m1-11.1*wireobj(w2).c1+17.7*wireobj(w2).by)*wireobj(w2).c1

if c3 ge 1 then $
 wireobj2(w3).feh = -2.63 + 23.6 * wireobj(w3).m1 - 41.3*(wireobj(w3).m1)^2.0 - $
                    45.4 * wireobj(w3).m1*(wireobj(w3).by) + $
                    74.0 * ((wireobj(w3).m1)^2.0)*(wireobj(w3).by) + $
                    17.0*wireobj(w3).m1*wireobj(w3).c1


; Ramirez et al. temperature calibrations:
str = [0.4129, 1.2570, -0.2268, -0.0242, -0.0464, -0.0200] ; Stromgren b-y
joh = [0.5002, 0.6440, -0.0690, -0.0230, -0.0566, -0.0170] ; Johnson B-V

; Ignoring the poly fit coefficients

; For the giants:
str2 = [0.5515, 0.9085, -0.1494, 0.0616, -0.0668, -0.0083] ; Stromgren b-y
joh2 = [0.5737, 0.4882, -0.0149, 0.0563, -0.1160, -0.0114] ; Johnson B-V

wireobj3 = wireobj2
wb = where(wireobj2.feh gt 5,cb)
wireobj3(wb).feh = -0.2 ; assume slighly lower metal abundance than in the Sun

; ==============================================================================
; Stromgren calibration for main seq stars:
; ==============================================================================
x = where(wireobj3.by gt 0.23 and wireobj3.by lt 0.8 and $
          wireobj3.lumclass ge 4 and wireobj3.lumclass le 5,cx)
 
teff1 = str(0) + str(1) * wireobj3(x).by + str(2) * (wireobj3(x).by)^2.0 + $
        str(3) * wireobj3(x).by * wireobj3(x).feh + $
        str(4) * wireobj3(x).feh + str(5) * (wireobj3(x).feh)^2.0
teff1 = 5040. / teff1


; Johnson calibration
x2 = where(wireobj3.bv gt 0.3 and wireobj3.bv lt 1.5 and $
          wireobj3.lumclass ge 4 and wireobj3.lumclass le 5,cx2)
 
teff2 = joh(0) + joh(1) * wireobj3(x2).bv + joh(2) * (wireobj3(x2).bv)^2.0 + $
        joh(3) * wireobj3(x2).bv * wireobj3(x2).feh + $
        joh(4) * wireobj3(x2).feh + joh(5) * (wireobj3(x2).feh)^2.0
teff2 = 5040. / teff2

wireobj4 = wireobj3

; Store Johnson results:
wireobj4(x2).teff = teff2
wireobj4(x2).lcfile3 = 'mainseq teff from B-V'

wireobj3(x).teff = teff1
wireobj3(x).lcfile3 = 'mainseq teff from Str. b-y'

; Prefer Stromgren b-y to Johnson B-V
w = where(wireobj3.teff gt 3000.,c)
if c ge 1 then begin
 wireobj4(w).teff    = wireobj3(w).teff
 wireobj4(w).lcfile3 = wireobj3(w).lcfile3
endif
; ==============================================================================





; ==============================================================================
; Stromgren calibration for giant stars:
; ==============================================================================
x = where(wireobj4.by gt 0.05 and wireobj4.by lt (1.08+slack_giant_by) and $
          wireobj4.lumclass ge 2 and wireobj4.lumclass le 3,cx)
x_slack = where(wireobj4.by gt 1.08 and wireobj4.by lt (1.08+slack_giant_by) and $
          wireobj4.lumclass ge 2 and wireobj4.lumclass le 3,cx_slack)

print, ' %%% ' + string(cx) + ' giants with stromgren indices'
print, ' %%% ' + string(cx_slack) + ' giants with stromgren indices (SLACK)'
 
teff1g = str2(0) + str2(1) * wireobj4(x).by + str2(2) * (wireobj4(x).by)^2.0 + $
         str2(3) * wireobj4(x).by * wireobj4(x).feh + $
         str2(4) * wireobj4(x).feh + str2(5) * (wireobj4(x).feh)^2.0
teff1g = 5040. / teff1g


; Johnson calibration
x2 = where(wireobj4.bv gt 0.19 and wireobj4.bv lt (1.67+slack_giant_bv) and $
          wireobj4.lumclass ge 2 and wireobj4.lumclass le 3,cx2)
x2_slack = where(wireobj4.bv gt 1.67 and wireobj4.bv lt (1.67+slack_giant_bv) and $
          wireobj4.lumclass ge 2 and wireobj4.lumclass le 3,cx2_slack)
 
print, ' %%% ' + string(cx2) + ' giants with Johnson indices'
print, ' %%% ' + string(cx2_slack) + ' giants with Johnson indices'


teff2g = joh2(0) + joh2(1) * wireobj4(x2).bv + joh2(2) * (wireobj4(x2).bv)^2.0 + $
         joh2(3) * wireobj4(x2).bv * wireobj4(x2).feh + $
         joh2(4) * wireobj4(x2).feh + joh2(5) * (wireobj4(x2).feh)^2.0
teff2g = 5040. / teff2g

wireobj5 = wireobj4

; Store final Teff for giant stars: also mark SLACK'ed stars:
wireobj4(x).teff = teff1g
wireobj4(x).lcfile3 = 'giant teff from Str. b-y'
if cx_slack ge 1 then wireobj5(x_slack).lcfile3 = 'giant teff from b-y (SLACK-by)'

wireobj5(x2).teff = teff2g
wireobj5(x2).lcfile3 = 'giant teff from B-V'
if cx2_slack ge 1 then wireobj5(x2_slack).lcfile3 = 'giant teff from B-V (SLACK-BV)'


; Prefer Stromgren b-y to Johnson B-V
w = where(wireobj4.teff gt 3000.,c)
if c ge 1 then begin
 wireobj5(w).teff = wireobj4(w).teff
 wireobj5(w).lcfile3 = wireobj4(w).lcfile3
endif

; Debug:
;wx = where(wireobj5.hd eq 206952)
;print,' %%% Teff of giant star HD 206952 : ',teff1g
;print,' %%% Comment: ',wireobj5(wx).lcfile3
;print,' %%% teff: ',wireobj5(wx).teff
; hitme,spp

; ==============================================================================
; END OF GIANTS' TEFF
; ==============================================================================



; ======================================================================
; Effective temperature for hot stars, ie Teff btw. ~50000 and 10000 K !
; BEWARE: Below 10000 K there is no sensitivity
; ======================================================================

; Napiwotzki et al. 1993:
w4 = where(abs(wireobj5.m1) lt 3. and abs(wireobj5.c1) lt 2. and $
            wireobj5.by ge -1.5 and wireobj5.by lt 0.19 and $
            wireobj5.lumclass ge 3 and wireobj5.lumclass le 5,c4)

nap_c1 = wireobj5(w4).c1 - 0.20 * wireobj5(w4).by
nap_m1 = wireobj5(w4).m1 + 0.18 * wireobj5(w4).by
nap_ub = nap_c1 + 2.00 * nap_m1

nap_teff = 0.1692 + 0.2828 * nap_ub - 0.0195 * (nap_ub)^2.0
nap_teff = 5040. / nap_teff


; col=getcolor(/load)
; plot,nap_ub,wireobj5(w4).teff,psym=2,xr=[0,3],yr=[4000,25000]
; oplot,nap_ub,nap_teff,psym=6,col=col.sky,thick=3


wireobj5(w4).teff = nap_teff
wireobj5(w4).lcfile3 = 'hotstar teff from Str. u-b'



; plot,wireobj5(w4).by,nap_teff,psym=2

if debug then begin
 plot,wireobj5.by,wireobj5.v,psym=1,xr=[-.5,3],yr=[7,-1]
 wx = where(wireobj5.teff gt 3000.,c)
 col=getcolor(/load)
 oplot,wireobj5(wx).by,wireobj5(wx).v,psym=2,col=col.sky
endif

wbad = where(wireobj5.teff lt 3000. and wireobj5.by lt .1,c)

; Log g calibration from Moon & Dworetsky for hot stars
; Du kan bruge Smalley & Dworetsky fra 1995. De har et
; syntetisk grid.

; For HD126341 er Hbeta ukendt ... jeg saette den lig. 
; w = where(tef gt 4.2 and tef lt 4.4 and wireobj2.hbeta lt 3.) & print,median(wireobj2(w).hbeta)
whd = where(wireobj5.hd eq 126341,chd)
if chd ge 1 then wireobj5(whd).hbeta = 2.628


for bstar=0,c4-1 do begin
 wire_smalley_logg, wireobj5(w4(bstar)).teff, wireobj5(w4(bstar)).hbeta, smalley_logg
 if smalley_logg ge 0.0 then begin
   wireobj5(w4(bstar)).logg = smalley_logg
   wireobj5(w4(bstar)).lcfile2 = 'logg from smalley for hotstars Hbeta grid'
 endif


; Error estimates of logg -- 5% error on Teff, and 0.002 error on Hbeta
 errt = 0.05  ; relative error on Teff
 errh = 0.002 ; error on Hbeta index 
 wire_smalley_logg, wireobj5(w4(bstar)).teff+ wireobj5(w4(bstar)).teff*errt, $
                    wireobj5(w4(bstar)).hbeta, smalley_logg5
 wire_smalley_logg, wireobj5(w4(bstar)).teff- wireobj5(w4(bstar)).teff*errt, $
                    wireobj5(w4(bstar)).hbeta, smalley_logg6
 wire_smalley_logg, wireobj5(w4(bstar)).teff, $
                    wireobj5(w4(bstar)).hbeta + errh, smalley_logg7
 wire_smalley_logg, wireobj5(w4(bstar)).teff, $
                    wireobj5(w4(bstar)).hbeta - errh, smalley_logg8


 loggall = [smalley_logg, smalley_logg5,smalley_logg6,smalley_logg7,smalley_logg8]
 errlogg = 0.5 * (max(loggall) - min(loggall))
 wireobj5(w4(bstar)).elogg = errlogg ; new 9th of april

                                ; Conclusion: at 9600 K
                                ; the 6% error on Teff dominates, +
                                ; gives + 0.3 log, -teff gives -0.3
                                ; logg error

endfor


; Metallicity for hot stars, Smalley 1993, A&A, 272
w5 = where(abs(wireobj5.m1) lt 3. and $
            wireobj5.hbeta ge 2.72 and wireobj5.hbeta le 2.88 and $
            wireobj5.lumclass ge 3 and wireobj5.lumclass le 5,c5)

strom_gettab,hbzams, m0zams, c0zams

m0z = interpol(m0zams, hbzams, wireobj5(w5).hbeta)
dm0 = m0z - wireobj5(w5).m1

wireobj5(w5).feh = -10.56 * dm0 + 0.081

; =============================================================
;  Determine bolometric corrections:
; =============================================================
wg = where(wireobj5.teff gt 3000 and wireobj5.t2 ge 0.)
bessell, wireobj5(wg).teff, wireobj5(wg).t2, bc
wireobj5(wg).bc = bc

; =============================================================

; =============================================================
; EXPORT THIS STRUCTURE:
; =============================================================
wireobj2 = wireobj5

END
