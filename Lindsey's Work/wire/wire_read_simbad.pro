PRO wire_read_simbad, simbadfile, sim

; Cut and paste output from SIMBAD, then read it with this program, eg.:
;  wire_read_simbad,'/ai39/bruntt/wire/altair/altair_neighbours_simbad.txt', sim


openr,1,simbadfile
a = strarr(9)

sim = replicate({nam:'',ra:0., dec:0., b:0., v:0., type:'', source:'', nref:0, wr:0.},15000)
sim.nam = 'N.A.'
sim.v = 99.9
sim.b = 89.9
sim.type = 'N.A.'
sim.source = 'N.A.'
sim.nref = 0
sim.ra = 999.9
sim.dec = 888.8

cnt = 0L

while not eof(1) do begin
 readf,1,a,$
  format='(A26,X,A4,X,A14,A15,X,A7,X,A2,A8,X,A12,X,A5)'
 
 sim(cnt).nam    = strcompress(a(0),/remove)
 sim(cnt).source = strcompress(a(1),/remove_all)
 sim(cnt).type   = strcompress(a(7),/remove)
 sim(cnt).nref   = fix(a(8))

; a(5) == variable type ... mira?

 bb = strcompress(a(4),/remove_all)
 vv = strcompress(a(6),/remove_all)
 if bb ne '' then sim(cnt).b = float(bb)
 if vv ne '' then sim(cnt).v = float(vv)

 x = strsplit(a(2),' ',/extract)
  sim(cnt).ra = x(0) * (360./24.)
 if n_elements(x) eq 2 then $
  sim(cnt).ra = x(0) * (360./24.) + x(1) * (360. / (24. * 60.) ) 
 if n_elements(x) eq 3 then $
  sim(cnt).ra = x(0) * (360./24.) + x(1) * (360. / (24. * 60.) ) + x(2) * (360. / (24. * 3600) )

 y = strsplit(a(3),' ',/extract)
 ff = float(y(0)) & s = 1.0 & if ff lt 0. then s = -1. ; GET THE RIGHT SIGN !!!
 sim(cnt).dec = y(0)
 if n_elements(y) eq 2 then $ 
  sim(cnt).dec = y(0) + s * y(1) / 60. 
 if n_elements(y) eq 3 then $
  sim(cnt).dec = y(0) + s * y(1) / 60. + s * y(2) * (1./3600. )

 cnt = cnt + 1 

endwhile
close,1

; Remove unused entries
 sim = sim(0:cnt-1)

w = where(sim.v gt -5 and sim.v lt 20. and $
          sim.b gt -5 and sim.b lt 20.,c)

sim = sim(w)

nbv = sim.b - sim.v
vv = sim.v

sim.wr = (534400 + 246230*(nbv)+ 976300*(nbv^2)) * 0.5 * (10.^(-0.4*vv)) ; in 0.5 seconds

print,' %%% Imported '+strcompress(string(c),/remove_all) + $
      ' stars from file -- stars with both V and B magnitudes...'

END
