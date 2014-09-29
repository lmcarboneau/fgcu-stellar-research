PRO wire_bstars_remfreq, remfreq

; Used wire wire_bstars.pro:
; Make a structure with the freq. you want to subtract before calc. a
; partially cleaned amplitude spectrum.

maxfreq  = 10
maxstars = 50

remfreq = replicate( {hd:0L, freq:fltarr(maxfreq)}, maxstars)
 remfreq( 0).hd =  29248 & remfreq( 0).freq(0:3) = [5.65086, 5.76109, 5.61315,11.4022]
 remfreq( 1).hd =  44743 & remfreq( 1).freq(0:1) = [3.980427, 4.180987]
 remfreq( 2).hd = 111123 & remfreq( 2).freq(0:1) = [5.229598, 5.477938]
 remfreq( 3).hd = 126341 & remfreq( 3).freq(0)   = [5.63752]
 remfreq( 4).hd = 129056 & remfreq( 4).freq(0)   = [3.8480]
 remfreq( 5).hd = 149757 & remfreq( 5).freq(0)   = [5.178134]
 remfreq( 6).hd = 158408 & remfreq( 6).freq(0:2) = [0.4019, 0.1955, 0.2012]
 remfreq( 7).hd = 158926 & remfreq( 7).freq(0)   = [4.6795]
 remfreq( 8).hd = 160578 & remfreq( 8).freq(0)   = [5.0039]
 remfreq( 9).hd = 205021 & remfreq( 9).freq(0)   = [5.2484]
 remfreq(10).hd = 159217 & remfreq(10).freq(0)   = [5.11184]

; wire_bstars.pro:
; betacep = [158926L , 111123, 44743, 129056, 160578, 205021, 149757, 158408, 126341, 29248]

g = where(remfreq.hd gt 0,cg)
remfreq = remfreq(g)

END
