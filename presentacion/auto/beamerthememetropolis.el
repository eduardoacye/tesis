(TeX-add-style-hook
 "beamerthememetropolis"
 (lambda ()
   (TeX-run-style-hooks
    "etoolbox"
    "pgfopts"
    "pgfplotsthemetol")
   (TeX-add-symbols
    '("plain" ["argument"] 1)
    '("metroset" 1)
    "metropolis"
    "mreducelistspacing"))
 :latex)

