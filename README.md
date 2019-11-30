Massive Derelict Satellite Conjunction Analysis
================

This repo is part of a larger project I’m contributing to to monitor and
characterise the risk posed by massive, non-operational satellites in
Low Earth Orbit (LEO). See [here](misc_files/IAC%202017%20Adelaide.pdf)
and [here](misc_files/IAC%202018%20Germany.pdf) for background.

Part of my work involved determining the [need for a new
cluster](misc_files/derelict%20debris%20generating%20risk%20paper.pdf).
This work determines the worst offenders according to two different
algorithms.

See results page
[here](https://rawitner.github.io/conjunction_analysis/).

Files in this repo: - `RDSfiles`: various R data files. They are saved
in this format to make reading and writing files faster. - `conj_data`:
daily files that report information any time two derelicts approached
each other within 5 km. The files start at 20OCT2019, which is when the
new clusters were entered into the external program that generates these
files. - `docs`: HTML files used to render [results
page](https://rawitner.github.io/conjunction_analysis/) - `output`:
various image files of graphics produced over time
