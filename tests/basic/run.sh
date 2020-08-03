#!/bin/sh

../../universo -l -o out  --theory theory/cts.dk --config universo_cfg.dk -I theory/ in/test2.dk # pass

../../universo -l -o out  --theory theory/cts.dk --config universo_cfg.dk -I theory/ in/test.dk # fail
