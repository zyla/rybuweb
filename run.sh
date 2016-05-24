#!/bin/bash

USER=$(whoami)

# supervisor gives weird environment
export HOME=/home/$USER
export PATH=/home/$USER/.local/bin:$PATH

exec nice -n 19 rybuweb +RTS -N -M512M
