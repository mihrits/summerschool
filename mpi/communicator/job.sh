#!/bin/bash
#SBATCH -t 00:01:00
#SBATCH -J communicators
#SBATCH -o out.%j
#SBATCH -e err.%j
#SBATCH -p test
#SBATCH --nodes=1
#SBATCH -n 4
aprun -n 4 ./prog
