#!/bin/bash
#SBATCH -t 00:01:00
#SBATCH -J msgchain
#SBATCH -o out.%j
#SBATCH -e err.%j
#SBATCH -p test
#SBATCH --nodes=1
#SBATCH -n 10
aprun -n 10 ./chain
