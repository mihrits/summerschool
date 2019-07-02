#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -p gpu
#SBATCH -t 00:01:00
#SBATCH -J acc043
#SBATCH --gres=gpu:p100:1
#SBATCH --reservation=Summerschool

module load cuda/10.0 pgi/19.1 openmpi/3.1.4 libpng/1.6
module list

srun ./a.out
