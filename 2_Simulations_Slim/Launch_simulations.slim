#!/bin/bash -l
#SBATCH -A Prject number
#SBATCH -p core -n 1
#SBATCH -t 8-00:00:00
#SBATCH -J slim_reindeer

#job id: $SLURM_JOB_ID
#job name (-J): $SLURM_JOB_NAME
#tmp directory: $SNIC_TMP

module load bioinfo-tools
module load SLiM/4.0.1


#run the command on screen as a check
#slim -s 1 -d K1=50000 -d K2=500 -d K_bot=100 -d seqLength=20000000 Reinder_nonWF_Founder_50_30_30.slim
#for S in $(shuf -i 1-100000 -n 1); do for K2 in 500 ; do for K_bot in 50 ; do slim -s $S -d K1=8000 -d K2=${K2} -d K_bot=${K_bot} -d seqLength=20000000 Reinder_nonWF_Founder_50_30_30.slim; done; done; done

#
#K2=founder population size
#K3=population recovery size c. 1200 years BP (kept fixed at 20000)
#K_bot= recent bottleneck population size
#
#usage for launching parallel jobs via slurm
#for S in $(shuf -i 1-100000 -n 30); do for K2 in 25 50 100 250 500; do for K_bot in 1800 500 ; do sbatch launch_slim.bsh $S $K2 $K_bot; done; done; done


#### command
seed=$1
K2=$2
K_bot=$3

echo 'Running with seed=' $1 'K2=' $2 'K_bot=' $3
slim -s $seed -d K1=50000 -d K2=${K2} -d K3=20000 -d K_bot=${K_bot} Reindeer_non_WF_Founder_recovery_bottleneck_genes.slim
