###################################################################################################################################
###################################################################################################################################
#############################################                                  ####################################################
#############################################            Linux 常用命令         ####################################################
#############################################                                   ###################################################
###################################################################################################################################
###################################################################################################################################

###################################################################################################################################
#########################################            Duration of disease         ##################################################
###################################################################################################################################

# 编辑
vi duration_disease.pbs

#----------------------------------
#PBS -N duration_disease
#PBS -l nodes=1:ppn=8
#PBS -l walltime=18:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com
cd ~/2021-02_CMMtodeath
/public/software/stata17/stata-mp -b $dofile $endpoint
#----------------------------------

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_disease.phs -v dofile=0.Code/04.Disease_durationV1.do,endpoint=$i ; 
done

###################################################################################################################################
#########################################             Duration  table            ##################################################
###################################################################################################################################

# 编辑
vi duration_dtable.pbs

#----------------------------------
#PBS -N duration_disease_table
#PBS -l nodes=1:ppn=8
#PBS -l walltime=18:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com
cd ~/2021-02_CMMtodeath
/public/software/stata17/stata-mp -b $dofile $endpoint
#----------------------------------

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/11.duration_tableV2.do,endpoint=$i ;
done

eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/11.duration_tableV3.do,endpoint=$i ;
done

eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/04.duration_tableV4.do,endpoint=$i ;
done

eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/04.duration_tableV4_nochg.do,endpoint=$i ;
done



###################################################################################################################################
#########################################             Duration  graph            ##################################################
###################################################################################################################################

# 编辑
vi duration_dtable.pbs

#----------------------------------
#PBS -N duration_disease_table
#PBS -l nodes=1:ppn=8
#PBS -l walltime=18:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com
cd ~/2021-02_CMMtodeath
/public/software/stata17/stata-mp -b $dofile $endpoint
#----------------------------------

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/06.DurationRCSgraphV1.do,endpoint=$i ;
done

eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/06.DurationRCSgraphV2.do,endpoint=$i ;
done

####### qsub循环 find cutoff
eplist="du_ep0001"
for i in $eplist; do 
	qsub duration_dtable.pbs -v dofile=0.Code/04.RCScutpointsV1.do,endpoint=$i ;
done
###################################################################################################################################
#########################################            Diff dicho disease          ##################################################
###################################################################################################################################

####### 编辑
vi diff_exposure_CMMV1.pbs

#----------------------------------
#PBS -N diff_biexposure
#PBS -l nodes=1:ppn=8
#PBS -l walltime=18:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com

echo begin time is `date`
cd ~/2021-02_CMMtodeath
source ~/.bashrc
stata-mp -b $dofile $endpoint
echo end time is `date`
#----------------------------------

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub diff_exposure_CMMV1.pbs -v dofile=0.Code/03.Diff_type_exposureV3.do,endpoint=$i ; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub diff_exposure_CMMV1.pbs -v dofile=0.Code/03.Diff_type_exposureV3_nochg.do,endpoint=$i ; 
done
###################################################################################################################################
#########################################          只更改一个dofile的pbs文件       ##################################################
###################################################################################################################################

####### 编辑
vi statado.pbs

#----------------------------------
#PBS -N statado
#PBS -l nodes=1:ppn=8
#PBS -l walltime=18:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com

echo begin time is `date`
cd ~/2021-02_CMMtodeath
source ~/.bashrc
stata-mp -b $dofile
echo end time is `date`
#----------------------------------

qsub statado.pbs -v dofile=0.Code/01.Datacleaning_V4_forcloud.do
qsub statado.pbs -v dofile=0.Code/01.Datacleaning_V5_forcloud.do
qsub statado.pbs -v dofile=0.Code/01.Datacleaning_V6_forcloud.do
qsub statado.pbs -v dofile=0.Code/05.DXDV1.do
qsub statado.pbs -v dofile=0.Code/05.DXDV2.do
qsub statado.pbs -v dofile=0.Code/test.do
qsub statado.pbs -v dofile=0.Code/02.Cohort_characteristics_cloudV2.do



###################################################################################################################################
#########################################          更改dofile和结局的pbs文件       ##################################################
###################################################################################################################################
####### 编辑
vi statado_multi.pbs

#----------------------------------
#PBS -N statado_multi
#PBS -l nodes=1:ppn=8
#PBS -l walltime=100:00:00
#PBS -o /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -e /public/home/hanyuting/2021-02_CMMtodeath/2.Result/phs_log
#PBS -m ae
#PBS -M 799683226@qq.com

echo begin time is `date`
cd ~/2021-02_CMMtodeath
source ~/.bashrc
stata-mp -b $dofile $endpoint
echo end time is `date`
#----------------------------------

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/05.DXDV2.do,endpoint=$i ; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/05.DXD_baselineV1.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/041.RCScutpoints.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/07.SubgroupsfordiseaseV2.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/08.SubgroupsfordiseasedurationV2.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/09.SubgroupsforRCSgraphV1.do,endpoint=$i; 
done


####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/07.SubgroupsfordiseaseV3.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/12.SensitivityDrop2y.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/13.Sensitivityadj_Med.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/14.Sensitivitynochg.do,endpoint=$i; 
done

####### qsub循环
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
for i in $eplist; do 
	qsub statado_multi.pbs -v dofile=0.Code/15.Sensitivitynochg_duration.do,endpoint=$i; 
done




qsub statado_multi.pbs -v dofile=0.Code/11.duration_tableV4_temp.do,endpoint="du_ep0001"
qsub statado_multi2.pbs -v dofile=0.Code/11.duration_tableV4_temp.do,endpoint="du_ep0001"

###################################################################################################################################
##################################          更改dofile、结局和exposure的pbs文件       ###############################################
###################################################################################################################################
# Incident dicho disease
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
exposure_list="diabetes_diag_updated chd_diag_updated stroke_diag_updated"
for i in $eplist; do
	for j in $exposure_list; do
			qsub statado_multi2.pbs -v dofile=0.Code/03.Diff_type_exposureV5_nochg_new.do,endpoint=$i,exposure=$j ;
	done
done

# Updated duration of disease
eplist="du_ep0001 du_ep0002 du_ep0014 du_ep0032 du_ep9999"
exposure_list="duration_diabetes_updated_8g duration_chd_updated_8g duration_stroke_updated_8g"
for i in $eplist; do
	for j in $exposure_list; do
			qsub statado_multi2.pbs -v dofile=0.Code/04.duration_tableV6_nochg.do,endpoint=$i,exposure=$j ;
	done
done


###################################################################################################################################
#########################################                  Tips                  ##################################################
###################################################################################################################################
# Delete all running 
qstat -u hanyuting | grep "R" | cut -d" " -f1 | xargs qdel
qstat -u hanyuting | grep "Q" | cut -d" " -f1 | xargs qdel