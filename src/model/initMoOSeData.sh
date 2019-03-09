path_root=/tmp/MoOSe_valid

mkdir $path_root

#run 1 incoming data
mkdir $path_root/180306_A00185_0020_BH7NFFDMXX
cp /mnt/seq1/Nova/180306_A00185_0020_BH7NFFDMXX/run_pool5_p28_MAMO_LAZI_THME.csv $path_root/180306_A00185_0020_BH7NFFDMXX

#run 2 data 
mkdir $path_root/180302_A00185_0019_BH7KMTDMXX
cp /mnt/seq1/Nova/180302_A00185_0019_BH7KMTDMXX/CopyComplete.txt $path_root/180302_A00185_0019_BH7KMTDMXX
cp /mnt/seq1/Nova/180302_A00185_0019_BH7KMTDMXX/run_pool4_p28_FAAN.csv $path_root/180302_A00185_0019_BH7KMTDMXX

#run 3 data + bcl2fastq
mkdir $path_root/180306_A00185_0021_AH7TFNDMXX
cp /mnt/seq1/Nova/180306_A00185_0021_AH7TFNDMXX/CopyComplete.txt $path_root/180306_A00185_0021_AH7TFNDMXX
cp /mnt/seq1/Nova/180306_A00185_0021_AH7TFNDMXX/run_pool8_p28_TA_STMA_Laura_NatCom.csv $path_root/180306_A00185_0021_AH7TFNDMXX

mkdir -p $path_root/180306_A00185_0021_AH7TFNDMXX/Data/Intensities/BaseCalls/unaligned/
cp -R /mnt/seq1/Nova/180306_A00185_0021_AH7TFNDMXX/Data/Intensities/BaseCalls/unaligned/Reports $path_root/180306_A00185_0021_AH7TFNDMXX/Data/Intensities/BaseCalls/unaligned/


#run 4 data + bcl2fastq + removed
mkdir $path_root/180312_A00461_0009_AH5WTYDMXX
cp /mnt/seq1/Nova/180312_A00461_0009_AH5WTYDMXX/CopyComplete.txt $path_root/180312_A00461_0009_AH5WTYDMXX
cp /mnt/seq1/Nova/180312_A00461_0009_AH5WTYDMXX/run_pool8_p28_TA_STMA_Laura_NatCom.csv $path_root/180312_A00461_0009_AH5WTYDMXX

mkdir -p $path_root/180312_A00461_0009_AH5WTYDMXX/Data/Intensities/BaseCalls/unaligned/
cp -R /mnt/seq1/Nova/180312_A00461_0009_AH5WTYDMXX/Data/Intensities/BaseCalls/unaligned/Reports $path_root/180312_A00461_0009_AH5WTYDMXX/Data/Intensities/BaseCalls/unaligned/
cp -r  /mnt/seq1/Nova/180306_A00185_0021_AH7TFNDMXX/Data/Intensities/BaseCalls/unaligned/Stats/   /tmp/MoOSe_valid/180306_A00185_0021_AH7TFNDMXX/Data/Intensities/BaseCalls/unaligned/

chmod -R 777 $path_root
