use MoOSe;

              
                
                
                SELECT * FROM  qc 
				where sample_id in (select sample_id from 
									 sample where run_id=(select run_id from run 
														  where run_name="190125_A00461_0053_BHHGWCDMXX")
								   );