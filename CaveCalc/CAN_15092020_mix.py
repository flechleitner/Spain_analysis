"""
Models following the mixing lines calculated from monitoring data ("Soil_mixing_lines.csv")
Run each combination separately, recombine later before input to R.
"""
from cavecalc.forward_models import ForwardModels
import cavecalc.analyse as cca

#
## STEP 1: Define the non-default settings and run the model
#

s =  {  'bedrock_mineral' :     ['Calcite'],
        'bedrock_MgCa':         100, 
        'bedrock_SrCa':         0.6,
        'bedrock_d44Ca':        0.58, 
        'temperature':          12, #4 for LGM 12 for EH

        'soil_pCO2':            [9780],
        'soil_d13C':            -23.04, 
        'soil_R14C':            [90, 95, 100],
        'cave_pCO2' :           [260, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 6000, 8000],
        'cave_R14C':            100, #default value for R14C, as we are only interested in the DCP
        'gas_volume':           [0, 50, 100, 150, 200, 250, 300, 350, 400, 450, 500], # 

        'atm_R14C':             100, 
        'atm_d13C':             -6.7, #-6.7 for EH values from EPICA Dome C (Lourantou et al 2010)
        'atm_pCO2':             260,#180 for LGM 260 for EH
        #'atmo_exchange':        [0, 0.25, 0.5], # 
        #'bedrock_pyrite':       [0, 0.00001] #, 0.0001
     }

dir = './CAN_15092020_mix_10k/' # directory to save model output
p = ForwardModels(  settings =      s,
                    output_dir  =   dir )   # initialise the model
p.run_models()                              # run the model
p.save()                                    # save the model output

#
## STEP 2: Load model output and save new data formats
#
e = cca.Evaluate()      # initialise an 'Evaluate' object
e.load_data( dir )      # load data from dir
f = e.filter_by_index(-1) 

#e.save_all_mat( dir )   # save data to a .mat file
f.save_all_mat( dir )
#

