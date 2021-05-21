##**********************
#### 0: Preparation ####
##**********************
# 0a Load modules

import pandas as pd 
import os 
from sas7bdat import SAS7BDAT

# 0b Define icd codes for stroke
# test and pick one type
icdstroke = ['43301', '43311', '43321', '43331', '43381', '43391','43401', '43411',
             '43491', '436', '436', '430' ,'431', '4310', '43100', '432', '4320','43200',
             '4321' ,'4329']
'''icdstroke = [43301, 43311, 43321, 43331, 43381, 43391,43401, 43411,
             43491, 436, 436, 430,431, 4310, 43100, 432, 4320,43200,
             4321 ,4329]'''

# 0c Define the columns we will keep for subsequent analysis
cols_ip = ['ADMDT', 'ADMHR', 'AGE', 'DOB', 'DXA', 'DX01', 'DX02', 'DX03', 'DX04', 'DX05',
        'DX06', 'DX07', 'DX08', 'DX09', 'DX10','DX11', 'DX12', 'DX13', 'DX14', 'DX15',
        'DX16', 'DX17', 'DX18', 'DX19', 'DX20','DX21', 'DX22', 'DX23', 'DX24', 'DX25',
	'ETHNIC', 'PR01', 'PR02', 'PR03', 'PR04', 'PR05', 'RACE', 'SEX', 
	'SOURCE1', 'SOURCE2', 'SOURCE3', 'SOURCE4', 'SOURCE5', 'UPID', 'ZIP', 'TPADM']
# the outpatient datasets do not have TPADM colun
cols_op = cols_ip[0:(len(cols_ip)-1)]

##***********************************
#### 1: Function to Curate SPARCS Admissions ####
##***********************************

# 1a Define function to keep only stroke admissions
def curateStrokes(YY, PP):
    # 1b Move to directory with the data
    os.chdir("N:/CSID/SPARCS/datasets")
    # 1c Define our SAS7BDAT object based on the year and pp
    iter = pd.read_sas(('sparcs_' + PP + YY+ '.sas7bdat'), iterator = True)
    # 1d Set the size of the chunks
    Chunk = 10000
    # 1e Determine the number of chunks we will readin
    # we add one to account for the final bit we readin
    NChunk = int(1 + round(iter.row_count / Chunk))
    # 1f Create the empty dataframe we will fill 
    df = iter.read(0)
    df = df[cols_op]
    # 1g Loop over the chuncks
    for i in range(NChunk):
        # 1g.i For all but the last chunk, readin the set number of rows
        if i < NChunk:   
            NRow = NChunk
        # 1g.ii For the final chunk, readin the number of remaining rows    
        if i == NChunk:
            NRow = iter.row_count - (NChunk-1) * Chunk
        # 1g.iii Readin the rows    
        df2 = iter.read(NRow)
        # 1g.iv Keep only stroke cases
        df2 = df2.query('DXA in @icdstroke | DX01 in @icdstroke | DX02 in @icdstroke | DX03 in @icdstroke')
        # 1g.v Keep only the revelvant columns
        df2 = df2[cols_op]
        # 1g.vi Add this data to that dataframe
        df = df.append(df2)
    # 1h Change directories to the project directory    
    os.chdir('H:/Temp_CVD_Analysis/DataPrep/Data/Raw_data/Outcome_Data')
    # 1i Save as csv
    with open(('raw_' + YY+ "_" + PP+ '_stroke.csv'), 'w') as csvfile:
            df.to_csv(csvfile, index=False)      

##***********************************
#### 2: Curate SPARCS Admissions ####
##***********************************
            
# 2a Curate the data       
'''curateStrokes('95', 'op')
curateStrokes('96', 'op')
curateStrokes('97', 'op')
curateStrokes('98', 'op')
curateStrokes('99', 'op')
curateStrokes('00', 'op')
curateStrokes('01', 'op')
curateStrokes('02', 'op')
curateStrokes('03', 'op')
curateStrokes('04', 'op')
curateStrokes('05', 'op')
curateStrokes('06', 'op')
curateStrokes('07', 'op')
curateStrokes('08', 'op')
curateStrokes('09', 'op') 
curateStrokes('10', 'op') 
curateStrokes('11', 'op')'''

'''curateStrokes('12', 'op')
curateStrokes('13', 'op')
curateStrokes('14', 'op')
curateStrokes('15', 'op')'''

curateStrokes('95', 'ip')
curateStrokes('96', 'ip')
curateStrokes('97', 'ip')
curateStrokes('98', 'ip')
curateStrokes('99', 'ip')
'''curateStrokes('00', 'ip')
curateStrokes('01', 'ip')
curateStrokes('02', 'ip')
curateStrokes('03', 'ip')
curateStrokes('04', 'ip')
curateStrokes('05', 'ip')
curateStrokes('06', 'ip')
curateStrokes('07', 'ip')
curateStrokes('08', 'ip')
curateStrokes('09', 'ip')
curateStrokes('10', 'ip')
curateStrokes('11', 'ip')
curateStrokes('12', 'ip')
curateStrokes('13', 'ip')
curateStrokes('14', 'ip')
curateStrokes('15', 'ip')'''
