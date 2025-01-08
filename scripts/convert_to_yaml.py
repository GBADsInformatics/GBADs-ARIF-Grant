import pandas as pd
import sys

extras = '''
species: smallruminants
nruns: 10000
seed_value: NULL\n
'''


if len(sys.argv) != 3: 
    sys.exit(1)

excel_fp = sys.argv[1]
out_path = sys.argv[2]

# FIXME determine whether there is a slash at the end of outpath 
# Check to see if the outpath is actually valid -- also this eventually 
# needs to redirect to s3 

df = pd.read_excel(excel_fp)

keys = df["AHLE Parameter"].tolist()
keys = list(map(str, keys))

df_val = df.iloc[:,2:]

for (col_name, col_data) in df_val.items():
    # print(col_name) # yaml file
    filename = out_path + col_name + ".yaml"
    print(filename)
    file = open(filename, "a")
    header = f"##### {col_name} #####\n"
    file.write(header)
    file.write(extras)
    for idx, data in enumerate(col_data):
        line = ""
        if keys[idx].startswith("#"):
            line = keys[idx]
        elif keys[idx] == "nan" or keys[idx].isspace():
            line = ""
        else:
            val = str(data)
            if ("/" in str(data)) and ("(" not in str(data)):
                nums = str(data).split("/")
                val = str(float(nums[0]) / float(nums[1]))
            elif str(data) == "0.9/(12*4)":
                val = str(0.9 / (12 * 4))
            line = keys[idx] + ": " + val
        line = line + "\n"
        file.write(line)
        # print(line)
    file.close()