import pandas as pd
# import yaml
import os

# requires openpyxl
# conda install -c anaconda openpyxl -> did NOT work
# pip install openpyxl -> worked

# execution from root of repo
os.chdir("../")

### GROUND TRUTH

# Load ground truth Excel file
ground_truth_file = 'expert_feedback/feedback_form_FINAL_groundtruth.xlsx'
ground_truth_df = pd.read_excel(ground_truth_file, sheet_name=None, engine='openpyxl')

# Concatenate all Excel sheets into one large data frame
all_sheets = []
for sheet_name in ground_truth_df.keys():
    if sheet_name == "INSTRUCTIONS":
        continue
    
    sheet_df = pd.read_excel(ground_truth_file, sheet_name=sheet_name, header=3, engine='openpyxl', usecols=["experiment","prompt.1","system"])
    all_sheets.append(sheet_df)

concatenated_ground_truth_df = pd.concat(all_sheets, ignore_index=True)
concatenated_ground_truth_df.rename(columns = {'experiment':'experiment_name', 'prompt.1':'prompt_name', 'system':'system_name'}, inplace = True)

### EXPERT FEEDBACK

expert_dfs = []

# expert = "TK"
for expert in ['expert1', 'expert2', 'expert3']:
    # Load filled out Excel form
    filled_out_file = 'expert_feedback/results/feedback_form_{}.xlsx'.format(expert)
    filled_out_df = pd.read_excel(filled_out_file, sheet_name=None, engine='openpyxl')

    # Concatenate all Excel sheets into one large data frame
    all_sheets = []
    for sheet_name in filled_out_df.keys():
        if sheet_name == "INSTRUCTIONS":
            continue

        sheet_df = pd.read_excel(filled_out_file, sheet_name=sheet_name, header=3, engine='openpyxl')
        sheet_df['expert'] = expert
        sheet_df['type']= sheet_name[-3:]
        all_sheets.append(sheet_df)

    concatenated_df = pd.concat(all_sheets, ignore_index=True)

    # join ground truth columns (experiment, prompt, system)
    expert_dfs.append(concatenated_df.join(concatenated_ground_truth_df))


# concatenate expert data frames into final
final_df = pd.concat(expert_dfs, ignore_index=True)

# Save data frame as CSV for visualization
final_df.to_csv('expert_feedback/results/expert_feedback_results.csv', index=False)
