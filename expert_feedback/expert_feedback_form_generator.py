import pandas as pd
import yaml
import os

# requires xlsxwriter
# conda install -c conda-forge xlsxwriter -> did not work
# pip install XlsxWriter -> works

# strip the document end marker
def strip_document_end_marker(s):
    if s.endswith('...\n'):
        return s[:-5]
    else:
        return s


# execution from root of repo
# os.chdir("../")

# Create Excel writer
writer = pd.ExcelWriter('expert_feedback/feedback_form.xlsx', engine='xlsxwriter')

# add an empty sheet called INSTRUCTIONS
pd.DataFrame().to_excel(writer, sheet_name="INSTRUCTIONS", index=False)

# set ground truth columns for evaluation
id_cols = ['experiment','prompt','system']

# Load question file
with open('expert_feedback/questions.txt', 'r') as f:
    questions = [line.strip() for line in f.readlines()]

# load annotation file
annot = pd.read_csv('expert_feedback/annotation.csv')

# loop through all experiments
experiments = annot.groupby('experiment', sort=False)

for experiment, experiment_data in experiments:
    # Create empty DataFrame with columns as template for each sheet
    df = pd.DataFrame(columns=['prompt', 'simulation', 'outcome', 'explanation'] + questions + ["comment"] + id_cols)
    
    for prompt_name in experiment_data['prompt']:

        # Load prompt file
        with open(os.path.join('experiments',experiment,'prompts',prompt_name), 'r') as f:
            prompt = f.read()

                  
        for system_prompt in ['baseline', 'high_complexity']:
            
            # Load YAML file
            with open(os.path.join('experiments',experiment,'ai_messages','{}--{}'.format(system_prompt, prompt_name)), 'r') as f:
                yaml_data = yaml.safe_load(f)

            # append to the end of DataFrame
            df.loc[df.shape[0]] = [prompt, 
                                       yaml.dump(yaml_data, sort_keys=False, width=1000, indent=2),
                                       strip_document_end_marker(yaml.dump(yaml_data['conclusion']['outcome'], sort_keys=False, width=1000, indent=2)),
                                       strip_document_end_marker(yaml.dump(yaml_data['conclusion']['explanation'], sort_keys=False, width=1000, indent=2))
                                      ] + ['' for _ in questions] + [''] + [experiment, prompt_name, system_prompt]
        
    # shuffle the rows randomly
    df = df.sample(frac=1)
    
    # Write each subset into a separate sheet of the Excel file (FYI: Excel worksheet name must be <= 31 chars -> short postfix _OUT, _EXP, _SIM)
    df.drop(['simulation', 'explanation'], axis=1).to_excel(writer, sheet_name="{}_{}".format(experiment, 'OUT'), index=False, header=False, startrow = 4, freeze_panes = (4,2))
    df.drop(['simulation'], axis=1).to_excel(writer, sheet_name="{}_{}".format(experiment, 'EXP'), index=False, header=False, startrow = 4, freeze_panes = (4,3))
    df.drop(['outcome', 'explanation'], axis=1).to_excel(writer, sheet_name="{}_{}".format(experiment, 'SIM'), index=False, header=False, startrow = 4, freeze_panes = (4,2))

    
    
# FORMAT workbook

# Get the workbook and worksheet objects
workbook = writer.book

# define formatting for questions
general_format = workbook.add_format({'bold': True,
                                       'font_color': 'black',
                                       'locked': True,
                                       'text_wrap': True,
                                       'bg_color': '#FDFD96',
                                       'border': True,
                                      'align': 'center',
                                      'valign': 'vcenter'
                                      })
question_format = workbook.add_format({'bold': True,
                                       'font_color': 'black',
                                       'locked': True,
                                       'text_wrap': True,
                                       'bg_color': '#C1E1C1',
                                       'border': True,
                                       'align': 'center',
                                       'valign': 'vcenter'
                                      })


# Write the first row to each sheet
for sheet_name in writer.sheets:
    # skip INSTRUCTIONS sheet
    if sheet_name=="INSTRUCTIONS":
        continue
    
    # get the worksheet
    worksheet = writer.sheets[sheet_name]
    
    ## add and format HEADER 

    # set header_questions
    header_questions = questions + ["comment"]
    
    # depending on the sheet the header_general differ and widths
    if "_SIM" in sheet_name:
        header_general = ['prompt', 'simulation']
        worksheet.set_column(first_col=0, last_col=1, width = 100)
        worksheet.set_column(first_col=2, last_col=2+len(header_questions), width = 25)
    elif "_EXP" in sheet_name:
        header_general = ['prompt', 'outcome', 'explanation']
        worksheet.set_column(first_col=0, last_col=0, width = 100)
        worksheet.set_column(first_col=1, last_col=2, width = 50)
        worksheet.set_column(first_col=3, last_col=3+len(header_questions), width = 25)
    elif "_OUT" in sheet_name:
        header_general = ['prompt', 'outcome']
        worksheet.set_column(first_col=0, last_col=0, width = 100)
        worksheet.set_column(first_col=1, last_col=1, width = 50)
        worksheet.set_column(first_col=2, last_col=2+len(header_questions), width = 25)
    
    header_id = id_cols
    
    # write and format headers
    worksheet.write_row(row=3, col=0, data=header_general, cell_format=general_format)
    worksheet.write_row(row=3, col=len(header_general), data=header_questions, cell_format=question_format)
    worksheet.write_row(row=3, col=len(header_general)+len(header_questions), data=header_id)
    
    ## LIKERT SCALE
    # insert likert scale as image
    worksheet.insert_image(row=0, col=0, filename="expert_feedback/likert_scale.png", options={'object_position': 3,
                                                                                               'x_scale': 0.5,
                                                                                               'y_scale': 0.5})

# Save Excel file
writer.save()