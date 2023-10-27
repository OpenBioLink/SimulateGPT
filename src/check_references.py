import os
import requests
import json
import yaml
import re
import pandas as pd
from pathlib import Path

def get_doi(input_string):
    # Send a request to the Crossref API
    response = requests.get("https://api.crossref.org/works", params={"query": input_string})

    # Parse the response
    data = json.loads(response.text)

    # If there are no results, return None
    if not data["message"]["items"]:
        return None

    # Otherwise, return the DOI of the first result
    return data["message"]["items"][0]["DOI"]

##### CONFIGURATION

# execution from root of repo
os.chdir("../")
input_path = Path(snakemake.input[0])#os.path.join("experiments/trained_immunity/ai_messages/high_complexity--training_betaglucan") # trained-immunity test
output_path = Path(snakemake.output[0])#os.path.join("/research/home/sreichl/projects/tmp/","{}.csv".format(os.path.basename(input_path)))

##### LOAD DATA

# load ai_message
with open(input_path, 'r') as f:
    yaml_data = yaml.safe_load(f)

##### EXTRACT REFERENCES and QUERY for DOI

# Extract the references
references = yaml_data.get('references', {})

# intitialize reference datafram
ref_df = pd.DataFrame(columns=['file', 'ref_number', 'ref_text', 'doi', 'doi_url'])

# go through every reference and look for DOI
for ref in references.keys():
#     print(references[ref])
    # get doi
    doi = get_doi(references[ref])
    doi_url = "https://doi.org/{}".format(doi)
    # append data to reference dataframe
    ref_df.loc[ref_df.shape[0]] = [input_path, ref, references[ref], doi, doi_url]

##### SAVE RESULTS

# print(ref_df)
ref_df.to_csv(output_path)
