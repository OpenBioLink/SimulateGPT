#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import requests
import json
import yaml
import re
import pandas as pd
from pathlib import Path

def get_info(input_doi):
    # Send a request to the Crossref API
    response = requests.get(
        "https://api.crossref.org/works/"+input_doi
    )

    # Parse the response
    data = json.loads(response.text)
    
    # If there are no results, return None
    if 'title' not in data["message"]:
        title = ''
    else:
        title = data["message"]["title"][0]

    # If there are no results, return None
    if "author" not in data["message"]:
        firstAuth = ''
    else:
        if "family" in  data["message"]["author"][0]:
            firstAuth = data["message"]["author"][0]["family"]
        else:
            print(data["message"]["author"][0])
            firstAuth=data["message"]["author"][0]['name']

    # Otherwise, return the DOI of the first result
    return [title,firstAuth]

# Define your directory path
dir_path = "/Users/rterhorst/Research/Projects/2023-04-19_gptSimulator/SimulateGPT/reference_analysis"

# Get list of all files in directory
file_list = os.listdir(dir_path)

# Create an empty list to store dataframes
df_list = []

# Iterate over the files in directory
for file in file_list:
    # Check if filename contains the desired substring
    if "--high_complexity--" in file:
        # Construct the full file path
        file_path = os.path.join(dir_path, file)
        # Load the data into a pandas DataFrame and append to the list
        df_list.append(pd.read_csv(file_path))

# Concatenate all dataframes in the list along the rows
df = pd.concat(df_list, ignore_index=True)

# Apply the function to the 'doi' column and split the returned list into 2 new columns
df[['titleDoi', 'firstAuthDoi']] = df['doi'].apply(get_info).apply(pd.Series)