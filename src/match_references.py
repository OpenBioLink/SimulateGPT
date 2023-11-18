import pandas as pd
import requests
import json
from fuzzywuzzy import fuzz


def get_info(input_doi):
    # Send a request to the Crossref API
    response = requests.get("https://api.crossref.org/works/" + input_doi)

    # Parse the response
    data = json.loads(response.text)

    # If there are no results, return None
    if "title" not in data["message"]:
        title = ""
    else:
        title = data["message"]["title"][0]

    # If there are no results, return None
    if "author" not in data["message"]:
        firstAuth = ""
    else:
        if "family" in data["message"]["author"][0]:
            firstAuth = data["message"]["author"][0]["family"]
        else:
            print(data["message"]["author"][0])
            firstAuth = data["message"]["author"][0]["name"]

    # Otherwise, return the DOI of the first result
    return [title, firstAuth]


def check_author_title(row):
    auth_similarity = fuzz.token_set_ratio(row["ref_text"], row["firstAuthDoi"])
    title_similarity = fuzz.token_set_ratio(row["ref_text"], row["titleDoi"])

    return pd.Series((auth_similarity, title_similarity))


df = pd.read_csv(snakemake.input[0], index_col=0)
if len(df) > 0:
    df[["titleDoi", "firstAuthDoi"]] = df["doi"].apply(get_info).apply(pd.Series)
    df[["author_similarity", "title_similarity"]] = df.apply(check_author_title, axis=1)
else:
    # add the empty columns
    df["titleDoi"] = ""
    df["firstAuthDoi"] = ""
    df["author_similarity"] = ""
    df["title_similarity"] = ""

df.to_csv(snakemake.output[0], index=False)
