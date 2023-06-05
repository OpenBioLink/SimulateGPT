"""
Generate prompts for Glioblastoma survival simulation scenario
"""

import pandas as pd
from pathlib import Path
import time, requests
from langchain.prompts import StringPromptTemplate
from pydantic import BaseModel, validator

# Load data
df = pd.read_csv(f"data/clinical.tsv", sep="\t")
df = df.query("project_id == 'CPTAC-3'")

# Load methylation data
mgmt_df = pd.read_csv(
    "data/gbm_cptac_2021/data_methylation_epic_MGMT.txt", sep="\t"
)  # downloaded from cBioPortal and filtered for 'MGMT' (using grep to reduce file size)
mgmt_df = mgmt_df.loc[
    mgmt_df.Hugo_Symbol == "MGMT"
]  # only necessary if not pre-filtered
df.set_index("case_submitter_id", inplace=True)
df["mgmt_methylation"] = mgmt_df.describe().loc[
    "mean"
]  # Aggregated methylation levels for MGMT
df["mgmt_methylation"] = df["mgmt_methylation"].fillna(0)
df.reset_index(inplace=True)

# top 20 mutated genes for glioblastomas genes from TCGA
top10_genes = (
    "PTEN, TP53, EGFR, MUC16, NF1, PIK3CA, RB1, ATRX, PIK3R1, IDH1, "
    "CNTNAP2, PDGFRA, GRIN2A, CSMD3, GRM3, FAT4, TRRAP, FLNA, ANK1, STAG2".split(", ")[
        :10
    ]
)

# Download cases for genotypes using API
URL = 'https://api.gdc.cancer.gov/ssm_occurrences?filters={"op":"=","content":{"field":"case.case_id","value":"%s"}}&fields=ssm.consequence.transcript.gene.symbol,ssm.consequence.transcript.consequence_type,ssm.genomic_dna_change&format=json&size=100000'

# API-requested all cases takes a while, so we cache the results in data/cases.csv
try:
    case_genes = pd.read_csv(
        "data/cases.csv",
        index_col=0,
    )["0"].to_dict()

    # case_genes is a dict, with a python string-encoded list of genes as its values. Converting to list
    case_genes = {
        k: v.strip("[]").replace("'", "").split(", ") for k, v in case_genes.items()
    }

except NameError:
    case_genes = {}

    for case_id in df["case_id"].drop_duplicates():
        if case_id in case_genes and len(case_genes[case_id]) > 0:
            continue
        # query the API, get the response as dict (json)
        response = requests.get(URL % case_id).json()
        hits = response["data"]["hits"]
        assert response["data"]["pagination"]["pages"] == 1  #

        genes = [
            hit["ssm"]["consequence"][0]["transcript"]["gene"][
                "symbol"
            ]  # could capture the mutation-type as well
            for hit in hits
        ]

        case_genes[case_id] = genes
        time.sleep(0.1)
    pd.Series(case_genes).to_csv("data/cases.csv")


# Aggregate cases (not relevant for CPTAC I believe)

# group by case_id and take the string concatenation of the deduplicated group
def agg_same_case(x):
    if len(x.drop_duplicates()) == 1 or not isinstance(x.iloc[0], str):
        return x.value_counts().index[0]
    else:
        return "__".join(x.drop_duplicates())


case_df = df.groupby("case_id")[
    [
        "age_at_diagnosis",
        "cause_of_death",
        "project_id",
        "days_to_death",
        "classification_of_tumor",
        "treatment_or_therapy",
        "treatment_type",
        "treatment_intent_type",
        "mgmt_methylation",
    ]
].agg(agg_same_case)

# Filter out cases without a death date
case_df = case_df[case_df["days_to_death"] != "'--"]

case_df["genes"] = case_df.index.map(lambda case_id: case_genes.get(case_id, []))
case_df["top10_filtered_genes"] = case_df["genes"].map(
    lambda genes: tuple(gene for gene in top10_genes if gene in genes)
)


# Add the "MGMT gene"
def _add_mgmt_methyl(mgmt_level):
    """
    Splits were inferred visually from the data distribution
    """
    if mgmt_level < 0.62:
        return "MGMT_low_methylated"
    elif mgmt_level > 0.7:
        return "MGMT_high_methylated"
    else:
        return "MGMT_medium_methylated"


case_df["top10_filtered_genes"] = case_df.apply(
    lambda row: tuple(list(row["top10_filtered_genes"])),
    axis=1,
)

cases_by_mutations = (
    case_df.reset_index().groupby("top10_filtered_genes")["case_id"].apply(list)
)
cases_by_mutations = cases_by_mutations.sort_values(
    key=lambda x: x.apply(len), ascending=False
).iloc[
    1:
]  # remove empty (most prevalent)

means = cases_by_mutations.apply(
    lambda x: case_df.loc[x, "days_to_death"].astype(int).describe()
)
means["methylation"] = cases_by_mutations.apply(
    lambda x: _add_mgmt_methyl(case_df.loc[x, "mgmt_methylation"].astype(float).mean())
)
means.index = means.apply(lambda v: tuple([*v.name, v["methylation"]]), axis=1)
means = means[means["count"] >= 4]  # only retain groups with at least 4 cases

# Generate the prompts
Path("prompts").mkdir(exist_ok=True)


class GlioblastomaPromptTemplate(StringPromptTemplate, BaseModel):
    """A custom prompt template that takes in the function name as input, and formats the prompt template to provide the source code of the function."""

    @validator("input_variables")
    def validate_input_variables(cls, v):
        """Validate that the input variables are correct."""
        if len(v) != 2 or "genes" not in v or "wt_genes" not in v:
            raise ValueError("[wt_]genes must be the input_variables.")
        return v

    def format(self, **kwargs) -> str:
        # Load the prompt
        prompt = Path("prompt_template").read_text()

        genes = kwargs["genes"]
        wt_genes = kwargs["wt_genes"]
        # if len(genes) == 1:
        #     genes = f"the gene {genes[0]}"
        # else:
        #     genes = "the genes " + ", ".join(genes[:-1]) + " and " + genes[-1]

        # wt_genes = ", ".join(wt_genes[:-1]) + " and " + wt_genes[-1]
        genes = "\n".join(
            [
                f"{gene}: mutations found"
                if not gene.startswith("MGMT")
                else f"MGMT methylation status: {gene.split('_')[1]}"
                for gene in genes
            ]
            + [f"{gene}: no mutations found" for gene in wt_genes]
        )

        return prompt.format(genes=genes, wt_genes=wt_genes)

    def _prompt_type(self):
        return "glioblastoma"


glioblastoma_prompt = GlioblastomaPromptTemplate(input_variables=["genes", "wt_genes"])

all_genes = set([c for case in means.index for c in case]) - {
    "MGMT_low_methylated",
    "MGMT_medium_methylated",
    "MGMT_high_methylated",
}

# Generate multi-gene cases
for mutations in means.index:
    prompt = glioblastoma_prompt.format(
        genes=mutations, wt_genes=list(all_genes - set(mutations))
    )
    (Path("prompts") / f"generated_{'_'.join(mutations)}").write_text(prompt)

means.to_csv("./data/means.csv")
