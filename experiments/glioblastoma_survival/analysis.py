from pathlib import Path
import seaborn as sns
import matplotlib.pyplot as plt
import re
import yaml
import pandas as pd

means = pd.read_csv("data/means.csv", index_col=0)
means.index = [eval(i) for i in means.index]


glioblastoma_results = Path("ai_messages")
df = []

# read in LLM output (yaml format) and extract conclusion->outcome field
for res_file in glioblastoma_results.iterdir():
    prompt = res_file.name.split("--")[0]
    if prompt not in "high_complexity".split(
        ", "
    ):  # baseline, medium_complexity, state_based
        continue
    gene_combination = res_file.name.split("--")[1].replace("generated_", "")
    if res_file.name.endswith("swp"):  # vim -.-
        continue
    parsed = yaml.load(res_file.read_text(), Loader=yaml.FullLoader)

    outcome = parsed["conclusion"]["outcome"]
    print(outcome)
    months = int(re.search("\d+", outcome).group())
    if "increased" in outcome and not "decreased" in outcome:
        outcome = "increased"
    elif "decreased" in outcome and not "increased" in outcome:
        outcome = "decreased"
    else:
        outcome = "unchanged"

    genes, mgmt = gene_combination.split("MGMT_")
    mgmt = "MGMT_" + mgmt
    genes = genes.strip("_").split("_") + [mgmt]
    if "" in genes:
        genes.remove("")
    life_expectancy = means.loc[[tuple(genes)]].iloc[0]

    df.append(
        {
            "outcome": outcome,
            "outcome_months": months,
            "system_prompt": res_file.name.split("--")[0],
            "gene_combination": gene_combination,
            "mean_life_expectancy": life_expectancy["mean"],
            "median_life_expectancy": life_expectancy["50%"],
            "gene_tuple": tuple(genes),
        }
    )
df = pd.DataFrame(df)
# Some exploratorive plotting
sns.ecdfplot(
    df.query("system_prompt == 'high_complexity'")[df["outcome"] == "increased"][
        "median_life_expectancy"
    ],
    label="increased",
)
sns.ecdfplot(
    df.query("system_prompt == 'high_complexity'")[df["outcome"] == "decreased"][
        "median_life_expectancy"
    ],
    label="decreased",
)
sns.ecdfplot(
    df.query("system_prompt == 'high_complexity'")[df["outcome"] == "unchanged"][
        "median_life_expectancy"
    ],
    label="unchanged",
)
plt.legend()

# Recover life expectancy in absolute numbers
MEDIAN_LIFE_EXPECTANCY = 15  # derived from our TCGA dataset and provided to prompt
df["predicted_life_expectancy"] = df.apply(
    lambda x: MEDIAN_LIFE_EXPECTANCY + x["outcome_months"]
    if x["outcome"] == "increased"
    else MEDIAN_LIFE_EXPECTANCY - x["outcome_months"],
    axis=1,
)
df["error"] = (
    (12 * df["median_life_expectancy"] / 365) - df["predicted_life_expectancy"]
).abs()
df.sort_values("error", ascending=False)
df.to_csv("data/results.csv")
