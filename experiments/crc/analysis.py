from pathlib import Path
import numpy as np

import re
import scipy
from scipy.stats import pearsonr, spearmanr
import yaml
import re
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt

project_dir = Path("/home/moritz/Projects/Simulator/")
fn = project_dir / "config.yaml"
simulators = yaml.safe_load(Path(fn).read_text())["system_names"]
# palette from sns
simulator_colors = dict(
    zip(simulators, sns.color_palette("colorblind", len(simulators)))
)
plot_simulators = ["baseline", "high_complexity"]
prompts_df = pd.read_csv(
    project_dir / "experiments/crc/crc_apc_impact_2020.csv", index_col=0
)
# simulator = "high_complexity" # "state_based"
res_df = pd.DataFrame()

fig, ax = plt.subplots(1, 1, figsize=(3, 3))
fig2, ax2 = plt.subplots(1, 1, figsize=(3, 3))
fig3, axes3 = plt.subplots(2, 3, figsize=(9, 9), sharex=True, sharey=True)
axes3 = axes3.flatten()

all_dfs = []
for i, simulator in enumerate(simulators):
    dataset = "crc_apc_impact_2020"

    def read_llm_result(i):
        yaml_text = (
            Path(
                project_dir / f"experiments/crc/ai_messages/{simulator}--{dataset}_{i}"
            )
        ).read_text()
        try:
            outcome = yaml.safe_load(yaml_text)["conclusion"]["outcome"]
        except yaml.scanner.ScannerError:
            print(
                project_dir / f"experiments/crc/ai_messages/{simulator}--{dataset}_{i}"
            )
            return None
        return float(np.mean([float(v) for v in re.findall(r"(\d+)", outcome)]))

    prompts_df["llm_result"] = prompts_df.index.map(read_llm_result)
    prompts_df["mae"] = (
        (prompts_df["PFS_MONTHS"] - prompts_df["llm_result"]) ** 1
    ).abs()
    prompts_df["rmsd"] = (prompts_df["PFS_MONTHS"] - prompts_df["llm_result"]) ** 2
    # prediction metrics between columns PFS_MONTHS and llm_result
    res_df.loc[simulator, "correlation"] = prompts_df["PFS_MONTHS"].corr(
        prompts_df["llm_result"], method=lambda x, y: pearsonr(x, y)[0]
    )
    res_df.loc[simulator, "correlation_pval"] = prompts_df["PFS_MONTHS"].corr(
        prompts_df["llm_result"], method=lambda x, y: pearsonr(x, y)[1]
    )
    res_df.loc[simulator, "spearman"] = prompts_df["PFS_MONTHS"].corr(
        prompts_df["llm_result"], method=lambda x, y: spearmanr(x, y)[0]
    )
    res_df.loc[simulator, "spearman_pval"] = prompts_df["PFS_MONTHS"].corr(
        prompts_df["llm_result"], method=lambda x, y: spearmanr(x, y)[1]
    )
    res_df.loc[simulator, "mae"] = (
        ((prompts_df["PFS_MONTHS"] - prompts_df["llm_result"]) ** 1).abs().mean()
    )
    res_df.loc[simulator, "rmsd"] = (
        ((prompts_df["PFS_MONTHS"] - prompts_df["llm_result"]) ** 2).abs().mean()
    )
    res_df.loc[simulator, "r2"] = (
        scipy.stats.linregress(prompts_df["PFS_MONTHS"], prompts_df["llm_result"])[2]
        ** 2
    )
    prompts_df["simulator"] = simulator
    label = f"{simulator} (spearman ρ: {res_df.loc[simulator, 'spearman']:.2f})"
    if simulator in plot_simulators:
        # plot_dfs.append(prompts_df.copy())
        sns.regplot(
            data=prompts_df,
            x="PFS_MONTHS",
            y="llm_result",
            label=label,
            ax=ax,
            color=simulator_colors[simulator],
        )
    sns.regplot(
        data=prompts_df,
        x="PFS_MONTHS",
        y="llm_result",
        label=label,
        ax=ax2,
        color=simulator_colors[simulator],
        ci=None,
        marker="+",
    )
    sns.regplot(
        data=prompts_df,
        x="PFS_MONTHS",
        y="llm_result",
        label=label,
        ax=axes3[i],
        color=simulator_colors[simulator],
        ci=None,
        marker="+",
    )
    axes3[i].set_title(simulator)
    all_dfs.append(prompts_df.copy())

ax.set_title("PFS regression plot")
_ = ax.set(xlabel="ground-truth", ylabel="predicted")
# plot the legend below the axis
ax.legend(
    bbox_to_anchor=(-0.25, -0.5, 1.3, 0.102),
    loc="lower left",
    ncol=1,
    mode="expand",
    borderaxespad=0.0,
)
ax2.legend(
    bbox_to_anchor=(-0.25, -0.8, 1.3, 0.102),
    loc="lower left",
    ncol=1,
    mode="expand",
    borderaxespad=0.0,
)

res_df.to_csv(project_dir / "experiments/crc/result_metrics.csv")
pd.concat(all_dfs).to_csv(project_dir / "experiments/crc/all_data_points.csv")

for f, name in [
    (fig, "baseline_vs_high_reg"),
    (fig2, "all_in_one_reg"),
    (fig3, "subplots_reg"),
]:
    f.savefig(
        project_dir / f"experiments/crc/plots/{name}.png", dpi=300, bbox_inches="tight"
    )
    f.savefig(
        project_dir / f"experiments/crc/plots/{name}.pdf", dpi=300, bbox_inches="tight"
    )
