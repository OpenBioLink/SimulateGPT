from pathlib import Path
import itertools
import os

configfile: "config.yaml"
experiment_name = config["experiment_name"]

human_names = [
    name.stem
    for name in Path(f"experiments/{experiment_name}/prompts").iterdir()
    if name.is_file()
]

combinations = list(itertools.product(config["system_names"], human_names))

rule all:
    input:
        f"reference_analysis/{experiment_name}_all.csv"

rule simulate:
    input:
        system_message=ancient("system_messages/{sys}"),
        human_message=ancient("experiments/{experiment_name}/prompts/{human}")
    output:
        protected("experiments/{experiment_name}/ai_messages/{sys}--{human}")
    conda: "env.yml"
    retries: 3
    script:
        "src/rule_simulate.py"

rule analyze_references:
    input:
        ancient("experiments/{experiment_name}/ai_messages/{sys}--{human}")
    output:
        "reference_analysis/cross_ref/{experiment_name}--{sys}--{human}.csv"
    conda: "env.yml"
    script:
        "src/check_references.py"

rule match_references:
    input:
        rules.analyze_references.output
    output:
        "reference_analysis/matched/{experiment_name}--{sys}--{human}.csv"
    conda: "env.yml"
    script:
        "src/match_references.py"

rule aggregate_matched_references:
    input:
        ["reference_analysis/matched/{}--{}--{}.csv".format(experiment_name, sys, human) for sys, human in combinations]
    output:
        f"reference_analysis/{experiment_name}_all.csv"
    run:
        import pandas as pd
        dfs = [
            pd.read_csv(input_fn)
            for input_fn in input
        ]
        df = pd.concat(dfs)
        df.to_csv(output[0], index=False)
