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
        ["reference_analysis/{}--{}--{}.csv".format(experiment_name, sys, human) for sys, human in combinations]


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
        "reference_analysis/{experiment_name}--{sys}--{human}.csv"
    conda: "env.yml"
    script:
        "src/check_references.py"
