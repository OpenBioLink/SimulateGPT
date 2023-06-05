from pathlib import Path
import itertools

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
        [f"experiments/{experiment_name}/ai_messages/{sys}--{human}"
         for sys, human in combinations]


rule simulate:
    input:
        system_message="system_messages/{sys}",
        human_message="experiments/{experiment_name}/prompts/{human}"
    output:
        "experiments/{experiment_name}/ai_messages/{sys}--{human}"
    conda: "env.yml"
    retries: 3
    script:
        "src/rule_simulate.py"
