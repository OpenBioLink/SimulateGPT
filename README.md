# Simulator

![logo](simulateGPT_logo.png)


<a target="_blank" href="https://colab.research.google.com/github/OpenBioLink/SimulateGPT/blob/main/SimulateGPT.ipynb">
  <img src="https://colab.research.google.com/assets/colab-badge.svg" alt="Open In Colab"/>
</a>


This repository contains code for the paper **"Large language models are universal biomedical simulators"** (Schaefer et al., 2023).

Computational simulation of biological processes can be a valuable tool in accelerating biomedical research, but usually requires a high level of domain knowledge and extensive manual adaptations. Recently, large language models (LLMs) – such as GPT-4 have proven surprisingly successful in solving complex tasks across diverse fields by emulating human language generation at a very large scale. Here we explore the potential of leveraging LLMs as simulators of biological systems. We establish proof-of-concept of a text-based simulator, **SimulateGPT**, that leverages LLM reasoning. We demonstrate good prediction performance across diverse biomedical use cases without explicit domain knowledge or manual tuning. Our results show that LLMs can be used as versatile and broadly applicable biological simulators.

## Repository structure

Folders:
- system_messages/: GPT-4 system prompts with simple descriptive names, e.g. "simulator_4_markdown"
- experiments/: Protocols, code and results for executed (and planned/running) experiments. For details, see subsection below
  - <experiment_name>/
    - main.md 
    - code or (meta-)data files
    - prompts/
      - ...
    - ai_messages:

## Experiments

Each experiment is kept in a separate folder containing:

- main.md: Experiment documentation (objective, method, results, conclusion) using Markdown (main.md), in addition to the paper's methods section.
- prompts/: prompts for this experiment user prompts
- ai_messages/:  (Chat)GPT4-generated results. File name schema: <system_message>--<prompt_filename>

## Using Snakemake to run experiments

Simply run `snakemake -c1 -k --config experiment_name=<your_experiment_name>` (1 core, continue with undone jobs if a job failed). If you want to use my conda env, add `--use-conda`.

The pipeline generates the files according to the schema indicated above.

## Code files

### src/utils.py

The top-level utils file provides 'everything you need' to run your prompts in an automated fashion. The functions are simple, documented and reflect the defined repository structure.

We streamlined our API access using snakemake.

Make sure to provide your private OPEN AI API key as argument (`api_key`), environment variable (`OPENAI_API_KEY`), or in the password store.

### Notebook 

The Simulator.ipynb notebook is configured to work within colab, but will also work on your local installation.


## Human/Input prompt guidelines

- Provide a starting point for the simulation e.g., a situation or experimental setup or a detailed/complex question that will be answered using a simulation.
  - Optional: Can include/imply a perturbation
- If you expect a final outcome, explicitly request it (use the words) ‘final outcome’
- Optional: You can increase the novelty by adding: "Focus on more novelty."
- The simulator can be used to ask detailed/complex questions about biology. The simulator has the potential to assess the question in more depth and provide more informed answers than the default ChatGPT.

