import os
from pathlib import Path
import subprocess
from typing import Optional
import itertools

from langchain.callbacks.base import CallbackManager

from langchain.callbacks.streaming_stdout import StreamingStdOutCallbackHandler

from langchain.chat_models import ChatOpenAI
from langchain.schema import HumanMessage, SystemMessage

batch_chat = None
stream_chat = None

PROJECT_DIR = Path(__file__).parent


def load_openai_api(api_key: Optional[str] = None, model="gpt-4"):
    """Make sure OpenAI API key is available. If not, then load it

    Args:
        api_key: The OpenAI API key to use. If None, then try to load it from the environment variable OPENAI_API_KEY or from the password store.
        model: The model to use. Defaults to "gpt-4". alternatively use gpt-3.5-turbo for faster and cheaper exploration
    """
    global batch_chat, stream_chat
    try:
        os.environ["OPENAI_API_KEY"]
    except KeyError:
        if api_key is None:

            def get_password_from_pass_store(key: str) -> str:
                # Run the pass command to get the password for the specified key
                password = subprocess.run(["pass", key], capture_output=True, text=True)

                # Check if the command execution was successful
                if password.returncode != 0:
                    raise RuntimeError(f"Failed to get password for key: {key}")

                # Remove trailing newline character and return the password
                return password.stdout.rstrip("\n")

            key = "openai.com/meduni_matthias_api_key"  # Moritz' setup
            os.environ["OPENAI_API_KEY"] = get_password_from_pass_store(key)
            # print(f"Password for {key}: {openai_api_key}")
        else:
            os.environ["OPENAI_API_KEY"] = api_key

    batch_chat = ChatOpenAI(
        temperature=0,
        model_name=model,
        request_timeout=600,
        max_retries=1,
    )
    stream_chat = ChatOpenAI(
        streaming=True,
        temperature=0,
        model_name=model,
        request_timeout=600,
        callback_manager=CallbackManager([StreamingStdOutCallbackHandler()]),
        # verbose=True,
    )
    return batch_chat, stream_chat


def generate_request(experiment_name, system_name, human_name):
    """ """

    system_message = PROJECT_DIR / "system_messages" / system_name
    human_message = (
        PROJECT_DIR / "experiments" / experiment_name / "prompts" / human_name
    )

    request = [
        SystemMessage(content=system_message.read_text()),
        HumanMessage(content=human_message.read_text()),
    ]

    return request


def run_combination(experiment_name, system_name, human_name, stream=True):
    """
    Run a single combination of system and human messages
    Args:
        experiment_name: The name of the experiment to run prompts from.
        system_name: The name of the system prompt to use.
        human_name: The name of the human prompt to use
    """
    request = generate_request(experiment_name, system_name, human_name)

    resp = stream_chat(request) if stream else batch_chat(request)
    return resp


def run_combinations(experiment_name, system_names, human_names=None, stream=True):
    """
    Run a set of combinations of system and human messages

    Args:
        experiment_name: The name of the experiment to run prompts from.
        system_names: The names of the system messages to use.
        human_names: The names of the prompts to use. Provide None (default) to run all for the given experiment

    """
    if human_names is None:
        human_names = [
            name.stem
            for name in (
                PROJECT_DIR / "experiments" / experiment_name / "prompts"
            ).iterdir()
            if name.is_file()
        ]

    combinations = list(itertools.product(system_names, human_names))
    requests = [
        generate_request(experiment_name, system_name, human_name)
        for system_name, human_name in combinations
    ]

    responses = (
        stream_chat.generate(requests) if stream else batch_chat.generate(requests)
    )

    return responses.generations, combinations


def save_result(experiment, results, request_combinations):
    # Store the results using the (README) specified file format
    for request, message in zip(request_combinations, results):
        output_file = (
            PROJECT_DIR
            / "experiments"
            / experiment
            / "ai_messages"
            / f"{request[0]}--{request[1]}"
        )
        output_file.parent.mkdir(parents=True, exist_ok=True)
        output_file.write_text(message[0].text)
