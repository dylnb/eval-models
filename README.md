# Eval Models

## Setup

First run:

`docker run -p 8888:8888 -v $PWD:/home/jovyan/pwd --env JUPYTER_ENABLE_LAB=yes --env JUPYTER_TOKEN=x --name ihaskell_notebook crosscompass/ihaskell-notebook:latest`

Second run:

`docker start -i ihaskell_notebook`
